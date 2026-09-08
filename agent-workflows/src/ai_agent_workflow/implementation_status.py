"""Derive one honest source-wide status from canonical physical evidence.

The status manifest contains references and gate state, not a second coverage
registry. Coverage is rebuilt from the checked-in source release and evidence
index on every evaluation.
"""

from __future__ import annotations

import argparse
import hashlib
import html as html_module
import json
import re
import sys
from html.parser import HTMLParser
from pathlib import Path
from typing import Any, Dict, List, Mapping, Optional, Sequence, Tuple

from .catalog import CatalogError, compile_registry
from .s0_evidence import evaluate_source_transition_fixture
from .schema_validation import SchemaValidationError, validate_document
from .section_transition_evidence import verify_current_canonical_lineage
from .source_release import SourceReleaseError, SourceReleaseV1, serialize_manifest
from .source_wide_fixtures import SourceWideFixtureError, SourceWideFixturesV1


class ImplementationStatusError(RuntimeError):
    """Physical evidence cannot support the requested completion claim."""


_STATUS_SCHEMA = "agent-workflows/schemas/implementation-status-v1.schema.json"
_EVALUATION_SCHEMA = "agent-workflows/schemas/implementation-evaluation-v1.schema.json"
_RELEASE_PATH = "agent-workflows/manifests/source-release.json"
_INDEX_PATH = "agent-workflows/manifests/source-evidence-index.json"
_PLAN_COVERAGE_PATH = "agent-workflows/manifests/plan-coverage.json"
_COMPLETION_REGISTRY = "full-workflow-readiness/v1"
_SOURCE_GATE_IDS = (
    "compiled-catalog-present",
    "group-manifests-implemented",
    "workflow-profiles-implemented",
    "two-stage-review-integrated-across-workflows",
    "release-readiness-review-completed",
    "source-wide-integration-completed",
)
_OPERATIONAL_GATE_IDS = (
    "actual-a7-handoff-complete",
    "current-objective-approved",
    "current-run-objective-achieved",
    "personal-profile-pilots-completed",
    "objective-audit-completed",
    "run-outcome-decided",
    "archive-or-continue-completed",
    "live-runtime-verified",
    "nix-generation-build-verified",
    "nix-rebuild-verified",
    "migration-complete",
    "activation-complete",
    "full-workflow-operational-adoption-approved",
)
_FIXTURE_SPECS = (
    (
        "agent-workflows/evidence/source-wide/profile-feature.json",
        "agent-workflows/tests/fixtures/s8/profile-feature.json",
        "profile-source-execution",
        "feature",
    ),
    (
        "agent-workflows/evidence/source-wide/profile-bug-fix.json",
        "agent-workflows/tests/fixtures/s8/profile-bug-fix.json",
        "profile-source-execution",
        "bug-fix",
    ),
    (
        "agent-workflows/evidence/source-wide/profile-improvement.json",
        "agent-workflows/tests/fixtures/s8/profile-improvement.json",
        "profile-source-execution",
        "improvement",
    ),
    (
        "agent-workflows/evidence/source-wide/cold-resume.json",
        "agent-workflows/tests/fixtures/s8/cold-resume.json",
        "cold-source-resume",
        None,
    ),
    (
        "agent-workflows/evidence/source-wide/h1-h2-h3-skeleton.json",
        "agent-workflows/tests/fixtures/s8/h1-h2-h3-skeleton.json",
        "h1-h2-h3-source-skeleton",
        None,
    ),
)
_SOURCE_ONLY_BOUNDARY = {
    "current_run_evidence": False,
    "live_runtime_evidence": False,
    "grants_approval": False,
    "objective_outcome_claimed": False,
    "run_completion_claimed": False,
    "build_or_rebuild_claimed": False,
    "migration_or_activation_claimed": False,
    "operational_adoption_claimed": False,
    "full_workflow_ready": False,
}
_DOCUMENTATION_TEMPLATE_DIGEST = "sha256:5d38af5aefc26f71fceb201f86c6558cb790bce6dd741943b98557864f2a0160"
_DOCUMENTATION_CLAIM_IDS = (
    "named-ratio",
    "status-summary",
    "distributed-count",
    "coverage-summary",
    "distributed-summary",
    "missing-summary",
)
_DOCUMENTATION_CLAIM_SHAPES = {
    "named-ratio": ("span", {"class": "num"}),
    "status-summary": (
        "div",
        {"class": "callout danger", "style": "margin-top:1rem"},
    ),
    "distributed-count": ("p", {}),
    "coverage-summary": ("li", {}),
    "distributed-summary": ("div", {}),
    "missing-summary": ("p", {}),
}


def _json_exact(left: Any, right: Any) -> bool:
    if type(left) is not type(right):
        return False
    if isinstance(right, dict):
        return set(left) == set(right) and all(
            _json_exact(left[key], right[key]) for key in right
        )
    if isinstance(right, list):
        return len(left) == len(right) and all(
            _json_exact(a, b) for a, b in zip(left, right)
        )
    return left == right


def _require(condition: bool, reason: str) -> None:
    if not condition:
        raise ImplementationStatusError(reason)


def _digest_bytes(raw: bytes) -> str:
    return "sha256:" + hashlib.sha256(raw).hexdigest()


def _resolve_root(source_root: Path) -> Path:
    supplied = Path(source_root)
    try:
        if supplied.is_symlink():
            raise ImplementationStatusError("source root cannot be a symlink")
        root = supplied.resolve(strict=True)
    except OSError as error:
        raise ImplementationStatusError("source root is unavailable") from error
    _require(root.is_dir(), "source root is not a directory")
    _require((root / "agent-workflows").is_dir(), "source root lacks agent-workflows")
    return root


def _source_path(root: Path, relative: Any, label: str) -> Path:
    _require(isinstance(relative, str) and bool(relative), "%s path is malformed" % label)
    candidate = Path(relative)
    _require(
        not candidate.is_absolute()
        and ".." not in candidate.parts
        and "." not in candidate.parts
        and "\\" not in relative
        and not re.match(r"^[A-Za-z]:", relative)
        and not any(character in relative for character in "*?[]"),
        "%s path escapes the source root" % label,
    )
    resolved = (root / candidate).resolve()
    try:
        resolved.relative_to(root)
    except ValueError as error:
        raise ImplementationStatusError("%s path escapes the source root" % label) from error
    return resolved


def _read_json(path: Path, label: str) -> Dict[str, Any]:
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeError, json.JSONDecodeError) as error:
        raise ImplementationStatusError("%s is unreadable or invalid" % label) from error
    _require(isinstance(value, dict), "%s must be a JSON object" % label)
    return value


def _read_ref(root: Path, ref: Mapping[str, Any], label: str) -> Tuple[Dict[str, Any], bytes]:
    _require(isinstance(ref, Mapping) and set(ref) == {"path", "digest"}, "%s ref is malformed" % label)
    path = _source_path(root, ref.get("path"), label)
    _require(path.is_file(), "%s target is not a file" % label)
    try:
        raw = path.read_bytes()
    except OSError as error:
        raise ImplementationStatusError("%s target is unreadable" % label) from error
    _require(_digest_bytes(raw) == ref.get("digest"), "%s digest mismatch" % label)
    try:
        value = json.loads(raw.decode("utf-8"))
    except (UnicodeError, json.JSONDecodeError) as error:
        raise ImplementationStatusError("%s target is not JSON" % label) from error
    _require(isinstance(value, dict), "%s target must be a JSON object" % label)
    return value, raw


def _verify_content_ref(root: Path, ref: Mapping[str, Any], label: str) -> None:
    _require(
        isinstance(ref, Mapping) and set(ref) == {"path", "digest"},
        "%s ref is malformed" % label,
    )
    path = _source_path(root, ref.get("path"), label)
    _require(path.is_file(), "%s target is not a file" % label)
    try:
        raw = path.read_bytes()
    except OSError as error:
        raise ImplementationStatusError("%s target is unreadable" % label) from error
    _require(_digest_bytes(raw) == ref.get("digest"), "%s digest mismatch" % label)


def _validate(root: Path, document: Mapping[str, Any], schema_relative: str, label: str) -> None:
    schema = _read_json(_source_path(root, schema_relative, label + " schema"), label + " schema")
    try:
        validate_document(document, schema, registry=dict(schema.get("$defs", {})))
    except SchemaValidationError as error:
        raise ImplementationStatusError("%s does not satisfy its schema: %s" % (label, error)) from error


def _unique_by_id(entries: Any, label: str) -> Dict[str, Mapping[str, Any]]:
    _require(isinstance(entries, list), "%s must be an array" % label)
    result: Dict[str, Mapping[str, Any]] = {}
    for entry in entries:
        _require(isinstance(entry, Mapping), "%s entry is malformed" % label)
        identifier = entry.get("id")
        _require(isinstance(identifier, str) and bool(identifier), "%s id is malformed" % label)
        _require(identifier not in result, "%s contains duplicate id %s" % (label, identifier))
        result[identifier] = entry
    return result


def project_plan_coverage(
    release: Mapping[str, Any],
    index: Mapping[str, Any],
    source_root: Optional[Path] = None,
) -> Dict[str, Any]:
    """Project the legacy S0 seam from the canonical source index."""

    root = _resolve_root(source_root or Path(__file__).resolve().parents[3])

    def receipt_selector(entry: Mapping[str, Any]) -> str:
        evidence = _read_json(
            _source_path(root, entry["evidence"]["path"], "coverage evidence"),
            "coverage evidence",
        )
        schema = evidence.get("schema")
        if schema == "qualified-contract-acceptance/v1":
            selector = evidence.get("test_ref", {}).get("selector")
            _require(
                selector == entry["test_selector"],
                "qualified coverage selector differs from the canonical index",
            )
            return entry["test_selector"]
        if schema == "agent-workflow-contract-acceptance/v1":
            test_ref = evidence.get("test_ref")
            _require(isinstance(test_ref, Mapping), "legacy coverage test ref is malformed")
            path = test_ref.get("path")
            selector = test_ref.get("selector")
            _require(
                isinstance(path, str)
                and isinstance(selector, str)
                and "%s::%s" % (path, selector) == entry["test_selector"],
                "legacy coverage selector differs from the canonical index",
            )
            return selector
        raise ImplementationStatusError("coverage evidence schema is not accepted")

    selectable = sorted(
        (entry for entry in index["entries"] if entry["kind"] != "required-surface"),
        key=lambda entry: entry["id"],
    )
    source_inventory = [
        {
            "id": entry["id"],
            "receipt": {
                "schema": "coverage-receipt/v1",
                "subject_id": entry["id"],
                "source": entry["source"]["path"],
                "source_digest": entry["source"]["digest"],
                "evidence": entry["evidence"]["path"],
                "evidence_digest": entry["evidence"]["digest"],
                "selector": receipt_selector(entry),
                "result": entry["evidence"]["result"],
            },
        }
        for entry in selectable
    ]
    coverage = release["coverage"]
    complete = all(
        item["complete"] and item["accepted"] == item.get("catalog_total", item["manifest_total"])
        for item in coverage.values()
    )
    return {
        "schema": "plan-coverage/v1",
        "planned": {
            "named": coverage["named_contracts"]["catalog_total"],
            "profiles": coverage["profile_steps"]["catalog_total"],
            "additional_surfaces": coverage["additional_required_surfaces"]["manifest_total"],
        },
        "source_inventory": source_inventory,
        "accepted_coverage": [entry["id"] for entry in selectable],
        "claims": {
            "s0_registry_ready": complete,
            "full_workflow_ready": False,
            "actual_a7_handoff_complete": False,
        },
    }


def _verify_release_and_index(
    root: Path, authority: Mapping[str, Any]
) -> Tuple[Dict[str, Any], Dict[str, Any]]:
    release_ref = authority["source_release_ref"]
    index_ref = authority["source_evidence_index_ref"]
    _require(release_ref["path"] == _RELEASE_PATH, "source release path is not canonical")
    _require(index_ref["path"] == _INDEX_PATH, "source evidence index path is not canonical")
    release, release_raw = _read_ref(root, release_ref, "source release")
    index, index_raw = _read_ref(root, index_ref, "source evidence index")
    try:
        compiled = SourceReleaseV1(source_root=root).compile()
    except SourceReleaseError as error:
        raise ImplementationStatusError("canonical source release is invalid: %s" % error) from error
    _require(
        release_raw == serialize_manifest(compiled["source_release"]),
        "source release differs from physical compilation",
    )
    _require(
        index_raw == serialize_manifest(compiled["evidence_index"]),
        "source evidence index differs from physical compilation",
    )
    _require(
        release["evidence_index"]["digest"] == index_ref["digest"],
        "source release does not bind the evidence index",
    )
    _require(
        authority["plan_ref"] == release["authority"]["plan"]
        and authority["catalog_ref"] == release["authority"]["step_catalog"],
        "fixed plan/catalog refs do not match the source release",
    )
    return release, index


def _verify_fixture_outputs(
    root: Path, refs: Any
) -> Tuple[List[Dict[str, Any]], int]:
    _require(isinstance(refs, list), "fixture refs must be an array")
    expected_paths = [spec[0] for spec in _FIXTURE_SPECS]
    _require(
        [ref.get("path") for ref in refs if isinstance(ref, Mapping)] == expected_paths,
        "fixture ref identity or order is not exact",
    )
    compiler = SourceWideFixturesV1(source_root=root, reference_root=root)
    outputs: List[Dict[str, Any]] = []
    required_findings = 0
    for ref, (evidence_path, input_path, kind, profile) in zip(refs, _FIXTURE_SPECS):
        evidence, _ = _read_ref(root, ref, "source-wide fixture %s" % evidence_path)
        fixture_input = _read_json(_source_path(root, input_path, "fixture input"), "fixture input")
        try:
            compiled = compiler.compile_fixture(fixture_input)
        except SourceWideFixtureError as error:
            raise ImplementationStatusError("source-wide fixture replay failed: %s" % error) from error
        _require(_json_exact(evidence, compiled), "source-wide fixture evidence is not exact: %s" % evidence_path)
        _require(evidence.get("result_kind") == kind, "source-wide fixture kind is cross-bound")
        if profile is not None:
            _require(evidence.get("profile") == profile, "source-wide profile fixture is cross-bound")
            required_findings += int(evidence["review_execution"]["open_required_finding_count"])
        boundary = evidence.get("claim_boundary")
        _require(
            isinstance(boundary, Mapping)
            and boundary.get("source_candidate_only") is True
            and all(
                boundary.get(key) is False
                for key in (
                    "current_run_evidence",
                    "live_runtime_evidence",
                    "grants_approval",
                    "objective_outcome_claimed",
                    "run_completion_claimed",
                    "full_workflow_ready",
                )
            ),
            "source-wide fixture exceeds its source-only boundary",
        )
        outputs.append(evidence)
    return outputs, required_findings


def _expected_gate_subjects(
    release: Mapping[str, Any], authority: Mapping[str, Any]
) -> Dict[str, List[str]]:
    fixture_paths = [spec[0] for spec in _FIXTURE_SPECS]
    profile_fixture_paths = fixture_paths[: len(release["inputs"]["profile_manifests"])]
    non_aggregate_gate_paths = [
        "agent-workflows/evidence/%s.json" % gate_id
        for gate_id in _SOURCE_GATE_IDS[:-1]
    ]
    return {
        "compiled-catalog-present": [
            _RELEASE_PATH,
            _INDEX_PATH,
            authority["plan_ref"]["path"],
            authority["catalog_ref"]["path"],
        ],
        "group-manifests-implemented": [
            _RELEASE_PATH,
            _INDEX_PATH,
            *[item["path"] for item in release["inputs"]["group_manifests"]],
        ],
        "workflow-profiles-implemented": [
            _RELEASE_PATH,
            _INDEX_PATH,
            *profile_fixture_paths,
        ],
        "two-stage-review-integrated-across-workflows": profile_fixture_paths,
        "release-readiness-review-completed": fixture_paths,
        "source-wide-integration-completed": [
            _RELEASE_PATH,
            _INDEX_PATH,
            _PLAN_COVERAGE_PATH,
            *fixture_paths,
            *non_aggregate_gate_paths,
        ],
    }


def _verify_source_gates(
    root: Path,
    gates_value: Any,
    expected_subjects: Mapping[str, Sequence[str]],
) -> Tuple[List[str], int, List[Dict[str, Any]]]:
    gates = _unique_by_id(gates_value, "source gates")
    _require(tuple(gates) == _SOURCE_GATE_IDS, "source gate identity or order is not exact")
    pending: List[str] = []
    required_findings = 0
    evidence_documents: List[Dict[str, Any]] = []
    for gate_id, gate in gates.items():
        refs = gate.get("evidence_refs")
        _require(isinstance(refs, list), "source gate evidence refs are malformed")
        if gate.get("status") != "passed":
            _require(not refs, "pending source gate cannot carry evidence")
            pending.append(gate_id)
            continue
        _require(len(refs) == 1, "passed source gate requires one evidence ref")
        ref = refs[0]
        expected_path = "agent-workflows/evidence/%s.json" % gate_id
        expected_schema = "agent-workflow-source-gate-evidence/%s/v1" % gate_id
        _require(
            isinstance(ref, Mapping)
            and set(ref) == {"kind", "schema", "path", "digest"}
            and ref.get("kind") == gate_id
            and ref.get("schema") == expected_schema
            and ref.get("path") == expected_path,
            "source gate evidence ref is not canonical: %s" % gate_id,
        )
        evidence, _ = _read_ref(
            root,
            {"path": ref["path"], "digest": ref["digest"]},
            "source gate evidence %s" % gate_id,
        )
        _require(
            set(evidence)
            == {
                "schema",
                "gate_id",
                "status",
                "scope",
                "subject_refs",
                "required_source_findings",
                "claim_boundary",
            },
            "source gate evidence shape is not strict: %s" % gate_id,
        )
        _require(
            evidence["schema"] == expected_schema
            and evidence["gate_id"] == gate_id
            and evidence["status"] == "passed"
            and evidence["scope"] == "source-only"
            and _json_exact(evidence["claim_boundary"], _SOURCE_ONLY_BOUNDARY),
            "source gate evidence exceeds source-only authority: %s" % gate_id,
        )
        findings = evidence["required_source_findings"]
        _require(
            isinstance(findings, list)
            and all(isinstance(item, str) and bool(item) for item in findings)
            and len(findings) == len(set(findings)),
            "source gate findings are malformed: %s" % gate_id,
        )
        subject_refs = evidence["subject_refs"]
        _require(
            isinstance(subject_refs, list)
            and [item.get("path") for item in subject_refs if isinstance(item, Mapping)]
            == list(expected_subjects[gate_id]),
            "source gate subject identity or order is not exact: %s" % gate_id,
        )
        for position, subject_ref in enumerate(subject_refs):
            _verify_content_ref(
                root, subject_ref, "%s subject_refs[%d]" % (gate_id, position)
            )
        required_findings += len(findings)
        evidence_documents.append(evidence)
    return pending, required_findings, evidence_documents


def _verify_operational_gates(value: Any) -> List[str]:
    gates = _unique_by_id(value, "operational gates")
    _require(tuple(gates) == _OPERATIONAL_GATE_IDS, "operational gate identity or order is not exact")
    for gate_id, gate in gates.items():
        _require(
            gate.get("status") == "pending" and gate.get("evidence_refs") == [],
            "source evidence cannot satisfy operational gate %s" % gate_id,
        )
    return list(gates)


def _verify_registry_projection(
    index: Mapping[str, Any], projection: Mapping[str, Any]
) -> None:
    selectable = {
        entry["id"]: entry
        for entry in index["entries"]
        if entry["kind"] != "required-surface"
    }
    _require(set(projection["target_registry"]) == set(selectable), "catalog projection target set differs from source index")
    _require(set(projection["accepted_coverage"]) == set(selectable), "catalog projection accepted set differs from source index")
    _require(not projection["missing_or_invalid"] and not projection["present_unaccepted"], "catalog projection contains incomplete source entries")
    for identifier, entry in selectable.items():
        target = projection["target_registry"][identifier]
        _require(
            target["name"] == entry["name"]
            and target["implementation_kind"] == entry["kind"]
            and target["canonical_source"] == entry["source"]["path"]
            and target["canonical_source_digest"] == entry["source"]["digest"]
            and target["evidence_ref"] == entry["evidence"]["path"]
            and target["evidence_digest"] == entry["evidence"]["digest"]
            and target["state"] == "accepted",
            "catalog projection entry differs from source index: %s" % identifier,
        )


def _evaluate_implementation(
    manifest_path: Path,
    source_root: Path,
    *,
    require_current_lineage: bool,
) -> Dict[str, Any]:
    root = _resolve_root(source_root)
    if require_current_lineage and not verify_current_canonical_lineage(root):
        raise ImplementationStatusError(
            "REG-V2-CANONICAL-LINEAGE: current canonical lineage is not trusted"
        )
    manifest = _read_json(Path(manifest_path), "implementation status manifest")
    _validate(root, manifest, _STATUS_SCHEMA, "implementation status manifest")
    authority = manifest["source_authority"]
    release, index = _verify_release_and_index(root, authority)

    plan_coverage_ref = authority["plan_coverage_ref"]
    _require(plan_coverage_ref["path"] == _PLAN_COVERAGE_PATH, "plan coverage path is not canonical")
    plan_coverage, _ = _read_ref(root, plan_coverage_ref, "plan coverage")
    expected_plan_coverage = project_plan_coverage(release, index, root)
    _require(_json_exact(plan_coverage, expected_plan_coverage), "plan coverage differs from canonical source index")

    fixture_outputs, fixture_findings = _verify_fixture_outputs(
        root, authority["source_fixture_refs"]
    )
    expected_gate_subjects = _expected_gate_subjects(release, authority)
    pending_source_gates, gate_findings, gate_evidence = _verify_source_gates(
        root, manifest["source_gates"], expected_gate_subjects
    )
    pending_operational_gates = _verify_operational_gates(manifest["operational_gates"])

    try:
        registry = compile_registry(
            source_root=root, current_lineage=require_current_lineage
        )
    except CatalogError as error:
        raise ImplementationStatusError("catalog compatibility projection is invalid: %s" % error) from error
    _verify_registry_projection(index, registry)

    entries = index["entries"]
    named_entries = [
        entry
        for entry in entries
        if entry["kind"] in ("skill", "shared-protocol-operation")
    ]
    profile_entries = [entry for entry in entries if entry["kind"] == "workflow-profile-step"]
    surface_entries = [entry for entry in entries if entry["kind"] == "required-surface"]
    _require(
        len(entries) == index["entry_count"]
        and len({entry["id"] for entry in entries}) == len(entries)
        and all(entry["evidence"]["result"] == "passed" for entry in entries),
        "source evidence index entries are incomplete",
    )
    source_coverage = release["coverage"]
    coverage_complete = (
        source_coverage["named_contracts"]["catalog_total"] == len(named_entries)
        == source_coverage["named_contracts"]["accepted"]
        and source_coverage["profile_steps"]["catalog_total"] == len(profile_entries)
        == source_coverage["profile_steps"]["accepted"]
        and source_coverage["additional_required_surfaces"]["manifest_total"]
        == len(surface_entries)
        == source_coverage["additional_required_surfaces"]["accepted"]
        and all(item["complete"] for item in source_coverage.values())
    )

    skill_names = sorted(entry["name"] for entry in named_entries if entry["kind"] == "skill")
    physical_skills = sorted(
        path.name
        for path in (root / "agent-workflows/skills").iterdir()
        if path.is_dir() and (path / "SKILL.md").is_file()
    )
    _require(skill_names == physical_skills, "physical distributed Skill directories differ from source index")
    shared_operations = [
        entry["id"] for entry in named_entries if entry["kind"] == "shared-protocol-operation"
    ]

    required_source_findings = fixture_findings + gate_findings
    source_wide = (
        coverage_complete
        and not pending_source_gates
        and required_source_findings == 0
        and bool(fixture_outputs)
        and len(gate_evidence) == len(_SOURCE_GATE_IDS)
        and registry["counts"]["accepted_named"] == len(named_entries)
        and registry["counts"]["accepted_profiles"] == len(profile_entries)
        and registry["counts"]["additional_surfaces"] == len(surface_entries)
        and release["claims"]["canonical_source_projection_complete"] is True
    )
    full_ready = source_wide and not pending_operational_gates
    expected_claim = {
        "overall_status": (
            "ready"
            if full_ready
            else "source-complete-operationally-pending"
            if source_wide
            else "source-incomplete"
        ),
        "source_wide_integration_complete": source_wide,
        "full_workflow_ready": full_ready,
    }
    _require(
        _json_exact(manifest["completion_claim"], expected_claim),
        "completion claim disagrees with physical evidence",
    )

    claims = {
        "s0_registry_ready": bool(registry["claims"]["s0_registry_ready"]),
        "source_transition_fixture_passed": evaluate_source_transition_fixture(root),
        "canonical_source_projection_complete": bool(
            release["claims"]["canonical_source_projection_complete"]
        ),
        "source_wide_integration_complete": source_wide,
        "actual_a7_handoff_complete": False,
        "current_objective_approved": False,
        "current_run_objective_achieved": False,
        "personal_profile_pilots_completed": False,
        "objective_audit_completed": False,
        "run_outcome_decided": False,
        "archive_or_continue_executed": False,
        "live_runtime_verified": False,
        "nix_generation_build_verified": False,
        "nix_rebuild_verified": False,
        "migration_complete": False,
        "activation_complete": False,
        "operational_adoption_approved": False,
        "full_workflow_ready": full_ready,
    }
    result = {
        "schema": "agent-workflow-implementation-evaluation/v1",
        "product_id": manifest["product_id"],
        "release_id": release["schema"],
        "release_scope": "source-wide",
        "release_scope_status": "source-complete" if source_wide else "source-incomplete",
        "overall_status": expected_claim["overall_status"],
        "source_wide_integration_complete": source_wide,
        "full_workflow_ready": full_ready,
        "activation_policy": manifest["activation_policy"],
        "completion_registry": _COMPLETION_REGISTRY,
        "coverage": {
            "named_contracts": {
                "accepted": len(named_entries),
                "planned": source_coverage["named_contracts"]["catalog_total"],
                "remaining_ids": [],
            },
            "profile_steps": {
                "accepted": len(profile_entries),
                "planned": source_coverage["profile_steps"]["catalog_total"],
                "remaining_ids": [],
            },
            "required_surfaces": {
                "accepted": len(surface_entries),
                "planned": source_coverage["additional_required_surfaces"]["manifest_total"],
                "remaining_ids": [],
            },
            "distributed_skills": physical_skills,
            "shared_protocol_operations": shared_operations,
        },
        "required_source_findings": required_source_findings,
        "pending_source_gates": pending_source_gates,
        "pending_operational_gates": pending_operational_gates,
        "missing_required_surfaces": [],
        "pending_completion_gates": pending_operational_gates,
        "source_refs": {
            "plan": dict(authority["plan_ref"]),
            "catalog": dict(authority["catalog_ref"]),
            "source_release": dict(authority["source_release_ref"]),
            "source_evidence_index": dict(authority["source_evidence_index_ref"]),
            "plan_coverage": dict(authority["plan_coverage_ref"]),
            "source_fixtures": [dict(item) for item in authority["source_fixture_refs"]],
        },
        "s0_coverage": registry,
        "claims": claims,
    }
    _validate(root, result, _EVALUATION_SCHEMA, "implementation evaluation")
    return result


def evaluate_implementation(manifest_path: Path, source_root: Path) -> Dict[str, Any]:
    """Evaluate the checked-in source status against its reviewed lineage."""

    return _evaluate_implementation(
        manifest_path, source_root, require_current_lineage=True
    )


class _CompletionProjectionParser(HTMLParser):
    def __init__(self) -> None:
        super().__init__()
        self.projections: List[Dict[str, Optional[str]]] = []
        self.claims: List[Tuple[str, str]] = []
        self.claim_shapes: List[Tuple[str, str, Dict[str, Optional[str]]]] = []
        self.duplicate_attributes: List[str] = []
        self.foreign_status_attributes: List[str] = []
        self.outside_text_parts: List[str] = []
        self._active_claim: Optional[str] = None
        self._active_claim_tag: Optional[str] = None
        self._active_claim_parts: List[str] = []
        self._ignored_depth = 0

    def handle_starttag(self, tag: str, attrs: List[Tuple[str, Optional[str]]]) -> None:
        names = [name for name, _ in attrs]
        self.duplicate_attributes.extend(name for name in set(names) if names.count(name) > 1)
        values = dict(attrs)
        if tag in ("script", "style"):
            self._ignored_depth += 1
        if values.get("data-completion-projection"):
            self.projections.append(values)
        else:
            self.foreign_status_attributes.extend(
                key for key in values if key in _DOCUMENTATION_STATUS_DATA_KEYS
            )
        claim = values.get("data-completion-claim")
        if claim is not None:
            _require(self._active_claim is None, "completion documentation claims cannot be nested")
            self._active_claim = str(claim)
            self._active_claim_tag = tag
            self._active_claim_parts = []
            self.claim_shapes.append((str(claim), tag, values))

    def handle_data(self, data: str) -> None:
        if self._ignored_depth:
            return
        if self._active_claim is None:
            self.outside_text_parts.append(data)
        else:
            self._active_claim_parts.append(data)

    def handle_endtag(self, tag: str) -> None:
        if tag in ("script", "style") and self._ignored_depth:
            self._ignored_depth -= 1
            return
        if self._active_claim is not None and tag == self._active_claim_tag:
            self.claims.append(
                (self._active_claim, " ".join(" ".join(self._active_claim_parts).split()))
            )
            self._active_claim = None
            self._active_claim_tag = None
            self._active_claim_parts = []


def _documentation_projection(status: Mapping[str, Any]) -> Dict[str, str]:
    coverage = status["coverage"]
    claims = status["claims"]
    return {
        "data-completion-projection": str(status["schema"]),
        "data-product-id": str(status["product_id"]),
        "data-release-id": str(status["release_id"]),
        "data-release-scope": str(status["release_scope"]),
        "data-release-scope-status": str(status["release_scope_status"]),
        "data-overall-status": str(status["overall_status"]),
        "data-source-wide-integration-complete": str(status["source_wide_integration_complete"]).lower(),
        "data-full-workflow-ready": str(status["full_workflow_ready"]).lower(),
        "data-s0-registry-ready": str(claims["s0_registry_ready"]).lower(),
        "data-source-transition-fixture-passed": str(claims["source_transition_fixture_passed"]).lower(),
        "data-actual-a7-handoff-complete": str(claims["actual_a7_handoff_complete"]).lower(),
        "data-current-objective-approved": str(claims["current_objective_approved"]).lower(),
        "data-migration-complete": str(claims["migration_complete"]).lower(),
        "data-activation-complete": str(claims["activation_complete"]).lower(),
        "data-activation-policy": str(status["activation_policy"]),
        "data-completion-registry": str(status["completion_registry"]),
        "data-named-accepted": str(coverage["named_contracts"]["accepted"]),
        "data-named-planned": str(coverage["named_contracts"]["planned"]),
        "data-profile-accepted": str(coverage["profile_steps"]["accepted"]),
        "data-profile-planned": str(coverage["profile_steps"]["planned"]),
        "data-surface-accepted": str(coverage["required_surfaces"]["accepted"]),
        "data-surface-planned": str(coverage["required_surfaces"]["planned"]),
        "data-distributed-skill-count": str(len(coverage["distributed_skills"])),
        "data-shared-operation-count": str(len(coverage["shared_protocol_operations"])),
        "data-required-source-findings": str(status["required_source_findings"]),
        "data-pending-source-gates": "|".join(status["pending_source_gates"]),
        "data-pending-operational-gates": "|".join(status["pending_operational_gates"]),
        "data-plan-path": str(status["source_refs"]["plan"]["path"]),
        "data-plan-digest": str(status["source_refs"]["plan"]["digest"]),
        "data-catalog-path": str(status["source_refs"]["catalog"]["path"]),
        "data-catalog-digest": str(status["source_refs"]["catalog"]["digest"]),
    }


_DOCUMENTATION_STATUS_DATA_KEYS = frozenset(
    (
        "data-completion-projection",
        "data-product-id",
        "data-release-id",
        "data-release-scope",
        "data-release-scope-status",
        "data-overall-status",
        "data-source-wide-integration-complete",
        "data-full-workflow-ready",
        "data-s0-registry-ready",
        "data-source-transition-fixture-passed",
        "data-actual-a7-handoff-complete",
        "data-current-objective-approved",
        "data-migration-complete",
        "data-activation-complete",
        "data-activation-policy",
        "data-completion-registry",
        "data-named-accepted",
        "data-named-planned",
        "data-profile-accepted",
        "data-profile-planned",
        "data-surface-accepted",
        "data-surface-planned",
        "data-distributed-skill-count",
        "data-shared-operation-count",
        "data-required-source-findings",
        "data-pending-source-gates",
        "data-pending-operational-gates",
        "data-plan-path",
        "data-plan-digest",
        "data-catalog-path",
        "data-catalog-digest",
    )
)


def _documentation_claims(status: Mapping[str, Any]) -> Dict[str, str]:
    coverage = status["coverage"]
    named = coverage["named_contracts"]
    profiles = coverage["profile_steps"]
    surfaces = coverage["required_surfaces"]
    operational = "、".join(status["pending_operational_gates"])
    return {
        "named-ratio": "%d / %d" % (named["accepted"], named["planned"]),
        "status-summary": (
            "source-wide機械判定: overall_status=%s; source_wide_integration_complete=%s; "
            "full_workflow_ready=%s; named contract=%d/%d; profile step=%d/%d; "
            "required surface=%d/%d; required source findings=%d。"
            % (
                status["overall_status"],
                str(status["source_wide_integration_complete"]).lower(),
                str(status["full_workflow_ready"]).lower(),
                named["accepted"],
                named["planned"],
                profiles["accepted"],
                profiles["planned"],
                surfaces["accepted"],
                surfaces["planned"],
                status["required_source_findings"],
            )
        ),
        "distributed-count": "physical distributed Skill directories: %d" % len(coverage["distributed_skills"]),
        "coverage-summary": (
            "source coverage: %d/%d named contract、%d/%d profile step、%d/%d required surface。"
            % (
                named["accepted"],
                named["planned"],
                profiles["accepted"],
                profiles["planned"],
                surfaces["accepted"],
                surfaces["planned"],
            )
        ),
        "distributed-summary": (
            "物理Skill directoryは%d件、Group F shared protocol operationは%d件。"
            "合計したnamed contract coverageと物理Skill数を混同しない。"
            % (len(coverage["distributed_skills"]), len(coverage["shared_protocol_operations"]))
        ),
        "missing-summary": (
            "pending source gate: %s。未検証のoperational boundary: %s。"
            % (
                "、".join(status["pending_source_gates"]) if status["pending_source_gates"] else "なし",
                operational if operational else "なし",
            )
        ),
    }


def render_documentation_projection(status: Mapping[str, Any], template_html: str) -> str:
    rendered = template_html
    for key, value in _documentation_projection(status).items():
        pattern = re.compile(r"(%s=\")[^\"]*(\")" % re.escape(key))
        rendered, count = pattern.subn(
            lambda match: match.group(1) + html_module.escape(value, quote=True) + match.group(2),
            rendered,
        )
        _require(count == 1, "completion documentation template has invalid projection field %s" % key)
    for key, value in _documentation_claims(status).items():
        pattern = re.compile(
            r'(<(?P<tag>[A-Za-z0-9]+)\b[^>]*data-completion-claim="%s"[^>]*>)'
            r'.*?(</(?P=tag)>)' % re.escape(key),
            re.DOTALL,
        )
        rendered, count = pattern.subn(
            lambda match: match.group(1) + html_module.escape(value) + match.group(3),
            rendered,
        )
        _require(count == 1, "completion documentation template has invalid claim %s" % key)
    return rendered


def _normalize_documentation_template(value: str) -> str:
    projection_pattern = re.compile(
        r'<section\b(?=[^>]*\bid="inventory")(?=[^>]*\bdata-completion-projection=")[^>]*>',
        re.DOTALL,
    )
    normalized, projection_count = projection_pattern.subn(
        '<section id="inventory" data-completion-projection-template>', value
    )
    _require(projection_count == 1, "completion documentation template projection is malformed")
    claim_pattern = re.compile(
        r'(<(?P<tag>[A-Za-z0-9]+)\b[^>]*data-completion-claim="[^"]+"[^>]*>)'
        r'.*?(</(?P=tag)>)',
        re.DOTALL,
    )
    normalized, claim_count = claim_pattern.subn(
        lambda match: match.group(1) + "__COMPLETION_CLAIM__" + match.group(3),
        normalized,
    )
    _require(claim_count == len(_DOCUMENTATION_CLAIM_IDS), "completion documentation template claim set is malformed")
    return normalized.replace("\r\n", "\n")


def validate_documentation_projection(html_path: Path, status: Mapping[str, Any]) -> None:
    try:
        document = Path(html_path).read_text(encoding="utf-8")
    except OSError as error:
        raise ImplementationStatusError("completion documentation is unreadable") from error
    parser = _CompletionProjectionParser()
    parser.feed(document)
    _require(not parser.duplicate_attributes, "completion documentation contains duplicate attributes")
    _require(len(parser.projections) == 1, "completion documentation must contain one projection")
    expected_projection = {"id": "inventory", **_documentation_projection(status)}
    _require(parser.projections[0] == expected_projection, "completion documentation projection disagrees with evaluated evidence")
    _require(not parser.foreign_status_attributes, "completion documentation contains status attributes outside its projection")
    _require(parser._active_claim is None, "completion documentation contains an unclosed claim")
    claims = _unique_by_id(
        [{"id": key, "value": value} for key, value in parser.claims],
        "completion documentation claims",
    )
    expected_claims = _documentation_claims(status)
    _require(
        set(claims) == set(expected_claims)
        and all(claims[key]["value"] == value for key, value in expected_claims.items()),
        "completion documentation claims disagree with evaluated evidence",
    )
    shapes = _unique_by_id(
        [{"id": key, "tag": tag, "attrs": attrs} for key, tag, attrs in parser.claim_shapes],
        "completion documentation claim shapes",
    )
    _require(set(shapes) == set(_DOCUMENTATION_CLAIM_SHAPES), "completion documentation claim shape registry mismatch")
    for key, (tag, attrs) in _DOCUMENTATION_CLAIM_SHAPES.items():
        _require(
            shapes[key]["tag"] == tag
            and shapes[key]["attrs"] == {"data-completion-claim": key, **attrs},
            "completion documentation claim has unexpected structure: %s" % key,
        )
    flattened = " ".join(" ".join(parser.outside_text_parts).split())
    stale_patterns = (
        r"29\s*/\s*60",
        r"0\s*/\s*23",
        r"missing required surface:\s*agent-workflows/workflows",
        r"named Skill contract=29/60",
        r"source_wide_integration_complete=false",
    )
    _require(
        not any(re.search(pattern, flattened, re.IGNORECASE) for pattern in stale_patterns),
        "completion documentation contains a stale source-wide claim",
    )
    template_digest = _digest_bytes(_normalize_documentation_template(document).encode("utf-8"))
    _require(template_digest == _DOCUMENTATION_TEMPLATE_DIGEST, "completion documentation differs from its closed template")


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--manifest", required=True)
    parser.add_argument("--source-root", required=True)
    parser.add_argument("--check-html")
    return parser


def main(argv: Optional[List[str]] = None) -> int:
    args = _parser().parse_args(argv)
    try:
        result = evaluate_implementation(Path(args.manifest), Path(args.source_root))
        if args.check_html:
            validate_documentation_projection(Path(args.check_html), result)
    except (ImplementationStatusError, OSError) as error:
        print("error: %s" % error, file=sys.stderr)
        return 2
    print(json.dumps(result, ensure_ascii=False, sort_keys=True, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())


__all__ = [
    "ImplementationStatusError",
    "evaluate_implementation",
    "project_plan_coverage",
    "render_documentation_projection",
    "validate_documentation_projection",
]
