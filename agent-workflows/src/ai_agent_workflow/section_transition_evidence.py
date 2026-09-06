"""Caller-root verification for the S0 to S1 compatibility close set.

The checked-in authority digest is the trust root.  The authority deliberately
does not bind this verifier's source digest, so updating the constant after
review does not create a digest cycle.
"""
from __future__ import annotations

import hashlib
import json
import re
from collections.abc import Mapping
from pathlib import Path
from typing import Any, Optional

from .catalog import compile_registry
from .s0_evidence import evaluate_source_transition_fixture
from .schema_validation import SchemaValidationError, validate_document


_AUTHORITY_PATH = "agent-workflows/manifests/section-transition-compatibility-authority.json"
_CHECKED_IN_AUTHORITY_DIGEST = "sha256:ac46857a453bca782e41f8f332ce42016338a685376de5b43c2a702d019014ae"
_AUTHORITY_DIGEST = _CHECKED_IN_AUTHORITY_DIGEST
_AUTHORITY_SCHEMA_PATH = "agent-workflows/schemas/section-transition-compatibility-authority-v1.schema.json"
_COMPATIBILITY_SCHEMA_PATH = "agent-workflows/schemas/section-transition-compatibility-v1.schema.json"
_MANIFEST_PATH = "agent-workflows/manifests/section-transition-compatibility.json"
_EVIDENCE_PATH = "agent-workflows/evidence/compatibility/S0-S1-transition.json"
_S0_INDEX_PATH = "agent-workflows/evidence/sections/S0/index.json"
_S0_ACCEPTED_PATH = "agent-workflows/evidence/sections/S0/accepted.json"
_S0_AUTHORITY_PATH = "agent-workflows/manifests/s0-source-transition-authority.json"

_CURRENT_LINEAGE_PATH = "agent-workflows/evidence/current-canonical-lineage.json"
_CURRENT_LINEAGE_SCHEMA_PATH = "agent-workflows/schemas/current-canonical-lineage-v1.schema.json"
_CURRENT_LINEAGE_DIGEST = "sha256:347c4aa1ec8961fa820bf3122895172c041bfc1a86a4d91e783b99c3f989bdf4"
_CURRENT_INVENTORY_PATH = "agent-workflows/manifests/catalog-contract-inventory.json"
_CURRENT_INVENTORY_DIGEST = "sha256:d924a10a18da2f8314797932e9bad4dc6fb1c3f926cafde9ef48292545694493"
_CURRENT_CANDIDATE_AGGREGATE = "sha256:06a8020960bd25112feba3d89be26d2f6cda898e18ec1c7be49c054ff8bdf90b"
_CURRENT_CANONICAL_INPUTS = (
    ("rebuild-plan", "docs/plans/ai-agent-workflow-rebuild.md"),
    ("step-catalog", "docs/plans/ai-agent-workflow-step-catalog.md"),
    ("full-implementation-plan", "docs/plans/ai-agent-workflow-full-implementation-plan.md"),
)
_CURRENT_STATUS_PATH = "agent-workflows/manifests/implementation-status.json"
_S0_EVIDENCE_PATH = "agent-workflows/src/ai_agent_workflow/s0_evidence.py"
_S0_EVIDENCE_DIGEST = "sha256:9ee8109be05fbe7c737c651d5609852cf33a07848ee305e81bc662a903874d7f"
_S0_INTEGRATION_TEST_PATH = "agent-workflows/tests/test_s0_integration.py"
_S0_INTEGRATION_TEST_DIGEST = "sha256:8dbfdb687d8135ef69e59b6069605b67ee37daf6b355c2deef3540ff14b2ff6c"

_NORMAL_PATH = re.compile(r"^[.A-Za-z0-9][A-Za-z0-9._/-]*$")

_SOURCE_PATHS = {
    "compiler": "agent-workflows/src/ai_agent_workflow/section_control_plane.py",
    "kernel": "agent-workflows/src/ai_agent_workflow/control_kernel.py",
    "compatibility-test": "agent-workflows/tests/test_section_transition_compatibility.py",
}
_HISTORICAL_TEST_RECEIPTS = {
    "agent-workflows/tests/test_section_transition_compatibility.py":
        "sha256:7b92590ad71cccbb2ca1bdd89da1d4f6a8ced970d74a6f396b752265eca7d5f0",
}
_U_C_CANDIDATE_DIGEST = "sha256:a0e36ab8d9968deaccfb0c064d7f5c2d75ad8552a0dffc2d1d89f5d285a71352"
_U_C_CLOSURE_REPORTS = {
    "closure_security": ".local/agent/reports/ai-agent-workflow-full-implementation/s1-c-upstream-closure-source-001.md",
    "closure_coverage": ".local/agent/reports/ai-agent-workflow-full-implementation/s1-c-upstream-closure-integration-001.md",
    "closure_validation": ".local/agent/reports/ai-agent-workflow-full-implementation/s1-c-upstream-closure-validation-001.md",
}
_SCHEMA_PATHS = {
    "section-status-v1": "agent-workflows/schemas/section-status-v1.schema.json",
    "section-plan-v1": "agent-workflows/schemas/section-plan-v1.schema.json",
    "section-control-plane-v1": "agent-workflows/schemas/section-control-plane-v1.schema.json",
    "dag-state-v1": "agent-workflows/schemas/dag-state-v1.schema.json",
    "section-transition-compatibility-v1": _COMPATIBILITY_SCHEMA_PATH,
    "section-transition-compatibility-authority-v1": _AUTHORITY_SCHEMA_PATH,
    "s0-source-transition-authority-v1": "agent-workflows/schemas/s0-source-transition-authority-v1.schema.json",
}
_INPUT_PATHS = {
    "docs/plans/ai-agent-workflow-s0-s1-transition-compatibility-plan.md",
    ".local/agent/reports/ai-agent-workflow-full-implementation/s1-c-upstream-closure-source-001.md",
    ".local/agent/reports/ai-agent-workflow-full-implementation/s1-c-upstream-closure-integration-001.md",
    *_U_C_CLOSURE_REPORTS.values(),
    "agent-workflows/schemas/dag-command-v1.schema.json",
    "agent-workflows/schemas/objective-approval-v1.schema.json",
    "agent-workflows/tests/test_control_kernel.py",
    "agent-workflows/tests/test_objective_transition_compatibility.py",
    "agent-workflows/tests/test_section_transition_compatibility.py",
    "agent-workflows/evidence/compatibility/S2-objective-transition.json",
    "agent-workflows/schemas/section-status-v1.schema.json",
    "agent-workflows/schemas/section-plan-v1.schema.json",
    "agent-workflows/schemas/section-control-plane-v1.schema.json",
    "agent-workflows/schemas/dag-state-v1.schema.json",
    _S0_INDEX_PATH,
    _S0_ACCEPTED_PATH,
    _S0_AUTHORITY_PATH,
    "agent-workflows/schemas/s0-source-transition-authority-v1.schema.json",
}
_EVIDENCE_KEYS = {
    "schema", "artifact_kind", "compatibility_id", "version", "fixture",
    "workspace_git_head", "expected_head", "sources", "schemas", "inputs",
    "s0_close", "test_receipts", "check_receipts",
    "review_validation_history", "claims",
}
_MANIFEST_KEYS = {
    "schema", "artifact_kind", "compatibility_id", "version", "status",
    "public_interface", "predecessor", "target_rule", "evidence",
    "claims",
}
_AUTHORITY_KEYS = {
    "schema", "authority_id", "issuer", "compatibility_id", "version",
    "fixture", "workspace_git_head", "expected_head", "manifest", "evidence",
    "sources", "schemas", "inputs", "s0_close", "test_receipts",
    "review_validation_history", "claims",
}
_CLAIMS = {
    "source_transition_fixture_passed": True,
    "actual_a7": False,
    "migration": False,
    "activation": False,
    "full_ready": False,
    "accepted_named": 3,
    "named_targets": 60,
    "accepted_profiles": 0,
    "profile_targets": 23,
}
_TEST_COMMANDS = {
    "focused": "PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=agent-workflows/src python3 -m unittest agent-workflows/tests/test_control_kernel.py agent-workflows/tests/test_objective_transition_compatibility.py",
    "affected-regression": "PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=agent-workflows/src python3 -m unittest agent-workflows/tests/test_s0_integration.py agent-workflows/tests/test_catalog_coverage.py agent-workflows/tests/test_s1_lifecycle_integration.py",
    "full-suite": "PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=agent-workflows/src python3 -m unittest agent-workflows/tests/test_control_kernel.py agent-workflows/tests/test_objective_transition_compatibility.py agent-workflows/tests/test_section_transition_compatibility.py agent-workflows/tests/test_s0_integration.py agent-workflows/tests/test_catalog_coverage.py agent-workflows/tests/test_s1_evidence.py agent-workflows/tests/test_a6r_evidence.py agent-workflows/tests/test_a6r_remediation.py agent-workflows/tests/test_a6r_rev34_convergence.py agent-workflows/tests/test_a7_self_host_handoff.py agent-workflows/tests/test_bootstrap_contracts.py agent-workflows/tests/test_operational_cli.py agent-workflows/tests/test_s1_lifecycle_integration.py agent-workflows/tests/test_section_control_plane_contract.py agent-workflows/tests/test_section_control_plane_kernel.py agent-workflows/tests/test_shared_closure_protocol.py",
}


def _source_digest(path: Path) -> str:
    return "sha256:" + hashlib.sha256(path.read_bytes()).hexdigest()


def _json_exact(actual: Any, expected: Any) -> bool:
    """Compare decoded JSON values without Python's bool/int coercion."""
    if type(actual) is not type(expected):
        return False
    if isinstance(expected, dict):
        return set(actual) == set(expected) and all(
            _json_exact(actual[key], expected[key]) for key in expected
        )
    if isinstance(expected, list):
        return len(actual) == len(expected) and all(
            _json_exact(left, right) for left, right in zip(actual, expected)
        )
    return actual == expected


def _normalized_source_root(source_root: Path) -> Path:
    supplied = Path(source_root)
    try:
        if supplied.is_symlink():
            raise ValueError("compatibility caller root cannot be a symlink")
        root = supplied.resolve(strict=True)
    except OSError as exc:
        raise ValueError("compatibility caller root is unavailable") from exc
    if not root.is_dir() or root.is_symlink():
        raise ValueError("compatibility caller root must be a regular directory")
    return root


def _source_file(root: Path, relative: object) -> Path:
    if (
        not isinstance(relative, str)
        or not relative
        or not _NORMAL_PATH.fullmatch(relative)
        or relative.startswith("/")
        or "//" in relative
        or any(part in {"", ".", ".."} for part in relative.split("/"))
    ):
        raise ValueError("compatibility source path is not normalized")
    candidate = root.joinpath(*relative.split("/"))
    current = candidate
    while current != root:
        if current.is_symlink():
            raise ValueError("compatibility source path contains a symlink")
        current = current.parent
        if current == current.parent and current != root:
            raise ValueError("compatibility source path escapes caller root")
    if not candidate.is_file():
        raise ValueError("compatibility source target is not a regular file")
    try:
        candidate.resolve(strict=True).relative_to(root)
    except (OSError, ValueError) as exc:
        raise ValueError("compatibility source path escapes caller root") from exc
    return candidate


def _load_json(root: Path, ref: Mapping[str, Any], label: str) -> Mapping[str, Any]:
    if set(ref) != {"path", "digest"}:
        raise ValueError("%s reference is malformed" % label)
    path = _source_file(root, ref.get("path"))
    if _source_digest(path) != ref.get("digest"):
        raise ValueError("%s digest does not bind physical bytes" % label)
    value = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(value, Mapping):
        raise ValueError("%s must be a JSON object" % label)
    return value


def _load_fixed_json(root: Path, relative: str, label: str) -> Mapping[str, Any]:
    path = _source_file(root, relative)
    value = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(value, Mapping):
        raise ValueError("%s must be a JSON object" % label)
    return value


def _validate_schema(root: Path, document: Mapping[str, Any], schema_path: str) -> None:
    schema = _load_fixed_json(root, schema_path, schema_path)
    validate_document(document, schema, registry=dict(schema.get("$defs", {})))


def _strings(value: Any):
    if isinstance(value, str):
        yield value
    elif isinstance(value, Mapping):
        for key, item in value.items():
            yield str(key)
            yield from _strings(item)
    elif isinstance(value, list):
        for item in value:
            yield from _strings(item)


def _require_physical_refs(
    root: Path,
    refs: Any,
    expected_paths: set[str],
    label: str,
    *,
    historical_test_receipts: Optional[Mapping[str, str]] = None,
) -> None:
    if not isinstance(refs, list) or len(refs) != len(expected_paths):
        raise ValueError("%s does not have the exact path count" % label)
    paths = [item.get("path") for item in refs if isinstance(item, Mapping)]
    if len(paths) != len(refs) or len(paths) != len(set(paths)) or set(paths) != expected_paths:
        raise ValueError("%s does not bind the exact path set" % label)
    historical = historical_test_receipts or {}
    for item in refs:
        if item["path"] in historical:
            if item.get("digest") != historical[item["path"]]:
                raise ValueError("%s historical test receipt is not exact" % label)
            continue
        path = _source_file(root, item["path"])
        if _source_digest(path) != item.get("digest"):
            raise ValueError("%s digest does not bind physical bytes" % label)


def _require_sources(root: Path, sources: Any) -> None:
    if not isinstance(sources, list) or len(sources) != len(_SOURCE_PATHS):
        raise ValueError("source list is not exact")
    by_role = {item.get("role"): item for item in sources if isinstance(item, Mapping)}
    if len(by_role) != len(sources) or {
        role: item.get("path") for role, item in by_role.items()
    } != _SOURCE_PATHS:
        raise ValueError("source roles do not bind the public implementation set")
    _require_physical_refs(
        root,
        sources,
        set(_SOURCE_PATHS.values()),
        "sources",
        historical_test_receipts=_HISTORICAL_TEST_RECEIPTS,
    )


def _require_schemas(root: Path, schemas: Any) -> None:
    if not isinstance(schemas, list) or len(schemas) != len(_SCHEMA_PATHS):
        raise ValueError("schema list is not exact")
    by_name = {item.get("name"): item for item in schemas if isinstance(item, Mapping)}
    if len(by_name) != len(schemas) or {
        name: item.get("path") for name, item in by_name.items()
    } != _SCHEMA_PATHS:
        raise ValueError("schema names do not bind the compatibility contract")
    _require_physical_refs(root, schemas, set(_SCHEMA_PATHS.values()), "schemas")


def _require_test_receipts(receipts: Any) -> None:
    if not isinstance(receipts, list) or len(receipts) != len(_TEST_COMMANDS):
        raise ValueError("test receipt set is not exact")
    by_scope = {item.get("scope"): item for item in receipts if isinstance(item, Mapping)}
    if len(by_scope) != len(receipts) or {
        scope: item.get("command") for scope, item in by_scope.items()
    } != _TEST_COMMANDS:
        raise ValueError("test receipt commands are not canonical")
    for receipt in receipts:
        if (
            type(receipt.get("exit_status")) is not int
            or receipt.get("exit_status") != 0
            or receipt.get("status") != "passed"
            or type(receipt.get("tests_run")) is not int
            or receipt["tests_run"] < 1
        ):
            raise ValueError("test receipt is not successful")


def _require_review_history(root: Path, history: Any) -> None:
    if (
        not isinstance(history, list)
        or [item.get("unit") for item in history if isinstance(item, Mapping)]
        != ["U-A", "U-B", "U-C"]
    ):
        raise ValueError("review-validation history is not canonical")
    expected_candidates = {
        "U-A": "sha256:a63392d56d23f6fb6c461db830d4f8dca0789b236f7894c9d62810d201a22ebd",
        "U-B": "sha256:1179cfe031b9539250099e1e1f005adaa4b486388555d2699c7ea94d193d7bc0",
        "U-C": _U_C_CANDIDATE_DIGEST,
    }
    expected_record_fields = {
        "U-A": {"unit", "candidate_digest", "closure_validation", "verdict", "required_open", "needs_user", "test_evidence_debt"},
        "U-B": {"unit", "candidate_digest", "closure_validation", "verdict", "required_open", "needs_user", "test_evidence_debt"},
        "U-C": {"unit", "candidate_digest", "closure_security", "closure_coverage", "closure_validation", "verdict", "required_open", "needs_user", "test_evidence_debt"},
    }
    expected_reports = {
        "U-A": {"closure_validation": ".local/agent/reports/ai-agent-workflow-full-implementation/s1-c-upstream-closure-source-001.md"},
        "U-B": {"closure_validation": ".local/agent/reports/ai-agent-workflow-full-implementation/s1-c-upstream-closure-integration-001.md"},
        "U-C": _U_C_CLOSURE_REPORTS,
    }
    for item in history:
        unit = item["unit"]
        if (
            set(item) != expected_record_fields[unit]
            or item.get("candidate_digest") != expected_candidates[unit]
            or item.get("verdict") != "accepted"
            or any(
                type(item.get(field)) is not int or item.get(field) != 0
                for field in ("required_open", "needs_user", "test_evidence_debt")
            )
        ):
            raise ValueError("review-validation verdict is not accepted and closed")
        for field, expected_path in expected_reports[unit].items():
            ref = item.get(field)
            if (
                not isinstance(ref, Mapping)
                or set(ref) != {"path", "digest"}
                or ref.get("path") != expected_path
            ):
                raise ValueError("review-validation report path is not canonical")
            report = _source_file(root, ref.get("path"))
            if _source_digest(report) != ref.get("digest"):
                raise ValueError("review-validation report digest does not bind physical bytes")


def _verify_bound_documents(root: Path, authority: Mapping[str, Any]) -> None:
    if set(authority) != _AUTHORITY_KEYS:
        raise ValueError("compatibility authority shape is not exact")
    _validate_schema(root, authority, _AUTHORITY_SCHEMA_PATH)
    if authority.get("issuer") != "codex-root":
        raise ValueError("compatibility authority issuer is not codex-root")
    manifest_ref, evidence_ref = authority["manifest"], authority["evidence"]
    if manifest_ref.get("path") != _MANIFEST_PATH or evidence_ref.get("path") != _EVIDENCE_PATH:
        raise ValueError("authority does not bind the fixed manifest and evidence paths")
    manifest = _load_json(root, manifest_ref, "compatibility manifest")
    evidence = _load_json(root, evidence_ref, "compatibility evidence")
    _validate_schema(root, manifest, _COMPATIBILITY_SCHEMA_PATH)
    _validate_schema(root, evidence, _COMPATIBILITY_SCHEMA_PATH)
    if set(manifest) != _MANIFEST_KEYS or set(evidence) != _EVIDENCE_KEYS:
        raise ValueError("manifest or evidence shape is not exact")
    if manifest.get("artifact_kind") != "manifest" or evidence.get("artifact_kind") != "evidence":
        raise ValueError("compatibility artifact kinds are reversed")
    if manifest.get("evidence") != evidence_ref:
        raise ValueError("manifest does not bind evidence one way")
    prohibited = {_AUTHORITY_PATH, _CHECKED_IN_AUTHORITY_DIGEST, _AUTHORITY_DIGEST}
    if any(
        token in value
        for value in (*_strings(manifest), *_strings(evidence))
        for token in prohibited
    ):
        raise ValueError("close-set document nominates its external authority")

    authority_common = (
        "compatibility_id", "version", "workspace_git_head", "expected_head",
        "sources", "schemas", "inputs", "s0_close", "test_receipts",
        "review_validation_history", "claims",
    )
    for field in authority_common:
        if not _json_exact(authority.get(field), evidence.get(field)):
            raise ValueError("authority and evidence disagree on %s" % field)
    for field in ("compatibility_id", "version", "claims"):
        if not _json_exact(manifest.get(field), evidence.get(field)):
            raise ValueError("manifest and evidence disagree on %s" % field)
    fixture = evidence["fixture"]
    if authority.get("fixture") != {
        "kind": fixture.get("kind"),
        "public_interface": fixture.get("public_interface"),
        "predecessor": fixture.get("predecessor"),
        "target_rule": fixture.get("target_rule"),
    }:
        raise ValueError("authority fixture identity disagrees with evidence")
    if {
        "public_interface": manifest.get("public_interface"),
        "predecessor": manifest.get("predecessor"),
        "target_rule": manifest.get("target_rule"),
    } != {
        "public_interface": fixture.get("public_interface"),
        "predecessor": fixture.get("predecessor"),
        "target_rule": fixture.get("target_rule"),
    }:
        raise ValueError("manifest transition contract disagrees with evidence")
    if [item.get("case") for item in fixture.get("observed_cases", [])] != [
        "S0-to-S1", "exact-retry", "fresh-cold-resume", "S1-to-S2"
    ]:
        raise ValueError("compatibility fixture does not record the ordered public cases")

    _require_sources(root, evidence["sources"])
    _require_schemas(root, evidence["schemas"])
    _require_physical_refs(
        root,
        evidence["inputs"],
        _INPUT_PATHS,
        "inputs",
        historical_test_receipts=_HISTORICAL_TEST_RECEIPTS,
    )
    _require_test_receipts(evidence["test_receipts"])
    _require_review_history(root, evidence["review_validation_history"])
    if not _json_exact(evidence.get("claims"), _CLAIMS):
        raise ValueError("compatibility claims exceed source-only scope")
    public_counts = compile_registry(source_root=root, current_lineage=False)["counts"]
    # This authority is immutable historical evidence for the S0 close.  Its
    # accepted frontier remains 3/0, while the current public registry may
    # advance.  Only the catalog target identity is a cross-version invariant.
    coverage_counts = {
        key: public_counts.get(key) for key in ("named_targets", "profile_targets")
    }
    expected_counts = {
        key: _CLAIMS[key] for key in ("named_targets", "profile_targets")
    }
    if not _json_exact(coverage_counts, expected_counts):
        raise ValueError("compatibility coverage claims disagree with the public catalog")

    s0_close = evidence["s0_close"]
    if (
        s0_close.get("index", {}).get("path") != _S0_INDEX_PATH
        or s0_close.get("accepted_parent_receipt", {}).get("path") != _S0_ACCEPTED_PATH
    ):
        raise ValueError("compatibility evidence does not bind the fixed S0 close inputs")
    index = _load_json(root, s0_close["index"], "S0 close index")
    _load_json(root, s0_close["accepted_parent_receipt"], "S0 accepted parent receipt")
    if index.get("refs", {}).get("accepted_result") != s0_close["accepted_parent_receipt"]:
        raise ValueError("S0 index does not bind the accepted parent receipt")
    if not evaluate_source_transition_fixture(root):
        raise ValueError("accepted S0 source close set is not valid")
    s0_authority = _load_fixed_json(root, _S0_AUTHORITY_PATH, "S0 source authority")
    expected_head = dict(s0_authority.get("expected_head", {}))
    expected_head["synthetic"] = True
    if evidence.get("expected_head") != expected_head:
        raise ValueError("compatibility expected synthetic HEAD disagrees with S0")


def _verify_current_lineage_documents(root: Path, lineage: Mapping[str, Any]) -> None:
    _validate_schema(root, lineage, _CURRENT_LINEAGE_SCHEMA_PATH)
    if (
        lineage.get("schema") != "current-canonical-lineage/v1"
        or lineage.get("lineage_id") != "ai-agent-workflow-current-canonical-lineage-v2"
        or lineage.get("version") != "v2"
    ):
        raise ValueError("current canonical lineage identity is not exact")
    predecessor = lineage.get("predecessor_authority")
    if predecessor != {"path": _AUTHORITY_PATH, "digest": _CHECKED_IN_AUTHORITY_DIGEST}:
        raise ValueError("current canonical lineage predecessor is not immutable")
    if _source_digest(_source_file(root, predecessor["path"])) != predecessor["digest"]:
        raise ValueError("current canonical lineage predecessor bytes are stale")

    canonical_inputs = lineage.get("canonical_inputs")
    identities = [
        (item.get("role"), item.get("path"))
        for item in canonical_inputs
        if isinstance(item, Mapping)
    ] if isinstance(canonical_inputs, list) else []
    if identities != list(_CURRENT_CANONICAL_INPUTS):
        raise ValueError("current canonical input order or identity is not exact")
    _require_physical_refs(
        root,
        canonical_inputs,
        {path for _, path in _CURRENT_CANONICAL_INPUTS},
        "current canonical inputs",
    )

    status_ref = lineage.get("implementation_status")
    if not isinstance(status_ref, Mapping) or status_ref.get("path") != _CURRENT_STATUS_PATH:
        raise ValueError("current implementation status identity is not exact")
    _load_json(root, status_ref, "current implementation status")

    candidate = lineage.get("corrected_candidate")
    records = candidate.get("records") if isinstance(candidate, Mapping) else None
    if not isinstance(records, list) or len(records) != 18:
        raise ValueError("current corrected candidate does not have exactly 18 records")
    record_lines = []
    paths = []
    for record in records:
        if not isinstance(record, Mapping) or set(record) != {"path", "digest"}:
            raise ValueError("current corrected candidate record is malformed")
        path = record.get("path")
        digest = record.get("digest")
        physical = _source_file(root, path)
        if _source_digest(physical) != digest:
            raise ValueError("current corrected candidate digest is stale")
        paths.append(path)
        record_lines.append("%s:%s" % (path, digest))
    if paths != sorted(paths) or len(paths) != len(set(paths)):
        raise ValueError("current corrected candidate records are not canonical")
    aggregate = "sha256:" + hashlib.sha256(
        ("\n".join(record_lines) + "\n").encode("utf-8")
    ).hexdigest()
    if aggregate != _CURRENT_CANDIDATE_AGGREGATE or candidate.get("aggregate") != aggregate:
        raise ValueError("current corrected candidate aggregate is not trusted")

    inventory_path = _source_file(root, _CURRENT_INVENTORY_PATH)
    if _source_digest(inventory_path) != _CURRENT_INVENTORY_DIGEST:
        raise ValueError("current catalog inventory digest is not trusted")
    inventory = json.loads(inventory_path.read_text(encoding="utf-8"))
    if not isinstance(inventory, Mapping) or inventory.get("current_lineage") != {
        "path": _CURRENT_LINEAGE_PATH,
        "digest": _CURRENT_LINEAGE_DIGEST,
    }:
        raise ValueError("current catalog inventory lineage ref is not exact")


def verify_current_canonical_lineage(source_root: Path) -> bool:
    """Verify the reviewed current lineage without weakening historical replay."""
    try:
        root = _normalized_source_root(source_root)
        lineage_path = _source_file(root, _CURRENT_LINEAGE_PATH)
        if _source_digest(lineage_path) != _CURRENT_LINEAGE_DIGEST:
            raise ValueError("current canonical lineage digest is not trusted")
        lineage = json.loads(lineage_path.read_text(encoding="utf-8"))
        if not isinstance(lineage, Mapping):
            raise ValueError("current canonical lineage must be a JSON object")
        _verify_current_lineage_documents(root, lineage)
        return True
    except (OSError, SchemaValidationError, TypeError, ValueError, KeyError):
        return False


def verify_section_transition_compatibility(source_root: Path) -> bool:
    """Fail-closed replay using only physical bytes below ``source_root``."""
    try:
        root = _normalized_source_root(source_root)
        authority_path = _source_file(root, _AUTHORITY_PATH)
        if _source_digest(authority_path) != _AUTHORITY_DIGEST:
            raise ValueError("compatibility authority digest is not trusted")
        authority = json.loads(authority_path.read_text(encoding="utf-8"))
        if not isinstance(authority, Mapping):
            raise ValueError("compatibility authority must be a JSON object")
        _verify_bound_documents(root, authority)
        if _source_digest(_source_file(root, _S0_EVIDENCE_PATH)) != _S0_EVIDENCE_DIGEST:
            raise ValueError("S0 current evidence verifier digest is not trusted")
        if (
            _source_digest(_source_file(root, _S0_INTEGRATION_TEST_PATH))
            != _S0_INTEGRATION_TEST_DIGEST
        ):
            raise ValueError("S0 current integration test digest is not trusted")
        return True
    except (OSError, SchemaValidationError, TypeError, ValueError, KeyError):
        return False


evaluate_section_transition_compatibility = verify_section_transition_compatibility


__all__ = [
    "evaluate_section_transition_compatibility",
    "verify_current_canonical_lineage",
    "verify_section_transition_compatibility",
]
