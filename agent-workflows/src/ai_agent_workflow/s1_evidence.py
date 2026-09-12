"""Strict caller-root validation seam for the future S1 close set."""

from __future__ import annotations

import hashlib
import json
import re
from collections.abc import Mapping
from pathlib import Path
from typing import Any, Optional

from .schema_validation import SchemaValidationError, validate_document


_SCHEMA_ROOT = Path(__file__).resolve().parents[2] / "schemas"
_DOCUMENT_SCHEMAS = {
    "section_review": "section-review-v1.schema.json",
    "finding_validation": "finding-validation-set-v1.schema.json",
    "section_bundle": "section-bundle-v1.schema.json",
    "accepted_result": "section-accepted-result-v1.schema.json",
}
_SLICE_KEYS = ("S1-A", "S1-B", "S1-C", "S1-D")
_SLICE_DOCUMENT_KEYS = {
    "schema", "section_id", "group_id", "run_id", "slice_id",
    "evidence_kind", "status",
}
_AUTHORITY_PATH = "agent-workflows/manifests/s1-source-authority.json"
_ANCHOR_AUTHORITY_SHA256 = "sha256:5ffa5ee5e5e5cb5723b44ea5441ef5f3e0fca6b62ab6b3bdef15375d9da21844"
_EXPECTED_ACCEPTANCE_TEST = {
    "command": "PYTHONDONTWRITEBYTECODE=1 python3 -m unittest discover -s agent-workflows/tests -p 'test_*.py'",
    "tests_run": 329,
    "exit_status": 0,
    "status": "passed",
}
_S1_ROOT = "agent-workflows/evidence/sections/S1/"
_S1_GRAPH_PATHS = {
    _S1_ROOT + name for name in (
        "plan.json", "catalog.json", "transaction.json",
        "artifact-S1-A.json", "artifact-S1-B.json", "artifact-S1-C.json", "artifact-S1-D.json",
        "result-S1-A.json", "result-S1-B.json", "result-S1-C.json", "result-S1-D.json",
        "review.json", "validation.json", "accepted.json", "next-S2.json",
        "bundle.json", "checkpoint.json", "section-result.json", "index.json",
    )
}
_TRUSTED_SCHEMA_PATHS = {
    "section_review": "agent-workflows/schemas/section-review-v1.schema.json",
    "finding_validation": "agent-workflows/schemas/finding-validation-set-v1.schema.json",
    "section_bundle": "agent-workflows/schemas/section-bundle-v1.schema.json",
    "accepted_result": "agent-workflows/schemas/section-accepted-result-v1.schema.json",
    "s1_authority": "agent-workflows/schemas/s1-source-authority-v1.schema.json",
}
_SUCCESSOR_SOURCE_TRANSITIONS = {
    "docs/plans/ai-agent-workflow-step-catalog.md": (
        "sha256:4ae2f11b89eedabc4c4a5cf71f96d4041c94ed58a48f6c1e75172ae648fe7c0d",
        "sha256:5bfbb5342bb8d3fbc1f85876ce588adb00eedaa4d961e41b14121603ead1da4b",
    ),
    "agent-workflows/src/ai_agent_workflow/section_transition_evidence.py": (
        "sha256:3b9f6eb5413bed2d721e19b79776a63bbe3a3ee354f481ebfb44ab1d3f145d1e",
        "sha256:9256ea68e63896132bb7d64f60812013af08e80aefdf2c903c79cbbb3cc2fbe5",
    ),
}


class S1EvidenceError(ValueError):
    """S1 close evidence is not strict, bound, and caller-root contained."""


def _object_without_duplicate_keys(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
    value: dict[str, Any] = {}
    for key, item in pairs:
        if key in value:
            raise S1EvidenceError("JSON object contains a duplicate member name")
        value[key] = item
    return value


def _contained_regular_file(root: Path, relative: object) -> Path:
    if not isinstance(relative, str) or not relative or "\x00" in relative:
        raise S1EvidenceError("evidence path is malformed")
    if "\\" in relative or re.search(
        r"(^/|^[A-Za-z]:|//|/$|(^|/)\.{1,2}(/|$)|[*?\[\]{}])", relative
    ):
        raise S1EvidenceError("evidence path is not strict normalized relative grammar")
    candidate = Path(relative)
    if candidate.is_absolute() or "." in candidate.parts or ".." in candidate.parts:
        raise S1EvidenceError("evidence path escapes caller root")
    if root.is_symlink():
        raise S1EvidenceError("caller root must not be a symlink")
    raw = root / candidate
    component = root
    for part in candidate.parts:
        component = component / part
        if component.is_symlink():
            raise S1EvidenceError("evidence path component must not be a symlink")
    resolved_root = root.resolve()
    resolved = raw.resolve()
    try:
        resolved.relative_to(resolved_root)
    except ValueError as exc:
        raise S1EvidenceError("evidence path escapes caller root") from exc
    if not resolved.is_file() or resolved.is_symlink():
        raise S1EvidenceError("evidence path is not a regular file")
    return resolved


def _load_ref(root: Path, ref: object, label: str) -> Mapping[str, Any]:
    if not isinstance(ref, Mapping) or set(ref) != {"path", "digest"}:
        raise S1EvidenceError("%s ref is not exact" % label)
    path = _contained_regular_file(root, ref.get("path"))
    raw = path.read_bytes()
    actual = "sha256:" + hashlib.sha256(raw).hexdigest()
    if actual != ref.get("digest"):
        raise S1EvidenceError("%s digest does not bind JSON bytes" % label)
    try:
        value = json.loads(raw.decode("utf-8"), object_pairs_hook=_object_without_duplicate_keys)
    except (UnicodeDecodeError, json.JSONDecodeError, S1EvidenceError) as exc:
        raise S1EvidenceError("%s is not strict JSON" % label) from exc
    if not isinstance(value, Mapping):
        raise S1EvidenceError("%s must be a JSON object" % label)
    return value


def _verify_ref_bytes(root: Path, ref: object, label: str) -> None:
    if not isinstance(ref, Mapping) or set(ref) != {"path", "digest"}:
        raise S1EvidenceError("%s ref is not exact" % label)
    raw = _contained_regular_file(root, ref.get("path")).read_bytes()
    actual = "sha256:" + hashlib.sha256(raw).hexdigest()
    accepted_transition = _SUCCESSOR_SOURCE_TRANSITIONS.get(str(ref.get("path")))
    if actual != ref.get("digest") and accepted_transition != (
        ref.get("digest"),
        actual,
    ):
        raise S1EvidenceError("%s digest does not bind bytes" % label)


def _validate_schema(document: Mapping[str, Any], filename: str) -> None:
    path = _SCHEMA_ROOT / filename
    try:
        schema = json.loads(
            path.read_text(encoding="utf-8"),
            object_pairs_hook=_object_without_duplicate_keys,
        )
        validate_document(document, schema, registry=dict(schema.get("$defs", {})))
    except (OSError, json.JSONDecodeError, S1EvidenceError, SchemaValidationError) as exc:
        raise S1EvidenceError("document is not valid against %s" % filename) from exc


def _schema_from_ref(root: Path, ref: Mapping[str, Any], expected_path: str) -> Mapping[str, Any]:
    if ref.get("path") != expected_path:
        raise S1EvidenceError("trusted schema path is not exact")
    return _load_ref(root, ref, "trusted schema")


def _validate_with_schema(document: Mapping[str, Any], schema: Mapping[str, Any]) -> None:
    try:
        validate_document(document, schema, registry=dict(schema.get("$defs", {})))
    except SchemaValidationError as exc:
        raise S1EvidenceError("document is not valid against trusted schema") from exc


def _validate_findings(
    review: Mapping[str, Any], validation: Mapping[str, Any], review_ref: Mapping[str, Any]
) -> None:
    if review.get("verdict") != "accepted" or validation.get("review_ref") != review_ref:
        raise S1EvidenceError("review and validation are not an accepted bound pair")
    findings = review["findings"]
    outcomes = validation["outcomes"]
    finding_ids = [item["id"] for item in findings]
    outcome_ids = [item["finding_id"] for item in outcomes]
    if (
        len(finding_ids) != len(set(finding_ids))
        or len(outcome_ids) != len(set(outcome_ids))
        or set(finding_ids) != set(outcome_ids)
    ):
        raise S1EvidenceError("review and validation Finding sets are not exact")
    outcome_by_id = {item["finding_id"]: item["state"] for item in outcomes}
    by_finding: dict[str, list[int]] = {}
    finding_id_set = set(finding_ids)
    for attempt in review["review_budget"]["consumed"]:
        finding_id = attempt["finding_id"]
        if finding_id not in finding_id_set:
            raise S1EvidenceError("Finding attempt is not bound to the review")
        by_finding.setdefault(finding_id, []).append(attempt["attempt"])
    for attempts in by_finding.values():
        if (
            len(attempts) > 5
            or len(attempts) != len(set(attempts))
            or attempts != list(range(1, len(attempts) + 1))
        ):
            raise S1EvidenceError("Finding attempts are not contiguous unique 1..N")
    for finding in findings:
        state = finding["state"]
        outcome = outcome_by_id[finding["id"]]
        if state == "required" and (outcome != "closed" or not by_finding.get(finding["id"])):
            raise S1EvidenceError("required Finding remains open")
        if state in {"candidate", "needs-user"} or outcome in {"open", "needs-user"}:
            raise S1EvidenceError("Finding remains non-terminal")
        if state == "defer" and outcome != "defer":
            raise S1EvidenceError("terminal Finding dispositions disagree")
        if state == "reject" and outcome != "reject":
            raise S1EvidenceError("terminal Finding dispositions disagree")


def _validate_slice_document(
    document: Mapping[str, Any], *, section_id: str, group_id: str,
    run_id: str, slice_id: str, kind: str,
) -> None:
    status = "accepted" if kind == "artifact" else "passed"
    if set(document) != _SLICE_DOCUMENT_KEYS or document != {
        "schema": "s1-slice-evidence/v1",
        "section_id": section_id,
        "group_id": group_id,
        "run_id": run_id,
        "slice_id": slice_id,
        "evidence_kind": kind,
        "status": status,
    }:
        raise S1EvidenceError("slice document identity, kind, status, or shape is invalid")


def evaluate_s1_close_documents(
    source_root: Path,
    refs: Mapping[str, Mapping[str, Any]],
    *,
    expected_section_id: str,
    expected_group_id: str,
    expected_run_id: str,
    _schema_documents: Optional[Mapping[str, Mapping[str, Any]]] = None,
) -> bool:
    """Validate only the reusable S1 close-document boundary.

    No fixed authority digest is consulted here.  A later Finalizer and Anchor
    may bind this seam to root-issued manifest bytes after the close graph exists.
    """

    if expected_section_id != "S1" or not expected_group_id or not expected_run_id:
        return False
    try:
        if not isinstance(refs, Mapping) or set(refs) != set(_DOCUMENT_SCHEMAS):
            raise S1EvidenceError("close-document ref set is not exact")
        root = Path(source_root)
        documents = {
            name: _load_ref(root, refs[name], name)
            for name in _DOCUMENT_SCHEMAS
        }
        for name, filename in _DOCUMENT_SCHEMAS.items():
            if _schema_documents is None:
                _validate_schema(documents[name], filename)
            else:
                _validate_with_schema(documents[name], _schema_documents[name])

        review = documents["section_review"]
        validation = documents["finding_validation"]
        bundle = documents["section_bundle"]
        accepted = documents["accepted_result"]
        for value in (review, validation, accepted):
            if (
                value.get("section_id") != expected_section_id
                or value.get("group_id") != expected_group_id
                or value.get("run_id") != expected_run_id
            ):
                raise S1EvidenceError("close document is cross-section, group, or Run")
        if bundle.get("section_id") != expected_section_id or bundle.get("run_id") != expected_run_id:
            raise S1EvidenceError("bundle is cross-section or Run")

        _validate_findings(review, validation, refs["section_review"])
        if accepted.get("review_ref") != {"digest": refs["section_review"]["digest"]}:
            raise S1EvidenceError("accepted result does not bind section review")
        if accepted.get("finding_validation_ref") != {
            "digest": refs["finding_validation"]["digest"]
        }:
            raise S1EvidenceError("accepted result does not bind Finding validation")

        artifact_refs = bundle["artifacts"]
        input_refs = bundle["next_section_inputs"]
        all_bundle_refs = [*artifact_refs, *input_refs]
        paths = [item["path"] for item in all_bundle_refs]
        digests = [item["digest"] for item in all_bundle_refs]
        if len(paths) != len(set(paths)) or len(digests) != len(set(digests)):
            raise S1EvidenceError("bundle path and digest mappings must be unique")
        materialized = {
            item["digest"]: _load_ref(root, item, "bundle member")
            for item in all_bundle_refs
        }
        for name in ("section_review", "finding_validation", "accepted_result"):
            if refs[name]["digest"] not in materialized:
                raise S1EvidenceError("bundle omits a required close document")

        slice_digests: list[str] = []
        for field, kind in (("artifact_refs", "artifact"), ("result_refs", "result")):
            slice_map = accepted[field]
            if set(slice_map) != set(_SLICE_KEYS):
                raise S1EvidenceError("accepted result slice set is not exact")
            for slice_id in _SLICE_KEYS:
                digest_value = slice_map[slice_id]["digest"]
                slice_digests.append(digest_value)
                document = materialized.get(digest_value)
                if document is None:
                    raise S1EvidenceError("accepted slice is not materialized")
                _validate_slice_document(
                    document, section_id=expected_section_id,
                    group_id=expected_group_id, run_id=expected_run_id,
                    slice_id=slice_id, kind=kind,
                )
        if len(slice_digests) != 8 or len(slice_digests) != len(set(slice_digests)):
            raise S1EvidenceError("artifact/result slice digests are not eight distinct values")
        if set(slice_digests) & {
            refs["section_review"]["digest"], refs["finding_validation"]["digest"],
            refs["accepted_result"]["digest"], refs["section_bundle"]["digest"],
        }:
            raise S1EvidenceError("slice digest is cross-bound to a close document")
        return True
    except (KeyError, OSError, TypeError, ValueError, S1EvidenceError):
        return False


def evaluate_s1_authority_candidate(
    source_root: Path,
    authority_ref: Mapping[str, Any],
    *,
    expected_canonical_findings: object = None,
) -> bool:
    """Validate disposable, digest-bound authority bytes before anchoring."""

    try:
        root = Path(source_root)
        authority = _load_ref(root, authority_ref, "S1 authority candidate")
        trusted_refs = authority.get("trusted_schemas")
        if not isinstance(trusted_refs, Mapping) or set(trusted_refs) != set(_TRUSTED_SCHEMA_PATHS):
            raise S1EvidenceError("trusted schema set is not exact")
        schemas = {
            name: _schema_from_ref(root, trusted_refs[name], expected)
            for name, expected in _TRUSTED_SCHEMA_PATHS.items()
        }
        _validate_with_schema(authority, schemas["s1_authority"])

        external_groups = [{"accepted_s1_plan": authority["accepted_s1_plan"]},
                           authority["current_inputs"], authority["s0_parent"],
                           authority["compatibility"]]
        for group in external_groups:
            for label, ref in group.items():
                _verify_ref_bytes(root, ref, label)
        expected_contracts = {
            *("group.A." + value for value in ("A1", "A2", "A3", "A4", "A5", "A6", "A6R", "A7")),
            *("group.F.F" + str(index) for index in range(1, 9)),
        }
        if set(authority["contracts"]) != expected_contracts:
            raise S1EvidenceError("contract binding set is not exact")
        for contract_id, binding in authority["contracts"].items():
            _verify_ref_bytes(root, binding["source"], contract_id + " source")
            evidence = _load_ref(root, binding["evidence"], contract_id + " evidence")
            evidence_id = evidence.get("contract_id")
            if evidence_id in {"A6", "A6R", "A7"}:
                evidence_id = "group.A." + evidence_id
            if (
                evidence_id != contract_id
                or evidence.get("implementation_ref") != binding["source"]
                or evidence.get("test_ref", {}).get("selector") != binding["selector"]
                or evidence.get("status") != "passed"
            ):
                raise S1EvidenceError("contract source/evidence/selector binding is false")

        index_ref = authority["close_index"]
        if index_ref.get("path") != _S1_ROOT + "index.json":
            raise S1EvidenceError("close index path is not exact")
        index = _load_ref(root, index_ref, "S1 close index")
        if index.get("schema") != "s1-source-index/v1" or index.get("section_id") != "S1":
            raise S1EvidenceError("close index identity is invalid")
        refs = index.get("documents")
        if not isinstance(refs, list):
            raise S1EvidenceError("close index documents are absent")
        indexed_paths = [item.get("path") for item in refs]
        indexed_digests = [item.get("digest") for item in refs]
        if (
            set(indexed_paths) != _S1_GRAPH_PATHS - {_S1_ROOT + "index.json"}
            or len(indexed_paths) != 18
            or len(indexed_paths) != len(set(indexed_paths))
            or len(indexed_digests) != len(set(indexed_digests))
            or _AUTHORITY_PATH in indexed_paths
            or __file__ in indexed_paths
        ):
            raise S1EvidenceError("close graph membership is not exact and distinct")
        documents = {ref["path"]: _load_ref(root, ref, "S1 graph node") for ref in refs}
        edges = index.get("edges")
        expected_edges = [
            {"from": "index", "to": path, "type": "contains"}
            for path in sorted(indexed_paths)
        ]
        if edges != expected_edges:
            raise S1EvidenceError("close graph is not the exact reachable acyclic star")

        keyed_refs = {ref["path"]: ref for ref in refs}
        close_refs = {
            "section_review": keyed_refs[_S1_ROOT + "review.json"],
            "finding_validation": keyed_refs[_S1_ROOT + "validation.json"],
            "section_bundle": keyed_refs[_S1_ROOT + "bundle.json"],
            "accepted_result": keyed_refs[_S1_ROOT + "accepted.json"],
        }
        for key, schema_name in (
            ("section_review", "section_review"), ("finding_validation", "finding_validation"),
            ("section_bundle", "section_bundle"), ("accepted_result", "accepted_result"),
        ):
            _validate_with_schema(documents[close_refs[key]["path"]], schemas[schema_name])
        fixture = authority["fixture"]
        if not evaluate_s1_close_documents(
            root, close_refs, expected_section_id="S1", expected_group_id=fixture["group_id"],
            expected_run_id=fixture["run_id"], _schema_documents={
                "section_review": schemas["section_review"],
                "finding_validation": schemas["finding_validation"],
                "section_bundle": schemas["section_bundle"],
                "accepted_result": schemas["accepted_result"],
            },
        ):
            raise S1EvidenceError("close document boundary is invalid")

        transaction = documents[_S1_ROOT + "transaction.json"]
        checkpoint = documents[_S1_ROOT + "checkpoint.json"]
        next_input = documents[_S1_ROOT + "next-S2.json"]
        section_result = documents[_S1_ROOT + "section-result.json"]
        plan_ref = keyed_refs[_S1_ROOT + "plan.json"]
        catalog_ref = keyed_refs[_S1_ROOT + "catalog.json"]
        transaction_ref = keyed_refs[_S1_ROOT + "transaction.json"]
        bundle_ref = keyed_refs[_S1_ROOT + "bundle.json"]
        checkpoint_ref = keyed_refs[_S1_ROOT + "checkpoint.json"]
        plan = documents[plan_ref["path"]]
        catalog = documents[catalog_ref["path"]]
        expected_checkpoint_id = "s1-source-closure-cp-2026-09-05"
        if (
            plan != {
                "schema": "s1-plan-evidence/v1", "section_id": "S1",
                "run_id": fixture["run_id"], "source_ref": authority["accepted_s1_plan"],
                "status": "accepted",
            }
            or catalog != {
                "schema": "s1-catalog-evidence/v1", "section_id": "S1",
                "run_id": fixture["run_id"],
                "source_ref": authority["current_inputs"]["step_catalog"],
                "status": "accepted",
            }
            or authority["acceptance_test"] != _EXPECTED_ACCEPTANCE_TEST
            or transaction != {
                "schema": "s1-transaction-evidence/v1", "section_id": "S1",
                "run_id": fixture["run_id"], "s0_parent": authority["s0_parent"],
                "expected_head": authority["expected_head"],
                "acceptance_test": _EXPECTED_ACCEPTANCE_TEST,
                "provenance": {
                    "checkpoint_digest": "sha256:8c7d31885a490529b1072ec0a74d6b9a8ea6cb3e50488f3b1fa2bfec3e48598c",
                    "kind": "fresh-disposable-s1-lifecycle-fixture",
                    "source": "LifecycleIntegrationTests.setUp",
                },
            }
            or checkpoint != {
                "schema": "checkpoint/v1", "workflow_version": "workflow/v1",
                "run_id": fixture["run_id"], "group_id": fixture["group_id"],
                "epoch_id": "S1-E-finalizer", "checkpoint_id": expected_checkpoint_id,
                "closure_revision": 19, "state_revision": 19,
                "clear_before_start": True,
                "bundle_ref": {"digest": bundle_ref["digest"]},
                "state_ref": {
                    "run_id": fixture["run_id"], "revision": 19, "state_revision": 19,
                },
            }
            or authority["checkpoint"] != {
                "checkpoint_id": expected_checkpoint_id, "digest": checkpoint_ref["digest"],
            }
            or authority["allowed_next_input"] != {
                "identity": "next-S2", "ref": keyed_refs[_S1_ROOT + "next-S2.json"]
            }
            or next_input != {
                "schema": "s1-next-section-input/v1", "section_id": "S1",
                "run_id": fixture["run_id"], "input_id": "next-S2",
                "status": "ready-source-only",
            }
            or section_result != {
                "schema": "section-result/v1", "workflow_id": "workflow/v1",
                "section_id": "S1", "run_id": fixture["run_id"],
                "expected_head": authority["expected_head"],
                "plan_ref": {"digest": plan_ref["digest"]},
                "catalog_ref": {"digest": catalog_ref["digest"]},
                "transaction_ref": {"digest": transaction_ref["digest"]},
                "bundle_ref": {"digest": bundle_ref["digest"]},
                "checkpoint_ref": {
                    "checkpoint_id": expected_checkpoint_id, "digest": checkpoint_ref["digest"],
                },
                "claim": "source-transition-fixture-passed",
                "claims": {"actual_a7": False, "activation": False, "full_ready": False},
                "nodes": [], "edges": [], "recovery": {"kind": "replay"},
                "rollback": {"kind": "source-only"},
            }
        ):
            raise S1EvidenceError("transaction/checkpoint/next/claim bindings are false")
        findings = authority["canonical_findings"]
        finding_ids = [finding["id"] for finding in findings]
        fingerprints = [finding["fingerprint"] for finding in findings]
        if len(finding_ids) != len(set(finding_ids)):
            raise S1EvidenceError("authority Finding IDs are not unique")
        if len(fingerprints) != len(set(fingerprints)):
            raise S1EvidenceError("authority Finding fingerprints are not unique")
        for finding in findings:
            attempts = finding["attempts"]
            if finding["state"] == "closed" and (not attempts or attempts != list(range(1, len(attempts) + 1))):
                raise S1EvidenceError("authority Finding attempts are not contiguous unique 1..N")
            if finding["state"] in {"defer", "reject"} and attempts:
                raise S1EvidenceError("terminal non-fix Finding must have no attempts")
        review = documents[_S1_ROOT + "review.json"]
        attempts_by_id: dict[str, list[int]] = {}
        for item in review["review_budget"]["consumed"]:
            attempts_by_id.setdefault(item["finding_id"], []).append(item["attempt"])
        review_states = {item["id"]: item["state"] for item in review["findings"]}
        if [item["id"] for item in findings] != [item["id"] for item in review["findings"]]:
            raise S1EvidenceError("canonical Finding order/history is not bound")
        for item in findings:
            expected_state = "required" if item["state"] == "closed" else item["state"]
            if review_states[item["id"]] != expected_state or attempts_by_id.get(item["id"], []) != item["attempts"]:
                raise S1EvidenceError("canonical Finding state/attempt history is false")
        if expected_canonical_findings is not None:
            if not isinstance(expected_canonical_findings, list) or findings != expected_canonical_findings:
                raise S1EvidenceError("authority canonical Findings differ from caller expectation")
        return True
    except (KeyError, OSError, TypeError, ValueError, S1EvidenceError):
        return False


def verify_s1_source_evidence(source_root: Path) -> bool:
    """Verify the checked-in authority only after a separate Anchor pins it."""

    if not re.fullmatch(r"sha256:[0-9a-f]{64}", _ANCHOR_AUTHORITY_SHA256):
        return False
    return evaluate_s1_authority_candidate(
        Path(source_root), {"path": _AUTHORITY_PATH, "digest": _ANCHOR_AUTHORITY_SHA256}
    )


__all__ = [
    "S1EvidenceError", "evaluate_s1_authority_candidate", "evaluate_s1_close_documents",
    "verify_s1_source_evidence",
]
