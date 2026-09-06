"""Fail-closed publication boundary for S0-E semantic acceptance evidence."""

from __future__ import annotations

import json
import hashlib
import re
from pathlib import Path
from typing import Any, Mapping, Protocol

from .control_kernel import AuthorizationError, IntegrityBlockedError
from .schema_validation import SchemaValidationError, validate_document


_SLICES = ("S0-A", "S0-B", "S0-C", "S0-D")
_SCHEMA_PATH = Path(__file__).resolve().parents[2] / "schemas" / "section-accepted-result-v1.schema.json"
_S0_ACCEPTANCE_COMMAND = "PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=agent-workflows/src python3 -m unittest discover -s agent-workflows/tests -p 'test_*.py'"
_S0_ACCEPTANCE_TESTS_RUN = 209
_AUTHORITY_PATH = "agent-workflows/manifests/s0-source-transition-authority.json"
# This is supplied by the reviewed evaluator package, never by the close set.
_AUTHORITY_DIGEST = "sha256:673114ec3e1ea69fcd9580805b7f872895062253a0a1ccc352f40f281ccc3070"
_CURRENT_AUTHORITY_PATH = "agent-workflows/manifests/s0-source-transition-current-authority.json"
# This successor trust root is supplied by the reviewed evaluator package too.
_CURRENT_AUTHORITY_DIGEST = "sha256:88d27e7d1748bae9bc10d379fabbe3c5787dba922659986dc6e55080bff8c29f"
_CURRENT_AUTHORITY_SCHEMA_PATH = (
    "agent-workflows/schemas/s0-source-transition-current-authority-v1.schema.json"
)
_CURRENT_AUTHORITY_SCHEMA_DIGEST = (
    "sha256:9433e7fd39b26526d30887dd3ddb6493c11176c9397b34aa7418ccd60e5ea959"
)
_CURRENT_LINEAGE_REF = {
    "path": "agent-workflows/evidence/current-canonical-lineage.json",
    "digest": "sha256:347c4aa1ec8961fa820bf3122895172c041bfc1a86a4d91e783b99c3f989bdf4",
}
_CURRENT_SOURCE_REFS = [
    {
        "role": "full-implementation-plan",
        "path": "docs/plans/ai-agent-workflow-full-implementation-plan.md",
        "digest": "sha256:c8e10469fcf310e913f7520990b833e259daf5d0d3144defdcd4ba31d5b46524",
    },
    {
        "role": "step-catalog",
        "path": "docs/plans/ai-agent-workflow-step-catalog.md",
        "digest": "sha256:4ae2f11b89eedabc4c4a5cf71f96d4041c94ed58a48f6c1e75172ae648fe7c0d",
    },
]
_ROTATED_SOURCE_PATHS = {item["path"] for item in _CURRENT_SOURCE_REFS}


class EvidenceKernel(Protocol):
    run_id: str

    def read_state(self) -> Mapping[str, Any]: ...
    def read_object(self, ref_or_digest: Any) -> Mapping[str, Any]: ...
    def publish_artifact(
        self, artifact_id: str, version: str, value: Any, *, kind: str,
        authority_ref: Mapping[str, Any],
    ) -> Mapping[str, Any]: ...


def _schema() -> Mapping[str, Any]:
    try:
        return json.loads(_SCHEMA_PATH.read_text(encoding="utf-8"))
    except (OSError, ValueError) as exc:
        raise AuthorizationError("S0 accepted-result schema is unreadable") from exc


def _validate_schema(document: Mapping[str, Any]) -> None:
    schema = _schema()
    try:
        validate_document(document, schema, registry={
            "sliceRefs": schema["$defs"]["sliceRefs"],
            "digestRef": schema["$defs"]["digestRef"],
        })
    except (SchemaValidationError, KeyError) as exc:
        raise AuthorizationError("S0 accepted result is not strict-schema valid") from exc


def _artifact_payload(kernel: EvidenceKernel, digest: str, label: str) -> Mapping[str, Any]:
    state = kernel.read_state()
    ref = state.get("object_refs", {}).get(digest) if isinstance(state, Mapping) else None
    if not isinstance(ref, Mapping) or ref.get("object_type") != "artifact":
        raise AuthorizationError("%s is not a current-run immutable artifact" % label)
    try:
        object_value = kernel.read_object(ref)
    except (IntegrityBlockedError, OSError, ValueError) as exc:
        raise AuthorizationError("%s cannot be read as a current-run artifact" % label) from exc
    payload = object_value.get("payload") if isinstance(object_value, Mapping) else None
    if not isinstance(payload, Mapping):
        raise AuthorizationError("%s payload is malformed" % label)
    return payload


def _require_slice_ref(kernel: EvidenceKernel, digest: str, slice_id: str, kind: str) -> None:
    payload = _artifact_payload(kernel, digest, "%s %s" % (slice_id, kind))
    value = payload.get("payload")
    if payload.get("kind") != kind or not isinstance(value, Mapping) or value.get("slice_id") != slice_id:
        raise AuthorizationError("%s %s does not bind its accepted slice" % (slice_id, kind))


def _require_kind(kernel: EvidenceKernel, digest: str, kind: str, label: str) -> None:
    payload = _artifact_payload(kernel, digest, label)
    if payload.get("kind") != kind:
        raise AuthorizationError("%s has the wrong immutable artifact kind" % label)


def _semantic_payload(kernel: EvidenceKernel, digest: str, kind: str, label: str) -> Mapping[str, Any]:
    envelope = _artifact_payload(kernel, digest, label)
    if envelope.get("kind") != kind or not isinstance(envelope.get("payload"), Mapping):
        raise AuthorizationError("%s has the wrong immutable artifact kind or payload" % label)
    return envelope["payload"]


def _validate_review_validation(review: Mapping[str, Any], validation: Mapping[str, Any], *, run_id: str, group_id: str, review_ref: Mapping[str, Any]) -> None:
    """Validate the S0-E Finding identity, disposition, and finite-budget join."""
    for value, name in ((review, "section-review-v1"), (validation, "finding-validation-set-v1")):
        _validate_source_schema(_SCHEMA_PATH.parents[2], value, name)
        if value.get("run_id") != run_id or value.get("section_id") != "S0" or value.get("group_id") != group_id:
            raise AuthorizationError("%s is not bound to this S0 group and Run" % name)
    if review.get("verdict") != "accepted" or validation.get("review_ref") != review_ref:
        raise AuthorizationError("review and validation are not an accepted bound pair")
    findings, outcomes = review["findings"], validation["outcomes"]
    finding_ids = [item["id"] for item in findings]
    outcome_ids = [item["finding_id"] for item in outcomes]
    if len(finding_ids) != len(set(finding_ids)) or len(outcome_ids) != len(set(outcome_ids)) or set(finding_ids) != set(outcome_ids):
        raise AuthorizationError("review findings and validation outcomes must be a unique equal ID set")
    outcome_by_id = {item["finding_id"]: item["state"] for item in outcomes}
    for finding in findings:
        state, outcome = finding["state"], outcome_by_id[finding["id"]]
        if state == "required" and outcome != "closed":
            raise AuthorizationError("required Finding is not closed")
        if state == "needs-user" or outcome in {"needs-user", "open"} or state == "candidate":
            raise AuthorizationError("Finding remains open or needs user input")
        if state in {"defer", "reject"} and outcome not in {"defer", "reject"}:
            raise AuthorizationError("terminal Finding disposition disagrees with validation")
    consumed = review["review_budget"]["consumed"]
    findings_by_id = {item["id"]: item for item in findings}
    attempts_by_finding: dict[str, list[int]] = {}
    for item in consumed:
        finding_id = item["finding_id"]
        finding = findings_by_id.get(finding_id)
        if finding is None:
            raise AuthorizationError("Finding attempt history is unbound")
        if finding["state"] in {"defer", "reject"}:
            raise AuthorizationError("terminal Finding must not consume fix history")
        attempts_by_finding.setdefault(finding_id, []).append(item["attempt"])
    for attempts in attempts_by_finding.values():
        if (len(attempts) > 5 or len(attempts) != len(set(attempts))
                or sorted(attempts) != list(range(1, len(attempts) + 1))):
            raise AuthorizationError("Finding attempt history is per-Finding unique contiguous 1..N within five")


def publish_section_accepted_result(
    kernel: EvidenceKernel,
    artifact_id: str,
    version: str,
    document: Mapping[str, Any],
    *,
    authority_ref: Mapping[str, Any],
) -> Mapping[str, Any]:
    """Validate semantic S0-E evidence before one generic-kernel publication.

    The generic kernel retains object/transaction authority.  This boundary
    owns only the strict S0-E document and its current-run reference topology.
    """

    _validate_schema(document)
    if document.get("run_id") != kernel.run_id:
        raise AuthorizationError("S0 accepted result run does not match the current run")
    seen = set()
    for field, kind in (("artifact_refs", "s0-accepted-artifact"), ("result_refs", "s0-accepted-result")):
        refs = document[field]
        for slice_id in _SLICES:
            digest = refs[slice_id]["digest"]
            if digest in seen:
                raise AuthorizationError("S0 accepted result references must be distinct")
            seen.add(digest)
            _require_slice_ref(kernel, digest, slice_id, kind)
    for field, kind in (("review_ref", "section-review"), ("finding_validation_ref", "finding-validation-set")):
        digest = document[field]["digest"]
        if digest in seen:
            raise AuthorizationError("S0 accepted result references must be distinct")
        seen.add(digest)
        _require_kind(kernel, digest, kind, field)
    review_ref, validation_ref = document["review_ref"], document["finding_validation_ref"]
    review = _semantic_payload(kernel, review_ref["digest"], "section-review", "review_ref")
    validation = _semantic_payload(kernel, validation_ref["digest"], "finding-validation-set", "finding_validation_ref")
    _validate_review_validation(review, validation, run_id=kernel.run_id, group_id=document["group_id"], review_ref=review_ref)
    return kernel.publish_artifact(
        artifact_id, version, dict(document), kind="section-accepted-result",
        authority_ref=authority_ref,
    )


def close_section_group(
    kernel: EvidenceKernel, *, accepted_result_digest: str, next_group: str,
    authority_ref: Mapping[str, Any],
) -> Mapping[str, Any]:
    """Recheck immutable S0-E acceptance evidence immediately before close."""
    state = kernel.read_state()
    if accepted_result_digest not in state.get("object_refs", {}):
        raise AuthorizationError("S0 accepted result is not current immutable close evidence")
    accepted = _semantic_payload(kernel, accepted_result_digest, "section-accepted-result", "accepted result")
    _validate_schema(accepted)
    if accepted.get("run_id") != kernel.run_id:
        raise AuthorizationError("S0 accepted result Run does not match close Run")
    current_group = state.get("group", {}).get("id")
    if current_group != accepted.get("group_id"):
        raise AuthorizationError("S0 accepted result group does not match close group")
    review = _semantic_payload(kernel, accepted["review_ref"]["digest"], "section-review", "close review")
    validation = _semantic_payload(kernel, accepted["finding_validation_ref"]["digest"], "finding-validation-set", "close validation")
    _validate_review_validation(review, validation, run_id=kernel.run_id, group_id=accepted["group_id"], review_ref=accepted["review_ref"])
    return kernel.close_group(next_group=next_group, acceptance_evidence=[accepted_result_digest], authority_ref=authority_ref)


def _source_file(root: Path, relative: object) -> Path:
    if not isinstance(relative, str) or not relative or "\x00" in relative:
        raise AuthorizationError("source evidence path is malformed")
    if "\\" in relative or re.search(r"(^/|^[A-Za-z]:|//|/$|(^|/)\.{1,2}(/|$)|[*?\[\]{}])", relative):
        raise AuthorizationError("source evidence path is not strict normalized relative grammar")
    candidate = Path(relative)
    if candidate.is_absolute() or ".." in candidate.parts or "." in candidate.parts or "\\" in relative:
        raise AuthorizationError("source evidence path escapes its source root")
    raw, resolved_root = root / candidate, root.resolve()
    if raw.is_symlink():
        raise AuthorizationError("source evidence target must not be a symlink")
    resolved = raw.resolve()
    try:
        resolved.relative_to(resolved_root)
    except ValueError as exc:
        raise AuthorizationError("source evidence path escapes its source root") from exc
    if not resolved.is_file() or resolved.is_symlink():
        raise AuthorizationError("source evidence target is not a regular source file")
    return resolved


def _source_digest(path: Path) -> str:
    return "sha256:" + hashlib.sha256(path.read_bytes()).hexdigest()


def _load_source_document(root: Path, ref: Mapping[str, Any], label: str) -> Mapping[str, Any]:
    path = _source_file(root, ref.get("path"))
    if _source_digest(path) != ref.get("digest"):
        raise AuthorizationError("%s digest does not bind source bytes" % label)
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, ValueError) as exc:
        raise AuthorizationError("%s is not JSON" % label) from exc
    if not isinstance(value, Mapping):
        raise AuthorizationError("%s must be an object" % label)
    return value


def _load_source_transition_authority(root: Path, index: Mapping[str, Any]) -> Mapping[str, Any]:
    """Load the immutable source-only fixture authority from a fixed path.

    The fixed package digest intentionally sits outside the re-digestable S0
    closure.  Neither index nor bundle may nominate this root of trust.
    """
    path = _source_file(root, _AUTHORITY_PATH)
    if _source_digest(path) != _AUTHORITY_DIGEST:
        raise AuthorizationError("source transition authority digest is not trusted")
    try:
        authority = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, ValueError) as exc:
        raise AuthorizationError("source transition authority is unreadable") from exc
    if not isinstance(authority, Mapping):
        raise AuthorizationError("source transition authority must be an object")
    _validate_source_schema(root, authority, "s0-source-transition-authority-v1")
    if authority.get("fixture", {}).get("kind") != "source-only":
        raise AuthorizationError("source transition authority is not source-only")
    close_index = authority.get("close_index", {})
    if close_index != {"path": "agent-workflows/evidence/sections/S0/index.json", "digest": _source_digest(_source_file(root, "agent-workflows/evidence/sections/S0/index.json"))}:
        raise AuthorizationError("source transition authority does not bind close index bytes")
    if authority.get("fixture", {}).get("run_id") != index.get("run_id"):
        raise AuthorizationError("source transition authority is cross-run")
    for source in authority.get("sources", []):
        if not isinstance(source, Mapping):
            raise AuthorizationError("source transition authority source is malformed")
        if source.get("path") in _ROTATED_SOURCE_PATHS:
            continue
        source_path = _source_file(root, source.get("path"))
        if _source_digest(source_path) != source.get("digest"):
            raise AuthorizationError("source transition authority source bytes disagree")
    return authority


def _load_current_source_transition_authority(root: Path) -> Mapping[str, Any]:
    """Load the fixed successor that owns the two intentionally rotated sources."""
    path = _source_file(root, _CURRENT_AUTHORITY_PATH)
    if _source_digest(path) != _CURRENT_AUTHORITY_DIGEST:
        raise AuthorizationError("current source transition authority digest is not trusted")
    try:
        authority = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, ValueError) as exc:
        raise AuthorizationError("current source transition authority is unreadable") from exc
    if not isinstance(authority, Mapping):
        raise AuthorizationError("current source transition authority must be an object")
    schema_path = _source_file(root, _CURRENT_AUTHORITY_SCHEMA_PATH)
    if _source_digest(schema_path) != _CURRENT_AUTHORITY_SCHEMA_DIGEST:
        raise AuthorizationError("current source transition authority schema is not trusted")
    _validate_source_schema(root, authority, "s0-source-transition-current-authority-v1")
    if authority.get("predecessor_authority") != {
        "path": _AUTHORITY_PATH,
        "digest": _AUTHORITY_DIGEST,
    }:
        raise AuthorizationError("current authority predecessor is not the immutable S0 close")
    if authority.get("current_canonical_lineage") != _CURRENT_LINEAGE_REF:
        raise AuthorizationError("current authority lineage ref is not exact")
    if authority.get("current_sources") != _CURRENT_SOURCE_REFS:
        raise AuthorizationError("current authority source refs are not exact")

    lineage = _load_source_document(root, _CURRENT_LINEAGE_REF, "current canonical lineage")
    _validate_source_schema(root, lineage, "current-canonical-lineage-v1")
    if (
        lineage.get("schema") != "current-canonical-lineage/v1"
        or lineage.get("lineage_id") != "ai-agent-workflow-current-canonical-lineage-v2"
        or lineage.get("version") != "v2"
    ):
        raise AuthorizationError("current canonical lineage identity is not exact")
    canonical_by_role = {
        item.get("role"): item
        for item in lineage.get("canonical_inputs", [])
        if isinstance(item, Mapping)
    }
    if [canonical_by_role.get(item["role"]) for item in _CURRENT_SOURCE_REFS] != _CURRENT_SOURCE_REFS:
        raise AuthorizationError("current authority sources disagree with canonical lineage")
    for source in _CURRENT_SOURCE_REFS:
        source_path = _source_file(root, source["path"])
        if _source_digest(source_path) != source["digest"]:
            raise AuthorizationError("current authority source bytes disagree")
    return authority


def _ref_key(ref: Mapping[str, Any]) -> tuple[str, str]:
    return (str(ref.get("path")), str(ref.get("digest")))


def _validate_state_snapshot(
    snapshot: Mapping[str, Any], *, refs: Mapping[str, Any], materialized: Mapping[str, Any],
    slice_digests: list[str], slice_documents: Mapping[str, Mapping[str, Any]],
    inputs: list[Mapping[str, Any]], input_documents: Mapping[str, Mapping[str, Any]],
    review: Mapping[str, Any],
) -> None:
    """Validate recovery state independently of the external authority anchor."""
    classes = ("accepted", "invalidated", "unresolved", "next_section_inputs")
    if not all(isinstance(snapshot.get(name), list) for name in classes) or not isinstance(snapshot.get("budget"), Mapping):
        raise AuthorizationError("state snapshot omits a required recovery class")
    class_refs: dict[str, list[tuple[str, str]]] = {}
    all_snapshot_refs: list[tuple[str, str]] = []
    for class_name in classes:
        members = snapshot[class_name]
        if not all(isinstance(member, Mapping) and isinstance(member.get("identity"), str) and isinstance(member.get("ref"), Mapping) for member in members):
            raise AuthorizationError("state snapshot member is malformed")
        identities = [member["identity"] for member in members]
        refs_for_class = [_ref_key(member["ref"]) for member in members]
        if len(identities) != len(set(identities)) or len(refs_for_class) != len(set(refs_for_class)):
            raise AuthorizationError("state snapshot has duplicate member")
        class_refs[class_name] = refs_for_class
        all_snapshot_refs.extend(refs_for_class)
    if len(all_snapshot_refs) != len(set(all_snapshot_refs)):
        raise AuthorizationError("state snapshot ref occurs across classes")
    if class_refs["invalidated"] or class_refs["unresolved"]:
        raise AuthorizationError("source close fixture has contradictory non-empty terminal state")
    accepted_snapshot = {_ref_key(member["ref"]): member["identity"] for member in snapshot["accepted"]}
    expected_accepted = {
        _ref_key(refs["accepted_result"]): "accepted-result",
        _ref_key(refs["section_review"]): "section-review",
        _ref_key(refs["finding_validation"]): "finding-validation",
    }
    expected_accepted.update(
        (_ref_key(materialized[digest]), slice_documents[digest]["evidence_kind"] + "-" + slice_documents[digest]["slice_id"])
        for digest in slice_digests
    )
    if set(accepted_snapshot) != set(expected_accepted) or any(accepted_snapshot[key] != expected_accepted[key] for key in expected_accepted):
        raise AuthorizationError("accepted state snapshot disagrees with close evidence")
    next_snapshot = {_ref_key(member["ref"]): member["identity"] for member in snapshot["next_section_inputs"]}
    expected_inputs = {_ref_key(ref): input_documents[ref["digest"]]["input_id"] for ref in inputs}
    if next_snapshot != expected_inputs:
        raise AuthorizationError("next input state snapshot disagrees with close evidence")
    budget = snapshot["budget"]
    if (budget.get("review_ref") != refs["section_review"]
            or budget.get("max_fixes") != review["review_budget"]["max_fixes"]
            or budget.get("consumed") != review["review_budget"]["consumed"]):
        raise AuthorizationError("state snapshot budget disagrees with review history")


def _validate_source_schema(root: Path, document: Mapping[str, Any], name: str) -> None:
    path = root / ("agent-workflows/schemas/%s.schema.json" % name)
    schema = _load_source_document(root, {"path": str(path.relative_to(root)), "digest": _source_digest(path)}, name + " schema")
    try:
        validate_document(document, schema, registry=dict(schema.get("$defs", {})))
    except SchemaValidationError as exc:
        raise AuthorizationError("%s is not strict-schema valid" % name) from exc


def evaluate_source_transition_fixture(source_root: Path) -> bool:
    """Fail-closed caller-root-only replay of the physical S0 close set."""
    root = Path(source_root)
    index_path = root / "agent-workflows/evidence/sections/S0/index.json"
    if not index_path.is_file() or index_path.is_symlink(): return False
    try:
        index = json.loads(index_path.read_text(encoding="utf-8"))
        _validate_source_schema(root, index, "s0-source-transition-index-v1")
        _load_current_source_transition_authority(root)
        authority = _load_source_transition_authority(root, index)
        refs, documents = index["refs"], {}
        names = {"section_result":"section-result-v1", "section_review":"section-review-v1", "finding_validation":"finding-validation-set-v1", "section_bundle":"section-bundle-v1", "checkpoint":"checkpoint-v1", "accepted_result":"section-accepted-result-v1"}
        seen = set()
        for key, schema_name in names.items():
            ref = refs[key]
            if ref["path"] in seen: raise AuthorizationError("source evidence references are duplicated")
            seen.add(ref["path"])
            document = _load_source_document(root, ref, key)
            _validate_source_schema(root, document, schema_name)
            if document.get("run_id") != index["run_id"]: raise AuthorizationError("source evidence is cross-run")
            documents[key] = document
        review, validation = documents["section_review"], documents["finding_validation"]
        _validate_review_validation(review, validation, run_id=index["run_id"], group_id=review["group_id"], review_ref=refs["section_review"])
        accepted = documents["accepted_result"]
        if accepted.get("group_id") != review["group_id"] or accepted["review_ref"] != {"digest": refs["section_review"]["digest"]} or accepted["finding_validation_ref"] != {"digest": refs["finding_validation"]["digest"]}: raise AuthorizationError("accepted result binding is invalid")
        bundle = documents["section_bundle"]
        artifacts, inputs = bundle["artifacts"], bundle["next_section_inputs"]
        if len({item["path"] for item in artifacts + inputs}) != len(artifacts + inputs) or len({item["digest"] for item in artifacts + inputs}) != len(artifacts + inputs):
            raise AuthorizationError("bundle members are not globally unique")
        materialized = {item["digest"]: item for item in artifacts}
        input_documents = {}
        for item in artifacts:
            _load_source_document(root, item, "bundle artifact reference")
        for item in inputs:
            input_documents[item["digest"]] = _load_source_document(root, item, "bundle next input reference")
            _validate_source_schema(root, input_documents[item["digest"]], "s0-next-section-input-v1")
        expected = {refs[key]["digest"] for key in ("section_review", "finding_validation", "accepted_result")}
        slice_digests, slice_documents = [], {}
        for field, kind in (("artifact_refs", "artifact"), ("result_refs", "result")):
            for slice_id in _SLICES:
                digest = accepted[field][slice_id]["digest"]
                ref = materialized.get(digest)
                if ref is None: raise AuthorizationError("accepted slice is not materialized in bundle")
                document = _load_source_document(root, ref, "accepted slice")
                _validate_source_schema(root, document, "s0-slice-evidence-v1")
                expected_status = "accepted" if kind == "artifact" else "passed"
                if (document.get("run_id") != index["run_id"] or document.get("section_id") != "S0"
                        or document.get("group_id") != accepted["group_id"] or document.get("slice_id") != slice_id
                        or document.get("evidence_kind") != kind or document.get("status") != expected_status):
                    raise AuthorizationError("accepted slice identity, kind, or status is invalid")
                slice_digests.append(digest); slice_documents[digest] = document
        if len(slice_digests) != 8 or len(slice_digests) != len(set(slice_digests)):
            raise AuthorizationError("accepted S0-A through S0-D slices are not eight distinct refs")
        result = documents["section_result"]
        if (result["checkpoint_ref"]["digest"] != refs["checkpoint"]["digest"]
                or result["checkpoint_ref"]["checkpoint_id"] != documents["checkpoint"]["checkpoint_id"]
                or result["bundle_ref"]["digest"] != refs["section_bundle"]["digest"]):
            raise AuthorizationError("result is not bound to index checkpoint and bundle")
        if documents["checkpoint"]["bundle_ref"]["digest"] != refs["section_bundle"]["digest"]: raise AuthorizationError("checkpoint is not bound to bundle")
        if result["run_id"] != index["run_id"] or result["section_id"] != "S0": raise AuthorizationError("result identity is invalid")
        required = expected | set(slice_digests) | {result["plan_ref"]["digest"], result["catalog_ref"]["digest"], result["transaction_ref"]["digest"]}
        if set(materialized) != required: raise AuthorizationError("bundle is not exactly the materialized close topology")
        for key, schema_name in (("plan_ref", "s0-plan-evidence-v1"), ("catalog_ref", "s0-catalog-evidence-v1"), ("transaction_ref", "s0-transaction-evidence-v1")):
            document = _load_source_document(root, materialized[result[key]["digest"]], key)
            _validate_source_schema(root, document, schema_name)
            if document.get("run_id") != index["run_id"] or document.get("section_id") != "S0": raise AuthorizationError("result dependency identity is invalid")
        transaction = _load_source_document(root, materialized[result["transaction_ref"]["digest"]], "transaction")
        if transaction.get("expected_head") != result["expected_head"]: raise AuthorizationError("transaction does not bind result expected head")
        acceptance_test = transaction.get("acceptance_test")
        if acceptance_test != {"command": _S0_ACCEPTANCE_COMMAND, "exit_status": 0, "tests_run": _S0_ACCEPTANCE_TESTS_RUN, "status": "passed"}:
            raise AuthorizationError("transaction does not bind the successful S0 acceptance test")
        input_digests = set(input_documents)
        if not input_digests: raise AuthorizationError("next section inputs are missing")
        input_ids = [input_document["input_id"] for input_document in input_documents.values()]
        if len(input_ids) != len(set(input_ids)):
            raise AuthorizationError("next section input IDs are not unique")
        for digest, input_document in input_documents.items():
            source = input_document.get("accepted_source_ref", {}).get("digest")
            if (source not in slice_documents or input_document.get("run_id") != index["run_id"]
                    or input_document.get("section_id") != "S0"
                    or input_document.get("group_id") != accepted["group_id"]):
                raise AuthorizationError("next section input is not bound to an accepted source")
        _validate_state_snapshot(
            bundle["state_snapshot"], refs=refs, materialized=materialized,
            slice_digests=slice_digests, slice_documents=slice_documents, inputs=inputs,
            input_documents=input_documents, review=review,
        )
        canonical_findings = authority.get("canonical_findings")
        observed_findings = [
            {"id": finding["id"], "state": finding["state"], "outcome": next(outcome["state"] for outcome in validation["outcomes"] if outcome["finding_id"] == finding["id"]),
             "attempts": [item["attempt"] for item in review["review_budget"]["consumed"] if item["finding_id"] == finding["id"]]}
            for finding in review["findings"]
        ]
        if canonical_findings != observed_findings:
            raise AuthorizationError("authority canonical finding history disagrees with close evidence")
        if authority.get("expected_head") != result["expected_head"] or authority.get("expected_head") != transaction["expected_head"]:
            raise AuthorizationError("authority expected head disagrees with source fixture")
        if authority.get("checkpoint") != {"checkpoint_id": documents["checkpoint"]["checkpoint_id"], "digest": refs["checkpoint"]["digest"]}:
            raise AuthorizationError("authority checkpoint disagrees with close evidence")
        if authority.get("allowed_next_inputs") != [
            {"identity": input_document["input_id"], "digest": digest}
            for digest, input_document in input_documents.items()
        ]:
            raise AuthorizationError("authority next inputs disagree with close evidence")
        if authority.get("acceptance_test") != acceptance_test:
            raise AuthorizationError("authority acceptance receipt disagrees with transaction")
        if authority.get("claim_vector") != {"source_transition_fixture_passed": True, "actual_a7": False, "migration": False, "activation": False, "full_ready": False, "named_targets": 3, "profile_targets": 0}:
            raise AuthorizationError("authority claim vector exceeds source-only scope")
        if result.get("claim") != "source-transition-fixture-passed" or result.get("claims") != {"actual_a7":False,"activation":False,"full_ready":False}: raise AuthorizationError("section result has an impermissible claim")
        if len({edge["edge_id"] for edge in index["edges"]}) != len(index["edges"]): raise AuthorizationError("whole graph has duplicate edges")
        all_refs = {**refs}
        for digest, item in materialized.items(): all_refs["artifact:" + digest] = item
        for digest, item in ((item["digest"], item) for item in inputs): all_refs["input:" + digest] = item
        expected_digests = {item["digest"] for item in all_refs.values()}
        nodes = {node["node_id"]: node["object_ref"].get("digest") for node in index["nodes"]}
        if len(nodes) != len(index["nodes"]) or len(set(nodes.values())) != len(nodes) or None in nodes.values() or set(nodes.values()) != expected_digests:
            raise AuthorizationError("whole graph nodes do not exactly materialize the close topology")
        node_for = {digest: node for node, digest in nodes.items()}
        def edge(source: str, target: str, edge_type: str) -> tuple[str, str, str]:
            return (node_for[source], node_for[target], edge_type)
        expected_edges = {
            *(edge(refs["section_result"]["digest"], refs[key]["digest"], "requires") for key in ("checkpoint", "section_bundle")),
            *(edge(refs["section_result"]["digest"], result[key]["digest"], "requires") for key in ("plan_ref", "catalog_ref", "transaction_ref")),
            edge(refs["checkpoint"]["digest"], refs["section_bundle"]["digest"], "requires"),
            *(edge(refs["section_bundle"]["digest"], item["digest"], "produces") for item in artifacts + inputs),
            *(edge(refs["accepted_result"]["digest"], digest, "requires") for digest in [refs["section_review"]["digest"], refs["finding_validation"]["digest"], *slice_digests]),
            edge(refs["finding_validation"]["digest"], refs["section_review"]["digest"], "verdict-for"),
            *(edge(digest, input_documents[digest]["accepted_source_ref"]["digest"], "requires") for digest in input_digests),
        }
        actual_edges = {(item["from"], item["to"], item["type"]) for item in index["edges"]}
        if len(actual_edges) != len(index["edges"]) or actual_edges != expected_edges:
            raise AuthorizationError("whole graph edges are not the exact physical dependency set")
        downstream_nodes = {node: digest for node, digest in nodes.items() if digest != refs["section_result"]["digest"]}
        result_node_ids = [node["node_id"] for node in result["nodes"]]
        result_node_digests = [node["object_ref"].get("digest") for node in result["nodes"]]
        result_edge_ids = [edge["edge_id"] for edge in result["edges"]]
        result_edge_triples = [(edge["from"], edge["to"], edge["type"]) for edge in result["edges"]]
        if (len(result_node_ids) != len(set(result_node_ids))
                or len(result_node_digests) != len(set(result_node_digests))
                or len(result_edge_ids) != len(set(result_edge_ids))
                or len(result_edge_triples) != len(set(result_edge_triples))):
            raise AuthorizationError("result downstream graph contains raw duplicate records")
        result_nodes = {node["node_id"]: node["object_ref"].get("digest") for node in result["nodes"]}
        result_edges = {(item["from"], item["to"], item["type"]) for item in result["edges"]}
        downstream_edges = {item for item in expected_edges if item[0] != node_for[refs["section_result"]["digest"]]}
        if result_nodes != downstream_nodes or result_edges != downstream_edges:
            raise AuthorizationError("result downstream graph does not cross-check whole graph")
        graph = {node: [] for node in nodes}
        reverse = {node: [] for node in nodes}
        for source, target, _ in actual_edges:
            if source == target: raise AuthorizationError("graph edge is self-authorizing")
            graph[source].append(target); reverse[target].append(source)
        active, done = set(), set()
        def visit(node: str) -> None:
            if node in active: raise AuthorizationError("section result graph is cyclic")
            if node not in done:
                active.add(node)
                for child in graph.get(node, []): visit(child)
                active.remove(node); done.add(node)
        for node in graph: visit(node)
        reachable, stack = set(), [next(iter(nodes))]
        while stack:
            node = stack.pop()
            if node not in reachable:
                reachable.add(node); stack.extend(graph[node]); stack.extend(reverse[node])
        if reachable != set(nodes): raise AuthorizationError("graph has disconnected close evidence")
        return True
    except (AuthorizationError, OSError, ValueError, KeyError, TypeError): return False


__all__ = ["close_section_group", "evaluate_source_transition_fixture", "publish_section_accepted_result"]
