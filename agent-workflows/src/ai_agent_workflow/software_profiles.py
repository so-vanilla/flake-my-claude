"""Pure source compiler for the three software-profile overlays.

The profiles add purpose-specific evidence to an already composed common
workflow.  They do not copy a Group body, run a command, update HEAD, approve
a decision, or claim that the user's objective was achieved.
"""

from __future__ import annotations

import copy
import hashlib
import json
import re
from collections.abc import Mapping, Sequence
from pathlib import Path
from typing import Any, Callable, Optional

from .schema_validation import SchemaValidationError, validate_document
from .workflow_composition import WorkflowCompositionError, WorkflowCompositionV1


class SoftwareProfileError(ValueError):
    """The compiler source or a requested physical binding is malformed."""


class _Refusal(ValueError):
    pass


_DEFAULT_SOURCE_ROOT = Path(__file__).resolve().parents[3]
_SHA256 = re.compile(r"^sha256:[0-9a-f]{64}$")
_SELECTORS = {
    "feature": tuple("profile.feature.F%d" % number for number in range(1, 9)),
    "bug-fix": tuple("profile.bug-fix.BGF%d" % number for number in range(1, 9)),
    "improvement": tuple("profile.improvement.I%d" % number for number in range(1, 8)),
}
_HOSTS = {
    "feature": ("group.D.D4", "group.D.D4", "group.D.D4", "group.D.D6", "group.E.E3", "group.E.E3", "group.E.E3", "group.E.E9"),
    "bug-fix": ("group.D.D2", "group.D.D2", "group.D.D2", "group.D.D2", "group.D.D2", "group.D.D5", "group.E.E3", "group.E.E9"),
    "improvement": ("group.D.D2", "group.D.D2", "group.D.D2", "group.D.D6", "group.E.E3", "group.E.E9", "group.E.E9"),
}
_RESULT_KINDS = {
    "profile.feature.F1": "feature-user-behavior-candidate",
    "profile.feature.F2": "feature-acceptance-examples-candidate",
    "profile.feature.F3": "feature-ux-api-contract-candidate",
    "profile.feature.F4": "feature-architecture-fit-candidate",
    "profile.feature.F5": "feature-vertical-slice-candidate",
    "profile.feature.F6": "feature-behavior-tdd-candidate",
    "profile.feature.F7": "feature-integration-candidate",
    "profile.feature.F8": "feature-acceptance-e2e-candidate",
    "profile.bug-fix.BGF1": "bug-symptom-candidate",
    "profile.bug-fix.BGF2": "bug-reproduction-candidate",
    "profile.bug-fix.BGF3": "bug-evidence-inventory-candidate",
    "profile.bug-fix.BGF4": "bug-hypotheses-candidate",
    "profile.bug-fix.BGF5": "bug-root-cause-candidate",
    "profile.bug-fix.BGF6": "bug-fix-options-candidate",
    "profile.bug-fix.BGF7": "bug-regression-candidate",
    "profile.bug-fix.BGF8": "bug-impact-verification-candidate",
    "profile.improvement.I1": "improvement-baseline-candidate",
    "profile.improvement.I2": "improvement-hypothesis-candidate",
    "profile.improvement.I3": "improvement-guardrails-candidate",
    "profile.improvement.I4": "improvement-change-design-candidate",
    "profile.improvement.I5": "improvement-execution-receipt-candidate",
    "profile.improvement.I6": "improvement-comparison-candidate",
    "profile.improvement.I7": "improvement-decision-candidate",
}
_APPROVALS = {
    "profile.feature.F4": ("responsibility-seam-migration", "design_decision"),
    "profile.bug-fix.BGF6": ("fix-option", "chosen_option_id"),
    "profile.improvement.I4": ("improvement-change", "change_id"),
}
_ACCEPTED_UPSTREAM = {
    "catalog": ("agent-workflows/catalog.yaml", "sha256:2fd1caaddf4509d2f56bd31a68b062290e70f2f2da33facf1d8187d476bb3f1d"),
    "workflow_composition": ("agent-workflows/src/ai_agent_workflow/workflow_composition.py", "sha256:07562ee404381b9d40c7e7ef9e22f20af068c4e5b96461134e3684ee2cfb4fe6"),
    "planning_system": ("agent-workflows/src/ai_agent_workflow/planning_system.py", "sha256:a99dca831deb2055ae920d2bc34daa3e86b8e0d60ba9022e2335f1b924c41660"),
    "execution_group": ("agent-workflows/src/ai_agent_workflow/execution_group.py", "sha256:a6718485618f2d9d5f72b82b20d797c181829d0e990adfbc9e148765608fd2d2"),
    "planning_manifest": ("agent-workflows/groups/planning.json", "sha256:754f7ac80f6f563df34757e058a7a5aa12e818db318e2ee852493a4702bed3e5"),
    "execution_manifest": ("agent-workflows/groups/execution.json", "sha256:2977b487dcc6a93ae5c7edb1dfb4e07f04a813a3c3d95d68e04923f0201a6e52"),
    "step_catalog": ("docs/plans/ai-agent-workflow-step-catalog.md", "sha256:4ae2f11b89eedabc4c4a5cf71f96d4041c94ed58a48f6c1e75172ae648fe7c0d"),
}


def _digest(value: Any) -> str:
    data = json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=True).encode()
    return "sha256:" + hashlib.sha256(data).hexdigest()


def _bytes_digest(value: bytes) -> str:
    return "sha256:" + hashlib.sha256(value).hexdigest()


def _require(condition: bool, code: str) -> None:
    if not condition:
        raise _Refusal(code)


def _nonempty(value: Any) -> bool:
    return isinstance(value, str) and bool(value.strip()) and value.strip().lower() not in {
        "todo", "tbd", "placeholder", "unknown", "unresolved", "<unknown>"
    }


def _exact(value: Any, keys: set[str], code: str) -> Mapping[str, Any]:
    _require(isinstance(value, Mapping) and set(value) == keys, code)
    return value


def _strings(value: Any, code: str, *, allow_empty: bool = False) -> list[str]:
    _require(isinstance(value, list) and (allow_empty or bool(value)), code)
    _require(all(_nonempty(item) for item in value), code)
    return value


def _head(value: Any) -> dict[str, Any]:
    _require(isinstance(value, Mapping) and set(value) == {"revision", "transaction_digest"}, "blocked_malformed_head")
    _require(isinstance(value["revision"], int) and not isinstance(value["revision"], bool) and value["revision"] >= 0, "blocked_malformed_head")
    _require(isinstance(value["transaction_digest"], str) and _SHA256.fullmatch(value["transaction_digest"]) is not None, "blocked_malformed_head")
    return copy.deepcopy(dict(value))


class SoftwareProfileV1:
    """Compile one profile artifact or an ordered profile composition candidate."""

    def __init__(
        self,
        *,
        source_root: Optional[Path] = None,
        reference_root: Optional[Path] = None,
        schema_path: Optional[Path] = None,
    ) -> None:
        self.source_root = Path(source_root or _DEFAULT_SOURCE_ROOT).resolve()
        self.reference_root = Path(reference_root or self.source_root).resolve()
        path = Path(schema_path or self.source_root / "agent-workflows/schemas/software-profile-result-v1.schema.json")
        try:
            self.schema = json.loads(path.read_text(encoding="utf-8"))
            self.host_schemas = {
                "planning-system-artifact/v1": json.loads((self.source_root / "agent-workflows/schemas/planning-system-v1.schema.json").read_text(encoding="utf-8")),
                "execution-group-candidate/v1": json.loads((self.source_root / "agent-workflows/schemas/execution-group-v1.schema.json").read_text(encoding="utf-8")),
            }
        except (OSError, json.JSONDecodeError) as error:
            raise SoftwareProfileError("cannot read software profile schema: %s" % error) from error

    @staticmethod
    def selectors(profile: str) -> tuple[str, ...]:
        if profile not in _SELECTORS:
            raise SoftwareProfileError("unknown software profile %s" % profile)
        return _SELECTORS[profile]

    def _upstream_refs(self) -> dict[str, dict[str, str]]:
        refs: dict[str, dict[str, str]] = {}
        for name, (relative, accepted) in _ACCEPTED_UPSTREAM.items():
            try:
                actual = _bytes_digest((self.source_root / relative).read_bytes())
            except OSError as error:
                raise _Refusal("blocked_upstream_unavailable:%s" % name) from error
            _require(actual == accepted, "blocked_upstream_digest_drift:%s" % name)
            refs[name] = {"path": relative, "digest": actual}
        return refs

    def _load_ref(self, value: Any, *, version: Optional[str] = None) -> tuple[dict[str, Any], dict[str, str]]:
        ref = _exact(value, {"path", "version", "digest"}, "blocked_malformed_physical_ref")
        _require(_nonempty(ref["path"]) and _nonempty(ref["version"]), "blocked_malformed_physical_ref")
        _require(isinstance(ref["digest"], str) and _SHA256.fullmatch(ref["digest"]) is not None, "blocked_malformed_physical_ref")
        relative = Path(ref["path"])
        _require(not relative.is_absolute() and ".." not in relative.parts, "blocked_unsafe_physical_ref")
        resolved: Optional[Path] = None
        for root in dict.fromkeys((self.reference_root, self.source_root)):
            candidate = (root / relative).resolve()
            try:
                candidate.relative_to(root)
            except ValueError:
                continue
            if candidate.is_file():
                resolved = candidate
                break
        _require(resolved is not None, "blocked_missing_physical_ref:%s" % ref["path"])
        try:
            raw = resolved.read_bytes()
            document = json.loads(raw.decode("utf-8"))
        except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
            raise _Refusal("blocked_unreadable_physical_ref:%s" % ref["path"]) from error
        _require(_bytes_digest(raw) == ref["digest"], "blocked_digest_drift:%s" % ref["path"])
        _require(isinstance(document, Mapping) and document.get("schema") == ref["version"], "blocked_ref_version_mismatch:%s" % ref["path"])
        if version is not None:
            _require(ref["version"] == version, "blocked_ref_kind_substitution:%s" % ref["path"])
        return copy.deepcopy(dict(document)), copy.deepcopy(dict(ref))

    def _authority(self, value: Any) -> tuple[dict[str, Any], str]:
        authority = _exact(value, {"authority_ref", "namespace", "scope", "owner_ref"}, "blocked_malformed_authority")
        _require(_nonempty(authority["namespace"]), "blocked_malformed_authority")
        _require(authority["scope"] in {"software-profile-candidate", "candidate-generic", "fixture-only"}, "blocked_authority_scope")
        self._load_ref(authority["authority_ref"])
        self._load_ref(authority["owner_ref"])
        copied = copy.deepcopy(dict(authority))
        return copied, _digest(copied)

    def _refs(self, values: Any, *, allow_empty: bool = False) -> tuple[list[dict[str, str]], dict[tuple[str, str, str], dict[str, Any]]]:
        _require(isinstance(values, list) and (allow_empty or bool(values)), "blocked_missing_evidence_refs")
        refs: list[dict[str, str]] = []
        documents: dict[tuple[str, str, str], dict[str, Any]] = {}
        for value in values:
            document, ref = self._load_ref(value)
            key = (ref["path"], ref["version"], ref["digest"])
            _require(key not in documents, "blocked_duplicate_physical_ref")
            refs.append(ref)
            documents[key] = document
        return refs, documents

    @staticmethod
    def _doc(ref: Any, documents: Mapping[tuple[str, str, str], dict[str, Any]], code: str) -> dict[str, Any]:
        _require(isinstance(ref, Mapping), code)
        key = (ref.get("path"), ref.get("version"), ref.get("digest"))
        _require(key in documents, code)
        return documents[key]

    def _host(
        self,
        selector: str,
        ref_value: Any,
        authority: Mapping[str, Any],
        expected_head: Mapping[str, Any],
    ) -> tuple[dict[str, str], str]:
        profile = selector.split(".")[1]
        index = _SELECTORS[profile].index(selector)
        host = _HOSTS[profile][index]
        expected_schema = "planning-system-artifact/v1" if host.startswith("group.D") else "execution-group-candidate/v1"
        document, ref = self._load_ref(ref_value, version=expected_schema)
        try:
            schema = self.host_schemas[expected_schema]
            validate_document(document, schema, schema.get("$defs", {}))
        except SchemaValidationError as error:
            raise _Refusal("blocked_malformed_host_receipt:%s" % error) from error
        _require(document.get("qualified_id") == host, "blocked_wrong_host")
        _require(document.get("expected_head") == expected_head, "blocked_stale_host_head")
        if expected_schema == "planning-system-artifact/v1":
            _require(document.get("authority") == authority, "blocked_stale_host_authority")
            _require(isinstance(document.get("input_refs"), list) and document["input_refs"], "blocked_host_missing_physical_refs")
            for item in document["input_refs"]:
                self._load_ref(item)
        else:
            unsigned = {key: copy.deepcopy(value) for key, value in document.items() if key != "candidate_digest"}
            _require(document.get("candidate_digest") == _digest(unsigned), "blocked_host_candidate_digest")
        return ref, host

    def _predecessors(
        self,
        selector: str,
        values: Any,
        authority_digest: str,
        expected_head: Mapping[str, Any],
    ) -> list[dict[str, str]]:
        profile = selector.split(".")[1]
        expected = _SELECTORS[profile][:_SELECTORS[profile].index(selector)]
        _require(isinstance(values, list) and len(values) == len(expected), "blocked_missing_or_extra_predecessor")
        refs: list[dict[str, str]] = []
        for index, (value, expected_selector) in enumerate(zip(values, expected)):
            document, ref = self._load_ref(value, version="software-profile-candidate/v1")
            _require(document.get("profile") == profile, "blocked_cross_profile_artifact")
            _require(document.get("qualified_id") == expected_selector, "blocked_reordered_predecessor")
            _require(document.get("expected_head") == expected_head, "blocked_stale_predecessor_head")
            _require(document.get("authority_digest") == authority_digest, "blocked_stale_predecessor_authority")
            _require(document.get("host_selector") == _HOSTS[profile][index], "blocked_predecessor_host_substitution")
            unsigned = {key: copy.deepcopy(item) for key, item in document.items() if key != "candidate_digest"}
            _require(document.get("candidate_digest") == _digest(unsigned), "blocked_predecessor_candidate_digest")
            prefix = document.get("predecessor_artifact_refs", [])
            _require(prefix == refs, "blocked_predecessor_chain")
            self._recompile_candidate(document)
            refs.append(ref)
        return refs

    def _recompile_candidate(self, document: Mapping[str, Any]) -> None:
        approval = document.get("approval")
        _require(isinstance(approval, Mapping), "blocked_persisted_candidate_approval")
        human_approval_ref = approval.get("receipt_ref") if approval.get("status") == "validated-human-receipt" else None
        inputs = {
            "profile": document.get("profile"),
            "host_receipt_ref": document.get("host_receipt_ref"),
            "predecessor_artifact_refs": document.get("predecessor_artifact_refs"),
            "evidence_refs": document.get("evidence_refs"),
            "evidence": document.get("evidence"),
            "human_approval_ref": human_approval_ref,
        }
        recompiled = self.compile(
            document.get("qualified_id"), inputs, document.get("authority"), document.get("expected_head")
        )
        _require(
            recompiled.get("schema") == "software-profile-candidate/v1",
            "blocked_persisted_candidate_semantics:%s" % recompiled.get("reason", "invalid"),
        )
        _require(recompiled == document, "blocked_persisted_candidate_replay_mismatch")

    def _approval(
        self,
        selector: str,
        value: Any,
        evidence: Mapping[str, Any],
        expected_head: Mapping[str, Any],
    ) -> dict[str, Any]:
        if selector not in _APPROVALS:
            _require(value is None, "blocked_unexpected_approval")
            return {"status": "not-applicable", "actor_id": None, "scope": None, "decision": None, "receipt_ref": None, "granted": False}
        scope, decision_field = _APPROVALS[selector]
        document, ref = self._load_ref(value, version="human-profile-approval/v1")
        required = {"schema", "profile", "qualified_id", "scope", "actor", "decision", "approved", "expected_head"}
        _require(set(document) == required, "blocked_malformed_human_approval")
        actor = document.get("actor")
        _require(isinstance(actor, Mapping) and set(actor) == {"type", "id"} and actor.get("type") == "human" and _nonempty(actor.get("id")), "blocked_nonhuman_approval")
        _require(document.get("profile") == selector.split(".")[1] and document.get("qualified_id") == selector, "blocked_approval_scope")
        _require(document.get("scope") == scope and document.get("decision") == evidence.get(decision_field), "blocked_approval_decision")
        _require(document.get("approved") is True and document.get("expected_head") == expected_head, "blocked_stale_or_unapproved_decision")
        return {"status": "validated-human-receipt", "actor_id": actor["id"], "scope": scope, "decision": document["decision"], "receipt_ref": ref, "granted": False}

    def compile(self, qualified_id: str, inputs: Mapping[str, Any], authority: Mapping[str, Any], expected_head: Mapping[str, Any]) -> dict[str, Any]:
        """Return one immutable candidate or a typed, non-authorizing refusal."""

        profile: Optional[str] = qualified_id.split(".")[1] if isinstance(qualified_id, str) and qualified_id.count(".") == 2 else None
        try:
            head = _head(expected_head)
            upstream = self._upstream_refs()
            _require(qualified_id in _RESULT_KINDS, "blocked_unknown_selector")
            profile = qualified_id.split(".")[1]
            values = _exact(inputs, {"profile", "host_receipt_ref", "predecessor_artifact_refs", "evidence_refs", "evidence", "human_approval_ref"}, "blocked_malformed_inputs")
            _require(values["profile"] == profile, "blocked_profile_identity")
            bound_authority, authority_digest = self._authority(authority)
            host_ref, host = self._host(qualified_id, values["host_receipt_ref"], bound_authority, head)
            predecessors = self._predecessors(qualified_id, values["predecessor_artifact_refs"], authority_digest, head)
            refs, documents = self._refs(values["evidence_refs"])
            evidence = copy.deepcopy(dict(values["evidence"])) if isinstance(values["evidence"], Mapping) else {}
            normalized = self._validate_evidence(qualified_id, evidence, documents)
            approval = self._approval(qualified_id, values["human_approval_ref"], normalized, head)
            result = {
                "schema": "software-profile-candidate/v1",
                "qualified_id": qualified_id,
                "profile": profile,
                "step_id": qualified_id.rsplit(".", 1)[-1],
                "host_selector": host,
                "result_kind": _RESULT_KINDS[qualified_id],
                "status": "candidate",
                "expected_head": head,
                "authority": bound_authority,
                "authority_digest": authority_digest,
                "host_receipt_ref": host_ref,
                "predecessor_artifact_refs": predecessors,
                "evidence_refs": refs,
                "evidence": normalized,
                "approval": approval,
                "source_interfaces": {key: upstream[key] for key in ("workflow_composition", "planning_system", "execution_group")},
                "non_mutating": True,
                "common_lifecycle_embedded": False,
                "grants_approval": False,
                "objective_outcome_claimed": False,
            }
            result["candidate_digest"] = _digest(result)
            return self._validated(result)
        except (_Refusal, SchemaValidationError, SoftwareProfileError) as error:
            return self._refusal(qualified_id, profile, str(error), expected_head)

    def compose_profile(
        self,
        profile: str,
        workflow_manifest_ref: Mapping[str, Any],
        artifact_refs: Sequence[Mapping[str, Any]],
        authority: Mapping[str, Any],
        expected_head: Mapping[str, Any],
    ) -> dict[str, Any]:
        """Bind a validated common workflow to all ordered profile candidates."""

        try:
            head = _head(expected_head)
            upstream = self._upstream_refs()
            _require(profile in _SELECTORS, "blocked_unknown_profile")
            bound_authority, authority_digest = self._authority(authority)
            manifest, manifest_ref = self._load_ref(workflow_manifest_ref, version="workflow-manifest/v1")
            try:
                workflow_receipt = WorkflowCompositionV1(source_root=self.source_root).validate(manifest)
            except WorkflowCompositionError as error:
                raise _Refusal("blocked_workflow_composition:%s" % error) from error
            _require(workflow_receipt.get("profile") == profile, "blocked_wrong_workflow_profile")
            _require(Path(manifest_ref["path"]).stem == workflow_receipt["workflow_id"], "blocked_workflow_manifest_identity")
            workflow_profile_selectors = [item for item in workflow_receipt["selectors"] if item.startswith("profile.")]
            _require(tuple(workflow_profile_selectors) == _SELECTORS[profile], "blocked_workflow_profile_order")
            _require(isinstance(artifact_refs, Sequence) and not isinstance(artifact_refs, (str, bytes)) and len(artifact_refs) == len(_SELECTORS[profile]), "blocked_incomplete_profile_artifacts")
            refs: list[dict[str, str]] = []
            kinds: list[str] = []
            for index, (value, selector) in enumerate(zip(artifact_refs, _SELECTORS[profile])):
                document, ref = self._load_ref(value, version="software-profile-candidate/v1")
                _require(document.get("profile") == profile, "blocked_cross_profile_artifact")
                _require(document.get("qualified_id") == selector, "blocked_reordered_profile_artifact")
                _require(document.get("host_selector") == _HOSTS[profile][index], "blocked_wrong_host")
                _require(document.get("expected_head") == head, "blocked_stale_artifact_head")
                _require(document.get("authority_digest") == authority_digest and document.get("authority") == bound_authority, "blocked_stale_artifact_authority")
                _require(document.get("result_kind") == _RESULT_KINDS[selector], "blocked_result_kind_substitution")
                _require(document.get("common_lifecycle_embedded") is False and document.get("grants_approval") is False and document.get("objective_outcome_claimed") is False, "blocked_authorizing_artifact")
                unsigned = {key: copy.deepcopy(item) for key, item in document.items() if key != "candidate_digest"}
                _require(document.get("candidate_digest") == _digest(unsigned), "blocked_profile_candidate_digest")
                _require(document.get("predecessor_artifact_refs", []) == refs, "blocked_profile_predecessor_chain")
                self._recompile_candidate(document)
                refs.append(ref)
                kinds.append(document["result_kind"])
            result = {
                "schema": "software-profile-composition/v1",
                "profile": profile,
                "status": "candidate",
                "workflow_manifest_ref": manifest_ref,
                "workflow_composition_receipt": workflow_receipt,
                "artifact_refs": refs,
                "selectors": list(_SELECTORS[profile]),
                "result_kinds": kinds,
                "expected_head": head,
                "authority": bound_authority,
                "authority_digest": authority_digest,
                "source_interfaces": {key: upstream[key] for key in ("workflow_composition", "planning_system", "execution_group")},
                "non_mutating": True,
                "common_lifecycle_embedded": False,
                "grants_approval": False,
                "objective_outcome_claimed": False,
            }
            result["composition_digest"] = _digest(result)
            return self._validated(result)
        except (_Refusal, SchemaValidationError, SoftwareProfileError) as error:
            return self._refusal("profile.%s.compose" % profile, profile if profile in _SELECTORS else None, str(error), expected_head)

    def _validated(self, result: dict[str, Any]) -> dict[str, Any]:
        validate_document(result, self.schema, self.schema.get("$defs", {}))
        return result

    def _refusal(self, qualified_id: Any, profile: Optional[str], reason: str, expected_head: Any) -> dict[str, Any]:
        head = None
        try:
            head = _head(expected_head)
        except _Refusal:
            pass
        result = {
            "schema": "software-profile-refusal/v1",
            "qualified_id": qualified_id if isinstance(qualified_id, str) and qualified_id else "unknown",
            "profile": profile if profile in _SELECTORS else None,
            "reason": reason or "blocked_invalid_profile_request",
            "expected_head": head,
            "non_mutating": True,
            "grants_approval": False,
            "objective_outcome_claimed": False,
        }
        return self._validated(result)

    def _validate_evidence(
        self,
        selector: str,
        evidence: Mapping[str, Any],
        documents: Mapping[tuple[str, str, str], dict[str, Any]],
    ) -> dict[str, Any]:
        handler: Callable[[Mapping[str, Any], Mapping[tuple[str, str, str], dict[str, Any]]], dict[str, Any]] = getattr(
            self, "_evidence_" + selector.rsplit(".", 1)[-1].lower()
        )
        try:
            return handler(evidence, documents)
        except _Refusal as error:
            raise _Refusal("blocked_incomplete_evidence:%s:%s" % (selector, error)) from error

    @staticmethod
    def _copy_exact(v: Mapping[str, Any], keys: set[str], string_keys: set[str]) -> dict[str, Any]:
        _exact(v, keys, "shape")
        _require(all(_nonempty(v.get(key)) for key in string_keys), "content")
        return copy.deepcopy(dict(v))

    def _test_pair(self, v: Mapping[str, Any], docs: Mapping[tuple[str, str, str], dict[str, Any]], *, regression: bool) -> dict[str, Any]:
        keys = {"behavior", "failing_before_ref", "passing_after_ref"} | ({"causal_chain_id"} if regression else set())
        result = self._copy_exact(v, keys, {"behavior"} | ({"causal_chain_id"} if regression else set()))
        before = self._doc(v["failing_before_ref"], docs, "failing-before-ref")
        after = self._doc(v["passing_after_ref"], docs, "passing-after-ref")
        required = {"schema", "test_id", "level", "subject", "phase", "command", "environment", "terminal", "status", "exit_code", "capture"} | ({"causal_chain_id"} if regression else set())
        _require(set(before) == required and set(after) == required, "test-receipt-shape")
        _require(before["schema"] == after["schema"] == "test-execution-receipt/v1", "test-receipt-schema")
        _require(before["test_id"] == after["test_id"] and before["subject"] == after["subject"] == v["behavior"], "test-identity")
        _require(before["phase"] == "before" and before["status"] == "failed" and before["terminal"] is True and isinstance(before["exit_code"], int) and before["exit_code"] != 0, "not-genuine-failing-before")
        _require(after["phase"] == "after" and after["status"] == "passed" and after["terminal"] is True and after["exit_code"] == 0, "not-genuine-passing-after")
        _require(isinstance(before["command"], Mapping) and isinstance(after["command"], Mapping) and isinstance(before["environment"], Mapping) and before["environment"] == after["environment"] and isinstance(before["capture"], Mapping) and isinstance(after["capture"], Mapping), "test-execution-evidence")
        if regression:
            _require(before["level"] == after["level"] == "regression" and before["causal_chain_id"] == after["causal_chain_id"] == v["causal_chain_id"], "regression-cause-binding")
        else:
            _require(before["level"] == after["level"] == "behavior", "behavior-test-level")
        return result

    def _evidence_f1(self, v: Mapping[str, Any], _: Mapping[Any, Any]) -> dict[str, Any]:
        return self._copy_exact(v, {"actor", "trigger", "new_behavior", "observable_outcome"}, {"actor", "trigger", "new_behavior", "observable_outcome"})

    def _evidence_f2(self, v: Mapping[str, Any], _: Mapping[Any, Any]) -> dict[str, Any]:
        result = self._copy_exact(v, {"examples"}, set())
        examples = v.get("examples")
        _require(isinstance(examples, list) and examples, "examples")
        kinds = set()
        for item in examples:
            _exact(item, {"id", "kind", "scenario", "expected_result"}, "example-shape")
            _require(all(_nonempty(item.get(key)) for key in ("id", "scenario", "expected_result")), "example-content")
            _require(item.get("kind") in {"happy", "boundary", "permission", "error", "cancel", "retry"}, "example-kind")
            kinds.add(item["kind"])
        _require({"happy", "boundary", "permission", "error"}.issubset(kinds) and bool({"cancel", "retry"}.intersection(kinds)), "example-coverage")
        return result

    def _evidence_f3(self, v: Mapping[str, Any], docs: Mapping[Any, Any]) -> dict[str, Any]:
        result = self._copy_exact(v, {"input_contract", "output_contract", "state_transitions", "compatibility", "accessibility", "fixture_refs"}, set())
        _require(all(isinstance(v[key], Mapping) and bool(v[key]) for key in ("input_contract", "output_contract", "compatibility")), "contract-map")
        _require(isinstance(v["state_transitions"], list) and v["state_transitions"] and all(isinstance(item, Mapping) and set(item) == {"from", "event", "to"} and all(_nonempty(item[key]) for key in item) for item in v["state_transitions"]), "state-transitions")
        _strings(v["accessibility"], "accessibility")
        _require(isinstance(v["fixture_refs"], list) and v["fixture_refs"] and all(self._doc(item, docs, "fixture-ref") for item in v["fixture_refs"]), "fixture-refs")
        return result

    def _evidence_f4(self, v: Mapping[str, Any], _: Mapping[Any, Any]) -> dict[str, Any]:
        result = self._copy_exact(v, {"responsibilities", "seam", "migration", "design_decision"}, {"design_decision"})
        _require(all(isinstance(v[key], Mapping) and bool(v[key]) for key in ("responsibilities", "seam", "migration")), "architecture-content")
        return result

    def _evidence_f5(self, v: Mapping[str, Any], docs: Mapping[Any, Any]) -> dict[str, Any]:
        result = self._copy_exact(v, {"scenario", "slice_steps", "end_to_end", "test_receipt_ref", "placeholder_conditions"}, {"scenario"})
        _strings(v["slice_steps"], "slice-steps")
        _require(v["end_to_end"] is True, "end-to-end")
        receipt = self._doc(v["test_receipt_ref"], docs, "slice-receipt")
        _require(receipt.get("schema") == "test-execution-receipt/v1" and receipt.get("level") == "vertical-slice" and receipt.get("subject") == v["scenario"] and receipt.get("status") == "passed" and receipt.get("terminal") is True and receipt.get("exit_code") == 0 and isinstance(receipt.get("command"), Mapping) and isinstance(receipt.get("environment"), Mapping) and isinstance(receipt.get("capture"), Mapping), "slice-receipt")
        _require(isinstance(v["placeholder_conditions"], list) and all(isinstance(item, Mapping) and set(item) == {"placeholder", "retained_until", "owner"} and all(_nonempty(item[key]) for key in item) for item in v["placeholder_conditions"]), "placeholder-conditions")
        return result

    def _evidence_f6(self, v: Mapping[str, Any], docs: Mapping[Any, Any]) -> dict[str, Any]:
        return self._test_pair(v, docs, regression=False)

    def _evidence_f7(self, v: Mapping[str, Any], docs: Mapping[Any, Any]) -> dict[str, Any]:
        result = self._copy_exact(v, {"cases"}, set())
        cases = v.get("cases")
        _require(isinstance(cases, list) and cases, "integration-cases")
        kinds = set()
        for item in cases:
            _exact(item, {"kind", "receipt_ref"}, "integration-case-shape")
            _require(item["kind"] in {"dependency", "error", "migration", "observability"}, "integration-kind")
            receipt = self._doc(item["receipt_ref"], docs, "integration-ref")
            _require(receipt.get("schema") == "test-execution-receipt/v1" and receipt.get("level") == "integration" and receipt.get("subject") == item["kind"] and receipt.get("status") == "passed" and receipt.get("terminal") is True and receipt.get("exit_code") == 0 and isinstance(receipt.get("command"), Mapping) and isinstance(receipt.get("environment"), Mapping) and isinstance(receipt.get("capture"), Mapping), "integration-receipt")
            kinds.add(item["kind"])
        _require(kinds == {"dependency", "error", "migration", "observability"}, "integration-coverage")
        return result

    def _evidence_f8(self, v: Mapping[str, Any], docs: Mapping[Any, Any]) -> dict[str, Any]:
        result = self._copy_exact(v, {"scenario", "e2e_receipt_ref"}, {"scenario"})
        receipt = self._doc(v["e2e_receipt_ref"], docs, "e2e-ref")
        required = {"schema", "test_id", "level", "subject", "phase", "command", "environment", "equivalent_environment", "terminal", "status", "exit_code", "capture"}
        _require(set(receipt) == required and receipt["schema"] == "test-execution-receipt/v1" and receipt["level"] == "e2e" and receipt["subject"] == v["scenario"] and receipt["equivalent_environment"] is True and receipt["terminal"] is True and receipt["status"] == "passed" and receipt["exit_code"] == 0, "physical-e2e")
        return result

    def _evidence_bgf1(self, v: Mapping[str, Any], _: Mapping[Any, Any]) -> dict[str, Any]:
        result = self._copy_exact(v, {"expected", "actual", "environment", "frequency", "impact", "first_observed_version"}, {"expected", "actual", "frequency", "impact", "first_observed_version"})
        _require(isinstance(v["environment"], Mapping) and bool(v["environment"]), "environment")
        return result

    def _evidence_bgf2(self, v: Mapping[str, Any], docs: Mapping[Any, Any]) -> dict[str, Any]:
        result = self._copy_exact(v, {"symptom", "reproducer_ref", "non_reproduction_conditions", "flakiness"}, {"symptom"})
        receipt = self._doc(v["reproducer_ref"], docs, "reproducer-ref")
        _require(receipt.get("schema") == "test-execution-receipt/v1" and receipt.get("phase") == "before" and receipt.get("level") == "reproducer" and receipt.get("terminal") is True and receipt.get("status") == "failed" and isinstance(receipt.get("exit_code"), int) and receipt["exit_code"] != 0, "failing-before-reproducer")
        _require(receipt.get("subject") == v["symptom"] and isinstance(receipt.get("command"), Mapping) and isinstance(receipt.get("environment"), Mapping) and isinstance(receipt.get("capture"), Mapping), "reproducer-physical-execution-binding")
        _strings(v["non_reproduction_conditions"], "non-reproduction-conditions")
        flaky = _exact(v["flakiness"], {"observed_runs", "failures", "classification"}, "flakiness-shape")
        _require(all(isinstance(flaky[key], int) and not isinstance(flaky[key], bool) and flaky[key] >= 0 for key in ("observed_runs", "failures")) and flaky["observed_runs"] > 0 and flaky["failures"] > 0 and flaky["failures"] <= flaky["observed_runs"], "flakiness-counts")
        expected = "reproducible" if flaky["failures"] == flaky["observed_runs"] else "flaky"
        _require(flaky["classification"] == expected, "flakiness-classification")
        return result

    def _evidence_bgf3(self, v: Mapping[str, Any], docs: Mapping[Any, Any]) -> dict[str, Any]:
        result = self._copy_exact(v, {"inventory"}, set())
        inventory = _exact(v.get("inventory"), {"logs", "traces", "state", "recent_changes", "boundary_evidence"}, "inventory-shape")
        observed = 0
        for item in inventory.values():
            _exact(item, {"status", "refs", "note"}, "inventory-item-shape")
            _require(item["status"] in {"observed", "unavailable"} and _nonempty(item["note"]) and isinstance(item["refs"], list), "inventory-item")
            if item["status"] == "observed":
                _require(item["refs"] and all(self._doc(ref, docs, "inventory-ref") for ref in item["refs"]), "observed-inventory-ref")
                observed += 1
            else:
                _require(not item["refs"], "unavailable-is-not-absence-proof")
        _require(observed > 0, "no-positive-evidence")
        return result

    def _evidence_bgf4(self, v: Mapping[str, Any], docs: Mapping[Any, Any]) -> dict[str, Any]:
        result = self._copy_exact(v, {"hypotheses"}, set())
        hypotheses = v.get("hypotheses")
        _require(isinstance(hypotheses, list) and hypotheses, "hypotheses")
        for index, item in enumerate(hypotheses, 1):
            _exact(item, {"id", "rank", "statement", "falsifier", "test_ref"}, "hypothesis-shape")
            _require(item["rank"] == index and all(_nonempty(item[key]) for key in ("id", "statement", "falsifier")), "hypothesis-order-content")
            self._doc(item["test_ref"], docs, "hypothesis-test")
        return result

    def _evidence_bgf5(self, v: Mapping[str, Any], docs: Mapping[Any, Any]) -> dict[str, Any]:
        result = self._copy_exact(v, {"causal_chain_id", "causal_chain", "missing_guard", "missing_test", "correlation_only"}, {"causal_chain_id", "missing_guard", "missing_test"})
        _require(v["correlation_only"] is False and isinstance(v["causal_chain"], list) and len(v["causal_chain"]) >= 2, "causality")
        for item in v["causal_chain"]:
            _exact(item, {"cause", "effect", "evidence_refs"}, "causal-link-shape")
            _require(_nonempty(item["cause"]) and _nonempty(item["effect"]) and isinstance(item["evidence_refs"], list) and item["evidence_refs"], "causal-link")
            for ref in item["evidence_refs"]:
                self._doc(ref, docs, "causal-evidence")
        return result

    def _evidence_bgf6(self, v: Mapping[str, Any], _: Mapping[Any, Any]) -> dict[str, Any]:
        result = self._copy_exact(v, {"options", "chosen_option_id"}, {"chosen_option_id"})
        options = v.get("options")
        _require(isinstance(options, list) and len(options) == 3, "fix-options")
        kinds = set()
        ids = set()
        for item in options:
            _exact(item, {"id", "kind", "description", "risks"}, "fix-option-shape")
            _require(all(_nonempty(item[key]) for key in ("id", "description")) and item["kind"] in {"minimal-fix", "workaround", "broader-fix"}, "fix-option-content")
            _strings(item["risks"], "fix-option-risks")
            kinds.add(item["kind"]); ids.add(item["id"])
        _require(kinds == {"minimal-fix", "workaround", "broader-fix"} and v["chosen_option_id"] in ids, "fix-option-coverage-choice")
        return result

    def _evidence_bgf7(self, v: Mapping[str, Any], docs: Mapping[Any, Any]) -> dict[str, Any]:
        return self._test_pair(v, docs, regression=True)

    def _evidence_bgf8(self, v: Mapping[str, Any], docs: Mapping[Any, Any]) -> dict[str, Any]:
        result = self._copy_exact(v, {"receipts"}, set())
        receipts = _exact(v.get("receipts"), {"original_symptom", "adjacent_contracts", "performance_safety", "equivalent_environment"}, "impact-receipts")
        for kind, ref in receipts.items():
            receipt = self._doc(ref, docs, "impact-ref")
            _require(receipt.get("schema") == "verification-receipt/v1" and receipt.get("kind") == kind.replace("_", "-") and receipt.get("status") == "passed" and receipt.get("terminal") is True and isinstance(receipt.get("method"), Mapping) and isinstance(receipt.get("capture"), Mapping), "impact-receipt")
            if kind == "equivalent_environment":
                _require(receipt.get("equivalent_environment") is True, "impact-equivalent-environment")
        return result

    def _evidence_i1(self, v: Mapping[str, Any], _: Mapping[Any, Any]) -> dict[str, Any]:
        result = self._copy_exact(v, {"metric", "availability", "value", "missingness", "variance", "method", "period", "environment"}, {"metric", "method", "period"})
        _require(v["availability"] in {"available", "unavailable"} and isinstance(v["environment"], Mapping) and bool(v["environment"]), "baseline-context")
        _require(isinstance(v["variance"], Mapping) and bool(v["variance"]), "baseline-variance")
        if v["availability"] == "unavailable":
            _require(v["value"] is None and _nonempty(v["missingness"]), "unavailable-not-zero")
        else:
            _require(v["value"] is not None and isinstance(v["missingness"], str), "available-value")
        return result

    def _evidence_i2(self, v: Mapping[str, Any], _: Mapping[Any, Any]) -> dict[str, Any]:
        return self._copy_exact(v, {"change", "mechanism", "metric", "predicted_direction", "falsification_condition"}, {"change", "mechanism", "metric", "predicted_direction", "falsification_condition"})

    def _evidence_i3(self, v: Mapping[str, Any], _: Mapping[Any, Any]) -> dict[str, Any]:
        result = self._copy_exact(v, {"guardrails"}, set())
        guardrails = v.get("guardrails")
        _require(isinstance(guardrails, list) and guardrails, "guardrails")
        kinds = set()
        for item in guardrails:
            _exact(item, {"kind", "metric", "threshold", "method"}, "guardrail-shape")
            _require(item["kind"] in {"quality", "cost", "user-impact"} and all(_nonempty(item[key]) for key in ("metric", "threshold", "method")), "guardrail-content")
            kinds.add(item["kind"])
        _require(kinds == {"quality", "cost", "user-impact"}, "guardrail-coverage")
        return result

    def _evidence_i4(self, v: Mapping[str, Any], _: Mapping[Any, Any]) -> dict[str, Any]:
        result = self._copy_exact(v, {"change_id", "approved_change", "comparison_method", "rollback"}, {"change_id", "approved_change", "comparison_method", "rollback"})
        return result

    def _evidence_i5(self, v: Mapping[str, Any], docs: Mapping[Any, Any]) -> dict[str, Any]:
        result = self._copy_exact(v, {"change_id", "scoped_paths", "action_receipt_ref"}, {"change_id"})
        _strings(v["scoped_paths"], "scoped-paths")
        receipt = self._doc(v["action_receipt_ref"], docs, "action-ref")
        _require(receipt.get("schema") == "scoped-action-receipt/v1" and receipt.get("change_id") == v["change_id"] and receipt.get("scoped_paths") == v["scoped_paths"] and receipt.get("status") == "completed" and receipt.get("adoption_claimed") is False, "scoped-action-receipt")
        return result

    def _evidence_i6(self, v: Mapping[str, Any], _: Mapping[Any, Any]) -> dict[str, Any]:
        result = self._copy_exact(v, {"metric", "before", "after"}, {"metric"})
        required = {"availability", "value", "method", "period", "environment", "variance"}
        before = _exact(v.get("before"), required, "before-observation")
        after = _exact(v.get("after"), required, "after-observation")
        _require(before["availability"] == after["availability"] == "available" and before["value"] is not None and after["value"] is not None, "comparison-missing")
        _require(before["method"] == after["method"] and before["period"] == after["period"] and before["environment"] == after["environment"], "insufficient-comparability")
        _require(isinstance(before["variance"], Mapping) and isinstance(after["variance"], Mapping), "comparison-variance")
        return result

    def _evidence_i7(self, v: Mapping[str, Any], _: Mapping[Any, Any]) -> dict[str, Any]:
        base = {"recommendation", "rationale"}
        replay = base | {"approved", "promotion_authorized"}
        _require(frozenset(v) in {frozenset(base), frozenset(replay)}, "shape")
        result = copy.deepcopy(dict(v))
        _require(_nonempty(v.get("rationale")), "content")
        _require(v["recommendation"] in {"adopt", "iterate", "rollback", "insufficient-evidence"}, "decision-candidate")
        _require(v.get("approved", False) is False and v.get("promotion_authorized", False) is False, "decision-not-approved")
        result["approved"] = False
        result["promotion_authorized"] = False
        return result


__all__ = ["SoftwareProfileError", "SoftwareProfileV1"]
