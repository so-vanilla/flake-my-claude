"""Pure, source-only compiler for the seven company-governance surfaces.

The compiler consumes only digest-bound fixture documents.  It never looks up
an identity or policy, approves a catalog entry, releases software, or writes an
audit event.  Persisted predecessors are accepted only after bounded semantic
replay under the same company authority and expected HEAD.
"""

from __future__ import annotations

import copy
import hashlib
import json
import math
import re
from collections.abc import Mapping
from datetime import datetime
from pathlib import Path
from typing import Any, Optional

from .schema_validation import SchemaValidationError, validate_document


class CompanyGovernanceError(ValueError):
    """The compiler source or a requested physical binding is malformed."""


class _Refusal(ValueError):
    def __init__(self, reason: str, details: Optional[Mapping[str, Any]] = None) -> None:
        super().__init__(reason)
        self.details = copy.deepcopy(dict(details or {}))


_DEFAULT_SOURCE_ROOT = Path(__file__).resolve().parents[3]
_SHA256 = re.compile(r"^sha256:[0-9a-f]{64}$")
_COMPANY_ID = re.compile(r"^[a-z0-9][a-z0-9-]{1,63}$")
_SECRET_KEY = re.compile(r"^(?:password|passwd|token|api[_-]?key|access[_-]?key|private[_-]?key|credential|raw[_-]?secret|secret[_-]?value)s?$", re.I)
_SECRET_VALUE = re.compile(r"(?:-----BEGIN [A-Z ]*PRIVATE KEY-----|\bAKIA[0-9A-Z]{16}\b|\b(?:sk|ghp|github_pat)_[A-Za-z0-9_-]{16,})")
_SELECTORS = (
    "surface.company.classify-data",
    "surface.company.resolve-identity",
    "surface.company.authorize-tools",
    "surface.company.approve-catalog",
    "surface.company.evaluate-change",
    "surface.company.release-workflow",
    "surface.company.audit-operation",
)
_RESULT_KINDS = {
    "surface.company.classify-data": "data-classification-candidate",
    "surface.company.resolve-identity": "identity-resolution-candidate",
    "surface.company.authorize-tools": "tool-authorization-candidate",
    "surface.company.approve-catalog": "catalog-approval-validation-candidate",
    "surface.company.evaluate-change": "comparative-evaluation-candidate",
    "surface.company.release-workflow": "release-workflow-candidate",
    "surface.company.audit-operation": "audit-operation-candidate",
}
_KNOWN_LICENSES = {"MIT", "Apache-2.0", "BSD-3-Clause", "MPL-2.0", "ISC", "Proprietary-Approved"}
_SCOPE_KEYS = {"filesystem", "network", "external_actions", "production"}
_DIMENSIONS = {
    "quality": "higher-better",
    "cost": "lower-better",
    "latency": "lower-better",
    "failure": "lower-better",
    "rollback": "lower-better",
}
_STEP_CATALOG = (
    "docs/plans/ai-agent-workflow-step-catalog.md",
    "sha256:4ae2f11b89eedabc4c4a5cf71f96d4041c94ed58a48f6c1e75172ae648fe7c0d",
)


def _digest(value: Any) -> str:
    raw = json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=True).encode()
    return "sha256:" + hashlib.sha256(raw).hexdigest()


def _bytes_digest(value: bytes) -> str:
    return "sha256:" + hashlib.sha256(value).hexdigest()


def _require(condition: bool, code: str, details: Optional[Mapping[str, Any]] = None) -> None:
    if not condition:
        raise _Refusal(code, details)


def _nonempty(value: Any) -> bool:
    return isinstance(value, str) and bool(value.strip()) and value.strip().lower() not in {
        "todo", "tbd", "placeholder", "unknown", "unresolved", "<unknown>"
    }


def _exact(value: Any, keys: set[str], code: str) -> Mapping[str, Any]:
    _require(isinstance(value, Mapping) and set(value) == keys, code)
    return value


def _strings(value: Any, code: str, *, allow_empty: bool = False) -> list[str]:
    _require(isinstance(value, list) and (allow_empty or bool(value)), code)
    _require(all(_nonempty(item) for item in value) and len(value) == len(set(value)), code)
    return list(value)


def _head(value: Any) -> dict[str, Any]:
    head = _exact(value, {"revision", "transaction_digest"}, "blocked_malformed_head")
    _require(isinstance(head["revision"], int) and not isinstance(head["revision"], bool) and head["revision"] >= 0, "blocked_malformed_head")
    _require(isinstance(head["transaction_digest"], str) and _SHA256.fullmatch(head["transaction_digest"]) is not None, "blocked_malformed_head")
    return copy.deepcopy(dict(head))


def _instant(value: Any, code: str) -> datetime:
    _require(_nonempty(value) and str(value).endswith("Z"), code)
    try:
        parsed = datetime.fromisoformat(str(value).replace("Z", "+00:00"))
    except ValueError as error:
        raise _Refusal(code) from error
    _require(parsed.tzinfo is not None, code)
    return parsed


def _empty_details() -> dict[str, list[str]]:
    return {
        "missing_event_ids": [],
        "unreadable_event_ids": [],
        "bypassed_event_ids": [],
        "unretained_event_ids": [],
    }


def _reject_raw_secrets(value: Any) -> None:
    def visit(item: Any, path: tuple[str, ...]) -> None:
        if isinstance(item, Mapping):
            for key, child in item.items():
                _require(not (_SECRET_KEY.fullmatch(str(key)) and child not in (None, False, 0, "", [], {})), "blocked_raw_secret_input")
                visit(child, (*path, str(key)))
        elif isinstance(item, list):
            for index, child in enumerate(item):
                visit(child, (*path, str(index)))
        elif isinstance(item, str):
            _require(_SECRET_VALUE.search(item) is None, "blocked_raw_secret_input")

    visit(value, ())


def _ref_key(value: Mapping[str, Any]) -> tuple[str, str, str]:
    return value["path"], value["version"], value["digest"]


class CompanyGovernanceV1:
    """Compile one immutable company candidate or typed refusal."""

    def __init__(
        self,
        *,
        source_root: Optional[Path] = None,
        reference_root: Optional[Path] = None,
        schema_path: Optional[Path] = None,
    ) -> None:
        self.source_root = Path(source_root or _DEFAULT_SOURCE_ROOT).resolve()
        self.reference_root = Path(reference_root or self.source_root).resolve()
        path = Path(schema_path or self.source_root / "agent-workflows/schemas/company-governance-result-v1.schema.json")
        try:
            self.schema = json.loads(path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError) as error:
            raise CompanyGovernanceError("cannot read company-governance schema: %s" % error) from error

    @staticmethod
    def selectors() -> tuple[str, ...]:
        return _SELECTORS

    @staticmethod
    def _safe_path(relative: Path, company_id: str, *, artifact: bool) -> bool:
        if relative.is_absolute() or ".." in relative.parts:
            return False
        required = ("company-artifacts" if artifact else "company-evidence", company_id)
        return relative.parts[:2] == required and not any(part.lower() in {"personal", "run", ".codex", ".claude"} for part in relative.parts)

    def _load_ref(
        self,
        value: Any,
        company_id: str,
        *,
        version: Optional[str] = None,
        artifact: bool = False,
    ) -> tuple[dict[str, Any], dict[str, str]]:
        ref = _exact(value, {"path", "version", "digest"}, "blocked_malformed_physical_ref")
        _require(_nonempty(ref["path"]) and _nonempty(ref["version"]), "blocked_malformed_physical_ref")
        _require(isinstance(ref["digest"], str) and _SHA256.fullmatch(ref["digest"]) is not None, "blocked_malformed_physical_ref")
        relative = Path(ref["path"])
        _require(self._safe_path(relative, company_id, artifact=artifact), "blocked_cross_company_or_personal_path")
        resolved = (self.reference_root / relative).resolve()
        try:
            resolved.relative_to(self.reference_root)
        except ValueError as error:
            raise _Refusal("blocked_cross_company_or_personal_path") from error
        _require(resolved.is_file(), "blocked_missing_physical_ref:%s" % ref["path"])
        try:
            raw = resolved.read_bytes()
            document = json.loads(raw.decode("utf-8"))
        except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
            raise _Refusal("blocked_unreadable_physical_ref:%s" % ref["path"]) from error
        _require(_bytes_digest(raw) == ref["digest"], "blocked_digest_drift:%s" % ref["path"])
        _require(isinstance(document, Mapping) and document.get("schema") == ref["version"], "blocked_ref_version_mismatch:%s" % ref["path"])
        _require(document.get("company_id") == company_id, "blocked_cross_company_document:%s" % ref["path"])
        if version is not None:
            _require(ref["version"] == version, "blocked_ref_kind_substitution:%s" % ref["path"])
        _reject_raw_secrets(document)
        return copy.deepcopy(dict(document)), copy.deepcopy(dict(ref))

    def _authority(self, value: Any) -> tuple[dict[str, Any], str]:
        authority = _exact(
            value,
            {"authority_ref", "company_id", "namespace", "store_namespace", "scope", "owner_ref", "policy_ref", "audit_plan_ref"},
            "blocked_malformed_authority",
        )
        company_id = authority["company_id"]
        _require(isinstance(company_id, str) and _COMPANY_ID.fullmatch(company_id) is not None, "blocked_malformed_authority")
        _require(authority["namespace"] == "company/%s/governance" % company_id, "blocked_company_namespace")
        _require(authority["store_namespace"] == "company-store/%s" % company_id, "blocked_company_store_namespace")
        _require(authority["scope"] in {"company-governance-candidate", "fixture-only"}, "blocked_authority_scope")
        authority_document, authority_ref = self._load_ref(authority["authority_ref"], company_id, version="company-authority/v1")
        owner_document, owner_ref = self._load_ref(authority["owner_ref"], company_id, version="company-owner/v1")
        policy_document, policy_ref = self._load_ref(authority["policy_ref"], company_id, version="company-effective-tool-policy/v1")
        audit_plan, audit_plan_ref = self._load_ref(authority["audit_plan_ref"], company_id, version="company-audit-operation-plan/v1")
        _require(len({ref["path"] for ref in (authority_ref, owner_ref, policy_ref, audit_plan_ref)}) == 4, "blocked_duplicate_authority_ref")
        _require(set(authority_document) == {"schema", "company_id", "authority_id"} and _nonempty(authority_document["authority_id"]), "blocked_malformed_authority_receipt")
        _require(set(owner_document) == {"schema", "company_id", "owner_id"} and _nonempty(owner_document["owner_id"]), "blocked_malformed_owner_receipt")
        _require(set(policy_document) == {"schema", "company_id", "identity", "scopes", "readback_status", "source"}, "blocked_malformed_effective_policy")
        policy_identity = _exact(policy_document["identity"], {"type", "id"}, "blocked_malformed_effective_policy")
        _require(policy_identity["type"] in {"human", "agent", "service"} and _nonempty(policy_identity["id"]), "blocked_malformed_effective_policy")
        self._scopes(policy_document["scopes"], "blocked_malformed_effective_policy")
        _require(policy_document["readback_status"] == "effective" and policy_document["source"] == "fixture", "blocked_ineffective_policy_readback")
        _require(set(audit_plan) == {"schema", "company_id", "operation_id", "release_binding", "expected_events", "retention_days"}, "blocked_malformed_audit_operation_plan")
        release_binding = _exact(audit_plan["release_binding"], {"release_id", "artifact_digest"}, "blocked_malformed_audit_operation_plan")
        _require(_nonempty(audit_plan["operation_id"]) and _nonempty(release_binding["release_id"]), "blocked_malformed_audit_operation_plan")
        _require(isinstance(release_binding["artifact_digest"], str) and _SHA256.fullmatch(release_binding["artifact_digest"]) is not None, "blocked_malformed_audit_operation_plan")
        _require(isinstance(audit_plan["retention_days"], int) and not isinstance(audit_plan["retention_days"], bool) and audit_plan["retention_days"] > 0, "blocked_malformed_audit_operation_plan")
        _require(isinstance(audit_plan["expected_events"], list) and bool(audit_plan["expected_events"]), "blocked_malformed_audit_operation_plan")
        event_ids: set[str] = set()
        for item in audit_plan["expected_events"]:
            event = _exact(item, {"event_id", "event_type"}, "blocked_malformed_audit_operation_plan")
            _require(_nonempty(event["event_id"]) and _nonempty(event["event_type"]) and event["event_id"] not in event_ids, "blocked_malformed_audit_operation_plan")
            event_ids.add(event["event_id"])
        copied = copy.deepcopy(dict(authority))
        return copied, _digest(copied)

    def _source_interfaces(self) -> dict[str, dict[str, str]]:
        compiler_path = "agent-workflows/src/ai_agent_workflow/company_governance.py"
        catalog_path, accepted_catalog_digest = _STEP_CATALOG
        try:
            compiler_digest = _bytes_digest((self.source_root / compiler_path).read_bytes())
            catalog_digest = _bytes_digest((self.source_root / catalog_path).read_bytes())
        except OSError as error:
            raise _Refusal("blocked_source_interface_unavailable") from error
        _require(catalog_digest == accepted_catalog_digest, "blocked_step_catalog_digest_drift")
        return {
            "compiler": {"path": compiler_path, "digest": compiler_digest},
            "step_catalog": {"path": catalog_path, "digest": catalog_digest},
        }

    def _refs(self, values: Any, company_id: str) -> tuple[list[dict[str, str]], dict[tuple[str, str, str], dict[str, Any]]]:
        _require(isinstance(values, list) and bool(values), "blocked_missing_evidence_refs")
        refs: list[dict[str, str]] = []
        documents: dict[tuple[str, str, str], dict[str, Any]] = {}
        paths: set[str] = set()
        for value in values:
            document, ref = self._load_ref(value, company_id)
            key = _ref_key(ref)
            _require(key not in documents and ref["path"] not in paths, "blocked_duplicate_or_changed_path_ref")
            refs.append(ref)
            documents[key] = document
            paths.add(ref["path"])
        return refs, documents

    @staticmethod
    def _bound(ref: Any, documents: Mapping[tuple[str, str, str], dict[str, Any]], code: str) -> dict[str, Any]:
        _require(isinstance(ref, Mapping), code)
        key = (ref.get("path"), ref.get("version"), ref.get("digest"))
        _require(key in documents, code)
        return documents[key]

    def _predecessors(
        self,
        selector: str,
        values: Any,
        authority: Mapping[str, Any],
        authority_digest: str,
        expected_head: Mapping[str, Any],
    ) -> tuple[list[dict[str, str]], list[dict[str, Any]]]:
        expected = _SELECTORS[:_SELECTORS.index(selector)]
        _require(isinstance(values, list) and len(values) == len(expected), "blocked_missing_or_extra_predecessor")
        refs: list[dict[str, str]] = []
        documents: list[dict[str, Any]] = []
        paths: set[str] = set()
        company_id = authority["company_id"]
        for value, expected_selector in zip(values, expected):
            document, ref = self._load_ref(value, company_id, version="company-governance-candidate/v1", artifact=True)
            _require(ref["path"] not in paths, "blocked_duplicate_or_changed_path_ref")
            _require(document.get("surface_id") == expected_selector, "blocked_reordered_predecessor")
            _require(document.get("expected_head") == expected_head, "blocked_stale_predecessor_head")
            _require(document.get("authority") == authority and document.get("authority_digest") == authority_digest, "blocked_stale_predecessor_authority")
            _require(document.get("company_id") == company_id and document.get("company_store_namespace") == authority["store_namespace"], "blocked_cross_company_predecessor")
            _require(document.get("predecessor_artifact_refs") == refs, "blocked_predecessor_chain")
            unsigned = {key: copy.deepcopy(item) for key, item in document.items() if key != "candidate_digest"}
            _require(document.get("candidate_digest") == _digest(unsigned), "blocked_predecessor_candidate_digest")
            self._recompile_candidate(document)
            document["_physical_ref"] = copy.deepcopy(ref)
            refs.append(ref)
            documents.append(document)
            paths.add(ref["path"])
        return refs, documents

    def _recompile_candidate(self, document: Mapping[str, Any]) -> None:
        approval = document.get("external_approval")
        _require(isinstance(approval, Mapping), "blocked_persisted_candidate_approval")
        approval_ref = approval.get("receipt_ref") if approval.get("status") == "validated-external-receipt" else None
        payload = copy.deepcopy(document.get("payload"))
        if isinstance(payload, dict):
            if document.get("surface_id") == "surface.company.approve-catalog":
                payload.pop("entry_digest", None)
            elif document.get("surface_id") == "surface.company.evaluate-change":
                payload.pop("comparison", None)
                payload.pop("adoption_status", None)
            elif document.get("surface_id") == "surface.company.release-workflow":
                payload.pop("plan_digest", None)
                payload.pop("release_status", None)
            elif document.get("surface_id") == "surface.company.audit-operation":
                payload.pop("completeness", None)
                payload.pop("audit_source", None)
        inputs = {
            "predecessor_artifact_refs": document.get("predecessor_artifact_refs"),
            "evidence_refs": document.get("evidence_refs"),
            "payload": payload,
            "approval_receipt_ref": approval_ref,
        }
        recompiled = self.compile(document.get("surface_id"), inputs, document.get("authority"), document.get("expected_head"))
        _require(recompiled.get("schema") == "company-governance-candidate/v1", "blocked_persisted_candidate_semantics:%s" % recompiled.get("reason", "invalid"))
        _require(recompiled == document, "blocked_persisted_candidate_replay_mismatch")

    def compile(self, surface_id: str, inputs: Mapping[str, Any], authority: Mapping[str, Any], expected_head: Mapping[str, Any]) -> dict[str, Any]:
        """Return a deterministic static candidate or a redacted typed refusal."""

        company_id: Optional[str] = authority.get("company_id") if isinstance(authority, Mapping) else None
        try:
            _reject_raw_secrets({"inputs": inputs, "authority": authority})
            head = _head(expected_head)
            _require(surface_id in _RESULT_KINDS, "blocked_unknown_selector")
            values = _exact(inputs, {"predecessor_artifact_refs", "evidence_refs", "payload", "approval_receipt_ref"}, "blocked_malformed_inputs")
            bound_authority, authority_digest = self._authority(authority)
            company_id = bound_authority["company_id"]
            predecessors, predecessor_documents = self._predecessors(surface_id, values["predecessor_artifact_refs"], bound_authority, authority_digest, head)
            evidence_refs, documents = self._refs(values["evidence_refs"], company_id)
            payload = copy.deepcopy(dict(values["payload"])) if isinstance(values["payload"], Mapping) else {}
            normalized, external_approval = self._validate_payload(
                surface_id,
                payload,
                documents,
                predecessor_documents,
                values["approval_receipt_ref"],
                bound_authority,
                authority_digest,
                head,
            )
            result = {
                "schema": "company-governance-candidate/v1",
                "surface_id": surface_id,
                "company_id": company_id,
                "stage": surface_id.rsplit(".", 1)[-1],
                "result_kind": _RESULT_KINDS[surface_id],
                "status": "candidate",
                "expected_head": head,
                "authority": bound_authority,
                "authority_digest": authority_digest,
                "company_store_namespace": bound_authority["store_namespace"],
                "predecessor_artifact_refs": predecessors,
                "evidence_refs": evidence_refs,
                "payload": normalized,
                "external_approval": external_approval,
                "source_interfaces": self._source_interfaces(),
                "non_mutating": True,
                "source_only": True,
                "uses_personal_state": False,
                "grants_approval": False,
                "performs_external_lookup": False,
                "performs_release": False,
                "reports_live_audit": False,
            }
            result["candidate_digest"] = _digest(result)
            return self._validated(result)
        except (_Refusal, SchemaValidationError, CompanyGovernanceError) as error:
            details = error.details if isinstance(error, _Refusal) else None
            return self._refusal(surface_id, company_id, str(error), expected_head, details)

    def _validate_payload(
        self,
        selector: str,
        payload: dict[str, Any],
        documents: Mapping[tuple[str, str, str], dict[str, Any]],
        predecessors: list[dict[str, Any]],
        approval_value: Any,
        authority: Mapping[str, Any],
        authority_digest: str,
        head: Mapping[str, Any],
    ) -> tuple[dict[str, Any], dict[str, Any]]:
        needs_approval = selector in {"surface.company.approve-catalog", "surface.company.release-workflow"}
        _require((approval_value is not None) == needs_approval, "blocked_unexpected_or_missing_external_approval")
        handlers = {
            "surface.company.classify-data": self._classify_data,
            "surface.company.resolve-identity": self._resolve_identity,
            "surface.company.authorize-tools": self._authorize_tools,
            "surface.company.approve-catalog": self._approve_catalog,
            "surface.company.evaluate-change": self._evaluate_change,
            "surface.company.release-workflow": self._release_workflow,
            "surface.company.audit-operation": self._audit_operation,
        }
        if not needs_approval:
            normalized = handlers[selector](payload, documents, predecessors, authority)
            return normalized, {"status": "not-applicable", "actor": None, "scope": None, "receipt_ref": None, "granted": False}
        approval_document, approval_ref = self._load_ref(approval_value, authority["company_id"])
        normalized = handlers[selector](payload, documents, predecessors, authority)
        approval = self._external_approval(selector, normalized, approval_document, approval_ref, authority_digest, head)
        return normalized, approval

    def _classify_data(self, value: dict[str, Any], _docs: Mapping[Any, Any], _pred: list[dict[str, Any]], _authority: Mapping[str, Any]) -> dict[str, Any]:
        v = _exact(value, {"dataset_id", "data_class", "retention", "region", "scan"}, "blocked_incomplete_evidence:surface.company.classify-data")
        _require(_nonempty(v["dataset_id"]) and v["data_class"] in {"public", "internal", "confidential", "restricted"}, "blocked_invalid_data_classification")
        retention = _exact(v["retention"], {"policy_id", "duration_days", "disposition"}, "blocked_invalid_retention")
        _require(_nonempty(retention["policy_id"]) and isinstance(retention["duration_days"], int) and not isinstance(retention["duration_days"], bool) and retention["duration_days"] > 0, "blocked_invalid_retention")
        _require(retention["disposition"] in {"delete", "archive", "legal-hold"}, "blocked_invalid_retention")
        region = _exact(v["region"], {"allowed_regions", "selected_region", "policy_status"}, "blocked_invalid_region")
        allowed = _strings(region["allowed_regions"], "blocked_invalid_region")
        _require(region["selected_region"] in allowed and region["policy_status"] == "satisfied", "blocked_invalid_region")
        scan = _exact(v["scan"], {"status", "secret_finding_count", "pii_finding_count", "secret_categories", "pii_categories", "raw_values_included"}, "blocked_incomplete_secret_pii_scan")
        _require(scan["status"] == "complete" and scan["raw_values_included"] is False, "blocked_raw_or_incomplete_scan")
        for key in ("secret_finding_count", "pii_finding_count"):
            _require(isinstance(scan[key], int) and not isinstance(scan[key], bool) and scan[key] >= 0, "blocked_incomplete_secret_pii_scan")
        secret_categories = _strings(scan["secret_categories"], "blocked_incomplete_secret_pii_scan", allow_empty=True)
        pii_categories = _strings(scan["pii_categories"], "blocked_incomplete_secret_pii_scan", allow_empty=True)
        _require((scan["secret_finding_count"] == 0) == (not secret_categories), "blocked_scan_count_mismatch")
        _require((scan["pii_finding_count"] == 0) == (not pii_categories), "blocked_scan_count_mismatch")
        return copy.deepcopy(dict(v))

    def _resolve_identity(self, value: dict[str, Any], _docs: Mapping[Any, Any], _pred: list[dict[str, Any]], _authority: Mapping[str, Any]) -> dict[str, Any]:
        v = _exact(value, {"identity", "delegation", "evaluated_at"}, "blocked_incomplete_evidence:surface.company.resolve-identity")
        identity = _exact(v["identity"], {"type", "id"}, "blocked_invalid_identity")
        _require(identity["type"] in {"human", "agent", "service"} and _nonempty(identity["id"]), "blocked_invalid_identity")
        delegation = _exact(v["delegation"], {"delegator", "delegatee", "scopes", "issued_at", "expires_at", "status"}, "blocked_invalid_delegation")
        delegator = _exact(delegation["delegator"], {"type", "id"}, "blocked_invalid_delegation")
        delegatee = _exact(delegation["delegatee"], {"type", "id"}, "blocked_invalid_delegation")
        _require(delegator["type"] in {"human", "service"} and _nonempty(delegator["id"]), "blocked_agent_self_delegation")
        _require(delegatee == identity and delegator != delegatee, "blocked_agent_self_delegation")
        self._scopes(delegation["scopes"], "blocked_invalid_delegation")
        issued = _instant(delegation["issued_at"], "blocked_invalid_delegation_expiry")
        expires = _instant(delegation["expires_at"], "blocked_invalid_delegation_expiry")
        evaluated = _instant(v["evaluated_at"], "blocked_invalid_delegation_expiry")
        _require(delegation["status"] == "active" and issued <= evaluated < expires, "blocked_expired_delegation")
        return copy.deepcopy(dict(v))

    @staticmethod
    def _scopes(value: Any, code: str) -> dict[str, list[str]]:
        scopes = _exact(value, _SCOPE_KEYS, code)
        return {key: _strings(scopes[key], code, allow_empty=True) for key in sorted(_SCOPE_KEYS)}

    def _authorize_tools(self, value: dict[str, Any], docs: Mapping[Any, Any], pred: list[dict[str, Any]], authority: Mapping[str, Any]) -> dict[str, Any]:
        v = _exact(value, {"requested_scopes", "effective_policy_ref", "least_privilege"}, "blocked_incomplete_evidence:surface.company.authorize-tools")
        requested = self._scopes(v["requested_scopes"], "blocked_invalid_tool_scopes")
        _require(any(requested.values()) and v["least_privilege"] is True, "blocked_least_privilege_not_proven")
        policy = self._bound(v["effective_policy_ref"], docs, "blocked_unbound_effective_policy")
        _require(v["effective_policy_ref"] == authority["policy_ref"], "blocked_policy_authority_mismatch")
        required = {"schema", "company_id", "identity", "scopes", "readback_status", "source"}
        _require(set(policy) == required and policy["schema"] == "company-effective-tool-policy/v1", "blocked_malformed_effective_policy")
        _require(policy["company_id"] == authority["company_id"] and policy["readback_status"] == "effective" and policy["source"] == "fixture", "blocked_ineffective_policy_readback")
        policy_identity = _exact(policy["identity"], {"type", "id"}, "blocked_malformed_effective_policy")
        _require(len(pred) >= 2 and policy_identity == pred[1]["payload"]["identity"], "blocked_policy_identity_mismatch")
        permitted = self._scopes(policy["scopes"], "blocked_malformed_effective_policy")
        delegated = self._scopes(pred[1]["payload"]["delegation"]["scopes"], "blocked_invalid_delegation")
        for key in sorted(_SCOPE_KEYS):
            _require(set(requested[key]).issubset(permitted[key]), "blocked_tool_scope_broader_than_policy:%s" % key)
            _require(set(requested[key]).issubset(delegated[key]), "blocked_tool_scope_broader_than_delegation:%s" % key)
        prefix = "company/%s/" % authority["company_id"]
        _require(all(item.startswith(prefix) and not item.startswith("/") and ".." not in Path(item).parts for item in requested["filesystem"]), "blocked_cross_company_or_personal_tool_path")
        return {"requested_scopes": requested, "effective_policy_ref": copy.deepcopy(dict(v["effective_policy_ref"])), "least_privilege": True}

    def _approve_catalog(self, value: dict[str, Any], _docs: Mapping[Any, Any], _pred: list[dict[str, Any]], _authority: Mapping[str, Any]) -> dict[str, Any]:
        v = _exact(value, {"entry", "evaluated_at"}, "blocked_incomplete_evidence:surface.company.approve-catalog")
        entry = _exact(v["entry"], {"name", "source", "version", "digest", "license", "review", "owner"}, "blocked_malformed_catalog_entry")
        _require(all(_nonempty(entry[key]) for key in ("name", "source", "version", "owner")), "blocked_malformed_catalog_entry")
        _require(isinstance(entry["digest"], str) and _SHA256.fullmatch(entry["digest"]) is not None, "blocked_malformed_catalog_entry")
        _require(entry["license"] in _KNOWN_LICENSES, "blocked_unknown_catalog_license")
        _require(not str(entry["source"]).startswith(("/Users/", "~", "personal/")), "blocked_cross_company_or_personal_path:catalog-source")
        review = _exact(entry["review"], {"status", "reviewer_type", "reviewer_id"}, "blocked_malformed_catalog_review")
        _require(review["status"] == "passed" and review["reviewer_type"] in {"human", "enterprise"} and _nonempty(review["reviewer_id"]), "blocked_unapproved_catalog_review")
        _instant(v["evaluated_at"], "blocked_invalid_approval_expiry")
        return {"entry": copy.deepcopy(dict(entry)), "entry_digest": _digest(entry), "evaluated_at": v["evaluated_at"]}

    @staticmethod
    def _metric(value: Any, dimension: str) -> dict[str, Any]:
        metric = _exact(value, {"status", "value", "unit", "method"}, "blocked_incomparable_evaluation:%s" % dimension)
        _require(metric["status"] == "available", "blocked_unavailable_evaluation_dimension:%s" % dimension)
        _require(isinstance(metric["value"], (int, float)) and not isinstance(metric["value"], bool) and math.isfinite(metric["value"]), "blocked_incomparable_evaluation:%s" % dimension)
        _require(_nonempty(metric["unit"]) and _nonempty(metric["method"]), "blocked_incomparable_evaluation:%s" % dimension)
        return copy.deepcopy(dict(metric))

    def _evaluate_change(self, value: dict[str, Any], _docs: Mapping[Any, Any], pred: list[dict[str, Any]], _authority: Mapping[str, Any]) -> dict[str, Any]:
        v = _exact(value, {"evaluation_id", "catalog_entry_digest", "baseline", "target"}, "blocked_incomplete_evidence:surface.company.evaluate-change")
        _require(_nonempty(v["evaluation_id"]) and isinstance(v["catalog_entry_digest"], str) and _SHA256.fullmatch(v["catalog_entry_digest"]) is not None, "blocked_malformed_evaluation")
        _require(len(pred) >= 4 and v["catalog_entry_digest"] == pred[3]["payload"]["entry_digest"], "blocked_evaluation_catalog_binding")
        baseline_raw = _exact(v["baseline"], set(_DIMENSIONS), "blocked_missing_evaluation_dimension")
        target_raw = _exact(v["target"], set(_DIMENSIONS), "blocked_missing_evaluation_dimension")
        baseline: dict[str, Any] = {}
        target: dict[str, Any] = {}
        comparison: dict[str, Any] = {}
        for dimension, direction in _DIMENSIONS.items():
            baseline[dimension] = self._metric(baseline_raw[dimension], dimension)
            target[dimension] = self._metric(target_raw[dimension], dimension)
            _require(baseline[dimension]["unit"] == target[dimension]["unit"] and baseline[dimension]["method"] == target[dimension]["method"], "blocked_incomparable_evaluation:%s" % dimension)
            before = baseline[dimension]["value"]
            after = target[dimension]["value"]
            delta = after - before
            improved = delta > 0 if direction == "higher-better" else delta < 0
            outcome = "equivalent" if delta == 0 else ("improved" if improved else "regressed")
            comparison[dimension] = {"baseline": before, "target": after, "unit": baseline[dimension]["unit"], "direction": direction, "delta": delta, "outcome": outcome}
        return {
            "evaluation_id": v["evaluation_id"],
            "catalog_entry_digest": v["catalog_entry_digest"],
            "baseline": baseline,
            "target": target,
            "comparison": comparison,
            "adoption_status": "not-decided",
        }

    def _release_workflow(self, value: dict[str, Any], docs: Mapping[Any, Any], pred: list[dict[str, Any]], _authority: Mapping[str, Any]) -> dict[str, Any]:
        v = _exact(value, {"release_id", "artifact_digest", "rollout_stages", "canary", "rollback", "communication", "evaluated_at"}, "blocked_incomplete_evidence:surface.company.release-workflow")
        _require(_nonempty(v["release_id"]) and isinstance(v["artifact_digest"], str) and _SHA256.fullmatch(v["artifact_digest"]) is not None, "blocked_malformed_release")
        _require(len(pred) >= 5 and v["artifact_digest"] == pred[4]["payload"]["catalog_entry_digest"], "blocked_release_artifact_binding")
        _require(isinstance(v["rollout_stages"], list) and len(v["rollout_stages"]) >= 2, "blocked_missing_staged_rollout")
        stages: list[dict[str, Any]] = []
        previous = 0
        for raw in v["rollout_stages"]:
            stage = _exact(raw, {"name", "percentage", "entry_criteria", "exit_criteria"}, "blocked_malformed_rollout_stage")
            _require(_nonempty(stage["name"]) and isinstance(stage["percentage"], int) and not isinstance(stage["percentage"], bool) and previous < stage["percentage"] <= 100, "blocked_malformed_rollout_stage")
            _strings(stage["entry_criteria"], "blocked_malformed_rollout_stage")
            _strings(stage["exit_criteria"], "blocked_malformed_rollout_stage")
            stages.append(copy.deepcopy(dict(stage)))
            previous = stage["percentage"]
        _require(stages[0]["name"] == "canary" and stages[-1]["percentage"] == 100, "blocked_missing_staged_rollout")
        canary = _exact(v["canary"], {"enabled", "cohort", "percentage", "success_criteria", "observation_minutes"}, "blocked_missing_canary")
        _require(canary["enabled"] is True and _nonempty(canary["cohort"]) and canary["percentage"] == stages[0]["percentage"], "blocked_missing_canary")
        _require(isinstance(canary["observation_minutes"], int) and not isinstance(canary["observation_minutes"], bool) and canary["observation_minutes"] > 0, "blocked_missing_canary")
        _strings(canary["success_criteria"], "blocked_missing_canary")
        rollback = _exact(v["rollback"], {"available", "plan_ref", "trigger_conditions", "restoration_target"}, "blocked_missing_rollback")
        _require(rollback["available"] is True and _nonempty(rollback["restoration_target"]), "blocked_missing_rollback")
        self._bound(rollback["plan_ref"], docs, "blocked_unbound_rollback_plan")
        _strings(rollback["trigger_conditions"], "blocked_missing_rollback")
        communication = _exact(v["communication"], {"audiences", "channel", "message_template", "owner"}, "blocked_missing_release_communication")
        _strings(communication["audiences"], "blocked_missing_release_communication")
        _require(all(_nonempty(communication[key]) for key in ("channel", "message_template", "owner")), "blocked_missing_release_communication")
        _instant(v["evaluated_at"], "blocked_invalid_approval_expiry")
        plan = {"rollout_stages": stages, "canary": copy.deepcopy(dict(canary)), "rollback": copy.deepcopy(dict(rollback)), "communication": copy.deepcopy(dict(communication))}
        return {
            "release_id": v["release_id"],
            "artifact_digest": v["artifact_digest"],
            **plan,
            "plan_digest": _digest(plan),
            "evaluated_at": v["evaluated_at"],
            "release_status": "not-executed",
        }

    def _audit_operation(self, value: dict[str, Any], docs: Mapping[Any, Any], _pred: list[dict[str, Any]], _authority: Mapping[str, Any]) -> dict[str, Any]:
        v = _exact(value, {"operation_id", "operation_plan_ref", "expected_event_ids", "events", "unreadable_event_ids", "bypass_event_ids", "incident_event_ids", "retention"}, "blocked_incomplete_evidence:surface.company.audit-operation")
        _require(_nonempty(v["operation_id"]), "blocked_malformed_audit")
        declared_expected = _strings(v["expected_event_ids"], "blocked_malformed_audit")
        unreadable = _strings(v["unreadable_event_ids"], "blocked_malformed_audit", allow_empty=True)
        bypassed = _strings(v["bypass_event_ids"], "blocked_malformed_audit", allow_empty=True)
        incidents = _strings(v["incident_event_ids"], "blocked_malformed_audit", allow_empty=True)
        _require(v["operation_plan_ref"] == _authority["audit_plan_ref"], "blocked_audit_operation_plan_mismatch")
        plan = self._bound(v["operation_plan_ref"], docs, "blocked_unbound_audit_operation_plan")
        _require(
            set(plan) == {"schema", "company_id", "operation_id", "release_binding", "expected_events", "retention_days"}
            and plan.get("schema") == "company-audit-operation-plan/v1"
            and plan.get("company_id") == _authority["company_id"],
            "blocked_malformed_audit_operation_plan",
        )
        _require(len(_pred) >= 6, "blocked_audit_release_binding")
        release_binding = _exact(plan["release_binding"], {"release_id", "artifact_digest"}, "blocked_malformed_audit_operation_plan")
        release = _pred[5]["payload"]
        _require(
            release_binding == {"release_id": release["release_id"], "artifact_digest": release["artifact_digest"]},
            "blocked_audit_release_binding",
        )
        _require(isinstance(plan["expected_events"], list) and bool(plan["expected_events"]), "blocked_malformed_audit_operation_plan")
        expected_types: dict[str, str] = {}
        for raw in plan["expected_events"]:
            planned = _exact(raw, {"event_id", "event_type"}, "blocked_malformed_audit_operation_plan")
            _require(_nonempty(planned["event_id"]) and _nonempty(planned["event_type"]) and planned["event_id"] not in expected_types, "blocked_malformed_audit_operation_plan")
            expected_types[planned["event_id"]] = planned["event_type"]
        expected = list(expected_types)
        _require(isinstance(v["events"], list), "blocked_malformed_audit")
        events: list[dict[str, Any]] = []
        observed_ids: list[str] = []
        unretained: list[str] = []
        actual_incidents: list[str] = []
        for raw in v["events"]:
            event = _exact(raw, {"event_id", "event_type", "record_ref", "retained"}, "blocked_malformed_audit_event")
            _require(_nonempty(event["event_id"]) and _nonempty(event["event_type"]) and event["event_id"] not in observed_ids, "blocked_malformed_audit_event")
            self._bound(event["record_ref"], docs, "blocked_unbound_audit_event")
            observed_ids.append(event["event_id"])
            if event["retained"] is not True:
                unretained.append(event["event_id"])
            if event["event_type"] == "incident":
                actual_incidents.append(event["event_id"])
            events.append(copy.deepcopy(dict(event)))
        missing = sorted(set(expected) - set(observed_ids))
        unexpected = sorted(set(observed_ids) - set(expected))
        details = {
            "missing_event_ids": missing,
            "unreadable_event_ids": sorted(unreadable),
            "bypassed_event_ids": sorted(bypassed),
            "unretained_event_ids": sorted(unretained),
        }
        _require(plan["operation_id"] == v["operation_id"], "blocked_audit_operation_plan_mismatch", details)
        _require(declared_expected == expected, "blocked_audit_expected_set_mismatch", details)
        _require(all(expected_types[event["event_id"]] == event["event_type"] for event in events if event["event_id"] in expected_types), "blocked_audit_event_type_mismatch", details)
        _require(not missing and not unreadable, "blocked_incomplete_audit_events", details)
        _require(not bypassed, "blocked_bypassed_audit_events", details)
        _require(not unretained, "blocked_unretained_audit_events", details)
        _require(not unexpected, "blocked_unexpected_audit_event", details)
        _require(set(incidents) == set(actual_incidents), "blocked_omitted_or_forged_incident_events", details)
        retention = _exact(v["retention"], {"required_days", "actual_days", "status"}, "blocked_malformed_audit_retention")
        _require(all(isinstance(retention[key], int) and not isinstance(retention[key], bool) and retention[key] > 0 for key in ("required_days", "actual_days")), "blocked_malformed_audit_retention")
        _require(retention["required_days"] == plan["retention_days"], "blocked_audit_operation_plan_mismatch", details)
        _require(retention["status"] == "retained" and retention["actual_days"] >= retention["required_days"], "blocked_unretained_audit_events", details)
        return {
            "operation_id": v["operation_id"],
            "operation_plan_ref": copy.deepcopy(dict(v["operation_plan_ref"])),
            "expected_event_ids": expected,
            "events": events,
            "unreadable_event_ids": [],
            "bypass_event_ids": [],
            "incident_event_ids": incidents,
            "retention": copy.deepcopy(dict(retention)),
            "completeness": "complete",
            "audit_source": "fixture-only",
        }

    def _external_approval(
        self,
        selector: str,
        payload: Mapping[str, Any],
        document: Mapping[str, Any],
        ref: Mapping[str, str],
        authority_digest: str,
        head: Mapping[str, Any],
    ) -> dict[str, Any]:
        actor = document.get("actor")
        _require(isinstance(actor, Mapping) and set(actor) == {"type", "id"} and actor.get("type") in {"human", "enterprise"} and _nonempty(actor.get("id")), "blocked_agent_self_approval")
        base = {"schema", "company_id", "actor", "scope", "decision", "authority_digest", "expected_head", "issued_at", "expires_at"}
        if selector == "surface.company.approve-catalog":
            required = base | {"entry_digest"}
            _require(set(document) == required and document.get("schema") == "company-catalog-approval/v1", "blocked_malformed_catalog_approval")
            _require(document.get("scope") == "catalog-entry" and document.get("entry_digest") == payload["entry_digest"], "blocked_forged_catalog_approval")
            evaluated_at = payload["evaluated_at"]
        else:
            required = base | {"release_id", "artifact_digest", "plan_digest"}
            _require(set(document) == required and document.get("schema") == "company-release-approval/v1", "blocked_malformed_release_approval")
            _require(document.get("scope") == "release-candidate" and document.get("release_id") == payload["release_id"], "blocked_forged_release_approval")
            _require(document.get("artifact_digest") == payload["artifact_digest"] and document.get("plan_digest") == payload["plan_digest"], "blocked_forged_release_approval")
            evaluated_at = payload["evaluated_at"]
        _require(document.get("decision") == "approved" and document.get("authority_digest") == authority_digest and document.get("expected_head") == head, "blocked_forged_external_approval")
        issued = _instant(document.get("issued_at"), "blocked_invalid_approval_expiry")
        expires = _instant(document.get("expires_at"), "blocked_invalid_approval_expiry")
        evaluated = _instant(evaluated_at, "blocked_invalid_approval_expiry")
        _require(issued <= evaluated < expires, "blocked_expired_external_approval")
        return {
            "status": "validated-external-receipt",
            "actor": copy.deepcopy(dict(actor)),
            "scope": document["scope"],
            "receipt_ref": copy.deepcopy(dict(ref)),
            "granted": False,
        }

    def _validated(self, result: dict[str, Any]) -> dict[str, Any]:
        validate_document(result, self.schema, self.schema.get("$defs", {}))
        return copy.deepcopy(result)

    def _refusal(
        self,
        surface_id: Any,
        company_id: Any,
        reason: str,
        expected_head: Any,
        details: Optional[Mapping[str, Any]],
    ) -> dict[str, Any]:
        safe_head: Optional[dict[str, Any]] = None
        try:
            safe_head = _head(expected_head)
        except _Refusal:
            pass
        safe_details = _empty_details()
        if isinstance(details, Mapping):
            for key in safe_details:
                values = details.get(key)
                if isinstance(values, list) and all(_nonempty(item) for item in values):
                    safe_details[key] = sorted(set(values))
        result = {
            "schema": "company-governance-refusal/v1",
            "surface_id": surface_id if surface_id in _SELECTORS else "unknown",
            "company_id": company_id if isinstance(company_id, str) and _COMPANY_ID.fullmatch(company_id) else None,
            "reason": reason or "blocked_invalid_company_governance_request",
            "expected_head": safe_head,
            "details": safe_details,
            "non_mutating": True,
            "source_only": True,
            "uses_personal_state": False,
            "grants_approval": False,
            "performs_external_lookup": False,
            "performs_release": False,
            "reports_live_audit": False,
        }
        return self._validated(result)


__all__ = ["CompanyGovernanceError", "CompanyGovernanceV1"]
