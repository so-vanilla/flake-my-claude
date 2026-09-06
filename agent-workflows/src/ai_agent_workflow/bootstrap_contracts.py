"""Pure, physical-reference-bound compilers for Bootstrap A1 through A5."""
from __future__ import annotations

import copy
import json
import math
import re
from pathlib import Path
from typing import Any, Dict, Mapping, Optional

from .schema_validation import validate_document


_SCHEMA_PATH = Path(__file__).resolve().parents[2] / "schemas" / "bootstrap-artifact-v1.schema.json"
_DIGEST = re.compile(r"^sha256:[0-9a-f]{64}$")
_ENTRY_FIELDS = {"path", "class", "owner", "source", "retention", "inspection"}
_ENTRY_CLASSES = {"source", "generated", "managed", "unmanaged", "app-owned", "secret-bearing", "ephemeral"}
_RETENTIONS = {"preserve", "restore", "exclude"}
_OPERATION_IDS = {
    "classify_environment": "group.A.A1",
    "plan_workspace": "group.A.A2",
    "design_rollback": "group.A.A3",
    "initialize_run_plan": "group.A.A4",
    "plan_legacy_detach": "group.A.A5",
}


class BootstrapContractError(ValueError):
    """A Bootstrap request cannot safely be dispatched."""


def _is_json_value(value: Any) -> bool:
    """Recognize canonical JSON values without invoking a serializer fallback."""
    if value is None or isinstance(value, (str, bool)):
        return True
    if isinstance(value, int) and not isinstance(value, bool):
        return True
    if isinstance(value, float):
        return math.isfinite(value)
    if isinstance(value, list):
        return all(_is_json_value(item) for item in value)
    if isinstance(value, Mapping):
        return all(isinstance(key, str) and _is_json_value(item) for key, item in value.items())
    return False


def _string_list(value: Any) -> Optional[list[str]]:
    if not isinstance(value, list) or not all(isinstance(item, str) and bool(item) for item in value):
        return None
    return copy.deepcopy(value)


def _physical_ref(value: Any, expected_kind: Optional[str] = None) -> Optional[Dict[str, str]]:
    if (
        not isinstance(value, Mapping)
        or set(value) != {"kind", "path", "digest"}
        or not isinstance(value.get("kind"), str)
        or not value["kind"]
        or (expected_kind is not None and value["kind"] != expected_kind)
        or not isinstance(value.get("path"), str)
        or not value["path"]
        or value["path"] == "candidate-generic"
        or not _DIGEST.fullmatch(str(value.get("digest")))
    ):
        return None
    return copy.deepcopy(dict(value))


def _physical_refs(
    value: Any, expected_kind: Optional[str] = None, *, non_empty: bool = True
) -> Optional[list[Dict[str, str]]]:
    if not isinstance(value, list) or (non_empty and not value):
        return None
    refs = [_physical_ref(item, expected_kind) for item in value]
    if any(item is None for item in refs):
        return None
    copied = [item for item in refs if item is not None]
    identities = [(item["kind"], item["path"], item["digest"]) for item in copied]
    if len(identities) != len(set(identities)):
        return None
    return copied


def _head(value: Any) -> Dict[str, Any]:
    if (
        not isinstance(value, Mapping)
        or set(value) != {"revision", "transaction_digest"}
        or not isinstance(value.get("revision"), int)
        or isinstance(value.get("revision"), bool)
        or value["revision"] < 0
        or not _DIGEST.fullmatch(str(value.get("transaction_digest")))
    ):
        raise BootstrapContractError("expected_head is malformed")
    return copy.deepcopy(dict(value))


def _authority(authority: Any, expected_head: Mapping[str, Any]) -> Dict[str, Any]:
    if (
        not isinstance(authority, Mapping)
        or set(authority) != {"approved", "expected_head"}
        or authority.get("approved") is not True
        or authority.get("expected_head") != expected_head
    ):
        raise BootstrapContractError("approved source authority bound to expected_head is required")
    return copy.deepcopy(dict(authority))


class BootstrapContractsV1:
    """Stateless compiler; it never reads or changes workspace or Run state."""

    def classify_environment(
        self,
        qualified_id: str,
        inputs: Mapping[str, Any],
        authority: Mapping[str, Any],
        expected_head: Mapping[str, Any],
    ) -> Dict[str, Any]:
        head, authority_ref = self._context("classify_environment", qualified_id, authority, expected_head)
        if not _is_json_value(inputs):
            return self._result(
                "classify_environment", qualified_id, "invalid-input",
                {"kind": "inventory", "entries": []}, [], authority_ref, head,
            )
        values = inputs if isinstance(inputs, Mapping) else {}
        entries = values.get("entries")
        input_refs = _physical_refs(values.get("input_refs"))
        entries_valid = isinstance(entries, list) and all(
            isinstance(entry, Mapping)
            and set(entry) == _ENTRY_FIELDS
            and isinstance(entry["path"], str)
            and bool(entry["path"])
            and entry["class"] in _ENTRY_CLASSES
            and isinstance(entry["owner"], str)
            and bool(entry["owner"])
            and isinstance(entry["source"], str)
            and bool(entry["source"])
            and entry["retention"] in _RETENTIONS
            and isinstance(entry["inspection"], str)
            and bool(entry["inspection"])
            for entry in entries
        )
        valid = set(values) == {"entries", "input_refs"} and entries_valid and input_refs is not None
        copied = copy.deepcopy(entries) if entries_valid else []
        unknown = valid and any(entry["owner"] == "unknown" for entry in copied)
        reason = "unknown-owner" if unknown else ("ok" if valid else "invalid-input")
        return self._result(
            "classify_environment", qualified_id, reason,
            {"kind": "inventory", "entries": copied}, input_refs or [], authority_ref, head,
        )

    def plan_workspace(
        self,
        qualified_id: str,
        inputs: Mapping[str, Any],
        authority: Mapping[str, Any],
        expected_head: Mapping[str, Any],
    ) -> Dict[str, Any]:
        head, authority_ref = self._context("plan_workspace", qualified_id, authority, expected_head)
        if not _is_json_value(inputs):
            return self._result(
                "plan_workspace", qualified_id, "invalid-input", self._workspace_artifact(), [], authority_ref, head,
            )
        values = inputs if isinstance(inputs, Mapping) else {}
        inventory_ref = _physical_ref(values.get("inventory_ref"), "inventory")
        base_ref = _physical_ref(values.get("base_ref"), "git-ref")
        dirty_state_ref = _physical_ref(values.get("dirty_state_ref"), "dirty-state")
        rollback_ref = _physical_ref(values.get("rollback_ref"), "rollback")
        local_inputs = _physical_refs(values.get("local_inputs"), "local-input", non_empty=False)
        dirty_paths = _string_list(values.get("dirty_paths"))
        write_scope = _string_list(values.get("write_scope"))
        required = {
            "inventory_ref", "base_ref", "dirty_state_ref", "dirty_paths", "write_scope",
            "local_inputs", "rollback_ref",
        }
        shapes_valid = (
            set(values) == required
            and inventory_ref is not None
            and base_ref is not None
            and dirty_state_ref is not None
            and rollback_ref is not None
            and local_inputs is not None
            and dirty_paths is not None
            and write_scope is not None
        )
        if values.get("rollback_ref") is None:
            reason = "missing-rollback"
        elif shapes_valid and not local_inputs:
            reason = "missing-local-input"
        elif shapes_valid and bool(set(dirty_paths or []) & set(write_scope or [])):
            reason = "foreign-diff-overlap"
        else:
            reason = "ok" if shapes_valid else "invalid-input"
        refs = [item for item in [inventory_ref, base_ref, dirty_state_ref] if item is not None]
        refs.extend(local_inputs or [])
        if rollback_ref is not None:
            refs.append(rollback_ref)
        artifact = self._workspace_artifact(
            base_ref["path"] if base_ref else "invalid",
            dirty_paths or [],
            write_scope or [],
            [item["path"] for item in (local_inputs or [])],
            rollback_ref["path"] if rollback_ref else "missing",
        )
        return self._result("plan_workspace", qualified_id, reason, artifact, refs, authority_ref, head)

    def design_rollback(
        self,
        qualified_id: str,
        inputs: Mapping[str, Any],
        authority: Mapping[str, Any],
        expected_head: Mapping[str, Any],
    ) -> Dict[str, Any]:
        head, authority_ref = self._context("design_rollback", qualified_id, authority, expected_head)
        if not _is_json_value(inputs):
            return self._result(
                "design_rollback", qualified_id, "invalid-input",
                {"kind": "rollback-plan", "details": {"restore_sources": []}}, [], authority_ref, head,
            )
        values = inputs if isinstance(inputs, Mapping) else {}
        inventory_ref = _physical_ref(values.get("inventory_ref"), "inventory")
        workspace_ref = _physical_ref(values.get("workspace_plan_ref"), "workspace-plan")
        restore_refs = _physical_refs(values.get("restore_sources"), "restore-source", non_empty=False)
        owned_sources = _string_list(values.get("owned_restore_sources"))
        backup_paths = _string_list(values.get("backup_paths"))
        secret_paths = _string_list(values.get("secret_paths"))
        required = {
            "inventory_ref", "workspace_plan_ref", "restore_sources", "owned_restore_sources",
            "backup_paths", "secret_paths",
        }
        shapes_valid = (
            set(values) == required
            and inventory_ref is not None
            and workspace_ref is not None
            and restore_refs is not None
            and owned_sources is not None
            and backup_paths is not None
            and secret_paths is not None
        )
        restore_paths = [item["path"] for item in (restore_refs or [])]
        secrets_requested = shapes_valid and bool(set(backup_paths or []) & set(secret_paths or []))
        owned = shapes_valid and bool(restore_paths) and set(restore_paths).issubset(set(owned_sources or []))
        reason = "secret-backup" if secrets_requested else (
            "missing-restore-source" if shapes_valid and not owned else ("ok" if shapes_valid else "invalid-input")
        )
        refs = [item for item in [inventory_ref, workspace_ref] if item is not None]
        refs.extend(restore_refs or [])
        return self._result(
            "design_rollback", qualified_id, reason,
            {"kind": "rollback-plan", "details": {"restore_sources": restore_paths}},
            refs, authority_ref, head,
        )

    def initialize_run_plan(
        self,
        qualified_id: str,
        inputs: Mapping[str, Any],
        authority: Mapping[str, Any],
        expected_head: Mapping[str, Any],
    ) -> Dict[str, Any]:
        head, authority_ref = self._context("initialize_run_plan", qualified_id, authority, expected_head)
        if not _is_json_value(inputs):
            return self._result(
                "initialize_run_plan", qualified_id, "invalid-input",
                {"kind": "run-plan", "details": {"objective_status": "unapproved"}},
                [], authority_ref, head,
            )
        values = inputs if isinstance(inputs, Mapping) else {}
        plan_ref = _physical_ref(values.get("approved_plan_ref"), "approved-plan")
        workspace_ref = _physical_ref(values.get("workspace_plan_ref"), "workspace-plan")
        objective = values.get("objective")
        objective_valid = (
            isinstance(objective, Mapping)
            and set(objective) == {"path", "digest", "approval_status"}
            and isinstance(objective.get("path"), str)
            and bool(objective["path"])
            and _DIGEST.fullmatch(str(objective.get("digest"))) is not None
            and objective.get("approval_status") in {"approved", "candidate", "unapproved"}
        )
        status = objective["approval_status"] if objective_valid else "unapproved"
        required = {"approved_plan_ref", "objective", "workspace_plan_ref"}
        shapes_valid = set(values) == required and plan_ref is not None and workspace_ref is not None and objective_valid
        reason = "objective-not-approved" if shapes_valid and status != "approved" else (
            "ok" if shapes_valid else "invalid-input"
        )
        refs = [item for item in [plan_ref] if item is not None]
        if objective_valid:
            refs.append({"kind": "objective", "path": objective["path"], "digest": objective["digest"]})
        if workspace_ref is not None:
            refs.append(workspace_ref)
        return self._result(
            "initialize_run_plan", qualified_id, reason,
            {"kind": "run-plan", "details": {"objective_status": status}}, refs, authority_ref, head,
        )

    def plan_legacy_detach(
        self,
        qualified_id: str,
        inputs: Mapping[str, Any],
        authority: Mapping[str, Any],
        expected_head: Mapping[str, Any],
    ) -> Dict[str, Any]:
        head, authority_ref = self._context("plan_legacy_detach", qualified_id, authority, expected_head)
        if not _is_json_value(inputs):
            return self._result(
                "plan_legacy_detach", qualified_id, "invalid-input", self._legacy_artifact(), [], authority_ref, head,
            )
        values = inputs if isinstance(inputs, Mapping) else {}
        inventory_ref = _physical_ref(values.get("inventory_ref"), "inventory")
        rollback_ref = _physical_ref(values.get("rollback_ref"), "rollback-plan")
        a6_refs = _physical_refs(values.get("a6_input_refs"), "a6-input", non_empty=False)
        receipt = values.get("non_use_authority")
        receipt_valid = (
            isinstance(receipt, Mapping)
            and set(receipt) == {"path", "digest"}
            and isinstance(receipt.get("path"), str)
            and bool(receipt["path"])
            and receipt["path"] != "candidate-generic"
            and _DIGEST.fullmatch(str(receipt.get("digest"))) is not None
        )
        required = {"inventory_ref", "rollback_ref", "a6_input_refs", "non_use_authority"}
        shapes_valid = (
            set(values) == required
            and inventory_ref is not None
            and rollback_ref is not None
            and a6_refs is not None
            and bool(a6_refs)
            and receipt_valid
        )
        reason = "missing-non-use-authority" if not receipt_valid else (
            "ok" if shapes_valid else "invalid-input"
        )
        refs = [item for item in [inventory_ref, rollback_ref] if item is not None]
        refs.extend(a6_refs or [])
        if receipt_valid:
            refs.append({"kind": "non-use-authority", "path": receipt["path"], "digest": receipt["digest"]})
        artifact = self._legacy_artifact(
            receipt["path"] if receipt_valid else "missing",
            [item["path"] for item in (a6_refs or [])],
        )
        return self._result("plan_legacy_detach", qualified_id, reason, artifact, refs, authority_ref, head)

    def compile(
        self,
        operation: str,
        qualified_id: str,
        inputs: Mapping[str, Any],
        authority: Mapping[str, Any],
        expected_head: Mapping[str, Any],
    ) -> Dict[str, Any]:
        operations = {
            "classify_environment": self.classify_environment,
            "plan_workspace": self.plan_workspace,
            "design_rollback": self.design_rollback,
            "initialize_run_plan": self.initialize_run_plan,
            "plan_legacy_detach": self.plan_legacy_detach,
        }
        method = operations.get(operation)
        if method is None:
            raise BootstrapContractError("unknown Bootstrap operation")
        return method(qualified_id, inputs, authority, expected_head)

    @staticmethod
    def _context(
        operation: str,
        qualified_id: str,
        authority: Mapping[str, Any],
        expected_head: Mapping[str, Any],
    ) -> tuple[Dict[str, Any], Dict[str, Any]]:
        if qualified_id != _OPERATION_IDS[operation]:
            raise BootstrapContractError("%s requires qualified selector %s" % (operation, _OPERATION_IDS[operation]))
        head = _head(expected_head)
        return head, _authority(authority, head)

    @staticmethod
    def _workspace_artifact(
        base_ref: str = "invalid",
        dirty_paths: Optional[list[str]] = None,
        write_scope: Optional[list[str]] = None,
        local_inputs: Optional[list[str]] = None,
        rollback_ref: str = "missing",
    ) -> Dict[str, Any]:
        return {
            "kind": "workspace-plan",
            "details": {
                "base_ref": base_ref,
                "dirty_paths": copy.deepcopy(dirty_paths or []),
                "write_scope": copy.deepcopy(write_scope or []),
                "local_inputs": copy.deepcopy(local_inputs or []),
                "rollback_ref": rollback_ref,
            },
        }

    @staticmethod
    def _legacy_artifact(
        authority_path: str = "missing", a6_input_paths: Optional[list[str]] = None
    ) -> Dict[str, Any]:
        return {
            "kind": "legacy-detach-plan",
            "details": {
                "non_use_authority": authority_path,
                "a6_input_refs": copy.deepcopy(a6_input_paths or []),
            },
        }

    @staticmethod
    def _result(
        operation: str,
        qualified_id: str,
        reason_code: str,
        artifact: Mapping[str, Any],
        input_refs: list[Mapping[str, str]],
        authority_ref: Mapping[str, Any],
        expected_head: Mapping[str, Any],
    ) -> Dict[str, Any]:
        result = {
            "schema": "bootstrap-artifact/v1",
            "operation": operation,
            "qualified_id": qualified_id,
            "outcome": {"status": "completed" if reason_code == "ok" else "refused", "reason_code": reason_code},
            "artifact": copy.deepcopy(dict(artifact)),
            "input_refs": copy.deepcopy(input_refs),
            "authority_ref": copy.deepcopy(dict(authority_ref)),
            "expected_head": copy.deepcopy(dict(expected_head)),
        }
        with _SCHEMA_PATH.open(encoding="utf-8") as source:
            validate_document(result, json.load(source))
        return result


__all__ = ["BootstrapContractError", "BootstrapContractsV1"]
