"""Read-only S1 composition and thin lifecycle delegation."""
from __future__ import annotations

import copy
import hashlib
import json
from pathlib import Path
from typing import Any, Dict, Mapping, Optional

from .schema_validation import SchemaValidationError, validate_document
from .section_control_plane import (
    SectionControlPlaneV1,
    open_group,
    open_section,
    resume as resume_section,
)


_A_IDS = tuple("group.A." + item for item in ("A1", "A2", "A3", "A4", "A5", "A6", "A6R", "A7"))
_F_IDS = tuple("group.F.F%d" % value for value in range(1, 9))
_A_OPERATIONS = {
    "A1": "BootstrapContractsV1.classify_environment",
    "A2": "BootstrapContractsV1.plan_workspace",
    "A3": "BootstrapContractsV1.design_rollback",
    "A4": "BootstrapContractsV1.initialize_run_plan",
    "A5": "BootstrapContractsV1.plan_legacy_detach",
}


class CompositionContractError(ValueError):
    """A physical S1 composition input is incomplete or cross-bound."""


def _digest(path: Path) -> str:
    try:
        value = path.read_bytes()
    except OSError as error:
        raise CompositionContractError("composition path is unreadable: %s" % path) from error
    return "sha256:" + hashlib.sha256(value).hexdigest()


def _load_json(path: Path) -> Mapping[str, Any]:
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, ValueError) as error:
        raise CompositionContractError("composition JSON is unreadable: %s" % path) from error
    if not isinstance(value, Mapping):
        raise CompositionContractError("composition JSON must be an object: %s" % path)
    return value


class BootstrapLifecycleV1:
    """Consume fixed manifests without owning Kernel state or lifecycle policy."""

    def __init__(self, agent_workflows_root: Optional[Path] = None) -> None:
        self.root = (agent_workflows_root or Path(__file__).resolve().parents[2]).resolve()
        self.repository_root = self.root.parent

    def load_group_manifests(
        self,
        bootstrap: Optional[Mapping[str, Any]] = None,
        shared: Optional[Mapping[str, Any]] = None,
    ) -> Dict[str, Any]:
        bootstrap_value = copy.deepcopy(dict(bootstrap)) if isinstance(bootstrap, Mapping) else _load_json(self.root / "groups" / "bootstrap.json")
        shared_value = copy.deepcopy(dict(shared)) if isinstance(shared, Mapping) else _load_json(self.root / "groups" / "shared-closure.json")
        try:
            validate_document(bootstrap_value, _load_json(self.root / "schemas" / "group-manifest-v1.schema.json"))
            validate_document(shared_value, _load_json(self.root / "schemas" / "shared-closure-manifest-v1.schema.json"))
        except SchemaValidationError as error:
            raise CompositionContractError("group manifest schema rejection: %s" % error) from error

        contracts = bootstrap_value["contracts"]
        qualified = [item["qualified_id"] for item in contracts]
        locals_ = [item["local_id"] for item in contracts]
        if qualified != list(_A_IDS) or locals_ != [item.rsplit(".", 1)[1] for item in _A_IDS]:
            raise CompositionContractError("Bootstrap contracts must be the canonical ordered qualified map")
        if shared_value["selectors"] != list(_F_IDS):
            raise CompositionContractError("shared closure selectors must be canonical qualified IDs")
        all_ids = qualified + list(shared_value["selectors"])
        if len(all_ids) != 16 or len(set(all_ids)) != 16:
            raise CompositionContractError("the two manifests must map 16 qualified IDs exactly once")

        retained: Dict[str, str] = {}
        for item in contracts:
            local_id = item["local_id"]
            if item["qualified_id"] != "group.A." + local_id:
                raise CompositionContractError("qualified and local contract identities are cross-bound")
            source_ref = item["source_ref"]
            selector_ref = item["selector_ref"]
            for label, reference in (("source", source_ref), ("selector", selector_ref)):
                path = self.repository_root / reference["path"]
                if not path.is_file() or path.is_symlink() or _digest(path) != reference["digest"]:
                    raise CompositionContractError("%s bytes do not match %s" % (label, item["qualified_id"]))
            if local_id in _A_OPERATIONS:
                if (item["status"] != "candidate-source" or item["receipt_ref"] is not None
                        or selector_ref["path"] != "agent-workflows/src/ai_agent_workflow/bootstrap_contracts.py"
                        or selector_ref["selector"] != _A_OPERATIONS[local_id]):
                    raise CompositionContractError("current Bootstrap selector/source mapping is invalid")
                continue
            receipt_ref = item["receipt_ref"]
            if item["status"] != "retained-accepted" or not isinstance(receipt_ref, Mapping):
                raise CompositionContractError("retained contract requires its accepted receipt")
            receipt_path = self.repository_root / receipt_ref["path"]
            if not receipt_path.is_file() or receipt_path.is_symlink() or _digest(receipt_path) != receipt_ref["digest"]:
                raise CompositionContractError("retained receipt bytes do not match %s" % item["qualified_id"])
            receipt = _load_json(receipt_path)
            if (receipt.get("schema") != "agent-workflow-contract-acceptance/v1"
                    or receipt.get("contract_id") != local_id or receipt.get("status") != "passed"
                    or receipt.get("implementation_ref") != source_ref
                    or not isinstance(receipt.get("test_ref"), Mapping)
                    or receipt["test_ref"].get("path") != selector_ref["path"]
                    or receipt["test_ref"].get("digest") != selector_ref["digest"]
                    or receipt["test_ref"].get("selector") != selector_ref["selector"]):
                raise CompositionContractError("retained receipt is cross-bound: %s" % item["qualified_id"])
            retained[item["qualified_id"]] = receipt_ref["digest"]

        shared_path = self.repository_root / shared_value["source"]["path"]
        if not shared_path.is_file() or shared_path.is_symlink():
            raise CompositionContractError("shared closure source is not a regular file")
        return {
            "schema": "s1-group-composition/v1",
            "qualified_ids": all_ids,
            "retained_receipt_digests": retained,
            "shared_source_digest": _digest(shared_path),
        }

    def open_s1(self, kernel: Any, section_plan: Mapping[str, Any]) -> Dict[str, Any]:
        """Compile and delegate one S1 open; the Kernel remains sole state owner."""
        self.load_group_manifests()
        if not isinstance(section_plan, Mapping) or section_plan.get("section", {}).get("id") != "S1":
            raise CompositionContractError("S1 lifecycle requires the S1 section plan")
        compiled = SectionControlPlaneV1().compile_registry(section_plan)
        return open_section(kernel, compiled["open_section"])

    def apply_closure_result(self, kernel: Any, result: Mapping[str, Any]) -> Dict[str, Any]:
        """Delegate one F6/F7 command compiled by the accepted shared owner."""
        try:
            validate_document(result, _load_json(self.root / "schemas" / "closure-operation-result-v1.schema.json"))
        except SchemaValidationError as error:
            raise CompositionContractError("closure result schema rejection: %s" % error) from error
        selector = result.get("selector") if isinstance(result, Mapping) else None
        command = result.get("result", {}).get("command") if isinstance(result.get("result"), Mapping) else None
        expected_operation = {"group.F.F6": "close_epoch", "group.F.F7": "close_group"}.get(selector)
        if (expected_operation is None or not isinstance(command, Mapping)
                or command.get("command_type") != expected_operation
                or command.get("expected_head") != result.get("expected_head")):
            raise CompositionContractError("closure selector and Kernel command are cross-bound")
        return kernel.apply(command)

    def open_next_group(self, kernel: Any, section_plan: Mapping[str, Any]) -> Dict[str, Any]:
        """Compile and delegate one later S1 Group open."""
        self.load_group_manifests()
        if not isinstance(section_plan, Mapping) or section_plan.get("section", {}).get("id") != "S1":
            raise CompositionContractError("later Bootstrap lifecycle requires the S1 section plan")
        command = SectionControlPlaneV1().compile_open_group(section_plan)
        return open_group(kernel, command)

    @staticmethod
    def resume(kernel: Any, expected_head: Mapping[str, Any]) -> Dict[str, Any]:
        """Read the canonical HEAD chain through the existing S0 resume seam."""
        return resume_section(kernel, expected_head)


__all__ = ["BootstrapLifecycleV1", "CompositionContractError"]
