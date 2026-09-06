"""Stateless B1--B7 ObjectiveSystemV1 compiler.

This module deliberately compiles candidates and refusals only.  It owns no
Run state, storage, pointer, Kernel invocation, or approval side effect.
"""
from __future__ import annotations

import copy
import json
from pathlib import Path
from typing import Any, Mapping

from .schema_validation import SchemaValidationError, validate_document


_ROOT = Path(__file__).resolve().parents[2]
_SCHEMA = json.loads((_ROOT / "schemas" / "objective-system-v1.schema.json").read_text())
_IDS = {"group.B.B%d" % index for index in range(1, 8)}


class ObjectiveSystemError(ValueError):
    """Inputs do not provide the physical authority required to compile."""


def _validate(value: Any, schema: Mapping[str, Any] = _SCHEMA) -> None:
    """Validate local references using the repository's dependency-free validator."""
    validate_document(value, schema, _SCHEMA["$defs"])


class ObjectiveSystemV1:
    """Pure dispatcher for the seven Group B selectors."""

    def compile(self, qualified_id: str, inputs: Mapping[str, Any], authority: Mapping[str, Any], expected_head: Mapping[str, Any]) -> dict[str, Any]:
        values = copy.deepcopy(dict(inputs)) if isinstance(inputs, Mapping) else {}
        auth = copy.deepcopy(dict(authority)) if isinstance(authority, Mapping) else authority
        head = copy.deepcopy(dict(expected_head)) if isinstance(expected_head, Mapping) else expected_head
        if qualified_id not in _IDS:
            return self._refusal("group.B.B1", "blocked_missing_authority", auth, head)
        self._context(auth, head)
        observed = values.get("observed_head")
        if observed is not None and observed != head:
            return self._refusal(qualified_id, "blocked_stale_head", auth, head)
        if qualified_id == "group.B.B1" and values.get("active_duplicate"):
            return self._refusal(qualified_id, "needs_user_duplicate_run", auth, head)
        if qualified_id == "group.B.B2" and any(not isinstance(item, Mapping) or not self._physical(item.get("source_ref")) for item in values.get("facts", [])):
            return self._refusal(qualified_id, "blocked_missing_authority", auth, head)
        if qualified_id == "group.B.B3" and (values.get("owner") == "unknown" or (values.get("operation") == "quick" and values.get("reversibility") == "risky")):
            return self._refusal(qualified_id, "blocked_missing_authority", auth, head)
        if qualified_id == "group.B.B4" and len(values.get("material_unknowns", [])) != 1:
            return self._refusal(qualified_id, "needs_user_purpose", auth, head)
        if qualified_id == "group.B.B5":
            options = values.get("options", [])
            distinguishable = isinstance(options, list) and 2 <= len(options) <= 3 and len({item.get("tradeoff") for item in options if isinstance(item, Mapping)}) == len(options)
            if values.get("selected_option") is not None:
                return self._refusal(qualified_id, "blocked_implicit_approval", auth, head)
            if not distinguishable:
                return self._refusal(qualified_id, "needs_user_purpose", auth, head)
        if qualified_id == "group.B.B6" and values.get("owner") == "unknown":
            return self._refusal(qualified_id, "blocked_missing_authority", auth, head)
        if qualified_id == "group.B.B7":
            return self._approval(values, auth, head)
        selectors = {
            "group.B.B1": "B1.raw-interpretation-assumption-unknown", "group.B.B2": "B2.sourced-context",
            "group.B.B3": "B3.scope-classified", "group.B.B4": "B4.one-material-question",
            "group.B.B5": "B5.unselected-options", "group.B.B6": "B6.constraints-separated",
        }
        return self._artifact(qualified_id, values, auth, head, selectors[qualified_id])

    def _approval(self, values: Mapping[str, Any], authority: Mapping[str, Any], head: Mapping[str, Any]) -> dict[str, Any]:
        receipt = values.get("approval_receipt")
        if not isinstance(receipt, Mapping) or receipt.get("source") != "human" or receipt.get("explicit") is not True or receipt.get("decision") != "approve":
            return self._refusal("group.B.B7", "blocked_implicit_approval", authority, head)
        if values.get("approval_scope") != "fixture-only" or not isinstance(values.get("namespace"), str) or not values["namespace"].startswith("fixture:"):
            return self._refusal("group.B.B7", "blocked_fixture_live", authority, head)
        if not self._physical(receipt.get("receipt_ref")) or receipt.get("actor_ref") != authority.get("owner_ref"):
            return self._refusal("group.B.B7", "blocked_missing_authority", authority, head)
        candidate = values.get("candidate")
        prior = values.get("prior_objective")
        if not isinstance(candidate, Mapping) or not isinstance(prior, Mapping) or candidate.get("namespace") != values["namespace"]:
            return self._refusal("group.B.B7", "blocked_fixture_live", authority, head)
        return self._command(values, authority, head)

    def _artifact(self, qualified_id: str, values: Mapping[str, Any], authority: Mapping[str, Any], head: Mapping[str, Any], selector: str) -> dict[str, Any]:
        inputs, candidate = self._parts(values, selector)
        result = {"schema": "objective-system-artifact/v1", "qualified_id": qualified_id, "inputs": inputs, "authority": copy.deepcopy(authority), "expected_head": copy.deepcopy(head), "candidate": candidate}
        _validate(result)
        return result

    def _command(self, values: Mapping[str, Any], authority: Mapping[str, Any], head: Mapping[str, Any]) -> dict[str, Any]:
        inputs, candidate = self._parts(values, "B7.explicit-fixture-approval")
        result = {"schema": "objective-approval-command/v1", "qualified_id": "group.B.B7", "inputs": inputs, "authority": copy.deepcopy(authority), "expected_head": copy.deepcopy(head), "candidate": candidate, "approval_receipt_ref": copy.deepcopy(values["approval_receipt"])}
        _validate(result)
        return result

    def _parts(self, values: Mapping[str, Any], selector: str) -> tuple[list[dict[str, Any]], dict[str, Any]]:
        refs = values.get("input_refs")
        candidate_ref = values.get("candidate_ref")
        if not isinstance(refs, list) or not refs or not all(self._physical(item) for item in refs) or not self._physical(candidate_ref):
            raise ObjectiveSystemError("input_refs and candidate_ref must be physical references")
        candidate = {"status": "candidate", "candidate_ref": copy.deepcopy(candidate_ref), "version": values.get("version", "v001")}
        candidate["candidate_ref"]["selector"] = selector
        return copy.deepcopy(refs), candidate

    @staticmethod
    def _physical(value: Any) -> bool:
        try:
            _validate(value, _SCHEMA["$defs"]["physicalRef"])
            return True
        except SchemaValidationError:
            return False

    @staticmethod
    def _context(authority: Any, head: Any) -> None:
        try:
            _validate(authority, _SCHEMA["$defs"]["authority"])
            _validate(head, _SCHEMA["$defs"]["head"])
        except SchemaValidationError as error:
            raise ObjectiveSystemError(str(error)) from error

    @staticmethod
    def _refusal(qualified_id: str, reason: str, authority: Any, head: Any) -> dict[str, Any]:
        result = {"schema": "objective-system-refusal/v1", "qualified_id": qualified_id, "reason": reason, "expected_head": copy.deepcopy(head), "authority": copy.deepcopy(authority)}
        _validate(result)
        return result


__all__ = ["ObjectiveSystemError", "ObjectiveSystemV1"]
