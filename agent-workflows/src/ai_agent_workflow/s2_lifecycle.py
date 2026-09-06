"""Fixture-only B7 adapter for the single ControlKernel state owner.

``ObjectiveSystemV1`` emits a schema-validated command candidate containing
physical references.  This adapter resolves only its dedicated disposable
fixture, validates every binding before side effects, then invokes
``ControlKernel.apply`` exactly once.  It deliberately owns no state, pointer,
reducer, retry store, or live conversion path.
"""
from __future__ import annotations

import copy
import hashlib
import json
from pathlib import Path
from typing import Any, Mapping

from .schema_validation import SchemaValidationError, validate_document


_ROOT = Path(__file__).resolve().parents[2]
_OBJECTIVE_SCHEMA = json.loads((_ROOT / "schemas" / "objective-system-v1.schema.json").read_text())
_FIXTURE_ROOT = _ROOT / "tests" / "fixtures" / "s2" / "objective" / "lifecycle"


class LifecycleAdapterError(ValueError):
    """The candidate is not an isolated, fully-bound fixture approval."""


def _digest_bytes(value: bytes) -> str:
    return "sha256:" + hashlib.sha256(value).hexdigest()


def _copy(value: Any) -> Any:
    return copy.deepcopy(value)


def _require(value: Any, message: str) -> None:
    if not value:
        raise LifecycleAdapterError(message)


class S2LifecycleAdapter:
    """Validate a B7 fixture command and delegate its one atomic transition."""

    def apply(self, command: Mapping[str, Any], kernel: Any) -> dict[str, Any]:
        """Apply one eligible fixture command, or fail before ``kernel.apply``.

        ``kernel`` must provide ``make_command`` and ``apply``.  The adapter
        intentionally does not retry: idempotency belongs to ControlKernel and
        a changed payload is rejected before delegation.
        """
        objective_command = self._validate_command(command)
        fixture = self._load_fixture(objective_command["candidate"]["candidate_ref"])
        payload, authority = self._validate_fixture(objective_command, fixture)
        envelope = kernel.make_command(
            "approve_objective",
            payload,
            authority_ref=_copy(authority),
            expected_head=_copy(objective_command["expected_head"]),
            idempotency_key=fixture["idempotency_key"],
            protected_fields=["objective_ref"],
            scope=[fixture["namespace"]],
        )
        result = kernel.apply(envelope)
        return self._validate_result(result, payload, fixture)

    @staticmethod
    def _validate_command(command: Mapping[str, Any]) -> dict[str, Any]:
        if not isinstance(command, Mapping):
            raise LifecycleAdapterError("objective command must be an object")
        try:
            validate_document(command, _OBJECTIVE_SCHEMA, _OBJECTIVE_SCHEMA["$defs"])
        except SchemaValidationError as error:
            raise LifecycleAdapterError("objective command is not schema-valid") from error
        if command.get("schema") != "objective-approval-command/v1" or command.get("qualified_id") != "group.B.B7":
            raise LifecycleAdapterError("only a B7 objective approval command is eligible")
        candidate = command.get("candidate")
        receipt = command.get("approval_receipt_ref")
        if not isinstance(candidate, Mapping) or not isinstance(receipt, Mapping):
            raise LifecycleAdapterError("approval command is incomplete")
        if receipt.get("source") != "human" or receipt.get("explicit") is not True or receipt.get("decision") != "approve":
            raise LifecycleAdapterError("implicit, AI, or task-start approval is forbidden")
        return _copy(dict(command))

    @staticmethod
    def _load_fixture(ref: Mapping[str, Any]) -> dict[str, Any]:
        path = ref.get("path") if isinstance(ref, Mapping) else None
        if not isinstance(path, str) or not path.startswith("agent-workflows/tests/fixtures/s2/objective/lifecycle/"):
            raise LifecycleAdapterError("candidate must reference a lifecycle fixture")
        fixture_path = (_ROOT.parent / path).resolve()
        if _FIXTURE_ROOT not in fixture_path.parents or fixture_path.suffix != ".json":
            raise LifecycleAdapterError("candidate fixture path escapes its namespace")
        try:
            raw = fixture_path.read_bytes()
            fixture = json.loads(raw)
        except (OSError, json.JSONDecodeError) as error:
            raise LifecycleAdapterError("candidate fixture is unreadable") from error
        if _digest_bytes(raw) != ref.get("digest"):
            raise LifecycleAdapterError("candidate fixture digest is stale")
        if not isinstance(fixture, Mapping):
            raise LifecycleAdapterError("candidate fixture must be an object")
        return _copy(dict(fixture))

    @staticmethod
    def _validate_fixture(command: Mapping[str, Any], fixture: Mapping[str, Any]) -> tuple[dict[str, Any], dict[str, Any]]:
        required = {"schema", "fixture_id", "namespace", "approval_scope", "idempotency_key", "payload", "authority", "expected_result"}
        if set(fixture) != required or fixture.get("schema") != "s2-lifecycle-fixture/v1":
            raise LifecycleAdapterError("lifecycle fixture has an unsupported shape")
        namespace = fixture.get("namespace")
        _require(isinstance(namespace, str) and namespace == "fixture:" + fixture.get("fixture_id"), "fixture namespace is malformed")
        _require(fixture.get("approval_scope") == "fixture-only", "fixture-only scope is required")
        _require(isinstance(fixture.get("idempotency_key"), str) and fixture["idempotency_key"], "fixture idempotency key is required")
        payload = fixture.get("payload")
        authority = fixture.get("authority")
        if not isinstance(payload, Mapping) or set(payload) != {"candidate_ref", "prior_objective", "proposal_digest", "approval"}:
            raise LifecycleAdapterError("fixture approval payload is malformed")
        if not isinstance(authority, Mapping):
            raise LifecycleAdapterError("fixture authority is malformed")
        approval = payload.get("approval")
        candidate = payload.get("candidate_ref")
        receipt = approval.get("receipt") if isinstance(approval, Mapping) else None
        command_candidate = command.get("candidate", {})
        command_receipt = command.get("approval_receipt_ref", {})
        _require(isinstance(candidate, Mapping) and candidate.get("namespace") == namespace, "candidate namespace is not fixture-bound")
        _require(fixture["idempotency_key"] == "fixture-%s-%s" % (fixture["fixture_id"], candidate.get("version")), "idempotency key does not bind the candidate payload")
        _require(isinstance(approval, Mapping) and approval.get("namespace") == namespace and approval.get("approval_scope") == "fixture-only", "approval is not fixture-only")
        _require(isinstance(receipt, Mapping) and receipt.get("source") == "human" and receipt.get("explicit") is True and receipt.get("decision") == "approve", "explicit human receipt is required")
        _require(candidate.get("version") == command_candidate.get("version"), "candidate version changed after compilation")
        command_candidate_ref = command_candidate.get("candidate_ref", {})
        receipt_ref = command_receipt.get("receipt_ref", {})
        _require(command_candidate_ref.get("path") == receipt_ref.get("path") and command_candidate_ref.get("digest") == receipt_ref.get("digest"), "candidate and receipt must bind the same immutable fixture")
        _require(command_receipt.get("actor_ref") == command.get("authority", {}).get("owner_ref"), "compiler receipt actor is not authority-bound")
        _require(receipt.get("candidate_digest") == candidate.get("digest") and receipt.get("candidate_version") == candidate.get("version") and receipt.get("candidate_namespace") == namespace, "receipt candidate binding changed")
        _require(approval.get("candidate_digest") == candidate.get("digest") and approval.get("candidate_version") == candidate.get("version"), "approval candidate binding changed")
        _require(approval.get("proposal_digest") == payload.get("proposal_digest") and receipt.get("proposal_digest") == payload.get("proposal_digest"), "proposal binding changed")
        _require(approval.get("prior_objective_digest") == payload["prior_objective"].get("digest") and approval.get("prior_objective_version") == payload["prior_objective"].get("version"), "prior objective binding changed")
        _require(approval.get("run_id") == receipt.get("run_id") and approval.get("actor", {}).get("actor_id") == receipt.get("actor_id"), "approval receipt authority changed")
        _require(authority.get("namespace") == namespace and authority.get("approval_scope") == "fixture-only" and authority.get("proposal_digest") == payload.get("proposal_digest") and authority.get("human_receipt") == receipt, "authority binding changed")
        _require(authority.get("actor_id") == approval.get("actor", {}).get("actor_id") and authority.get("run_id") == approval.get("run_id"), "authority actor binding changed")
        return _copy(dict(payload)), _copy(dict(authority))

    @staticmethod
    def _validate_result(result: Any, payload: Mapping[str, Any], fixture: Mapping[str, Any]) -> dict[str, Any]:
        if not isinstance(result, Mapping):
            raise LifecycleAdapterError("kernel returned a malformed result")
        if result.get("accepted") is False:
            if not isinstance(result.get("reason"), str) or not result["reason"]:
                raise LifecycleAdapterError("kernel refusal is malformed")
            return {"schema": "s2-lifecycle-refusal/v1", "reason": result["reason"]}
        expected = fixture["expected_result"]
        objective = result.get("objective_ref")
        if (not isinstance(expected, Mapping) or not isinstance(objective, Mapping)
                or objective.get("version") != expected.get("approved_version")
                or objective.get("digest") != payload["candidate_ref"].get("digest")):
            raise LifecycleAdapterError("kernel result does not prove the approved fixture objective")
        return _copy(dict(result))


__all__ = ["LifecycleAdapterError", "S2LifecycleAdapter"]
