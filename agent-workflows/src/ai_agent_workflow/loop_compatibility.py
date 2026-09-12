"""Read-only planning for legacy budget state to loop-control state."""
from __future__ import annotations

import copy
from collections.abc import Mapping, Sequence
from typing import Any

from .loop_contracts import (
    LoopContractError,
    canonical_digest,
    counter_identity,
    phase_policy,
    validate_iteration_event,
    validate_ref,
    validate_work_identity,
)
from .loop_state import reduce_history, validate_history


class LoopCompatibilityError(LoopContractError):
    """Legacy input is corrupt or the requested transition is ambiguous."""


def _refusal(source_ref: dict[str, str], source_schema: str, reason: str) -> dict[str, Any]:
    result = {
        "schema": "loop-migration-plan/v1",
        "status": "migration-refused",
        "source_schema": source_schema,
        "source_ref": source_ref,
        "history_preserved": True,
        "authority_expanded": False,
        "reason": reason,
        "proposed_loop_state": None,
    }
    result["plan_digest"] = canonical_digest(result)
    return result


def plan_legacy_transition(
    legacy_state: Any,
    history: Sequence[Mapping[str, Any]],
    target_identity: Any,
    source_ref: Any,
) -> dict[str, Any]:
    """Return a non-mutating transition plan or a typed refusal.

    The caller must separately authorize and apply a returned plan.  This
    function never rewrites the supplied legacy object, resets counters, or
    widens authority inherited from that object.
    """
    if not isinstance(legacy_state, Mapping) or not isinstance(legacy_state.get("schema"), str):
        raise LoopCompatibilityError("legacy_state must have a schema")
    state = copy.deepcopy(dict(legacy_state))
    state_before = copy.deepcopy(state)
    legacy = validate_ref(source_ref, "source_ref")
    if legacy["digest"] != canonical_digest(state):
        raise LoopCompatibilityError("source_ref does not bind the supplied legacy state")
    identity = validate_work_identity(target_identity)
    if not isinstance(history, Sequence) or isinstance(history, (str, bytes)):
        raise LoopCompatibilityError("history must be a sequence")
    try:
        events = validate_history(
            [validate_iteration_event(item) for item in history], identity=identity
        )
    except LoopContractError as error:
        raise LoopCompatibilityError("legacy history is invalid: " + str(error)) from error
    if len({item["event_id"] for item in events}) != len(events) or len({item["command_id"] for item in events}) != len(events):
        raise LoopCompatibilityError("history event and command IDs must be unique")
    if any(counter_identity(item["identity"]) != counter_identity(identity) for item in events):
        raise LoopCompatibilityError("history belongs to another logical counter")

    source_schema = state["schema"]
    if source_schema == "dag-state/v1" and isinstance(state.get("loop_control"), Mapping):
        current = state["loop_control"]
        try:
            current_identity = validate_work_identity(current.get("identity"))
            expected = reduce_history(events, identity=current_identity)
        except LoopContractError as error:
            return _refusal(legacy, source_schema, "malformed-current-loop-control:" + str(error))
        required_current = {
            "schema", "identity", "event_refs", "archives", "counter_identity", "counters",
            "status", "outcome", "terminal_outcome", "recovery_required", "dispatch_allowed",
            "recovery", "control_refs", "terminal_ref", "terminal_record",
        }
        if (
            set(current) != required_current
            or current.get("schema") != "loop-control-state/v1"
            or current_identity != identity
            or current.get("counter_identity") != counter_identity(current_identity)
            or not isinstance(current.get("event_refs"), list)
            or len(current["event_refs"]) != len(events)
            or current.get("counters") != expected["counters"]
            or not isinstance(current.get("archives"), list)
            or not isinstance(current.get("control_refs"), list)
            or (current.get("terminal_ref") is None) != (current.get("terminal_record") is None)
        ):
            return _refusal(legacy, source_schema, "malformed-current-loop-control")
        result = {
            "schema": "loop-migration-plan/v1", "status": "already-current",
            "source_schema": source_schema, "source_ref": legacy,
            "history_preserved": True, "authority_expanded": False,
            "reason": "loop-control-already-present", "proposed_loop_state": copy.deepcopy(current),
        }
        result["plan_digest"] = canonical_digest(result)
        return result
    if source_schema != "dag-state/v1":
        return _refusal(legacy, source_schema, "unsupported-legacy-state")
    if state.get("budget_terminal") is not None:
        return _refusal(legacy, source_schema, "legacy-terminal-requires-human-decision")
    budget = state.get("review_budget")
    if not isinstance(budget, Mapping):
        return _refusal(legacy, source_schema, "missing-legacy-review-history")
    rounds = budget.get("rounds_used")
    attempts = budget.get("finding_attempts")
    if (
        not isinstance(rounds, int)
        or isinstance(rounds, bool)
        or rounds < 0
        or not isinstance(attempts, Mapping)
        or any(
            not isinstance(value, int) or isinstance(value, bool) or value < 0
            for value in attempts.values()
        )
    ):
        return _refusal(legacy, source_schema, "malformed-legacy-review-history")
    active_legacy = rounds > 0 or any(isinstance(value, int) and value > 0 for value in attempts.values())
    if active_legacy and not events:
        return _refusal(legacy, source_schema, "insufficient-iteration-history")

    initial_count = sum(item["kind"] == "initial" for item in events)
    if events and initial_count != 1:
        return _refusal(legacy, source_schema, "ambiguous-initial-attempt-history")
    additional = sum(item["kind"] in {"improvement", "integration-return"} for item in events)
    technical = sum(item["kind"] == "technical-retry" for item in events)
    if active_legacy and additional < max(rounds, max((value for value in attempts.values() if isinstance(value, int)), default=0)):
        return _refusal(legacy, source_schema, "legacy-counters-exceed-bound-history")
    policy = phase_policy(identity["phase"])
    if additional > policy["additional_iteration_limit"] or technical > policy["technical_retry_limit"]:
        return _refusal(legacy, source_schema, "target-policy-already-exhausted")

    proposed = {
        "schema": "loop-control-migration-candidate/v1",
        "contract_version": "workflow-loop/v1",
        "identity": identity,
        "counter_identity": counter_identity(identity),
        "policy": policy,
        "initial_attempt_recorded": initial_count == 1,
        "additional_iterations_used": additional,
        "technical_retries_used": technical,
        "event_refs": [
            {"id": item["event_id"], "digest": canonical_digest(item)}
            for item in events
        ],
        "legacy_source_ref": legacy,
        "legacy_budget_disposition": "preserved-read-only-not-used-for-progress",
        "status": "migration-candidate",
    }
    proposed["state_digest"] = canonical_digest(proposed)
    result = {
        "schema": "loop-migration-plan/v1",
        "status": "transition-ready",
        "source_schema": source_schema,
        "source_ref": legacy,
        "history_preserved": True,
        "authority_expanded": False,
        "reason": "exact-history-mapped",
        "proposed_loop_state": proposed,
    }
    result["plan_digest"] = canonical_digest(result)
    if state != state_before:
        raise LoopCompatibilityError("legacy state was mutated")
    return result


__all__ = ["LoopCompatibilityError", "plan_legacy_transition"]
