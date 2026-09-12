"""Pure policy and decision helpers for the workflow improvement loop.

The durable control kernel owns the event log.  This module only derives
values from supplied contract objects: it does not read a clock, inspect the
filesystem, dispatch work, or persist anything.  In particular, counters are
keyed by :func:`counter_identity`; a renamed scope, candidate, or session does
not reset them.
"""
from __future__ import annotations

import copy
from collections.abc import Iterable, Mapping, Sequence
from typing import Any

from .loop_contracts import (
    TERMINAL_OUTCOMES,
    LoopContractError,
    counter_identity,
    phase_policy,
    validate_iteration_event,
)

OUTCOMES = tuple(TERMINAL_OUTCOMES) + ("continue",)
_COUNTED_ITERATION_KINDS = frozenset({"improvement", "integration-return"})
_KNOWN_EVENT_CONTAINERS = ("events", "iteration_events", "state_entries", "entries")
_KNOWN_HISTORY_CONTAINERS = ("history", "states", "observations")
_COUNTER_FIELDS = ("work_lineage_id", "logical_task_id", "policy_id")
_REQUIRED_REVIEW_AXES = frozenset({"architecture-safety", "integration-operability"})
_PASS_STATUSES = frozenset({"pass", "passed", "success", "succeeded", "complete", "completed"})
_FAIL_STATUSES = frozenset({"fail", "failed", "failure", "error", "errored"})
_UNKNOWN_STATUSES = frozenset({"unknown", "needs-input", "needs_input", "input-required", "input_required"})
_RECOVERY_STATUSES = frozenset({
    "unknown", "execution-unknown", "execution_unknown", "interrupted", "crashed",
    "aborted", "lost", "recovery-required", "recovery_required",
})

# The implementation plan names this seam for the later orchestration layer.
# A plain mapping keeps the A1 module serialisable without introducing a
# mutable state object or a persistence dependency.
Decision = dict[str, Any]


class LoopPolicyError(LoopContractError):
    """A value cannot be evaluated under the loop policy."""


def _policy_for(value: Any) -> dict[str, Any]:
    """Resolve a phase selector, identity, or already-resolved policy."""
    if isinstance(value, str):
        return phase_policy(value)
    if not isinstance(value, Mapping):
        raise LoopPolicyError("phase or policy is required")
    if "phase" in value and isinstance(value.get("phase"), str):
        return phase_policy(value["phase"])
    identity = value.get("identity")
    if isinstance(identity, Mapping) and isinstance(identity.get("phase"), str):
        return phase_policy(identity["phase"])
    if {
        "policy_id", "phase", "additional_iteration_limit", "technical_retry_limit",
        "counter_scope",
    }.issubset(value):
        policy = dict(value)
        for field in ("additional_iteration_limit", "technical_retry_limit"):
            limit = policy.get(field)
            if not isinstance(limit, int) or isinstance(limit, bool) or limit < 0:
                raise LoopPolicyError(field + " must be a non-negative integer")
        return copy.deepcopy(policy)
    raise LoopPolicyError("phase or policy is required")


def phase_limits(phase: Any) -> dict[str, Any]:
    """Return the immutable policy record for a concrete workflow phase.

    The result has the same shape as ``loop_contracts.phase_policy``.  Keeping
    this lookup here gives callers a policy-only dependency while preserving a
    single source of truth for the limits.
    """
    try:
        return _policy_for(phase)
    except LoopContractError as error:
        raise LoopPolicyError(str(error)) from error


def additional_iteration_limit(phase: Any) -> int:
    """Return only the additional-iteration limit for ``phase``."""
    return int(phase_limits(phase)["additional_iteration_limit"])


def technical_retry_limit(phase: Any) -> int:
    """Return only the technical-retry limit for ``phase``."""
    return int(phase_limits(phase)["technical_retry_limit"])


def _event_values(value: Any) -> list[dict[str, Any]]:
    """Extract and validate iteration events without changing the input.

    A state entry may wrap an event in ``event`` or place a list under one of
    the named containers.  The wire event itself remains strict and is always
    passed through ``validate_iteration_event``.
    """
    if value is None:
        return []
    if isinstance(value, Mapping):
        if value.get("schema") == "loop-iteration-event/v1":
            try:
                return [validate_iteration_event(value)]
            except LoopContractError as error:
                raise LoopPolicyError(str(error)) from error
        if "event" in value:
            result = _event_values(value["event"])
            if isinstance(value.get("identity"), Mapping):
                result = [
                    dict(event, identity=copy.deepcopy(value["identity"]))
                    for event in result
                ]
            return result
        for field in _KNOWN_EVENT_CONTAINERS:
            if field in value:
                result = _event_values(value[field])
                if isinstance(value.get("identity"), Mapping):
                    result = [
                        dict(event, identity=copy.deepcopy(value["identity"]))
                        for event in result
                    ]
                return result
        # A mapping of event IDs to events is a useful read-only projection of
        # state.  Do not mistake an arbitrary state mapping for that shape.
        if value and all(
            isinstance(item, Mapping) and item.get("schema") == "loop-iteration-event/v1"
            for item in value.values()
        ):
            try:
                return [validate_iteration_event(item) for item in value.values()]
            except LoopContractError as error:
                raise LoopPolicyError(str(error)) from error
        return []
    if isinstance(value, (str, bytes)):
        raise LoopPolicyError("iteration events must be event objects")
    try:
        values = list(value)
    except TypeError as error:
        raise LoopPolicyError("iteration events must be iterable") from error
    result: list[dict[str, Any]] = []
    for item in values:
        result.extend(_event_values(item))
    return result


def counter_key(identity: Any) -> tuple[str, str, str]:
    """Return a hashable counter key derived from the contract identity."""
    try:
        item = counter_identity(identity)
    except LoopContractError as error:
        raise LoopPolicyError(str(error)) from error
    return tuple(item[field] for field in _COUNTER_FIELDS)


def _event_key(event: Mapping[str, Any]) -> tuple[str, str, str]:
    try:
        return counter_key(event["identity"])
    except (KeyError, LoopContractError, TypeError) as error:
        raise LoopPolicyError("iteration event identity is invalid") from error


def _unique_events(events: Any) -> list[dict[str, Any]]:
    """Drop a replayed event only within its own counter identity.

    The same ``event_id``/``command_id`` can legitimately appear in separate
    task and integration state entries.  Such entries represent two counters
    and must both be charged.  A duplicate within one counter, however, is a
    replay and must not consume the iteration allowance twice.
    """
    unique: list[dict[str, Any]] = []
    seen_events: set[tuple[tuple[str, str, str], str]] = set()
    seen_commands: set[tuple[tuple[str, str, str], str]] = set()
    for event in _event_values(events):
        counter = _event_key(event)
        event_key = (counter, event["event_id"])
        command_key = (counter, event["command_id"])
        if event_key in seen_events or command_key in seen_commands:
            continue
        seen_events.add(event_key)
        seen_commands.add(command_key)
        unique.append(event)
    return unique


def _count_events(
    events: Any,
    *,
    kind: str,
    identity: Any = None,
) -> int:
    values = _unique_events(events)
    expected = counter_key(identity) if identity is not None else None
    return sum(
        1
        for event in values
        if event["kind"] == kind and (expected is None or _event_key(event) == expected)
    )


def count_additional_iterations(events: Any, identity: Any = None) -> int:
    """Count improvement and integration-return attempts.

    The initial attempt is deliberately excluded.  A return from integration
    is counted once for each supplied state entry; this function never invents
    a second entry or silently charges another counter.  Pass an identity to
    obtain the count for exactly one counter identity.
    """
    values = _unique_events(events)
    expected = counter_key(identity) if identity is not None else None
    return sum(
        1
        for event in values
        if event["kind"] in _COUNTED_ITERATION_KINDS
        and (expected is None or _event_key(event) == expected)
    )


def count_technical_retries(events: Any, identity: Any = None) -> int:
    """Count technical-retry events, preserving the event's counter identity."""
    return _count_events(events, kind="technical-retry", identity=identity)


def iteration_allowed(
    phase: Any,
    events: Any = (),
    identity: Any = None,
    *,
    kind: str = "improvement",
) -> bool:
    """Return whether another event of ``kind`` may be issued.

    Product improvement and technical retry limits are independent. Exhausting
    a transport retry must not silently consume or disable a content repair.
    """
    policy = phase_limits(phase)
    if kind in _COUNTED_ITERATION_KINDS:
        return count_additional_iterations(events, identity) < policy["additional_iteration_limit"]
    if kind == "technical-retry":
        return count_technical_retries(events, identity) < policy["technical_retry_limit"]
    raise LoopPolicyError("kind must be improvement, integration-return, or technical-retry")


def count_counters(events: Any) -> dict[tuple[str, str, str], dict[str, Any]]:
    """Return independent counter totals for every supplied state entry.

    Tuple keys are stable and intentionally omit scope/candidate/session
    names.  This is useful when a caller supplies separate task and
    integration-return entries: each identity receives its own increment.
    """
    result: dict[tuple[str, str, str], dict[str, Any]] = {}
    for event in _unique_events(events):
        key = _event_key(event)
        bucket = result.setdefault(
            key,
            {
                "counter_identity": {
                    field: key[index] for index, field in enumerate(_COUNTER_FIELDS)
                },
                "additional_iterations": 0,
                "technical_retries": 0,
            },
        )
        if event["kind"] in _COUNTED_ITERATION_KINDS:
            bucket["additional_iterations"] += 1
        elif event["kind"] == "technical-retry":
            bucket["technical_retries"] += 1
    return copy.deepcopy(result)


def _canonical(value: Any) -> Any:
    """Build a deterministic, non-mutating comparison value."""
    if isinstance(value, Mapping):
        return tuple(sorted((str(key), _canonical(item)) for key, item in value.items()))
    if isinstance(value, (list, tuple)):
        return tuple(_canonical(item) for item in value)
    if isinstance(value, (set, frozenset)):
        return tuple(sorted(_canonical(item) for item in value))
    if isinstance(value, (str, int, float, bool, type(None))):
        return value
    return (type(value).__name__, repr(value))


def _explicit_progress(value: Any) -> bool | None:
    if not isinstance(value, Mapping):
        return None
    for field in ("progressed", "made_progress", "advanced"):
        if isinstance(value.get(field), bool):
            return value[field]
    if isinstance(value.get("no_progress"), bool):
        return not value["no_progress"]
    if isinstance(value.get("stalled"), bool):
        return not value["stalled"]
    marker = value.get("progress")
    if isinstance(marker, bool):
        return marker
    if isinstance(marker, str):
        if marker.lower() in {"progress", "progressed", "advanced", "changed"}:
            return True
        if marker.lower() in {"none", "stalled", "unchanged", "no-progress", "no_progress"}:
            return False
    return None


def _transition_progress(previous: Any, current: Any) -> bool:
    explicit = _explicit_progress(current)
    if explicit is not None:
        return explicit
    return _canonical(previous) != _canonical(current)


def _history_values(value: Any) -> list[Any]:
    if isinstance(value, Mapping):
        for field in _KNOWN_HISTORY_CONTAINERS:
            if field in value:
                return _history_values(value[field])
        return [value]
    if isinstance(value, (str, bytes)):
        return [value]
    if isinstance(value, Sequence):
        return list(value)
    try:
        return list(value)
    except TypeError:
        return [value]


def detect_progress(previous: Any, current: Any = None) -> bool:
    """Return whether the supplied observation advanced the loop.

    With two observations, the comparison is direct.  With one history value,
    every adjacent transition is examined and the function returns whether at
    least one transition progressed.  Explicit ``progressed``/``stalled``
    markers are honoured; otherwise deterministic value comparison is used.
    """
    if current is not None:
        return _transition_progress(previous, current)
    history = _history_values(previous)
    if len(history) < 2:
        explicit = _explicit_progress(history[0]) if history else None
        return bool(explicit) if explicit is not None else False
    return any(
        _transition_progress(history[index], history[index + 1])
        for index in range(len(history) - 1)
    )


def progress_report(
    previous: Any,
    current: Any = None,
    *,
    stall_window: int = 1,
) -> dict[str, Any]:
    """Return progress and trailing-stall facts without storing state."""
    if not isinstance(stall_window, int) or isinstance(stall_window, bool) or stall_window < 1:
        raise LoopPolicyError("stall_window must be a positive integer")
    if current is not None:
        progressed = _transition_progress(previous, current)
        unchanged = 0 if progressed else 1
        return {
            "progressed": progressed,
            "stalled": unchanged >= stall_window,
            "unchanged_transitions": unchanged,
            "stall_window": stall_window,
        }
    history = _history_values(previous)
    if len(history) < 2:
        explicit = _explicit_progress(history[0]) if history else None
        return {
            "progressed": explicit,
            "stalled": explicit is False,
            "unchanged_transitions": 1 if explicit is False else 0,
            "stall_window": stall_window,
        }
    transitions = [
        _transition_progress(history[index], history[index + 1])
        for index in range(len(history) - 1)
    ]
    unchanged = 0
    for progressed in reversed(transitions):
        if progressed:
            break
        unchanged += 1
    return {
        "progressed": any(transitions),
        "stalled": unchanged >= stall_window,
        "unchanged_transitions": unchanged,
        "stall_window": stall_window,
    }


def is_stalled(
    previous: Any,
    current: Any = None,
    *,
    stall_window: int = 1,
) -> bool:
    """Return whether the latest observation has exhausted its stall window."""
    return bool(progress_report(previous, current, stall_window=stall_window)["stalled"])


def _assessment_values(value: Any, field: str) -> list[Mapping[str, Any]]:
    if value is None:
        return []
    if isinstance(value, Mapping):
        if field in value and isinstance(value[field], (Mapping, list, tuple)):
            return _assessment_values(value[field], field)
        if "assessments" in value:
            return _assessment_values(value["assessments"], field)
        if "status" in value or "completed" in value or "outcome" in value:
            return [value]
        if value and all(isinstance(item, Mapping) for item in value.values()):
            return list(value.values())
        return []
    if isinstance(value, (str, bytes)):
        return []
    try:
        return [item for item in value if isinstance(item, Mapping)]
    except TypeError:
        return []


def _status(value: Mapping[str, Any]) -> str | None:
    raw = value.get("status", value.get("outcome"))
    return raw.lower() if isinstance(raw, str) else None


def _input_needed(values: Iterable[Mapping[str, Any]]) -> bool:
    for item in values:
        if item.get("needs_input") is True or item.get("input_required") is True:
            return True
        if _status(item) in _UNKNOWN_STATUSES:
            return True
        if item.get("status") == "blocked" and item.get("blocking") is True:
            return True
    return False


def _assessment_complete(values: list[Mapping[str, Any]], *, kind: str) -> bool:
    if not values:
        return False
    if kind == "review":
        by_axis = {
            item.get("axis"): item
            for item in values
            if isinstance(item.get("axis"), str)
        }
        if not _REQUIRED_REVIEW_AXES.issubset(by_axis):
            return False
        return all(
            item.get("completed") is True
            and not item.get("unevaluated")
            and not item.get("finding_refs")
            for item in (by_axis[axis] for axis in _REQUIRED_REVIEW_AXES)
        )
    if kind == "evidence":
        return all(_status(item) in _PASS_STATUSES for item in values)
    return all(_status(item) in _PASS_STATUSES for item in values)


def _status_value(value: Any) -> str | None:
    if isinstance(value, Mapping):
        raw = value.get("status", value.get("outcome"))
    else:
        raw = value
    return raw.lower() if isinstance(raw, str) else None


def _execution_flags(value: Any) -> tuple[bool, bool, bool]:
    """Return ``(failed, recovery_required, success)`` for a status value."""
    status = _status_value(value)
    if isinstance(value, Mapping):
        failed = value.get("execution_failed") is True or value.get("failed") is True
        recovery = value.get("recovery_required") is True or value.get("recover") is True
        success = value.get("success") is True or value.get("succeeded") is True
    else:
        failed = recovery = success = False
    failed = failed or (status in _FAIL_STATUSES)
    recovery = recovery or (status in _RECOVERY_STATUSES)
    success = success or (status in _PASS_STATUSES)
    return failed, recovery, success


def _state_from_inputs(
    phase: Any,
    state: Mapping[str, Any] | None,
    events: Any,
    requirements: Any,
    reviews: Any,
    evidence: Any,
) -> tuple[Any, Mapping[str, Any], Any, Any, Any, Any]:
    """Allow either explicit keyword inputs or one state mapping."""
    if (
        state is None
        and isinstance(phase, Mapping)
        and "phase" not in phase
        and any(field in phase for field in ("events", "requirements", "reviews", "evidence", "identity"))
    ):
        state = phase
        phase = phase.get("phase") or (
            phase.get("identity", {}).get("phase")
            if isinstance(phase.get("identity"), Mapping) else None
        )
    state = state or {}
    if phase is None:
        phase = state.get("phase")
    if phase is None and isinstance(state.get("identity"), Mapping):
        phase = state["identity"].get("phase")
    if events in (None, ()) and "events" in state:
        events = state["events"]
    if requirements in (None, ()) and "requirements" in state:
        requirements = state["requirements"]
    if reviews in (None, ()) and "reviews" in state:
        reviews = state["reviews"]
    if evidence in (None, ()) and "evidence" in state:
        evidence = state["evidence"]
    return phase, state, events, requirements, reviews, evidence


def evaluate_loop(
    phase: Any = None,
    *,
    state: Mapping[str, Any] | None = None,
    events: Any = (),
    requirements: Any = (),
    reviews: Any = (),
    evidence: Any = (),
    execution_status: Any = None,
    previous: Any = None,
    current: Any = None,
    progress: Any = None,
    input_required: bool = False,
    recovery_required: bool = False,
    completed: bool | None = None,
    stalled: bool | None = None,
    stall_window: int = 1,
) -> dict[str, Any]:
    """Evaluate one supplied loop snapshot and return an outcome record.

    The outcome precedence is recovery, execution failure, completion, human
    input, exhausted technical retry limit, exhausted improvement limit,
    stall, then continuation.  A caller may use :func:`decide_loop_outcome`
    when only the outcome string is needed.
    """
    phase, state, events, requirements, reviews, evidence = _state_from_inputs(
        phase, state, events, requirements, reviews, evidence,
    )
    if phase is None:
        raise LoopPolicyError("phase is required")
    try:
        policy = _policy_for(phase)
    except LoopContractError as error:
        raise LoopPolicyError(str(error)) from error
    event_values = _event_values(events)
    additional = count_additional_iterations(event_values)
    retries = count_technical_retries(event_values)

    req_values = _assessment_values(requirements, "requirements")
    review_values = _assessment_values(reviews, "reviews")
    evidence_values = _assessment_values(evidence, "evidence")
    execution_values = [execution_status, state.get("execution")]
    failed = False
    recovery = recovery_required or state.get("recovery_required") is True
    for value in execution_values:
        item_failed, item_recovery, _ = _execution_flags(value)
        failed = failed or item_failed
        recovery = recovery or item_recovery
    recovery = recovery or any(event["status"] == "execution-unknown" for event in event_values)

    needs_input = input_required or state.get("needs_input") is True or state.get("input_required") is True
    needs_input = needs_input or _input_needed(req_values + review_values + evidence_values)
    needs_input = needs_input or bool(state.get("mandatory_unknowns")) or bool(state.get("contradictions"))

    explicit_complete = completed
    if explicit_complete is None and isinstance(state.get("completed"), bool):
        explicit_complete = state["completed"]
    if explicit_complete is None and isinstance(state.get("complete"), bool):
        explicit_complete = state["complete"]
    derived_complete = (
        _assessment_complete(req_values, kind="requirement")
        and _assessment_complete(review_values, kind="review")
        and _assessment_complete(evidence_values, kind="evidence")
    )
    if explicit_complete is None:
        explicit_complete = derived_complete
    # A caller may provide a precomputed completion flag for a state with no
    # assessment projection.  Once assessments are present, however, the
    # predicate is recomputed so an optimistic flag cannot hide an unmet gate.
    complete = derived_complete if explicit_complete and (req_values or review_values or evidence_values) else bool(explicit_complete)

    if progress is None and "progress" in state:
        progress = state["progress"]
    if isinstance(progress, Mapping):
        progressed = _explicit_progress(progress)
    elif isinstance(progress, bool):
        progressed = progress
    elif previous is not None or current is not None:
        progressed = detect_progress(previous, current)
    elif "history" in state or "states" in state:
        progressed = detect_progress(state.get("history", state.get("states")))
    else:
        progressed = None

    if stalled is None and isinstance(state.get("stalled"), bool):
        stalled = state["stalled"]
    if stalled is None:
        if previous is not None or current is not None:
            stalled = is_stalled(previous, current, stall_window=stall_window)
        elif "history" in state or "states" in state:
            stalled = is_stalled(state.get("history", state.get("states")), stall_window=stall_window)
        elif progressed is False and progress is not None:
            stalled = True
        else:
            stalled = False

    if recovery:
        outcome = "recovery-required"
        reason = "execution state is unknown or requires recovery"
    elif failed:
        outcome = "execution-failed"
        reason = "execution reported failure"
    elif complete:
        outcome = "completed"
        reason = "requirements and required reviews are complete"
    elif needs_input:
        outcome = "needs-input"
        reason = "an unresolved human input or unknown assessment remains"
    elif retries >= policy["technical_retry_limit"]:
        outcome = "execution-failed"
        reason = "technical retry limit exhausted"
    elif additional >= policy["additional_iteration_limit"]:
        outcome = "iteration-limit"
        reason = "additional iteration limit exhausted"
    elif stalled:
        outcome = "stalled"
        reason = "the supplied observation window made no progress"
    else:
        outcome = "continue"
        reason = "loop remains within configured limits and has no terminal condition"

    return {
        "schema": "loop-decision/v1",
        "phase": policy["phase"],
        "policy": copy.deepcopy(policy),
        "outcome": outcome,
        "reason": reason,
        "additional_iterations": additional,
        "technical_retries": retries,
        "additional_iterations_remaining": max(0, policy["additional_iteration_limit"] - additional),
        "technical_retries_remaining": max(0, policy["technical_retry_limit"] - retries),
        "progressed": progressed,
        "stalled": bool(stalled),
    }


def decide_next(
    state: Mapping[str, Any],
    assessment: Mapping[str, Any] | None = None,
    profile: Any = None,
) -> Decision:
    """Evaluate the orchestration seam proposed for the next workflow wave.

    ``state`` supplies immutable event/history projections.  ``assessment``
    supplies the latest machine observation and may override the corresponding
    projection for this call.  ``profile`` is a phase selector or a policy
    record.  The three inputs are copied and joined in memory only; this
    helper never becomes a state writer.
    """
    if not isinstance(state, Mapping):
        raise LoopPolicyError("state must be a mapping")
    if assessment is not None and not isinstance(assessment, Mapping):
        raise LoopPolicyError("assessment must be a mapping")
    assessment = assessment or {}
    merged = copy.deepcopy(dict(state))
    merged.update(copy.deepcopy(dict(assessment)))

    selected_phase = profile
    if selected_phase is None:
        selected_phase = merged.get("phase", merged.get("profile"))
    if selected_phase is None:
        selected_phase = merged.get("identity")
    if isinstance(selected_phase, Mapping) and "phase" not in selected_phase:
        selected_phase = selected_phase.get("phase") or selected_phase.get("identity")
    if selected_phase is None:
        raise LoopPolicyError("state/profile must supply a phase")

    explicit_completed = None
    for field in ("completed", "complete", "completion"):
        if isinstance(merged.get(field), bool):
            explicit_completed = merged[field]
            break
    completion = merged.get("completion")
    if explicit_completed is None and isinstance(completion, Mapping):
        completion_checks = (
            "all_requirements_pass", "requirements_pass", "reviews_complete", "evidence_valid",
        )
        if all(isinstance(completion.get(field), bool) for field in completion_checks):
            explicit_completed = all(completion[field] for field in completion_checks)
        if completion.get("open_required") or completion.get("open_required_findings"):
            explicit_completed = False
        if completion.get("mandatory_unknowns") or completion.get("contradictions"):
            explicit_completed = False
    if explicit_completed is None:
        predicate_fields = (
            "all_requirements_pass", "requirements_pass", "reviews_complete", "evidence_valid",
        )
        if all(isinstance(merged.get(field), bool) for field in predicate_fields):
            explicit_completed = all(merged[field] for field in predicate_fields)
        if merged.get("open_required") or merged.get("open_required_findings"):
            explicit_completed = False
        if merged.get("mandatory_unknowns") or merged.get("contradictions"):
            explicit_completed = False

    requested_outcome = merged.get("outcome")
    if isinstance(requested_outcome, str):
        requested_outcome = requested_outcome.lower().replace("_", "-")

    kwargs: dict[str, Any] = {
        "state": merged,
        "events": merged.get("events", merged.get("iteration_events", ())),
        "requirements": merged.get("requirements", ()),
        "reviews": merged.get("reviews", ()),
        "evidence": merged.get("evidence", ()),
        "execution_status": merged.get("execution_status", merged.get("execution")),
        "progress": merged.get("progress"),
        "input_required": merged.get("needs_input") is True or merged.get("input_required") is True,
        "recovery_required": merged.get("recovery_required") is True,
        "completed": explicit_completed,
        "stalled": merged.get("stalled") if isinstance(merged.get("stalled"), bool) else (
            True if requested_outcome == "stalled" else None
        ),
        "stall_window": merged.get("stall_window", 1),
    }
    if requested_outcome == "needs-input":
        kwargs["input_required"] = True
    elif requested_outcome == "execution-failed":
        kwargs["execution_status"] = "failed"
    elif requested_outcome == "recovery-required":
        kwargs["recovery_required"] = True
    if "previous" in merged or "current" in merged:
        kwargs["previous"] = merged.get("previous")
        kwargs["current"] = merged.get("current")
    result = evaluate_loop(selected_phase, **kwargs)
    if merged.get("next_hypothesis") and result["outcome"] == "stalled":
        # A concrete, testable next hypothesis is precisely what separates a
        # normal continuation from a genuine stall.  The counter remains
        # unchanged; only this observation's classification is adjusted.
        result["outcome"] = "continue"
        result["reason"] = "a new testable next hypothesis is supplied"
        result["stalled"] = False
    return result


def decide_loop_outcome(*args: Any, **kwargs: Any) -> str:
    """Return only one of the six terminal outcomes or ``continue``."""
    return evaluate_loop(*args, **kwargs)["outcome"]


__all__ = [
    "OUTCOMES",
    "Decision",
    "LoopPolicyError",
    "additional_iteration_limit",
    "count_additional_iterations",
    "count_counters",
    "count_technical_retries",
    "counter_key",
    "decide_loop_outcome",
    "decide_next",
    "detect_progress",
    "evaluate_loop",
    "is_stalled",
    "iteration_allowed",
    "phase_limits",
    "progress_report",
    "technical_retry_limit",
]
