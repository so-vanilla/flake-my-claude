"""Pure reduction and boundary adapters for the workflow improvement loop.

This module is deliberately smaller than a Run/Control Kernel.  A loop
history is supplied by its caller, validated, and reduced to an ephemeral
view.  The module never opens a file, reads a clock, launches work, or keeps a
second durable copy of a Run.  :class:`LoopStateController` only delegates a
candidate history to an injected ``snapshot``/``compare_and_swap`` adapter.

The wire shapes used here are the ones in :mod:`loop_contracts`.  In
particular, an iteration event is a *current immutable value* for one attempt;
status transitions return a new history and never mutate the caller's list or
event.  A reservation is therefore visible before a dispatcher is called,
and an interrupted dispatch can be represented as ``execution-unknown``.
"""
from __future__ import annotations

import copy
from collections.abc import Callable, Mapping, Sequence
from typing import Any, NoReturn, Protocol

from .loop_contracts import (
    LOOP_CONTRACT_VERSION,
    LoopContractError,
    canonical_digest,
    counter_identity,
    phase_policy,
    validate_iteration_event,
    validate_ref,
    validate_work_identity,
)

_STATE_SCHEMA = "loop-control-state/v1"
_RESERVATION_STATUS = "reserved"
_TERMINAL_RECOVERY_OUTCOME = "recovery-required"
_NON_TECHNICAL_KINDS = {"improvement", "integration-return"}
_KINDS = {"initial", "improvement", "integration-return", "technical-retry"}


class LoopStateError(LoopContractError):
    """A loop history or transition is invalid."""


class HistoryValidationError(LoopStateError):
    """An event-history invariant is broken."""


class CommandConflictError(LoopStateError):
    """A command id was reused for a different payload."""


class EventConflictError(LoopStateError):
    """An event id was reused for a different immutable value."""


class RevisionConflictError(LoopStateError):
    """The injected Control Kernel adapter rejected an expected revision."""


class ResultRejectedError(LoopStateError):
    """A result cannot be accepted for the supplied history."""


class RecoveryRequiredError(LoopStateError):
    """Execution outcome is ambiguous and needs an explicit recovery proof."""


class ExecutionUnknownError(RecoveryRequiredError):
    """Dispatch failed after reservation and its external outcome is unknown."""


class CounterExhaustedError(LoopStateError):
    """A phase's finite iteration or technical retry limit is exhausted."""


class LoopStateAdapter(Protocol):
    """The only mutable boundary the controller is allowed to call.

    ``snapshot`` must return ``{"revision": int, "history": list}``.
    ``compare_and_swap`` must atomically publish the supplied history iff its
    ``expected_revision`` is current.  Implementations normally delegate this
    operation to the existing Control Kernel; this module does not provide a
    file-backed implementation.
    """

    def snapshot(self) -> Mapping[str, Any]: ...

    def compare_and_swap(
        self, *, expected_revision: int, history: Sequence[Mapping[str, Any]]
    ) -> Mapping[str, Any] | None: ...


def _error(message: str, error_type: type[LoopStateError] = LoopStateError) -> NoReturn:
    raise error_type(message)


def _copy(value: Any) -> Any:
    return copy.deepcopy(value)


def _event_digest(event: Mapping[str, Any]) -> str:
    """Return the digest used by predecessor/result references."""

    return canonical_digest(validate_iteration_event(event))


def event_digest(event: Mapping[str, Any]) -> str:
    """Public non-mutating digest helper for an iteration event."""

    return _event_digest(event)


def event_ref(event: Mapping[str, Any]) -> dict[str, str]:
    """Return the exact reference that can identify ``event`` as a predecessor."""

    validated = validate_iteration_event(event)
    return {"id": validated["event_id"], "digest": canonical_digest(validated)}


def _command_payload(event: Mapping[str, Any]) -> dict[str, Any]:
    """Return the command payload while excluding the generated event id.

    Retransmission may choose a different event id, but it must not be able to
    reuse a command id for a different identity, status, attempt, or result.
    """

    payload = _copy(dict(event))
    payload.pop("event_id", None)
    return payload


def command_payload_digest(event: Mapping[str, Any]) -> str:
    """Digest the idempotency payload bound to an iteration command."""

    return canonical_digest(_command_payload(validate_iteration_event(event)))


def _as_history(value: Any) -> list[Mapping[str, Any]]:
    if not isinstance(value, (list, tuple)):
        _error("history must be a list of iteration events", HistoryValidationError)
    return list(value)


def _same_counter_key(left: Mapping[str, str], right: Mapping[str, str]) -> bool:
    return dict(left) == dict(right)


def _ref_matches(value: Mapping[str, Any], predecessor: Mapping[str, Any]) -> bool:
    """Match an id/path-plus-digest ref against one validated event."""

    digest = _event_digest(predecessor)
    if value.get("digest") != digest:
        return False
    if "id" in value and value.get("id") != predecessor.get("event_id"):
        return False
    # A path-only reference is permitted by the shared contract.  There is no
    # filesystem lookup here; its digest is the immutable identity.
    return "id" in value or "path" in value


def _validate_history(
    history: Any,
    *,
    identity: Mapping[str, Any] | None = None,
    deduplicate: bool = True,
) -> list[dict[str, Any]]:
    """Validate and copy a linear history without changing the input.

    Exact retransmissions of an event/command are collapsed.  A command id
    remains bound to its complete payload (except ``event_id``), so a retry
    cannot silently alter the identity, attempt, predecessor, or result.
    """

    values = _as_history(history)
    expected_identity = validate_work_identity(identity) if identity is not None else None
    result: list[dict[str, Any]] = []
    by_event: dict[str, tuple[str, dict[str, Any]]] = {}
    by_command: dict[str, tuple[str, dict[str, Any]]] = {}
    expected_counter: dict[str, str] | None = None

    for position, raw in enumerate(values):
        try:
            item = validate_iteration_event(raw)
        except LoopContractError as error:
            raise HistoryValidationError(f"history event {position} is invalid: {error}") from error

        digest = canonical_digest(item)
        command_digest = canonical_digest(_command_payload(item))
        event_id = item["event_id"]
        command_id = item["command_id"]

        previous_event = by_event.get(event_id)
        if previous_event is not None:
            if previous_event[0] != digest:
                _error("duplicate event id has a different payload", EventConflictError)
            if deduplicate:
                continue

        previous_command = by_command.get(command_id)
        if previous_command is not None:
            if previous_command[0] != command_digest:
                _error("duplicate command id has a different payload", CommandConflictError)
            if deduplicate:
                continue

        item_counter = counter_identity(item["identity"])
        if expected_counter is None:
            expected_counter = item_counter
            if expected_identity is not None and not _same_counter_key(expected_counter, counter_identity(expected_identity)):
                _error("history identity does not match the supplied identity", HistoryValidationError)
        elif not _same_counter_key(item_counter, expected_counter):
            _error("counter key drift is not permitted", HistoryValidationError)

        if not result:
            if item["kind"] != "initial" or item["attempt"] != 0 or item["predecessor_ref"] is not None:
                _error("history must begin with an initial reservation", HistoryValidationError)
        else:
            previous = result[-1]
            if item["kind"] == "initial" or item["attempt"] == 0:
                _error("only the first event may be initial/attempt zero", HistoryValidationError)
            predecessor = item["predecessor_ref"]
            if not isinstance(predecessor, Mapping) or not _ref_matches(predecessor, previous):
                _error("event predecessor reference is broken", HistoryValidationError)
            if item["kind"] == "technical-retry":
                # The shared contract reserves attempt zero for ``initial``;
                # a retry therefore receives the next immutable attempt
                # number even though it is charged to the technical-retry
                # counter rather than the product-improvement counter.
                if item["attempt"] != previous["attempt"] + 1:
                    _error("technical retry attempts must be contiguous", HistoryValidationError)
            elif item["attempt"] != previous["attempt"] + 1:
                _error("iteration attempts must be contiguous", HistoryValidationError)

        if expected_identity is not None and not _same_counter_key(item_counter, counter_identity(expected_identity)):
            # Scope/requirements may legitimately advance between iterations;
            # only the contract's stable counter identity is invariant.
            _error("counter key drift is not permitted", HistoryValidationError)

        result.append(item)
        by_event[event_id] = (digest, item)
        by_command[command_id] = (command_digest, item)

    return _copy(result)


def validate_history(
    history: Any, *, identity: Mapping[str, Any] | None = None
) -> list[dict[str, Any]]:
    """Return a validated, detached history or raise a fail-closed error."""

    return _validate_history(history, identity=identity)


def derive_counters(history: Any) -> dict[str, Any]:
    """Derive finite loop counters from validated history.

    ``scope_revision``, candidate/session labels outside the shared identity,
    and resume labels do not occur in ``counter_key`` and therefore cannot
    reset these counters.  The returned mapping is newly allocated on every
    call.
    """

    events = _validate_history(history)
    if not events:
        return {
            "counter_key": None,
            "policy": None,
            "total": 0,
            "initial": 0,
            "additional_iterations": 0,
            "technical_retries": 0,
            "by_kind": {kind: 0 for kind in sorted(_KINDS)},
            "highest_attempt": -1,
            "next_attempt": 0,
        }

    identity = events[0]["identity"]
    policy = phase_policy(identity["phase"])
    by_kind = {kind: 0 for kind in sorted(_KINDS)}
    for item in events:
        by_kind[item["kind"]] += 1
    additional = sum(by_kind[kind] for kind in _NON_TECHNICAL_KINDS)
    highest = max(item["attempt"] for item in events)
    return {
        "counter_key": counter_identity(identity),
        "policy": _copy(policy),
        "total": len(events),
        "initial": by_kind["initial"],
        "additional_iterations": additional,
        "technical_retries": by_kind["technical-retry"],
        "by_kind": by_kind,
        "highest_attempt": highest,
        "next_attempt": highest + 1,
    }


def reduce_history(
    history: Any, *, identity: Mapping[str, Any] | None = None
) -> dict[str, Any]:
    """Reduce an event history to an ephemeral state projection.

    The projection is not a Run and is never written by this function.  An
    ``execution-unknown`` latest event deliberately maps to the terminal
    ``recovery-required`` outcome and disables dispatch.
    """

    events = _validate_history(history, identity=identity)
    counters = derive_counters(events)
    if not events and identity is not None:
        checked_identity = validate_work_identity(identity)
        counters["counter_key"] = counter_identity(checked_identity)
        counters["policy"] = phase_policy(checked_identity["phase"])
    latest = _copy(events[-1]) if events else None
    unknown = latest is not None and latest["status"] == "execution-unknown"
    state = {
        "schema": _STATE_SCHEMA,
        "state_schema": "loop-state/v1",
        "contract_version": LOOP_CONTRACT_VERSION,
        "identity": _copy(events[0]["identity"]) if events else (_copy(identity) if identity is not None else None),
        "counter_key": _copy(counters["counter_key"]),
        "counter_identity": _copy(counters["counter_key"]),
        "policy": _copy(counters["policy"]),
        "counters": _copy(counters),
        "initial_attempt_recorded": bool(counters["initial"]),
        "additional_iterations_used": counters["additional_iterations"],
        "technical_retries_used": counters["technical_retries"],
        "history": _copy(events),
        "events": _copy(events),
        "event_refs": [event_ref(item) for item in events],
        "latest_event": latest,
        "status": latest["status"] if latest is not None else "idle",
        "outcome": _TERMINAL_RECOVERY_OUTCOME if unknown else None,
        "terminal_outcome": _TERMINAL_RECOVERY_OUTCOME if unknown else None,
        "recovery_required": unknown,
        "dispatch_allowed": not unknown,
    }
    return state


def build_iteration_event(
    identity: Mapping[str, Any],
    command_id: str,
    *,
    kind: str = "initial",
    attempt: int = 0,
    predecessor_ref: Mapping[str, Any] | None = None,
    event_id: str | None = None,
    status: str = _RESERVATION_STATUS,
    result_ref: Mapping[str, Any] | None = None,
) -> dict[str, Any]:
    """Build one deterministic event value; no id or timestamp is generated."""

    if event_id is None:
        # Deterministic fallback is intentional: this helper never reads a
        # clock or random source.  Callers that need a distinct retry id must
        # supply it explicitly.
        event_id = "event-" + str(command_id)
    event = {
        "schema": "loop-iteration-event/v1",
        "event_id": event_id,
        "command_id": command_id,
        "identity": _copy(validate_work_identity(identity)),
        "kind": kind,
        "status": status,
        "attempt": attempt,
        "predecessor_ref": _copy(predecessor_ref),
        "result_ref": _copy(result_ref),
    }
    try:
        return validate_iteration_event(event)
    except LoopContractError as error:
        raise LoopStateError(f"cannot build iteration event: {error}") from error


def _coerce_event(
    event: Mapping[str, Any] | None,
    *,
    identity: Mapping[str, Any] | None,
    command_id: str | None,
    event_id: str | None,
    kind: str,
    attempt: int | None,
    predecessor_ref: Mapping[str, Any] | None,
) -> dict[str, Any]:
    if event is not None:
        if not isinstance(event, Mapping):
            _error("event must be a mapping", HistoryValidationError)
        value = _copy(dict(event))
        if value.get("status") != _RESERVATION_STATUS:
            _error("a reservation event must have reserved status", HistoryValidationError)
        try:
            return validate_iteration_event(value)
        except LoopContractError as error:
            raise HistoryValidationError(f"reservation event is invalid: {error}") from error
    if identity is None or command_id is None:
        _error("identity and command_id are required to reserve", HistoryValidationError)
    if attempt is None:
        attempt = 0
    return build_iteration_event(
        identity,
        command_id,
        kind=kind,
        attempt=attempt,
        predecessor_ref=predecessor_ref,
        event_id=event_id,
        status=_RESERVATION_STATUS,
        result_ref=None,
    )


def _find_event(
    events: Sequence[Mapping[str, Any]],
    *,
    event_id: str | None = None,
    command_id: str | None = None,
) -> tuple[int, dict[str, Any]] | None:
    matches: list[tuple[int, dict[str, Any]]] = []
    for index, item in enumerate(events):
        if (
            (event_id is not None and item.get("event_id") == event_id)
            or (event_id is None and command_id is not None and item.get("command_id") == command_id)
        ):
            matches.append((index, _copy(dict(item))))
    if not matches:
        return None
    if len(matches) > 1:
        _error("event selector is not unique", HistoryValidationError)
    return matches[0]


def _replace_latest(
    events: Sequence[Mapping[str, Any]],
    replacement: Mapping[str, Any],
    *,
    expected_event_id: str,
) -> dict[str, Any]:
    values = _validate_history(events)
    if not values:
        _error("event does not have a reservation", ResultRejectedError)
    if values[-1]["event_id"] != expected_event_id:
        _error("late result or status update is not accepted", ResultRejectedError)
    candidate = _copy(values)
    candidate[-1] = _copy(dict(replacement))
    return reduce_history(candidate)


def _counter_limit_check(events: Sequence[Mapping[str, Any]], event: Mapping[str, Any]) -> None:
    counters = derive_counters(events)
    policy = phase_policy(event["identity"]["phase"])
    if event["kind"] in _NON_TECHNICAL_KINDS and counters["additional_iterations"] >= policy["additional_iteration_limit"]:
        _error("additional iteration limit is exhausted", CounterExhaustedError)
    if event["kind"] == "technical-retry" and counters["technical_retries"] >= policy["technical_retry_limit"]:
        _error("technical retry limit is exhausted", CounterExhaustedError)


def reserve_iteration(
    history: Any,
    event: Mapping[str, Any] | None = None,
    *,
    identity: Mapping[str, Any] | None = None,
    command_id: str | None = None,
    event_id: str | None = None,
    kind: str = "initial",
    attempt: int | None = None,
    predecessor_ref: Mapping[str, Any] | None = None,
) -> dict[str, Any]:
    """Purely reserve one event before any dispatcher is allowed to run."""

    existing = _validate_history(history, identity=identity)
    candidate_event = _coerce_event(
        event,
        identity=identity,
        command_id=command_id,
        event_id=event_id,
        kind=kind,
        attempt=attempt,
        predecessor_ref=predecessor_ref,
    )

    # Retries of an identical command are idempotent and must not consume a
    # second attempt or invoke a dispatcher again.
    candidate_command_digest = command_payload_digest(candidate_event)
    candidate_event_digest = _event_digest(candidate_event)
    for item in existing:
        if item["command_id"] == candidate_event["command_id"]:
            if command_payload_digest(item) != candidate_command_digest:
                _error("duplicate command id has a different payload", CommandConflictError)
            state = reduce_history(existing)
            state.update({"idempotent": True, "reserved": False, "event": _copy(item)})
            return state
        if item["event_id"] == candidate_event["event_id"]:
            if _event_digest(item) != candidate_event_digest:
                _error("duplicate event id has a different payload", EventConflictError)
            state = reduce_history(existing)
            state.update({"idempotent": True, "reserved": False, "event": _copy(item)})
            return state

    if existing and existing[-1]["status"] == "execution-unknown":
        _error("execution outcome is unknown; explicit recovery is required", RecoveryRequiredError)
    if existing and existing[-1]["status"] in {"reserved", "running"}:
        _error("an active reservation must be evaluated before another dispatch", LoopStateError)
    _counter_limit_check(existing, candidate_event)
    result = reduce_history(existing + [candidate_event], identity=identity)
    result.update({"idempotent": False, "reserved": True, "event": _copy(candidate_event)})
    return result


def mark_running(history: Any, event_id: str) -> dict[str, Any]:
    """Move the latest reservation to running without mutating input."""

    events = _validate_history(history)
    found = _find_event(events, event_id=event_id)
    if found is None:
        _error("running status requires an existing reservation", ResultRejectedError)
    _, current = found
    if current["status"] == "running":
        state = reduce_history(events)
        state.update({"idempotent": True, "event": _copy(current)})
        return state
    if current["status"] == "execution-unknown":
        _error("execution outcome is unknown; explicit recovery is required", RecoveryRequiredError)
    if current["status"] == "evaluated":
        _error("an evaluated result cannot return to running", ResultRejectedError)
    replacement = _copy(current)
    replacement["status"] = "running"
    result = _replace_latest(events, replacement, expected_event_id=event_id)
    result.update({"idempotent": False, "event": _copy(replacement)})
    return result


def accept_result(
    history: Any,
    event_id: str | Mapping[str, Any] | None = None,
    result_ref: Mapping[str, Any] | None = None,
    *,
    command_id: str | None = None,
) -> dict[str, Any]:
    """Accept one result only for an existing, non-unknown reservation."""

    # Accepting a fully formed result event is convenient for adapters and
    # remains strict: only its selector and result reference are consumed.
    if isinstance(event_id, Mapping):
        value = event_id
        event_id = value.get("event_id")
        command_id = value.get("command_id", command_id)
        result_ref = value.get("result_ref", result_ref)
    events = _validate_history(history)
    found = _find_event(events, event_id=event_id if isinstance(event_id, str) else None, command_id=command_id)
    if found is None:
        _error("result before reservation is rejected", ResultRejectedError)
    _, current = found
    if result_ref is None:
        _error("accepted result requires a result reference", ResultRejectedError)
    try:
        checked_ref = validate_ref(result_ref, "result_ref")
    except LoopContractError as error:
        raise ResultRejectedError(str(error)) from error
    if current["status"] == "execution-unknown":
        _error("execution outcome is unknown; explicit recovery is required", RecoveryRequiredError)
    if current["status"] == "evaluated":
        if current.get("result_ref") == checked_ref:
            state = reduce_history(events)
            state.update({"idempotent": True, "accepted": False, "event": _copy(current)})
            return state
        _error("evaluated result cannot be replaced by another result", ResultRejectedError)
    replacement = _copy(current)
    replacement["status"] = "evaluated"
    replacement["result_ref"] = _copy(checked_ref)
    result = _replace_latest(events, replacement, expected_event_id=current["event_id"])
    result.update({"idempotent": False, "accepted": True, "event": _copy(replacement)})
    return result


def mark_execution_unknown(history: Any, event_id: str) -> dict[str, Any]:
    """Record an ambiguous post-reservation dispatch outcome."""

    events = _validate_history(history)
    found = _find_event(events, event_id=event_id)
    if found is None:
        _error("unknown execution requires an existing reservation", ResultRejectedError)
    _, current = found
    if current["status"] == "execution-unknown":
        state = reduce_history(events)
        state.update({"idempotent": True, "event": _copy(current)})
        return state
    if current["status"] == "evaluated":
        _error("a known result cannot become execution-unknown", ResultRejectedError)
    replacement = _copy(current)
    replacement["status"] = "execution-unknown"
    replacement["result_ref"] = None
    result = _replace_latest(events, replacement, expected_event_id=event_id)
    result.update({"idempotent": False, "event": _copy(replacement)})
    return result


def recover_execution(
    history: Any,
    event_id: str,
    *,
    resolution: str | None = None,
    evidence_ref: Mapping[str, Any] | None = None,
    result_ref: Mapping[str, Any] | None = None,
    retry_event: Mapping[str, Any] | None = None,
    retry_command_id: str | None = None,
    retry_event_id: str | None = None,
) -> dict[str, Any]:
    """Resolve an unknown execution only with an explicit recovery proof.

    ``resolution='accept-result'`` replaces the unknown attempt with a
    verified result.  ``resolution='retry'`` retains the unknown event and
    appends a new, explicitly identified technical retry.  A bare retry or a
    missing evidence reference is intentionally ambiguous and is rejected.
    """

    events = _validate_history(history)
    found = _find_event(events, event_id=event_id)
    if found is None:
        _error("recovery requires an existing event", RecoveryRequiredError)
    index, current = found
    if index != len(events) - 1:
        _error("only the latest unknown execution can be recovered", RecoveryRequiredError)
    if current["status"] != "execution-unknown":
        _error("recovery is only valid for execution-unknown", RecoveryRequiredError)
    if resolution not in {"accept-result", "retry"}:
        _error("ambiguous crash recovery requires an explicit resolution", RecoveryRequiredError)
    if evidence_ref is None:
        _error("recovery requires a digest-bound evidence reference", RecoveryRequiredError)
    try:
        checked_evidence = validate_ref(evidence_ref, "evidence_ref")
    except LoopContractError as error:
        raise RecoveryRequiredError(str(error)) from error

    if resolution == "accept-result":
        if result_ref is None:
            _error("accept-result recovery requires a result reference", RecoveryRequiredError)
        try:
            checked_result = validate_ref(result_ref, "result_ref")
        except LoopContractError as error:
            raise RecoveryRequiredError(str(error)) from error
        replacement = _copy(current)
        replacement["status"] = "evaluated"
        replacement["result_ref"] = _copy(checked_result)
        state = _replace_latest(events, replacement, expected_event_id=event_id)
        state.update({
            "recovered": True,
            "recovery": {"resolution": resolution, "evidence_ref": _copy(checked_evidence)},
            "event": _copy(replacement),
        })
        return state

    if retry_event is not None:
        candidate = _copy(dict(retry_event))
        if candidate.get("status") != _RESERVATION_STATUS:
            _error("a recovery retry must reserve before dispatch", RecoveryRequiredError)
        try:
            candidate = validate_iteration_event(candidate)
        except LoopContractError as error:
            raise RecoveryRequiredError(f"retry event is invalid: {error}") from error
    else:
        if not retry_command_id or not retry_event_id:
            _error("retry recovery requires explicit command and event ids", RecoveryRequiredError)
        candidate = build_iteration_event(
            current["identity"],
            retry_command_id,
            event_id=retry_event_id,
            kind="technical-retry",
            attempt=current["attempt"] + 1,
            predecessor_ref=event_ref(current),
            status=_RESERVATION_STATUS,
            result_ref=None,
        )
    if candidate["identity"] != current["identity"]:
        # Scope revisions may change during ordinary improvement, but a
        # recovery retry must bind the exact unknown assignment.
        _error("recovery retry identity does not bind the unknown event", RecoveryRequiredError)
    if candidate["kind"] != "technical-retry" or candidate["attempt"] != current["attempt"] + 1:
        _error("recovery retry must use the next attempt number", RecoveryRequiredError)
    if not isinstance(candidate.get("predecessor_ref"), Mapping) or not _ref_matches(candidate["predecessor_ref"], current):
        _error("recovery retry predecessor is broken", RecoveryRequiredError)
    _counter_limit_check(events, candidate)
    state = reduce_history(events + [candidate])
    state.update({
        "recovered": True,
        "recovery": {"resolution": resolution, "evidence_ref": _copy(checked_evidence)},
        "event": _copy(candidate),
    })
    return state


def _adapter_snapshot(adapter: Any) -> dict[str, Any]:
    method = getattr(adapter, "snapshot", None)
    if method is None:
        method = getattr(adapter, "read", None)
    if method is None:
        method = getattr(adapter, "read_state", None)
    if method is None:
        method = getattr(adapter, "load", None)
    if method is None or not callable(method):
        _error("adapter must expose snapshot()", RevisionConflictError)
    value = method()
    if not isinstance(value, Mapping):
        _error("adapter snapshot must be a mapping", RevisionConflictError)
    revision = value.get("revision")
    if not isinstance(revision, int) or isinstance(revision, bool) or revision < 0:
        _error("adapter snapshot revision is invalid", RevisionConflictError)
    history = value.get("history", value.get("events"))
    if history is None:
        _error("adapter snapshot must include history", RevisionConflictError)
    checked = _validate_history(history)
    return {"revision": revision, "history": checked}


def _adapter_compare_and_swap(
    adapter: Any,
    *,
    expected_revision: int,
    history: Sequence[Mapping[str, Any]],
) -> dict[str, Any]:
    method = getattr(adapter, "compare_and_swap", None)
    if method is None:
        method = getattr(adapter, "compare_and_swap_history", None)
    if method is None:
        method = getattr(adapter, "cas", None)
    if method is None or not callable(method):
        _error("adapter must expose compare_and_swap()", RevisionConflictError)
    try:
        raw = method(expected_revision=expected_revision, history=_copy(list(history)))
    except RevisionConflictError:
        raise
    except (KeyError, ValueError) as error:
        raise RevisionConflictError(str(error)) from error
    if raw is None:
        return {"revision": expected_revision + 1, "history": _copy(list(history))}
    if not isinstance(raw, Mapping):
        _error("adapter compare_and_swap result must be a mapping", RevisionConflictError)
    if raw.get("accepted", True) is False:
        _error("compare-and-swap rejected the expected revision", RevisionConflictError)
    revision = raw.get("revision", expected_revision + 1)
    if not isinstance(revision, int) or isinstance(revision, bool) or revision != expected_revision + 1:
        _error("adapter returned an invalid next revision", RevisionConflictError)
    returned_history = raw.get("history", history)
    checked = _validate_history(returned_history)
    if checked != _copy(list(history)):
        _error("adapter returned a history different from the candidate", RevisionConflictError)
    return {"revision": revision, "history": checked}


class LoopStateController:
    """Pure loop compiler plus an injected CAS publication boundary.

    The controller stores only the adapter reference.  Every operation reads a
    fresh snapshot, computes a candidate, and publishes it with the caller's
    expected revision.  The adapter remains the owner of durable Run state.
    """

    def __init__(self, adapter: LoopStateAdapter):
        self._adapter = adapter

    def snapshot(self) -> dict[str, Any]:
        """Read and reduce one adapter snapshot without publishing anything."""

        snap = _adapter_snapshot(self._adapter)
        state = reduce_history(snap["history"])
        state["revision"] = snap["revision"]
        return state

    @staticmethod
    def _check_expected_revision(snapshot: Mapping[str, Any], expected_revision: int | None) -> None:
        if expected_revision is None:
            return
        if (
            not isinstance(expected_revision, int)
            or isinstance(expected_revision, bool)
            or expected_revision != snapshot["revision"]
        ):
            _error("caller expected revision is stale", RevisionConflictError)

    def _publish(self, snapshot: Mapping[str, Any], state: Mapping[str, Any]) -> dict[str, Any]:
        committed = _adapter_compare_and_swap(
            self._adapter,
            expected_revision=int(snapshot["revision"]),
            history=state["history"],
        )
        result = reduce_history(committed["history"])
        result["revision"] = committed["revision"]
        return result

    def reserve(
        self,
        event: Mapping[str, Any] | None = None,
        *,
        identity: Mapping[str, Any] | None = None,
        command_id: str | None = None,
        event_id: str | None = None,
        kind: str = "initial",
        attempt: int | None = None,
        predecessor_ref: Mapping[str, Any] | None = None,
        expected_revision: int | None = None,
    ) -> dict[str, Any]:
        snap = _adapter_snapshot(self._adapter)
        self._check_expected_revision(snap, expected_revision)
        state = reserve_iteration(
            snap["history"],
            event,
            identity=identity,
            command_id=command_id,
            event_id=event_id,
            kind=kind,
            attempt=attempt,
            predecessor_ref=predecessor_ref,
        )
        if state.get("idempotent"):
            state["revision"] = snap["revision"]
            return state
        committed = self._publish(snap, state)
        committed.update({"reserved": True, "idempotent": False, "event": _copy(state["event"])})
        return committed

    def reserve_before_dispatch(
        self,
        dispatch: Callable[[Mapping[str, Any]], Any],
        event: Mapping[str, Any] | None = None,
        **kwargs: Any,
    ) -> dict[str, Any]:
        """Publish ``reserved`` with CAS, then invoke exactly one dispatcher.

        A duplicate command returns before invoking ``dispatch``.  Any
        exception from dispatch is treated as an ambiguous outcome; the
        controller attempts one CAS transition to ``execution-unknown`` and
        raises :class:`ExecutionUnknownError` (or a recovery error if that CAS
        itself loses the race).
        """

        state = self.reserve(event, **kwargs)
        if state.get("idempotent"):
            state["dispatch_invoked"] = False
            return state
        reserved_event = _copy(state["event"])
        try:
            dispatched = dispatch(_copy(reserved_event))
        except Exception as error:
            try:
                snap = {"revision": state["revision"], "history": state["history"]}
                unknown = mark_execution_unknown(state["history"], reserved_event["event_id"])
                committed = self._publish(snap, unknown)
            except Exception as recovery_error:
                raise RecoveryRequiredError(
                    "dispatch outcome is unknown and unknown-state publication failed"
                ) from recovery_error
            error_state = committed
            error_state.update({"dispatch_invoked": True, "event": error_state["latest_event"]})
            raise ExecutionUnknownError(
                "dispatch raised after reservation; recovery is required"
            ) from error
        state["dispatch_invoked"] = True
        state["dispatch_result"] = _copy(dispatched)
        return state

    def mark_running(self, event_id: str, *, expected_revision: int | None = None) -> dict[str, Any]:
        snap = _adapter_snapshot(self._adapter)
        self._check_expected_revision(snap, expected_revision)
        state = mark_running(snap["history"], event_id)
        if state.get("idempotent"):
            state["revision"] = snap["revision"]
            return state
        committed = self._publish(snap, state)
        committed["event"] = _copy(state["event"])
        return committed

    def accept_result(
        self,
        event_id: str | Mapping[str, Any] | None = None,
        result_ref: Mapping[str, Any] | None = None,
        *,
        command_id: str | None = None,
        expected_revision: int | None = None,
    ) -> dict[str, Any]:
        snap = _adapter_snapshot(self._adapter)
        self._check_expected_revision(snap, expected_revision)
        state = accept_result(snap["history"], event_id, result_ref, command_id=command_id)
        if state.get("idempotent"):
            state["revision"] = snap["revision"]
            return state
        committed = self._publish(snap, state)
        committed.update({"accepted": True, "event": _copy(state["event"])})
        return committed

    def mark_execution_unknown(self, event_id: str, *, expected_revision: int | None = None) -> dict[str, Any]:
        snap = _adapter_snapshot(self._adapter)
        self._check_expected_revision(snap, expected_revision)
        state = mark_execution_unknown(snap["history"], event_id)
        if state.get("idempotent"):
            state["revision"] = snap["revision"]
            return state
        committed = self._publish(snap, state)
        committed["event"] = _copy(state["event"])
        return committed

    def recover_execution(self, event_id: str, **kwargs: Any) -> dict[str, Any]:
        snap = _adapter_snapshot(self._adapter)
        expected_revision = kwargs.pop("expected_revision", None)
        self._check_expected_revision(snap, expected_revision)
        state = recover_execution(snap["history"], event_id, **kwargs)
        committed = self._publish(snap, state)
        committed.update({key: _copy(value) for key, value in state.items() if key in {"recovered", "recovery", "event"}})
        return committed

__all__ = [
    "CommandConflictError",
    "CounterExhaustedError",
    "EventConflictError",
    "ExecutionUnknownError",
    "HistoryValidationError",
    "LoopStateAdapter",
    "LoopStateController",
    "LoopStateError",
    "RecoveryRequiredError",
    "ResultRejectedError",
    "RevisionConflictError",
    "accept_result",
    "build_iteration_event",
    "command_payload_digest",
    "derive_counters",
    "event_digest",
    "event_ref",
    "mark_execution_unknown",
    "mark_running",
    "recover_execution",
    "reduce_history",
    "reserve_iteration",
    "validate_history",
]
