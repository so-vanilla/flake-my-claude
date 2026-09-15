"""Persisted, lineage-checked Group E execution for one planned Task.

The caller supplies review judgments and command evidence, never lifecycle
truth. This parent derives readiness from the current Kernel and derives every
candidate, closure, aggregate, review, and validation binding from actual data.
"""
from __future__ import annotations

import copy
import hashlib
import json
from collections.abc import Mapping, Sequence
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

from .completion import CompletionError
from .evidence_validity import EvidenceValidityError, assess_evidence
from .execution_group import (
    ArtifactCandidateBuilder,
    ExecutionGroupV1,
    _declared_source_path,
)
from .execution_v2 import (
    ExecutionClosureBuilder,
    FindingValidator,
    MechanicalCompletion,
    ReceiptAggregator,
    RegressionFrontier,
    V2ContractError,
    WorkflowLoopValidator,
)
from .inception_cli import InceptionError
from .inception_runtime import InceptionRuntime
from .loop_contracts import (
    LOOP_CONTRACT_VERSION,
    TERMINAL_OUTCOMES,
    LoopContractError,
    canonical_digest,
    counter_identity,
    phase_policy,
    validate_evidence_record,
    validate_ref,
    validate_requirement_assessment,
    validate_resume_record,
    validate_review_assessment,
    validate_terminal_record,
    validate_work_identity,
)
from .loop_metrics import LoopMetricsError, derive_metrics
from .loop_policy import phase_limits
from .loop_state import CounterExhaustedError, event_ref
from .macos_task_process import MacOSTaskProcessBroker

_LOOP_SCHEMA = LOOP_CONTRACT_VERSION
_LOOP_PROGRESS_FIELDS = frozenset(
    {
        "allowances",
        "budget",
        "budget_digest",
        "deadline",
        "remaining_seconds",
        "reopen_budget",
        "replacement_budget",
        "review_budget",
        "review_budget_remaining",
        "observed_budget",
        "wall_clock_deadline",
        "wall_clock_minutes",
    }
)
_LOOP_TIMEOUT_FIELDS = frozenset(
    {"process_timeout", "timeout_seconds", "supervision_timeout_seconds"}
)


class RuntimeExecutionError(InceptionError):
    """Refusal at the persisted D-to-E runtime boundary."""


def _digest(value: Any) -> str:
    return "sha256:" + hashlib.sha256(
        json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=False).encode("utf-8")
    ).hexdigest()


def _head(kernel) -> dict[str, Any]:
    current = kernel.head()
    if not isinstance(current, Mapping):
        raise RuntimeExecutionError("runtime execution requires a persisted HEAD")
    return {"revision": current["revision"], "transaction_digest": current["transaction_digest"]}


def _source_ref(path: str, kind: str, revision: int) -> dict[str, Any]:
    physical = _declared_source_path(path)
    try:
        raw = physical.read_bytes()
    except OSError as error:
        raise RuntimeExecutionError("Group E contract reference is unavailable: " + path) from error
    return {"kind": kind, "path": path, "version": "v1", "digest": "sha256:" + hashlib.sha256(raw).hexdigest(), "creator": "persisted-runtime-parent", "revision": revision, "fresh": True}


def _durable_compiled(value: Any) -> Any:
    if isinstance(value, Mapping):
        return {key: _durable_compiled(item) for key, item in value.items() if key not in {"non_authorizing", "birth_token", "dispatch_authority"}}
    if isinstance(value, list):
        return [_durable_compiled(item) for item in value]
    return copy.deepcopy(value)


def _event(sequence: int, parent: str | None, event_type: str, payload: Mapping[str, Any]) -> dict[str, Any]:
    value = {"schema": "orchestrator-history-event/v2", "sequence": sequence, "parent_digest": parent, "event_type": event_type, "contract_version": "workflow-execution/v2", "payload": copy.deepcopy(dict(payload))}
    value["event_digest"] = _digest(value)
    return value


def _loop_request(value: Any) -> Mapping[str, Any] | None:
    """Return an explicit workflow-loop/v1 request, if one is present."""

    if not isinstance(value, Mapping):
        return None
    if value.get("schema") == _LOOP_SCHEMA:
        return value
    for field in ("workflow_loop", "loop_request", "completion_request"):
        nested = value.get(field)
        if isinstance(nested, Mapping) and (
            nested.get("schema") == _LOOP_SCHEMA
            or "identity" in nested
            or "candidate_digest" in nested
        ):
            return nested
    return None


def _loop_progress_fields(value: Any) -> set[str]:
    """Find legacy progress controls without allowing them to decide progress."""

    found: set[str] = set()
    if isinstance(value, Mapping):
        found.update(_LOOP_PROGRESS_FIELDS.intersection(value))
        for item in value.values():
            found.update(_loop_progress_fields(item))
    elif isinstance(value, (list, tuple)):
        for item in value:
            found.update(_loop_progress_fields(item))
    return found


def _loop_timeout_values(value: Any) -> dict[str, Any]:
    """Copy process-call monitoring values, never phase progress values."""

    if not isinstance(value, Mapping):
        return {}
    return {
        key: copy.deepcopy(value[key])
        for key in sorted(_LOOP_TIMEOUT_FIELDS)
        if key in value
    }


def _loop_ref(value: Any, label: str) -> dict[str, str]:
    try:
        return validate_ref(value, label)
    except LoopContractError as error:
        raise RuntimeExecutionError(str(error)) from error


class RuntimeExecution:
    """Execute, review, optionally repair once, and persist one Group E route."""

    _POLICY = "agent-workflows/groups/required-only-feedback-execution-policy-v1.json"
    # Compiler contract references. Actual readiness observations are in the
    # persisted frontier's runtime-e-readiness-receipt.
    _E1_CONTRACT_SOURCES = (
        ("readiness-approval", "agent-workflows/skills/implementation-readiness-review/SKILL.md"),
        ("task-dag", "agent-workflows/groups/planning.json"),
        ("workspace-receipt", "agent-workflows/catalog.yaml"),
        ("git-state-receipt", _POLICY),
    )

    def __init__(
        self,
        project: str | Path | None = None,
        run_id: str | None = None,
        *,
        runtime: Any | None = None,
        kernel: Any | None = None,
    ) -> None:
        """Create the legacy runtime or the additive loop facade.

        ``project``/``run_id`` retain the historical constructor.  Supplying a
        Kernel is a small boundary seam for the v1 loop path: it lets tests and
        callers provide the already-selected Control Kernel without making the
        loop facade discover files or create a second state owner.
        """

        if runtime is not None and kernel is not None and getattr(runtime, "kernel", kernel) is not kernel:
            raise RuntimeExecutionError("runtime and kernel refer to different Control Kernels")
        if runtime is None and kernel is None:
            if project is None or run_id is None:
                raise RuntimeExecutionError("runtime execution requires project and run_id")
            runtime = InceptionRuntime(project, run_id)
            kernel = runtime.kernel
        elif kernel is None:
            kernel = getattr(runtime, "kernel", None)
        if kernel is None:
            raise RuntimeExecutionError("runtime execution requires a Control Kernel")
        self.runtime = runtime
        self.kernel = kernel

    # -- workflow-loop/v1 execution facade ---------------------------------

    def loop_status(self) -> dict[str, Any]:
        """Return the current immutable loop projection without a clock read."""

        state = self._loop_state()
        loop = state["loop_control"]
        identity = self._loop_identity(loop)
        history = self._loop_history(state)
        latest = copy.deepcopy(history[-1]) if history else None
        outcome = loop.get("outcome") or loop.get("terminal_outcome")
        if latest is not None and latest.get("status") == "execution-unknown":
            outcome = "recovery-required"
        return {
            "schema": "workflow-loop-runtime-status/v1",
            "contract_version": _LOOP_SCHEMA,
            "run_id": state.get("run_id"),
            "revision": state.get("revision"),
            "head": _head(self.kernel),
            "identity": identity,
            "policy": phase_limits(identity["phase"]),
            "loop_control": copy.deepcopy(loop),
            "history": history,
            "latest_event": latest,
            "status": loop.get("status", "idle"),
            "outcome": outcome,
            "recovery_required": outcome == "recovery-required" or bool(loop.get("recovery_required")),
            "dispatch_allowed": bool(loop.get("dispatch_allowed", True)) and outcome != "recovery-required",
            "durable": True,
            "non_mutating": True,
        }

    def transition_loop_phase(
        self,
        phase: str,
        logical_task_id: str,
        *,
        identity: Mapping[str, Any] | None = None,
        reason: str = "operational-phase-entry",
    ) -> dict[str, Any]:
        """Enter one logical task phase through the Kernel's CAS boundary."""

        state = self._loop_state()
        current = self._loop_identity(state["loop_control"])
        desired = self._desired_loop_identity(
            state,
            phase,
            logical_task_id,
            supplied=identity,
        )
        # Scope/candidate/session revisions are deliberately outside the
        # counter identity.  Treating such a rename as a phase transition
        # would make the Kernel reset or archive the same finite loop.
        if counter_identity(current) == counter_identity(desired):
            return self.loop_status()
        try:
            result = self.kernel.transition_loop_phase(
                desired,
                reason=reason,
                expected_revision=state["revision"],
            )
        except Exception as error:
            raise RuntimeExecutionError("loop phase transition was rejected: " + str(error)) from error
        return self._loop_transition_result("phase-transition", result, desired)

    def execute_loop(
        self,
        task_id: str | Mapping[str, Any] | None = None,
        request: Mapping[str, Any] | None = None,
        *,
        dispatcher: Any | None = None,
        result_ref: Mapping[str, Any] | None = None,
        phase: str = "E3",
        integration: bool = False,
        process_timeout: float | None = None,
        metric_events: Sequence[Mapping[str, Any]] | None = None,
    ) -> dict[str, Any]:
        """Run one workflow-loop/v1 dispatch and derive its next decision.

        This is the additive path for loop-control Runs.  It performs only the
        durable loop transitions required around one caller-supplied dispatch:
        phase entry, reserve, running, and result acceptance (or
        ``execution-unknown``).  B4/B5 review, repair-batch, evidence, and
        completion facades remain non-authorizing derivations over the supplied
        request and current state.
        """

        if isinstance(task_id, Mapping) and request is None:
            request = task_id
            task_id = None
        if request is None or not isinstance(request, Mapping):
            raise RuntimeExecutionError("workflow-loop execution requires a request mapping")
        values = copy.deepcopy(dict(request))
        nested = _loop_request(values)
        if nested is None:
            raise RuntimeExecutionError("workflow-loop execution requires workflow-loop/v1")
        if nested is not values:
            # Keep envelope sidecars (notably review/evidence inputs) while
            # treating the explicit nested request as the canonical payload.
            merged = copy.deepcopy(dict(nested))
            for key, value in values.items():
                if key not in {"workflow_loop", "loop_request", "completion_request"}:
                    merged.setdefault(key, copy.deepcopy(value))
            values = merged

        state = self._loop_state()
        current_identity = self._loop_identity(state["loop_control"])
        supplied_identity = values.get("identity")
        requested_identity = supplied_identity if isinstance(supplied_identity, Mapping) else None
        requested_task = task_id or (
            requested_identity.get("logical_task_id") if requested_identity else None
        )
        if not isinstance(requested_task, str) or not requested_task:
            requested_task = current_identity["logical_task_id"]
        self._validate_loop_task_grant(state, requested_task)
        requested_phase = values.get("phase", phase)
        if requested_identity is not None:
            try:
                requested_phase = validate_work_identity(requested_identity)["phase"]
            except LoopContractError as error:
                raise RuntimeExecutionError("workflow-loop identity is invalid: " + str(error)) from error
        if integration or str(requested_phase).startswith("E8") or str(requested_phase).startswith("E9"):
            requested_phase = "E8" if str(requested_phase) in {"E8", "E8-E9"} else "E9"
        else:
            # E3 is the explicit logical-task entry point.  E4-E7 are review
            # sub-stages of the same E3-E7 counter identity and do not create a
            # second task identity here.
            requested_phase = "E3"

        supplied_result = result_ref if result_ref is not None else values.get("result_ref")
        facade_preflight = self._loop_facades(values)
        if facade_preflight.get("error"):
            raise RuntimeExecutionError(
                "workflow-loop completion input is invalid: "
                + str(facade_preflight["error"])
            )
        if dispatcher is None and supplied_result is not None:
            # A caller-supplied result has no ambiguous external side effect.
            # Reject malformed references before reserve/running can mutate
            # durable history.
            self._dispatch_result_ref(supplied_result, "preflight")

        # Caller validation and phase-selection refusals are not execution
        # outcomes.  In particular, a foreign lineage must not be able to
        # close an otherwise untouched Run by presenting invalid input.
        self.transition_loop_phase(
            requested_phase,
            requested_task,
            identity=requested_identity,
            reason="logical-task-entry" if requested_phase == "E3" else "integration-entry",
        )

        state = self._loop_state()
        identity_value = self._loop_identity(state["loop_control"])
        history = self._loop_history(state)
        progress_fields = _loop_progress_fields(values)
        monitor = _loop_timeout_values(values)
        if process_timeout is not None:
            monitor["process_timeout"] = copy.deepcopy(process_timeout)
        if not monitor:
            monitor = _loop_timeout_values({"process_timeout": values.get("process_timeout")})

        # A current recovery terminal is never silently re-dispatched.
        if state["loop_control"].get("recovery_required") or (
            history and history[-1].get("status") == "execution-unknown"
        ):
            return self._loop_terminal_result(
                state,
                outcome="recovery-required",
                reason="execution outcome is unknown; explicit recovery is required",
                task_id=requested_task,
                progress_fields=progress_fields,
                monitor=monitor,
                record_values=values,
            )
        terminal = state["loop_control"].get("terminal_record")
        if isinstance(terminal, Mapping):
            resume_evidence = values.get("resume_evidence_ref")
            integration_return = values.get("iteration_kind") in {"integration-return", "integration"}
            if resume_evidence is None and integration_return and terminal.get("outcome") == "completed":
                resume_evidence = identity_value.get("predecessor_ref")
            if resume_evidence is not None:
                try:
                    self.resume_loop_outcome(
                        resume_evidence,
                        reason=values.get("resume_reason", "integration return" if integration_return else "required input received"),
                    )
                    state = self._loop_state()
                    history = self._loop_history(state)
                except RuntimeExecutionError as error:
                    return self._loop_terminal_result(
                        state,
                        outcome=terminal["outcome"],
                        reason="workflow-loop resume was rejected: " + str(error),
                        task_id=requested_task,
                        progress_fields=progress_fields,
                        monitor=monitor,
                        record_values=values,
                    )
            else:
                return self._loop_terminal_result(
                    state,
                    outcome=terminal["outcome"],
                    reason="workflow-loop outcome is terminal; explicit resume evidence is required",
                    task_id=requested_task,
                    progress_fields=progress_fields,
                    monitor=monitor,
                    record_values=values,
                )

        callback = dispatcher
        if callback is None and supplied_result is None:
            return self._loop_terminal_result(
                state,
                outcome="needs-input",
                reason="a dispatcher or result_ref is required before reservation",
                task_id=requested_task,
                progress_fields=progress_fields,
                monitor=monitor,
                record_values=values,
            )
        explicit_kind = values.get("iteration_kind", values.get("event_kind"))
        if explicit_kind == "integration":
            explicit_kind = "integration-return"
        if explicit_kind == "retry":
            explicit_kind = "technical-retry"
        kind = explicit_kind
        if kind is None:
            kind = "initial" if not history else "improvement"
        explicit_command_id = values.get("command_id")
        if explicit_command_id is None:
            implicit_identity = {
                "counter_identity": counter_identity(identity_value),
                "iteration_kind": explicit_kind or "default",
            }
            request_digest = canonical_digest(implicit_identity)[7:31]
            command_id = f"workflow-loop-{requested_task}-{requested_phase}-{request_digest}"
        else:
            command_id = explicit_command_id
        if not isinstance(command_id, str) or not command_id:
            raise RuntimeExecutionError("workflow-loop command_id must be a stable identifier")

        requested_event_id = values.get("event_id")
        if explicit_command_id is not None:
            frozen_request = {
                key: copy.deepcopy(value)
                for key, value in values.items()
                if key not in {"command_id", "event_id", "idempotency_key"}
            }
            payload_digest = canonical_digest(frozen_request)[7:31]
            bound_event_id = f"event-{command_id}-{payload_digest}"
            if requested_event_id is not None and requested_event_id != bound_event_id:
                raise RuntimeExecutionError(
                    "explicit event_id does not bind the workflow-loop request payload"
                )
            requested_event_id = bound_event_id

        matching = [item for item in history if item.get("command_id") == command_id]
        if len(matching) > 1:
            raise RuntimeExecutionError("workflow-loop command_id is not unique in durable history")
        event = copy.deepcopy(matching[0]) if matching else None
        if event is not None:
            incompatible = (
                event.get("identity") != identity_value
                or (explicit_kind is not None and event.get("kind") != explicit_kind)
                or (values.get("attempt") is not None and event.get("attempt") != values["attempt"])
            )
            if incompatible:
                raise RuntimeExecutionError(
                    "workflow-loop command_id was replayed with a different reservation payload"
                )
            if event.get("status") == "evaluated":
                if supplied_result is not None:
                    replay_ref, _ = self._dispatch_result_ref(
                        supplied_result, event["event_id"]
                    )
                    if replay_ref != event.get("result_ref"):
                        raise RuntimeExecutionError(
                            "workflow-loop command_id was replayed with a different result"
                        )
                if requested_event_id is not None and event.get("event_id") != requested_event_id:
                    raise RuntimeExecutionError(
                        "workflow-loop command_id was replayed with a different request payload"
                    )
                return self._derive_loop_decision(
                    values,
                    state,
                    task_id=requested_task,
                    event=event,
                    progress_fields=progress_fields,
                    monitor=monitor,
                    metric_events=metric_events,
                )
            if event.get("status") == "running":
                return self._mark_loop_unknown_result(
                    state,
                    event["event_id"],
                    "replayed command has a running dispatch with an ambiguous outcome",
                    task_id=requested_task,
                    progress_fields=progress_fields,
                    monitor=monitor,
                    record_values=values,
                )
            if requested_event_id is not None and event.get("event_id") != requested_event_id:
                raise RuntimeExecutionError(
                    "workflow-loop command_id was replayed with a different request payload"
                )
            if event.get("status") != "reserved":
                raise RuntimeExecutionError(
                    "workflow-loop command cannot be replayed from its current status"
                )
            after_reserve = state
        else:
            attempt = values.get("attempt")
            if attempt is None:
                attempt = 0 if not history else history[-1]["attempt"] + 1
            predecessor = event_ref(history[-1]) if history else None
            event_id = requested_event_id
            try:
                self.kernel.reserve_loop_event(
                    identity=identity_value,
                    command_id=command_id,
                    event_id=event_id,
                    kind=kind,
                    attempt=attempt,
                    predecessor_ref=predecessor,
                    expected_revision=state["revision"],
                    idempotency_key=values.get("idempotency_key", "workflow-loop-reserve:" + command_id),
                )
            except CounterExhaustedError as error:
                return self._loop_terminal_result(
                    state,
                    outcome="iteration-limit",
                    reason=str(error),
                    task_id=requested_task,
                    progress_fields=progress_fields,
                    monitor=monitor,
                    record_values=values,
                )
            except Exception as error:  # validation/CAS refusal, no dispatch occurred
                raise RuntimeExecutionError(
                    "workflow-loop reservation was rejected: " + str(error)
                ) from error

            after_reserve = self._loop_state()
            reserved_history = self._loop_history(after_reserve)
            event = reserved_history[-1] if reserved_history else None
        if not isinstance(event, Mapping):
            return self._loop_terminal_result(
                after_reserve,
                outcome="execution-failed",
                reason="Kernel accepted a reservation without an event",
                task_id=requested_task,
                progress_fields=progress_fields,
                monitor=monitor,
                record_values=values,
            )
        # A replayed evaluated command has a durable result already; invoking a
        # dispatcher again would violate exactly-once execution.
        if event.get("status") == "evaluated":
            return self._derive_loop_decision(
                values,
                after_reserve,
                task_id=requested_task,
                event=event,
                progress_fields=progress_fields,
                monitor=monitor,
                metric_events=metric_events,
            )
        try:
            running = self.kernel.mark_loop_running(
                event["event_id"],
                expected_revision=after_reserve["revision"],
                idempotency_key="workflow-loop-running:" + event["event_id"],
            )
            running_state = self._loop_state(running)
        except Exception as error:
            # The event is durably reserved and no external dispatch has run,
            # so this is a retryable control-plane refusal, not a Run outcome.
            raise RuntimeExecutionError(
                "loop running transition was rejected: " + str(error)
            ) from error

        dispatch_result: Any = supplied_result
        if callback is not None:
            package = {
                "schema": "workflow-loop-dispatch/v1",
                "identity": copy.deepcopy(identity_value),
                "event": copy.deepcopy(event),
                "request": copy.deepcopy(values),
                "process_timeout": copy.deepcopy(monitor),
                "non_authorizing": True,
            }
            try:
                dispatch_result = callback(package)
            except Exception as error:  # noqa: BLE001 - dispatch outcome is unknown
                return self._mark_loop_unknown_result(
                    running_state,
                    event["event_id"],
                    "dispatch outcome is ambiguous: " + str(error),
                    task_id=requested_task,
                    progress_fields=progress_fields,
                    monitor=monitor,
                    record_values=values,
                )

        if dispatch_result is None:
            return self._mark_loop_unknown_result(
                running_state,
                event["event_id"],
                "dispatch returned no terminal result reference",
                task_id=requested_task,
                progress_fields=progress_fields,
                monitor=monitor,
                record_values=values,
            )
        if isinstance(dispatch_result, Mapping) and str(
            dispatch_result.get("status", dispatch_result.get("outcome", ""))
        ).lower() in {"unknown", "ambiguous", "execution-unknown", "recovery-required"}:
            return self._mark_loop_unknown_result(
                running_state,
                event["event_id"],
                "dispatch reported an ambiguous execution outcome",
                task_id=requested_task,
                progress_fields=progress_fields,
                monitor=monitor,
                record_values=values,
            )
        try:
            accepted_ref, dispatch_status = self._dispatch_result_ref(
                dispatch_result, event["event_id"]
            )
            accepted = self.kernel.accept_loop_result(
                event["event_id"],
                accepted_ref,
                expected_revision=running_state["revision"],
                idempotency_key="workflow-loop-accept:" + event["event_id"],
            )
        except Exception as error:  # noqa: BLE001 - acceptance outcome is unknown
            return self._mark_loop_unknown_result(
                running_state,
                event["event_id"],
                "result acceptance is ambiguous: " + str(error),
                task_id=requested_task,
                progress_fields=progress_fields,
                monitor=monitor,
                record_values=values,
            )

        accepted_state = self._loop_state(accepted)
        if dispatch_status in {"failed", "error", "failure"}:
            values["execution_status"] = "failed"
        values.setdefault("events", self._loop_history(accepted_state))
        return self._derive_loop_decision(
            values,
            accepted_state,
            task_id=requested_task,
            event=self._loop_history(accepted_state)[-1],
            progress_fields=progress_fields,
            monitor=monitor,
            metric_events=metric_events,
        )

    def _loop_state(self, supplied: Mapping[str, Any] | None = None) -> dict[str, Any]:
        state = supplied
        if state is None:
            reader = getattr(self.kernel, "read_state", None) or getattr(self.kernel, "snapshot", None)
            if not callable(reader):
                raise RuntimeExecutionError("workflow-loop Kernel must expose read_state()")
            state = reader()
        if not isinstance(state, Mapping) or not isinstance(state.get("loop_control"), Mapping):
            raise RuntimeExecutionError("workflow-loop/v1 requires loop_control state")
        return copy.deepcopy(dict(state))

    @staticmethod
    def _loop_identity(loop: Mapping[str, Any]) -> dict[str, Any]:
        try:
            return validate_work_identity(loop.get("identity"))
        except LoopContractError as error:
            raise RuntimeExecutionError("loop identity is invalid: " + str(error)) from error

    def _validate_loop_task_grant(self, state: Mapping[str, Any], task_id: str) -> None:
        """Bind project-local loop dispatches to the accepted D8/D10 task set.

        Kernel-only fixtures intentionally omit the operational grant.  A
        project-local runtime, however, must never turn a renamed task id into
        a fresh logical counter that evades the planning boundary.
        """

        if self.runtime is None:
            return
        metadata = state.get("metadata")
        grant = metadata.get("operational_task_grant") if isinstance(metadata, Mapping) else None
        tasks = grant.get("tasks") if isinstance(grant, Mapping) else None
        if not isinstance(tasks, Mapping) or task_id not in tasks:
            raise RuntimeExecutionError("Task is not present in the accepted D8/D10 operational grant")

    def _loop_history(self, state: Mapping[str, Any]) -> list[dict[str, Any]]:
        loader = getattr(self.kernel, "_loop_history", None)
        if callable(loader):
            try:
                value = loader(state)
            except Exception as error:
                raise RuntimeExecutionError("loop history cannot be loaded: " + str(error)) from error
            if isinstance(value, list):
                return copy.deepcopy(value)
        loop = state.get("loop_control", {})
        value = loop.get("history", loop.get("events")) if isinstance(loop, Mapping) else None
        if isinstance(value, list):
            return copy.deepcopy(value)
        refs = loop.get("event_refs", []) if isinstance(loop, Mapping) else []
        reader = getattr(self.kernel, "read_object", None)
        if isinstance(refs, list) and callable(reader):
            result = []
            for ref in refs:
                loaded = reader(ref)
                if not isinstance(loaded, Mapping) or not isinstance(loaded.get("payload"), Mapping):
                    raise RuntimeExecutionError("loop event reference is malformed")
                result.append(copy.deepcopy(dict(loaded["payload"])))
            return result
        return []

    def _desired_loop_identity(
        self,
        state: Mapping[str, Any],
        phase: str,
        logical_task_id: str,
        *,
        supplied: Mapping[str, Any] | None,
    ) -> dict[str, Any]:
        current = self._loop_identity(state["loop_control"])
        if supplied is not None:
            try:
                identity = validate_work_identity(supplied)
            except LoopContractError as error:
                raise RuntimeExecutionError("workflow-loop identity is invalid: " + str(error)) from error
            if identity["work_lineage_id"] != current["work_lineage_id"]:
                raise RuntimeExecutionError("workflow-loop identity changes work lineage")
            if identity["logical_task_id"] != logical_task_id:
                raise RuntimeExecutionError("workflow-loop identity changes logical task")
            if phase_policy(identity["phase"])["policy_id"] != phase_policy(phase)["policy_id"]:
                raise RuntimeExecutionError("workflow-loop identity phase is not the requested policy")
            return identity
        history = self._loop_history(state)
        identity = copy.deepcopy(current)
        identity["logical_task_id"] = logical_task_id
        identity["phase"] = phase
        identity["scope_revision"] = f"{current['scope_revision']}:{phase}"
        identity["predecessor_ref"] = event_ref(history[-1]) if history else None
        return validate_work_identity(identity)

    def _loop_transition_result(
        self, operation: str, state: Mapping[str, Any], identity: Mapping[str, Any]
    ) -> dict[str, Any]:
        value = self._loop_state(state)
        return {
            "schema": "workflow-loop-transition/v1",
            "contract_version": _LOOP_SCHEMA,
            "operation": operation,
            "revision": value.get("revision"),
            "head": _head(self.kernel),
            "identity": copy.deepcopy(dict(identity)),
            "policy": phase_limits(identity["phase"]),
            "loop_control": copy.deepcopy(value["loop_control"]),
            "history": self._loop_history(value),
            "durable": True,
            "non_mutating": True,
        }

    @staticmethod
    def _dispatch_result_ref(value: Any, event_id: str) -> tuple[dict[str, str], str | None]:
        status = None
        candidate = value
        if isinstance(value, Mapping):
            status = value.get("status", value.get("outcome"))
            candidate = value.get("result_ref", value)
        if isinstance(candidate, Mapping) and {"id", "digest"}.issubset(candidate):
            return _loop_ref({"id": candidate["id"], "digest": candidate["digest"]}, "result_ref"), status
        try:
            generated = {"id": "result-" + event_id, "digest": canonical_digest(value)}
            return _loop_ref(generated, "result_ref"), status
        except (LoopContractError, RuntimeExecutionError, TypeError, ValueError) as error:
            raise RuntimeExecutionError("dispatch result is not digest-bound: " + str(error)) from error

    def _mark_loop_unknown_result(
        self,
        state: Mapping[str, Any],
        event_id: str,
        reason: str,
        *,
        task_id: str,
        progress_fields: set[str],
        monitor: Mapping[str, Any] | None = None,
        record_values: Mapping[str, Any] | None = None,
    ) -> dict[str, Any]:
        try:
            current = self._loop_state(state)
            updated = self.kernel.mark_loop_execution_unknown(
                event_id,
                expected_revision=current["revision"],
                idempotency_key="workflow-loop-unknown:" + event_id,
            )
            return self._loop_terminal_result(
                updated,
                outcome="recovery-required",
                reason=reason,
                task_id=task_id,
                progress_fields=progress_fields,
                monitor=monitor,
                record_values=record_values,
            )
        except Exception as error:  # noqa: BLE001 - inability to persist unknown is terminal
            return self._loop_terminal_result(
                current if "current" in locals() else state,
                outcome="execution-failed",
                reason=reason + "; could not persist execution-unknown: " + str(error),
                task_id=task_id,
                progress_fields=progress_fields,
                monitor=monitor,
                record_values=record_values,
            )

    def _derive_loop_decision(
        self,
        values: Mapping[str, Any],
        state: Mapping[str, Any],
        *,
        task_id: str,
        event: Mapping[str, Any],
        progress_fields: set[str],
        monitor: Mapping[str, Any],
        metric_events: Sequence[Mapping[str, Any]] | None,
    ) -> dict[str, Any]:
        current = self._loop_state(state)
        history = self._loop_history(current)
        command = copy.deepcopy(dict(values))
        command["schema"] = _LOOP_SCHEMA
        command["phase"] = self._loop_identity(current["loop_control"])["phase"]
        command["identity"] = self._loop_identity(current["loop_control"])
        command["events"] = history
        try:
            from .execution_v2_orchestrator import DAGOrchestrator

            compiled = DAGOrchestrator().compile_workflow_loop(command, {"events": history})
        except Exception as error:  # noqa: BLE001 - facade errors become explicit failure
            compiled = {
                "schema": "workflow-loop-transition/v1",
                "phase": command["phase"],
                "policy": phase_limits(command["phase"]),
                "outcome": "execution-failed",
                "reason": "workflow-loop facade failed",
                "error": str(error),
                "counters": {},
            }
        facade = self._loop_facades(command)
        evidence_assessments = self._loop_evidence_assessments(command)
        metrics = self._loop_metrics(metric_events or command.get("metric_events", command.get("metrics_events")))
        result = {
            "schema": "workflow-loop-runtime-execution/v1",
            "contract_version": _LOOP_SCHEMA,
            "task_id": task_id,
            "phase": command["phase"],
            "identity": copy.deepcopy(command["identity"]),
            "event": copy.deepcopy(dict(event)),
            "loop_control": copy.deepcopy(current["loop_control"]),
            "history": history,
            "head": _head(self.kernel),
            "transition": compiled,
            "policy": copy.deepcopy(compiled.get("policy", phase_limits(command["phase"]))),
            "outcome": compiled.get("outcome", "execution-failed"),
            "reason": compiled.get("reason", "workflow-loop decision unavailable"),
            "counters": copy.deepcopy(compiled.get("counters", {})),
            "completion": copy.deepcopy(compiled.get("completion")),
            "validation": facade.get("validation"),
            "facade_error": facade.get("error"),
            "repair_plan": copy.deepcopy(compiled.get("repair_plan")),
            "review_packages": copy.deepcopy(compiled.get("review_packages", [])),
            "accepted_reviews": copy.deepcopy(compiled.get("accepted_reviews", [])),
            "batch_resolutions": copy.deepcopy(compiled.get("batch_resolutions", [])),
            "evidence_assessments": evidence_assessments,
            "metrics": metrics,
            "process_timeout": copy.deepcopy(dict(monitor)),
            "ignored_progress_fields": sorted(progress_fields),
            "hard_failure": compiled.get("hard_failure") is True,
            "limit_exhausted": compiled.get("limit_exhausted") is True,
            "execution_unknown": compiled.get("execution_unknown") is True,
            "needs_recovery": compiled.get("needs_recovery") is True,
            "needs_input": compiled.get("needs_input") is True,
            "durable": True,
            "non_mutating": True,
        }
        if facade.get("error") and result["outcome"] == "completed":
            result["outcome"] = "execution-failed"
            result["reason"] = facade["error"]
            result["hard_failure"] = True
        if result["outcome"] in TERMINAL_OUTCOMES:
            return self._loop_terminal_result(
                current,
                outcome=result["outcome"],
                reason=result["reason"],
                task_id=task_id,
                progress_fields=progress_fields,
                monitor=monitor,
                record_values=command,
                completion=compiled.get("completion"),
                base_result=result,
            )
        return result

    @staticmethod
    def _loop_facades(values: Mapping[str, Any]) -> dict[str, Any]:
        explicit_completion = "completion_request" in values
        candidate = values.get("completion_request", values)
        if not isinstance(candidate, Mapping):
            return {"validation": None, "error": "completion request is malformed"}
        request = copy.deepcopy(dict(candidate))
        request.pop("phase", None)
        request.pop("profile", None)
        request.pop("events", None)
        request.pop("history", None)
        request.pop("state_entries", None)
        request.pop("iteration_events", None)
        request.pop("loop_control", None)
        request.pop("schema", None)
        for field in _LOOP_PROGRESS_FIELDS:
            request.pop(field, None)
        required_identity = {"identity", "candidate_digest", "package_digest"}
        if explicit_completion and not required_identity.issubset(request):
            return {
                "validation": None,
                "error": "completion request is missing identity or digest bindings",
            }
        if not required_identity.issubset(request):
            return {"validation": None, "error": None}
        try:
            validation = WorkflowLoopValidator().validate(
                {"schema": _LOOP_SCHEMA, **request}
            )
            # Evaluate through the named B4 facade as well.  The result is
            # intentionally advisory and never grants dispatch or closure.
            MechanicalCompletion().evaluate({"schema": _LOOP_SCHEMA, **request})
            return {"validation": validation, "error": None}
        except (V2ContractError, CompletionError, ValueError, TypeError) as error:
            return {"validation": None, "error": str(error)}

    @staticmethod
    def _loop_evidence_assessments(values: Mapping[str, Any]) -> list[dict[str, Any]]:
        candidate = values.get("completion_request")
        if not isinstance(candidate, Mapping):
            candidate = values
        current = candidate.get("current_inputs", values.get("current_inputs"))
        impact = candidate.get("change_impact", values.get("change_impact"))
        evidence = candidate.get("evidence", values.get("evidence", []))
        if current is None or impact is None or not isinstance(evidence, list):
            return []
        result = []
        for item in evidence:
            try:
                result.append(assess_evidence(item, current, impact))
            except EvidenceValidityError as error:
                result.append({
                    "schema": "loop-evidence-assessment/v1",
                    "evidence_id": item.get("evidence_id", "unknown") if isinstance(item, Mapping) else "unknown",
                    "status": "unknown",
                    "reasons": [str(error)],
                })
        return result

    @staticmethod
    def _loop_metrics(events: Any) -> dict[str, Any] | None:
        if events is None:
            return None
        try:
            return derive_metrics(events)
        except (LoopMetricsError, TypeError, ValueError) as error:
            return {"schema": "loop-metrics-error/v1", "error": str(error)}

    def _loop_terminal_result(
        self,
        state: Mapping[str, Any],
        *,
        outcome: str,
        reason: str,
        task_id: str,
        progress_fields: set[str],
        monitor: Mapping[str, Any] | None = None,
        record_values: Mapping[str, Any] | None = None,
        completion: Mapping[str, Any] | None = None,
        base_result: Mapping[str, Any] | None = None,
    ) -> dict[str, Any]:
        value = self._loop_state(state)
        requested_outcome = outcome
        recorded = False
        terminal_error = None
        terminal = value["loop_control"].get("terminal_record")
        if isinstance(terminal, Mapping):
            outcome = terminal["outcome"]
            reason = terminal["reason"]
            recorded = True
        elif outcome in TERMINAL_OUTCOMES:
            try:
                record = self._build_loop_terminal_record(
                    value,
                    outcome=outcome,
                    reason=reason,
                    record_values=record_values,
                    completion=completion,
                )
                value = self._loop_state(
                    self.kernel.record_loop_outcome(
                        record,
                        expected_revision=value["revision"],
                        idempotency_key="workflow-loop-terminal:" + record["terminal_id"],
                    )
                )
                terminal = value["loop_control"].get("terminal_record")
                recorded = isinstance(terminal, Mapping)
            except Exception as error:  # noqa: BLE001 - report a non-durable stop honestly
                terminal_error = str(error)
        durable_recovery = bool(value["loop_control"].get("recovery_required"))
        if (
            requested_outcome in TERMINAL_OUTCOMES
            and not recorded
            and not (requested_outcome == "recovery-required" and durable_recovery)
        ):
            outcome = "execution-failed"
            reason = (
                f"could not durably record {requested_outcome} outcome: "
                f"{terminal_error or 'terminal record was not accepted'}"
            )
        identity = self._loop_identity(value["loop_control"])
        policy = phase_limits(identity["phase"])
        history = self._loop_history(value)
        result = copy.deepcopy(dict(base_result or {}))
        result.update({
            "schema": "workflow-loop-runtime-execution/v1",
            "contract_version": _LOOP_SCHEMA,
            "task_id": task_id,
            "phase": identity["phase"],
            "identity": identity,
            "loop_control": copy.deepcopy(value["loop_control"]),
            "history": history,
            "head": _head(self.kernel),
            "policy": policy,
            "outcome": outcome,
            "reason": reason,
            "counters": copy.deepcopy(value["loop_control"].get("counters", {})),
            "process_timeout": copy.deepcopy(dict(monitor or {})),
            "ignored_progress_fields": sorted(progress_fields),
            "hard_failure": outcome == "execution-failed",
            "limit_exhausted": outcome == "iteration-limit",
            "execution_unknown": outcome == "recovery-required" or durable_recovery,
            "needs_recovery": outcome == "recovery-required" or durable_recovery,
            "needs_input": outcome == "needs-input",
            "terminal_recorded": recorded,
            "terminal_ref": copy.deepcopy(value["loop_control"].get("terminal_ref")),
            "terminal_record": copy.deepcopy(terminal),
            "terminal_error": terminal_error,
            "requested_outcome": requested_outcome,
            "durable": recorded,
            "non_mutating": True,
        })
        return result

    @staticmethod
    def _validated_terminal_assessments(
        values: Mapping[str, Any],
    ) -> tuple[list[dict[str, Any]], list[dict[str, Any]], list[dict[str, Any]], list[str]]:
        aliases = (
            (("requirements", "completion_requirements", "requirement_assessments"), validate_requirement_assessment),
            (("reviews", "completion_reviews", "review_assessments"), validate_review_assessment),
            (("evidence", "completion_evidence", "evidence_records"), validate_evidence_record),
        )
        outputs: list[list[dict[str, Any]]] = []
        invalid: list[str] = []
        for fields, validator in aliases:
            raw = next((values[field] for field in fields if field in values), [])
            if not isinstance(raw, list):
                outputs.append([])
                invalid.append(fields[0] + "-malformed")
                continue
            checked = []
            try:
                checked = [validator(item) for item in raw]
            except (LoopContractError, TypeError, ValueError):
                checked = []
                invalid.append(fields[0] + "-malformed")
            outputs.append(checked)
        return outputs[0], outputs[1], outputs[2], invalid

    def _build_loop_terminal_record(
        self,
        state: Mapping[str, Any],
        *,
        outcome: str,
        reason: str,
        record_values: Mapping[str, Any] | None,
        completion: Mapping[str, Any] | None,
    ) -> dict[str, Any]:
        identity = self._loop_identity(state["loop_control"])
        values = copy.deepcopy(dict(record_values or {}))
        classification = completion if isinstance(completion, Mapping) else None
        source = classification.get("source_request") if classification is not None else None
        if not isinstance(source, Mapping):
            source = values.get("completion_request")
        if not isinstance(source, Mapping):
            source = values
        requirements, reviews, evidence, invalid = self._validated_terminal_assessments(source)

        candidate_digest = classification.get("candidate_digest") if classification is not None else None
        if candidate_digest is None:
            candidate_digest = source.get("candidate_digest", values.get("candidate_digest"))
        candidate_ref = source.get("candidate_ref", values.get("candidate_ref"))
        try:
            candidate_ref = validate_ref(candidate_ref, "candidate_ref", nullable=True)
        except LoopContractError:
            candidate_ref = None
            invalid.append("candidate-ref-malformed")
        if candidate_ref is not None and candidate_digest is not None and candidate_ref["digest"] != candidate_digest:
            candidate_ref = None
            invalid.append("candidate-ref-mismatch")
        if candidate_ref is None and isinstance(candidate_digest, str):
            try:
                candidate_ref = validate_ref(
                    {"id": "candidate-" + candidate_digest.removeprefix("sha256:")[:24], "digest": candidate_digest},
                    "candidate_ref",
                )
            except LoopContractError:
                invalid.append("candidate-digest-malformed")
        if candidate_ref is None:
            history = self._loop_history(state)
            latest_ref = history[-1].get("result_ref") if history else None
            try:
                candidate_ref = validate_ref(latest_ref, "candidate_ref", nullable=True)
            except LoopContractError:
                candidate_ref = None

        resume_ref = values.get("resume_ref", values.get("next_input_ref"))
        try:
            resume_ref = validate_ref(resume_ref, "resume_ref", nullable=True)
        except LoopContractError:
            resume_ref = None
            invalid.append("resume-ref-malformed")

        open_items = []
        if outcome != "completed":
            explicit = values.get("open_items", [])
            if isinstance(explicit, list):
                open_items.extend(item for item in explicit if isinstance(item, str) and item)
            if classification is not None and isinstance(classification.get("reason_codes"), list):
                open_items.extend(
                    item for item in classification["reason_codes"] if isinstance(item, str) and item
                )
            open_items.extend(invalid)
            open_items.append(reason)
        unsigned = {
            "schema": "loop-terminal-record/v1",
            "identity": identity,
            "outcome": outcome,
            "reason": reason,
            "candidate_ref": candidate_ref,
            "requirements": requirements,
            "reviews": reviews,
            "evidence": evidence,
            "open_items": list(dict.fromkeys(open_items)),
            "resume_ref": resume_ref if outcome != "completed" else None,
            "non_authorizing": True,
        }
        record = {
            "terminal_id": "terminal-" + canonical_digest(unsigned).removeprefix("sha256:")[:24],
            **unsigned,
        }
        try:
            return validate_terminal_record(record)
        except LoopContractError as error:
            raise RuntimeExecutionError("workflow-loop terminal record is invalid: " + str(error)) from error

    def resume_loop_outcome(
        self,
        evidence_ref: Mapping[str, Any],
        *,
        reason: str,
        resume_id: str | None = None,
    ) -> dict[str, Any]:
        """Persist evidence for reopening one non-exhausted terminal loop."""

        state = self._loop_state()
        loop = state["loop_control"]
        terminal_ref = loop.get("terminal_ref")
        if not isinstance(terminal_ref, Mapping):
            raise RuntimeExecutionError("workflow-loop has no terminal outcome to resume")
        terminal_payload_ref = {
            key: terminal_ref[key] for key in ("path", "digest") if key in terminal_ref
        }
        checked_evidence = _loop_ref(evidence_ref, "evidence_ref")
        unsigned = {
            "schema": "loop-resume-record/v1",
            "identity": self._loop_identity(loop),
            "terminal_ref": _loop_ref(terminal_payload_ref, "terminal_ref"),
            "reason": reason,
            "evidence_ref": checked_evidence,
            "non_authorizing": True,
        }
        record = {
            "resume_id": resume_id
            or "resume-" + canonical_digest(unsigned).removeprefix("sha256:")[:24],
            **unsigned,
        }
        try:
            checked = validate_resume_record(record)
            updated = self.kernel.resume_loop_outcome(
                checked,
                expected_revision=state["revision"],
                idempotency_key="workflow-loop-resume:" + checked["resume_id"],
            )
        except Exception as error:
            raise RuntimeExecutionError("workflow-loop resume was rejected: " + str(error)) from error
        return self._loop_transition_result("resume", updated, checked["identity"])

    def execute(
        self,
        task_id: str,
        execution_package_input: Mapping[str, Any],
        *,
        worker_assignment_id: str,
        broker: MacOSTaskProcessBroker,
        probes: Sequence[Mapping[str, str]],
        system_read_roots: Sequence[str],
        runtime_read_roots: Sequence[str],
        stage_inputs: Mapping[str, Mapping[str, Any]],
        changed_paths: Sequence[str],
        loop_level: str = "artifact",
    ) -> dict[str, Any]:
        # A loop-control Run has an explicit additive facade.  Keep the old
        # physical E1-E9 contract byte-for-byte for legacy review-budget Runs.
        current = self.kernel.read_state()
        loop_request = _loop_request(execution_package_input) or _loop_request(stage_inputs)
        if (isinstance(current, Mapping) and "loop_control" in current) or loop_request is not None:
            if loop_request is None:
                raise RuntimeExecutionError("workflow-loop/v1 request is required for a loop-control Run")
            callback = stage_inputs.get("dispatcher") if isinstance(stage_inputs, Mapping) else None
            supplied_result = stage_inputs.get("result_ref") if isinstance(stage_inputs, Mapping) else None
            return self.execute_loop(
                task_id,
                loop_request,
                dispatcher=callback if callable(callback) else None,
                result_ref=supplied_result,
                phase=loop_request.get("phase", "E3"),
                process_timeout=loop_request.get("process_timeout"),
            )
        required = {"E4", "E5", "E6", "E8"}
        if not isinstance(stage_inputs, Mapping) or not required.issubset(stage_inputs) or any(not isinstance(stage_inputs[key], Mapping) for key in required):
            raise RuntimeExecutionError("Group E stage inputs are incomplete")
        status = self.runtime.status()
        self.runtime._budget()
        if status["group"] != {"id": "E", "next_group": None, "status": "open"}:
            raise RuntimeExecutionError("runtime execution requires the current open Group E frontier")
        if self.runtime.records("E"):
            raise RuntimeExecutionError("Group E already contains canonical execution records")

        state = self.runtime.state
        grant = state.get("metadata", {}).get("operational_task_grant")
        if not isinstance(grant, Mapping) or self.kernel._operational_execution_grant(state, grant.get("bundle_ref")) != grant:
            raise RuntimeExecutionError("accepted D-to-E operational task grant is missing or stale")
        planned = grant.get("tasks", {}).get(task_id)
        if not isinstance(planned, Mapping):
            raise RuntimeExecutionError("Task is not present in the accepted D8/D10 plan")
        planned_scope = sorted(planned["write_scope"])
        if sorted(execution_package_input.get("resource_claims", {}).get("write_paths", [])) != planned_scope:
            raise RuntimeExecutionError("execution closure write scope differs from the D8/D10 grant")
        if execution_package_input.get("workspace_identity") != str(self.runtime.project):
            raise RuntimeExecutionError("execution package workspace differs from the selected project")
        if task_id in state.get("tasks", {}):
            raise RuntimeExecutionError("planned Task already has a persisted execution attempt")

        review_keys = {"axis", "actor_id", "reviewer_epoch_id", "other_reviewer_epoch_id", "worker_actor_id", "other_reviewer_actor_id", "findings"}
        for stage in ("E4", "E5"):
            if frozenset(stage_inputs[stage]) not in {frozenset(review_keys), frozenset(review_keys | {"frozen_binding"})}:
                raise RuntimeExecutionError(stage + " accepts only review judgment fields and an optional exact binding assertion")
        reviewer_ids = {stage_inputs[stage].get("actor_id") for stage in ("E4", "E5")}
        if worker_assignment_id in reviewer_ids or "runtime-e-validator" in reviewer_ids or len(reviewer_ids) != 2:
            raise RuntimeExecutionError("Worker, both Reviewers, and Validator must be distinct")
        normalized_findings = {stage: self._normalize_detailed_findings(stage_inputs[stage].get("findings", [])) for stage in ("E4", "E5")}
        self._prevalidate_dispositions(normalized_findings, stage_inputs["E6"])
        required_scopes = [item["proposed_scope"] for item in stage_inputs["E6"]["dispositions"] if item["classification"] == "required"]
        has_required = bool(required_scopes)
        if not has_required and ("E9" not in stage_inputs or not isinstance(stage_inputs["E9"], Mapping)):
            raise RuntimeExecutionError("the no-required Group E route requires E9")
        if has_required and "E7" in stage_inputs:
            raise RuntimeExecutionError("real required Findings cannot consume caller-supplied one-shot E7 evidence")

        # Reject cross-candidate E8 and, on the ordinary route, E9 evidence
        # before a Task or lease exists.  Required Findings stop at the
        # persisted E6 frontier, so their whole-verification evidence cannot
        # exist yet.
        closure, e8_base, aggregate = self._prevalidate_execution_lineage(
            execution_package_input, stage_inputs, require_e9=not has_required
        )
        pre_fix_artifacts = self._snapshot_artifacts(required_scopes[0]) if len(required_scopes) == 1 else None

        d_records = self.runtime.records("D")
        d_refs = {key: copy.deepcopy(d_records[key]["ref"]) for key in ("D8", "D10", "D12")}
        task_authority = self._kernel_authority(["publish_task_package", "claim_task", "accept_task_result", "publish_artifact", "release_task", "invalidate_task"], planned_scope)
        task_package = {
            "task_id": task_id,
            "attempt_id": "attempt-" + task_id + "-runtime-e",
            "assignment": {"role": "worker", "assignment_id": worker_assignment_id},
            "input_refs": list(d_refs.values()),
            "write_scope": planned_scope,
            "acceptance": ["E3 broker receipt", "E4/E5 independent review", "E9 whole verification"],
            "freshness": {"epoch_id": state["epoch"]["id"], "created_at_revision": state["revision"] + 1},
            "stop_conditions": ["scope escape", "stale HEAD", "broker refusal", "finite budget exhausted"],
            "invalidated": False,
            "stop_requested": False,
            "output_path": planned["report_path"],
            "status": "ready",
        }
        self.kernel.publish_task_package(task_id, task_package, assignment_id=worker_assignment_id, authority_ref=task_authority, input_refs=list(d_refs.values()), idempotency_key="runtime-e-package-" + task_id)
        self.kernel.claim_task(task_id, assignment_id=worker_assignment_id, authority_ref=task_authority, idempotency_key="runtime-e-claim-" + task_id)

        accepted = False
        try:
            state = self.kernel.read_state()
            task = state["tasks"][task_id]
            native_lease = state.get("leases", {}).get(task_id)
            if not isinstance(native_lease, Mapping) or native_lease.get("status") != "leased":
                raise RuntimeExecutionError("Kernel did not issue the planned Task lease")
            frozen_head = _head(self.kernel)
            package_ref = task["package_ref"]
            lease_id = "lease-" + _digest(native_lease)[7:23]
            authority = self._compiler_authority(frozen_head, package_ref, lease_id, worker_assignment_id)
            common = self._common_inputs(frozen_head, authority, loop_level)
            readiness = self._derive_readiness(state, grant, task_id, task, native_lease, d_refs, closure, authority)
            frontier = {
                "schema": "persisted-runtime-e-frontier/v2", "group": copy.deepcopy(state["group"]), "epoch": copy.deepcopy(state["epoch"]),
                "expected_head": copy.deepcopy(frozen_head), "objective_digest": state["objective_ref"]["digest"], "d_bundle_ref": copy.deepcopy(grant["bundle_ref"]),
                "d8_ref": d_refs["D8"], "d10_ref": d_refs["D10"], "d12_ref": d_refs["D12"], "task_id": task_id, "task_package_ref": copy.deepcopy(package_ref),
                "native_lease": copy.deepcopy(native_lease), "derived_lease_id": lease_id, "write_scope": planned_scope, "readiness_receipt": readiness,
            }
            frontier["frontier_digest"] = _digest(frontier)
            compiler = ExecutionGroupV1()
            compiled: dict[str, dict[str, Any]] = {}
            compiled["E1"] = self._compile(compiler, "E1", {
                **common, "readiness_checks": copy.deepcopy(readiness["checks"]), "open_blockers": copy.deepcopy(readiness["open_blockers"]),
                "resource_conflict": readiness["resource_conflict"], "terminal": readiness["terminal"],
                "physical_refs": [_source_ref(path, kind, frozen_head["revision"]) for kind, path in self._E1_CONTRACT_SOURCES], "persisted_frontier": frontier,
            }, authority, frozen_head)
            compiled["E2"] = self._compile(compiler, "E2", {
                **common, "preflight": compiled["E1"], "preflight_digest": compiled["E1"]["candidate_digest"], "execution_package_input": copy.deepcopy(dict(execution_package_input)),
                "assigned_role": "worker", "assigned_worker": worker_assignment_id, "output_path": planned["report_path"], "write_scope": planned_scope,
                "non_goals": ["writes outside accepted D8/D10 scope", "external mutation"], "acceptance": ["parent-broker-authenticated terminal receipt", "focused task check"],
                "stop_conditions": ["OS isolation unavailable", "scope escape", "stale package"], "task_process_mode": "macos-positive-allowlist",
                "broker_state_root": str(broker.state_root), "system_read_roots": list(system_read_roots), "runtime_read_roots": list(runtime_read_roots),
            }, authority, frozen_head)
            if compiled["E2"]["package"]["execution_closure"] != closure:
                raise RuntimeExecutionError("E2 closure differs from prevalidated lineage")
            runner_package, runner_policy = ArtifactCandidateBuilder.runner_inputs(compiled["E2"]["package"])
            executed = broker.execute(compiled["E2"]["package"], runner_package, runner_policy, probes=probes)
            compiled["E3"] = self._compile(compiler, "E3", {
                **common, "package": compiled["E2"]["package"], "terminal": "DONE", "changed_paths": sorted(changed_paths), "receipt": executed["terminal_receipt"],
                "recovery_receipt": executed["recovery_receipt"], "isolation_receipt_ref": executed["isolation_receipt_ref"], "task_id": task_id,
            }, authority, frozen_head, trusted_isolation_broker=broker)
            artifact_candidate = compiled["E3"]["artifact_candidate"]
            candidate_digest = artifact_candidate["candidate_digest"]
            if candidate_digest != closure["candidate_ref"]["digest"]:
                raise RuntimeExecutionError("E3 candidate differs from the E2 closure")

            budget = self._observed_budget(stage_inputs["E6"], state)
            binding = {"candidate_digest": candidate_digest, "aggregate_digest": aggregate["aggregate_digest"], "spec_digest": package_ref["digest"], "closure_digest": closure["closure_digest"], "budget_digest": _digest(budget)}
            for stage in ("E4", "E5"):
                supplied = copy.deepcopy(dict(stage_inputs[stage]))
                supplied_binding = supplied.pop("frozen_binding", None)
                if supplied_binding is not None and supplied_binding != binding:
                    raise RuntimeExecutionError(stage + " frozen binding differs from actual E2/E3 lineage")
                supplied["findings"] = normalized_findings[stage]
                compiled[stage] = self._compile(compiler, stage, {**common, **supplied, "frozen_binding": binding}, authority, frozen_head)

            ordinary_reviews = self._ordinary_reviews(compiled, candidate_digest, aggregate["aggregate_digest"])
            e6_supplied = copy.deepcopy(dict(stage_inputs["E6"]))
            caller_reviews = e6_supplied.pop("reviews", None)
            if caller_reviews is not None and caller_reviews != ordinary_reviews:
                raise RuntimeExecutionError("E6 caller reviews differ from exact compiled E4/E5 reports")
            initial_e6 = self._compile(compiler, "E6", {**common, **e6_supplied, "observed_budget": budget, "reviews": ordinary_reviews}, authority, frozen_head)
            compiled["E6"] = initial_e6
            final_reviews = ordinary_reviews
            final_dispositions = copy.deepcopy(e6_supplied["dispositions"])
            final_budget = budget
            repair = None
            if initial_e6.get("next") == "E7":
                compiled["E6-initial"] = compiled.pop("E6")
                native_refs = self._persist_initial_native_lineage(
                    task_id, package_ref, compiled, ordinary_reviews, initial_e6, worker_assignment_id
                )
                previous_ref = copy.deepcopy(package_ref)
                published = {}
                for stage in ("E1", "E2", "E3", "E4", "E5", "E6-initial"):
                    record = self._record(
                        stage, compiled[stage], frontier, frozen_head,
                        previous_ref=previous_ref, native_refs=native_refs.get(stage), lineage_refs=published,
                    )
                    artifact_id = "execution-E6-initial" if stage == "E6-initial" else "runtime-" + stage
                    previous_ref = self._publish(artifact_id, record, "runtime-skill")
                    published[stage] = copy.deepcopy(previous_ref)
                self.runtime._refresh()
                finding_ids = sorted({
                    finding_id
                    for stage in ("E4", "E5")
                    for finding_id in native_refs[stage]["finding_ids"]
                })
                route = "repair_required" if len(finding_ids) == 1 else "replan_required"
                return {
                    "status": route,
                    "next_operation": "begin" if route == "repair_required" else "replan",
                    "task_id": task_id,
                    "finding_id": finding_ids[0] if len(finding_ids) == 1 else None,
                    "finding_ids": finding_ids,
                    "initial_e6_ref": copy.deepcopy(published["E6-initial"]),
                    "execution_frontier_digest": frontier["frontier_digest"],
                    "head": _head(self.kernel),
                    "stages": ["E1", "E2", "E3", "E4", "E5", "E6-initial"],
                    "native_review_refs": copy.deepcopy(native_refs),
                    "lineage_refs": published,
                }
            elif initial_e6.get("next") != "E8":
                raise RuntimeExecutionError("E6 did not produce a finite accepting or repair route")
            elif "E7" in stage_inputs:
                raise RuntimeExecutionError("E7 input is valid only for one validated required Finding")

            v2_join = self._complete_v2_join(e8_base, final_reviews, final_dispositions, final_budget)
            compiled["E8"] = self._compile(compiler, "E8", {**common, "v2_join": v2_join, "sibling_refs": self._sibling_refs(v2_join), "complete": True, "conflicting": False, "open_required": False}, authority, frozen_head)
            e9_supplied = copy.deepcopy(repair["e9"] if repair is not None else dict(stage_inputs["E9"]))
            if e9_supplied.get("candidate_digest") != candidate_digest or e9_supplied.get("closure_digest") != closure["closure_digest"]:
                raise RuntimeExecutionError("E9 candidate or closure differs from the actual E2/E3 lineage")
            compiled["E9"] = self._compile(compiler, "E9", {**common, **e9_supplied}, authority, frozen_head)

            # Only after every join passes may the parent accept the Work Product.
            self.kernel.accept_task_result(task_id, {
                "status": "success", "changed_paths": sorted(changed_paths), "terminal_receipt_digest": executed["terminal_receipt"]["receipt_digest"],
                "execution_candidate_digest": candidate_digest, "e3_artifact_digest": artifact_candidate["artifact_digest"], "e8_finalization_digest": compiled["E8"]["finalization"]["finalization_digest"],
            }, worker_assignment_id=worker_assignment_id, authority_ref=task_authority, idempotency_key="runtime-e-result-" + task_id)
            accepted = True
            native_refs = self._persist_native_lineage(task_id, package_ref, compiled, ordinary_reviews, initial_e6, worker_assignment_id, repair)
            previous_ref = copy.deepcopy(package_ref)
            stage_order = ["E1", "E2", "E3", "E4", "E5", "E6-initial", "E7", "E6", "E8", "E9"]
            published = {}
            for stage in stage_order:
                if stage not in compiled or (stage == "E6-initial" and "E7" not in compiled):
                    continue
                record = self._record(stage, compiled[stage], frontier, frozen_head, previous_ref=previous_ref, native_refs=native_refs.get(stage), lineage_refs=published)
                artifact_id = "execution-E6-initial" if stage == "E6-initial" else "runtime-" + stage
                previous_ref = self._publish(artifact_id, record, "runtime-skill")
                published[stage] = copy.deepcopy(previous_ref)
            self.runtime._refresh()
            return {**self.runtime.status(), "task_id": task_id, "task_result_ref": copy.deepcopy(self.runtime.state["tasks"][task_id]["result_ref"]), "execution_frontier_digest": frontier["frontier_digest"], "stages": [stage for stage in stage_order if stage in compiled], "native_review_refs": copy.deepcopy(native_refs), "lineage_refs": published}
        except Exception as error:
            if not accepted:
                self._terminalize_live_attempt(task_id, worker_assignment_id, task_authority, str(error))
            else:
                self._terminalize_accepted_attempt(task_id, task_authority, str(error))
            if isinstance(error, RuntimeExecutionError):
                raise
            raise RuntimeExecutionError("persisted Group E execution refused: " + str(error)) from error

    def _prevalidate_execution_lineage(self, package, stages, *, require_e9=True):
        try:
            closure = ExecutionClosureBuilder().freeze(package)
            e8 = stages["E8"]
            if set(e8) != {"candidate", "plan", "receipts"}:
                raise RuntimeExecutionError("E8 accepts only candidate, plan, and receipt evidence; aggregate truth is derived")
            candidate, plan, receipts = (copy.deepcopy(e8[key]) for key in ("candidate", "plan", "receipts"))
            if candidate.get("candidate_digest") != closure["candidate_ref"]["digest"] or candidate.get("execution_closure_digest") != closure["closure_digest"]:
                raise RuntimeExecutionError("E8 candidate differs from actual E2 candidate or closure")
            if plan.get("execution_closure") != closure or plan.get("execution_closure_digest") != closure["closure_digest"]:
                raise RuntimeExecutionError("E8 plan closure differs from actual E2 closure")
            aggregate = ReceiptAggregator().aggregate(candidate, plan, receipts)
            if not require_e9:
                return closure, {"candidate": candidate, "plan": plan, "receipts": receipts}, aggregate
            e9 = stages["E9"]
            if set(e9) - {"candidate_digest", "closure_digest", "verification_receipts", "complete", "open_required", "budget_exhausted"}:
                raise RuntimeExecutionError("E9 contains caller-defined lifecycle fields")
            if e9.get("candidate_digest") != closure["candidate_ref"]["digest"] or e9.get("closure_digest") != closure["closure_digest"]:
                raise RuntimeExecutionError("E9 identity differs from actual candidate or closure")
            verification_receipts = e9.get("verification_receipts")
            if not isinstance(verification_receipts, Mapping) or set(verification_receipts) != {"integration", "e2e", "regression", "objective"}:
                raise RuntimeExecutionError("E9 verification receipt inventory is incomplete")
            for category, receipt in verification_receipts.items():
                if (not isinstance(receipt, Mapping) or receipt.get("category") != category
                        or receipt.get("candidate_digest") != closure["candidate_ref"]["digest"]
                        or receipt.get("closure_digest") != closure["closure_digest"]
                        or _digest({key: copy.deepcopy(value) for key, value in receipt.items() if key != "receipt_digest"}) != receipt.get("receipt_digest")):
                    raise RuntimeExecutionError("E9 verification receipt differs from actual candidate or closure")
            return closure, {"candidate": candidate, "plan": plan, "receipts": receipts}, aggregate
        except V2ContractError as error:
            raise RuntimeExecutionError("E8 lineage is invalid: " + str(error)) from error

    @staticmethod
    def _normalize_detailed_findings(findings):
        if not isinstance(findings, list):
            raise RuntimeExecutionError("review findings must be arrays")
        normalized = []
        for supplied in findings:
            if not isinstance(supplied, Mapping):
                raise RuntimeExecutionError("review finding is malformed")
            finding = copy.deepcopy(dict(supplied))
            required = {"finding_id", "fingerprint", "background", "as_is", "to_be", "gap", "requirement_refs", "evidence_refs", "severity", "blocking_proposal", "owner_proposal"}
            if set(finding) != required:
                raise RuntimeExecutionError("review finding lacks the exact detailed contract")
            requirement = finding["requirement_refs"][0] if isinstance(finding.get("requirement_refs"), list) and finding["requirement_refs"] else None
            fingerprint = _digest({"requirement_ref": requirement, "description": finding.get("gap"), "severity": finding.get("severity")})
            if finding.get("fingerprint") not in {fingerprint, "derive"}:
                raise RuntimeExecutionError("review finding fingerprint differs from its stable native fields")
            finding["fingerprint"] = fingerprint
            normalized.append(finding)
        return normalized

    @staticmethod
    def _prevalidate_dispositions(findings, e6):
        if set(e6) != {"dispositions", "observed_budget"}:
            raise RuntimeExecutionError("E6 accepts only dispositions and observed budget; review reports are derived")
        expected = {item["fingerprint"] for stage in ("E4", "E5") for item in findings[stage]}
        dispositions = e6.get("dispositions")
        supplied = [item.get("fingerprint") for item in dispositions if isinstance(item, Mapping)] if isinstance(dispositions, list) else []
        if not isinstance(dispositions, list) or len(supplied) != len(dispositions) or len(supplied) != len(set(supplied)) or set(supplied) != expected:
            raise RuntimeExecutionError("E6 dispositions do not cover the exact E4/E5 finding roots")

    def _derive_readiness(self, state, grant, task_id, task, lease, d_refs, closure, authority):
        checks = {key: False for key in ("authority", "expected_head", "lease", "dependencies", "conflicts", "blockers", "purpose", "budget", "workspace", "git")}
        self.kernel._validate_live_task_authority(state, task, operation="claim_task", current_head=_head(self.kernel))
        checks["authority"] = True
        checks["expected_head"] = authority["expected_head"] == _head(self.kernel)
        checks["lease"] = lease.get("assignment_id") == task["assignment"]["assignment_id"] and lease.get("status") == "leased"
        task_node = "task:" + task_id
        unmet = [edge for edge in state.get("edges", []) if edge.get("to") == task_node and edge.get("type") == "requires" and edge.get("from") not in state.get("nodes", {})]
        checks["dependencies"] = not unmet
        live_scopes = [set(item.get("write_scope", [])) for other_id, item in state.get("leases", {}).items() if other_id != task_id and item.get("status") == "leased"]
        checks["conflicts"] = not any(set(lease.get("write_scope", [])) & scope for scope in live_scopes)
        open_blockers = sorted(key for key, item in state.get("findings", {}).items() if item.get("blocking") and self.kernel._finding_blocks(item))
        checks["blockers"] = not open_blockers and state.get("budget_terminal") is None
        checks["purpose"] = grant.get("objective_digest") == state.get("objective_ref", {}).get("digest")
        deadline = datetime.fromisoformat(state["review_budget"]["deadline"].replace("Z", "+00:00"))
        checks["budget"] = datetime.now(timezone.utc) < deadline
        workspace = self.runtime.project
        checks["workspace"] = workspace.is_dir() and closure.get("workspace_identity") == str(workspace)
        git_marker = workspace / ".git"
        git_observation = {"classification": "worktree" if git_marker.is_file() else "repository" if git_marker.is_dir() else "not-a-git-repository", "marker_path": str(git_marker), "marker_exists": git_marker.exists()}
        # Git is observed, not a hidden prerequisite. This route performs no
        # Git mutation, and a non-Git fixture is therefore a valid observation.
        checks["git"] = git_observation["classification"] in {"worktree", "repository", "not-a-git-repository"}
        receipt = {
            "schema": "runtime-e-readiness-receipt/v1", "checks": checks, "open_blockers": open_blockers,
            "resource_conflict": not checks["conflicts"], "terminal": state.get("budget_terminal") is not None,
            "d_bundle_ref": copy.deepcopy(grant["bundle_ref"]), "d_artifact_refs": copy.deepcopy(d_refs), "task_package_ref": copy.deepcopy(task["package_ref"]),
            "grant_object_digest": task["authority_digest"], "lease_digest": _digest(lease), "budget_digest": _digest(state["review_budget"]),
            "workspace": {"path": str(workspace), "is_directory": workspace.is_dir()}, "git": git_observation,
            "dependency_evidence": {"unmet": unmet}, "conflict_evidence": {"other_live_scopes": [sorted(scope) for scope in live_scopes]},
        }
        receipt["receipt_digest"] = _digest(receipt)
        if not all(checks.values()) or open_blockers or receipt["resource_conflict"] or receipt["terminal"]:
            raise RuntimeExecutionError("current Kernel readiness observations do not admit E1")
        return receipt

    @staticmethod
    def _observed_budget(e6, state):
        supplied = e6.get("observed_budget")
        if not isinstance(supplied, Mapping) or set(supplied) != {"remaining_seconds", "review_round", "product_fix_attempt"}:
            raise RuntimeExecutionError("E6 observed budget is malformed")
        deadline = datetime.fromisoformat(state["review_budget"]["deadline"].replace("Z", "+00:00"))
        remaining = max(0, int((deadline - datetime.now(timezone.utc)).total_seconds()))
        actual = {"remaining_seconds": remaining, "review_round": state["review_budget"]["rounds_used"], "product_fix_attempt": max(state["review_budget"].get("finding_attempts", {}).values(), default=0)}
        if supplied.get("review_round") != actual["review_round"] or supplied.get("product_fix_attempt") != actual["product_fix_attempt"] or not isinstance(supplied.get("remaining_seconds"), int) or supplied["remaining_seconds"] > remaining:
            raise RuntimeExecutionError("E6 observed budget differs from current Kernel budget")
        actual["remaining_seconds"] = supplied["remaining_seconds"]
        return actual

    @staticmethod
    def _ordinary_reviews(compiled, candidate_digest, aggregate_digest):
        reviews = []
        for stage in ("E4", "E5"):
            report = compiled[stage]["report"]
            reviews.append({
                "schema": "ordinary-review-report/v1", "report_id": "runtime-" + stage.lower() + "-report", "axis": report["axis"],
                "actor_id": report["actor_id"], "context_epoch_id": report["reviewer_epoch_id"], "package_digest": compiled[stage]["report_digest"],
                "candidate_digest": candidate_digest, "aggregate_digest": aggregate_digest,
                "findings": [{"finding_id": item["finding_id"], "fingerprint": item["fingerprint"], "severity": item["severity"], "summary": item["gap"]} for item in report["findings"]],
            })
        return reviews

    def _compile_e7(self, compiler, common, authority, head, e6, supplied):
        advice = e6["validation"]
        if len([item for item in advice["dispositions"] if item["classification"] == "required"]) != 1:
            raise RuntimeExecutionError("runtime E7 supports exactly one bounded required root per invocation")
        first = _event(0, None, "review-round-opened", {"candidate_digest": advice["candidate_digest"], "review_round": 1})
        second = _event(1, first["event_digest"], "advisory-accepted", {"candidate_digest": advice["candidate_digest"], "disposition_digest": advice["disposition_digest"]})
        allowances = supplied.get("allowances", {key: 1 for key in ("command_timeout_seconds", "grace_seconds", "terminal_publication_seconds", "affected_regression_seconds", "round_two_reviews_seconds", "validator_seconds", "parent_seconds")})
        command = {
            "schema": "orchestrator-command/v2", "command_id": "runtime-e7-command", "operation": "evaluate-advice",
            "actor": {"role": "orchestrator", "assignment_id": authority["assignment_id"]}, "expected_head": copy.deepcopy(head),
            "contract_version": "workflow-execution/v2", "lease_id": authority["lease_id"], "source_ref": {"id": "runtime-e6-advice", "digest": advice["disposition_digest"]},
            "remaining_seconds": authority["budget"]["seconds"], "allowances": copy.deepcopy(allowances), "replacement_budget": None,
        }
        observed = {
            "current_head": copy.deepcopy(head),
            "active_lease": {"schema": "orchestrator-lease/v2", "lease_id": authority["lease_id"], "status": "active", "holder_assignment_id": authority["assignment_id"], "contract_version": "workflow-execution/v2", "expected_head": copy.deepcopy(head), "unaccepted_dispatch_ids": []},
            "advisory_disposition": copy.deepcopy(advice), "immutable_history": [first, second], "prior_terminal": None,
        }
        return self._compile(compiler, "E7", {**common, "orchestrator_validated": True, "advice": "required", "orchestrator_command": command, "observed_state": observed}, authority, head)

    @staticmethod
    def _strict_keys(value, keys, label):
        if not isinstance(value, Mapping) or set(value) != set(keys):
            raise RuntimeExecutionError(label + " has fields outside its closed contract")
        return copy.deepcopy(dict(value))

    def _project_file(self, relative, label):
        if not isinstance(relative, str) or not relative or relative.startswith("/"):
            raise RuntimeExecutionError(label + " path must be project-relative")
        parts = Path(relative).parts
        if not parts or any(part in {"", ".", ".."} for part in parts):
            raise RuntimeExecutionError(label + " path is not canonical")
        current = self.runtime.project
        for part in parts:
            current = current / part
            if current.is_symlink():
                raise RuntimeExecutionError(label + " path traverses a symlink")
        try:
            resolved = current.resolve(strict=True)
        except OSError as error:
            raise RuntimeExecutionError(label + " physical file is unavailable") from error
        try:
            resolved.relative_to(self.runtime.project.resolve(strict=True))
        except ValueError as error:
            raise RuntimeExecutionError(label + " escapes the selected project") from error
        if not resolved.is_file():
            raise RuntimeExecutionError(label + " is not a regular file")
        return resolved

    def _physical_json(self, supplied, label):
        ref = self._strict_keys(supplied, {"path", "digest"}, label + " reference")
        path = self._project_file(ref["path"], label)
        raw = path.read_bytes()
        actual = "sha256:" + hashlib.sha256(raw).hexdigest()
        if ref["digest"] != actual:
            raise RuntimeExecutionError(label + " digest differs from physical bytes")
        try:
            value = json.loads(raw)
        except (UnicodeDecodeError, json.JSONDecodeError) as error:
            raise RuntimeExecutionError(label + " is not a physical JSON document") from error
        if not isinstance(value, Mapping):
            raise RuntimeExecutionError(label + " document must be an object")
        return ref, copy.deepcopy(dict(value))

    def _snapshot_artifacts(self, scope):
        if not isinstance(scope, list) or not scope:
            raise RuntimeExecutionError("required E7 permitted scope must be non-empty")
        result = {}
        for relative in sorted(scope):
            path = self._project_file(relative, "required fix scope")
            result[relative] = "sha256:" + hashlib.sha256(path.read_bytes()).hexdigest()
        return result

    def _verify_e7_repair(self, e7, initial_e6, ordinary_reviews, worker, prior_candidate_digest, prior_closure, base, pre_fix):
        allowed = {"post_fix_candidate_ref", "worker_result_ref", "focused_receipt_refs", "fresh_review_refs", "verification_receipt_refs", "allowances"}
        if not isinstance(e7, Mapping) or set(e7) - allowed or set(e7) - {"allowances"} != allowed - {"allowances"}:
            raise RuntimeExecutionError("E7 accepts only a complete physical repair and fresh-rereview bundle")
        required = [item for item in initial_e6["validation"]["dispositions"] if item["classification"] == "required"]
        finding_ids = [finding["finding_id"] for review in ordinary_reviews for finding in review["findings"]]
        if len(required) != 1 or len(finding_ids) != 1 or pre_fix is None:
            raise RuntimeExecutionError("E7 requires exactly one validated required Finding")
        finding_id = finding_ids[0]
        scope = sorted(required[0]["proposed_scope"])

        candidate_ref, candidate_doc = self._physical_json(e7["post_fix_candidate_ref"], "post-fix candidate")
        candidate_doc = self._strict_keys(candidate_doc, {"schema", "finding_id", "candidate_id", "prior_candidate_digest", "permitted_fix_scope", "artifacts"}, "post-fix candidate")
        if (candidate_doc["schema"] != "runtime-e7-post-fix-candidate/v1" or candidate_doc["finding_id"] != finding_id
                or candidate_doc["prior_candidate_digest"] != prior_candidate_digest or sorted(candidate_doc["permitted_fix_scope"]) != scope
                or candidate_ref["digest"] == prior_candidate_digest or not isinstance(candidate_doc["candidate_id"], str) or not candidate_doc["candidate_id"]):
            raise RuntimeExecutionError("post-fix candidate does not bind the required Finding, prior candidate, and permitted scope")
        artifacts = candidate_doc["artifacts"]
        if not isinstance(artifacts, list) or not artifacts:
            raise RuntimeExecutionError("post-fix candidate requires physical changed artifacts")
        normalized_artifacts = {}
        for index, artifact in enumerate(artifacts):
            artifact = self._strict_keys(artifact, {"path", "digest"}, "post-fix artifact[%d]" % index)
            path = self._project_file(artifact["path"], "post-fix artifact[%d]" % index)
            actual = "sha256:" + hashlib.sha256(path.read_bytes()).hexdigest()
            if artifact["digest"] != actual:
                raise RuntimeExecutionError("post-fix artifact digest differs from physical bytes")
            normalized_artifacts[artifact["path"]] = actual
        if sorted(normalized_artifacts) != scope or any(pre_fix.get(path) == digest for path, digest in normalized_artifacts.items()):
            raise RuntimeExecutionError("post-fix candidate is unchanged or escapes the permitted fix scope")

        worker_ref, worker_doc = self._physical_json(e7["worker_result_ref"], "E7 worker result")
        worker_doc = self._strict_keys(worker_doc, {"schema", "finding_id", "worker_assignment_id", "post_fix_candidate_digest", "permitted_fix_scope", "changed_paths", "status"}, "E7 worker result")
        if (worker_doc["schema"] != "runtime-e7-worker-result/v1" or worker_doc["finding_id"] != finding_id
                or worker_doc["worker_assignment_id"] != worker or worker_doc["post_fix_candidate_digest"] != candidate_ref["digest"]
                or sorted(worker_doc["permitted_fix_scope"]) != scope or sorted(worker_doc["changed_paths"]) != scope or worker_doc["status"] != "passed"):
            raise RuntimeExecutionError("E7 worker result does not bind the physical post-fix candidate")

        package = {key: copy.deepcopy(value) for key, value in prior_closure.items() if key != "closure_digest"}
        package["schema"] = "execution-package-input/v2"
        package["candidate_ref"] = {"id": candidate_doc["candidate_id"], "digest": candidate_ref["digest"]}
        post_closure = ExecutionClosureBuilder().freeze(package)
        post_candidate = {
            "schema": "artifact-candidate/v1", "candidate_id": candidate_doc["candidate_id"], "candidate_digest": candidate_ref["digest"],
            "execution_closure_digest": post_closure["closure_digest"], "regression_inventory": copy.deepcopy(base["candidate"]["regression_inventory"]), "frozen": True,
        }
        shard_inputs = [{key: copy.deepcopy(shard[key]) for key in ("shard_id", "members", "command", "resource_claims", "isolation")} for shard in base["plan"]["shards"]]
        post_plan = RegressionFrontier().plan(post_candidate, post_closure, shard_inputs)

        focused_refs = e7["focused_receipt_refs"]
        if not isinstance(focused_refs, list) or len(focused_refs) != len(post_plan["shards"]):
            raise RuntimeExecutionError("E7 focused receipts must exactly cover the post-fix regression plan")
        physical_focused_refs, receipts = [], []
        shards = {item["shard_id"]: item for item in post_plan["shards"]}
        for index, supplied in enumerate(focused_refs):
            physical_ref, document = self._physical_json(supplied, "focused receipt[%d]" % index)
            document = self._strict_keys(document, {"schema", "finding_id", "post_fix_candidate_digest", "worker_result_digest", "receipt"}, "focused receipt[%d]" % index)
            if (document["schema"] != "runtime-e7-focused-regression/v1" or document["finding_id"] != finding_id
                    or document["post_fix_candidate_digest"] != candidate_ref["digest"] or document["worker_result_digest"] != worker_ref["digest"]):
                raise RuntimeExecutionError("focused receipt does not bind the repaired Finding and Worker result")
            receipt = document["receipt"]
            shard = shards.get(receipt.get("shard_id")) if isinstance(receipt, Mapping) else None
            if shard is None or receipt.get("candidate_digest") != candidate_ref["digest"] or receipt.get("execution_closure_digest") != shard["execution_closure_ref"]["digest"]:
                raise RuntimeExecutionError("focused receipt does not bind the post-fix closure")
            physical_focused_refs.append(physical_ref)
            receipts.append(copy.deepcopy(receipt))
        aggregate = ReceiptAggregator().aggregate(post_candidate, post_plan, receipts)
        if not aggregate["accepted"]:
            raise RuntimeExecutionError("post-fix focused regression did not pass")

        review_refs = e7["fresh_review_refs"]
        if not isinstance(review_refs, list) or len(review_refs) != 2:
            raise RuntimeExecutionError("E7 requires two physical fresh review reports")
        reviews, physical_review_refs = [], []
        old_actors = {worker, "runtime-e-validator", *(item["actor_id"] for item in ordinary_reviews)}
        expected_focus = sorted(item["digest"] for item in physical_focused_refs)
        for index, supplied in enumerate(review_refs):
            physical_ref, report = self._physical_json(supplied, "fresh review[%d]" % index)
            report = self._strict_keys(report, {"schema", "finding_id", "axis", "actor_id", "reviewer_epoch_id", "post_fix_candidate_digest", "worker_result_digest", "focused_receipt_digests", "verdict", "findings"}, "fresh review[%d]" % index)
            if (report["schema"] != "runtime-e7-fresh-review/v1" or report["finding_id"] != finding_id
                    or report["post_fix_candidate_digest"] != candidate_ref["digest"] or report["worker_result_digest"] != worker_ref["digest"]
                    or sorted(report["focused_receipt_digests"]) != expected_focus or report["verdict"] != "pass" or report["findings"] != []
                    or report["actor_id"] in old_actors or not isinstance(report["reviewer_epoch_id"], str) or not report["reviewer_epoch_id"]):
                raise RuntimeExecutionError("fresh review does not bind exact physical repair evidence")
            reviews.append({
                "schema": "ordinary-review-report/v1", "report_id": "runtime-e7-rereview-" + report["axis"], "axis": report["axis"],
                "actor_id": report["actor_id"], "context_epoch_id": report["reviewer_epoch_id"], "package_digest": physical_ref["digest"],
                "candidate_digest": candidate_ref["digest"], "aggregate_digest": aggregate["aggregate_digest"], "findings": [],
            })
            physical_review_refs.append(physical_ref)
        if ({item["axis"] for item in reviews} != {"architecture-safety", "integration-operability"}
                or len({item["actor_id"] for item in reviews}) != 2 or len({item["context_epoch_id"] for item in reviews}) != 2):
            raise RuntimeExecutionError("fresh rereview requires two distinct actors and Epochs")

        verification_refs = e7["verification_receipt_refs"]
        if not isinstance(verification_refs, Mapping) or set(verification_refs) != {"integration", "e2e", "regression", "objective"}:
            raise RuntimeExecutionError("E7 post-fix verification inventory is incomplete")
        verification = {}
        physical_verification_refs = {}
        for category, supplied in verification_refs.items():
            physical_ref, receipt = self._physical_json(supplied, "post-fix %s verification" % category)
            expected_keys = {"schema", "category", "candidate_digest", "closure_digest", "command", "environment", "capture", "terminal", "status", "receipt_digest"}
            receipt = self._strict_keys(receipt, expected_keys, "post-fix %s verification" % category)
            unsigned = {key: copy.deepcopy(value) for key, value in receipt.items() if key != "receipt_digest"}
            if (receipt["schema"] != "whole-verification-receipt/v1" or receipt["category"] != category
                    or receipt["candidate_digest"] != candidate_ref["digest"] or receipt["closure_digest"] != post_closure["closure_digest"]
                    or receipt["terminal"] is not True or receipt["status"] != "passed" or receipt["receipt_digest"] != _digest(unsigned)):
                raise RuntimeExecutionError("post-fix verification receipt is stale or fabricated")
            verification[category] = receipt
            physical_verification_refs[category] = physical_ref

        return {
            "finding_id": finding_id, "candidate": post_candidate, "candidate_ref": candidate_ref, "candidate_document": candidate_doc,
            "worker_result_ref": worker_ref, "focused_receipt_refs": physical_focused_refs, "fresh_review_refs": physical_review_refs,
            "verification_receipt_refs": physical_verification_refs, "closure": post_closure,
            "e8_base": {"candidate": post_candidate, "plan": post_plan, "receipts": receipts}, "aggregate": aggregate, "reviews": reviews,
            "e9": {"candidate_digest": candidate_ref["digest"], "closure_digest": post_closure["closure_digest"], "verification_receipts": verification, "complete": True, "open_required": False},
        }

    @staticmethod
    def _complete_v2_join(base, reviews, dispositions, budget):
        aggregate = ReceiptAggregator().aggregate(base["candidate"], base["plan"], base["receipts"])
        validation = FindingValidator().validate(reviews, dispositions, budget)
        branches = [
            {"branch_id": "regression", "state": "accepted", "complete": True, "terminal_ref": {"id": "aggregate", "digest": aggregate["aggregate_digest"]}},
            {"branch_id": "review", "state": "accepted", "complete": True, "terminal_ref": {"id": "disposition", "digest": validation["disposition_digest"]}},
        ]
        inventory = {"schema": "declared-branch-inventory/v1", "inventory_id": "runtime-e-branches", "candidate_ref": {"id": base["candidate"]["candidate_id"], "digest": base["candidate"]["candidate_digest"]}, "branches": [{"branch_id": item["branch_id"], "terminal_ref": item["terminal_ref"]} for item in branches]}
        inventory["inventory_digest"] = _digest(inventory)
        return {**copy.deepcopy(base), "reviews": copy.deepcopy(reviews), "dispositions": copy.deepcopy(dispositions), "observed_budget": copy.deepcopy(budget), "inventory_ref": {"id": inventory["inventory_id"], "digest": inventory["inventory_digest"]}, "inventory": inventory, "branches": branches}

    @staticmethod
    def _sibling_refs(v2_join):
        joined = ExecutionGroupV1.v2_join(v2_join)
        return [{"id": item["terminal_ref"]["id"], "digest": item["terminal_ref"]["digest"], "status": "accepted"} for item in joined["finalization"]["branches"]]

    def _persist_initial_native_lineage(self, task_id, package_ref, compiled, ordinary_reviews, initial_e6, worker):
        """Persist only the review/validation facts known at the E6 frontier.

        This deliberately excludes a resolution claim, closure review, Task
        result, and final E6-E9 records.  Those facts can only be produced by a
        post-Finding ``RuntimeRepairCoordinator`` attempt.
        """
        authority = self._kernel_authority(
            ["publish_artifact", "open_review_epoch", "open_review", "validate_findings"], []
        )
        native: dict[str, Any] = {}
        dispositions = {item["fingerprint"]: item for item in initial_e6["validation"]["dispositions"]}
        for stage, ordinary in zip(("E4", "E5"), ordinary_reviews):
            report_value = {
                "schema": "runtime-native-review-evidence/v1",
                "compiled_report": copy.deepcopy(compiled[stage]["report"]),
                "compiled_report_digest": compiled[stage]["report_digest"],
                "ordinary_report": copy.deepcopy(ordinary),
            }
            report_ref = self._publish("runtime-" + stage + "-report", report_value, "review-evidence")
            reviewer = ordinary["actor_id"]
            if reviewer == worker:
                raise RuntimeExecutionError("native reviewer must be independent from the Task Worker")
            review_epoch, review_id = "runtime-" + stage.lower() + "-review", "runtime-" + stage.lower()
            self.kernel.open_review_epoch(
                review_epoch, package_ref, reviewer_assignment_id=reviewer,
                authority_ref=authority, idempotency_key="open-" + review_epoch,
            )
            native_findings = []
            for detailed in compiled[stage]["report"]["findings"]:
                native_findings.append({
                    "finding_id": detailed["finding_id"],
                    "fingerprint": detailed["fingerprint"],
                    "requirement_ref": detailed["requirement_refs"][0],
                    "description": detailed["gap"],
                    "evidence": [report_ref],
                    "severity": detailed["severity"],
                    "blocking": detailed["blocking_proposal"] == "blocking",
                })
            self.kernel.open_review(
                review_id, task_id, native_findings, reviewer_assignment_id=reviewer,
                fresh_epoch_id=review_epoch, authority_ref=authority,
                idempotency_key="open-" + review_id,
            )
            validator, validator_epoch = "runtime-e-validator", "runtime-" + stage.lower() + "-validator"
            self.kernel.open_review_epoch(
                validator_epoch, package_ref, reviewer_assignment_id=validator,
                authority_ref=authority, idempotency_key="open-" + validator_epoch,
            )
            outcomes = []
            for finding in native_findings:
                disposition = dispositions[finding["fingerprint"]]
                classification = disposition["classification"]
                outcomes.append({
                    "candidate_id": finding["finding_id"],
                    "disposition": classification if classification in {"required", "needs-user", "duplicate"} else "defer",
                    "reason": "runtime E6 exact disposition: " + classification,
                    "materiality": disposition["materiality"],
                    "requirement_ref": finding["requirement_ref"],
                    "permitted_fix_scope": disposition["proposed_scope"],
                })
            self.kernel.validate_findings(
                review_id, outcomes, validator_assignment_id=validator,
                fresh_epoch_id=validator_epoch, authority_ref=authority,
                idempotency_key="validate-" + review_id,
            )
            current = self.kernel.read_state()
            native[stage] = {
                "report_evidence_ref": report_ref,
                "review_package_ref": copy.deepcopy(current["reviews"][review_id]["package_ref"]),
                "validation_ref": copy.deepcopy(current["finding_validations"][review_id]["validation_ref"]),
                "reviewer_assignment_id": reviewer,
                "validator_assignment_id": validator,
                "finding_ids": [item["finding_id"] for item in native_findings],
            }
        native["E6-initial"] = {
            "joined_validation_refs": [native["E4"]["validation_ref"], native["E5"]["validation_ref"]],
            "validation_digest": initial_e6["validation"]["disposition_digest"],
        }
        return native

    def _persist_native_lineage(self, task_id, package_ref, compiled, ordinary_reviews, initial_e6, worker, repair):
        authority = self._kernel_authority(["publish_artifact", "open_review_epoch", "open_review", "validate_findings", "accept_resolution_claim", "accept_finding_closure"], [])
        native: dict[str, Any] = {}
        dispositions = {item["fingerprint"]: item for item in initial_e6["validation"]["dispositions"]}
        for stage, ordinary in zip(("E4", "E5"), ordinary_reviews):
            report_value = {"schema": "runtime-native-review-evidence/v1", "compiled_report": copy.deepcopy(compiled[stage]["report"]), "compiled_report_digest": compiled[stage]["report_digest"], "ordinary_report": copy.deepcopy(ordinary)}
            report_ref = self._publish("runtime-" + stage + "-report", report_value, "review-evidence")
            reviewer = ordinary["actor_id"]
            if reviewer == worker:
                raise RuntimeExecutionError("native reviewer must be independent from the Task Worker")
            review_epoch, review_id = "runtime-" + stage.lower() + "-review", "runtime-" + stage.lower()
            self.kernel.open_review_epoch(review_epoch, package_ref, reviewer_assignment_id=reviewer, authority_ref=authority, idempotency_key="open-" + review_epoch)
            native_findings = []
            for detailed in compiled[stage]["report"]["findings"]:
                native_findings.append({
                    "finding_id": detailed["finding_id"], "fingerprint": detailed["fingerprint"], "requirement_ref": detailed["requirement_refs"][0],
                    "description": detailed["gap"], "evidence": [report_ref], "severity": detailed["severity"],
                    "blocking": detailed["blocking_proposal"] == "blocking",
                })
            self.kernel.open_review(review_id, task_id, native_findings, reviewer_assignment_id=reviewer, fresh_epoch_id=review_epoch, authority_ref=authority, idempotency_key="open-" + review_id)
            validator, validator_epoch = "runtime-e-validator", "runtime-" + stage.lower() + "-validator"
            self.kernel.open_review_epoch(validator_epoch, package_ref, reviewer_assignment_id=validator, authority_ref=authority, idempotency_key="open-" + validator_epoch)
            outcomes = []
            for finding in native_findings:
                disposition = dispositions[finding["fingerprint"]]
                classification = disposition["classification"]
                outcomes.append({
                    "candidate_id": finding["finding_id"], "disposition": classification if classification in {"required", "needs-user", "duplicate"} else "defer",
                    "reason": "runtime E6 exact disposition: " + classification, "materiality": disposition["materiality"],
                    "requirement_ref": finding["requirement_ref"], "permitted_fix_scope": disposition["proposed_scope"],
                })
            self.kernel.validate_findings(review_id, outcomes, validator_assignment_id=validator, fresh_epoch_id=validator_epoch, authority_ref=authority, idempotency_key="validate-" + review_id)
            current = self.kernel.read_state()
            native[stage] = {"report_evidence_ref": report_ref, "review_package_ref": copy.deepcopy(current["reviews"][review_id]["package_ref"]), "validation_ref": copy.deepcopy(current["finding_validations"][review_id]["validation_ref"]), "reviewer_assignment_id": reviewer, "validator_assignment_id": validator, "finding_ids": [item["finding_id"] for item in native_findings]}
        native["E6-initial" if "E7" in compiled else "E6"] = {"joined_validation_refs": [native["E4"]["validation_ref"], native["E5"]["validation_ref"]], "validation_digest": initial_e6["validation"]["disposition_digest"]}
        if "E7" in compiled:
            finding_ids = native["E4"]["finding_ids"] + native["E5"]["finding_ids"]
            if len(finding_ids) != 1 or not isinstance(repair, Mapping) or repair.get("finding_id") != finding_ids[0]:
                raise RuntimeExecutionError("native E7 requires exactly one admitted Finding")
            finding_id = finding_ids[0]
            resolution_evidence = [copy.deepcopy(repair["candidate_ref"]), copy.deepcopy(repair["worker_result_ref"]), *copy.deepcopy(repair["focused_receipt_refs"])]
            self.kernel.accept_resolution_claim(finding_id, resolution_evidence, worker_assignment_id=worker, authority_ref=authority, idempotency_key="resolve-" + finding_id)
            state = self.kernel.read_state()
            resolution_ref = copy.deepcopy(state["findings"][finding_id]["resolution_ref"])
            physical_review_artifacts = []
            affected_axis = next(item["axis"] for item in ordinary_reviews if item["findings"])
            closure_review = next(item for item in repair["reviews"] if item["axis"] == affected_axis)
            for index, physical_ref in enumerate(repair["fresh_review_refs"]):
                _, physical_report = self._physical_json(physical_ref, "persisted fresh review[%d]" % index)
                physical_review_artifacts.append(self._publish("runtime-e7-physical-review-%d" % index, {
                    "schema": "runtime-native-fresh-review-evidence/v1", "physical_ref": copy.deepcopy(physical_ref),
                    "physical_report": physical_report, "post_fix_candidate_ref": copy.deepcopy(repair["candidate_ref"]),
                    "worker_result_ref": copy.deepcopy(repair["worker_result_ref"]), "focused_receipt_refs": copy.deepcopy(repair["focused_receipt_refs"]),
                }, "review-evidence"))
            reviewer, epoch, review_id = closure_review["actor_id"], closure_review["context_epoch_id"], "runtime-e7-rereview"
            self.kernel.open_review_epoch(epoch, resolution_ref, reviewer_assignment_id=reviewer, authority_ref=authority, idempotency_key="open-" + epoch)
            self.kernel.open_review(review_id, task_id, [], reviewer_assignment_id=reviewer, fresh_epoch_id=epoch, review_kind="closure", target_finding_id=finding_id, authority_ref=authority, idempotency_key="open-" + review_id)
            self.kernel.accept_finding_closure(finding_id, [resolution_ref], reviewer_assignment_id=reviewer, fresh_epoch_id=epoch, review_id=review_id, authority_ref=authority, idempotency_key="close-" + finding_id)
            state = self.kernel.read_state()
            native["E7"] = {
                "finding_id": finding_id, "pre_fix_candidate_digest": initial_e6["validation"]["candidate_digest"],
                "post_fix_candidate_ref": copy.deepcopy(repair["candidate_ref"]), "worker_result_ref": copy.deepcopy(repair["worker_result_ref"]),
                "focused_receipt_refs": copy.deepcopy(repair["focused_receipt_refs"]), "fresh_review_physical_refs": copy.deepcopy(repair["fresh_review_refs"]),
                "fresh_review_artifact_refs": physical_review_artifacts, "verification_receipt_refs": copy.deepcopy(repair["verification_receipt_refs"]),
                "resolution_ref": resolution_ref, "closure_review_ref": copy.deepcopy(state["reviews"][review_id]["package_ref"]),
                "closure_verdict_ref": copy.deepcopy(state["findings"][finding_id]["closure_ref"]), "worker_self_close": False,
            }
            native["E6"] = {"routed_from": "E7", "finding_id": finding_id, "closure_verdict_ref": copy.deepcopy(state["findings"][finding_id]["closure_ref"]), "next": "E8"}
            native["E8"] = {"post_fix_candidate_ref": copy.deepcopy(repair["candidate_ref"]), "execution_closure_digest": repair["closure"]["closure_digest"], "aggregate_digest": repair["aggregate"]["aggregate_digest"]}
            native["E9"] = {"post_fix_candidate_ref": copy.deepcopy(repair["candidate_ref"]), "execution_closure_digest": repair["closure"]["closure_digest"], "verification_receipt_refs": copy.deepcopy(repair["verification_receipt_refs"])}
        return native

    def _terminalize_live_attempt(self, task_id, worker, authority, reason):
        try:
            state = self.kernel.read_state()
            if isinstance(state.get("leases", {}).get(task_id), Mapping):
                self.kernel.release_task(task_id, assignment_id=worker, authority_ref=authority, idempotency_key="runtime-e-release-" + task_id)
            state = self.kernel.read_state()
            task = state.get("tasks", {}).get(task_id)
            if isinstance(task, Mapping) and not task.get("invalidated"):
                self.kernel.invalidate_task(task_id, "runtime Group E terminal refusal: " + reason[:300], authority_ref=authority, idempotency_key="runtime-e-invalidate-" + task_id)
        except Exception:
            # The original refusal remains authoritative. Cold resume will also
            # expose an unexpected cleanup integrity failure.
            return

    def _terminalize_accepted_attempt(self, task_id, authority, reason):
        try:
            state = self.kernel.read_state()
            task = state.get("tasks", {}).get(task_id)
            if isinstance(task, Mapping) and not task.get("invalidated"):
                self.kernel.invalidate_task(task_id, "runtime Group E post-acceptance integrity refusal: " + reason[:300], authority_ref=authority, idempotency_key="runtime-e-postaccept-invalidate-" + task_id)
        except Exception:
            return

    def _kernel_authority(self, scopes: Sequence[str], write_scope: Sequence[str]) -> dict[str, Any]:
        state = self.kernel.read_state()
        return {"approved": True, "scopes": list(scopes), "run_id": state["run_id"], "write_scopes": sorted(write_scope), "human_receipt": copy.deepcopy(self.runtime.approval["receipt"])}

    def _compiler_authority(self, head, package_ref, lease_id, worker):
        state = self.kernel.read_state()
        deadline = datetime.fromisoformat(state["review_budget"]["deadline"].replace("Z", "+00:00"))
        attempts = state["review_budget"].get("finding_attempts", {})
        return {
            "authority_id": "runtime-e-parent", "actor_id": self.runtime.approval["actor"]["actor_id"], "role": "orchestrator", "assignment_id": "parent-" + worker,
            "scope_ref": {"path": package_ref["path"], "digest": package_ref["digest"]}, "epoch_id": state["epoch"]["id"], "lease_id": lease_id,
            "idempotency_key": "runtime-e-" + state["epoch"]["id"] + "-" + worker,
            "budget": {"seconds": max(1, int((deadline - datetime.now(timezone.utc)).total_seconds())), "review_round": state["review_budget"]["rounds_used"], "product_fix_attempts": max(attempts.values(), default=0)},
            "expected_head": copy.deepcopy(dict(head)),
        }

    def _common_inputs(self, head, authority, loop_level):
        policy_path = _declared_source_path(self._POLICY)
        return {"expected_head": copy.deepcopy(dict(head)), "loop_level": loop_level, "authority_ref": {key: copy.deepcopy(authority[key]) for key in ("authority_id", "role", "assignment_id", "lease_id", "scope_ref")}, "required_only_policy_ref": {"path": self._POLICY, "digest": "sha256:" + hashlib.sha256(policy_path.read_bytes()).hexdigest()}}

    @staticmethod
    def _compile(compiler, stage, inputs, authority, head, **kwargs):
        selector = "group.E." + ("E6" if stage == "E6-initial" else stage)
        result = compiler.compile(selector, inputs, authority, head, **kwargs)
        if result.get("schema") == "execution-group-refusal/v1":
            raise RuntimeExecutionError(stage + " refused: " + result.get("reason", "unknown"))
        return result

    @staticmethod
    def _record(stage, compiled, frontier, frozen_head, *, previous_ref, native_refs=None, lineage_refs=None):
        return {
            "schema": "runtime-execution-step/v2", "qualified_id": "group.E." + ("E6" if stage == "E6-initial" else stage), "record_id": stage,
            "objective_digest": frontier["objective_digest"], "inputs": {"persisted_frontier": copy.deepcopy(frontier), "native_kernel_refs": copy.deepcopy(native_refs), "prior_stage_refs": copy.deepcopy(lineage_refs or {})},
            "compiled_source_digest": _digest(compiled), "compiled": _durable_compiled(compiled), "previous_ref": copy.deepcopy(previous_ref), "runtime_identity": None,
            "started_head": copy.deepcopy(dict(frozen_head)), "finished_at": datetime.now(timezone.utc).isoformat(),
        }

    def _publish(self, artifact_id: str, value: Mapping[str, Any], kind: str) -> dict[str, Any]:
        value = copy.deepcopy(dict(value))
        value["runtime_identity"] = copy.deepcopy(self.runtime.identity)
        command = self.kernel.make_command("publish_artifact", {"artifact_id": artifact_id, "version": "v1", "value": value, "kind": kind, "path": None}, authority_ref={"approved": True, "scopes": ["publish_artifact"], "human_receipt": self.runtime.state["objective_ref"]["approval_ref"]["digest"]}, expected_head=_head(self.kernel), idempotency_key="publish-" + artifact_id)
        state = self.kernel.apply(command)
        return copy.deepcopy(state["artifacts"][artifact_id]["object_ref"])


__all__ = ["RuntimeExecution", "RuntimeExecutionError"]
