"""One compiler invocation per call, backed by the project-local Kernel.

This adapter does not impersonate a Skill/LLM, infer approval, execute shell
commands, or update a global current-Run pointer. The calling Skill supplies
its substantive inputs. A cold caller discovers the next step from the Kernel.
"""
from __future__ import annotations

import argparse
import copy
import hashlib
import json
import secrets
import sys
from datetime import datetime, timezone
from pathlib import Path

from .control_kernel import ControlKernel, KernelError
from .inception_cli import SKILLS, InceptionError, read_json, regular
from .outcome_system import OutcomeSystemV1
from .planning_system import PlanningSystemV1

SOURCE = Path(__file__).resolve().parents[3]
ROUTES = {"B": ["B%d" % n for n in range(1, 8)],
          "C": ["C%d" % n for n in range(1, 7)],
          "D": ["D%d" % n for n in range(1, 13)],
          "E": ["E%d" % n for n in range(1, 10)]}

_EXECUTION_INPUTS = {
    "task_id", "execution_package_input", "worker_assignment_id", "probes",
    "system_read_roots", "runtime_read_roots", "stage_inputs", "changed_paths",
}
_EXECUTION_OPTIONAL_INPUTS = {"loop_level"}
_LOOP_EXECUTION_INPUTS = {
    "task_id", "workflow_loop", "loop_request", "completion_request",
    "result_ref", "phase", "integration", "process_timeout", "metric_events",
}
_REPAIR_ACTIONS = (
    "status", "begin", "worker", "focused", "reviews-issue", "reviews-accept",
    "validator-issue", "validator-accept", "whole", "finalize",
)
_REPAIR_CAS_INPUTS = {"attempt_id", "nonce", "previous_state_ref", "expected_head"}
_WORKFLOW_LOOP_ACTIONS = (
    "status", "begin", "reserve", "running", "mark-running", "accept", "accept-result",
    "execution-unknown", "mark-execution-unknown", "recover", "repair", "repair-batch",
    "review-packages", "review-results", "evidence-validity", "completion", "phase-transition",
    "loop-policy", "loop-status", "loop-reserve", "loop-running", "loop-accept-result",
    "loop-execution-unknown", "loop-recover", "loop-repair", "loop-repair-batch",
    "loop-review-packages", "loop-review-results", "loop-evidence-validity",
    "loop-completion", "loop-phase-transition",
)
_WORKFLOW_LOOP_ENVELOPES = ("workflow_loop", "loop_request", "completion_request")


def _reject_durable_broker_secrets(value):
    """Keep broker authentication and boot identity out of durable caller JSON."""
    if isinstance(value, dict):
        for key, child in value.items():
            normalized = str(key).lower().replace("-", "_")
            if "capability" in normalized or "hmac" in normalized or normalized in {
                    "boot_id", "broker_state_root", "broker_secret"}:
                raise InceptionError("runtime execution JSON may not supply broker secrets or identity")
            _reject_durable_broker_secrets(child)
    elif isinstance(value, list):
        for child in value:
            _reject_durable_broker_secrets(child)


def execute_group_e(project, run_id, inputs):
    """Construct the trusted E parent locally and consume one physical JSON input."""
    if not isinstance(inputs, dict):
        raise InceptionError("runtime execute-e requires one JSON object")
    if inputs.get("schema") == "workflow-loop/v1":
        if any(name in inputs for name in _WORKFLOW_LOOP_ENVELOPES):
            raise InceptionError(
                "runtime execute-e cannot mix a canonical loop request with an envelope"
            )
        legacy_fields = (set(inputs) & (_EXECUTION_INPUTS | _EXECUTION_OPTIONAL_INPUTS)) - {
            "task_id"
        }
        if legacy_fields:
            raise InceptionError(
                "runtime execute-e canonical workflow-loop cannot carry legacy broker inputs"
            )
        identity = inputs.get("identity")
        task_id = inputs.get("task_id")
        if task_id is None and isinstance(identity, dict):
            task_id = identity.get("logical_task_id")
        if not isinstance(task_id, str) or not task_id:
            raise InceptionError("runtime execute-e workflow-loop identity is incomplete")
        _reject_durable_broker_secrets(inputs)
        from .runtime_execution import RuntimeExecution

        runtime = RuntimeExecution(project, run_id)
        result = runtime.execute_loop(
            task_id,
            inputs,
            result_ref=inputs.get("result_ref"),
            phase=inputs.get("phase", "E3"),
            integration=inputs.get("integration", False),
            process_timeout=inputs.get("process_timeout"),
            metric_events=inputs.get("metric_events"),
        )
        modules = ("inception_runtime", "runtime_execution")
        result["runtime_module_paths"] = {
            name: str(Path(sys.modules[__package__ + "." + name].__file__).resolve())
            for name in modules
        }
        return result
    loop_envelopes = [name for name in _WORKFLOW_LOOP_ENVELOPES if name in inputs]
    if loop_envelopes:
        if len(loop_envelopes) != 1 or set(inputs) - _LOOP_EXECUTION_INPUTS:
            raise InceptionError("runtime execute-e workflow-loop inputs are unsupported")
        if "task_id" not in inputs or not isinstance(inputs[loop_envelopes[0]], dict):
            raise InceptionError("runtime execute-e workflow-loop inputs are incomplete")
        _reject_durable_broker_secrets(inputs)
        from .runtime_execution import RuntimeExecution

        runtime = RuntimeExecution(project, run_id)
        result = runtime.execute_loop(
            inputs["task_id"],
            inputs[loop_envelopes[0]],
            result_ref=inputs.get("result_ref"),
            phase=inputs.get("phase", "E3"),
            integration=inputs.get("integration", False),
            process_timeout=inputs.get("process_timeout"),
            metric_events=inputs.get("metric_events"),
        )
        modules = ("inception_runtime", "runtime_execution")
        result["runtime_module_paths"] = {
            name: str(Path(sys.modules[__package__ + "." + name].__file__).resolve())
            for name in modules
        }
        return result
    if not isinstance(inputs, dict) or set(inputs) - (_EXECUTION_INPUTS | _EXECUTION_OPTIONAL_INPUTS):
        raise InceptionError("runtime execute-e input fields are unsupported")
    if not _EXECUTION_INPUTS <= set(inputs):
        raise InceptionError("runtime execute-e inputs are incomplete")
    _reject_durable_broker_secrets(inputs)
    from .macos_task_process import MacOSTaskProcessBroker
    from .runtime_execution import RuntimeExecution

    runtime = RuntimeExecution(project, run_id)
    # The location is parent-owned and deterministic. Authentication material
    # and boot identity are generated in this process, never read from JSON.
    run_key = hashlib.sha256(str(run_id).encode("utf-8")).hexdigest()[:32]
    broker_root = runtime.runtime.project / ".agent-workflow/state/runtime-e" / run_key
    broker = MacOSTaskProcessBroker(
        broker_root, capability=secrets.token_bytes(32), boot_id=secrets.token_hex(16)
    )
    try:
        result = runtime.execute(
            inputs["task_id"], inputs["execution_package_input"],
            worker_assignment_id=inputs["worker_assignment_id"], broker=broker,
            probes=inputs["probes"], system_read_roots=inputs["system_read_roots"],
            runtime_read_roots=inputs["runtime_read_roots"],
            stage_inputs=inputs["stage_inputs"], changed_paths=inputs["changed_paths"],
            loop_level=inputs.get("loop_level", "artifact"),
        )
    finally:
        broker.close()
    modules = ("inception_runtime", "runtime_execution", "macos_task_process")
    result["runtime_module_paths"] = {
        name: str(Path(sys.modules[__package__ + "." + name].__file__).resolve())
        for name in modules
    }
    return result


def _reject_repair_result_claims(value):
    """Keep caller-authored success and runner evidence out of repair actions."""
    if isinstance(value, dict):
        for key, child in value.items():
            normalized = str(key).lower().replace("-", "_")
            if normalized == "passed":
                raise InceptionError("runtime repair-e JSON may not supply a passed claim")
            _reject_repair_result_claims(child)
    elif isinstance(value, list):
        for child in value:
            _reject_repair_result_claims(child)


def _repair_broker(project, run_id, action, attempt_id):
    """Construct one fresh trusted-parent broker without serializing its identity."""
    from .macos_task_process import MacOSTaskProcessBroker

    selected = Path(project).resolve(strict=True)
    run_key = hashlib.sha256(str(run_id).encode("utf-8")).hexdigest()[:32]
    attempt_key = hashlib.sha256(str(attempt_id).encode("utf-8")).hexdigest()[:32]
    broker_root = selected / ".agent-workflow/state/runtime-e-repair" / run_key / attempt_key / action
    return MacOSTaskProcessBroker(
        broker_root, capability=secrets.token_bytes(32), boot_id=secrets.token_hex(16)
    )


def _has_loop_control_run(project, run_id):
    """Read-only detection for the explicit workflow-loop repair route."""
    try:
        kernel = ControlKernel(Path(project).resolve(strict=True), run_id)
        state = kernel.read_state()
    except (KernelError, KeyError, OSError, TypeError, ValueError):
        return False
    return isinstance(state, dict) and isinstance(state.get("loop_control"), dict)


def _workflow_loop_requested(project, run_id, action, inputs):
    """Select v1 only from a v1 envelope, alias, or loop-control Run."""
    explicit = (
        inputs.get("schema") == "workflow-loop/v1"
        or any(name in inputs for name in _WORKFLOW_LOOP_ENVELOPES)
        or action.startswith("loop-")
    )
    if explicit:
        return True
    # ``status`` and ``begin`` are historical repair actions.  Keep their
    # unwrapped spelling on the legacy route even when a Run happens to carry
    # loop-control state; v1 callers use the explicit loop-* aliases or the
    # workflow-loop/v1 envelope.
    if action in {"status", "begin"}:
        return False
    return action in _WORKFLOW_LOOP_ACTIONS and _has_loop_control_run(project, run_id)


def _workflow_loop_action(action):
    if action == "loop-policy":
        return "loop-policy"
    if action.startswith("loop-"):
        return action[5:]
    return {
        "begin": "reserve",
        "mark-running": "running",
        "accept": "accept",
        "mark-execution-unknown": "execution-unknown",
        "repair": "repair-batch",
    }.get(action, action)


def _workflow_loop_payload(inputs):
    """Unwrap one v1 envelope without accepting caller authority fields."""
    payload = copy.deepcopy(inputs)
    if payload.get("schema") == "workflow-loop/v1":
        payload.pop("schema", None)
    for name in _WORKFLOW_LOOP_ENVELOPES:
        nested = payload.pop(name, None)
        if nested is None:
            continue
        if not isinstance(nested, dict):
            raise InceptionError("workflow-loop input envelope is malformed")
        merged = {key: value for key, value in payload.items() if key not in nested}
        merged.update(nested)
        payload = merged
        if payload.get("schema") == "workflow-loop/v1":
            payload.pop("schema", None)
        break
    return payload


def execute_repair_e(project, run_id, action, inputs):
    """Dispatch one staged repair action through the trusted parent adapter."""
    if action not in _REPAIR_ACTIONS and action not in _WORKFLOW_LOOP_ACTIONS:
        raise InceptionError("runtime repair-e action is unsupported")
    if not isinstance(inputs, dict):
        raise InceptionError("runtime repair-e requires one JSON object")
    _reject_durable_broker_secrets(inputs)
    _reject_repair_result_claims(inputs)

    if _workflow_loop_requested(project, run_id, action, inputs):
        from .runtime_repair import WorkflowLoopRepairCoordinator

        coordinator = WorkflowLoopRepairCoordinator(project, run_id)
        return coordinator.dispatch(
            _workflow_loop_action(action), _workflow_loop_payload(inputs)
        )

    from .runtime_repair import RuntimeRepairCoordinator

    coordinator = RuntimeRepairCoordinator(project, run_id)
    if action == "status":
        if set(inputs) - {"task_id", "finding_id"}:
            raise InceptionError("runtime repair-e status inputs are unsupported")
        return coordinator.status(
            task_id=inputs.get("task_id"), finding_id=inputs.get("finding_id")
        )

    methods = {
        "begin": coordinator.begin,
        "worker": coordinator.accept_worker,
        "reviews-issue": coordinator.issue_reviews,
        "reviews-accept": coordinator.accept_review,
        "validator-issue": coordinator.issue_validator,
        "validator-accept": coordinator.accept_validator,
        "finalize": coordinator.finalize,
    }
    if action in methods:
        return methods[action](copy.deepcopy(inputs))

    # Focused and whole checks consume only the persisted attempt identity and
    # CAS predecessor. Runner status, receipts, captures, capability, boot ID,
    # and state-root identity are all derived inside this trusted process.
    if set(inputs) != _REPAIR_CAS_INPUTS:
        raise InceptionError(
            "runtime repair-e %s accepts only attempt identity and CAS inputs" % action
        )
    broker = _repair_broker(project, run_id, action, inputs["attempt_id"])
    try:
        method = coordinator.run_focused if action == "focused" else coordinator.run_whole
        return method(copy.deepcopy(inputs), broker)
    finally:
        broker.close()


def physical(ref, project):
    if not isinstance(ref, dict) or not {"path", "digest"} <= ref.keys():
        raise InceptionError("physical path and digest required")
    path = Path(ref["path"])
    if not path.is_absolute():
        base = SOURCE if path.parts and path.parts[0] == "agent-workflows" else project
        path = base / path
    path = regular(path)
    if "sha256:" + hashlib.sha256(path.read_bytes()).hexdigest() != ref["digest"]:
        raise InceptionError("stale physical input: " + str(path))
    return path


def verify_refs(value, project):
    if isinstance(value, dict):
        if "path" in value and "digest" in value:
            physical(value, project)
        for child in value.values():
            verify_refs(child, project)
    elif isinstance(value, list):
        for child in value:
            verify_refs(child, project)


def file_ref(path):
    path = regular(path)
    return {"path": str(path), "version": "v1",
            "digest": "sha256:" + hashlib.sha256(path.read_bytes()).hexdigest()}


class InceptionRuntime:
    def __init__(self, project, run_id):
        self.project = Path(project).resolve(strict=True)
        self.kernel = ControlKernel(self.project, run_id)
        self._refresh()

    def _refresh(self):
        with self.kernel._lock():
            self.state, head = self.kernel._load_current()
            self.bound_head = {key: head[key] for key in ("revision", "transaction_digest")}
        self.identity = self.state.get("metadata", {}).get("runtime_identity")
        if not self.identity or self.identity.get("project_root") != str(self.project):
            raise InceptionError("project-local runtime identity required")
        approval_ref = self.state["objective_ref"].get("approval_ref")
        if not approval_ref:
            raise InceptionError("actual objective approval transaction required")
        self.approval = self.kernel.read_object(approval_ref)["payload"]

    def _authority(self, operation):
        return {"approved": True, "scopes": [operation],
                "human_receipt": self.state["objective_ref"]["approval_ref"]["digest"]}

    def _head(self):
        return copy.deepcopy(self.bound_head)

    def _publish(self, artifact_id, value, kind):
        command = self.kernel.make_command("publish_artifact", {"artifact_id": artifact_id, "version": "v1", "value": value, "kind": kind, "path": None},
                                           authority_ref=self._authority("publish_artifact"), expected_head=self.bound_head)
        self.state = self.kernel.apply(command)

    def _record_file_ref(self, record):
        return file_ref(self.kernel.objects_dir / (record["ref"]["digest"][7:] + ".json"))

    def _budget(self):
        """Reject a non-dispatch state without turning elapsed time into progress.

        The method name is retained for legacy callers.  A loop-control Run is
        governed only by its immutable history and typed terminal state; only
        an explicitly old Run reads the historical wall-clock field.
        """
        loop = self.state.get("loop_control")
        if isinstance(loop, dict):
            if loop.get("recovery_required"):
                raise InceptionError("Run requires explicit execution recovery evidence")
            terminal = loop.get("terminal_outcome") or loop.get("outcome")
            if terminal is not None:
                raise InceptionError("Run loop is non-dispatch: " + str(terminal))
            if loop.get("dispatch_allowed") is not True:
                raise InceptionError("Run loop does not permit dispatch")
            return
        # Explicit legacy Run: preserve its old deadline byte-for-byte and do
        # not synthesize or extend it while resuming.
        budget = self.state.get("review_budget", {})
        deadline = budget.get("deadline") or budget.get("wall_clock_deadline")
        if deadline and datetime.now(timezone.utc) >= datetime.fromisoformat(deadline.replace("Z", "+00:00")):
            raise InceptionError("Run wall-clock budget exhausted")

    def _desired_loop_identity(self, phase, logical_task_id):
        predecessor = None
        loop = self.state.get("loop_control")
        refs = loop.get("event_refs", []) if isinstance(loop, dict) else []
        if refs:
            latest = self.kernel.read_object(refs[-1])["payload"]
            predecessor = {"id": latest["event_id"], "digest": refs[-1]["digest"]}
        objective = self.state["objective_ref"]
        return {
            "schema": "loop-work-identity/v1",
            "work_lineage_id": self.state["run_id"],
            "logical_task_id": logical_task_id,
            "phase": phase,
            "scope_revision": str(objective["version"]) + ":" + phase,
            "requirements_digest": objective["digest"],
            "predecessor_ref": predecessor,
        }

    def _ensure_loop_phase(self, phase, logical_task_id):
        loop = self.state.get("loop_control")
        if not isinstance(loop, dict):
            return
        desired = self._desired_loop_identity(phase, logical_task_id)
        current = loop["identity"]
        if all(
            current.get(key) == desired[key]
            for key in ("work_lineage_id", "logical_task_id", "phase")
        ):
            return
        self.state = self.kernel.transition_loop_phase(
            desired,
            reason="operational-phase-entry",
            authority_ref=self._authority("transition_loop_phase"),
            expected_revision=self.state["revision"],
        )
        self._refresh()

    def _ensure_step_loop_phase(self, short):
        if short.startswith("C"):
            self._ensure_loop_phase("C", "group-C-outcomes")
        elif short in {"D1", "D2", "D3", "D4"}:
            self._ensure_loop_phase("D1", "group-D-specification")
        elif short == "D5":
            self._ensure_loop_phase("D5", "group-D-option-set")
        elif short.startswith("D"):
            self._ensure_loop_phase("D6", "group-D-planning")

    def records(self, group=None):
        result = {}
        for key, item in self.state["artifacts"].items():
            if key.startswith("runtime-"):
                loaded = self.kernel.read_object(item["object_ref"])["payload"]
                value = loaded.get("payload", {})
                if isinstance(value, dict) and value.get("qualified_id", "").startswith("group."):
                    short = value["qualified_id"].split(".")[-1]
                    if (key != "runtime-" + short or loaded.get("kind") != "runtime-skill"
                            or value.get("objective_digest") != self.state["objective_ref"]["digest"]):
                        raise InceptionError("runtime Skill record identity or objective is stale")
                    compiled = value.get("compiled", {})
                    output = compiled.get("output", {})
                    if (value["qualified_id"] != "group.%s.%s" % (short[0], short)
                            or "refusal" in compiled.get("schema", "") or output.get("kind") == "refusal"
                            or output.get("status") == "not_ready"):
                        raise InceptionError("refused or mismatched Skill record cannot advance the frontier")
                    if group is None or short.startswith(group):
                        result[short] = {"value": value, "ref": item["object_ref"]}
        return result

    def status(self):
        self._refresh()
        group = self.state["group"]
        group_records = self.records(group["id"])
        route = list(ROUTES.get(group["id"], []))
        if (group["id"] == "E" and "E7" not in group_records
                and group_records.get("E6", {}).get("value", {}).get("compiled", {}).get("next") == "E8"):
            route.remove("E7")
        missing = [step for step in route if step not in group_records]
        next_id = missing[0] if missing and group["status"] == "open" else None
        ids = [*ROUTES["B"], *ROUTES["C"], *ROUTES["D"]]
        loop = self.state.get("loop_control")
        return {"run_id": self.state["run_id"], "mode": self.identity["mode"],
                "head": self._head(), "group": group, "epoch": self.state["epoch"],
                "record_refs": {key: copy.deepcopy(value["ref"]) for key, value in group_records.items()},
                "record_file_refs": {key: self._record_file_ref(value) for key, value in group_records.items()},
                "next_skill": SKILLS[ids.index(next_id)] if next_id in ids else None,
                "next_id": "group.%s.%s" % (next_id[0], next_id) if next_id and next_id in ids else None,
                "next_action": "execute-group-e-via-parent-runtime" if group["id"] == "E" and group["status"] == "open" and next_id else "invoke-one-skill-then-clear" if next_id else "close-group-with-audit" if group["status"] == "open" else "clear-then-advance",
                "progress_control": "iteration-and-evidence" if isinstance(loop, dict) else "legacy-wall-clock-budget",
                "loop_control": copy.deepcopy(loop),
                "objective_approval_source": self.approval["receipt"]["source"],
                "execution_authorized": False,
                "execution_ready": (group["id"] == "E" and group["status"] == "open"
                                          and self.records("D").get("D12", {}).get("value", {}).get("compiled", {}).get("output", {}).get("status") in {"ready", "ready_with_accepted_risks"})}

    def step(self, qualified_id, inputs, *, actor_ref):
        status = self.status()
        short = qualified_id.split(".")[-1]
        if qualified_id != status["next_id"] or short[0] not in ("C", "D"):
            raise InceptionError("wrong frontier; use actual B approval adoption for Group B")
        self._ensure_step_loop_phase(short)
        self._budget()
        values = copy.deepcopy(inputs)
        verify_refs(values, self.project)
        physical(actor_ref, self.project)
        actor = read_json(physical(actor_ref, self.project))
        expected_source = "mock" if self.identity["mode"] == "rehearsal" else "human"
        if (actor.get("actor_id") != self.approval["actor"]["actor_id"]
                or actor.get("source") != expected_source):
            raise InceptionError("owner does not match approved actor")
        objective = {key: self.state["objective_ref"][key] for key in ("path", "version", "digest")}
        physical(objective, self.project)
        if values.get("objective_ref", objective) != objective:
            raise InceptionError("step cannot substitute another objective")
        records = self.records()
        b7_actor = records.get("B7", {}).get("value", {}).get("inputs", {}).get("approval_receipt", {}).get("actor_ref", {}).get("authority_ref")
        if b7_actor and actor_ref["digest"] != b7_actor["digest"]:
            raise InceptionError("actor bytes differ from the adopted approval actor")
        predecessor = records.get(short[0] + str(int(short[1:]) - 1))
        self._bind_inputs(short, values, records, predecessor, objective)
        namespace = self.identity["namespace"]
        if short.startswith("C"):
            values["objective_ref"] = objective
            authority = {"namespace": namespace, "scope": "candidate-generic",
                         "owner_ref": {"kind": "system" if self.identity["mode"] == "rehearsal" else "human",
                                       "stable_id": self.approval["actor"]["actor_id"], "role": "objective-owner",
                                       "path": actor_ref["path"], "digest": actor_ref["digest"]}}
            compiled = OutcomeSystemV1().compile(qualified_id, values, authority, self._head())
        else:
            authority = {"namespace": "rehearsal:" + self.state["run_id"] if self.identity["mode"] == "rehearsal" else namespace,
                         "scope": "candidate-generic", "owner_ref": actor_ref, "authority_ref": objective}
            if short == "D6":
                receipt = values.get("approved_option_receipt", {})
                stored = read_json(physical(receipt.get("receipt_ref"), self.project))
                expected_source = "mock" if self.identity["mode"] == "rehearsal" else "human"
                if (stored.get("source") != expected_source or stored.get("decision") != "approve"
                        or stored.get("explicit") is not True or stored.get("scope") != "option-selection"
                        or stored.get("actor_id") != self.approval["actor"]["actor_id"]
                        or stored.get("objective_digest") != objective["digest"]):
                    raise InceptionError("physical option receipt is not an explicit bound approval")
                if not predecessor or stored.get("options_digest") != predecessor["ref"]["digest"]:
                    raise InceptionError("option receipt must bind current D5 candidate")
                options = predecessor["value"]["compiled"]["output"]["options"]
                option_ids = [item.get("id", item.get("option_id", item.get("name"))) for item in options]
                selected = stored.get("selected_option")
                if (any(not isinstance(item, str) or not item for item in option_ids)
                        or len(set(option_ids)) != len(option_ids)
                        or selected not in option_ids or values.get("design", {}).get("selected_option") != selected):
                    raise InceptionError("design must bind the exact option selected by its receipt")
            compiled = PlanningSystemV1().compile(qualified_id, values, authority, self._head())
        output = compiled.get("output", {})
        if "refusal" in compiled["schema"] or output.get("kind") == "refusal" or output.get("status") == "not_ready":
            raise InceptionError("compiler refused: " + json.dumps(compiled, ensure_ascii=False))
        value = {"schema": "runtime-skill-step/v1", "qualified_id": qualified_id,
                 "objective_digest": objective["digest"], "inputs": values, "compiled": compiled,
                 "previous_ref": predecessor["ref"] if predecessor else self.state.get("epoch_contexts", {}).get(self.state["epoch"]["id"], {}).get("input_ref"),
                 "runtime_identity": self.identity, "started_head": self._head(),
                 "finished_at": datetime.now(timezone.utc).isoformat()}
        self._publish("runtime-" + short, value, "runtime-skill")
        return self.status()

    def _bind_inputs(self, short, values, records, predecessor, objective):
        def output(step):
            record = records.get(step)
            if not record:
                raise InceptionError("missing current upstream Skill: " + step)
            return record["value"]["compiled"]["output"]
        if short.startswith("C"):
            if short in {"C2", "C3", "C4", "C5", "C6"}:
                outcomes = output("C1")["payload"]["outcomes"]
                ids = {item["outcome_id"] for item in outcomes}
                if short == "C2" and set(values.get("dependency_graph", {}).get("node_ids", [])) != ids:
                    raise InceptionError("dependency graph must bind current C1 outcomes")
                if short == "C3" and values.get("measurement_plan", {}).get("outcome_id") not in ids:
                    raise InceptionError("measurement must bind current C1 outcomes")
                if short == "C4" and values.get("target_set", {}).get("outcome_id") != output("C3")["payload"]["outcome_id"]:
                    raise InceptionError("target must bind current C3 measurement")
                if short == "C5" and values.get("target_set") != output("C4")["payload"]:
                    raise InceptionError("baseline must bind current C4 target")
                if short == "C6":
                    expected = {"objective": objective, **{key: output(step)["payload"] for key, step in (("outcome_map", "C1"), ("dependency_graph", "C2"), ("measurement_plan", "C3"), ("target_set", "C4"), ("baseline", "C5"))}}
                    if values.get("trace") != expected:
                        raise InceptionError("validation trace must contain exact current C1-C5 outputs")
        else:
            if predecessor and self._record_file_ref(predecessor) not in values.get("input_refs", []):
                raise InceptionError("planning input_refs must include the current predecessor physical object")
            if short == "D9" and values.get("tasks") != output("D8")["tasks"]:
                raise InceptionError("execution DAG must bind the current D8 tasks")
            if short in {"D10", "D11"}:
                current_tasks = output("D8")["tasks"]
                ids = {task["task_id"] for task in current_tasks}
                items = values.get(
                    "briefs" if short == "D10" else "task_loop_policies", []
                )
                if {item.get("task_id") for item in items} != ids or len(items) != len(ids):
                    raise InceptionError(
                        "briefs and loop policies must cover exact current D8 tasks"
                    )
                if short == "D11":
                    if values.get("tasks", current_tasks) != current_tasks:
                        raise InceptionError("loop policies must bind exact current D8 tasks")
                    values["tasks"] = copy.deepcopy(current_tasks)
            if short == "D12":
                supplied = values.get("evidence_refs", [])
                if any(self._record_file_ref(records["D%d" % index]) not in supplied for index in range(1, 12)):
                    raise InceptionError("readiness evidence must bind current D1-D11 outputs")
                for risk in values.get("open_risks", []):
                    receipt = read_json(physical(risk.get("receipt_ref"), self.project))
                    source = "mock" if self.identity["mode"] == "rehearsal" else "human"
                    if (receipt.get("source") != source or receipt.get("actor_id") != self.approval["actor"]["actor_id"]
                            or receipt.get("objective_digest") != objective["digest"] or receipt.get("scope") != "risk-acceptance"
                            or receipt.get("explicit") is not True or receipt.get("decision") != "approve"
                            or not risk.get("risk_id") or receipt.get("risk_id") != risk["risk_id"]):
                        raise InceptionError("accepted risk requires its explicit bound physical receipt")

    def close(self, audit_ref):
        from .runtime_closure import close_runtime_group
        self.status()
        self._budget()
        group = self.state["group"]["id"]
        records = self.records(group)
        required = set(ROUTES[group]) if group in ROUTES else set()
        # E7 exists only when E6 validated a required repair.  The normal
        # no-finding route is E1-E6,E8,E9; a repair route must also persist E7.
        if group == "E" and "E7" not in records:
            if records.get("E6", {}).get("value", {}).get("compiled", {}).get("next") != "E8":
                raise InceptionError("E7 may be omitted only after E6 routes directly to E8")
            required.remove("E7")
        if group not in ROUTES or set(records) != required:
            raise InceptionError("all required Group Skill results must exist before closure")
        audit = read_json(physical(audit_ref, self.project))
        refs = [records[item]["ref"] for item in ROUTES[group] if item in records]
        if (audit.get("group_id") != group or audit.get("objective_digest") != self.state["objective_ref"]["digest"]
                or audit.get("alignment") != "aligned" or audit.get("artifact_refs") != refs
                or not audit.get("reviewer") or not audit.get("rationale")):
            raise InceptionError("Group audit must bind the exact current candidates and objective")
        if any(self.kernel._finding_blocks(item) for item in self.state["findings"].values()) or self.state.get("leases"):
            raise InceptionError("Group cannot close with active work or blocking findings")
        audit_id = "runtime-audit-" + group
        existing = self.state["artifacts"].get(audit_id)
        if existing:
            loaded = self.kernel.read_object(existing["object_ref"])["payload"]
            if loaded.get("kind") != "runtime-group-audit" or loaded.get("payload") != audit:
                raise InceptionError("closure retry cannot change the published Group audit")
        else:
            self._publish(audit_id, audit, "runtime-group-audit")
        refs.append(self.state["artifacts"]["runtime-audit-" + group]["object_ref"])
        result = close_runtime_group(self.kernel, {"approved": True, "human_receipt": self.state["objective_ref"]["approval_ref"]["digest"]},
                                     next_group={"B": "C", "C": "D", "D": "E", "E": "H"}[group], evidence_refs=refs)
        self.state = result["state"]
        return {**self.status(), "closure_report_ref": result["closure_report_ref"], "evidence_root": result["evidence_root"]}

    def advance(self):
        self.status()
        self._budget()
        group = self.state["group"]["id"]
        # Opening the successor and rotating its loop identity are separate
        # Kernel transactions. If the process stops between them, replaying
        # ``advance`` finishes that already-authorized transition.
        recovery_phase = {
            "C": ("C", "group-C-outcomes"),
            "D": ("D1", "group-D-specification"),
            "H": ("H", "group-H-objective-audit"),
        }.get(group)
        if self.state["group"]["status"] == "open" and recovery_phase:
            loop = self.state.get("loop_control")
            if isinstance(loop, dict) and loop["identity"].get("phase") != recovery_phase[0]:
                self._ensure_loop_phase(*recovery_phase)
                return self.status()
        next_group = {"B": "C", "C": "D", "D": "E", "E": "H"}.get(group)
        if not next_group or self.state["group"]["status"] != "closed":
            raise InceptionError("advance requires a closed Group; it never closes implicitly")
        authority = {"approved": True, "scopes": ["open_operational_group"], "runtime_identity": self.identity,
                     "run_id": self.state["run_id"], "write_scopes": [self.identity["namespace"]],
                     "protected_fields": ["group", "epoch", "ready"], "human_receipt": self.approval["receipt"]}
        self.state = self.kernel.open_operational_group(next_group, next_group + "-01", self.state["group"]["bundle_ref"], authority_ref=authority)
        if next_group == "C":
            self._ensure_loop_phase("C", "group-C-outcomes")
        elif next_group == "D":
            self._ensure_loop_phase("D1", "group-D-specification")
        elif next_group == "H":
            self._ensure_loop_phase("H", "group-H-objective-audit")
        return self.status()


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("operation", choices=["status", "step", "close", "advance", "adopt", "execute-e", "repair-e"])
    parser.add_argument("--project", required=True)
    parser.add_argument("--run-id", required=True)
    parser.add_argument("--inputs")
    parser.add_argument("--qualified-id")
    parser.add_argument("--actor")
    parser.add_argument("--action", choices=(*_REPAIR_ACTIONS, *_WORKFLOW_LOOP_ACTIONS))
    args = parser.parse_args(argv)
    try:
        if args.operation == "adopt":
            from .runtime_approval import adopt_approved_objective
            adopt_approved_objective(args.project, args.run_id, **read_json(args.inputs))
            result = InceptionRuntime(args.project, args.run_id).status()
        elif args.operation == "execute-e":
            result = execute_group_e(args.project, args.run_id, read_json(args.inputs))
        elif args.operation == "repair-e":
            if args.action is None or args.inputs is None:
                raise InceptionError("runtime repair-e requires --action and --inputs")
            result = execute_repair_e(
                args.project, args.run_id, args.action, read_json(args.inputs)
            )
        else:
            runtime = InceptionRuntime(args.project, args.run_id)
            if args.operation == "step":
                result = runtime.step(args.qualified_id, read_json(args.inputs), actor_ref=file_ref(Path(args.actor).resolve()))
            elif args.operation == "close":
                result = runtime.close(file_ref(Path(args.inputs).resolve()))
            else:
                result = getattr(runtime, args.operation)()
        print(json.dumps(result, ensure_ascii=False, indent=2))
        return 0
    except (ValueError, KernelError, OSError, TypeError, KeyError) as error:
        print(json.dumps({"status": "blocked", "reason": str(error)}, ensure_ascii=False))
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
