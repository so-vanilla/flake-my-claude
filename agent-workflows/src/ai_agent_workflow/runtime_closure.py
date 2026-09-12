"""Execute shared F1--F7 against an operational Run and immutable evidence.

The supplementary store contains compiler inputs, never canonical Run state.
Its checkpoint is explicitly a preclosure snapshot; only the Kernel creates
closed-boundary bundles/checkpoints and advances HEAD.
"""
from __future__ import annotations

import copy
import hashlib
import json
import os
from collections.abc import Mapping, Sequence
from pathlib import Path
from typing import Any

from .closure_protocol import ClosureProtocolError, SharedClosureProtocolV1
from .completion import CompletionError
from .control_kernel import ControlKernel, canonical_digest
from .evidence_validity import EvidenceValidityError, assess_evidence
from .execution_v2 import MechanicalCompletion, WorkflowLoopValidator
from .loop_contracts import (
    LOOP_CONTRACT_VERSION,
    LoopContractError,
    counter_identity,
    validate_terminal_record,
    validate_work_identity,
)
from .loop_metrics import LoopMetricsError, derive_metrics
from .loop_state import event_ref

GROUPS = ("B", "C", "D", "E", "H", "completed")


def _current(kernel: ControlKernel) -> tuple[dict, dict]:
    # Read the state and its parent-linked HEAD together. Supplementary
    # snapshots must never join revision N with HEAD N+1.
    with kernel._lock():
        state, head = kernel._load_current()
        return copy.deepcopy(state), {key: head[key] for key in ("revision", "transaction_digest")}


def _snapshot(root: Path, object_type: str, payload: Mapping[str, Any], **metadata: Any) -> dict:
    """Publish a content-addressed supplementary input without changing HEAD."""
    body = {"schema": "runtime-closure-input/v1", "object_type": object_type, "payload": dict(payload)}
    digest = canonical_digest(body)
    path = root / "objects" / (digest[7:] + ".json")
    path.parent.mkdir(parents=True, exist_ok=True)
    encoded = json.dumps(body, sort_keys=True, separators=(",", ":"), ensure_ascii=False).encode()
    try:
        with path.open("xb") as stream:
            stream.write(encoded)
            stream.flush()
            os.fsync(stream.fileno())
    except FileExistsError:
        if path.read_bytes() != encoded:
            raise ClosureProtocolError("immutable closure input collision")
    return {"digest": digest, "object_type": object_type, "path": "objects/" + path.name, **metadata}


def _loop_history(kernel: ControlKernel, state: Mapping[str, Any]) -> list[dict[str, Any]]:
    loader = getattr(kernel, "_loop_history", None)
    if callable(loader):
        try:
            value = loader(state)
        except Exception as error:
            raise ClosureProtocolError("workflow-loop history is unavailable: " + str(error)) from error
        if isinstance(value, list):
            return copy.deepcopy(value)
    loop = state.get("loop_control", {})
    value = loop.get("history", loop.get("events")) if isinstance(loop, Mapping) else None
    if isinstance(value, list):
        return copy.deepcopy(value)
    refs = loop.get("event_refs", []) if isinstance(loop, Mapping) else []
    if not isinstance(refs, list):
        raise ClosureProtocolError("workflow-loop event references are malformed")
    result = []
    for ref in refs:
        loaded = kernel.read_object(ref)
        if not isinstance(loaded, Mapping) or not isinstance(loaded.get("payload"), Mapping):
            raise ClosureProtocolError("workflow-loop event reference is malformed")
        result.append(copy.deepcopy(dict(loaded["payload"])))
    return result


def _loop_completion_request(value: Any, identity: Mapping[str, Any]) -> dict[str, Any]:
    if not isinstance(value, Mapping):
        raise ClosureProtocolError("workflow-loop closure requires a completion request")
    request = copy.deepcopy(dict(value))
    for field in ("workflow_loop", "loop_request", "completion_request"):
        nested = request.get(field)
        if nested is not None:
            if not isinstance(nested, Mapping):
                raise ClosureProtocolError("workflow-loop completion request is malformed")
            supplied = copy.deepcopy(dict(nested))
            for key, item in request.items():
                if key not in {"workflow_loop", "loop_request", "completion_request"}:
                    supplied.setdefault(key, copy.deepcopy(item))
            request = supplied
            break
    request["schema"] = LOOP_CONTRACT_VERSION
    request.setdefault("identity", copy.deepcopy(dict(identity)))
    for field in ("phase", "profile", "events", "history", "state_entries", "iteration_events", "loop_control"):
        request.pop(field, None)
    return request


def record_workflow_loop_outcome(
    kernel: ControlKernel,
    authority: Mapping[str, Any],
    *,
    record: Mapping[str, Any],
) -> dict[str, Any]:
    """Persist one explicit, non-authorizing workflow-loop terminal record.

    ``close_workflow_loop`` remains a read/validate close-set compiler so a
    caller can inspect strict completion before choosing a durable stop.  This
    separate seam is the only runtime-closure helper that records a loop
    outcome; it never advances Group/H state or claims objective achievement.
    """

    state, initial_head = _current(kernel)
    if not isinstance(state.get("loop_control"), Mapping):
        raise ClosureProtocolError("workflow-loop outcome requires loop_control state")
    if authority.get("approved") is not True and authority.get("status") != "approved":
        raise ClosureProtocolError("approved loop outcome authority is required")
    if authority.get("expected_head") not in (None, initial_head):
        raise ClosureProtocolError("workflow-loop outcome authority HEAD is stale")
    try:
        checked = validate_terminal_record(record)
        identity = validate_work_identity(state["loop_control"].get("identity"))
    except (LoopContractError, TypeError, ValueError) as error:
        raise ClosureProtocolError("workflow-loop terminal record is invalid: " + str(error)) from error
    if checked["identity"] != identity:
        raise ClosureProtocolError("workflow-loop terminal record identity differs from current Run")
    existing = state["loop_control"].get("terminal_record")
    if existing is not None:
        if existing != checked:
            raise ClosureProtocolError("workflow-loop Run already has a different terminal record")
        return {
            "schema": "workflow-loop-runtime-outcome/v1",
            "status": "already-recorded",
            "outcome": checked["outcome"],
            "record": checked,
            "terminal_ref": copy.deepcopy(state["loop_control"].get("terminal_ref")),
            "state": state,
            "head": initial_head,
            "non_authorizing": True,
            "objective_achievement": False,
            "activation": False,
        }
    try:
        updated = kernel.record_loop_outcome(
            checked,
            authority_ref=authority,
            expected_revision=state["revision"],
        )
    except Exception as error:
        raise ClosureProtocolError("workflow-loop terminal outcome was rejected: " + str(error)) from error
    final_head = _current(kernel)[1]
    return {
        "schema": "workflow-loop-runtime-outcome/v1",
        "status": "recorded",
        "outcome": checked["outcome"],
        "record": checked,
        "terminal_ref": copy.deepcopy(updated["loop_control"].get("terminal_ref")),
        "state": updated,
        "head": final_head,
        "non_authorizing": True,
        "objective_achievement": False,
        "activation": False,
    }


def close_workflow_loop(
    kernel: ControlKernel,
    authority: Mapping[str, Any],
    *,
    next_group: str,
    evidence_refs: Sequence[Mapping[str, Any]] = (),
    completion_request: Mapping[str, Any] | None = None,
    workflow_loop: Mapping[str, Any] | None = None,
    loop_request: Mapping[str, Any] | None = None,
    metrics_events: Sequence[Mapping[str, Any]] | None = None,
    integration_identity: Mapping[str, Any] | None = None,
    accepted_decisions: Sequence[Mapping[str, Any]] = (),
) -> dict[str, Any]:
    """Validate a strict workflow-loop close set without composing H.

    The legacy ``close_runtime_group`` path owns F1--F7 and remains unchanged.
    A loop-control Run uses this additive, non-authorizing boundary: current
    Kernel state/HEAD, immutable loop history, current evidence, two-axis
    review, required findings, and integration identity are checked, then a
    digest-bound close candidate is returned.  Group/H advancement, objective
    achievement, activation, and external writes are deliberately outside this
    function.
    """

    state, initial_head = _current(kernel)
    loop = state.get("loop_control")
    if not isinstance(loop, Mapping):
        raise ClosureProtocolError("workflow-loop closure requires loop_control state")
    if authority.get("approved") is not True and authority.get("status") != "approved":
        raise ClosureProtocolError("approved closure authority is required")
    if authority.get("expected_head") not in (None, initial_head):
        raise ClosureProtocolError("workflow-loop closure authority HEAD is stale")
    if accepted_decisions:
        raise ClosureProtocolError("workflow-loop closure does not compose objective decisions")
    if not isinstance(next_group, str) or not next_group:
        raise ClosureProtocolError("workflow-loop closure next_group is malformed")
    try:
        identity = validate_work_identity(loop.get("identity"))
    except LoopContractError as error:
        raise ClosureProtocolError("workflow-loop identity is invalid: " + str(error)) from error
    history = _loop_history(kernel, state)
    if not history:
        raise ClosureProtocolError("workflow-loop closure requires an evaluated loop event")
    latest = history[-1]
    if latest.get("status") == "execution-unknown" or loop.get("recovery_required"):
        raise ClosureProtocolError("workflow-loop closure requires execution recovery evidence")
    if latest.get("status") != "evaluated":
        raise ClosureProtocolError("workflow-loop closure requires an accepted result")
    if state.get("leases") or any(
        item.get("status") in {"running", "needs_decision"}
        for item in state.get("tasks", {}).values()
        if isinstance(item, Mapping)
    ):
        raise ClosureProtocolError("workflow-loop closure has unfinished active tasks")

    supplied_integration = integration_identity
    if supplied_integration is None and isinstance(workflow_loop, Mapping):
        supplied_integration = workflow_loop.get("integration_identity")
    if supplied_integration is None and isinstance(loop_request, Mapping):
        supplied_integration = loop_request.get("integration_identity")
    # A caller-supplied E8/E9 value cannot substitute for a durable Kernel
    # transition.  The current Run must already be in an integration phase;
    # this keeps the E8-E9 counter and evaluated event in the same identity.
    if identity["phase"] not in {"E8", "E9"}:
        raise ClosureProtocolError("workflow-loop closure requires a durable E8-E9 integration identity")
    if supplied_integration is not None:
        try:
            checked = validate_work_identity(supplied_integration)
        except LoopContractError as error:
            raise ClosureProtocolError("integration identity is invalid: " + str(error)) from error
        if checked != identity:
            raise ClosureProtocolError("integration identity differs from current loop identity")
        supplied_integration = checked
    else:
        supplied_integration = copy.deepcopy(identity)

    request_value = (
        completion_request
        if completion_request is not None
        else workflow_loop
        if workflow_loop is not None
        else loop_request
    )
    request = _loop_completion_request(request_value, identity)
    try:
        request_identity = validate_work_identity(request.get("identity"))
    except LoopContractError as error:
        raise ClosureProtocolError("workflow-loop completion identity is invalid: " + str(error)) from error
    if counter_identity(request_identity) != counter_identity(identity):
        raise ClosureProtocolError("workflow-loop completion identity differs from current integration")
    try:
        validation = WorkflowLoopValidator().validate(request)
        # MechanicalCompletion is a named B4 facade; its output must agree
        # with the joined validator before a close candidate can be returned.
        mechanical = MechanicalCompletion().evaluate(request)
    except (CompletionError, ValueError, TypeError) as error:
        raise ClosureProtocolError("workflow-loop completion is not mechanically valid: " + str(error)) from error
    classification = mechanical.get("classification", validation.get("classification"))
    if not isinstance(classification, Mapping) or classification.get("outcome") != "completed":
        outcome = classification.get("outcome") if isinstance(classification, Mapping) else "needs-input"
        raise ClosureProtocolError("workflow-loop closure rejected: " + str(outcome))
    if validation.get("validator", {}).get("skipped") is not True or validation.get("next") != "complete":
        raise ClosureProtocolError("workflow-loop closure requires strict mechanical zero-finding completion")

    current_inputs = request.get("current_inputs")
    change_impact = request.get("change_impact")
    evidence_assessments = []
    if current_inputs is not None or change_impact is not None:
        if current_inputs is None or change_impact is None:
            raise ClosureProtocolError("workflow-loop evidence validity inputs are incomplete")
        for evidence in request.get("evidence", []):
            try:
                assessment = assess_evidence(evidence, current_inputs, change_impact)
            except EvidenceValidityError as error:
                raise ClosureProtocolError("workflow-loop evidence validity is unknown: " + str(error)) from error
            evidence_assessments.append(assessment)
            if assessment.get("status") != "valid":
                raise ClosureProtocolError("workflow-loop closure rejects stale or invalid evidence")

    loaded_evidence = []
    for ref in evidence_refs:
        try:
            loaded = kernel.read_object(ref)
        except Exception as error:
            raise ClosureProtocolError("workflow-loop closure evidence is unavailable") from error
        if loaded.get("digest") not in state.get("object_refs", {}):
            raise ClosureProtocolError("workflow-loop closure evidence is not in the current Run")
        loaded_evidence.append({
            "digest": loaded["digest"],
            "object_type": loaded.get("object_type"),
            "path": "objects/" + loaded["digest"][7:] + ".json",
        })
    if len({item["digest"] for item in loaded_evidence}) != len(loaded_evidence):
        raise ClosureProtocolError("workflow-loop closure evidence must be unique")

    metrics = None
    if metrics_events is not None:
        try:
            metrics = derive_metrics(metrics_events)
        except (LoopMetricsError, TypeError, ValueError) as error:
            raise ClosureProtocolError("workflow-loop metrics are malformed: " + str(error)) from error
    close_set = {
        "schema": "workflow-loop-close-set/v1",
        "contract_version": LOOP_CONTRACT_VERSION,
        "run_id": state.get("run_id"),
        "group": state.get("group"),
        "next_group": next_group,
        "identity": identity,
        "integration_identity": copy.deepcopy(supplied_integration),
        "head": initial_head,
        "history": history,
        "event_ref": event_ref(latest),
        "completion": copy.deepcopy(classification),
        "validation": copy.deepcopy(validation),
        "evidence_refs": loaded_evidence,
        "evidence_assessments": evidence_assessments,
        "metrics": metrics,
        "non_authorizing": True,
        "objective_achievement": False,
        "activation": False,
    }
    close_set["close_set_digest"] = canonical_digest(close_set)
    return {
        "schema": "workflow-loop-runtime-closure/v1",
        "status": "ready",
        "outcome": "completed",
        "close_set": close_set,
        "close_set_digest": close_set["close_set_digest"],
        "state": state,
        "head": initial_head,
        "non_authorizing": True,
        "objective_achievement": False,
        "activation": False,
    }


def close_runtime_group(
    kernel: ControlKernel, authority: Mapping[str, Any], *, next_group: str,
    evidence_refs: Sequence[Mapping[str, Any]], accepted_decisions: Sequence[Mapping[str, Any]] = (),
    completion_request: Mapping[str, Any] | None = None,
    workflow_loop: Mapping[str, Any] | None = None,
    loop_request: Mapping[str, Any] | None = None,
    metrics_events: Sequence[Mapping[str, Any]] | None = None,
    integration_identity: Mapping[str, Any] | None = None,
) -> dict:
    """Compile every F gate, then delegate the actual close to ControlKernel.

``human_receipt`` must identify the current immutable objective approval by
digest, approval ID, or receipt ID.  Rehearsal provenance remains in both the
Run and the published closure record; no human approval is synthesized.
    """
    state, initial_head = _current(kernel)
    # Existing InceptionRuntime Runs may contain loop_control while still
    # using the historical F1-F7 closure (notably B/C/D bootstrap).  Opt into
    # the additive workflow-loop closure only through its explicit request or
    # the dedicated close_workflow_loop API; a bare legacy close remains
    # byte-compatible.
    if "loop_control" in state and any(
        value is not None
        for value in (completion_request, workflow_loop, loop_request, integration_identity, metrics_events)
    ):
        return close_workflow_loop(
            kernel,
            authority,
            next_group=next_group,
            evidence_refs=evidence_refs,
            accepted_decisions=accepted_decisions,
            completion_request=completion_request,
            workflow_loop=workflow_loop,
            loop_request=loop_request,
            metrics_events=metrics_events,
            integration_identity=integration_identity,
        )
    identity = state.get("metadata", {}).get("runtime_identity")
    if not isinstance(identity, Mapping) or identity.get("mode") not in {"real", "rehearsal"}:
        raise ClosureProtocolError("runtime closure requires a project-local Run identity")
    if authority.get("approved") is not True and authority.get("status") != "approved":
        raise ClosureProtocolError("approved closure authority is required")
    if authority.get("expected_head") not in (None, initial_head):
        raise ClosureProtocolError("runtime closure authority HEAD is stale")
    group = state["group"]["id"]
    if group not in GROUPS[:-1] or next_group != GROUPS[GROUPS.index(group) + 1]:
        raise ClosureProtocolError("runtime closure must follow B, C, D, E, H, completed")
    if (state["group"].get("status"), state["epoch"].get("status")) not in {
        ("open", "open"), ("open", "closed"), ("closed", "closed"),
    }:
        raise ClosureProtocolError("runtime closure requires an open boundary or an accepted closure to resume")
    if any(kernel._finding_blocks(item) for item in state.get("findings", {}).values()):
        raise ClosureProtocolError("runtime closure has open blocking findings")
    if state.get("leases") or any(item.get("status") in {"running", "needs_decision"} for item in state.get("tasks", {}).values()):
        raise ClosureProtocolError("runtime closure has unfinished active tasks")
    objective = state.get("objective_ref", {})
    approval_ref = objective.get("approval_ref")
    if not isinstance(approval_ref, Mapping):
        raise ClosureProtocolError("runtime closure requires an approved objective")
    approval = kernel.read_object(approval_ref)["payload"]
    receipt = approval["receipt"]
    if (not isinstance(authority.get("human_receipt"), str)
            or authority["human_receipt"] not in {approval_ref["digest"], approval["approval_id"], receipt["receipt_id"]}):
        raise ClosureProtocolError("closure authority must identify the actual objective receipt")
    objective_path = Path(objective["path"])
    if not objective_path.is_file() or "sha256:" + hashlib.sha256(objective_path.read_bytes()).hexdigest() != objective["digest"]:
        raise ClosureProtocolError("objective physical bytes no longer match approval")
    if not evidence_refs:
        raise ClosureProtocolError("runtime closure needs physical Group evidence")
    evidence, audits = [], []
    for ref in evidence_refs:
        loaded = kernel.read_object(ref)
        if loaded["digest"] not in state["object_refs"]:
            raise ClosureProtocolError("closure evidence is not in the current Run")
        if ref.get("object_type", loaded["object_type"]) != loaded["object_type"]:
            raise ClosureProtocolError("closure evidence object type does not match")
        evidence.append({"digest": loaded["digest"], "object_type": loaded["object_type"], "path": "objects/" + loaded["digest"][7:] + ".json"})
        if loaded["object_type"] == "artifact" and loaded["payload"].get("kind") == "runtime-group-audit":
            audits.append(loaded["payload"].get("payload"))
    if len({ref["digest"] for ref in evidence}) != len(evidence):
        raise ClosureProtocolError("closure evidence must be unique")
    if len(audits) != 1 or not isinstance(audits[0], Mapping):
        raise ClosureProtocolError("runtime closure requires exactly one explicit Group alignment audit")
    audit = audits[0]
    if (audit.get("objective_digest") != objective["digest"] or audit.get("group_id") != group
            or audit.get("alignment") != "aligned"
            or not (audit.get("reviewer") or audit.get("evidence_source") or audit.get("source"))):
        raise ClosureProtocolError("Group audit is unaligned, unbound, or lacks its evidence source")
    audited_refs = audit.get("artifact_refs")
    if not isinstance(audited_refs, list) or not audited_refs:
        raise ClosureProtocolError("Group audit must identify required semantic artifacts")
    required_ids = []
    for audited_ref in audited_refs:
        if not isinstance(audited_ref, Mapping):
            raise ClosureProtocolError("Group audit artifact reference is malformed")
        matches = [key for key, value in state["artifacts"].items() if value["object_ref"] == audited_ref]
        if len(matches) != 1 or audited_ref["digest"] not in {item["digest"] for item in evidence}:
            raise ClosureProtocolError("Group audit requires a missing or stale semantic artifact")
        artifact = state["artifacts"][matches[0]]
        artifact_epoch = state.get("epoch_contexts", {}).get(artifact.get("epoch_id"), {})
        if (artifact.get("status") != "available"
                or artifact_epoch.get("group_id") != group):
            raise ClosureProtocolError("Group audit artifact is unavailable or belongs to another Group")
        required_ids.append(matches[0])
    if len(set(required_ids)) != len(required_ids):
        raise ClosureProtocolError("Group audit required artifacts must be unique")
    decisions = copy.deepcopy(list(accepted_decisions))
    events = []
    for decision in decisions:
        ref = decision.get("object_ref", decision)
        loaded = kernel.read_object(ref)
        if loaded["digest"] not in state["object_refs"]:
            raise ClosureProtocolError("decision is not in the current Run")
        if identity["mode"] == "rehearsal":
            events.append({"id": decision.get("id", loaded["digest"]), "digest": loaded["digest"], "kind": "assumption", "approved": False})
        else:
            # The caller must supply a real approval receipt as the decision
            # object. A plain proposal cannot become an approved decision.
            if loaded["object_type"] != "objective-approval" or loaded["payload"]["receipt"].get("source") != "human":
                raise ClosureProtocolError("accepted decision lacks a real approval receipt")
            events.append({"id": decision.get("id", loaded["digest"]), "digest": loaded["digest"], "kind": "decision", "approved": True})
    if state["epoch"]["status"] == "closed":
        return _resume_accepted_epoch(kernel, state, initial_head, identity, approval_ref, evidence, events, next_group)
    protocol = SharedClosureProtocolV1()
    source_authority = {"approved": True, "expected_head": initial_head}
    root = kernel.run_dir / "runtime-closure"
    results = []
    purpose = {"objective": {"status": "accepted", "digest": objective["digest"]}, "subobjective": {"objective_digest": objective["digest"], "group": group}, "group_result": {"objective_digest": objective["digest"], "outcome": "aligned", "evidence_refs": evidence}}
    results.append(protocol.audit_group_purpose("group.F.F1", purpose, source_authority, initial_head))
    inventory = [{"id": key, "digest": value["digest"], "classification": "canonical" if value.get("status") == "available" else "unverified"} for key, value in sorted(state["artifacts"].items())]
    results.append(protocol.collect_group_artifacts("group.F.F2", {"artifacts": inventory, "required_artifact_ids": required_ids}, source_authority, initial_head))
    results.append(protocol.extract_decision_candidates("group.F.F3", {"events": events}, source_authority, initial_head))
    history = [item["group"]["id"] for item in state["metadata"].get("operational_group_history", [])]
    if history != list(GROUPS[:GROUPS.index(group)]):
        raise ClosureProtocolError("runtime Group history is incomplete or out of order")
    dag = {"schema": "artifact-task-dag/v1", "expected_head": initial_head, "nodes": [{"id": item, "order": index} for index, item in enumerate(GROUPS)], "edges": [{"from": left, "to": right} for left, right in zip(GROUPS, GROUPS[1:])], "accepted_history": history}
    dag["digest"] = "sha256:" + hashlib.sha256(json.dumps(dag, sort_keys=True, separators=(",", ":"), ensure_ascii=True).encode()).hexdigest()
    result_ref = _snapshot(root, "group-result", {"objective_ref": objective, "group": group, "evidence_refs": evidence, "runtime_identity": identity})
    # This Group remains on the frontier until F6/F7 commit.  Including it
    # in accepted history here would fabricate a pre-close acceptance.
    plan = {"accepted_history": history, "future_frontier": list(GROUPS[GROUPS.index(group):])}
    results.append(protocol.replan_future("group.F.F4", {"accepted_plan": plan, "proposed_plan": plan, "group_result": result_ref, "dag": dag}, source_authority, initial_head))
    publish_authority = {"approved": True, "scopes": ["publish_artifact"], "human_receipt": approval_ref["digest"]}
    closure_refs = []
    owned_head = initial_head
    def publish(result: dict) -> dict:
        nonlocal owned_head
        artifact_id = "runtime-closure-%s-%s-%s" % (group, state["epoch"]["id"], result["selector"].split(".")[-1])
        command = kernel.make_command("publish_artifact", {"artifact_id": artifact_id, "version": "v1", "value": {"operation": result, "runtime_identity": identity, "objective_approval_ref": approval_ref, "evidence_refs": evidence}, "kind": "runtime-closure-operation", "path": None}, authority_ref=publish_authority, expected_head=owned_head)
        new_state = kernel.apply(command)
        owned_head = {"revision": kernel.last_receipt["revision"], "transaction_digest": kernel.last_receipt["transaction_digest"]}
        ref = new_state["artifacts"][artifact_id]["object_ref"]
        closure_refs.append({"selector": result["selector"], "digest": ref["digest"]})
        return ref
    for result in results:
        publish(result)
    current, head = _current(kernel)
    if head != owned_head:
        raise ClosureProtocolError("runtime closure state changed outside this adapter")
    workflow = _snapshot(root, "workflow", {"version": current["workflow_version"], "groups": list(GROUPS), "runtime_identity": identity}, id=current["workflow_version"])
    snapshot = _snapshot(root, "dag-state", {"state": current, "head": head}, revision=head["revision"])
    checkpoint = _snapshot(root, "checkpoint", {"schema": "runtime-preclosure-snapshot/v1", "phase": "before-close", "state_digest": snapshot["digest"], "expected_head": head}, state_digest=snapshot["digest"], expected_head=head)
    inputs = []
    for index, ref in enumerate(evidence):
        inputs.append(_snapshot(root, "artifact", {"canonical_ref": ref, "canonical_object": kernel.read_object(ref)}, id="evidence-%d" % index))
    bindings = {"workflow_digest": workflow["digest"], "state_digest": snapshot["digest"], "checkpoint_digest": checkpoint["digest"], "input_digests": [ref["digest"] for ref in inputs], "clear_boundary": True, "expected_head": head, "next_group": next_group}
    future = _snapshot(root, "future-plan", {**bindings, "plan": plan, "clear_boundary_requirement": "F6 and F7 must succeed before next Group"}, id="after-" + group, **bindings)
    f5 = protocol.advise_next_group("group.F.F5", {"workflow_ref": workflow, "state_ref": snapshot, "checkpoint_ref": checkpoint, "input_refs": inputs, "clear_boundary": True, "expected_head": head, "future_plan_ref": future, "next_group": next_group}, {"approved": True, "expected_head": head}, head)
    publish(f5)
    def command_authority(bound_head: dict) -> dict:
        return {"approved": True, "scopes": ["source", "close_epoch", "close_group"], "write_scopes": ["source"], "protected_fields": ["head", "group", "epoch", "ready"], "human_receipt": approval_ref["digest"], "expected_head": bound_head, "assignment_id": "shared-closure-protocol", "run_id": current["run_id"], "workflow_version": current["workflow_version"]}
    head = owned_head
    common = {"run_id": current["run_id"], "workflow_version": current["workflow_version"], "group_id": group}
    f6 = protocol.write_checkpoint("group.F.F6", {**common, "closure_refs": closure_refs, "idempotency_key": "runtime-F6-" + state["epoch"]["id"]}, command_authority(head), head)
    epoch_state = kernel.apply(f6["result"]["command"])
    epoch_receipt = copy.deepcopy(kernel.last_receipt)
    head = {"revision": epoch_receipt["revision"], "transaction_digest": epoch_receipt["transaction_digest"]}
    # The canonical closed Epoch bundle is the immutable acceptance object;
    # the transaction receipt proves which successful command produced it.
    epoch_ref = epoch_state["epoch"]["bundle_ref"]
    kernel.read_object(epoch_ref)
    f7 = protocol.clear_boundary("group.F.F7", {**common, "idempotency_key": "runtime-F7-" + state["epoch"]["id"], "checkpoint_ref": {"digest": epoch_state["epoch"]["checkpoint_ref"]["digest"]}, "close_receipt": {"status": "accepted", "head": head, "clear_before_next": epoch_state["epoch"]["clear_before_next"], "digest": epoch_ref["digest"]}}, command_authority(head), head)
    f7["result"]["command"]["payload"]["next_group"] = next_group
    final = kernel.apply(f7["result"]["command"])
    report = {"schema": "runtime-group-closure/v1", "runtime_identity": identity, "group": group, "next_group": next_group, "objective_ref": objective, "objective_approval_ref": approval_ref, "evidence_refs": evidence, "decision_events": events, "closure_refs": closure_refs, "f6": f6, "f7": f7, "epoch_close_transaction": epoch_receipt, "group_close_transaction": copy.deepcopy(kernel.last_receipt), "closure_bundle_ref": final["group"]["bundle_ref"], "checkpoint_ref": final["group"]["checkpoint_ref"]}
    report_ref = _snapshot(root, "runtime-group-closure", report)
    return {"state": final, "closure_bundle_ref": final["group"]["bundle_ref"], "checkpoint_ref": final["group"]["checkpoint_ref"], "epoch_close_receipt_ref": epoch_ref, "group_close_receipt_ref": final["group"]["bundle_ref"], "closure_report_ref": report_ref, "evidence_root": str(root)}


def _resume_accepted_epoch(kernel, state, current_head, identity, approval_ref, evidence, events, next_group):
    """Resume F7 only from HEAD-reachable F6 acceptance, or rebuild its report.

    Partial F1--F5 publication is intentionally not resumed by this path.
    """
    group, epoch = state["group"]["id"], state["epoch"]["id"]
    with kernel._lock():
        f6_tx = kernel._find_transaction_for_command("runtime-F6-" + epoch, current_head)
        f7_tx = kernel._find_transaction_for_command("runtime-F7-" + epoch, current_head)
    terminal = f7_tx if state["group"]["status"] == "closed" else f6_tx
    if not f6_tx or not terminal or current_head != {"revision": terminal["revision"], "transaction_digest": terminal["digest"]}:
        raise ClosureProtocolError("closure recovery requires the accepted F6 or F7 transaction at HEAD")
    epoch_state = f6_tx["state"]
    if epoch_state["epoch"] != state["epoch"]:
        raise ClosureProtocolError("closure recovery Epoch differs from accepted F6")
    refs, operations = [], []
    for index in range(1, 6):
        selector = "group.F.F%d" % index
        artifact = state["artifacts"].get("runtime-closure-%s-%s-F%d" % (group, epoch, index))
        if not artifact or artifact.get("epoch_id") != epoch:
            raise ClosureProtocolError("accepted closure operation is missing")
        loaded = kernel.read_object(artifact["object_ref"])["payload"]
        value = loaded.get("payload", {})
        operation = value.get("operation", {})
        if (loaded.get("kind") != "runtime-closure-operation" or value.get("runtime_identity") != identity
                or value.get("objective_approval_ref") != approval_ref or value.get("evidence_refs") != evidence
                or operation.get("selector") != selector or operation.get("status") != "compiled"):
            raise ClosureProtocolError("closure retry changes accepted evidence or operation binding")
        refs.append({"selector": selector, "digest": artifact["object_ref"]["digest"]})
        operations.append(operation)
    expected_events = {"decision_candidates": [event for event in events if event["approved"]],
                       "unapproved_items": [event for event in events if not event["approved"]]}
    if (operations[2]["result"] != expected_events
            or operations[2]["input_refs"] != [{"kind": "event", "digest": event["digest"]} for event in events]
            or operations[4]["result"]["advice"]["next_group"] != next_group):
        raise ClosureProtocolError("closure retry changes decisions or next Group")
    command = f6_tx["command"]
    if (command["command_type"] != "close_epoch"
            or command["actor"] != {"role": "orchestrator", "assignment_id": "shared-closure-protocol"}
            or command["input_refs"] != [{"kind": ref["selector"], "digest": ref["digest"]} for ref in refs]
            or command["payload"]["acceptance_evidence"] != [ref["digest"] for ref in refs]
            or command["authority_ref"].get("human_receipt") != approval_ref["digest"]):
        raise ClosureProtocolError("accepted F6 does not bind this closure")
    f6 = {"schema": "closure-operation-result/v1", "selector": "group.F.F6", "status": "compiled",
          "expected_head": command["expected_head"], "input_refs": command["input_refs"], "result": {"command": command}}
    head = {"revision": f6_tx["revision"], "transaction_digest": f6_tx["digest"]}
    authority = {**command["authority_ref"], "expected_head": head}
    epoch_ref = epoch_state["epoch"]["bundle_ref"]
    f7 = SharedClosureProtocolV1().clear_boundary("group.F.F7", {
        "run_id": state["run_id"], "workflow_version": state["workflow_version"], "group_id": group,
        "idempotency_key": "runtime-F7-" + epoch,
        "checkpoint_ref": {"digest": epoch_state["epoch"]["checkpoint_ref"]["digest"]},
        "close_receipt": {"status": "accepted", "head": head, "clear_before_next": epoch_state["epoch"]["clear_before_next"], "digest": epoch_ref["digest"]},
    }, authority, head)
    f7["result"]["command"]["payload"]["next_group"] = next_group
    def receipt(transaction):
        return {"duplicate": False, "transaction_digest": transaction["digest"],
                "revision": transaction["revision"], "command_id": transaction["command"]["command_id"]}
    if f7_tx:
        if f7_tx["command"] != f7["result"]["command"]:
            raise ClosureProtocolError("accepted F7 differs from the requested closure")
        final, group_receipt = state, receipt(f7_tx)
    else:
        final = kernel.apply(f7["result"]["command"])
        group_receipt = copy.deepcopy(kernel.last_receipt)
    root = kernel.run_dir / "runtime-closure"
    report = {"schema": "runtime-group-closure/v1", "runtime_identity": identity, "group": group, "next_group": next_group,
              "objective_ref": state["objective_ref"], "objective_approval_ref": approval_ref, "evidence_refs": evidence,
              "decision_events": events, "closure_refs": refs, "f6": f6, "f7": f7, "epoch_close_transaction": receipt(f6_tx),
              "group_close_transaction": group_receipt, "closure_bundle_ref": final["group"]["bundle_ref"], "checkpoint_ref": final["group"]["checkpoint_ref"]}
    report_ref = _snapshot(root, "runtime-group-closure", report)
    return {"state": final, "closure_bundle_ref": final["group"]["bundle_ref"], "checkpoint_ref": final["group"]["checkpoint_ref"],
            "epoch_close_receipt_ref": epoch_ref, "group_close_receipt_ref": final["group"]["bundle_ref"],
            "closure_report_ref": report_ref, "evidence_root": str(root)}


__all__ = [
    "GROUPS",
    "close_runtime_group",
    "close_workflow_loop",
    "record_workflow_loop_outcome",
]
