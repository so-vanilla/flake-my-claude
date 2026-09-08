"""Pure, digest-bound compilation for the shared F1--F8 closure protocol."""
from __future__ import annotations

import copy
import hashlib
import json
import re
from pathlib import Path
from typing import Any, Dict, Mapping

from .control_kernel import IntegrityBlockedError, StaleHeadError
from .schema_validation import SchemaValidationError, validate_document
from .section_control_plane import SectionContractError, resume as _s0_resume

_DIGEST = re.compile(r"^sha256:[0-9a-f]{64}$")
_SCHEMA_DIRECTORY = Path(__file__).resolve().parents[2] / "schemas"


class ClosureProtocolError(ValueError):
    """A shared-closure request cannot safely be compiled."""


def _schema(name: str) -> Mapping[str, Any]:
    with (_SCHEMA_DIRECTORY / name).open(encoding="utf-8") as source:
        return json.load(source)


def _head(value: Any) -> Dict[str, Any]:
    if (not isinstance(value, Mapping) or set(value) != {"revision", "transaction_digest"}
            or not isinstance(value.get("revision"), int) or isinstance(value.get("revision"), bool)
            or value["revision"] < 0 or not _DIGEST.fullmatch(str(value.get("transaction_digest")))):
        raise ClosureProtocolError("expected_head is malformed")
    return copy.deepcopy(dict(value))


def _authority(authority: Any, expected_head: Mapping[str, Any]) -> None:
    if not isinstance(authority, Mapping) or authority.get("approved") is not True:
        raise ClosureProtocolError("approved source authority is required")
    if authority.get("expected_head") != expected_head:
        raise ClosureProtocolError("authority must bind expected_head")


def _command_authority(
    authority: Any,
    expected_head: Mapping[str, Any],
    inputs: Mapping[str, Any],
    operation: str,
) -> Dict[str, Any]:
    required = {
        "approved", "scopes", "write_scopes", "protected_fields", "human_receipt", "expected_head",
        "assignment_id", "run_id", "workflow_version",
    }
    protected = ["head", "group", "epoch", "ready"]
    if not isinstance(authority, Mapping) or set(authority) != required:
        raise ClosureProtocolError("F6/F7 command authority is malformed")
    _authority(authority, expected_head)
    scopes = authority.get("scopes")
    if (not isinstance(scopes, list) or any(not isinstance(item, str) or not item for item in scopes)
            or len(scopes) != len(set(scopes)) or not {"source", operation}.issubset(set(scopes))):
        raise ClosureProtocolError("F6/F7 command authority scopes are incomplete")
    if authority.get("write_scopes") != ["source"]:
        raise ClosureProtocolError("F6/F7 command authority write scope is incomplete")
    if authority.get("protected_fields") != protected:
        raise ClosureProtocolError("F6/F7 protected fields must bind the Kernel command")
    if not isinstance(authority.get("human_receipt"), str) or not authority["human_receipt"]:
        raise ClosureProtocolError("F6/F7 protected fields require a human receipt")
    if authority.get("assignment_id") != "shared-closure-protocol":
        raise ClosureProtocolError("F6/F7 authority assignment must bind the command actor")
    if authority.get("run_id") != inputs.get("run_id"):
        raise ClosureProtocolError("F6/F7 authority must bind the command Run")
    if authority.get("workflow_version") != inputs.get("workflow_version"):
        raise ClosureProtocolError("F6/F7 authority must bind the workflow version")
    return copy.deepcopy(dict(authority))


def _canonical_digest(value: Mapping[str, Any]) -> str:
    encoded = json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=True).encode("utf-8")
    return "sha256:" + hashlib.sha256(encoded).hexdigest()


def _physical_object_ref(value: Any, *, object_type: str, require_id: bool) -> Dict[str, Any]:
    base = {"digest", "object_type", "path"} | ({"id"} if require_id else set())
    if not isinstance(value, Mapping) or not base.issubset(value):
        raise ClosureProtocolError("next-group advice requires exact physical references")
    digest = value.get("digest")
    if not _DIGEST.fullmatch(str(digest)) or value.get("object_type") != object_type:
        raise ClosureProtocolError("next-group advice physical reference is malformed")
    if require_id and (not isinstance(value.get("id"), str) or not value["id"]):
        raise ClosureProtocolError("next-group advice physical identity is malformed")
    if value.get("path") != "objects/%s.json" % str(digest).removeprefix("sha256:"):
        raise ClosureProtocolError("next-group advice physical reference path is malformed")
    return copy.deepcopy(dict(value))


def _closure_refs(value: Any, selectors: set[str]) -> list[Dict[str, str]]:
    if not isinstance(value, list) or len(value) != len(selectors):
        raise ClosureProtocolError("F1-F5 closure references must be complete")
    refs, seen = [], set()
    for item in value:
        if (not isinstance(item, Mapping) or set(item) != {"selector", "digest"}
                or item.get("selector") not in selectors or item["selector"] in seen
                or not _DIGEST.fullmatch(str(item.get("digest")))):
            raise ClosureProtocolError("closure reference is malformed")
        seen.add(item["selector"])
        refs.append(copy.deepcopy(dict(item)))
    if seen != selectors:
        raise ClosureProtocolError("F1-F5 closure references must be complete")
    return refs


def _validated_result(document: Dict[str, Any], label: str) -> Dict[str, Any]:
    try:
        validate_document(document, _schema("closure-operation-result-v1.schema.json"))
    except SchemaValidationError as error:
        raise ClosureProtocolError("%s result schema rejection: %s" % (label, error)) from error
    return document


class SharedClosureProtocolV1:
    """Stateless compiler; it never owns or mutates workflow state."""

    def audit_group_purpose(
        self, qualified_id: str, inputs: Mapping[str, Any], authority: Mapping[str, Any], expected_head: Mapping[str, Any]
    ) -> Dict[str, Any]:
        if qualified_id != "group.F.F1":
            raise ClosureProtocolError("F1 requires qualified selector group.F.F1")
        head = _head(expected_head)
        _authority(authority, head)
        if not isinstance(inputs, Mapping):
            raise ClosureProtocolError("F1 inputs are malformed")
        objective = inputs.get("objective")
        subobjective = inputs.get("subobjective")
        group_result = inputs.get("group_result")
        if not isinstance(objective, Mapping) or objective.get("status") != "accepted" or not _DIGEST.fullmatch(str(objective.get("digest"))):
            raise ClosureProtocolError("objective requires accepted digest-bound evidence")
        if not isinstance(subobjective, Mapping) or subobjective.get("objective_digest") != objective["digest"]:
            raise ClosureProtocolError("subobjective must bind objective evidence")
        if not isinstance(group_result, Mapping) or group_result.get("objective_digest") != objective["digest"]:
            raise ClosureProtocolError("group result must bind objective evidence")
        if group_result.get("outcome") == "diverged":
            raise ClosureProtocolError("diverged objective requires replan or user stop")
        if group_result.get("outcome") not in {"aligned", "uncertain"}:
            raise ClosureProtocolError("group result outcome is malformed")
        document = {
            "schema": "closure-operation-result/v1", "selector": qualified_id, "status": "compiled",
            "expected_head": head, "input_refs": [{"kind": "objective", "digest": objective["digest"]}],
            "result": {"alignment": group_result["outcome"]},
        }
        try:
            validate_document(document, _schema("closure-operation-result-v1.schema.json"))
        except SchemaValidationError as error:
            raise ClosureProtocolError("F1 result schema rejection: %s" % error) from error
        return document

    def collect_group_artifacts(
        self, qualified_id: str, inputs: Mapping[str, Any], authority: Mapping[str, Any], expected_head: Mapping[str, Any]
    ) -> Dict[str, Any]:
        if qualified_id != "group.F.F2":
            raise ClosureProtocolError("F2 requires qualified selector group.F.F2")
        head = _head(expected_head)
        _authority(authority, head)
        if not isinstance(inputs, Mapping) or not isinstance(inputs.get("artifacts"), list) or not isinstance(inputs.get("required_artifact_ids"), list):
            raise ClosureProtocolError("F2 artifact inputs are malformed")
        artifacts = copy.deepcopy(inputs["artifacts"])
        required = inputs["required_artifact_ids"]
        seen = set()
        for artifact in artifacts:
            if (not isinstance(artifact, Mapping) or set(artifact) != {"id", "digest", "classification"}
                    or not isinstance(artifact.get("id"), str) or not artifact["id"]
                    or artifact["id"] in seen or not _DIGEST.fullmatch(str(artifact.get("digest")))
                    or artifact.get("classification") not in {"canonical", "partial", "unverified"}):
                raise ClosureProtocolError("artifact inventory entry is malformed")
            seen.add(artifact["id"])
        if not all(isinstance(item, str) and item for item in required) or len(required) != len(set(required)):
            raise ClosureProtocolError("required artifact identifiers are malformed")
        missing = sorted(set(required) - seen)
        if missing:
            raise ClosureProtocolError("required artifact is missing: %s" % ", ".join(missing))
        document = {
            "schema": "closure-operation-result/v1", "selector": qualified_id, "status": "compiled",
            "expected_head": head,
            "input_refs": [{"kind": "artifact", "digest": artifact["digest"]} for artifact in artifacts],
            "result": {"artifact_inventory": artifacts},
        }
        try:
            validate_document(document, _schema("closure-operation-result-v1.schema.json"))
        except SchemaValidationError as error:
            raise ClosureProtocolError("F2 result schema rejection: %s" % error) from error
        return document

    def extract_decision_candidates(
        self, qualified_id: str, inputs: Mapping[str, Any], authority: Mapping[str, Any], expected_head: Mapping[str, Any]
    ) -> Dict[str, Any]:
        if qualified_id != "group.F.F3":
            raise ClosureProtocolError("F3 requires qualified selector group.F.F3")
        head = _head(expected_head)
        _authority(authority, head)
        if not isinstance(inputs, Mapping) or not isinstance(inputs.get("events"), list):
            raise ClosureProtocolError("F3 events are malformed")
        if inputs.get("promote_unapproved") is True:
            raise ClosureProtocolError("automatic promotion of unapproved material is forbidden")
        events = copy.deepcopy(inputs["events"])
        decisions, unapproved, seen = [], [], set()
        for event in events:
            if (not isinstance(event, Mapping) or set(event) != {"id", "digest", "kind", "approved"}
                    or not isinstance(event.get("id"), str) or not event["id"] or event["id"] in seen
                    or not _DIGEST.fullmatch(str(event.get("digest"))) or not isinstance(event.get("approved"), bool)):
                raise ClosureProtocolError("decision event is malformed")
            seen.add(event["id"])
            if event["kind"] == "decision" and event["approved"] is True:
                decisions.append(event)
            elif event["kind"] in {"proposal", "observation", "assumption"} and event["approved"] is False:
                unapproved.append(event)
            else:
                raise ClosureProtocolError("decision event has an unsafe approval state")
        document = {
            "schema": "closure-operation-result/v1", "selector": qualified_id, "status": "compiled",
            "expected_head": head,
            "input_refs": [{"kind": "event", "digest": event["digest"]} for event in events],
            "result": {"decision_candidates": decisions, "unapproved_items": unapproved},
        }
        try:
            validate_document(document, _schema("closure-operation-result-v1.schema.json"))
        except SchemaValidationError as error:
            raise ClosureProtocolError("F3 result schema rejection: %s" % error) from error
        return document

    def replan_future(
        self, qualified_id: str, inputs: Mapping[str, Any], authority: Mapping[str, Any], expected_head: Mapping[str, Any]
    ) -> Dict[str, Any]:
        if qualified_id != "group.F.F4":
            raise ClosureProtocolError("F4 requires qualified selector group.F.F4")
        head = _head(expected_head)
        _authority(authority, head)
        if (not isinstance(inputs, Mapping)
                or set(inputs) != {"accepted_plan", "proposed_plan", "group_result", "dag"}
                or not isinstance(inputs.get("accepted_plan"), Mapping)
                or not isinstance(inputs.get("proposed_plan"), Mapping)):
            raise ClosureProtocolError("F4 plans are malformed")
        accepted, proposed = inputs["accepted_plan"], inputs["proposed_plan"]
        if set(accepted) != set(proposed) or not {"future_frontier", "accepted_history"}.issubset(accepted):
            raise ClosureProtocolError("future frontier is the only permitted plan revision")
        if any(accepted[key] != proposed[key] for key in accepted if key != "future_frontier"):
            raise ClosureProtocolError("future frontier is the only permitted plan revision")
        frontier = proposed["future_frontier"]
        if not isinstance(frontier, list) or not frontier or not all(isinstance(item, str) and item for item in frontier) or len(frontier) != len(set(frontier)):
            raise ClosureProtocolError("future frontier is malformed")
        group_result = inputs.get("group_result")
        if not isinstance(group_result, Mapping) or not _DIGEST.fullmatch(str(group_result.get("digest"))):
            raise ClosureProtocolError("group result requires a digest-bound reference")
        dag = inputs.get("dag")
        dag_keys = {"schema", "digest", "expected_head", "nodes", "edges", "accepted_history"}
        if not isinstance(dag, Mapping) or set(dag) != dag_keys or dag.get("schema") != "artifact-task-dag/v1":
            raise ClosureProtocolError("F4 requires a strict digest-bound DAG")
        if dag.get("expected_head") != head:
            raise ClosureProtocolError("F4 DAG expected HEAD is stale")
        digest_payload = copy.deepcopy(dict(dag))
        claimed_digest = digest_payload.pop("digest")
        if not _DIGEST.fullmatch(str(claimed_digest)) or claimed_digest != _canonical_digest(digest_payload):
            raise ClosureProtocolError("F4 DAG digest does not bind its graph")
        nodes = dag.get("nodes")
        if not isinstance(nodes, list) or not nodes:
            raise ClosureProtocolError("F4 DAG nodes are malformed")
        order_by_id: Dict[str, int] = {}
        for node in nodes:
            if (not isinstance(node, Mapping) or set(node) != {"id", "order"}
                    or not isinstance(node.get("id"), str) or not node["id"] or node["id"] in order_by_id
                    or not isinstance(node.get("order"), int) or isinstance(node.get("order"), bool) or node["order"] < 0
                    or node["order"] in order_by_id.values()):
                raise ClosureProtocolError("F4 DAG nodes are malformed")
            order_by_id[node["id"]] = node["order"]
        edges = dag.get("edges")
        if not isinstance(edges, list):
            raise ClosureProtocolError("F4 DAG edges are malformed")
        incoming: Dict[str, set[str]] = {node_id: set() for node_id in order_by_id}
        seen_edges = set()
        for edge in edges:
            if (not isinstance(edge, Mapping) or set(edge) != {"from", "to"}
                    or edge.get("from") not in order_by_id or edge.get("to") not in order_by_id
                    or edge["from"] == edge["to"] or (edge["from"], edge["to"]) in seen_edges
                    or order_by_id[edge["from"]] >= order_by_id[edge["to"]]):
                raise ClosureProtocolError("F4 DAG order is invalid")
            seen_edges.add((edge["from"], edge["to"]))
            incoming[edge["to"]].add(edge["from"])
        history = dag.get("accepted_history")
        if (not isinstance(history, list) or any(not isinstance(item, str) or item not in order_by_id for item in history)
                or len(history) != len(set(history)) or history != accepted.get("accepted_history")):
            raise ClosureProtocolError("F4 accepted history is malformed or stale")
        if history != sorted(history, key=order_by_id.__getitem__):
            raise ClosureProtocolError("F4 accepted history order is invalid")
        if any(item not in order_by_id or item in history for item in frontier):
            raise ClosureProtocolError("F4 future frontier is absent from the DAG or already accepted")
        if frontier != sorted(frontier, key=order_by_id.__getitem__):
            raise ClosureProtocolError("F4 future frontier order is invalid")
        available = set(history)
        for node_id in frontier:
            if not incoming[node_id].issubset(available):
                raise ClosureProtocolError("F4 future frontier bypasses DAG prerequisites")
            available.add(node_id)
        document = {
            "schema": "closure-operation-result/v1", "selector": qualified_id, "status": "compiled",
            "expected_head": head,
            "input_refs": [
                {"kind": "group_result", "digest": group_result["digest"]},
                {"kind": "artifact_task_dag", "digest": claimed_digest},
            ],
            "result": {"future_frontier": copy.deepcopy(frontier)},
        }
        try:
            validate_document(document, _schema("closure-operation-result-v1.schema.json"))
        except SchemaValidationError as error:
            raise ClosureProtocolError("F4 result schema rejection: %s" % error) from error
        return document

    def advise_next_group(
        self, qualified_id: str, inputs: Mapping[str, Any], authority: Mapping[str, Any], expected_head: Mapping[str, Any]
    ) -> Dict[str, Any]:
        if qualified_id != "group.F.F5":
            raise ClosureProtocolError("F5 requires qualified selector group.F.F5")
        head = _head(expected_head)
        _authority(authority, head)
        required = {
            "workflow_ref", "state_ref", "checkpoint_ref", "input_refs", "clear_boundary",
            "expected_head", "future_plan_ref", "next_group",
        }
        if not isinstance(inputs, Mapping) or set(inputs) != required:
            raise ClosureProtocolError("next-group advice requires physical prerequisites")
        workflow = _physical_object_ref(inputs.get("workflow_ref"), object_type="workflow", require_id=True)
        if set(workflow) != {"id", "digest", "object_type", "path"}:
            raise ClosureProtocolError("next-group workflow reference is malformed")
        state = _physical_object_ref(inputs.get("state_ref"), object_type="dag-state", require_id=False)
        if set(state) != {"revision", "digest", "object_type", "path"} or state.get("revision") != head["revision"]:
            raise ClosureProtocolError("next-group state reference is stale")
        checkpoint = _physical_object_ref(inputs.get("checkpoint_ref"), object_type="checkpoint", require_id=False)
        if (set(checkpoint) != {"digest", "object_type", "path", "state_digest", "expected_head"}
                or checkpoint.get("state_digest") != state["digest"] or checkpoint.get("expected_head") != head):
            raise ClosureProtocolError("next-group checkpoint reference is stale or unbound")
        input_refs = inputs.get("input_refs")
        if not isinstance(input_refs, list) or not input_refs:
            raise ClosureProtocolError("next-group input references are required")
        physical_inputs = []
        for item in input_refs:
            ref = _physical_object_ref(item, object_type="artifact", require_id=True)
            if set(ref) != {"id", "digest", "object_type", "path"}:
                raise ClosureProtocolError("next-group input reference is malformed")
            physical_inputs.append(ref)
        if len({item["id"] for item in physical_inputs}) != len(physical_inputs) or len({item["digest"] for item in physical_inputs}) != len(physical_inputs):
            raise ClosureProtocolError("next-group input references must be unique")
        if inputs.get("clear_boundary") is not True or inputs.get("expected_head") != head:
            raise ClosureProtocolError("next-group advice requires a fresh clear boundary and HEAD")
        if not isinstance(inputs.get("next_group"), str) or not inputs["next_group"]:
            raise ClosureProtocolError("next-group identity is malformed")
        future = _physical_object_ref(inputs.get("future_plan_ref"), object_type="future-plan", require_id=True)
        future_keys = {
            "id", "digest", "object_type", "path", "workflow_digest", "state_digest",
            "checkpoint_digest", "input_digests", "clear_boundary", "expected_head", "next_group",
        }
        if set(future) != future_keys:
            raise ClosureProtocolError("future plan reference is malformed")
        if (future.get("workflow_digest") != workflow["digest"]
                or future.get("state_digest") != state["digest"]
                or future.get("checkpoint_digest") != checkpoint["digest"]
                or future.get("input_digests") != [item["digest"] for item in physical_inputs]
                or future.get("clear_boundary") is not True or future.get("expected_head") != head
                or future.get("next_group") != inputs["next_group"]):
            raise ClosureProtocolError("next-group advice is not cross-bound to its future plan or next group")
        all_digests = [workflow["digest"], state["digest"], checkpoint["digest"], future["digest"]] + [item["digest"] for item in physical_inputs]
        if len(all_digests) != len(set(all_digests)):
            raise ClosureProtocolError("next-group advice reuses unrelated physical identities")
        advice = {"schema": "next-group-advice/v1", **copy.deepcopy(dict(inputs))}
        try:
            validate_document(advice, _schema("next-group-advice-v1.schema.json"))
        except SchemaValidationError as error:
            raise ClosureProtocolError("next-group advice schema rejection: %s" % error) from error
        document = {
            "schema": "closure-operation-result/v1", "selector": qualified_id, "status": "compiled", "expected_head": head,
            "input_refs": [{"kind": "workflow", "digest": advice["workflow_ref"]["digest"]}, {"kind": "state", "digest": advice["state_ref"]["digest"]}, {"kind": "checkpoint", "digest": advice["checkpoint_ref"]["digest"]}] + [{"kind": "input", "digest": item["digest"]} for item in advice["input_refs"]],
            "result": {"advice": advice},
        }
        try:
            validate_document(document, _schema("closure-operation-result-v1.schema.json"))
        except SchemaValidationError as error:
            raise ClosureProtocolError("F5 result schema rejection: %s" % error) from error
        return document

    def write_checkpoint(
        self, qualified_id: str, inputs: Mapping[str, Any], authority: Mapping[str, Any], expected_head: Mapping[str, Any]
    ) -> Dict[str, Any]:
        if qualified_id != "group.F.F6":
            raise ClosureProtocolError("F6 requires qualified selector group.F.F6")
        head = _head(expected_head)
        _authority(authority, head)
        if not isinstance(inputs, Mapping) or not all(isinstance(inputs.get(key), str) and inputs[key] for key in ("run_id", "workflow_version", "group_id", "idempotency_key")):
            raise ClosureProtocolError("checkpoint command identity is malformed")
        command_authority = _command_authority(authority, head, inputs, "close_epoch")
        refs = _closure_refs(inputs.get("closure_refs"), {"group.F.F1", "group.F.F2", "group.F.F3", "group.F.F4", "group.F.F5"})
        command = {
            "schema": "dag-command/v1", "command_id": "closure-close-epoch-" + inputs["group_id"], "command_type": "close_epoch",
            "run_id": inputs["run_id"], "expected_head": head, "workflow_version": inputs["workflow_version"], "graph_version": "artifact-task-dag/v1",
            "actor": {"role": "orchestrator", "assignment_id": "shared-closure-protocol"}, "authority_ref": command_authority,
            "input_refs": [{"kind": item["selector"], "digest": item["digest"]} for item in refs], "idempotency_key": inputs["idempotency_key"],
            "protected_fields": ["head", "group", "epoch", "ready"], "scope": ["source"],
            "payload": {"acceptance_evidence": [item["digest"] for item in refs], "approved_decisions": [], "unresolved_items": [], "invalidated_artifacts": [], "next_inputs": [], "context_budget": {"target": 200000, "normal_limit": 300000, "absolute_limit": 500000, "token_status": "unavailable", "token_count": None}, "clear_before_next": True, "boundary_reason": "shared-closure-checkpoint"},
        }
        try:
            validate_document(command, _schema("dag-command-v1.schema.json"))
        except SchemaValidationError as error:
            raise ClosureProtocolError("checkpoint command schema rejection: %s" % error) from error
        return _validated_result({"schema": "closure-operation-result/v1", "selector": qualified_id, "status": "compiled", "expected_head": head, "input_refs": [{"kind": item["selector"], "digest": item["digest"]} for item in refs], "result": {"command": command}}, "F6")

    def clear_boundary(
        self, qualified_id: str, inputs: Mapping[str, Any], authority: Mapping[str, Any], expected_head: Mapping[str, Any]
    ) -> Dict[str, Any]:
        if qualified_id != "group.F.F7":
            raise ClosureProtocolError("F7 requires qualified selector group.F.F7")
        head = _head(expected_head)
        _authority(authority, head)
        if not isinstance(inputs, Mapping) or not all(isinstance(inputs.get(key), str) and inputs[key] for key in ("run_id", "workflow_version", "group_id", "idempotency_key")):
            raise ClosureProtocolError("clear command identity is malformed")
        command_authority = _command_authority(authority, head, inputs, "close_group")
        receipt = inputs.get("close_receipt")
        checkpoint = inputs.get("checkpoint_ref")
        if (not isinstance(receipt, Mapping) or set(receipt) != {"status", "head", "clear_before_next", "digest"}
                or receipt.get("status") != "accepted" or receipt.get("clear_before_next") is not True
                or receipt.get("head") != head or not _DIGEST.fullmatch(str(receipt.get("digest")))):
            raise ClosureProtocolError("pre-close or stale close receipt is refused")
        if not isinstance(checkpoint, Mapping) or set(checkpoint) != {"digest"} or not _DIGEST.fullmatch(str(checkpoint.get("digest"))):
            raise ClosureProtocolError("clear requires a digest-bound checkpoint")
        command = {
            "schema": "dag-command/v1", "command_id": "closure-close-group-" + inputs["group_id"], "command_type": "close_group",
            "run_id": inputs["run_id"], "expected_head": head, "workflow_version": inputs["workflow_version"], "graph_version": "artifact-task-dag/v1",
            "actor": {"role": "orchestrator", "assignment_id": "shared-closure-protocol"}, "authority_ref": command_authority,
            "input_refs": [{"kind": "close_receipt", "digest": receipt["digest"]}, {"kind": "checkpoint", "digest": checkpoint["digest"]}], "idempotency_key": inputs["idempotency_key"],
            "protected_fields": ["head", "group", "epoch", "ready"], "scope": ["source"],
            "payload": {"acceptance_evidence": [receipt["digest"]], "approved_decisions": [], "unresolved_items": [], "invalidated_artifacts": [], "next_inputs": [], "next_group": None, "boundary_reason": "shared-closure-clear"},
        }
        try:
            validate_document(command, _schema("dag-command-v1.schema.json"))
        except SchemaValidationError as error:
            raise ClosureProtocolError("clear command schema rejection: %s" % error) from error
        return _validated_result({"schema": "closure-operation-result/v1", "selector": qualified_id, "status": "compiled", "expected_head": head, "input_refs": copy.deepcopy(command["input_refs"]), "result": {"command": command}}, "F7")

    def resume(
        self, qualified_id: str, inputs: Mapping[str, Any], authority: Mapping[str, Any], expected_head: Mapping[str, Any]
    ) -> Dict[str, Any]:
        if qualified_id != "group.F.F8":
            raise ClosureProtocolError("F8 requires qualified selector group.F.F8")
        head = _head(expected_head)
        _authority(authority, head)
        if not isinstance(inputs, Mapping) or not isinstance(inputs.get("identifier"), str) or not inputs["identifier"] or not hasattr(inputs.get("kernel"), "resume"):
            raise ClosureProtocolError("resume requires an existing S0 kernel reader and identifier")
        try:
            status = _s0_resume(inputs["kernel"], head)
        except (IntegrityBlockedError, StaleHeadError, SectionContractError, SchemaValidationError) as error:
            raise ClosureProtocolError("corrupt canonical resume chain: %s" % error) from error
        if status.get("head") != head:
            raise ClosureProtocolError("corrupt canonical resume head chain")
        return _validated_result({"schema": "closure-operation-result/v1", "selector": qualified_id, "status": "compiled", "expected_head": head, "input_refs": [], "result": {"resume": {"identifier": inputs["identifier"], "head": copy.deepcopy(head), "section_id": status["section_id"], "status": status["status"]}}}, "F8")

    def compile(
        self, operation: str, qualified_id: str, inputs: Mapping[str, Any], authority: Mapping[str, Any], expected_head: Mapping[str, Any]
    ) -> Dict[str, Any]:
        operations = {
            "audit_group_purpose": self.audit_group_purpose, "collect_group_artifacts": self.collect_group_artifacts,
            "extract_decision_candidates": self.extract_decision_candidates, "replan_future": self.replan_future,
            "advise_next_group": self.advise_next_group, "write_checkpoint": self.write_checkpoint,
            "clear_boundary": self.clear_boundary, "resume": self.resume,
        }
        if qualified_id not in {"group.F.F1", "group.F.F2", "group.F.F3", "group.F.F4", "group.F.F5", "group.F.F6", "group.F.F7", "group.F.F8"}:
            raise ClosureProtocolError("qualified group.F selector is required")
        method = operations.get(operation)
        if method is None:
            raise ClosureProtocolError("unknown shared closure operation")
        return method(qualified_id, inputs, authority, expected_head)


__all__ = ["ClosureProtocolError", "SharedClosureProtocolV1"]
