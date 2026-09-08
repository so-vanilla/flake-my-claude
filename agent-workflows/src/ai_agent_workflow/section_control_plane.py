"""Digest-bound section compiler and thin ControlKernel public adapters."""
from __future__ import annotations
import copy
import hashlib
import json
import re
from pathlib import Path
from typing import Any, Dict, Mapping, Protocol

from .schema_validation import SchemaValidationError, validate_document

_DIGEST = re.compile(r"^sha256:[0-9a-f]{64}$")
_TASK_ID = re.compile(r"^S[0-9]+\.[A-Za-z][A-Za-z0-9_-]*$")
_SECTION_ID = re.compile(r"^S([0-9]+)$")
_SCHEMA_DIRECTORY = Path(__file__).resolve().parents[2] / "schemas"

class SectionContractError(ValueError):
    """A source-only section-control-plane input is invalid."""

class SectionKernelProtocol(Protocol):
    def apply(self, command: Mapping[str, Any]) -> Dict[str, Any]: ...
    def status(self, *, expected_revision: int, expected_head_digest: str) -> Dict[str, Any]: ...
    def resume(self, *, expected_revision: int, expected_head_digest: str) -> Dict[str, Any]: ...

def _head_kwargs(value: Mapping[str, Any]) -> Dict[str, Any]:
    if not isinstance(value, Mapping) or set(value) != {"revision", "transaction_digest"}:
        raise SectionContractError("expected_head must bind revision and transaction_digest")
    revision, digest = value.get("revision"), value.get("transaction_digest")
    if not isinstance(revision, int) or isinstance(revision, bool) or revision < 0 or not _DIGEST.fullmatch(str(digest)):
        raise SectionContractError("expected_head is malformed")
    return {"expected_revision": revision, "expected_head_digest": digest}

def _section_position(section_id: Any) -> int:
    match = _SECTION_ID.fullmatch(str(section_id))
    if match is None:
        raise SectionContractError("section identity is malformed")
    return int(match.group(1))

def _history_ref(value: Any, *, object_type: str) -> None:
    if not isinstance(value, Mapping) or set(value) != {"digest", "object_type", "path"}:
        raise SectionContractError("section history requires an exact object reference")
    digest = value.get("digest")
    if not _DIGEST.fullmatch(str(digest)) or value.get("object_type") != object_type:
        raise SectionContractError("section history reference is malformed")
    if value.get("path") != "objects/%s.json" % str(digest).removeprefix("sha256:"):
        raise SectionContractError("section history reference path is malformed")

def _schema(name: str) -> Mapping[str, Any]:
    with (_SCHEMA_DIRECTORY / name).open(encoding="utf-8") as source:
        return json.load(source)

def _validate_history(history: Any, *, target_position: int, include_predecessor: bool) -> list[Dict[str, Any]]:
    expected_length = target_position if include_predecessor else target_position - 1
    if not isinstance(history, list) or len(history) != expected_length:
        raise SectionContractError("section history is not contiguous")
    result = []
    receipts = set()
    for position, item in enumerate(history):
        if not isinstance(item, Mapping) or set(item) != {"position", "section_id", "accepted_receipt_ref", "bundle_ref", "checkpoint_ref"}:
            raise SectionContractError("section history item is malformed")
        if item.get("position") != position or item.get("section_id") != "S%d" % position:
            raise SectionContractError("section history is not ordered")
        _history_ref(item.get("accepted_receipt_ref"), object_type="artifact")
        _history_ref(item.get("bundle_ref"), object_type="artifact-bundle")
        _history_ref(item.get("checkpoint_ref"), object_type="checkpoint")
        receipt = item["accepted_receipt_ref"]["digest"]
        if receipt in receipts:
            raise SectionContractError("section history reuses an accepted receipt")
        receipts.add(receipt)
        result.append(copy.deepcopy(dict(item)))
    return result

def open_section(kernel: SectionKernelProtocol, command: Mapping[str, Any]) -> Dict[str, Any]:
    """Delegate exactly one command; the kernel is the only state owner."""
    _validate_public_command(command, "open_section")
    return kernel.apply(command)

def open_group(kernel: SectionKernelProtocol, command: Mapping[str, Any]) -> Dict[str, Any]:
    """Delegate exactly one command; the kernel is the only state owner."""
    _validate_public_command(command, "open_group")
    return kernel.apply(command)

def status(kernel: SectionKernelProtocol, expected_head: Mapping[str, Any]) -> Dict[str, Any]:
    expected = _head_kwargs(expected_head)
    return _validate_status_result(kernel.status(**expected), expected)

def resume(kernel: SectionKernelProtocol, expected_head: Mapping[str, Any]) -> Dict[str, Any]:
    expected = _head_kwargs(expected_head)
    return _validate_status_result(kernel.resume(**expected), expected)

def _validate_status_result(result: Dict[str, Any], expected: Mapping[str, Any]) -> Dict[str, Any]:
    validate_section_status(result)
    public_head = result["head"]
    if (public_head["revision"] != expected["expected_revision"]
            or public_head["transaction_digest"] != expected["expected_head_digest"]):
        raise SchemaValidationError("$.head: must match caller expected_head")
    return result

def validate_section_status(document: Mapping[str, Any]) -> None:
    """Validate pending versus accepted evidence without creating state."""
    validate_document(document, _schema("section-status-v1.schema.json"))
    evidence = document.get("transition_evidence") if isinstance(document, Mapping) else None
    if not isinstance(evidence, Mapping):
        raise SchemaValidationError("$.transition_evidence: required")
    passed = document.get("claims", {}).get("source_transition_fixture_passed")
    if passed is True:
        if evidence.get("state") != "accepted" or not _DIGEST.fullmatch(str(evidence.get("receipt_digest"))) or _head_kwargs(evidence.get("head", {})) is None:
            raise SchemaValidationError("$.transition_evidence: accepted status requires receipt and head")
        public_head = document.get("head", {})
        if (evidence["head"]["revision"] != public_head.get("revision")
                or evidence["head"]["transaction_digest"] != public_head.get("transaction_digest")):
            raise SchemaValidationError("$.transition_evidence.head: must match the public status head")
    elif passed is False:
        if evidence != {"state": "pending"}:
            raise SchemaValidationError("$.transition_evidence: pending status cannot carry acceptance evidence")
    else:
        raise SchemaValidationError("$.claims.source_transition_fixture_passed: boolean required")
    try:
        position = _section_position(document.get("section_id"))
        history = document.get("section_history", [])
        if position == 0:
            if history != []:
                raise SectionContractError("S0 status cannot have section history")
        else:
            _validate_history(history, target_position=position, include_predecessor=True)
    except SectionContractError as error:
        raise SchemaValidationError("$.section_history: %s" % error) from error

def _validate_public_command(command: Mapping[str, Any], expected_kind: str) -> None:
    """Strictly validate the S0-B handoff before exactly one delegation."""
    command_schema = _schema("section-control-plane-v1.schema.json").get("properties", {}).get(expected_kind)
    if command_schema is None and expected_kind == "open_group":
        command_schema = copy.deepcopy(
            _schema("section-control-plane-v1.schema.json").get("properties", {}).get("open_section")
        )
        if isinstance(command_schema, Mapping):
            command_schema["properties"]["command_type"] = {"const": "open_group"}
    if not isinstance(command_schema, Mapping):
        raise SchemaValidationError("$: missing section command schema")
    validate_document(command, command_schema)
    if not isinstance(command, Mapping) or command.get("command_type") != expected_kind:
        raise SchemaValidationError("$.command_type: incorrect section command")
    required_fields = {"schema", "command_id", "command_type", "run_id", "expected_head", "workflow_version", "graph_version", "actor", "authority_ref", "input_refs", "idempotency_key", "protected_fields", "scope", "payload"}
    if set(command) != required_fields or command.get("schema") != "dag-command/v1" or command.get("graph_version") != "artifact-task-dag/v1" or not isinstance(command.get("idempotency_key"), str) or not command["idempotency_key"]:
        raise SchemaValidationError("$: strict S0-B command shape required")
    actor = command.get("actor")
    if not isinstance(actor, Mapping) or set(actor) != {"role", "assignment_id"} or actor.get("role") != "orchestrator" or actor.get("assignment_id") != "section-control-plane":
        raise SchemaValidationError("$.actor: strict actor required")
    payload = command.get("payload")
    section = payload.get("section") if isinstance(payload, Mapping) else None
    try:
        successor = expected_kind == "open_section" and _section_position(section.get("id")) > 0 if isinstance(section, Mapping) else False
    except SectionContractError as error:
        raise SchemaValidationError("$.payload.section: malformed section") from error
    required = {"plan", "catalog", "checkpoint", "bundle", "lifecycle_prerequisite", "accepted_section_receipt" if successor else "closed_group_receipt"}
    refs = command.get("input_refs")
    if not isinstance(refs, list) or {item.get("kind") for item in refs if isinstance(item, Mapping)} != required or len(refs) != len(required):
        raise SchemaValidationError("$.input_refs: six required kinds exactly once")
    if any(not isinstance(item, Mapping) or set(item) != {"kind", "digest"} or not isinstance(item.get("kind"), str) or not isinstance(item.get("digest"), str) for item in refs):
        raise SchemaValidationError("$.input_refs: malformed ref")
    by_kind = {item["kind"]: item["digest"] for item in refs}
    if any(not _DIGEST.fullmatch(str(value)) for value in by_kind.values()):
        raise SchemaValidationError("$.input_refs: malformed digest")
    expected = command.get("expected_head")
    _head_kwargs(expected)
    authority = command.get("authority_ref")
    if not isinstance(authority, Mapping) or authority.get("expected_head") != expected or authority.get("protected_fields") != command.get("protected_fields"):
        raise SchemaValidationError("$.authority_ref: command binding mismatch")
    if not isinstance(payload, Mapping) or payload.get("transition") != {"intent": "source-transition-fixture-passed", "state": "pending"}:
        raise SchemaValidationError("$.payload.transition: pending intent required")
    pre = payload.get("preconditions")
    binds = {"plan_digest": "plan", "catalog_digest": "catalog", "checkpoint_digest": "checkpoint", "bundle_digest": "bundle", "lifecycle_prerequisite_digest": "lifecycle_prerequisite", "accepted_section_receipt_digest" if successor else "closed_group_receipt_digest": "accepted_section_receipt" if successor else "closed_group_receipt"}
    if not isinstance(pre, Mapping) or any(pre.get(field) != by_kind[kind] for field, kind in binds.items()):
        raise SchemaValidationError("$.payload.preconditions: input ref binding mismatch")
    if successor:
        try:
            if pre.get("predecessor_section_id") != "S%d" % (_section_position(section["id"]) - 1):
                raise SectionContractError("successor precondition is malformed")
        except SectionContractError as error:
            raise SchemaValidationError("$.payload.preconditions: %s" % error) from error

class SectionControlPlaneV1:
    """Compile deterministic commands. This class keeps no mutable state."""
    def compile_registry(self, inputs: Mapping[str, Any]) -> Dict[str, Any]:
        predecessor = self._validate(inputs, later=False)
        closed_id = predecessor.get("group_id", predecessor.get("section_id"))
        ordered = self._ordered(inputs["groups"], closed_id)
        group = next((item for item in ordered if item["id"] == predecessor["next_group"]), None) if "next_group" in predecessor else ordered[0]
        if group is None:
            raise SectionContractError("closed Bootstrap next group does not exist in schedule")
        self._resolve(inputs, ordered)
        section = inputs["section"]
        history = self._successor_history(inputs, predecessor)
        return {"schema": "section-control-plane/v1", "section": copy.deepcopy(dict(section)),
                "schedule": {"schema": "section-schedule/v1", "section_id": section["id"], "workflow_id": section["workflow_id"], "groups": [{"id": g["id"], "first_epoch": g["first_epoch"], "first_frontier": list(g["first_frontier"])} for g in ordered], "first_frontier": list(group["first_frontier"])},
                "open_section": self.compile_open_section(inputs, group),
                "status": {"schema": "section-status/v1", "section_id": section["id"], "status": "active", "group": {"id": group["id"], "status": "open", "next_group": None}, "epoch": {"id": group["first_epoch"], "status": "open", "group_id": group["id"], "boundary_reason": "open_section", "clear_before_next": False}, "ready": list(group["first_frontier"]), "head": copy.deepcopy(dict(inputs["expected_head"])), "claims": {"source_transition_fixture_passed": False, "actual_a7": False, "activation": False, "full_ready": False}, "transition_evidence": {"state": "pending"}, **({"section_history": history} if history else {})}}

    def compile_open_section(self, inputs: Mapping[str, Any], group: Mapping[str, Any]) -> Dict[str, Any]:
        self._validate(inputs, later=False); self._resolve(inputs, [group])
        return self._command(inputs, group, "open_section")

    def compile_open_group(self, inputs: Mapping[str, Any]) -> Dict[str, Any]:
        self._validate(inputs, later=True); group = inputs["groups"][0]; self._resolve(inputs, [group])
        return self._command(inputs, group, "open_group")

    @staticmethod
    def _validate(inputs: Mapping[str, Any], *, later: bool) -> None:
        if not isinstance(inputs, Mapping) or inputs.get("schema") != "section-plan/v1": raise SectionContractError("section plan must be section-plan/v1")
        try:
            validate_document(inputs, _schema("section-plan-v1.schema.json"))
        except SchemaValidationError as error:
            raise SectionContractError("section plan does not satisfy section-plan/v1: %s" % error) from error
        section, groups = inputs.get("section"), inputs.get("groups")
        if not isinstance(section, Mapping): raise SectionContractError("section identity is malformed")
        position = _section_position(section.get("id"))
        if not isinstance(groups, list) or not groups or (later and len(groups) != 1): raise SectionContractError("scheduled groups are malformed")
        _head_kwargs(inputs.get("expected_head", {})); SectionControlPlaneV1._authority(inputs.get("authority"), inputs["expected_head"])
        predecessor = inputs.get("closed_bootstrap") if position == 0 or later else inputs.get("accepted_section")
        if not isinstance(predecessor, Mapping) or predecessor.get("status") != "paused_after_group" or predecessor.get("ready") != [] or predecessor.get("clear_boundary") is not True:
            raise SectionContractError("closed predecessor must be paused_after_group with ready=[] and clear boundary")
        if not isinstance(predecessor.get("run_id"), str) or not predecessor["run_id"] or not isinstance(predecessor.get("workflow_version"), str) or not predecessor["workflow_version"]:
            raise SectionContractError("closed predecessor run identity is required")
        for name in ("plan_digest", "catalog_digest"):
            if not _DIGEST.fullmatch(str(predecessor.get(name))): raise SectionContractError("%s must be a sha256 digest" % name)
        for name in ("checkpoint_ref", "bundle_ref"):
            if not isinstance(predecessor.get(name), Mapping) or not _DIGEST.fullmatch(str(predecessor[name].get("digest"))): raise SectionContractError("%s requires a digest-bound reference" % name)
        prerequisite = predecessor.get("lifecycle_prerequisite")
        if not isinstance(prerequisite, Mapping) or not _DIGEST.fullmatch(str(prerequisite.get("receipt_digest"))): raise SectionContractError("lifecycle prerequisite binding is required")
        if position == 0 or later:
            receipt = predecessor.get("closed_group_receipt")
            if not isinstance(receipt, Mapping) or receipt.get("group_id") != predecessor.get("group_id") or not _DIGEST.fullmatch(str(receipt.get("digest"))): raise SectionContractError("closed group receipt binding is required")
        else:
            receipt = predecessor.get("accepted_section_receipt")
            if predecessor.get("section_id") != "S%d" % (position - 1) or predecessor.get("next_section") != section["id"]:
                raise SectionContractError("section successor must be the numeric immediate successor")
            if prerequisite.get("kind") != "accepted-section" or prerequisite.get("section_id") != predecessor.get("section_id"):
                raise SectionContractError("successor requires its accepted predecessor section")
            _history_ref(receipt, object_type="artifact")
            _history_ref(predecessor.get("bundle_ref"), object_type="artifact-bundle")
            _history_ref(predecessor.get("checkpoint_ref"), object_type="checkpoint")
            if receipt["digest"] != prerequisite["receipt_digest"]:
                raise SectionContractError("accepted section receipt must bind lifecycle prerequisite")
            prior_history = _validate_history(predecessor.get("section_history"), target_position=position, include_predecessor=False)
            if receipt["digest"] in {item["accepted_receipt_ref"]["digest"] for item in prior_history}:
                raise SectionContractError("section history reuses an accepted receipt")
        plan, catalog = inputs.get("plan"), inputs.get("catalog")
        if not isinstance(plan, Mapping) or plan.get("digest") != predecessor.get("plan_digest") or not _DIGEST.fullmatch(str(plan.get("digest"))):
            raise SectionContractError("digest-bound plan reference is required")
        if not isinstance(catalog, Mapping) or catalog.get("digest") != predecessor.get("catalog_digest") or not _DIGEST.fullmatch(str(catalog.get("digest"))) or not isinstance(catalog.get("tasks"), list): raise SectionContractError("digest-bound catalog task mapping is required")
        canonical_tasks = json.dumps(catalog["tasks"], sort_keys=True, separators=(",", ":"), ensure_ascii=True).encode("utf-8")
        if catalog["digest"] != "sha256:" + hashlib.sha256(canonical_tasks).hexdigest():
            raise SectionContractError("catalog digest does not bind task mapping bytes")
        target = groups[0]
        if later:
            if predecessor.get("group_id") == target.get("id") or predecessor.get("next_group") != target.get("id"):
                raise SectionContractError("later group must advance from a distinct closed group")
            if prerequisite.get("kind") != "accepted-section" or prerequisite.get("section_id") != section["id"]:
                raise SectionContractError("later group requires its accepted parent section")
        elif position == 0 and (predecessor.get("group_id") != "Bootstrap" or predecessor.get("next_group") != target.get("id") or target.get("id") != "contracts-and-schema" or prerequisite.get("kind") != "closed-bootstrap" or prerequisite.get("group_id") != "Bootstrap"):
            raise SectionContractError("section opening requires closed Bootstrap to contracts-and-schema")
        return predecessor

    @staticmethod
    def _successor_history(inputs: Mapping[str, Any], predecessor: Mapping[str, Any]) -> list[Dict[str, Any]]:
        position = _section_position(inputs["section"]["id"])
        if position == 0:
            return []
        history = _validate_history(predecessor.get("section_history"), target_position=position, include_predecessor=False)
        history.append({"position": position - 1, "section_id": predecessor["section_id"], "accepted_receipt_ref": copy.deepcopy(dict(predecessor["accepted_section_receipt"])), "bundle_ref": copy.deepcopy(dict(predecessor["bundle_ref"])), "checkpoint_ref": copy.deepcopy(dict(predecessor["checkpoint_ref"]))})
        return history

    @staticmethod
    def _authority(authority: Any, expected: Mapping[str, Any]) -> None:
        if not isinstance(authority, Mapping) or authority.get("execution_class") != "candidate-generic": raise SectionContractError("only candidate-generic source transitions are allowed")
        scopes = authority.get("scopes")
        if not isinstance(scopes, list) or len(scopes) != len(set(scopes)) or not {"source", "open_section", "open_group"}.issubset(set(scopes)): raise SectionContractError("authority scopes do not preserve ControlKernel grammar")
        if authority.get("approved") is not True or not isinstance(authority.get("human_receipt"), str) or not authority["human_receipt"]: raise SectionContractError("approved authority receipt is required")
        fields = authority.get("protected_fields")
        if not isinstance(fields, list) or not {"group", "epoch", "ready"}.issubset(set(fields)): raise SectionContractError("authority protected-field binding is required")
        if authority.get("expected_head") != expected: raise SectionContractError("authority expected-head binding is required")

    @staticmethod
    def _ordered(groups: Any, closed_id: str) -> list:
        if not all(isinstance(g, Mapping) and isinstance(g.get("id"), str) and g["id"] for g in groups): raise SectionContractError("groups require non-empty IDs")
        by_id = {g["id"]: g for g in groups}
        if len(by_id) != len(groups): raise SectionContractError("duplicate group mapping")
        for g in groups:
            if not isinstance(g.get("depends_on", []), list) or any(dep not in by_id and dep != closed_id for dep in g["depends_on"]): raise SectionContractError("unknown group dependency")
        result, permanent, temporary = [], set(), set()
        def visit(key: str) -> None:
            if key in permanent: return
            if key in temporary: raise SectionContractError("group dependency cycle")
            temporary.add(key)
            for dep in sorted(by_id[key].get("depends_on", [])):
                if dep in by_id: visit(dep)
            temporary.remove(key); permanent.add(key); result.append(by_id[key])
        for key in sorted(by_id): visit(key)
        return result

    @staticmethod
    def _resolve(inputs: Mapping[str, Any], groups: list) -> None:
        tasks = {}
        for item in inputs["catalog"]["tasks"]:
            if not isinstance(item, Mapping) or not isinstance(item.get("id"), str) or item["id"] in tasks: raise SectionContractError("catalog contains duplicate or malformed task mapping")
            tasks[item["id"]] = item
        seen = set()
        for group in groups:
            frontier = group.get("first_frontier")
            if not isinstance(frontier, list) or not frontier or len(frontier) != len(set(frontier)): raise SectionContractError("first frontier must be unique and non-empty")
            for task_id in frontier:
                task = tasks.get(task_id)
                if not isinstance(task_id, str) or not _TASK_ID.fullmatch(task_id) or task is None: raise SectionContractError("catalog does not resolve frontier task")
                if task.get("section_id") != inputs["section"]["id"] or task.get("group_id") != group.get("id") or task.get("epoch_id") != group.get("first_epoch"): raise SectionContractError("catalog frontier conflicts with section/group/epoch")
                if task_id in seen: raise SectionContractError("duplicate frontier task")
                seen.add(task_id)

    @staticmethod
    def _command(inputs: Mapping[str, Any], group: Mapping[str, Any], kind: str) -> Dict[str, Any]:
        section = inputs["section"]
        successor = _section_position(section["id"]) > 0 and kind == "open_section"
        closed = inputs["accepted_section"] if successor else inputs["closed_bootstrap"]
        receipt_kind = "accepted_section_receipt" if successor else "closed_group_receipt"
        receipt = closed[receipt_kind]
        refs = [{"kind": "plan", "digest": closed["plan_digest"]}, {"kind": "catalog", "digest": closed["catalog_digest"]}, {"kind": "checkpoint", "digest": closed["checkpoint_ref"]["digest"]}, {"kind": "bundle", "digest": closed["bundle_ref"]["digest"]}, {"kind": "lifecycle_prerequisite", "digest": closed["lifecycle_prerequisite"]["receipt_digest"]}, {"kind": receipt_kind, "digest": receipt["digest"]}]
        pre = {"plan_digest": closed["plan_digest"], "catalog_digest": closed["catalog_digest"], "lifecycle_prerequisite_digest": closed["lifecycle_prerequisite"]["receipt_digest"], "checkpoint_digest": closed["checkpoint_ref"]["digest"], "bundle_digest": closed["bundle_ref"]["digest"]}
        if successor:
            pre.update({"predecessor_section_id": closed["section_id"], "accepted_section_receipt_digest": receipt["digest"]})
        else:
            pre.update({"closed_group_id": closed["group_id"], "closed_group_receipt_digest": receipt["digest"]})
        return {"schema": "dag-command/v1", "command_id": ("section-" if kind == "open_section" else "group-") + section["id"].lower() + "-" + group["id"].lower(), "command_type": kind, "run_id": closed["run_id"], "expected_head": copy.deepcopy(dict(inputs["expected_head"])), "workflow_version": closed["workflow_version"], "graph_version": "artifact-task-dag/v1", "actor": {"role": "orchestrator", "assignment_id": "section-control-plane"}, "authority_ref": copy.deepcopy(dict(inputs["authority"])), "input_refs": refs, "idempotency_key": inputs["idempotency_key"], "protected_fields": ["section", "group", "epoch", "ready"], "scope": ["source"], "payload": {"section": copy.deepcopy(dict(section)), "group": {"id": group["id"], "first_epoch": group["first_epoch"]}, "first_frontier": list(group["first_frontier"]), "preconditions": pre, "transition": {"intent": "source-transition-fixture-passed", "state": "pending"}}}

__all__ = ["SectionContractError", "SectionControlPlaneV1", "SectionKernelProtocol", "open_group", "open_section", "resume", "status", "validate_section_status"]
