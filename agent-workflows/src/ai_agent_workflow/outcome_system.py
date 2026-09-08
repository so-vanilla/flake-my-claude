"""Stateless C1--C6 OutcomeSystemV1 compiler and epoch composer.

The module validates supplied physical receipts.  It intentionally has no
collector, store, network client, mutable run state, or worker-local target.
"""
from __future__ import annotations

import copy
import hashlib
import json
from pathlib import Path
from typing import Any, Mapping

from .schema_validation import SchemaValidationError, validate_document


_ROOT = Path(__file__).resolve().parents[2]
_SCHEMA = json.loads((_ROOT / "schemas" / "outcome-system-v1.schema.json").read_text())
_BUNDLE_SCHEMA = json.loads((_ROOT / "schemas" / "outcome-epoch-bundle-v1.schema.json").read_text())
_OBSERVATION_SCHEMA = json.loads((_ROOT / "schemas" / "measurement-observation-v1.schema.json").read_text())
_IDS = {"group.C.C%d" % index for index in range(1, 7)}


class OutcomeSystemError(ValueError):
    """A supplied document does not meet the physical compile contract."""


def _validate(value: Any, schema: Mapping[str, Any], registry: Mapping[str, Any] | None = None) -> None:
    validate_document(value, schema, registry or schema.get("$defs", {}))


def _digest(value: Mapping[str, Any]) -> str:
    payload = json.dumps(value, sort_keys=True, separators=(",", ":")).encode("utf-8")
    return "sha256:" + hashlib.sha256(payload).hexdigest()


class OutcomeSystemV1:
    """Pure dispatcher for C1--C6 and composition for closed outcome epochs."""

    def compile(self, qualified_id: str, inputs: Mapping[str, Any], authority: Mapping[str, Any], expected_head: Mapping[str, Any]) -> dict[str, Any]:
        values = copy.deepcopy(dict(inputs)) if isinstance(inputs, Mapping) else {}
        auth, head = copy.deepcopy(authority), copy.deepcopy(expected_head)
        self._context(auth, head)
        objective = values.get("objective_ref")
        if not self._valid(objective, _SCHEMA["$defs"]["ref"]):
            raise OutcomeSystemError("objective_ref must be a physical reference")
        if qualified_id not in _IDS:
            return self._refusal("needs_user", auth)
        if values.get("observed_head") is not None and values["observed_head"] != head:
            return self._refusal("blocked_stale_input", auth)
        handlers = {
            "group.C.C1": self._c1, "group.C.C2": self._c2, "group.C.C3": self._c3,
            "group.C.C4": self._c4, "group.C.C5": self._c5, "group.C.C6": self._c6,
        }
        output = handlers[qualified_id](values, auth)
        result = {"schema": "outcome-system-artifact/v1", "qualified_id": qualified_id,
                  "objective_ref": objective, "authority": auth, "expected_head": head,
                  "output": output}
        _validate(result, _SCHEMA)
        return result

    def decompose_outcomes(self, inputs: Mapping[str, Any], authority: Mapping[str, Any], expected_head: Mapping[str, Any]) -> dict[str, Any]:
        """Compile the public C1 outcome-decomposition contract."""
        return self.compile("group.C.C1", inputs, authority, expected_head)

    def build_dependency_graph(self, inputs: Mapping[str, Any], authority: Mapping[str, Any], expected_head: Mapping[str, Any]) -> dict[str, Any]:
        """Compile the public C2 dependency-graph contract."""
        return self.compile("group.C.C2", inputs, authority, expected_head)

    def design_measurement(self, inputs: Mapping[str, Any], authority: Mapping[str, Any], expected_head: Mapping[str, Any]) -> dict[str, Any]:
        """Compile the public C3 measurement-design contract."""
        return self.compile("group.C.C3", inputs, authority, expected_head)

    def define_targets(self, inputs: Mapping[str, Any], authority: Mapping[str, Any], expected_head: Mapping[str, Any]) -> dict[str, Any]:
        """Compile the public C4 target-definition contract."""
        return self.compile("group.C.C4", inputs, authority, expected_head)

    def record_baseline(self, inputs: Mapping[str, Any], authority: Mapping[str, Any], expected_head: Mapping[str, Any]) -> dict[str, Any]:
        """Compile the public C5 baseline-recording contract."""
        return self.compile("group.C.C5", inputs, authority, expected_head)

    def validate_system(self, inputs: Mapping[str, Any], authority: Mapping[str, Any], expected_head: Mapping[str, Any]) -> dict[str, Any]:
        """Compile the public C6 outcome-system-validation contract."""
        return self.compile("group.C.C6", inputs, authority, expected_head)

    def compose_c01(self, bundle: Mapping[str, Any]) -> dict[str, Any]:
        """Return a validated, copied C-01 bundle; no state is retained."""
        result = copy.deepcopy(dict(bundle)) if isinstance(bundle, Mapping) else bundle
        try:
            _validate(result, _BUNDLE_SCHEMA)
        except SchemaValidationError as error:
            raise OutcomeSystemError(str(error)) from error
        if result.get("epoch_id") != "C-01":
            raise OutcomeSystemError("C-01 composition requires epoch C-01")
        return result

    def compose_c02(self, bundle: Mapping[str, Any], current_c01: Mapping[str, Any]) -> dict[str, Any]:
        """Compose only against the supplied current C-01 bytes.

        A changed C-01 invalidates this C-02 candidate without altering either
        input.  The returned refusal is deliberately typed and state-free.
        """
        candidate = copy.deepcopy(dict(bundle)) if isinstance(bundle, Mapping) else bundle
        current = self.compose_c01(current_c01)
        binding = candidate.get("c01_bundle_ref") if isinstance(candidate, Mapping) else None
        if not isinstance(binding, Mapping) or binding.get("identity") != "C-01" or binding.get("digest") != _digest(current):
            return {"schema": "outcome-system-refusal/v1", "kind": "refusal", "code": "blocked_stale_input", "owner_ref": copy.deepcopy({"kind": "fixture", "stable_id": "epoch-composer", "role": "upstream-owner", "path": "outcome-system", "digest": "sha256:" + "0" * 64})}
        try:
            _validate(candidate, _BUNDLE_SCHEMA)
        except SchemaValidationError as error:
            raise OutcomeSystemError(str(error)) from error
        if candidate.get("epoch_id") != "C-02":
            raise OutcomeSystemError("C-02 composition requires epoch C-02")
        return candidate

    def _c1(self, values: Mapping[str, Any], authority: Mapping[str, Any]) -> dict[str, Any]:
        payload = values.get("outcome_map")
        if not isinstance(payload, Mapping):
            return self._refusal("needs_user", authority)
        for node in payload.get("outcomes", []):
            state = str(node.get("achieved_state", "")).lower() if isinstance(node, Mapping) else ""
            if state.startswith(("implement", "build ", "write ", "configure ", "run ")):
                return self._refusal("outcome_is_implementation_task", authority)
        required = set(values.get("required_contributions", []))
        actual = {item for node in payload.get("outcomes", []) if isinstance(node, Mapping) for item in node.get("objective_contribution", [])}
        if not required.issubset(actual):
            return self._refusal("coverage_hole", authority)
        return self._candidate("outcome_map", payload)

    def _c2(self, values: Mapping[str, Any], authority: Mapping[str, Any]) -> dict[str, Any]:
        graph = values.get("dependency_graph")
        if not isinstance(graph, Mapping):
            return self._refusal("needs_user", authority)
        nodes = graph.get("node_ids", [])
        node_set = set(nodes) if isinstance(nodes, list) else set()
        for edge in graph.get("edges", []):
            if not isinstance(edge, Mapping) or edge.get("prerequisite") not in node_set or edge.get("dependent") not in node_set:
                return self._refusal("unknown_dependency_endpoint", authority)
        for join in graph.get("joins", []):
            if not isinstance(join, Mapping) or join.get("outcome_id") not in node_set or not self._valid(join.get("owner_ref"), _SCHEMA["$defs"]["owner"]):
                return self._refusal("ownerless_join", authority)
        edges = [(edge["prerequisite"], edge["dependent"]) for edge in graph.get("edges", []) if isinstance(edge, Mapping)]
        if self._cyclic(node_set, edges):
            return self._refusal("dependency_cycle", authority)
        # A multi-node graph must expose a dependency or owned convergence; an
        # isolated singleton is a valid root outcome.
        connected = set()
        for left, right in edges:
            connected.update((left, right))
        if len(node_set) > 1 and connected != node_set:
            return self._refusal("orphan_outcome", authority)
        return self._candidate("dependency_graph", graph)

    def _c3(self, values: Mapping[str, Any], authority: Mapping[str, Any]) -> dict[str, Any]:
        plan = values.get("measurement_plan")
        if not isinstance(plan, Mapping):
            return self._refusal("needs_user", authority)
        strategy = plan.get("strategy")
        if not self._valid(values.get("gaming_guard_ref"), _SCHEMA["$defs"]["ref"]):
            return self._refusal("needs_user", authority)
        if strategy == "proxy" and not values.get("gaming_risk"):
            return self._refusal("needs_user", authority)
        if strategy == "qualitative_rubric" and not self._refs(values.get("rubric_anchor_refs")):
            return self._refusal("needs_user", authority)
        if strategy == "not_measured" and not self._valid(values.get("decision_ref"), _SCHEMA["$defs"]["ref"]):
            return self._refusal("needs_user", authority)
        return self._candidate("measurement_plan", plan)

    def _c4(self, values: Mapping[str, Any], authority: Mapping[str, Any]) -> dict[str, Any]:
        target, detail = values.get("target_set"), values.get("target")
        required = {"unit", "formula", "source", "frequency", "window", "guard"}
        if not isinstance(detail, Mapping) or not required.issubset(detail) or detail.get("local") is True:
            return self._refusal("needs_user", authority)
        return self._candidate("target_set", target)

    def _c5(self, values: Mapping[str, Any], authority: Mapping[str, Any]) -> dict[str, Any]:
        target, observation = values.get("target_set"), values.get("observation")
        if not isinstance(target, Mapping) or not isinstance(observation, Mapping):
            return self._refusal("needs_user", authority)
        try:
            _validate(observation, _OBSERVATION_SCHEMA)
        except SchemaValidationError:
            return self._refusal("needs_user", authority)
        if observation["status"] == "available":
            payload = {"schema": "baseline/v1", "target_ref": copy.deepcopy(target["target_ref"]), "availability": "available", "value": observation["value"], "reason": None}
        else:
            reasons = {"stale": "stale_source", "access-denied": "access_denied", "side-effect-required": "measurement_side_effect", "incomparable": "incomparable_condition", "not-measured": "intentionally_unmeasured"}
            payload = {"schema": "baseline/v1", "target_ref": copy.deepcopy(target["target_ref"]), "availability": "unavailable", "value": None, "reason": reasons[observation["reason"]["code"]]}
        return self._candidate("baseline", payload)

    def _c6(self, values: Mapping[str, Any], authority: Mapping[str, Any]) -> dict[str, Any]:
        trace = values.get("trace")
        required = {"objective", "outcome_map", "dependency_graph", "measurement_plan", "target_set", "baseline"}
        if not isinstance(trace, Mapping) or not required.issubset(trace):
            return self._refusal("needs_user", authority)
        if not self._valid(trace["objective"], _SCHEMA["$defs"]["ref"]) or not self._valid(trace["outcome_map"], _SCHEMA["$defs"]["outcome_map"]) or not self._valid(trace["dependency_graph"], _SCHEMA["$defs"]["dependency_graph"]) or not self._valid(trace["measurement_plan"], _SCHEMA["$defs"]["measurement_plan"]) or not self._valid(trace["target_set"], _SCHEMA["$defs"]["target_set"]) or not self._valid(trace["baseline"], _SCHEMA["$defs"]["baseline"]):
            return self._refusal("needs_user", authority)
        outcome_ids = {item.get("outcome_id") for item in trace["outcome_map"].get("outcomes", []) if isinstance(item, Mapping)}
        plan_id = trace["measurement_plan"].get("outcome_id") if isinstance(trace["measurement_plan"], Mapping) else None
        target_id = trace["target_set"].get("outcome_id") if isinstance(trace["target_set"], Mapping) else None
        if not outcome_ids or plan_id not in outcome_ids or target_id != plan_id or trace["baseline"].get("target_ref") != trace["target_set"].get("target_ref"):
            return self._refusal("needs_user", authority)
        return self._candidate("validation", {"schema": "outcome-validation/v1", "status": "valid", "upstream_owner_ref": copy.deepcopy(authority["owner_ref"])})

    @staticmethod
    def _cyclic(nodes: set[Any], edges: list[tuple[Any, Any]]) -> bool:
        adjacent = {node: [] for node in nodes}
        for left, right in edges: adjacent[left].append(right)
        active, done = set(), set()
        def visit(node: Any) -> bool:
            if node in active: return True
            if node in done: return False
            active.add(node)
            found = any(visit(next_node) for next_node in adjacent[node])
            active.remove(node); done.add(node)
            return found
        return any(visit(node) for node in nodes)

    @staticmethod
    def _candidate(kind: str, payload: Any) -> dict[str, Any]:
        return {"schema": "outcome-system-output/v1", "kind": kind, "payload": copy.deepcopy(payload)}

    @staticmethod
    def _refs(value: Any) -> bool:
        return isinstance(value, list) and bool(value) and all(OutcomeSystemV1._valid(item, _SCHEMA["$defs"]["ref"]) for item in value)

    @staticmethod
    def _valid(value: Any, schema: Mapping[str, Any]) -> bool:
        try: _validate(value, schema, _SCHEMA["$defs"])
        except SchemaValidationError: return False
        return True

    @staticmethod
    def _context(authority: Any, head: Any) -> None:
        if not OutcomeSystemV1._valid(authority, _SCHEMA["$defs"]["authority"]) or not OutcomeSystemV1._valid(head, _SCHEMA["$defs"]["head"]):
            raise OutcomeSystemError("authority and expected_head must be strict physical references")

    @staticmethod
    def _refusal(code: str, authority: Mapping[str, Any]) -> dict[str, Any]:
        return {"schema": "outcome-system-refusal/v1", "kind": "refusal", "code": code, "owner_ref": copy.deepcopy(authority["owner_ref"])}


__all__ = ["OutcomeSystemError", "OutcomeSystemV1"]
