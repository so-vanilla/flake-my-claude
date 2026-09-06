"""Public-seam regression tests for the stateless Group C compiler."""
from __future__ import annotations

import copy
import hashlib
import json
from pathlib import Path
import unittest

from ai_agent_workflow.outcome_system import OutcomeSystemV1
from ai_agent_workflow.schema_validation import SchemaValidationError, validate_document


ROOT = Path(__file__).resolve().parents[2]


def digest(value):
    return "sha256:" + hashlib.sha256(json.dumps(value, sort_keys=True, separators=(",", ":")).encode()).hexdigest()


def ref(name):
    character = hashlib.sha256(name.encode()).hexdigest()[0]
    return {"path": "fixtures/%s.json" % name, "version": "v1", "digest": "sha256:" + (character * 64)}


OWNER = {"kind": "fixture", "stable_id": "fixture-owner", "role": "owner", "path": "fixtures/owner.json", "digest": "sha256:" + "a" * 64}
AUTHORITY = {"namespace": "fixture:s2", "scope": "fixture-only", "owner_ref": OWNER}
HEAD = {"revision": 1, "transaction_digest": "sha256:" + "b" * 64}
OBJECTIVE = ref("objective")


def outcome(identifier="result", contribution="objective-success"):
    return {"outcome_id": identifier, "achieved_state": "A verifiable user result exists", "why_required": "It advances the objective", "objective_contribution": [contribution], "exclusion_conditions": ["No implementation task is asserted"], "owner_ref": OWNER, "acceptance_predicate_refs": [ref("predicate")]}


def outcome_map():
    return {"schema": "outcome-map/v1", "outcomes": [outcome()], "coverage_refs": [ref("coverage")]}


def graph():
    return {"schema": "outcome-dependency-graph/v1", "node_ids": ["result"], "edges": [], "joins": [], "runtime_checks": ["unknown_endpoint", "orphan", "cycle", "ownerless_join"]}


class C1_DecomposeOutcomesTests(unittest.TestCase):
    def test_rejects_task_shape_and_coverage_hole(self):
        system = OutcomeSystemV1()
        task = outcome(); task["achieved_state"] = "Implement the dashboard"
        self.assertEqual("outcome_is_implementation_task", system.compile("group.C.C1", {"objective_ref": OBJECTIVE, "outcome_map": {**outcome_map(), "outcomes": [task]}}, AUTHORITY, HEAD)["output"]["code"])
        self.assertEqual("coverage_hole", system.compile("group.C.C1", {"objective_ref": OBJECTIVE, "outcome_map": outcome_map(), "required_contributions": ["objective-success", "safety"]}, AUTHORITY, HEAD)["output"]["code"])

    def test_returns_a_typed_outcome_map_without_mutating_input(self):
        values = {"objective_ref": OBJECTIVE, "outcome_map": outcome_map()}; original = copy.deepcopy(values)
        result = OutcomeSystemV1().compile("group.C.C1", values, AUTHORITY, HEAD)
        self.assertEqual("outcome_map", result["output"]["kind"]); self.assertEqual(values, original)


class C2_OutcomeDAGTests(unittest.TestCase):
    def test_refuses_unknown_cycle_orphan_and_ownerless_join(self):
        system = OutcomeSystemV1()
        cases = [
            ({**graph(), "edges": [{"prerequisite": "missing", "dependent": "result"}]}, "unknown_dependency_endpoint"),
            ({**graph(), "node_ids": ["a", "b"], "edges": [{"prerequisite": "a", "dependent": "b"}, {"prerequisite": "b", "dependent": "a"}]}, "dependency_cycle"),
            ({**graph(), "node_ids": ["a", "b"], "edges": []}, "orphan_outcome"),
            ({**graph(), "node_ids": ["a", "b", "c"], "edges": [{"prerequisite": "a", "dependent": "c"}, {"prerequisite": "b", "dependent": "c"}], "joins": [{"outcome_id": "c", "incoming": ["a", "b"], "owner_ref": None}]}, "ownerless_join"),
        ]
        for candidate, code in cases:
            with self.subTest(code=code):
                value = system.compile("group.C.C2", {"objective_ref": OBJECTIVE, "outcome_map": outcome_map(), "dependency_graph": candidate}, AUTHORITY, HEAD)
                self.assertEqual(code, value["output"]["code"])


class C3_MeasurementDesignTests(unittest.TestCase):
    def test_requires_proxy_guard_rubric_anchor_and_not_measured_decision(self):
        system = OutcomeSystemV1()
        for plan, extra in [
            ({"schema": "measurement-plan/v1", "outcome_id": "result", "strategy": "proxy", "rationale": "direct unavailable"}, {}),
            ({"schema": "measurement-plan/v1", "outcome_id": "result", "strategy": "qualitative_rubric", "rationale": "quality"}, {"gaming_guard_ref": ref("guard")}),
            ({"schema": "measurement-plan/v1", "outcome_id": "result", "strategy": "not_measured", "rationale": "no safe measure"}, {"gaming_guard_ref": ref("guard")}),
        ]:
            result = system.compile("group.C.C3", {"objective_ref": OBJECTIVE, "measurement_plan": plan, **extra}, AUTHORITY, HEAD)
            self.assertEqual("needs_user", result["output"]["code"])


class C4_TargetDefinitionTests(unittest.TestCase):
    def test_requires_source_bound_nonlocal_target(self):
        target = {"schema": "target-set/v1", "outcome_id": "result", "target_ref": ref("target")}
        result = OutcomeSystemV1().compile("group.C.C4", {"objective_ref": OBJECTIVE, "target_set": target, "target": {"unit": "events", "formula": "count", "frequency": "daily", "window": "week", "guard": "no regression", "local": True}}, AUTHORITY, HEAD)
        self.assertEqual("needs_user", result["output"]["code"])


class C5_BaselineTests(unittest.TestCase):
    def test_preserves_zero_and_unavailable_not_measured_decision(self):
        target = {"schema": "target-set/v1", "outcome_id": "result", "target_ref": ref("target")}
        available = {"schema": "measurement-observation/v1", "observation_id": "zero", "status": "available", "value": 0, "unit": "events", "condition": "same", "window": {"start": "2026-01-01T00:00:00Z", "end": "2026-01-02T00:00:00Z"}, "source_ref": {"path": "fixtures/source.json", "digest": "sha256:" + "c" * 64}}
        result = OutcomeSystemV1().compile("group.C.C5", {"objective_ref": OBJECTIVE, "target_set": target, "observation": available}, AUTHORITY, HEAD)
        self.assertEqual(("available", 0, None), (result["output"]["payload"]["availability"], result["output"]["payload"]["value"], result["output"]["payload"]["reason"]))
        unavailable = {"schema": "measurement-observation/v1", "observation_id": "none", "status": "unavailable", "value": None, "reason": {"code": "not-measured", "decision_ref": {"path": "fixtures/decision.json", "digest": "sha256:" + "d" * 64}}}
        result = OutcomeSystemV1().compile("group.C.C5", {"objective_ref": OBJECTIVE, "target_set": target, "observation": unavailable}, AUTHORITY, HEAD)
        self.assertEqual(("unavailable", None, "intentionally_unmeasured"), (result["output"]["payload"]["availability"], result["output"]["payload"]["value"], result["output"]["payload"]["reason"]))


class C6_OutcomeValidationTests(unittest.TestCase):
    def test_requires_complete_trace_and_returns_upstream_owner(self):
        result = OutcomeSystemV1().compile("group.C.C6", {"objective_ref": OBJECTIVE, "trace": {"objective": OBJECTIVE, "outcome_map": outcome_map(), "dependency_graph": graph(), "measurement_plan": {"schema": "measurement-plan/v1", "outcome_id": "result", "strategy": "direct_metric", "rationale": "direct"}, "target_set": {"schema": "target-set/v1", "outcome_id": "result", "target_ref": ref("target")}}}, AUTHORITY, HEAD)
        self.assertEqual("needs_user", result["output"]["code"])


class GroupCPublicSelectorReplayTests(unittest.TestCase):
    """The six physical Skill selectors remain thin public compile delegates."""

    SELECTORS = {
        "group.C.C1": "decompose_outcomes",
        "group.C.C2": "build_dependency_graph",
        "group.C.C3": "design_measurement",
        "group.C.C4": "define_targets",
        "group.C.C5": "record_baseline",
        "group.C.C6": "validate_system",
    }

    def test_public_selectors_replay_to_their_exact_compile_contract(self):
        system = OutcomeSystemV1()
        inputs = {"objective_ref": OBJECTIVE}
        for qualified_id, selector in self.SELECTORS.items():
            with self.subTest(qualified_id=qualified_id):
                self.assertEqual(
                    system.compile(qualified_id, inputs, AUTHORITY, HEAD),
                    getattr(system, selector)(inputs, AUTHORITY, HEAD),
                )

    def test_group_c_manifest_replays_physical_digests_and_selectors(self):
        manifest_path = ROOT / "agent-workflows/groups/outcomes.json"
        manifest = json.loads(manifest_path.read_text())
        for contract in manifest["contracts"]:
            with self.subTest(qualified_id=contract["qualified_id"]):
                for reference in (contract["source_ref"], contract["selector_ref"], contract["receipt_ref"]):
                    physical = ROOT / reference["path"]
                    self.assertTrue(physical.is_file())
                    self.assertEqual("sha256:" + hashlib.sha256(physical.read_bytes()).hexdigest(), reference["digest"])
                selector = contract["selector_ref"]["selector"].split(".", 1)[1]
                self.assertEqual(self.SELECTORS[contract["qualified_id"]], selector)
                self.assertTrue(callable(getattr(OutcomeSystemV1, selector, None)))


class GroupCManifestSchemaTests(unittest.TestCase):
    def setUp(self):
        self.schema = json.loads((ROOT / "agent-workflows/schemas/group-c-manifest-v1.schema.json").read_text())
        self.fixtures = ROOT / "agent-workflows/tests/fixtures/s2/foundation/outcome"

    def fixture(self, name):
        return json.loads((self.fixtures / name).read_text())

    def validate(self, document):
        validate_document(document, self.schema, self.schema["$defs"])

    def test_accepts_exact_six_contract_map(self):
        self.validate(self.fixture("group-c-valid.json"))

    def test_refuses_mismatched_duplicate_and_extra_contracts(self):
        malformed = {
            "mismatched": self.fixture("group-c-invalid-mismatched-contract.json"),
            "extra": self.fixture("invalid-extra-nested-key.json"),
        }
        duplicate = self.fixture("group-c-valid.json")
        duplicate["contracts"][1]["selector_ref"]["selector"] = duplicate["contracts"][0]["selector_ref"]["selector"]
        malformed["duplicate"] = duplicate
        for name, document in malformed.items():
            with self.subTest(name=name):
                with self.assertRaises(SchemaValidationError):
                    self.validate(document)


class OutcomeEpochBundleTests(unittest.TestCase):
    def test_c01_then_current_c02_and_old_c01_refusal(self):
        system = OutcomeSystemV1()
        c01 = {"schema": "outcome-epoch-bundle/v1", "bundle_id": "c01", "epoch_id": "C-01", "epoch_order": 1, "approved_objective_ref": {"identity": "objective", **{k: v for k, v in OBJECTIVE.items() if k != "version"}}, "approval_event_ref": {"identity": "approval", "path": "fixtures/a", "digest": "sha256:" + "e" * 64}, "current_objective_ref": {"identity": "current", "path": "fixtures/c", "digest": "sha256:" + "f" * 64}, "git_head": {"identity": "git-head", "commit": "a" * 40}, "accepted_refs": [], "unresolved_refs": [], "invalidated_refs": [], "next_refs": [{"identity": "C3", "path": "fixtures/c3", "digest": "sha256:" + "1" * 64}], "context_budget": {"target": 200000, "normal_limit": 300000, "absolute_limit": 500000}, "outcome_map_ref": {"identity": "map", "path": "fixtures/map", "digest": "sha256:" + "2" * 64}, "outcome_dag_ref": {"identity": "dag", "path": "fixtures/dag", "digest": "sha256:" + "3" * 64}, "review_ref": {"identity": "review", "path": "fixtures/review", "digest": "sha256:" + "4" * 64}, "validation_ref": {"identity": "validation", "path": "fixtures/validation", "digest": "sha256:" + "5" * 64}}
        c01 = system.compose_c01(c01)
        c02 = {**{k: v for k, v in c01.items() if k not in {"outcome_map_ref", "outcome_dag_ref", "review_ref", "validation_ref", "epoch_id", "epoch_order", "bundle_id"}}, "schema": "outcome-epoch-bundle/v1", "bundle_id": "c02", "epoch_id": "C-02", "epoch_order": 2, "c01_bundle_ref": {"identity": "C-01", "path": "fixtures/c01", "digest": digest(c01)}, "measurement_plan_ref": {"identity": "plan", "path": "fixtures/plan", "digest": "sha256:" + "6" * 64}, "measurement_targets_ref": {"identity": "targets", "path": "fixtures/targets", "digest": "sha256:" + "7" * 64}, "baseline_receipt_ref": {"identity": "baseline", "path": "fixtures/base", "digest": "sha256:" + "8" * 64}, "validation_ref": {"identity": "validation", "path": "fixtures/validation", "digest": "sha256:" + "9" * 64}}
        self.assertEqual("C-02", system.compose_c02(c02, c01)["epoch_id"])
        c02["c01_bundle_ref"]["digest"] = "sha256:" + "0" * 64
        self.assertEqual("blocked_stale_input", system.compose_c02(c02, c01)["code"])
