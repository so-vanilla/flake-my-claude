import copy
import hashlib
import json
import unittest
from pathlib import Path

from ai_agent_workflow.planning_system import PlanningSystemV1


ROOT = Path(__file__).resolve().parents[2]


def ref(name="artifact"):
    return {"path": "docs/planning/%s.md" % name, "version": "v1", "digest": "sha256:" + "a" * 64}


def source_ref(path):
    raw = (ROOT / path).read_bytes()
    return {"path": path, "version": "v1", "digest": "sha256:" + hashlib.sha256(raw).hexdigest()}


def digest(value):
    return "sha256:" + hashlib.sha256(json.dumps(value, sort_keys=True, separators=(",", ":")).encode()).hexdigest()


class PlanningTestCase(unittest.TestCase):
    def setUp(self):
        self.planner = PlanningSystemV1()
        self.authority = {"authority_ref": ref("authority"), "namespace": "fixture:planning", "scope": "fixture-only", "owner_ref": ref("owner")}
        self.head = {"revision": 1, "transaction_digest": "sha256:" + "b" * 64}

    def compile(self, selector, **inputs):
        inputs.setdefault("input_refs", [ref("input")])
        return self.planner.compile(selector, inputs, self.authority, self.head)

    def assert_refusal(self, result, code):
        self.assertEqual("planning-system-refusal/v1", result["schema"])
        self.assertEqual(code, result["reason"])

    @staticmethod
    def spec():
        return {key: "defined" for key in ("behavior", "scenarios", "capabilities", "constraints", "non_goals", "edges", "errors", "acceptance")}


class D1_ProfileSelectionTests(PlanningTestCase):
    def test_validated_manifest_and_execution_policy_receipt(self):
        manifest = json.loads((ROOT / "agent-workflows/workflows/feature-bounded.json").read_text())
        result = self.compile("group.D.D1", domain="software", workflow_id="feature-bounded", workflow_manifest_ref=source_ref("agent-workflows/workflows/feature-bounded.json"))
        self.assertEqual("workflow_profile", result["output"]["kind"])
        self.assertEqual(manifest["execution_policy"]["digest"], result["output"]["execution_policy_digest"])

    def test_unresolved_domain_and_legacy_owner_refuse(self):
        self.assert_refusal(self.compile("group.D.D1", domain="unresolved"), "needs_user_domain")
        self.assert_refusal(self.compile("group.D.D1", domain="software", lifecycle_owner="AI-DLC"), "blocked_lifecycle_owner_conflict")


class D2_CurrentSystemTests(PlanningTestCase):
    def test_requires_all_sourced_inventory_surfaces(self):
        areas = ("entrypoints", "domain_terms", "data_control_flows", "external_contracts", "tests", "constraints", "prior_decisions", "verification_surfaces")
        good = {area: {"source_refs": [ref(area)]} for area in areas}
        self.assertEqual("current_system", self.compile("group.D.D2", inventory=good)["output"]["kind"])
        good.pop("tests")
        self.assert_refusal(self.compile("group.D.D2", inventory=good), "blocked_unsourced_gap")


class D3_PracticeDiscoveryTests(PlanningTestCase):
    def test_constraints_need_multiple_sources_without_conflict(self):
        result = self.compile("group.D.D3", guidance_sources=[ref("ci"), ref("team")], constraints=[{"source_refs": [ref("ci")], "rule": "test"}])
        self.assertEqual("global_constraints", result["output"]["kind"])
        self.assert_refusal(self.compile("group.D.D3", guidance_sources=[ref()], constraints=[{"source_refs": [ref()]}]), "blocked_unsupported_convention")


class D4_SpecificationTests(PlanningTestCase):
    def test_spec_keeps_technical_choices_and_placeholders_out(self):
        self.assertEqual("what_why_specification", self.compile("group.D.D4", specification=self.spec())["output"]["kind"])
        bad = self.spec(); bad["behavior"] = "TODO decide"
        self.assert_refusal(self.compile("group.D.D4", specification=bad), "blocked_ambiguous_specification")


class D5_OptionExplorationTests(PlanningTestCase):
    def test_unselected_options_have_status_quo_and_falsification(self):
        options = [{"name": "a", "falsification_condition": "fails"}, {"name": "b", "falsification_condition": "cost"}]
        self.assertEqual("unapproved_options", self.compile("group.D.D5", options=options, status_quo={"name": "none"}, recommendation="a")["output"]["kind"])
        self.assert_refusal(self.compile("group.D.D5", options=options, status_quo={}, recommendation="a", selected_option="a"), "blocked_implicit_approval")


class D6_SolutionDesignTests(PlanningTestCase):
    def test_design_requires_explicit_human_approved_option(self):
        design = {key: "defined" for key in ("architecture", "responsibilities", "interfaces", "flow", "errors", "compatibility", "migration", "observability", "security", "test_seams")}
        receipt = {"receipt_ref": ref("approval"), "source": "human", "explicit": True, "decision": "approve", "scope": "option-selection", "actor_ref": self.authority["owner_ref"]}
        self.assertEqual("solution_design", self.compile("group.D.D6", approved_option_receipt=receipt, design=design)["output"]["kind"])
        self.assert_refusal(self.compile("group.D.D6", design=design), "blocked_missing_approved_option")


class D7_ContractDesignTests(PlanningTestCase):
    def test_owned_contracts_reject_shared_mutable_write(self):
        base = {"owner": "team", "version": "v1", "consumes": [], "produces": [], "schema_ref": ref("schema"), "fixture_refs": [ref("fixture")], "failures": [], "idempotency": "yes", "backward_compatibility": "yes"}
        one = dict(base, mutable_writes=["a"]); two = dict(base, mutable_writes=["b"])
        self.assertEqual("versioned_contracts", self.compile("group.D.D7", contracts=[one, two])["output"]["kind"])
        self.assert_refusal(self.compile("group.D.D7", contracts=[one, dict(base, mutable_writes=["a"])]), "blocked_inseparable_write_overlap")


class D8_TaskDecompositionTests(PlanningTestCase):
    def test_exact_task_contract_rejects_placeholder_component(self):
        task = {"task_id": "T1", "files": ["x.py"], "interface": "parse", "inputs": [], "outputs": [], "checks": ["unit"], "stop": "stop", "report_path": ".local/r.md", "parent_outcome_contribution": "O1"}
        self.assertEqual("task_decomposition", self.compile("group.D.D8", tasks=[task])["output"]["kind"])
        self.assert_refusal(self.compile("group.D.D8", tasks=[dict(task, interface="component")]), "blocked_vague_task")


class D9_ExecutionDAGTests(PlanningTestCase):
    def test_cycles_refuse_and_overlapping_parallel_is_serialized(self):
        tasks = [{"task_id": "A", "files": ["a"]}, {"task_id": "B", "files": ["a"]}]
        result = self.compile("group.D.D9", tasks=tasks, edges=[], parallel_batches=[["A", "B"]])
        self.assertEqual([["A", "B"]], result["output"]["serialized_batches"])
        self.assert_refusal(self.compile("group.D.D9", tasks=tasks, edges=[{"from": "A", "to": "B"}, {"from": "B", "to": "A"}]), "blocked_dependency_cycle")


class D10_WorkerBriefTests(PlanningTestCase):
    def test_briefs_are_narrow_and_physical(self):
        brief = {"task_id": "T1", "purpose_ref": ref("purpose"), "task": "do", "interface": "api", "write_scope": ["x"], "checks": ["unit"], "stop": "stop", "report_path": ".local/r", "exploration_refs": [ref("explore")]}
        self.assertEqual("worker_briefs", self.compile("group.D.D10", briefs=[brief])["output"]["kind"])
        self.assert_refusal(self.compile("group.D.D10", briefs=[dict(brief, whole_plan="copy")]), "blocked_ambiguous_brief")


class D11_VerificationRecoveryTests(PlanningTestCase):
    def test_finite_budgets_and_distinct_gates(self):
        gates = {key: {"required": True} for key in ("test", "review", "finding_validation", "e2e", "dry_run", "rollback", "post_check", "activation", "git", "external")}
        result = self.compile("group.D.D11", task_budgets=[{"wall_clock_minutes": 10, "review_rounds": 1, "fix_attempts": 5}], gates=gates)
        self.assertEqual("verification_recovery", result["output"]["kind"])
        self.assert_refusal(self.compile("group.D.D11", task_budgets=[{"wall_clock_minutes": 0, "review_rounds": 1, "fix_attempts": 6}], gates=gates), "blocked_unbounded_budget")


class D12_ReadinessReviewTests(PlanningTestCase):
    def test_not_ready_routes_upstream_and_accepted_risk_is_distinct(self):
        result = self.compile("group.D.D12", contradictions=["spec/design"], evidence_refs=[ref("evidence")], upstream_route="group.D.D4")
        self.assertEqual("not_ready", result["output"]["status"])
        result = self.compile("group.D.D12", evidence_refs=[ref("evidence")], open_risks=[{"accepted": True, "receipt_ref": ref("risk")}])
        self.assertEqual("ready_with_accepted_risks", result["output"]["status"])


class PlanningEpochTests(PlanningTestCase):
    def bundle(self, epoch, receipts):
        return {"schema": "planning-epoch-bundle/v1", "bundle_id": epoch + "-1", "epoch_id": epoch, "authority_digest": "sha256:" + "c" * 64, "canonical_refs": [ref(epoch)], "selector_receipts": {item: ref(item) for item in receipts}, "acceptance_evidence": [ref("evidence")], "approved_decisions": [], "unresolved": [], "invalidated": [], "next_inputs": [ref("next")]}

    def test_d01_d02_d03_handoff_and_stale_predecessor(self):
        d01 = self.bundle("D-01", ("D1", "D2", "D3", "D4")); self.assertEqual("planning-epoch-bundle/v1", self.planner.compose_epoch(d01)["schema"])
        d02 = self.bundle("D-02", ("D5", "D6", "D7")); d02["predecessor_ref"] = {"epoch_id": "D-01", "path": ".local/d01.json", "digest": digest(d01)}
        self.assertEqual("planning-epoch-bundle/v1", self.planner.compose_epoch(d02, d01)["schema"])
        d03 = self.bundle("D-03", ("D8", "D9", "D10", "D11", "D12")); d03["predecessor_ref"] = {"epoch_id": "D-02", "path": ".local/d02.json", "digest": digest(d02)}
        self.assertEqual("planning-epoch-bundle/v1", self.planner.compose_epoch(d03, d02)["schema"])
        d02["predecessor_ref"]["digest"] = "sha256:" + "d" * 64
        self.assertEqual("blocked_stale_predecessor", self.planner.compose_epoch(d02, d01)["code"])

    def test_compile_does_not_mutate_input_aliases(self):
        source = {"specification": self.spec()}; original = copy.deepcopy(source)
        self.compile("group.D.D4", **source)
        self.assertEqual(original, source)

    def test_stale_head_and_requested_side_effect_are_refused(self):
        self.assert_refusal(self.compile("group.D.D4", specification=self.spec(), observed_head={"revision": 0}), "blocked_stale_head")
        self.assert_refusal(self.compile("group.D.D4", specification=self.spec(), dispatch=True), "blocked_side_effect")

    def test_success_requires_physical_inputs_and_context_is_strict(self):
        with self.assertRaises(ValueError):
            self.planner.compile("group.D.D4", {"specification": self.spec()}, self.authority, self.head)
        with self.assertRaises(ValueError):
            self.planner.compile("group.D.D4", {"input_refs": [ref()], "specification": self.spec()}, self.authority, {"x": 1})

    def test_d1_refuses_manifest_digest_or_workflow_drift(self):
        bad = source_ref("agent-workflows/workflows/feature-bounded.json"); bad["digest"] = "sha256:" + "0" * 64
        self.assert_refusal(self.compile("group.D.D1", domain="software", workflow_id="feature-bounded", workflow_manifest_ref=bad), "blocked_unvalidated_workflow")
        self.assert_refusal(self.compile("group.D.D1", domain="software", workflow_id="wrong", workflow_manifest_ref=source_ref("agent-workflows/workflows/feature-bounded.json")), "blocked_unknown_workflow")

    def test_d6_forged_receipt_and_d12_missing_evidence_cannot_pass(self):
        design = {key: "defined" for key in ("architecture", "responsibilities", "interfaces", "flow", "errors", "compatibility", "migration", "observability", "security", "test_seams")}
        forged = {"receipt_ref": ref("approval"), "source": "human", "explicit": True, "decision": "reject", "scope": "option-selection", "actor_ref": self.authority["owner_ref"]}
        self.assert_refusal(self.compile("group.D.D6", approved_option_receipt=forged, design=design), "blocked_missing_approved_option")
        minimal = {"receipt_ref": ref("approval"), "source": "human", "explicit": True}
        self.assert_refusal(self.compile("group.D.D6", approved_option_receipt=minimal, design=design), "blocked_missing_approved_option")
        result = self.compile("group.D.D12")
        self.assertEqual("not_ready", result["output"]["status"])
