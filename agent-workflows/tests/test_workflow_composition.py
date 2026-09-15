from __future__ import annotations

import copy
import hashlib
import json
import unittest
from pathlib import Path
from unittest.mock import patch

from ai_agent_workflow.workflow_composition import (
    WorkflowCompositionError,
    WorkflowCompositionV1,
)

ROOT = Path(__file__).resolve().parents[2]
WORKFLOWS = ROOT / "agent-workflows" / "workflows"


class WorkflowCompositionTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.validator = WorkflowCompositionV1(source_root=ROOT)

    @staticmethod
    def _load(name: str) -> dict:
        return json.loads((WORKFLOWS / (name + ".json")).read_text(encoding="utf-8"))

    def _stage(self, stage_id, kind, scope, selectors, *, mode="serial", host=None):
        return {
            "stage_id": stage_id,
            "kind": kind,
            "scope": scope,
            "mode": mode,
            "host_selector": host,
            "selectors": list(selectors),
        }

    def _closure(self, suffix):
        return self._stage(
            "close-" + suffix,
            "closure",
            "shared-closure",
            ["group.F.F%d" % number for number in range(1, 8)],
        )

    def _normal(self, *, profile=None, include_g_h=False):
        self.assertIn(profile, (None, "feature"))
        _ = include_g_h
        return self._bind_current_policy_digest(
            self._load("feature-bounded" if profile == "feature" else "documentation-change")
        )

    @staticmethod
    def _bind_current_policy_digest(document):
        reference = document.get("execution_policy")
        if reference is not None:
            policy_path = ROOT / reference["path"]
            raw = policy_path.read_bytes()
            reference["digest"] = "sha256:" + hashlib.sha256(raw).hexdigest()
        return document

    def _policy_mutation(self, document, mutate):
        policy_path = ROOT / document["execution_policy"]["path"]
        policy = json.loads(policy_path.read_text(encoding="utf-8"))
        mutate(policy)
        raw = json.dumps(policy, sort_keys=True, separators=(",", ":")).encode("utf-8")
        document["execution_policy"]["digest"] = "sha256:" + hashlib.sha256(raw).hexdigest()
        with patch.object(Path, "read_bytes", return_value=raw):
            self.validator.validate(document)

    def test_three_special_manifests_and_complete_directory_validate(self):
        expected = {
            "bootstrap-new-workflow": ("bootstrap", 1),
            "resume-interrupted-work": ("resume", 0),
            "execution-exception-arbitration": ("exception-resume", 0),
        }
        for workflow_id, (entry_mode, closure_count) in expected.items():
            with self.subTest(workflow_id=workflow_id):
                receipt = self.validator.validate_file(WORKFLOWS / (workflow_id + ".json"))
                self.assertEqual("passed", receipt["result"])
                self.assertEqual(entry_mode, receipt["entry_mode"])
                self.assertEqual(closure_count, receipt["closure_count"])
                self.assertTrue(receipt["source_only"])
        suite = self.validator.validate_directory(WORKFLOWS)
        self.assertEqual("passed", suite["result"])
        self.assertEqual(18, suite["manifest_count"])
        self.assertTrue(set(expected).issubset(suite["workflow_ids"]))

    def test_receipts_are_deterministic_for_document_and_suite_order(self):
        first = self._normal(profile="feature")
        second = self._load("resume-interrupted-work")
        self.assertEqual(self.validator.validate(first), self.validator.validate(copy.deepcopy(first)))
        forward = self.validator.validate_suite([first, second])
        reverse = self.validator.validate_suite([second, first])
        self.assertEqual(forward, reverse)

    def test_required_only_policy_compiles_two_positive_paths_and_rejects_representative_drift(self):
        feature = self._bind_current_policy_digest(self._load("feature-bounded"))
        receipt = self.validator.validate(feature)
        self.assertEqual(receipt, self.validator.validate(copy.deepcopy(feature)))
        policy = receipt["execution_policy"]
        self.assertEqual("v1", policy["version"])
        self.assertEqual(feature["execution_policy"]["path"], policy["path"])
        self.assertEqual(feature["execution_policy"]["digest"], policy["digest"])
        self.assertEqual(
            {
                "contract_version": "workflow-loop/v1",
                "phase": "E3-E7",
                "additional_iteration_limit": 3,
                "technical_retry_limit": 1,
                "terminal_statuses": [
                    "completed",
                    "needs-input",
                    "stalled",
                    "iteration-limit",
                    "execution-failed",
                    "recovery-required",
                ],
                "completion": "required-evidence-and-independent-review",
            },
            policy["loop_control"],
        )
        transitions = policy["compiled_conditional_transitions"]
        terminal_transitions = [
            item for item in transitions if item.get("from") == "any-active-E-state"
        ]
        self.assertEqual(
            [{"from": "any-active-E-state", "when": "loop-control-terminal", "terminal_statuses": policy["loop_control"]["terminal_statuses"], "dispatch": False}],
            terminal_transitions,
        )
        self.assertIn(
            {"from": "group.E.E6", "when": "no-open-required-needs-user-or-unresolved", "to": "group.E.E8"},
            transitions,
        )
        self.assertIn(
            {"from": "group.E.E6", "when": "open-required-without-needs-user-or-unresolved", "to": "group.E.E7"},
            transitions,
        )
        self.assertIn(
            {"from": "group.E.E7", "when": "required-resolution-claim-and-fresh-rereview", "to": "group.E.E6", "fresh": True},
            transitions,
        )

        missing = copy.deepcopy(feature)
        missing.pop("execution_policy")
        drift = copy.deepcopy(feature)
        drift["execution_policy"]["digest"] = "sha256:" + "0" * 64
        special = self._load("resume-interrupted-work")
        special["execution_policy"] = copy.deepcopy(feature["execution_policy"])
        for document in (missing, drift, special):
            with self.assertRaises(WorkflowCompositionError):
                self.validator.validate(document)
        for mutate in (
            lambda policy: policy["resolution"].update({"fresh_rereview": False}),
            lambda policy: policy["resolution"].update({"return_to_selector": "group.E.E8"}),
            lambda policy: policy.pop("loop_control"),
        ):
            with self.assertRaises(WorkflowCompositionError):
                self._policy_mutation(copy.deepcopy(feature), mutate)

    def test_rejects_selector_group_closure_and_e_order_bypasses(self):
        cases = {}
        unknown = self._load("resume-interrupted-work")
        unknown["stages"][0]["selectors"] = ["group.F.F9"]
        cases["unknown-selector"] = unknown
        common_order = self._normal()
        common_order["stages"][0]["selectors"][0:2] = reversed(common_order["stages"][0]["selectors"][0:2])
        cases["common-order"] = common_order
        missing_closure = self._normal()
        missing_closure["stages"].pop(1)
        cases["missing-closure"] = missing_closure
        f8_closure = self._normal()
        f8_closure["stages"][1]["selectors"][-1] = "group.F.F8"
        cases["f8-as-closure"] = f8_closure
        serial_reviews = self._normal()
        next(stage for stage in serial_reviews["stages"] if "group.E.E4" in stage["selectors"])["mode"] = "serial"
        cases["serial-e4-e5"] = serial_reviews
        bypass_e6 = self._normal()
        converge = next(stage for stage in bypass_e6["stages"] if "group.E.E6" in stage["selectors"])
        converge["selectors"][0], converge["selectors"][1] = converge["selectors"][1], converge["selectors"][0]
        cases["e7-before-e6"] = bypass_e6
        e10 = self._normal()
        next(stage for stage in e10["stages"] if "group.E.E6" in stage["selectors"])["selectors"].append("group.E.E10")
        cases["e10-normal"] = e10
        for name, document in cases.items():
            with self.subTest(name=name), self.assertRaises(WorkflowCompositionError):
                self.validator.validate(document)

    def test_rejects_profile_order_completeness_host_and_parallel_bypasses(self):
        cases = {}
        incomplete = self._normal(profile="feature")
        next(stage for stage in incomplete["stages"] if stage["stage_id"] == "feature-contract")["selectors"].pop()
        cases["incomplete"] = incomplete
        reordered = self._normal(profile="feature")
        values = next(stage for stage in reordered["stages"] if stage["stage_id"] == "feature-contract")["selectors"]
        values[0], values[1] = values[1], values[0]
        cases["reordered"] = reordered
        wrong_host = self._normal(profile="feature")
        next(stage for stage in wrong_host["stages"] if stage["stage_id"] == "feature-architecture")["host_selector"] = "group.D.D4"
        cases["wrong-host"] = wrong_host
        parallel = self._normal(profile="feature")
        next(stage for stage in parallel["stages"] if stage["stage_id"] == "feature-contract")["mode"] = "parallel"
        cases["parallel-profile"] = parallel
        for name, document in cases.items():
            with self.subTest(name=name), self.assertRaises(WorkflowCompositionError):
                self.validator.validate(document)

    def test_rejects_missing_objective_decision_outcome_effect_and_risk_gates(self):
        cases = {}
        objective = self._normal()
        objective["required_human_gates"] = []
        cases["objective"] = objective
        decision = self._normal(include_g_h=True)
        decision["required_human_gates"].remove("decision-promotion-approval")
        cases["decision"] = decision
        outcome = self._normal(include_g_h=True)
        outcome["required_human_gates"].remove("run-outcome-approval")
        cases["outcome"] = outcome
        external = self._normal()
        external["planned_effect"] = "external-operation"
        cases["external"] = external
        protected = self._normal()
        protected["risk_class"] = "protected"
        cases["protected"] = protected
        bootstrap = self._load("bootstrap-new-workflow")
        bootstrap["required_human_gates"].remove("migration-approval")
        cases["migration"] = bootstrap
        exception = self._load("execution-exception-arbitration")
        exception["required_human_gates"] = []
        cases["exception-risk"] = exception
        for name, document in cases.items():
            with self.subTest(name=name), self.assertRaises(WorkflowCompositionError):
                self.validator.validate(document)

    def test_exception_manifest_uses_evidence_bounded_gates(self):
        document = self._load("execution-exception-arbitration")
        self.assertEqual(["risk-acceptance"], document["required_human_gates"])
        self.assertIn("evidence-bounded", document["title"])
        receipt = self.validator.validate(document)
        self.assertEqual(["risk-acceptance"], receipt["required_human_gates"])

    def test_f8_is_resume_only_dynamic_and_e10_follows_it(self):
        wrong_completion = self._load("resume-interrupted-work")
        wrong_completion["completion"] = {"mode": "closed", "terminal_selector": "group.F.F8"}
        reversed_exception = self._load("execution-exception-arbitration")
        reversed_exception["stages"].reverse()
        for document in (wrong_completion, reversed_exception):
            with self.assertRaises(WorkflowCompositionError):
                self.validator.validate(document)

    def test_rejects_named_identity_scope_effect_and_domain_gate_drift(self):
        cases = {}
        feature = self._load("feature-bounded")
        feature["intent_class"] = "documentation"
        feature["profile"] = None
        feature["required_human_gates"].remove("design-approval")
        feature["stages"] = [stage for stage in feature["stages"] if stage["kind"] != "profile"]
        cases["feature-relabelled"] = feature
        missing_c = self._load("feature-bounded")
        missing_c["stages"] = [
            stage for stage in missing_c["stages"]
            if stage["scope"] != "C" and stage["stage_id"] != "close-outcomes"
        ]
        cases["feature-missing-c"] = missing_c
        external = self._load("external-operation-plan")
        external["intent_class"] = "research"
        external["planned_effect"] = "none"
        external["risk_class"] = "standard"
        external["required_human_gates"] = [
            gate for gate in external["required_human_gates"]
            if gate not in {"external-operation-approval", "risk-acceptance"}
        ]
        cases["external-downgrade"] = external
        for workflow_id, gate in (
            ("configuration-dotfiles-source", "dotfiles-ownership-approval"),
            ("company-governed-change", "company-policy-approval"),
            ("incident-response", "incident-authority"),
            ("migration-cutover-plan", "migration-approval"),
        ):
            document = self._load(workflow_id)
            document["required_human_gates"].remove(gate)
            cases[workflow_id + "-gate"] = document
        for name, document in cases.items():
            with self.subTest(name=name), self.assertRaises(WorkflowCompositionError):
                self.validator.validate(document)

    def test_rejects_special_routes_inside_a_normal_workflow(self):
        resume = self._load("documentation-change")
        resume["stages"].append(self._stage(
            "resume-mid-run", "resume", "resume", ["group.F.F8"]
        ))
        exception = self._load("documentation-change")
        exception["stages"].append(self._stage(
            "exception-mid-run", "exception", "execution-exception",
            ["group.E.E10"], host="group.F.F8",
        ))
        for document in (resume, exception):
            with self.assertRaises(WorkflowCompositionError):
                self.validator.validate(document)

    def test_rejects_parallel_dependent_common_groups(self):
        for scope in ("C", "D", "G", "H"):
            with self.subTest(scope=scope):
                document = self._load("documentation-change")
                next(stage for stage in document["stages"] if stage["scope"] == scope)["mode"] = "parallel"
                with self.assertRaises(WorkflowCompositionError):
                    self.validator.validate(document)

    def test_rejects_profile_step_delayed_beyond_its_host_boundary(self):
        document = self._load("feature-bounded")
        index = next(
            i for i, stage in enumerate(document["stages"])
            if stage["stage_id"] == "feature-architecture"
        )
        stage = document["stages"].pop(index)
        destination = next(
            i for i, item in enumerate(document["stages"])
            if item["stage_id"] == "planning-readiness"
        )
        document["stages"].insert(destination + 1, stage)
        with self.assertRaises(WorkflowCompositionError):
            self.validator.validate(document)


if __name__ == "__main__":
    unittest.main()
