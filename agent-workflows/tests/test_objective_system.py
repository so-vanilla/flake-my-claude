import copy
import unittest
from pathlib import Path
import sys

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "src"))

from ai_agent_workflow.objective_system import ObjectiveSystemV1, ObjectiveSystemError  # noqa: E402


def ref(name, selector="source"):
    return {"path": "agent-workflows/tests/fixtures/s2/objective/core/" + name + ".json", "digest": "sha256:" + "a" * 64, "selector": selector}


HEAD = {"revision": 1, "transaction_digest": "sha256:" + "b" * 64}
AUTHORITY = {"owner_ref": {"owner_kind": "human", "owner_id": "fixture-owner", "authority_ref": ref("owner", "authority")}, "source_refs": [ref("source")]}


class ObjectiveSystemTests(unittest.TestCase):
    def compile(self, ident, inputs, authority=AUTHORITY, head=HEAD):
        return ObjectiveSystemV1().compile(ident, inputs, authority, head)

    def artifact_inputs(self, **extra):
        result = {"input_refs": [ref("input")], "candidate_ref": ref("candidate", "candidate"), "version": "v001"}
        result.update(extra)
        return result


class B1_EntryTests(ObjectiveSystemTests):
    def test_actual_kernel_genesis_contract_and_invalid_cross_products(self):
        genesis = {"revision": 0, "transaction_digest": None}
        out = self.compile("group.B.B1", self.artifact_inputs(), head=genesis)
        self.assertEqual(out["expected_head"], genesis)
        for bad in ({"revision": 1, "transaction_digest": None},
                    {"revision": 0, "transaction_digest": HEAD["transaction_digest"]}):
            with self.assertRaises(ObjectiveSystemError):
                self.compile("group.B.B1", self.artifact_inputs(), head=bad)

    def test_separates_raw_interpretation_assumption_and_unknown(self):
        out = self.compile("group.B.B1", self.artifact_inputs(raw="fix it", interpretation="repair", assumptions=["repo"], unknowns=["scope"]))
        self.assertEqual(out["schema"], "objective-system-artifact/v1")
        self.assertEqual(out["candidate"]["candidate_ref"]["selector"], "B1.raw-interpretation-assumption-unknown")

    def test_duplicate_run_refuses(self):
        self.assertEqual(self.compile("group.B.B1", self.artifact_inputs(active_duplicate=True))["reason"], "needs_user_duplicate_run")


class B2_DiscoverContextTests(ObjectiveSystemTests):
    def test_facts_require_sources(self):
        self.assertEqual(self.compile("group.B.B2", self.artifact_inputs(facts=[{"text": "x"}]))["reason"], "blocked_missing_authority")
        out = self.compile("group.B.B2", self.artifact_inputs(facts=[{"text": "x", "source_ref": ref("fact")}]))
        self.assertEqual(out["candidate"]["candidate_ref"]["selector"], "B2.sourced-context")


class B3_ClassifyScopeTests(ObjectiveSystemTests):
    def test_risky_quick_and_unknown_owner_refuse(self):
        for values in ({"operation": "quick", "reversibility": "risky"}, {"owner": "unknown"}):
            self.assertEqual(self.compile("group.B.B3", self.artifact_inputs(**values))["reason"], "blocked_missing_authority")

    def test_classifies_required_dimensions(self):
        out = self.compile("group.B.B3", self.artifact_inputs(depth="deep", operation="change", ownership="personal", reversibility="reversible"))
        self.assertEqual(out["candidate"]["candidate_ref"]["selector"], "B3.scope-classified")


class B4_GrillPurposeTests(ObjectiveSystemTests):
    def test_resolved_inquiry_needs_reason_and_physical_evidence(self):
        values = self.artifact_inputs(material_unknowns=[], inquiry_complete=True,
                                     resolution_reason="No critical unknown remains in supplied context",
                                     resolution_refs=[ref("context")])
        out = self.compile("group.B.B4", values)
        self.assertEqual(out["candidate"]["candidate_ref"]["selector"], "B4.inquiry-resolved")
        for field, bad in (("resolution_refs", []), ("resolution_reason", ""), ("inquiry_complete", False)):
            changed = {**values, field: bad}
            self.assertEqual(self.compile("group.B.B4", changed)["reason"], "needs_user_purpose")

    def test_exactly_one_material_nondiscoverable_question(self):
        out = self.compile("group.B.B4", self.artifact_inputs(material_unknowns=["why"]))
        self.assertEqual(out["candidate"]["candidate_ref"]["selector"], "B4.one-material-question")
        self.assertEqual(self.compile("group.B.B4", self.artifact_inputs(material_unknowns=[]))["reason"], "needs_user_purpose")


class B5_ProposePurposeTests(ObjectiveSystemTests):
    def test_two_or_three_distinguishable_unselected_options(self):
        options = [{"id": "a", "tradeoff": "fast"}, {"id": "b", "tradeoff": "safe"}]
        out = self.compile("group.B.B5", self.artifact_inputs(options=options))
        self.assertEqual(out["candidate"]["candidate_ref"]["selector"], "B5.unselected-options")
        self.assertEqual(self.compile("group.B.B5", self.artifact_inputs(options=options, selected_option="a"))["reason"], "blocked_implicit_approval")


class B6_FeasibilityTests(ObjectiveSystemTests):
    def test_separates_constraint_kinds_and_needs_authority(self):
        constraints = {"hard": ["budget"], "soft": ["style"], "assumption": ["time"], "open": ["owner"]}
        out = self.compile("group.B.B6", self.artifact_inputs(constraints=constraints))
        self.assertEqual(out["candidate"]["candidate_ref"]["selector"], "B6.constraints-separated")
        self.assertEqual(self.compile("group.B.B6", self.artifact_inputs(constraints=constraints, owner="unknown"))["reason"], "blocked_missing_authority")


class B7_ApproveObjectiveTests(ObjectiveSystemTests):
    def test_only_explicit_fixture_human_receipt_constructs_command(self):
        candidate = {"path": "fixture/candidate.md", "version": "v002", "digest": "sha256:" + "c" * 64, "namespace": "fixture:core"}
        prior = {"version": "v001", "digest": "sha256:" + "d" * 64}
        receipt = {"receipt_ref": ref("receipt", "human-receipt"), "source": "human", "explicit": True, "decision": "approve", "actor_ref": AUTHORITY["owner_ref"]}
        inputs = self.artifact_inputs(candidate=candidate, prior_objective=prior, approval_receipt=receipt, namespace="fixture:core", approval_scope="fixture-only")
        out = self.compile("group.B.B7", inputs)
        self.assertEqual(out["schema"], "objective-approval-command/v1")
        for bad in ({}, {**receipt, "source": "ai"}, {**receipt, "source": "task-start"}):
            changed = copy.deepcopy(inputs); changed["approval_receipt"] = bad
            self.assertIn(self.compile("group.B.B7", changed)["reason"], {"blocked_implicit_approval", "blocked_fixture_live"})

    def test_live_fixture_and_stale_head_refuse_and_inputs_are_unchanged(self):
        receipt = {"receipt_ref": ref("receipt", "human-receipt"), "source": "human", "explicit": True, "decision": "approve", "actor_ref": AUTHORITY["owner_ref"]}
        values = self.artifact_inputs(namespace="live:run", approval_scope="live", approval_receipt=receipt)
        before = copy.deepcopy(values)
        self.assertEqual(self.compile("group.B.B7", values)["reason"], "blocked_fixture_live")
        self.assertEqual(values, before)
        stale = self.artifact_inputs(observed_head=HEAD)
        self.assertEqual(self.compile("group.B.B7", stale, head={"revision": 1, "transaction_digest": "sha256:" + "e" * 64})["reason"], "blocked_stale_head")

    def test_deterministic_and_unknown_selector_refuses(self):
        values = self.artifact_inputs()
        self.assertEqual(self.compile("group.B.B1", values), self.compile("group.B.B1", values))
        self.assertEqual(self.compile("group.B.BAD", values)["reason"], "blocked_missing_authority")
