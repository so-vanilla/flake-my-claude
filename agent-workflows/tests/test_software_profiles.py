from __future__ import annotations

import copy
import hashlib
import json
import tempfile
import unittest
from pathlib import Path

from ai_agent_workflow.software_profiles import SoftwareProfileV1


ROOT = Path(__file__).resolve().parents[2]
HEAD = {"revision": 41, "transaction_digest": "sha256:" + "a" * 64}
SELECTORS = {
    "feature": tuple("profile.feature.F%d" % number for number in range(1, 9)),
    "bug-fix": tuple("profile.bug-fix.BGF%d" % number for number in range(1, 9)),
    "improvement": tuple("profile.improvement.I%d" % number for number in range(1, 8)),
}
HOSTS = {
    "feature": ("group.D.D4", "group.D.D4", "group.D.D4", "group.D.D6", "group.E.E3", "group.E.E3", "group.E.E3", "group.E.E9"),
    "bug-fix": ("group.D.D2", "group.D.D2", "group.D.D2", "group.D.D2", "group.D.D2", "group.D.D5", "group.E.E3", "group.E.E9"),
    "improvement": ("group.D.D2", "group.D.D2", "group.D.D2", "group.D.D6", "group.E.E3", "group.E.E9", "group.E.E9"),
}


def canonical_digest(value):
    raw = json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=True).encode()
    return "sha256:" + hashlib.sha256(raw).hexdigest()


class SoftwareProfileTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.reference_root = Path(self.temp.name)
        self.compiler = SoftwareProfileV1(source_root=ROOT, reference_root=self.reference_root)
        self.authority_ref = self.write("context/authority.json", {"schema": "profile-authority/v1", "authority_id": "authority-1"})
        self.owner_ref = self.write("context/owner.json", {"schema": "profile-owner/v1", "owner_id": "human-owner"})
        self.authority = {
            "authority_ref": self.authority_ref,
            "namespace": "software-profile-test",
            "scope": "fixture-only",
            "owner_ref": self.owner_ref,
        }
        self.host_input_ref = self.write("context/host-input.json", {"schema": "host-input/v1", "accepted": True})
        self.generic_ref = self.write("evidence/generic.json", {"schema": "profile-evidence/v1", "observed": True})

    def tearDown(self):
        self.temp.cleanup()

    def write(self, relative, document):
        path = self.reference_root / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        raw = (json.dumps(document, indent=2, sort_keys=True) + "\n").encode()
        path.write_bytes(raw)
        return {"path": relative, "version": document["schema"], "digest": "sha256:" + hashlib.sha256(raw).hexdigest()}

    def repo_ref(self, relative):
        raw = (ROOT / relative).read_bytes()
        document = json.loads(raw)
        return {"path": relative, "version": document["schema"], "digest": "sha256:" + hashlib.sha256(raw).hexdigest()}

    def host_ref(self, selector, *, wrong=False):
        profile = selector.split(".")[1]
        host = HOSTS[profile][SELECTORS[profile].index(selector)]
        qualified_id = "group.D.D1" if wrong else host
        if host.startswith("group.D"):
            document = {
                "schema": "planning-system-artifact/v1", "qualified_id": qualified_id,
                "authority": self.authority, "expected_head": HEAD,
                "input_refs": [self.host_input_ref], "output": {"kind": "accepted-test-host"},
            }
        else:
            document = {
                "schema": "execution-group-candidate/v1", "qualified_id": qualified_id,
                "loop_level": "artifact", "expected_head": HEAD,
                "authority_ref": {"authority_id": "authority-1", "lease_id": "lease-1", "epoch_id": "epoch-1"},
                "non_mutating": True, "stage": host.rsplit(".", 1)[-1],
            }
            if host == "group.E.E3":
                document.update({"status": "submitted", "artifact_candidate": {}, "recovery": {}})
            else:
                document.update({"status": "verified", "new_findings_route": "E6"})
            document["candidate_digest"] = canonical_digest(document)
        return self.write("hosts/%s-%s.json" % (profile, selector.rsplit(".", 1)[-1]), document)

    def make_test_receipt(self, name, *, level, subject, phase="after", status="passed", exit_code=0, causal_chain_id=None, equivalent=False):
        document = {
            "schema": "test-execution-receipt/v1", "test_id": name, "level": level,
            "subject": subject, "phase": phase, "command": {"argv": ["test", name]},
            "environment": {"kind": "equivalent-test"}, "terminal": True,
            "status": status, "exit_code": exit_code, "capture": {"stdout": name},
        }
        if causal_chain_id is not None:
            document["causal_chain_id"] = causal_chain_id
        if equivalent:
            document["equivalent_environment"] = True
        return self.write("evidence/%s.json" % name, document)

    def evidence(self, selector):
        step = selector.rsplit(".", 1)[-1]
        refs = [self.generic_ref]
        if selector == "profile.feature.F1":
            value = {"actor": "member", "trigger": "select publish", "new_behavior": "publish a draft", "observable_outcome": "draft is visible"}
        elif selector == "profile.feature.F2":
            value = {"examples": [{"id": kind, "kind": kind, "scenario": kind + " scenario", "expected_result": kind + " result"} for kind in ("happy", "boundary", "permission", "error", "retry")]}
        elif selector == "profile.feature.F3":
            fixture = self.write("evidence/feature-fixture.json", {"schema": "feature-fixture/v1", "input": "draft"}); refs.append(fixture)
            value = {"input_contract": {"draft": "string"}, "output_contract": {"id": "string"}, "state_transitions": [{"from": "draft", "event": "publish", "to": "published"}], "compatibility": {"old_client": "supported"}, "accessibility": ["keyboard path"], "fixture_refs": [fixture]}
        elif selector == "profile.feature.F4":
            value = {"responsibilities": {"publisher": "owns publication"}, "seam": {"api": "publish"}, "migration": {"mode": "compatible"}, "design_decision": "use-publisher-seam"}
        elif selector == "profile.feature.F5":
            receipt = self.make_test_receipt("vertical-slice", level="vertical-slice", subject="member publishes"); refs.append(receipt)
            value = {"scenario": "member publishes", "slice_steps": ["enter draft", "publish", "observe"], "end_to_end": True, "test_receipt_ref": receipt, "placeholder_conditions": [{"placeholder": "memory store", "retained_until": "database task", "owner": "task-db"}]}
        elif selector == "profile.feature.F6":
            before = self.make_test_receipt("behavior-before", level="behavior", subject="publish behavior", phase="before", status="failed", exit_code=1)
            after = self.make_test_receipt("behavior-after-file", level="behavior", subject="publish behavior")
            before_doc = json.loads((self.reference_root / before["path"]).read_text()); after_doc = json.loads((self.reference_root / after["path"]).read_text())
            after_doc["test_id"] = before_doc["test_id"]
            after = self.write("evidence/behavior-after.json", after_doc); refs += [before, after]
            value = {"behavior": "publish behavior", "failing_before_ref": before, "passing_after_ref": after}
        elif selector == "profile.feature.F7":
            cases = []
            for kind in ("dependency", "error", "migration", "observability"):
                receipt = self.make_test_receipt("integration-" + kind, level="integration", subject=kind); refs.append(receipt); cases.append({"kind": kind, "receipt_ref": receipt})
            value = {"cases": cases}
        elif selector == "profile.feature.F8":
            receipt = self.make_test_receipt("acceptance-e2e", level="e2e", subject="member publishes", equivalent=True); refs.append(receipt)
            value = {"scenario": "member publishes", "e2e_receipt_ref": receipt}
        elif selector == "profile.bug-fix.BGF1":
            value = {"expected": "save succeeds", "actual": "save fails", "environment": {"os": "test"}, "frequency": "every run", "impact": "work is lost", "first_observed_version": "v2"}
        elif selector == "profile.bug-fix.BGF2":
            receipt = self.make_test_receipt("bug-reproducer", level="reproducer", subject="save symptom", phase="before", status="failed", exit_code=2); refs.append(receipt)
            value = {"symptom": "save symptom", "reproducer_ref": receipt, "non_reproduction_conditions": ["empty draft"], "flakiness": {"observed_runs": 3, "failures": 3, "classification": "reproducible"}}
        elif selector == "profile.bug-fix.BGF3":
            observed = self.write("evidence/bug-log.json", {"schema": "diagnostic-evidence/v1", "line": "failed save"}); refs.append(observed)
            value = {"inventory": {name: ({"status": "observed", "refs": [observed], "note": "captured from reproducer"} if name == "logs" else {"status": "unavailable", "refs": [], "note": "not emitted by this boundary"}) for name in ("logs", "traces", "state", "recent_changes", "boundary_evidence")}}
        elif selector == "profile.bug-fix.BGF4":
            test = self.write("evidence/hypothesis-test.json", {"schema": "hypothesis-test/v1", "result": "supports"}); refs.append(test)
            value = {"hypotheses": [{"id": "h1", "rank": 1, "statement": "guard skips empty token", "falsifier": "guard sees token", "test_ref": test}]}
        elif selector == "profile.bug-fix.BGF5":
            proof = self.write("evidence/causal-proof.json", {"schema": "causal-evidence/v1", "observed": "token absent"}); refs.append(proof)
            value = {"causal_chain_id": "cause-1", "causal_chain": [{"cause": "token absent", "effect": "guard skipped", "evidence_refs": [proof]}, {"cause": "guard skipped", "effect": "save failed", "evidence_refs": [proof]}], "missing_guard": "reject absent token", "missing_test": "absent-token regression", "correlation_only": False}
        elif selector == "profile.bug-fix.BGF6":
            value = {"options": [{"id": "minimal", "kind": "minimal-fix", "description": "add guard", "risks": ["narrow behavior change"]}, {"id": "workaround", "kind": "workaround", "description": "retry save", "risks": ["extra latency"]}, {"id": "broad", "kind": "broader-fix", "description": "replace token flow", "risks": ["migration risk"]}], "chosen_option_id": "minimal"}
        elif selector == "profile.bug-fix.BGF7":
            before = self.make_test_receipt("regression-before", level="regression", subject="absent-token regression", phase="before", status="failed", exit_code=1, causal_chain_id="cause-1")
            after_doc = json.loads((self.reference_root / before["path"]).read_text()); after_doc.update({"phase": "after", "status": "passed", "exit_code": 0})
            after = self.write("evidence/regression-after.json", after_doc); refs += [before, after]
            value = {"behavior": "absent-token regression", "causal_chain_id": "cause-1", "failing_before_ref": before, "passing_after_ref": after}
        elif selector == "profile.bug-fix.BGF8":
            receipts = {}
            for kind in ("original-symptom", "adjacent-contracts", "performance-safety", "equivalent-environment"):
                doc = {"schema": "verification-receipt/v1", "kind": kind, "status": "passed", "terminal": True, "method": {"command": "verify"}, "capture": {"result": "passed"}}
                if kind == "equivalent-environment": doc["equivalent_environment"] = True
                ref = self.write("evidence/verify-%s.json" % kind, doc); refs.append(ref); receipts[kind.replace("-", "_")] = ref
            value = {"receipts": receipts}
        elif selector == "profile.improvement.I1":
            value = {"metric": "latency", "availability": "unavailable", "value": None, "missingness": "instrumentation not installed", "variance": {"status": "unavailable"}, "method": "request timer", "period": "one day", "environment": {"tier": "test"}}
        elif selector == "profile.improvement.I2":
            value = {"change": "cache lookup", "mechanism": "avoid repeated IO", "metric": "latency", "predicted_direction": "decrease", "falsification_condition": "median does not decrease"}
        elif selector == "profile.improvement.I3":
            value = {"guardrails": [{"kind": kind, "metric": kind + " metric", "threshold": "no worse than baseline", "method": "same-condition observation"} for kind in ("quality", "cost", "user-impact")]}
        elif selector == "profile.improvement.I4":
            value = {"change_id": "cache-lookup", "approved_change": "add request cache", "comparison_method": "same request timer", "rollback": "remove cache layer"}
        elif selector == "profile.improvement.I5":
            receipt = self.write("evidence/scoped-action.json", {"schema": "scoped-action-receipt/v1", "change_id": "cache-lookup", "scoped_paths": ["src/cache.py"], "status": "completed", "adoption_claimed": False}); refs.append(receipt)
            value = {"change_id": "cache-lookup", "scoped_paths": ["src/cache.py"], "action_receipt_ref": receipt}
        elif selector == "profile.improvement.I6":
            observation = {"availability": "available", "value": 10, "method": "request timer", "period": "one day", "environment": {"tier": "test"}, "variance": {"stdev": 1}}
            value = {"metric": "latency", "before": observation, "after": dict(observation, value=7)}
        elif selector == "profile.improvement.I7":
            value = {"recommendation": "adopt", "rationale": "same-condition latency improved within guardrails"}
        else:
            raise AssertionError(step)
        return value, refs

    def approval_ref(self, selector, evidence, *, actor_type="human", decision=None):
        if selector == "profile.feature.F4": scope, field = "responsibility-seam-migration", "design_decision"
        elif selector == "profile.bug-fix.BGF6": scope, field = "fix-option", "chosen_option_id"
        elif selector == "profile.improvement.I4": scope, field = "improvement-change", "change_id"
        else: return None
        document = {"schema": "human-profile-approval/v1", "profile": selector.split(".")[1], "qualified_id": selector, "scope": scope, "actor": {"type": actor_type, "id": "user-1"}, "decision": evidence[field] if decision is None else decision, "approved": True, "expected_head": HEAD}
        return self.write("approvals/%s.json" % selector.rsplit(".", 1)[-1], document)

    def inputs(self, selector, predecessors, *, wrong_host=False):
        evidence, refs = self.evidence(selector)
        return {"profile": selector.split(".")[1], "host_receipt_ref": self.host_ref(selector, wrong=wrong_host), "predecessor_artifact_refs": predecessors, "evidence_refs": refs, "evidence": evidence, "human_approval_ref": self.approval_ref(selector, evidence)}

    def compile_profile(self, profile):
        refs, candidates = [], []
        for selector in SELECTORS[profile]:
            candidate = self.compiler.compile(selector, self.inputs(selector, refs), self.authority, HEAD)
            self.assertEqual(candidate["schema"], "software-profile-candidate/v1", candidate)
            refs.append(self.write("artifacts/%s-%s.json" % (profile, selector.rsplit(".", 1)[-1]), candidate))
            candidates.append(candidate)
        return refs, candidates

    def test_all_23_selectors_compile_purpose_specific_candidates(self):
        candidates = []
        for profile in SELECTORS:
            _, compiled = self.compile_profile(profile); candidates.extend(compiled)
        self.assertEqual(len(candidates), 23)
        self.assertEqual(len({item["result_kind"] for item in candidates}), 23)
        self.assertTrue(all(not item["grants_approval"] and not item["objective_outcome_claimed"] and not item["common_lifecycle_embedded"] for item in candidates))

    def test_each_selector_has_purpose_specific_incomplete_evidence_refusal(self):
        for profile, selectors in SELECTORS.items():
            refs = []
            for selector in selectors:
                inputs = self.inputs(selector, refs)
                incomplete = copy.deepcopy(inputs); incomplete["evidence"] = {}
                refused = self.compiler.compile(selector, incomplete, self.authority, HEAD)
                self.assertEqual(refused["schema"], "software-profile-refusal/v1", selector)
                self.assertIn("blocked_incomplete_evidence:%s" % selector, refused["reason"])
                accepted = self.compiler.compile(selector, inputs, self.authority, HEAD)
                refs.append(self.write("artifacts/incomplete-chain-%s-%s.json" % (profile, selector.rsplit(".", 1)[-1]), accepted))

    def test_integrity_identity_host_and_predecessor_refusals(self):
        selector = "profile.feature.F1"
        values = self.inputs(selector, [])
        wrong_profile = copy.deepcopy(values); wrong_profile["profile"] = "bug-fix"
        self.assertIn("blocked_profile_identity", self.compiler.compile(selector, wrong_profile, self.authority, HEAD)["reason"])
        self.assertIn("blocked_wrong_host", self.compiler.compile(selector, self.inputs(selector, [], wrong_host=True), self.authority, HEAD)["reason"])
        self.assertIn("blocked_unknown_selector", self.compiler.compile("profile.feature.F99", values, self.authority, HEAD)["reason"])
        refs, _ = self.compile_profile("feature")
        f3 = "profile.feature.F3"; bad = self.inputs(f3, list(reversed(refs[:2])))
        self.assertIn("blocked_reordered_predecessor", self.compiler.compile(f3, bad, self.authority, HEAD)["reason"])
        missing = self.inputs(f3, refs[:1])
        self.assertIn("blocked_missing_or_extra_predecessor", self.compiler.compile(f3, missing, self.authority, HEAD)["reason"])

    def test_physical_digest_stale_head_authority_and_cross_profile_refusals(self):
        refs, _ = self.compile_profile("feature")
        drift = copy.deepcopy(self.inputs("profile.feature.F2", refs[:1])); drift["evidence_refs"][0]["digest"] = "sha256:" + "0" * 64
        self.assertIn("blocked_digest_drift", self.compiler.compile("profile.feature.F2", drift, self.authority, HEAD)["reason"])
        stale_head = {"revision": 42, "transaction_digest": HEAD["transaction_digest"]}
        self.assertIn("blocked_stale_host_head", self.compiler.compile("profile.feature.F1", self.inputs("profile.feature.F1", []), self.authority, stale_head)["reason"])
        other_authority = dict(self.authority, namespace="other")
        self.assertIn("blocked_stale_host_authority", self.compiler.compile("profile.feature.F1", self.inputs("profile.feature.F1", []), other_authority, HEAD)["reason"])
        bug_inputs = self.inputs("profile.bug-fix.BGF2", [refs[0]])
        self.assertIn("blocked_cross_profile_artifact", self.compiler.compile("profile.bug-fix.BGF2", bug_inputs, self.authority, HEAD)["reason"])

    def test_material_evidence_guards(self):
        refs, _ = self.compile_profile("feature")
        f5 = self.inputs("profile.feature.F5", refs[:4])
        slice_receipt = json.loads((self.reference_root / f5["evidence"]["test_receipt_ref"]["path"]).read_text()); slice_receipt.update({"subject": "unrelated", "exit_code": 1})
        slice_ref = self.write("evidence/forged-vertical-slice.json", slice_receipt); f5["evidence_refs"][-1] = slice_ref; f5["evidence"]["test_receipt_ref"] = slice_ref
        self.assertIn("slice-receipt", self.compiler.compile("profile.feature.F5", f5, self.authority, HEAD)["reason"])
        f6 = self.inputs("profile.feature.F6", refs[:5])
        before = json.loads((self.reference_root / f6["evidence"]["failing_before_ref"]["path"]).read_text()); before["status"] = "passed"; before["exit_code"] = 0
        changed = self.write("evidence/not-failing-before.json", before); f6["evidence_refs"][-2] = changed; f6["evidence"]["failing_before_ref"] = changed
        self.assertIn("not-genuine-failing-before", self.compiler.compile("profile.feature.F6", f6, self.authority, HEAD)["reason"])
        f6 = self.inputs("profile.feature.F6", refs[:5])
        before = json.loads((self.reference_root / f6["evidence"]["failing_before_ref"]["path"]).read_text()); before["level"] = "integration"
        level_ref = self.write("evidence/wrong-behavior-level.json", before); f6["evidence_refs"][-2] = level_ref; f6["evidence"]["failing_before_ref"] = level_ref
        self.assertIn("behavior-test-level", self.compiler.compile("profile.feature.F6", f6, self.authority, HEAD)["reason"])
        bug_refs, _ = self.compile_profile("bug-fix")
        bgf2 = self.inputs("profile.bug-fix.BGF2", bug_refs[:1])
        reproducer = json.loads((self.reference_root / bgf2["evidence"]["reproducer_ref"]["path"]).read_text()); reproducer.update({"status": "passed", "exit_code": 0})
        reproducer_ref = self.write("evidence/non-failing-reproducer.json", reproducer); bgf2["evidence_refs"][-1] = reproducer_ref; bgf2["evidence"]["reproducer_ref"] = reproducer_ref
        self.assertIn("failing-before-reproducer", self.compiler.compile("profile.bug-fix.BGF2", bgf2, self.authority, HEAD)["reason"])
        bgf2 = self.inputs("profile.bug-fix.BGF2", bug_refs[:1])
        reproducer = json.loads((self.reference_root / bgf2["evidence"]["reproducer_ref"]["path"]).read_text()); reproducer["subject"] = "different symptom"
        reproducer_ref = self.write("evidence/wrong-symptom-reproducer.json", reproducer); bgf2["evidence_refs"][-1] = reproducer_ref; bgf2["evidence"]["reproducer_ref"] = reproducer_ref
        self.assertIn("reproducer-physical-execution-binding", self.compiler.compile("profile.bug-fix.BGF2", bgf2, self.authority, HEAD)["reason"])
        bgf7 = self.inputs("profile.bug-fix.BGF7", bug_refs[:6])
        regression = json.loads((self.reference_root / bgf7["evidence"]["failing_before_ref"]["path"]).read_text()); regression.update({"status": "passed", "exit_code": 0})
        regression_ref = self.write("evidence/non-failing-regression.json", regression); bgf7["evidence_refs"][-2] = regression_ref; bgf7["evidence"]["failing_before_ref"] = regression_ref
        self.assertIn("not-genuine-failing-before", self.compiler.compile("profile.bug-fix.BGF7", bgf7, self.authority, HEAD)["reason"])
        f8 = self.inputs("profile.feature.F8", refs[:7]); unit = json.loads((self.reference_root / f8["evidence"]["e2e_receipt_ref"]["path"]).read_text()); unit["level"] = "unit"
        unit_ref = self.write("evidence/unit-only.json", unit); f8["evidence_refs"][-1] = unit_ref; f8["evidence"]["e2e_receipt_ref"] = unit_ref
        self.assertIn("physical-e2e", self.compiler.compile("profile.feature.F8", f8, self.authority, HEAD)["reason"])
        f7 = self.inputs("profile.feature.F7", refs[:6])
        integration = json.loads((self.reference_root / f7["evidence"]["cases"][0]["receipt_ref"]["path"]).read_text()); integration.update({"subject": "unrelated", "exit_code": 1})
        integration_ref = self.write("evidence/forged-integration.json", integration); f7["evidence_refs"][1] = integration_ref; f7["evidence"]["cases"][0]["receipt_ref"] = integration_ref
        self.assertIn("integration-receipt", self.compiler.compile("profile.feature.F7", f7, self.authority, HEAD)["reason"])
        irefs, _ = self.compile_profile("improvement")
        i1 = self.inputs("profile.improvement.I1", []); i1["evidence"]["value"] = 0
        self.assertIn("unavailable-not-zero", self.compiler.compile("profile.improvement.I1", i1, self.authority, HEAD)["reason"])
        i6 = self.inputs("profile.improvement.I6", irefs[:5]); i6["evidence"]["after"]["method"] = "different timer"
        self.assertIn("insufficient-comparability", self.compiler.compile("profile.improvement.I6", i6, self.authority, HEAD)["reason"])
        i7 = self.inputs("profile.improvement.I7", irefs[:6]); i7["evidence"]["approved"] = True
        self.assertIn("blocked_incomplete_evidence", self.compiler.compile("profile.improvement.I7", i7, self.authority, HEAD)["reason"])
        i5 = self.inputs("profile.improvement.I5", irefs[:4])
        action = json.loads((self.reference_root / i5["evidence"]["action_receipt_ref"]["path"]).read_text()); action["scoped_paths"] = ["src/other.py"]
        action_ref = self.write("evidence/wrong-scoped-action.json", action); i5["evidence_refs"][-1] = action_ref; i5["evidence"]["action_receipt_ref"] = action_ref
        self.assertIn("scoped-action-receipt", self.compiler.compile("profile.improvement.I5", i5, self.authority, HEAD)["reason"])

    def test_human_approval_scope_actor_decision_and_no_other_step_grants_it(self):
        for selector in ("profile.feature.F4", "profile.bug-fix.BGF6", "profile.improvement.I4"):
            profile = selector.split(".")[1]; index = SELECTORS[profile].index(selector)
            refs, _ = self.compile_profile(profile)
            values = self.inputs(selector, refs[:index]); evidence = values["evidence"]
            values["human_approval_ref"] = self.approval_ref(selector, evidence, actor_type="agent")
            self.assertIn("blocked_nonhuman_approval", self.compiler.compile(selector, values, self.authority, HEAD)["reason"])
            values["human_approval_ref"] = self.approval_ref(selector, evidence, decision="different")
            self.assertIn("blocked_approval_decision", self.compiler.compile(selector, values, self.authority, HEAD)["reason"])
        f1 = self.inputs("profile.feature.F1", []); f1["human_approval_ref"] = self.authority_ref
        self.assertIn("blocked_unexpected_approval", self.compiler.compile("profile.feature.F1", f1, self.authority, HEAD)["reason"])

    def test_three_profile_compositions_bind_validated_workflow_and_ordered_artifacts(self):
        workflows = {"feature": "feature-bounded", "bug-fix": "bug-fix-standard", "improvement": "improvement-measured"}
        compositions = {}
        for profile, workflow in workflows.items():
            refs, _ = self.compile_profile(profile)
            result = self.compiler.compose_profile(profile, self.repo_ref("agent-workflows/workflows/%s.json" % workflow), refs, self.authority, HEAD)
            self.assertEqual(result["schema"], "software-profile-composition/v1", result)
            self.assertEqual(result["workflow_composition_receipt"]["result"], "passed")
            compositions[profile] = result
        self.assertEqual({item["profile"] for item in compositions.values()}, {"feature", "bug-fix", "improvement"})

    def test_composition_refuses_wrong_workflow_missing_reordered_stale_and_cross_profile(self):
        feature_refs, _ = self.compile_profile("feature")
        bug_refs, _ = self.compile_profile("bug-fix")
        wrong = self.compiler.compose_profile("feature", self.repo_ref("agent-workflows/workflows/bug-fix-standard.json"), feature_refs, self.authority, HEAD)
        self.assertIn("blocked_wrong_workflow_profile", wrong["reason"])
        missing = self.compiler.compose_profile("feature", self.repo_ref("agent-workflows/workflows/feature-bounded.json"), feature_refs[:-1], self.authority, HEAD)
        self.assertIn("blocked_incomplete_profile_artifacts", missing["reason"])
        reordered = self.compiler.compose_profile("feature", self.repo_ref("agent-workflows/workflows/feature-bounded.json"), [feature_refs[1], feature_refs[0], *feature_refs[2:]], self.authority, HEAD)
        self.assertIn("blocked_reordered_profile_artifact", reordered["reason"])
        cross = self.compiler.compose_profile("feature", self.repo_ref("agent-workflows/workflows/feature-bounded.json"), [bug_refs[0], *feature_refs[1:]], self.authority, HEAD)
        self.assertIn("blocked_cross_profile_artifact", cross["reason"])
        other = dict(self.authority, namespace="other")
        stale = self.compiler.compose_profile("feature", self.repo_ref("agent-workflows/workflows/feature-bounded.json"), feature_refs, other, HEAD)
        self.assertIn("blocked_stale_artifact_authority", stale["reason"])
        stale_head = {"revision": 42, "transaction_digest": HEAD["transaction_digest"]}
        stale = self.compiler.compose_profile("feature", self.repo_ref("agent-workflows/workflows/feature-bounded.json"), feature_refs, self.authority, stale_head)
        self.assertIn("blocked_stale_artifact_head", stale["reason"])
        drifted = copy.deepcopy(feature_refs); drifted[0]["digest"] = "sha256:" + "0" * 64
        drift = self.compiler.compose_profile("feature", self.repo_ref("agent-workflows/workflows/feature-bounded.json"), drifted, self.authority, HEAD)
        self.assertIn("blocked_digest_drift", drift["reason"])

    def test_same_path_changed_digest_cannot_reuse_stale_successor_chain(self):
        feature_refs, _ = self.compile_profile("feature")
        first_path = self.reference_root / feature_refs[0]["path"]
        replacement = json.loads(first_path.read_text())
        replacement["evidence"]["observable_outcome"] = "replacement outcome"
        replacement["candidate_digest"] = canonical_digest({key: value for key, value in replacement.items() if key != "candidate_digest"})
        replacement_ref = self.write(feature_refs[0]["path"], replacement)
        self.assertEqual(replacement_ref["path"], feature_refs[0]["path"])
        self.assertNotEqual(replacement_ref["digest"], feature_refs[0]["digest"])

        composed = self.compiler.compose_profile(
            "feature",
            self.repo_ref("agent-workflows/workflows/feature-bounded.json"),
            [replacement_ref, *feature_refs[1:]],
            self.authority,
            HEAD,
        )
        self.assertEqual(composed["schema"], "software-profile-refusal/v1")
        self.assertIn("blocked_profile_predecessor_chain", composed["reason"])

        f3 = self.inputs("profile.feature.F3", [replacement_ref, feature_refs[1]])
        compiled = self.compiler.compile("profile.feature.F3", f3, self.authority, HEAD)
        self.assertEqual(compiled["schema"], "software-profile-refusal/v1")
        self.assertIn("blocked_predecessor_chain", compiled["reason"])

    def test_persisted_candidate_semantics_are_recompiled_at_both_consumers(self):
        feature_refs, _ = self.compile_profile("feature")
        first = json.loads((self.reference_root / feature_refs[0]["path"]).read_text())
        first["evidence"]["observable_outcome"] = ""
        first["candidate_digest"] = canonical_digest({key: value for key, value in first.items() if key != "candidate_digest"})
        forged_first_ref = self.write("artifacts/forged-feature-F1.json", first)
        f2 = self.inputs("profile.feature.F2", [forged_first_ref])
        compiled = self.compiler.compile("profile.feature.F2", f2, self.authority, HEAD)
        self.assertEqual(compiled["schema"], "software-profile-refusal/v1")
        self.assertIn("blocked_persisted_candidate_semantics", compiled["reason"])

        last = json.loads((self.reference_root / feature_refs[-1]["path"]).read_text())
        last["evidence_refs"] = [self.generic_ref]
        last["evidence"] = {"scenario": "fake", "e2e_receipt_ref": self.generic_ref}
        last["candidate_digest"] = canonical_digest({key: value for key, value in last.items() if key != "candidate_digest"})
        forged_last_ref = self.write("artifacts/forged-feature-F8.json", last)
        composed = self.compiler.compose_profile(
            "feature",
            self.repo_ref("agent-workflows/workflows/feature-bounded.json"),
            [*feature_refs[:-1], forged_last_ref],
            self.authority,
            HEAD,
        )
        self.assertEqual(composed["schema"], "software-profile-refusal/v1")
        self.assertIn("blocked_persisted_candidate_semantics", composed["reason"])


if __name__ == "__main__":
    unittest.main()
