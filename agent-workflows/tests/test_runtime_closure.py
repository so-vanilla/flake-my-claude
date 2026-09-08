"""Real Kernel closure commands, with only dialogue marked as mock."""
import json
import copy
import unittest
from unittest.mock import patch

import test_runtime_objective_approval as approval_fixture
from ai_agent_workflow.closure_protocol import ClosureProtocolError
from ai_agent_workflow.control_kernel import ControlKernel, canonical_digest
from ai_agent_workflow.runtime_closure import close_runtime_group


class RuntimeClosureTests(unittest.TestCase):
    setUp = approval_fixture.RuntimeApprovalTests.setUp
    setup_run = approval_fixture.RuntimeApprovalTests.setup_run
    command = approval_fixture.RuntimeApprovalTests.command

    def prepare(self):
        self.setup_run("rehearsal")
        state = self.kernel.apply(self.command())
        self.close_authority = {"approved": True, "human_receipt": state["objective_ref"]["approval_ref"]["digest"]}
        state = self.kernel.publish_artifact("group-B-result", "v1", {"objective_digest": state["objective_ref"]["digest"], "result": "aligned", "source": "compiled"}, authority_ref={"approved": True, "scopes": ["publish_artifact"]})
        self.semantic_ref = state["artifacts"]["group-B-result"]["object_ref"]
        state = self.kernel.publish_artifact("group-B-audit", "v1", {"objective_digest": state["objective_ref"]["digest"], "alignment": "aligned", "group_id": "B", "artifact_refs": [self.semantic_ref], "evidence_source": "semantic-compiler"}, kind="runtime-group-audit", authority_ref={"approved": True, "scopes": ["publish_artifact"]})
        return [self.semantic_ref, state["artifacts"]["group-B-audit"]["object_ref"]]

    def test_actual_f1_f7_close_and_cold_resume(self):
        ref = self.prepare()
        result = close_runtime_group(self.kernel, self.close_authority, next_group="C", evidence_refs=ref)
        cold = ControlKernel(self.project, "runtime-run")
        state = cold.read_state()
        self.assertEqual(state["group"]["status"], "closed")
        self.assertEqual(state["epoch"]["status"], "closed")
        self.assertEqual(state["group"]["next_group"], "C")
        self.assertEqual(cold.read_object(result["closure_bundle_ref"])["object_type"], "artifact-bundle")
        self.assertEqual(cold.read_object(result["checkpoint_ref"])["object_type"], "checkpoint")
        root = self.kernel.run_dir / "runtime-closure"
        report = json.loads((root / result["closure_report_ref"]["path"]).read_text())
        self.assertEqual(canonical_digest(report), result["closure_report_ref"]["digest"])
        self.assertEqual(report["payload"]["runtime_identity"]["mode"], "rehearsal")
        self.assertEqual(len(report["payload"]["closure_refs"]), 5)
        for ref in report["payload"]["closure_refs"]:
            self.assertEqual(cold.read_object(ref)["payload"]["payload"]["operation"]["status"], "compiled")
        self.assertEqual(report["payload"]["f6"]["result"]["command"]["command_type"], "close_epoch")
        self.assertEqual(report["payload"]["f7"]["result"]["command"]["command_type"], "close_group")
        self.assertEqual(cold.resume()["status"], "paused_after_group")

    def test_missing_evidence_and_wrong_receipt_refuse_before_mutation(self):
        ref = self.prepare()
        before = self.kernel.head()
        for authority, evidence, next_group in [(self.close_authority, [], "C"), ({"approved": True, "human_receipt": "invented"}, ref, "C"), (self.close_authority, ref, "E"), (self.close_authority, [self.semantic_ref], "C")]:
            with self.assertRaises(ClosureProtocolError):
                close_runtime_group(self.kernel, authority, next_group=next_group, evidence_refs=evidence)
            self.assertEqual(self.kernel.head(), before)

    def test_audit_objective_group_and_required_artifacts_are_checked(self):
        refs = self.prepare()
        audit = self.kernel.read_object(refs[-1])["payload"]["payload"]
        for index, change in enumerate([
            {"objective_digest": "sha256:" + "a" * 64}, {"group_id": "C"},
            {"alignment": "uncertain"}, {"artifact_refs": []},
            {"artifact_refs": [{**self.semantic_ref, "digest": "sha256:" + "f" * 64}]},
        ]):
            value = copy.deepcopy(audit)
            value.update(change)
            state = self.kernel.publish_artifact("bad-audit-%d" % index, "v1", value, kind="runtime-group-audit", authority_ref={"approved": True, "scopes": ["publish_artifact"]})
            before = self.kernel.head()
            with self.subTest(change=change), self.assertRaises(ClosureProtocolError):
                close_runtime_group(self.kernel, self.close_authority, next_group="C", evidence_refs=[self.semantic_ref, state["artifacts"]["bad-audit-%d" % index]["object_ref"]])
            self.assertEqual(self.kernel.head(), before)

    def test_mock_decision_remains_an_unapproved_assumption(self):
        refs = self.prepare()
        approval_ref = self.kernel.read_state()["objective_ref"]["approval_ref"]
        result = close_runtime_group(self.kernel, self.close_authority, next_group="C", evidence_refs=refs, accepted_decisions=[{"id": "mock-decision", "object_ref": approval_ref}])
        report = json.loads((self.kernel.run_dir / "runtime-closure" / result["closure_report_ref"]["path"]).read_text())["payload"]
        self.assertEqual(report["decision_events"], [{"id": "mock-decision", "digest": approval_ref["digest"], "kind": "assumption", "approved": False}])
        self.assertEqual(self.kernel.read_object(result["closure_bundle_ref"])["payload"]["approved_decisions"], [])

    def test_cold_retry_after_f6_completes_only_f7_and_reconstructs_report(self):
        refs = self.prepare()
        apply = self.kernel.apply
        def stop_before_f7(command):
            if command["command_type"] == "close_group":
                raise RuntimeError("interrupted before F7")
            return apply(command)
        with patch.object(self.kernel, "apply", side_effect=stop_before_f7):
            with self.assertRaisesRegex(RuntimeError, "before F7"):
                close_runtime_group(self.kernel, self.close_authority, next_group="C", evidence_refs=refs)
        cold = ControlKernel(self.project, "runtime-run")
        before = cold.head()
        self.assertEqual(cold.read_state()["epoch"]["status"], "closed")
        with patch("ai_agent_workflow.runtime_closure.SharedClosureProtocolV1.write_checkpoint", side_effect=AssertionError("F6 must not repeat")):
            result = close_runtime_group(cold, self.close_authority, next_group="C", evidence_refs=refs)
        self.assertEqual(cold.head()["revision"], before["revision"] + 1)
        self.assertEqual(result["state"]["group"]["status"], "closed")
        head = cold.head()
        retry = close_runtime_group(ControlKernel(self.project, "runtime-run"), self.close_authority, next_group="C", evidence_refs=refs)
        self.assertEqual(cold.head(), head)
        self.assertEqual(retry["closure_report_ref"], result["closure_report_ref"])

    def test_f6_retry_rejects_changed_evidence_and_decisions(self):
        refs = self.prepare()
        apply = self.kernel.apply
        def stop_before_f7(command):
            if command["command_type"] == "close_group":
                raise RuntimeError("interrupted")
            return apply(command)
        with patch.object(self.kernel, "apply", side_effect=stop_before_f7), self.assertRaises(RuntimeError):
            close_runtime_group(self.kernel, self.close_authority, next_group="C", evidence_refs=refs)
        cold = ControlKernel(self.project, "runtime-run")
        head = cold.head()
        for evidence, decisions in [
            (list(reversed(refs)), []),
            (refs, [{"id": "new-decision", "object_ref": cold.read_state()["objective_ref"]["approval_ref"]}]),
        ]:
            with self.assertRaises(ClosureProtocolError):
                close_runtime_group(cold, self.close_authority, next_group="C", evidence_refs=evidence, accepted_decisions=decisions)
            self.assertEqual(cold.head(), head)

    def test_runtime_close_reuses_identical_audit_after_f6(self):
        from test_inception_runtime import InceptionRuntimeTests
        from ai_agent_workflow.inception_runtime import InceptionRuntime
        from ai_agent_workflow.runtime_approval import adopt_approved_objective
        fixture = InceptionRuntimeTests()
        fixture.setUp()
        self.addCleanup(fixture.doCleanups)
        args = fixture.arguments("rehearsal")
        args["preapproval_steps"] = fixture.steps(args)
        adopt_approved_objective(fixture.project, "sample", **args)
        fixture.runtime = InceptionRuntime(fixture.project, "sample")
        apply = fixture.runtime.kernel.apply
        def stop_before_f7(command):
            if command["command_type"] == "close_group":
                raise RuntimeError("interrupted")
            return apply(command)
        with patch.object(fixture.runtime.kernel, "apply", side_effect=stop_before_f7), self.assertRaises(RuntimeError):
            fixture.close_group("B")
        before = fixture.runtime.kernel.head()
        fixture.runtime = InceptionRuntime(fixture.project, "sample")
        fixture.close_group("B")
        self.assertEqual(fixture.runtime.kernel.head()["revision"], before["revision"] + 1)
        self.assertEqual(fixture.runtime.status()["group"]["status"], "closed")


if __name__ == "__main__":
    unittest.main()
