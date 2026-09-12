"""Real Kernel closure commands, with only dialogue marked as mock."""
import copy
import json
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

import test_runtime_objective_approval as approval_fixture
from test_execution_v2 import workflow_loop_request

from ai_agent_workflow.closure_protocol import ClosureProtocolError
from ai_agent_workflow.control_kernel import ControlKernel, canonical_digest
from ai_agent_workflow.runtime_closure import (
    close_runtime_group,
    close_workflow_loop,
    record_workflow_loop_outcome,
)
from ai_agent_workflow.runtime_execution import RuntimeExecution


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
            state = self.kernel.publish_artifact(f"bad-audit-{index}", "v1", value, kind="runtime-group-audit", authority_ref={"approved": True, "scopes": ["publish_artifact"]})
            before = self.kernel.head()
            with self.subTest(change=change), self.assertRaises(ClosureProtocolError):
                close_runtime_group(self.kernel, self.close_authority, next_group="C", evidence_refs=[self.semantic_ref, state["artifacts"][f"bad-audit-{index}"]["object_ref"]])
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
        with patch.object(self.kernel, "apply", side_effect=stop_before_f7), self.assertRaisesRegex(RuntimeError, "before F7"):
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


class WorkflowLoopClosureTests(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix="workflow-loop-closure-")
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        self.authority = {"status": "approved", "scopes": ["*"]}
        self.identity = {
            "schema": "loop-work-identity/v1",
            "work_lineage_id": "lineage-closure",
            "logical_task_id": "integration-1",
            "phase": "E8",
            "scope_revision": "scope-e8",
            "requirements_digest": "sha256:" + "a" * 64,
            "predecessor_ref": None,
        }
        self.kernel = ControlKernel(self.root, "workflow-loop-close")
        self.kernel.entry(
            {"path": "objectives/loop.md", "version": "v1", "digest": "sha256:" + "b" * 64},
            authority_ref=self.authority,
            loop_control={"identity": self.identity, "history": []},
        )
        RuntimeExecution(kernel=self.kernel).execute_loop(
            "integration-1",
            {"schema": "workflow-loop/v1", "identity": self.identity, "phase": "E8"},
            integration=True,
            result_ref={"id": "result-e8", "digest": "sha256:" + "c" * 64},
        )

    def completion(self, **changes):
        value = workflow_loop_request()
        value["identity"] = copy.deepcopy(self.identity)
        value.update(copy.deepcopy(changes))
        return value

    def close_authority(self):
        return {"approved": True, "scopes": ["*"]}

    def test_strict_zero_finding_close_is_non_authorizing_and_does_not_write(self):
        before = self.kernel.head()
        result = close_workflow_loop(
            self.kernel,
            self.close_authority(),
            next_group="H",
            completion_request=self.completion(),
        )
        self.assertEqual("ready", result["status"])
        self.assertEqual("completed", result["outcome"])
        self.assertTrue(result["non_authorizing"])
        self.assertFalse(result["objective_achievement"])
        self.assertFalse(result["activation"])
        self.assertEqual(before, self.kernel.head())
        self.assertEqual("E8", result["close_set"]["identity"]["phase"])
        self.assertEqual("complete", result["close_set"]["validation"]["next"])

    def test_required_findings_and_stale_evidence_refuse_closure(self):
        required = workflow_loop_request(required_finding=True)
        required["identity"] = copy.deepcopy(self.identity)
        with self.assertRaisesRegex(ClosureProtocolError, "closure rejected"):
            close_workflow_loop(
                self.kernel,
                self.close_authority(),
                next_group="H",
                completion_request=required,
            )
        stale = self.completion(
            current_inputs={
                "candidate_digest": "sha256:" + "a" * 64,
                "spec_digest": "sha256:" + "b" * 64,
                "source_digest": "sha256:" + "9" * 64,
                "dependency_digest": "sha256:" + "d" * 64,
                "environment_digest": "sha256:" + "e" * 64,
                "check_definition_digest": "sha256:" + "f" * 64,
                "required_coverage": ["R1"],
            },
            change_impact={"known": True, "invalidated_dimensions": ["source"], "impacted_coverage": ["R1"]},
        )
        with self.assertRaisesRegex(ClosureProtocolError, "closure rejected|stale or invalid evidence"):
            close_workflow_loop(self.kernel, self.close_authority(), next_group="H", completion_request=stale)

    def test_caller_supplied_integration_identity_cannot_replace_durable_phase(self):
        temporary = tempfile.TemporaryDirectory(prefix="workflow-loop-task-")
        self.addCleanup(temporary.cleanup)
        kernel = ControlKernel(Path(temporary.name), "workflow-loop-task")
        task_identity = copy.deepcopy(self.identity)
        task_identity.update({"logical_task_id": "task-1", "phase": "E3", "scope_revision": "scope-e3"})
        kernel.entry(
            {"path": "objectives/loop.md", "version": "v1", "digest": "sha256:" + "b" * 64},
            authority_ref=self.authority,
            loop_control={"identity": task_identity, "history": []},
        )
        RuntimeExecution(kernel=kernel).execute_loop(
            "task-1", {"schema": "workflow-loop/v1", "identity": task_identity, "phase": "E3"},
            result_ref={"id": "result-e3", "digest": "sha256:" + "c" * 64},
        )
        integration_identity = copy.deepcopy(self.identity)
        with self.assertRaisesRegex(ClosureProtocolError, "durable E8-E9"):
            close_workflow_loop(
                kernel,
                {"approved": True},
                next_group="H",
                completion_request=self.completion(),
                integration_identity=integration_identity,
            )

    def test_execution_unknown_requires_recovery_before_closure(self):
        temporary = tempfile.TemporaryDirectory(prefix="workflow-loop-unknown-")
        self.addCleanup(temporary.cleanup)
        kernel = ControlKernel(Path(temporary.name), "workflow-loop-unknown")
        kernel.entry(
            {"path": "objectives/loop.md", "version": "v1", "digest": "sha256:" + "b" * 64},
            authority_ref=self.authority,
            loop_control={"identity": self.identity, "history": []},
        )
        RuntimeExecution(kernel=kernel).execute_loop(
            "integration-1",
            {"schema": "workflow-loop/v1", "identity": self.identity, "phase": "E8"},
            dispatcher=lambda _package: (_ for _ in ()).throw(RuntimeError("unknown")),
        )
        with self.assertRaisesRegex(ClosureProtocolError, "execution recovery"):
            close_workflow_loop(
                kernel,
                {"approved": True},
                next_group="H",
                completion_request=self.completion(),
            )

    def test_incomplete_outcome_has_an_explicit_durable_stop_api(self):
        record = {
            "schema": "loop-terminal-record/v1",
            "terminal_id": "terminal-input-1",
            "identity": copy.deepcopy(self.identity),
            "outcome": "needs-input",
            "reason": "a required choice is missing",
            "candidate_ref": None,
            "requirements": [],
            "reviews": [],
            "evidence": [],
            "open_items": ["choose an option"],
            "resume_ref": {"id": "question-1", "digest": "sha256:" + "d" * 64},
            "non_authorizing": True,
        }
        result = record_workflow_loop_outcome(self.kernel, self.close_authority(), record=record)
        self.assertEqual("recorded", result["status"])
        self.assertEqual("needs-input", result["outcome"])
        self.assertTrue(result["non_authorizing"])
        self.assertEqual(record, self.kernel.read_state()["loop_control"]["terminal_record"])
        blocked = RuntimeExecution(kernel=self.kernel).execute_loop(
            "integration-1",
            {"schema": "workflow-loop/v1", "identity": self.identity, "phase": "E8"},
            result_ref={"id": "result-after-stop", "digest": "sha256:" + "e" * 64},
        )
        self.assertEqual("needs-input", blocked["outcome"])


if __name__ == "__main__":
    unittest.main()
