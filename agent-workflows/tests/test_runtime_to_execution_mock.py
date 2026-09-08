"""Compiler-shaped boundary test; not evidence of an LLM or OS sandbox run."""
import copy
import io
import sys
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
EXAMPLE = ROOT / "examples" / "support-report"
sys.path[:0] = [str(ROOT / "src"), str(ROOT / "tests"), str(EXAMPLE)]

from ai_agent_workflow.bounded_read_scope import observe_read_scope  # noqa: E402
from ai_agent_workflow.execution_group import ArtifactCandidateBuilder, ExecutionGroupV1  # noqa: E402
from ai_agent_workflow.inception_runtime import InceptionRuntime  # noqa: E402
from runtime_trial import audited_close, initialize, plan_group  # noqa: E402
from test_execution_group import (  # noqa: E402
    detailed_finding,
    execution_package_source,
    frozen_binding,
    inputs,
    review,
    siblings_for,
    v2_join_inputs,
    verification_receipts,
)
from test_execution_v2_orchestrator import (  # noqa: E402
    advisory as orchestrator_advice,
    command as orchestrator_command,
    state as orchestrator_state,
)


class RuntimeToExecutionMockTests(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        self.project = Path(temporary.name).resolve()

    def _reach_execution_frontier(self):
        with redirect_stdout(io.StringIO()):
            initialize(self.project)
            audited_close(self.project, "B compiler-shaped outputs bind the approved mock objective")
            InceptionRuntime(self.project, "support-report").advance()
            plan_group(self.project, "C")
            audited_close(self.project, "C compiler-shaped outputs retain objective traceability")
            InceptionRuntime(self.project, "support-report").advance()
            plan_group(self.project, "D")
            audited_close(self.project, "D compiler-shaped outputs retain finite task boundaries")
            return InceptionRuntime(self.project, "support-report").advance()

    @staticmethod
    def _authority(head, **overrides):
        value = {
            "authority_id": "runtime-parent", "actor_id": "runtime-parent", "role": "orchestrator",
            "assignment_id": "runtime-e", "scope_ref": {"path": "scope-e", "digest": "sha256:" + "f" * 64},
            "epoch_id": "runtime-e-compiler", "lease_id": "runtime-e-lease", "idempotency_key": "runtime-e-key",
            "budget": {"seconds": 300, "review_round": 0, "product_fix_attempts": 0},
            "expected_head": copy.deepcopy(head),
        }
        value.update(overrides)
        return value

    @staticmethod
    def _inputs(head, authority, **overrides):
        authority_ref = {key: authority[key] for key in ("authority_id", "role", "assignment_id", "lease_id", "scope_ref")}
        return inputs(expected_head=copy.deepcopy(head), authority_ref=authority_ref, **overrides)

    def test_mocked_bcd_closure_reaches_e_and_all_normal_e_compilers_accept(self):
        status = self._reach_execution_frontier()
        self.assertEqual(status["group"], {"id": "E", "next_group": None, "status": "open"})
        self.assertTrue(status["execution_ready"])
        self.assertFalse(status["execution_authorized"])

        head = status["head"]
        authority = self._authority(head)
        compiler = ExecutionGroupV1()
        e1 = compiler.compile("group.E.E1", self._inputs(head, authority), authority, head)
        self.assertEqual(e1["status"], "ready")

        package_source = execution_package_source()
        package_source["workspace_identity"] = str(self.project)
        package_source["command"]["cwd"] = str(self.project)
        package_source["isolation"]["cwd"] = str(self.project)
        package_source["resource_claims"]["read_paths"] = [".local/agent/support-report-trial"]
        e2 = compiler.compile("group.E.E2", self._inputs(
            head, authority, preflight=e1, preflight_digest=e1["candidate_digest"],
            execution_package_input=package_source, assigned_role="worker", assigned_worker="mock-worker",
            output_path="out/task", write_scope=["src"], non_goals=["external mutation"],
            acceptance=["focused check"], stop_conditions=["scope escape"],
        ), authority, head)
        self.assertEqual(e2["status"], "issued")
        self.assertFalse(e2["package"]["read_scope"]["os_isolation_enforced"])

        package = e2["package"]
        with tempfile.TemporaryDirectory() as receipt_root:
            runner_package, runner_policy = ArtifactCandidateBuilder.runner_inputs(package)
            recovery = ArtifactCandidateBuilder.verify_terminal_reuse(receipt_root, runner_package, runner_policy)
        read_receipt = observe_read_scope(
            package["read_scope"], [".local/agent/support-report-trial/objective.json"],
            observation_complete=True,
        )
        e3 = compiler.compile("group.E.E3", self._inputs(
            head, authority, package=package, terminal="DONE", changed_paths=["src/app.py"],
            receipt=recovery["terminal_receipt"], recovery_receipt=recovery,
            read_scope_receipt=read_receipt, task_id="mock-task",
        ), authority, head)
        self.assertEqual(e3["status"], "submitted")

        e4 = compiler.compile("group.E.E4", self._inputs(
            head, authority, axis="architecture-safety", actor_id="spec-reviewer",
            reviewer_epoch_id="spec-review-epoch", other_reviewer_epoch_id="quality-review-epoch",
            worker_actor_id="mock-worker", other_reviewer_actor_id="quality-reviewer",
            frozen_binding=frozen_binding(), findings=[detailed_finding()],
        ), authority, head)
        e5 = compiler.compile("group.E.E5", self._inputs(
            head, authority, axis="integration-operability", actor_id="quality-reviewer",
            reviewer_epoch_id="quality-review-epoch", other_reviewer_epoch_id="spec-review-epoch",
            worker_actor_id="mock-worker", other_reviewer_actor_id="spec-reviewer",
            frozen_binding=frozen_binding(), findings=[detailed_finding()],
        ), authority, head)
        self.assertEqual((e4["status"], e5["status"]), ("reviewed", "reviewed"))

        finding = {"finding_id": "f1", "fingerprint": "fp1", "severity": "major", "summary": "gap"}
        reviews = [review("architecture-safety", "spec-reviewer", "spec-review-epoch", [finding]),
                   review("integration-operability", "quality-reviewer", "quality-review-epoch", [])]
        e6 = compiler.compile("group.E.E6", self._inputs(
            head, authority, reviews=reviews,
            dispositions=[{"fingerprint": "fp1", "classification": "required", "materiality": "material", "proposed_scope": ["src"]}],
            observed_budget={"remaining_seconds": 200, "review_round": 0, "product_fix_attempt": 0},
        ), authority, head)
        self.assertEqual((e6["status"], e6["next"]), ("validated", "E7"))

        advice = orchestrator_advice()
        command = orchestrator_command(advice)
        observed = orchestrator_state(advice)
        command["expected_head"] = copy.deepcopy(head)
        command["lease_id"] = authority["lease_id"]
        command["actor"]["assignment_id"] = authority["assignment_id"]
        observed["current_head"] = copy.deepcopy(head)
        observed["active_lease"]["expected_head"] = copy.deepcopy(head)
        observed["active_lease"]["lease_id"] = authority["lease_id"]
        observed["active_lease"]["holder_assignment_id"] = authority["assignment_id"]
        e7 = compiler.compile("group.E.E7", self._inputs(
            head, authority, orchestrator_validated=True, advice="required",
            orchestrator_command=command, observed_state=observed,
        ), authority, head)
        self.assertEqual((e7["status"], e7["next"]), ("rereview-required", "E6"))

        joined = v2_join_inputs()
        e8 = compiler.compile("group.E.E8", self._inputs(
            head, authority, v2_join=joined, sibling_refs=siblings_for(joined),
            complete=True, conflicting=False, open_required=False,
        ), authority, head)
        self.assertEqual(e8["status"], "converged")
        e9 = compiler.compile("group.E.E9", self._inputs(
            head, authority, candidate_digest="sha256:" + "a" * 64,
            closure_digest="sha256:" + "b" * 64,
            verification_receipts=verification_receipts(), complete=True, open_required=False,
        ), authority, head)
        self.assertEqual(e9["status"], "verified")


if __name__ == "__main__":
    unittest.main()
