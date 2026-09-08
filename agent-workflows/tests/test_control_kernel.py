import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
SRC = ROOT / "src"
sys.path.insert(0, str(SRC))

from ai_agent_workflow.control_kernel import (  # noqa: E402
    AuthorizationError,
    ControlKernel,
    DuplicateCommandError,
    InjectedCrash,
    IntegrityBlockedError,
    StaleHeadError,
)


class ControlKernelTests(unittest.TestCase):
    def setUp(self):
        self.tempdir = tempfile.TemporaryDirectory()
        self.root = Path(self.tempdir.name)
        self.kernel = ControlKernel(self.root, "run-a6r")
        self.authority = {
            "status": "approved", "scopes": ["*"],
            "fixture_identity": {"schema": "canonical-fixture-identity/v1", "run_id": "run-a6r", "namespace": "fixture:s2-u", "approval_scope": "fixture-only"},
        }
        self.objective = {"path": "objectives/v001.md", "version": "v001", "digest": "a" * 64}
        self.kernel.entry(self.objective, authority_ref=self.authority, aliases=["demo"], external_refs=["issue:42"])

    def tearDown(self):
        self.tempdir.cleanup()

    def test_immutable_objects_parent_chain_and_stale_head(self):
        before = self.kernel.head()
        self.kernel.publish_artifact("spec", "v1", {"ok": True}, authority_ref=self.authority)
        after = self.kernel.head()
        self.assertEqual(after["revision"], 2)
        self.assertNotEqual(before["transaction_digest"], after["transaction_digest"])
        tx = json.loads((self.kernel.transactions_dir / (after["transaction_digest"][7:] + ".json")).read_text())
        self.assertEqual(tx["parent"]["transaction_digest"], before["transaction_digest"])
        with self.assertRaises(StaleHeadError):
            stale = {
                "schema": "dag-command/v1",
                "command_id": "cmd-stale",
                "command_type": "publish_artifact",
                "run_id": "run-a6r",
                "expected_head": {"revision": 1, "transaction_digest": before["transaction_digest"]},
                "workflow_version": "manual-bootstrap/v1",
                "graph_version": "artifact-task-dag/v1",
                "actor": {"role": "orchestrator", "assignment_id": "orchestrator"},
                "authority_ref": self.authority,
                "input_refs": [],
                "idempotency_key": "stale",
                "protected_fields": [],
                "scope": [],
                "payload": {"artifact_id": "late", "version": "v1", "value": {}, "kind": "artifact", "path": None}
            }
            self.kernel.apply(stale)

    def test_only_orchestrator_advances_head_and_worker_submission_is_inbox(self):
        head = self.kernel.head()
        command = self.kernel._command_for("publish_artifact", {"artifact_id": "x", "version": "v1", "value": {}, "kind": "artifact", "path": None}, authority_ref=self.authority)
        command["actor"] = {"role": "worker", "assignment_id": "worker-x"}
        with self.assertRaises(AuthorizationError):
            self.kernel.apply(command)
        self.kernel.publish_task_package("task-a", {"acceptance": ["passes"]}, assignment_id="worker-a", authority_ref=self.authority)
        head = self.kernel.head()
        receipt = self.kernel.submit_task_result("task-a", {"status": "success", "artifact": "ok"}, worker_assignment_id="worker-a")
        self.assertFalse(receipt["accepted"])
        self.assertEqual(head["transaction_digest"], self.kernel.head()["transaction_digest"])
        self.assertTrue((self.kernel.inbox_dir / (receipt["submission_digest"][7:] + ".json")).exists())

    def test_minimal_review_fix_loop_keeps_successful_sibling(self):
        self.kernel.publish_task_package("task-good", {"acceptance": ["ok"]}, assignment_id="worker-good", sibling_group="batch", authority_ref=self.authority)
        self.kernel.publish_task_package("task-bad", {"acceptance": ["review"]}, assignment_id="worker-bad", sibling_group="batch", authority_ref=self.authority)
        self.assertEqual(self.kernel.ready_tasks(), ["task-bad", "task-good"])
        self.kernel.claim_task("task-good", assignment_id="worker-good", authority_ref=self.authority)
        self.kernel.claim_task("task-bad", assignment_id="worker-bad", authority_ref=self.authority)
        self.kernel.accept_task_result("task-good", {"status": "success"}, worker_assignment_id="worker-good", authority_ref=self.authority)
        self.kernel.accept_task_result("task-bad", {"status": "partial"}, worker_assignment_id="worker-bad", authority_ref=self.authority)
        task_ref = self.kernel.read_state()["tasks"]["task-bad"]["package_ref"]
        self.kernel.open_review_epoch("epoch-review", task_ref, reviewer_assignment_id="reviewer-1", authority_ref=self.authority)
        self.kernel.open_review("review-bad", "task-bad", [{"finding_id": "finding-stable", "requirement_ref": "R1", "description": "missing check", "severity": "major", "blocking": True}], reviewer_assignment_id="reviewer-1", fresh_epoch_id="epoch-review", authority_ref=self.authority)
        self.kernel.open_review_epoch("epoch-validator", task_ref, reviewer_assignment_id="validator-1", authority_ref=self.authority)
        self.kernel.validate_findings("review-bad", [{"candidate_id": "finding-stable", "disposition": "required", "reason": "mandatory", "materiality": "material", "requirement_ref": "R1", "permitted_fix_scope": ["task-bad"]}], validator_assignment_id="validator-1", fresh_epoch_id="epoch-validator", authority_ref=self.authority)
        with self.assertRaises(AuthorizationError):
            self.kernel.submit_finding_closure("finding-stable", ["bad"], reviewer_assignment_id="reviewer-1", fresh_epoch_id="epoch-rereview")
        with self.assertRaises(AuthorizationError):
            self.kernel.submit_resolution_claim("finding-stable", ["fix"], worker_assignment_id="worker-not-bad")
        self.kernel.accept_resolution_claim("finding-stable", ["fix"], worker_assignment_id="worker-bad", authority_ref=self.authority)
        state = self.kernel.read_state()
        self.assertNotIn("closed", state["findings"]["finding-stable"])
        self.assertEqual(state["findings"]["finding-stable"]["state"], "resolved")
        self.kernel.open_review_epoch("epoch-rereview", state["findings"]["finding-stable"]["resolution_ref"], reviewer_assignment_id="reviewer-2", authority_ref=self.authority)
        self.kernel.open_review(
            "review-rereview",
            "task-bad",
            [],
            reviewer_assignment_id="reviewer-2",
            fresh_epoch_id="epoch-rereview",
            review_kind="closure",
            target_finding_id="finding-stable",
            authority_ref=self.authority,
        )
        self.kernel.submit_finding_closure("finding-stable", [state["findings"]["finding-stable"]["resolution_ref"]], reviewer_assignment_id="reviewer-2", fresh_epoch_id="epoch-rereview", review_id="review-rereview")
        self.kernel.accept_finding_closure("finding-stable", [state["findings"]["finding-stable"]["resolution_ref"]], reviewer_assignment_id="reviewer-2", fresh_epoch_id="epoch-rereview", review_id="review-rereview", authority_ref=self.authority)
        state = self.kernel.read_state()
        self.assertEqual(state["findings"]["finding-stable"]["state"], "resolved")
        self.assertEqual(state["tasks"]["task-good"]["status"], "succeeded")
        self.assertEqual(state["tasks"]["task-bad"]["status"], "succeeded")
        self.assertNotIn("task-bad", state["leases"])
        self.assertEqual(state["reviews"]["review-bad"]["verdict"], "pass")
        self.kernel.close_epoch(
            acceptance_evidence=["fresh independent closure passed"],
            authority_ref=self.authority,
            token_status="unavailable",
        )
        self.kernel.close_group(
            acceptance_evidence=["sibling retained and reviewed"],
            authority_ref=self.authority,
        )
        resumed = ControlKernel(self.root, "run-a6r").resume()
        self.assertEqual(resumed["status"], "paused_after_group")
        self.assertEqual(resumed["revision"], self.kernel.head()["revision"])
        self.assertEqual(resumed["open_blocking_findings"], [])

    def test_fresh_validator_can_accept_an_empty_review_candidate_set(self):
        self.kernel.publish_task_package(
            "task-clean", {"acceptance": ["clean review"]},
            assignment_id="worker-clean", authority_ref=self.authority,
        )
        self.kernel.claim_task(
            "task-clean", assignment_id="worker-clean", authority_ref=self.authority,
        )
        self.kernel.accept_task_result(
            "task-clean", {"status": "success"},
            worker_assignment_id="worker-clean", authority_ref=self.authority,
        )
        task_ref = self.kernel.read_state()["tasks"]["task-clean"]["package_ref"]
        self.kernel.open_review_epoch(
            "epoch-clean-review", task_ref,
            reviewer_assignment_id="reviewer-clean", authority_ref=self.authority,
        )
        self.kernel.open_review(
            "review-clean", "task-clean", [],
            reviewer_assignment_id="reviewer-clean",
            fresh_epoch_id="epoch-clean-review", authority_ref=self.authority,
        )
        self.kernel.open_review_epoch(
            "epoch-clean-validator", task_ref,
            reviewer_assignment_id="validator-clean", authority_ref=self.authority,
        )
        self.kernel.validate_findings(
            "review-clean", [], validator_assignment_id="validator-clean",
            fresh_epoch_id="epoch-clean-validator", authority_ref=self.authority,
        )
        state = self.kernel.read_state()
        self.assertEqual(state["finding_validations"]["review-clean"]["outcomes"], [])
        self.assertEqual(state["reviews"]["review-clean"]["verdict"], "pass")
        self.assertEqual(state["tasks"]["task-clean"]["status"], "succeeded")
        self.assertNotIn("task-clean", state["leases"])

    def test_required_review_keeps_live_lease_until_task_result(self):
        self.kernel.publish_task_package(
            "task-repair", {"acceptance": ["split repair"]},
            assignment_id="worker-repair", authority_ref=self.authority,
        )
        self.kernel.claim_task(
            "task-repair", assignment_id="worker-repair", authority_ref=self.authority,
        )
        task_ref = self.kernel.read_state()["tasks"]["task-repair"]["package_ref"]
        self.kernel.open_review_epoch(
            "epoch-repair-review", task_ref,
            reviewer_assignment_id="reviewer-repair", authority_ref=self.authority,
        )
        self.kernel.open_review(
            "review-repair", "task-repair",
            [{
                "finding_id": "finding-repair", "requirement_ref": "R-repair",
                "description": "requires a split repair", "severity": "major",
                "blocking": True,
            }],
            reviewer_assignment_id="reviewer-repair",
            fresh_epoch_id="epoch-repair-review", authority_ref=self.authority,
        )
        self.kernel.open_review_epoch(
            "epoch-repair-validator", task_ref,
            reviewer_assignment_id="validator-repair", authority_ref=self.authority,
        )
        self.kernel.validate_findings(
            "review-repair",
            [{
                "candidate_id": "finding-repair", "disposition": "required",
                "reason": "current acceptance blocker", "materiality": "material",
                "requirement_ref": "R-repair", "permitted_fix_scope": ["src/repair.py"],
            }],
            validator_assignment_id="validator-repair",
            fresh_epoch_id="epoch-repair-validator", authority_ref=self.authority,
        )
        blocked = self.kernel.read_state()
        self.assertEqual(blocked["tasks"]["task-repair"]["status"], "blocked_review")
        self.assertEqual(blocked["tasks"]["task-repair"]["lease_status"], "leased")
        self.assertEqual(blocked["leases"]["task-repair"]["status"], "leased")

        invalid_status = json.loads(json.dumps(blocked))
        invalid_status["tasks"]["task-repair"]["status"] = "succeeded"
        with self.assertRaisesRegex(IntegrityBlockedError, "lease/status binding"):
            self.kernel._validate_state(invalid_status, load_objects=False)
        invalid_marker = json.loads(json.dumps(blocked))
        invalid_marker["tasks"]["task-repair"]["lease_status"] = "released"
        with self.assertRaisesRegex(IntegrityBlockedError, "lease/status binding"):
            self.kernel._validate_state(invalid_marker, load_objects=False)

        self.kernel.accept_resolution_claim(
            "finding-repair", ["physical repair evidence"],
            worker_assignment_id="worker-repair", authority_ref=self.authority,
        )
        resolved = self.kernel.read_state()
        self.assertEqual(resolved["tasks"]["task-repair"]["status"], "fix_claimed")
        self.assertEqual(resolved["leases"]["task-repair"]["status"], "leased")
        resolution_ref = resolved["findings"]["finding-repair"]["resolution_ref"]
        self.kernel.open_review_epoch(
            "epoch-repair-closure", resolution_ref,
            reviewer_assignment_id="reviewer-repair-closure", authority_ref=self.authority,
        )
        self.kernel.open_review(
            "review-repair-closure", "task-repair", [],
            reviewer_assignment_id="reviewer-repair-closure",
            fresh_epoch_id="epoch-repair-closure", review_kind="closure",
            target_finding_id="finding-repair", authority_ref=self.authority,
        )
        self.kernel.accept_finding_closure(
            "finding-repair", [resolution_ref],
            reviewer_assignment_id="reviewer-repair-closure",
            fresh_epoch_id="epoch-repair-closure", review_id="review-repair-closure",
            authority_ref=self.authority,
        )
        closed_finding = self.kernel.read_state()
        self.assertEqual(closed_finding["tasks"]["task-repair"]["status"], "leased")
        self.assertIsNone(closed_finding["tasks"]["task-repair"]["result_ref"])
        self.assertEqual(closed_finding["leases"]["task-repair"]["status"], "leased")

        self.kernel.accept_task_result(
            "task-repair", {"status": "success"},
            worker_assignment_id="worker-repair", authority_ref=self.authority,
        )
        completed = self.kernel.read_state()
        self.assertEqual(completed["tasks"]["task-repair"]["status"], "succeeded")
        self.assertIsNotNone(completed["tasks"]["task-repair"]["result_ref"])
        self.assertNotIn("task-repair", completed["leases"])

    def test_duplicate_and_fault_recovery(self):
        command = self.kernel._command_for("publish_artifact", {"artifact_id": "once", "version": "v1", "value": 1, "kind": "artifact", "path": None}, authority_ref=self.authority, idempotency_key="once-key")
        self.kernel.apply(command)
        revision = self.kernel.head()["revision"]
        duplicate = self.kernel.apply(command)
        self.assertEqual(duplicate["revision"], revision)
        self.assertTrue(self.kernel.last_receipt["duplicate"])
        other = dict(command)
        other["command_id"] = "cmd-different"
        other["payload"] = {"artifact_id": "different", "version": "v1", "value": 2, "kind": "artifact", "path": None}
        with self.assertRaises(DuplicateCommandError):
            self.kernel.apply(other)
        self.kernel.publish_artifact("wrapper-once", "v1", 1, authority_ref=self.authority, idempotency_key="wrapper-key")
        wrapper_duplicate = self.kernel.publish_artifact("wrapper-once", "v1", 1, authority_ref=self.authority, idempotency_key="wrapper-key")
        self.assertEqual(wrapper_duplicate["revision"], self.kernel.head()["revision"])
        self.assertTrue(self.kernel.last_receipt["duplicate"])

        for phase in ("before_publish", "after_publish_before_head", "after_head_before_projection"):
            k = ControlKernel(self.root / phase, "run-a6r")
            k.entry(self.objective, authority_ref=self.authority)
            with self.assertRaises(InjectedCrash):
                with k.fault(phase):
                    k.publish_artifact("faulty", "v1", {"phase": phase}, authority_ref=self.authority)
            old_head = k.head()
            if phase != "after_head_before_projection":
                self.assertEqual(old_head["revision"], 1)
            report = k.recover()
            self.assertTrue(report["head_unchanged"])
            self.assertEqual(k.read_state()["revision"], 2 if phase == "after_head_before_projection" else 1)

    def test_corruption_blocks_without_silent_advance_and_projection_rebuilds(self):
        self.kernel.publish_artifact("stable", "v1", {"ok": True}, authority_ref=self.authority)
        head_before = self.kernel.head()
        self.kernel.projection_dir.joinpath("status.json").unlink()
        self.assertEqual(self.kernel.read_state()["revision"], 2)
        self.assertTrue(self.kernel.projection_dir.joinpath("status.json").exists())
        self.kernel.projection_dir.joinpath("plan.yaml").unlink()
        self.assertEqual(self.kernel.read_state()["revision"], 2)
        self.assertTrue(self.kernel.projection_dir.joinpath("plan.yaml").exists())
        head_path = self.kernel.head_path
        value = json.loads(head_path.read_text())
        value["revision"] = 999
        head_path.write_text(json.dumps(value))
        with self.assertRaises(IntegrityBlockedError):
            self.kernel.read_state()
        self.assertEqual(head_before["revision"], 2)
        self.assertEqual(self.kernel.head_path.exists(), True)

    def test_budget_and_epoch_group_close(self):
        self.kernel.close_epoch(acceptance_evidence=["review passed"], authority_ref=self.authority, token_status="unavailable")
        self.kernel.close_group(acceptance_evidence=["group passed"], authority_ref=self.authority)
        resumed = ControlKernel(self.root, "run-a6r").resume(expected_workflow_version="manual-bootstrap/v1")
        self.assertEqual(resumed["status"], "paused_after_group")
        self.assertEqual(resumed["revision"], 3)

    def test_fresh_process_resume(self):
        self.kernel.publish_task_package("task-fresh", {"acceptance": ["fresh"]}, assignment_id="worker-fresh", authority_ref=self.authority)
        env = os.environ.copy()
        env["PYTHONPATH"] = str(SRC)
        code = "from ai_agent_workflow.control_kernel import ControlKernel; import json; print(json.dumps(ControlKernel(%r, 'run-a6r').resume()))" % str(self.root)
        result = subprocess.run([sys.executable, "-c", code], env=env, text=True, capture_output=True, check=True)
        self.assertEqual(json.loads(result.stdout)["revision"], 2)

    def test_direct_run_and_keyword_entry_forms(self):
        generic_authority = {"status": "approved", "scopes": ["*"]}
        direct = ControlKernel(self.root / "direct")
        direct.entry("run-direct", objective=self.objective, authority=generic_authority)
        self.assertEqual(direct.resume()["run_id"], "run-direct")
        keyword = ControlKernel(self.root, "run-keyword")
        keyword.entry(objective_ref=self.objective, authority_ref=generic_authority)
        self.assertEqual(keyword.resume()["run_id"], "run-keyword")

    def test_apply_approve_objective_atomically_advances_current_objective(self):
        prior = self.kernel.read_state()["objective_ref"]
        candidate_digest = "sha256:" + "b" * 64
        proposal_digest = "sha256:" + "c" * 64
        receipt = {
            "schema": "human-approval-receipt/v1", "receipt_id": "receipt-s2-u",
            "approval_id": "approval-s2-u",
            "decision": "approve", "explicit": True, "source": "human",
            "actor_id": "human-fixture", "run_id": "run-a6r",
            "namespace": "fixture:s2-u", "approval_scope": "fixture-only",
            "candidate_path": "objectives/v002.md", "candidate_version": "v002",
            "candidate_namespace": "fixture:s2-u", "candidate_digest": candidate_digest,
            "prior_objective_digest": prior["digest"], "prior_objective_version": prior["version"],
            "proposal_digest": proposal_digest,
            "issued_at": "2026-09-05T00:00:00Z",
        }
        approval = {
            "schema": "objective-approval/v1", "approval_id": "approval-s2-u",
            "run_id": "run-a6r", "namespace": "fixture:s2-u",
            "approval_scope": "fixture-only", "decision": "approve",
            "actor": {"kind": "human", "actor_id": "human-fixture"},
            "receipt": receipt, "candidate_digest": candidate_digest,
            "candidate_version": "v002", "prior_objective_digest": prior["digest"],
            "prior_objective_version": prior["version"], "proposal_digest": proposal_digest,
        }
        authority = {
            "status": "approved", "scopes": ["approve_objective"],
            "run_id": "run-a6r", "namespace": "fixture:s2-u",
            "approval_scope": "fixture-only", "actor_id": "human-fixture",
            "proposal_digest": proposal_digest, "human_receipt": receipt,
            "protected_fields": ["objective_ref"], "write_scopes": ["fixture:s2-u"],
        }
        command = self.kernel.make_command(
            "approve_objective",
            {"candidate_ref": {"path": "objectives/v002.md", "version": "v002", "digest": candidate_digest, "namespace": "fixture:s2-u"},
             "prior_objective": {"version": prior["version"], "digest": prior["digest"]},
             "proposal_digest": proposal_digest, "approval": approval},
            authority_ref=authority, idempotency_key="approve-s2-u",
            protected_fields=["objective_ref"], scope=["fixture:s2-u"],
        )
        result = self.kernel.apply(command)
        self.assertEqual(result["objective_ref"]["version"], "v002")
        self.assertEqual(result["objective_history"][0]["version"], "v001")
        self.assertEqual(len(result["objective_approvals"]), 1)
        self.assertEqual(result["objective_events"][0]["event"], "objective-approved")


if __name__ == "__main__":
    unittest.main()
