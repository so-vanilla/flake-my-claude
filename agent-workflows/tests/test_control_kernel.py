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

from ai_agent_workflow.control_kernel import (
    AuthorizationError,
    ControlKernel,
    DuplicateCommandError,
    InjectedCrash,
    IntegrityBlockedError,
    KernelError,
    LifecycleClosedError,
    StaleHeadError,
)
from ai_agent_workflow.loop_contracts import canonical_digest
from ai_agent_workflow.loop_state import (
    CommandConflictError,
    RecoveryRequiredError,
    ResultRejectedError,
    event_ref,
)
from ai_agent_workflow.schema_validation import SchemaValidationError, validate_document


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
            with self.assertRaises(InjectedCrash), k.fault(phase):
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
        code = f"from ai_agent_workflow.control_kernel import ControlKernel; import json; print(json.dumps(ControlKernel({str(self.root)!r}, 'run-a6r').resume()))"
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

    def _loop_fixture(self):
        kernel = ControlKernel(self.root, "run-loop")
        authority = {
            "status": "approved", "scopes": ["*"],
            "fixture_identity": {
                "schema": "canonical-fixture-identity/v1", "run_id": "run-loop",
                "namespace": "fixture:loop", "approval_scope": "fixture-only",
            },
        }
        identity = {
            "schema": "loop-work-identity/v1", "work_lineage_id": "lineage-1",
            "logical_task_id": "task-1", "phase": "B", "scope_revision": "scope-1",
            "requirements_digest": "sha256:" + "a" * 64, "predecessor_ref": None,
        }
        kernel.entry(
            {"path": "objectives/loop.md", "version": "v001", "digest": "b" * 64},
            authority_ref=authority,
            loop_control={"identity": identity, "history": []},
        )
        return kernel, authority, identity

    def test_loop_entry_keeps_context_budget_and_omits_legacy_progress_fields(self):
        kernel, _, _ = self._loop_fixture()
        state = kernel.read_state()
        self.assertIn("loop_control", state)
        self.assertEqual(
            set(state["loop_control"]),
            {
                "schema", "identity", "event_refs", "archives", "counter_identity", "counters",
                "status", "outcome", "terminal_outcome", "recovery_required",
                "dispatch_allowed", "recovery", "control_refs", "terminal_ref",
                "terminal_record",
            },
        )
        self.assertNotIn("review_budget", state)
        self.assertNotIn("budget_terminal", state)
        self.assertNotIn("terminal_history", state)
        self.assertNotIn("expires_at", state["authority"])
        self.assertEqual(
            state["loop_control"]["counters"]["counter_key"],
            state["loop_control"]["counter_identity"],
        )
        self.assertEqual(state["loop_control"]["counters"]["policy"]["policy_id"], "B")
        self.assertEqual(
            state["context_budget"],
            {"target": 200000, "normal_limit": 300000, "absolute_limit": 500000,
             "token_status": "unavailable", "token_count": None},
        )
        legacy = self.kernel.read_state()
        self.assertNotIn("loop_control", legacy)

        registry = {
            path.name: json.loads(path.read_text())
            for path in (ROOT / "schemas").glob("*.schema.json")
        }
        validate_document(state, registry["dag-state-v1.schema.json"], registry)
        mixed = json.loads(json.dumps(state))
        mixed["review_budget"] = {}
        with self.assertRaises(SchemaValidationError):
            validate_document(mixed, registry["dag-state-v1.schema.json"], registry)

    def test_loop_run_keeps_normal_dag_operations_but_rejects_legacy_progress_commands(self):
        kernel, authority, _ = self._loop_fixture()
        state = kernel.publish_artifact(
            "loop-artifact",
            "v1",
            {"value": "available"},
            authority_ref=authority,
        )
        self.assertEqual(state["artifacts"]["loop-artifact"]["version"], "v1")
        with self.assertRaises(AuthorizationError):
            kernel.terminal_review("rounds_exhausted", unresolved_finding_ids=[], authority_ref=authority)

    def test_entry_rejects_mixed_progress_control_schemas(self):
        authority = {"status": "approved", "scopes": ["*"]}
        identity = {
            "schema": "loop-work-identity/v1", "work_lineage_id": "lineage-1",
            "logical_task_id": "task-1", "phase": "B", "scope_revision": "scope-1",
            "requirements_digest": "sha256:" + "a" * 64, "predecessor_ref": None,
        }
        kernel = ControlKernel(self.root / "mixed", "run-mixed")
        with self.assertRaises(KernelError):
            kernel.entry(
                self.objective,
                authority_ref=authority,
                review_budget={"version": "v1"},
                loop_control={"identity": identity, "history": []},
            )

    def test_kernel_loop_cas_idempotency_and_counter_identity(self):
        kernel, authority, identity = self._loop_fixture()
        first = kernel.reserve_loop_event(
            identity=identity, command_id="command-initial", authority_ref=authority,
            expected_revision=1, idempotency_key="loop-reserve-initial",
        )
        duplicate = kernel.reserve_loop_event(
            identity=identity, command_id="command-initial", authority_ref=authority,
            expected_revision=2, idempotency_key="loop-reserve-initial",
        )
        self.assertEqual(duplicate["revision"], first["revision"])
        self.assertEqual(len(duplicate["loop_control"]["event_refs"]), 1)
        with self.assertRaises(StaleHeadError):
            kernel.mark_loop_running("event-command-initial", authority_ref=authority, expected_revision=1)
        kernel.mark_loop_running(
            "event-command-initial", authority_ref=authority, expected_revision=2,
        )
        accepted = kernel.accept_loop_result(
            "event-command-initial",
            {"id": "result-initial", "digest": "sha256:" + "c" * 64},
            authority_ref=authority,
            expected_revision=3,
        )
        event = kernel.read_object(accepted["loop_control"]["event_refs"][0])["payload"]
        changed_identity = dict(identity, scope_revision="scope-2")
        next_state = kernel.reserve_loop_event(
            identity=changed_identity,
            command_id="command-improvement",
            event_id="event-improvement",
            kind="improvement",
            attempt=1,
            predecessor_ref=event_ref(event),
            authority_ref=authority,
            expected_revision=4,
        )
        self.assertEqual(
            next_state["loop_control"]["counter_identity"],
            first["loop_control"]["counter_identity"],
        )
        self.assertEqual(next_state["loop_control"]["counters"]["additional_iterations"], 1)
        with self.assertRaises(CommandConflictError):
            kernel.reserve_loop_event(
                identity=identity, command_id="command-improvement", event_id="other-event",
                kind="improvement", attempt=1, predecessor_ref=event_ref(event),
                authority_ref=authority, idempotency_key="different-idempotency-key",
            )
        with self.assertRaises(DuplicateCommandError):
            kernel.reserve_loop_event(
                identity=identity, command_id="command-other", authority_ref=authority,
                idempotency_key="loop-reserve-initial",
            )

    def test_loop_result_unknown_and_explicit_recovery_are_not_budget_terminals(self):
        kernel, authority, identity = self._loop_fixture()
        with self.assertRaises(ResultRejectedError):
            kernel.accept_loop_result(
                "event-before-reservation", {"id": "result", "digest": "sha256:" + "c" * 64},
                authority_ref=authority,
            )
        kernel.reserve_loop_event(identity=identity, command_id="command-unknown", authority_ref=authority)
        kernel.mark_loop_running("event-command-unknown", authority_ref=authority)
        unknown = kernel.mark_loop_execution_unknown("event-command-unknown", authority_ref=authority)
        self.assertEqual(unknown["loop_control"]["outcome"], "recovery-required")
        self.assertTrue(unknown["loop_control"]["recovery_required"])
        self.assertNotIn("review_budget", unknown)
        self.assertNotIn("budget_terminal", unknown)
        with self.assertRaises(RecoveryRequiredError):
            kernel.reserve_loop_event(
                identity=identity, command_id="command-blocked", kind="improvement", attempt=1,
                predecessor_ref=event_ref(kernel.read_object(unknown["loop_control"]["event_refs"][0])["payload"]),
                authority_ref=authority,
            )
        recovered = kernel.recover_loop_execution(
            "event-command-unknown",
            resolution="accept-result",
            evidence_ref={"id": "recovery-evidence", "digest": "sha256:" + "d" * 64},
            result_ref={"id": "result-unknown", "digest": "sha256:" + "e" * 64},
            authority_ref=authority,
        )
        self.assertEqual(recovered["loop_control"]["status"], "evaluated")
        self.assertFalse(recovered["loop_control"]["recovery_required"])
        self.assertEqual(recovered["loop_control"]["recovery"]["resolution"], "accept-result")

    def test_phase_transition_archives_immutable_history_without_counter_reset(self):
        kernel, authority, identity = self._loop_fixture()
        kernel.reserve_loop_event(identity=identity, command_id="command-b", authority_ref=authority)
        kernel.mark_loop_running("event-command-b", authority_ref=authority)
        evaluated = kernel.accept_loop_result(
            "event-command-b", {"id": "result-b", "digest": "sha256:" + "c" * 64},
            authority_ref=authority,
        )
        old_ref = evaluated["loop_control"]["event_refs"][0]
        next_identity = dict(identity, phase="E4", scope_revision="scope-e4")
        transitioned = kernel.transition_loop_phase(
            next_identity, authority_ref=authority, expected_revision=4,
        )
        self.assertEqual(transitioned["loop_control"]["event_refs"], [])
        self.assertEqual(len(transitioned["loop_control"]["archives"]), 1)
        self.assertEqual(transitioned["loop_control"]["archives"][0]["event_refs"], [old_ref])
        self.assertIn(old_ref["digest"], transitioned["object_refs"])
        self.assertEqual(transitioned["loop_control"]["counter_identity"]["policy_id"], "E3-E7")
        self.assertEqual(kernel.read_state()["loop_control"]["archives"][0]["event_refs"], [old_ref])
        with self.assertRaises(KernelError):
            kernel.transition_loop_phase(
                dict(next_identity, scope_revision="scope-e5"), authority_ref=authority,
            )
        resumed = kernel.transition_loop_phase(
            dict(identity, scope_revision="scope-b2"), authority_ref=authority,
        )
        self.assertEqual(resumed["loop_control"]["identity"]["phase"], "B")
        self.assertEqual(resumed["loop_control"]["event_refs"], [old_ref])
        self.assertEqual(resumed["loop_control"]["counters"]["initial"], 1)
        self.assertEqual(
            resumed["loop_control"]["archives"][0]["counter_identity"]["policy_id"],
            "E3-E7",
        )
        with self.assertRaises(AuthorizationError):
            kernel.transition_loop_phase(
                dict(identity, work_lineage_id="lineage-other", phase="C"),
                authority_ref=authority,
            )

    def test_loop_incomplete_terminal_is_durable_and_requires_explicit_resume(self):
        kernel, authority, identity = self._loop_fixture()
        terminal = {
            "schema": "loop-terminal-record/v1",
            "terminal_id": "terminal-input-1",
            "identity": identity,
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
        stopped = kernel.record_loop_outcome(terminal, authority_ref=authority)
        self.assertEqual(stopped["loop_control"]["outcome"], "needs-input")
        self.assertFalse(stopped["loop_control"]["dispatch_allowed"])
        self.assertEqual(len(stopped["loop_control"]["control_refs"]), 1)
        with self.assertRaises(LifecycleClosedError):
            kernel.reserve_loop_event(identity=identity, command_id="blocked", authority_ref=authority)

        terminal_ref = stopped["loop_control"]["terminal_ref"]
        resumed = kernel.resume_loop_outcome(
            {
                "schema": "loop-resume-record/v1",
                "resume_id": "resume-input-1",
                "identity": identity,
                "terminal_ref": {"path": terminal_ref["path"], "digest": terminal_ref["digest"]},
                "reason": "the required choice was supplied",
                "evidence_ref": {"id": "answer-1", "digest": "sha256:" + "e" * 64},
                "non_authorizing": True,
            },
            authority_ref=authority,
        )
        self.assertIsNone(resumed["loop_control"]["terminal_ref"])
        self.assertTrue(resumed["loop_control"]["dispatch_allowed"])
        self.assertEqual(len(resumed["loop_control"]["control_refs"]), 2)
        kernel.reserve_loop_event(identity=identity, command_id="after-resume", authority_ref=authority)
        self.assertTrue(kernel.verify_integrity()["ok"])

    def test_completed_loop_terminal_rechecks_machine_predicate(self):
        kernel, authority, identity = self._loop_fixture()
        kernel.reserve_loop_event(identity=identity, command_id="command-complete", authority_ref=authority)
        kernel.mark_loop_running("event-command-complete", authority_ref=authority)
        candidate_digest = "sha256:" + "c" * 64
        kernel.accept_loop_result(
            "event-command-complete",
            {"id": "candidate-1", "digest": candidate_digest},
            authority_ref=authority,
        )
        evidence = {
            "schema": "loop-evidence-record/v1", "evidence_id": "evidence-1",
            "evidence_digest": "", "candidate_digest": candidate_digest,
            "spec_digest": "sha256:" + "d" * 64,
            "source_digest": "sha256:" + "e" * 64,
            "dependency_digest": "sha256:" + "f" * 64,
            "environment_digest": "sha256:" + "1" * 64,
            "check_definition_digest": "sha256:" + "2" * 64,
            "coverage": ["R1"], "status": "pass",
        }
        evidence["evidence_digest"] = canonical_digest({
            key: value for key, value in evidence.items() if key != "evidence_digest"
        })
        requirement = {
            "schema": "loop-requirement-assessment/v1", "requirement_id": "R1",
            "status": "pass", "scope": ["src/a.py"],
            "evidence_refs": [{"id": "evidence-1", "digest": evidence["evidence_digest"]}],
        }
        reviews = [
            {
                "schema": "loop-review-assessment/v1", "review_id": "review-architecture",
                "axis": "architecture-safety", "actor_id": "reviewer-a",
                "context_epoch": "epoch-a", "candidate_digest": candidate_digest,
                "package_digest": "sha256:" + "3" * 64, "coverage": ["R1"],
                "completed": True, "unevaluated": [], "finding_refs": [],
            },
            {
                "schema": "loop-review-assessment/v1", "review_id": "review-integration",
                "axis": "integration-operability", "actor_id": "reviewer-b",
                "context_epoch": "epoch-b", "candidate_digest": candidate_digest,
                "package_digest": "sha256:" + "3" * 64, "coverage": ["R1"],
                "completed": True, "unevaluated": [], "finding_refs": [],
            },
        ]
        terminal = {
            "schema": "loop-terminal-record/v1", "terminal_id": "terminal-complete-1",
            "identity": identity, "outcome": "completed", "reason": "all gates passed",
            "candidate_ref": {"id": "candidate-1", "digest": candidate_digest},
            "requirements": [requirement], "reviews": reviews, "evidence": [evidence],
            "open_items": [], "resume_ref": None, "non_authorizing": True,
        }
        foreign = json.loads(json.dumps(terminal))
        foreign_digest = "sha256:" + "9" * 64
        foreign["terminal_id"] = "terminal-foreign-1"
        foreign["candidate_ref"]["digest"] = foreign_digest
        foreign["evidence"][0]["candidate_digest"] = foreign_digest
        foreign["evidence"][0]["evidence_digest"] = canonical_digest({
            key: value
            for key, value in foreign["evidence"][0].items()
            if key != "evidence_digest"
        })
        foreign["requirements"][0]["evidence_refs"][0]["digest"] = foreign["evidence"][0]["evidence_digest"]
        for review in foreign["reviews"]:
            review["candidate_digest"] = foreign_digest
        with self.assertRaisesRegex(KernelError, "latest evaluated result"):
            kernel.record_loop_outcome(foreign, authority_ref=authority)
        completed = kernel.record_loop_outcome(terminal, authority_ref=authority)
        self.assertEqual(completed["loop_control"]["outcome"], "completed")
        self.assertFalse(completed["loop_control"]["dispatch_allowed"])
        self.assertEqual(
            kernel.read_object(completed["loop_control"]["terminal_ref"])["payload"],
            terminal,
        )
        self.assertTrue(kernel.verify_integrity()["ok"])


if __name__ == "__main__":
    unittest.main()
