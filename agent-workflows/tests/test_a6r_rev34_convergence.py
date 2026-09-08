import sys
import tempfile
import unittest
import json
import os
import subprocess
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "src"))

from ai_agent_workflow.control_kernel import AuthorizationError, ControlKernel  # noqa: E402
from ai_agent_workflow.schema_validation import validate_document  # noqa: E402


AUTHORITY = {"status": "approved", "scopes": ["*"]}
OBJECTIVE = {"path": "objectives/v001.md", "version": "v001", "digest": "a" * 64}


def replacement_budget():
    return {"version": "v2", "deadline": "9999-12-31T23:59:59+00:00", "max_rounds": 2, "max_attempts_per_finding": 2, "rounds_used": 0, "finding_attempts": {}}


def reopen_receipt(kernel, approval_id, budget, head):
    expected = {"revision": head["revision"], "transaction_digest": head["transaction_digest"]}
    return {
        "status": "approved", "scopes": ["reopen_review"], "run_id": kernel.run_id,
        "approval_receipt": {
            "approval_id": approval_id, "operation": "reopen_review", "run_id": kernel.run_id,
            "replacement_budget": budget, "replacement_budget_digest": kernel._command_digest(budget),
            "expected_head": expected,
        },
    }


class Revision34ConvergenceTests(unittest.TestCase):
    def test_convergence_fixture_covers_required_adversarial_branches(self):
        fixture = json.loads((ROOT / "tests" / "fixtures" / "a6r" / "review-convergence-v1.json").read_text())
        self.assertEqual(fixture["schema"], "a6r-review-convergence/v1")
        self.assertEqual(set(fixture["branches"]), {"duplicate", "approved_choice", "immaterial", "required", "needs_user", "time_exhausted", "rounds_exhausted", "attempts_exhausted", "guarded_reopen"})

    def test_validator_admission_is_durable_and_recoverable(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            kernel = ControlKernel(root, "rev34")
            kernel.entry(OBJECTIVE, authority_ref=AUTHORITY)
            kernel.publish_task_package("task-a", {"acceptance": ["ok"]}, assignment_id="worker-a", authority_ref=AUTHORITY)
            task_ref = kernel.read_state()["tasks"]["task-a"]["package_ref"]
            kernel.open_review_epoch("review-epoch", task_ref, reviewer_assignment_id="reviewer-a", authority_ref=AUTHORITY)
            kernel.open_review("review-a", "task-a", [{"finding_id": "candidate-a", "requirement_ref": "R-1", "description": "required gap", "severity": "major", "blocking": True}], reviewer_assignment_id="reviewer-a", fresh_epoch_id="review-epoch", authority_ref=AUTHORITY)
            kernel.open_review_epoch("validator-epoch", task_ref, reviewer_assignment_id="validator-a", authority_ref=AUTHORITY)
            kernel.validate_findings("review-a", [{"candidate_id": "candidate-a", "disposition": "required", "reason": "mandatory", "materiality": "material", "requirement_ref": "R-1", "permitted_fix_scope": ["task-a"]}], validator_assignment_id="validator-a", fresh_epoch_id="validator-epoch", authority_ref=AUTHORITY)

            recovered = ControlKernel(root, "rev34").read_state()
            validation = recovered["finding_validations"]["review-a"]
            self.assertEqual(validation["outcomes"][0]["disposition"], "required")
            self.assertEqual(recovered["findings"]["candidate-a"]["state"], "open")
            self.assertEqual(recovered["findings"]["candidate-a"]["validator_assignment_id"], "validator-a")
            result = subprocess.run([sys.executable, "-c", "from ai_agent_workflow.control_kernel import ControlKernel; import json; print(json.dumps(ControlKernel(%r, 'rev34').read_state()['finding_validations']['review-a']['outcomes']))" % str(root)], env={**os.environ, "PYTHONPATH": str(ROOT / "src")}, text=True, capture_output=True, check=True)
            self.assertEqual(json.loads(result.stdout)[0]["candidate_id"], "candidate-a")

    def test_terminal_stops_dispatch_and_guarded_reopen_preserves_history(self):
        with tempfile.TemporaryDirectory() as directory:
            kernel = ControlKernel(Path(directory), "terminal")
            kernel.entry(OBJECTIVE, authority_ref=AUTHORITY)
            kernel.publish_task_package("task-a", {"acceptance": ["ok"]}, assignment_id="worker-a", authority_ref=AUTHORITY)
            kernel.terminal_review("rounds_exhausted", unresolved_finding_ids=["finding-a"], authority_ref=AUTHORITY)
            self.assertEqual(kernel.ready_tasks(), [])
            terminal = kernel.read_state()["budget_terminal"]
            self.assertEqual(terminal["reason"], "rounds_exhausted")
            head = kernel.head()
            budget = replacement_budget()
            kernel.reopen_review("approved-reopen", budget, expected_head=head, authority_ref=reopen_receipt(kernel, "approved-reopen", budget, head))
            state = kernel.read_state()
            self.assertEqual(state["status"], "active")
            self.assertIsNone(state["budget_terminal"])
            self.assertEqual(state["terminal_history"][0]["reason"], "rounds_exhausted")

    def test_wall_clock_exhaustion_creates_a_durable_non_dispatch_terminal(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            kernel = ControlKernel(root, "expired")
            kernel.entry(OBJECTIVE, authority_ref=AUTHORITY, review_budget={"version": "v1", "deadline": "2000-01-01T00:00:00+00:00", "max_rounds": 2, "max_attempts_per_finding": 2, "rounds_used": 0, "finding_attempts": {}})
            kernel.publish_task_package("task-a", {"acceptance": ["ok"]}, assignment_id="worker-a", authority_ref=AUTHORITY)
            kernel.claim_task("task-a", assignment_id="worker-a", authority_ref=AUTHORITY)
            recovered = ControlKernel(root, "expired").read_state()
            self.assertEqual(recovered["budget_terminal"]["reason"], "time_exhausted")
            self.assertEqual(recovered["tasks"]["task-a"]["status"], "invalidated")
            self.assertEqual(recovered["leases"], {})
            self.assertEqual(ControlKernel(root, "expired").ready_tasks(), [])

    def test_fixture_rounds_exhaustion_is_automatic_and_fresh_process_recoverable(self):
        """Exercise the fixture contract through the public review dispatch seam."""
        fixture = json.loads((ROOT / "tests" / "fixtures" / "a6r" / "review-convergence-v1.json").read_text())
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            kernel = ControlKernel(root, "rounds-exhausted")
            kernel.entry(OBJECTIVE, authority_ref=AUTHORITY, review_budget={"version": "v1", "deadline": "9999-12-31T23:59:59+00:00", "max_rounds": 1, "max_attempts_per_finding": 1, "rounds_used": 0, "finding_attempts": {}})
            kernel.publish_task_package("task-a", {"acceptance": ["ok"]}, assignment_id="worker-a", authority_ref=AUTHORITY)
            task_ref = kernel.read_state()["tasks"]["task-a"]["package_ref"]
            kernel.open_review_epoch("review-1", task_ref, reviewer_assignment_id="reviewer-1", authority_ref=AUTHORITY)
            kernel.open_review("review-1", "task-a", [{"finding_id": "finding-1", "requirement_ref": "R-1", "description": "first round", "severity": "major", "blocking": True}], reviewer_assignment_id="reviewer-1", fresh_epoch_id="review-1", authority_ref=AUTHORITY)

            kernel.open_review_epoch("review-2", task_ref, reviewer_assignment_id="reviewer-2", authority_ref=AUTHORITY)
            kernel.open_review("review-2", "task-a", [{"finding_id": "finding-2", "requirement_ref": "R-2", "description": "exhausted round", "severity": "major", "blocking": True}], reviewer_assignment_id="reviewer-2", fresh_epoch_id="review-2", authority_ref=AUTHORITY)

            result = subprocess.run(
                [sys.executable, "-c", "from ai_agent_workflow.control_kernel import ControlKernel; import json; k = ControlKernel(%r, 'rounds-exhausted'); s = k.read_state(); print(json.dumps({'reason': s['budget_terminal']['reason'], 'status': s['status'], 'ready_tasks': k.ready_tasks()}))" % str(root)],
                env={**os.environ, "PYTHONPATH": str(ROOT / "src")},
                text=True,
                capture_output=True,
                check=True,
            )
            recovered = json.loads(result.stdout)
            self.assertEqual(recovered["reason"], fixture["branches"]["rounds_exhausted"]["terminal_reason"])
            self.assertEqual(recovered["status"], "terminal")
            self.assertEqual(recovered["ready_tasks"], [])

    def test_fixture_branches_execute_at_the_public_recovery_seam(self):
        fixture = json.loads((ROOT / "tests" / "fixtures" / "a6r" / "review-convergence-v1.json").read_text())
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            budget = {"version": "v1", "deadline": "9999-12-31T23:59:59+00:00", "max_rounds": 1, "max_attempts_per_finding": 1, "rounds_used": 0, "finding_attempts": {}}
            kernel = ControlKernel(root, "branches")
            kernel.entry(OBJECTIVE, authority_ref=AUTHORITY, review_budget=budget)
            kernel.publish_task_package("task-a", {"acceptance": ["ok"]}, assignment_id="worker-a", authority_ref=AUTHORITY)
            task_ref = kernel.read_state()["tasks"]["task-a"]["package_ref"]
            kernel.open_review_epoch("review-1", task_ref, reviewer_assignment_id="reviewer-1", authority_ref=AUTHORITY)
            kernel.open_review("review-1", "task-a", [{"finding_id": "finding-required", "requirement_ref": "R-1", "description": "gap", "severity": "major", "blocking": True}], reviewer_assignment_id="reviewer-1", fresh_epoch_id="review-1", authority_ref=AUTHORITY)
            kernel.open_review_epoch("validator-1", task_ref, reviewer_assignment_id="validator-1", authority_ref=AUTHORITY)
            kernel.validate_findings("review-1", [{"candidate_id": "finding-required", "disposition": fixture["branches"]["required"]["disposition"], "reason": "required", "materiality": "material", "requirement_ref": "R-1", "permitted_fix_scope": ["task-a"]}], validator_assignment_id="validator-1", fresh_epoch_id="validator-1", authority_ref=AUTHORITY)
            kernel.accept_resolution_claim("finding-required", [], worker_assignment_id="worker-a", authority_ref=AUTHORITY)
            # The second claim is rejected by durable automatic accounting, not by an ad-hoc test branch.
            kernel.accept_resolution_claim("finding-required", [], worker_assignment_id="worker-a", authority_ref=AUTHORITY)
            recovered = ControlKernel(root, "branches").read_state()
            self.assertEqual(recovered["budget_terminal"]["reason"], fixture["branches"]["attempts_exhausted"]["terminal_reason"])
            head = ControlKernel(root, "branches").head()
            replacement = {"version": "v2", "deadline": "9999-12-31T23:59:59+00:00", "max_rounds": 2, "max_attempts_per_finding": 2, "rounds_used": 0, "finding_attempts": {}}
            ControlKernel(root, "branches").reopen_review(fixture["branches"]["guarded_reopen"]["approval"], replacement, expected_head=head, authority_ref=reopen_receipt(kernel, fixture["branches"]["guarded_reopen"]["approval"], replacement, head))
            self.assertEqual(ControlKernel(root, "branches").read_state()["status"], "active")

    def test_duplicate_reuses_canonical_finding_without_another_round(self):
        with tempfile.TemporaryDirectory() as directory:
            kernel = ControlKernel(Path(directory), "duplicate")
            kernel.entry(OBJECTIVE, authority_ref=AUTHORITY, review_budget={"version": "v1", "deadline": "9999-12-31T23:59:59+00:00", "max_rounds": 3, "max_attempts_per_finding": 2, "rounds_used": 0, "finding_attempts": {}})
            kernel.publish_task_package("task-a", {"acceptance": ["ok"]}, assignment_id="worker-a", authority_ref=AUTHORITY)
            ref = kernel.read_state()["tasks"]["task-a"]["package_ref"]
            finding = {"finding_id": "finding-required", "requirement_ref": "R-1", "description": "gap", "severity": "major", "blocking": True}
            kernel.open_review_epoch("review-1", ref, reviewer_assignment_id="reviewer-1", authority_ref=AUTHORITY)
            kernel.open_review("review-1", "task-a", [finding], reviewer_assignment_id="reviewer-1", fresh_epoch_id="review-1", authority_ref=AUTHORITY)
            used = kernel.read_state()["review_budget"]["rounds_used"]
            kernel.open_review_epoch("review-2", ref, reviewer_assignment_id="reviewer-2", authority_ref=AUTHORITY)
            kernel.open_review("review-2", "task-a", [finding], reviewer_assignment_id="reviewer-2", fresh_epoch_id="review-2", authority_ref=AUTHORITY)
            recovered = ControlKernel(Path(directory), "duplicate").read_state()
            self.assertEqual(recovered["review_budget"]["rounds_used"], used)
            self.assertEqual(set(recovered["findings"]), {"finding-required"})

    def test_same_fingerprint_different_id_uses_canonical_finding_and_e6_provenance(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            kernel = ControlKernel(root, "fingerprint-duplicate")
            kernel.entry(OBJECTIVE, authority_ref=AUTHORITY, review_budget={"version": "v1", "deadline": "9999-12-31T23:59:59+00:00", "max_rounds": 3, "max_attempts_per_finding": 2, "rounds_used": 0, "finding_attempts": {}})
            kernel.publish_task_package("task-a", {"acceptance": ["ok"]}, assignment_id="worker-a", authority_ref=AUTHORITY)
            ref = kernel.read_state()["tasks"]["task-a"]["package_ref"]
            original = {"finding_id": "original", "requirement_ref": "R-1", "description": "same gap", "severity": "major", "blocking": True}
            kernel.open_review_epoch("review-1", ref, reviewer_assignment_id="reviewer-1", authority_ref=AUTHORITY)
            kernel.open_review("review-1", "task-a", [original], reviewer_assignment_id="reviewer-1", fresh_epoch_id="review-1", authority_ref=AUTHORITY)
            original_ref = kernel.read_state()["findings"]["original"]["object_ref"]
            used = kernel.read_state()["review_budget"]["rounds_used"]
            kernel.open_review_epoch("review-2", ref, reviewer_assignment_id="reviewer-2", authority_ref=AUTHORITY)
            duplicate = {**original, "finding_id": "renamed-duplicate"}
            kernel.open_review("review-2", "task-a", [duplicate], reviewer_assignment_id="reviewer-2", fresh_epoch_id="review-2", authority_ref=AUTHORITY)
            kernel.open_review_epoch("validator-2", ref, reviewer_assignment_id="validator-2", authority_ref=AUTHORITY)
            kernel.validate_findings("review-2", [{"candidate_id": "original", "disposition": "required", "reason": "mandatory", "materiality": "material", "requirement_ref": "R-1", "permitted_fix_scope": ["task-a"]}], validator_assignment_id="validator-2", fresh_epoch_id="validator-2", authority_ref=AUTHORITY)
            recovered = ControlKernel(root, "fingerprint-duplicate").read_state()
            self.assertEqual(set(recovered["findings"]), {"original"})
            self.assertEqual(recovered["findings"]["original"]["object_ref"], original_ref)
            self.assertEqual(recovered["review_budget"]["rounds_used"], used)
            outcome = recovered["finding_validations"]["review-2"]["outcomes"][0]
            self.assertEqual(recovered["metadata"]["review_duplicates"]["review-2"][0], {"supplied_finding_id": "renamed-duplicate", "canonical_finding_id": "original", "duplicate_of": "original"})
            self.assertEqual((outcome["canonical_finding_id"], outcome["duplicate_of"]), ("original", "original"))
            distinct = {**original, "finding_id": "distinct", "description": "other gap"}
            kernel = ControlKernel(root, "fingerprint-duplicate")
            kernel.open_review_epoch("review-3", ref, reviewer_assignment_id="reviewer-3", authority_ref=AUTHORITY)
            kernel.open_review("review-3", "task-a", [distinct], reviewer_assignment_id="reviewer-3", fresh_epoch_id="review-3", authority_ref=AUTHORITY)
            self.assertEqual(set(kernel.read_state()["findings"]), {"original", "distinct"})

    def test_duplicate_validation_emits_canonical_provenance_and_matches_schema(self):
        with tempfile.TemporaryDirectory() as directory:
            kernel = ControlKernel(Path(directory), "duplicate-validation")
            kernel.entry(OBJECTIVE, authority_ref=AUTHORITY)
            kernel.publish_task_package("task-a", {"acceptance": ["ok"]}, assignment_id="worker-a", authority_ref=AUTHORITY)
            ref = kernel.read_state()["tasks"]["task-a"]["package_ref"]
            kernel.open_review_epoch("review-1", ref, reviewer_assignment_id="reviewer-1", authority_ref=AUTHORITY)
            kernel.open_review("review-1", "task-a", [{"finding_id": "original", "requirement_ref": "R-1", "description": "same gap", "severity": "major", "blocking": True}], reviewer_assignment_id="reviewer-1", fresh_epoch_id="review-1", authority_ref=AUTHORITY)
            kernel.open_review_epoch("validator-1", ref, reviewer_assignment_id="validator-1", authority_ref=AUTHORITY)
            kernel.validate_findings("review-1", [{"candidate_id": "original", "disposition": "duplicate", "reason": "same fingerprint", "materiality": "immaterial", "requirement_ref": "R-1", "permitted_fix_scope": []}], validator_assignment_id="validator-1", fresh_epoch_id="validator-1", authority_ref=AUTHORITY)
            state = ControlKernel(Path(directory), "duplicate-validation").read_state()
            outcome = state["finding_validations"]["review-1"]["outcomes"][0]
            self.assertEqual((outcome["canonical_finding_id"], outcome["duplicate_of"]), ("original", "original"))
            validate_document({key: state["finding_validations"]["review-1"][key] for key in ("review_id", "review_package_ref", "validator_assignment_id", "fresh_epoch_id", "outcomes")} | {"admissibility_verdict": "required-only"}, json.loads((ROOT / "schemas" / "finding-validation-v1.schema.json").read_text()), {})

    def test_reopen_requires_exact_new_bound_receipt_before_mutation(self):
        with tempfile.TemporaryDirectory() as directory:
            kernel = ControlKernel(Path(directory), "receipt-bound")
            kernel.entry(OBJECTIVE, authority_ref=AUTHORITY)
            kernel.terminal_review("rounds_exhausted", unresolved_finding_ids=[], authority_ref=AUTHORITY)
            head = kernel.head()
            before = kernel.read_state()
            budget = replacement_budget()
            bad_authorities = [
                AUTHORITY,
                reopen_receipt(kernel, "different-id", budget, head),
                {**reopen_receipt(kernel, "approval-1", budget, head), "run_id": "another-run"},
                reopen_receipt(kernel, "approval-1", {**budget, "version": "v3"}, head),
                reopen_receipt(kernel, "approval-1", budget, {**head, "revision": head["revision"] + 1}),
            ]
            for authority in bad_authorities:
                with self.assertRaises(AuthorizationError):
                    kernel.reopen_review("approval-1", budget, expected_head=head, authority_ref=authority)
                unchanged = kernel.read_state()
                self.assertEqual(unchanged["budget_terminal"], before["budget_terminal"])
                self.assertEqual(unchanged["terminal_history"], before["terminal_history"])
                self.assertEqual(kernel.head(), head)
            receipt = reopen_receipt(kernel, "approval-1", budget, head)
            kernel.reopen_review("approval-1", budget, expected_head=head, authority_ref=receipt)
            recovered = ControlKernel(Path(directory), "receipt-bound").read_state()
            self.assertEqual(recovered["status"], "active")
            self.assertEqual(recovered["terminal_history"], before["terminal_history"])
            self.assertEqual(recovered["metadata"]["reopen_history"][0]["receipt_digest"], kernel._command_digest(receipt["approval_receipt"]))

    def test_validator_needs_user_stops_and_round_exhaustion_is_automatic(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            kernel = ControlKernel(root, "needs-user")
            kernel.entry(OBJECTIVE, authority_ref=AUTHORITY, review_budget={"version": "v1", "deadline": "9999-12-31T23:59:59+00:00", "max_rounds": 1, "max_attempts_per_finding": 2, "rounds_used": 0, "finding_attempts": {}})
            kernel.publish_task_package("task-a", {"acceptance": ["ok"]}, assignment_id="worker-a", authority_ref=AUTHORITY)
            ref = kernel.read_state()["tasks"]["task-a"]["package_ref"]
            kernel.open_review_epoch("review-1", ref, reviewer_assignment_id="reviewer-1", authority_ref=AUTHORITY)
            candidates = [{"finding_id": "choice", "requirement_ref": "R-1", "description": "choice", "severity": "minor", "blocking": False}, {"finding_id": "immaterial", "requirement_ref": "R-2", "description": "minor", "severity": "minor", "blocking": False}, {"finding_id": "needs-user", "requirement_ref": "R-3", "description": "scope", "severity": "major", "blocking": True}]
            kernel.open_review("review-1", "task-a", candidates, reviewer_assignment_id="reviewer-1", fresh_epoch_id="review-1", authority_ref=AUTHORITY)
            kernel.open_review_epoch("validator-1", ref, reviewer_assignment_id="validator-1", authority_ref=AUTHORITY)
            outcomes = [{"candidate_id": "choice", "disposition": "reject", "reason": "approved decision", "materiality": "immaterial", "requirement_ref": "R-1", "permitted_fix_scope": []}, {"candidate_id": "immaterial", "disposition": "defer", "reason": "immaterial improvement", "materiality": "immaterial", "requirement_ref": "R-2", "permitted_fix_scope": []}, {"candidate_id": "needs-user", "disposition": "needs-user", "reason": "scope decision", "materiality": "material", "requirement_ref": "R-3", "permitted_fix_scope": []}]
            kernel.validate_findings("review-1", outcomes, validator_assignment_id="validator-1", fresh_epoch_id="validator-1", authority_ref=AUTHORITY)
            self.assertEqual(ControlKernel(root, "needs-user").read_state()["budget_terminal"]["reason"], "needs_user")


if __name__ == "__main__":
    unittest.main()
