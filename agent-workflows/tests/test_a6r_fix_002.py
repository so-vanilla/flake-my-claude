import json
import hashlib
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SRC = ROOT / "agent-workflows" / "src"
sys.path.insert(0, str(SRC))

from ai_agent_workflow.control_kernel import (  # noqa: E402
    AuthorizationError,
    CommandValidationError,
    ControlKernel,
    IntegrityBlockedError,
    LifecycleClosedError,
    ResultAcceptanceError,
    ReviewProvenanceError,
)
from ai_agent_workflow.migration import LegacyConverter, LegacyReader, MigrationError, NewReader, PointerCutover  # noqa: E402


AUTHORITY = {"status": "approved", "scopes": ["*"]}
OBJECTIVE = {"path": "objectives/v001.md", "version": "v001", "digest": "a" * 64}


def _raw_digest(path):
    return "sha256:" + hashlib.sha256(Path(path).read_bytes()).hexdigest()


class A6RFix002Tests(unittest.TestCase):
    def setUp(self):
        self.tempdir = tempfile.TemporaryDirectory()
        self.root = Path(self.tempdir.name)
        self.kernel = ControlKernel(self.root, "fix-002")
        self.kernel.entry(OBJECTIVE, authority_ref=AUTHORITY)

    def tearDown(self):
        self.tempdir.cleanup()

    def test_invalid_complete_envelope_has_no_durable_side_effect(self):
        before = self.kernel.head()
        before_transactions = sorted(self.kernel.transactions_dir.glob("*.json"))
        before_objects = sorted(self.kernel.objects_dir.glob("*.json"))
        command = self.kernel.make_command(
            "publish_artifact",
            {"artifact_id": "bad", "version": "v1", "value": {}},
            authority_ref=AUTHORITY,
            idempotency_key="bad-envelope",
        )
        del command["expected_head"]["transaction_digest"]
        with self.assertRaises(CommandValidationError):
            self.kernel.apply(command)
        self.assertEqual(self.kernel.head(), before)
        self.assertEqual(sorted(self.kernel.transactions_dir.glob("*.json")), before_transactions)
        self.assertEqual(sorted(self.kernel.objects_dir.glob("*.json")), before_objects)

    def test_genesis_authority_assignment_is_bound_before_layout_creation(self):
        with tempfile.TemporaryDirectory() as td:
            root = Path(td)
            kernel = ControlKernel(root, "bad-genesis")
            authority = {"status": "approved", "scopes": ["*"], "assignment_id": "worker-not-orchestrator"}
            with self.assertRaises(AuthorizationError):
                kernel.entry(OBJECTIVE, authority_ref=authority)
            self.assertFalse(root.exists() and any(root.iterdir()))

    def test_nested_unknown_and_secret_submission_are_rejected_before_inbox(self):
        command = self.kernel.make_command(
            "publish_artifact",
            {"artifact_id": "bad-nested", "version": "v1", "value": {}, "kind": "artifact", "path": None},
            authority_ref=AUTHORITY,
            idempotency_key="bad-nested",
        )
        command["expected_head"]["unknown"] = 1
        with self.assertRaises(CommandValidationError):
            self.kernel.submit_command(command)
        command = self.kernel.make_command(
            "publish_artifact",
            {"artifact_id": "bad-secret", "version": "v1", "value": {}, "kind": "artifact", "path": None},
            authority_ref=AUTHORITY,
            idempotency_key="bad-secret",
        )
        command["payload"]["value"] = {"metadata": ({"access_token": "secret"},)}
        with self.assertRaises(AuthorizationError):
            self.kernel.submit_command(command)
        self.assertFalse(list((self.kernel.inbox_dir / "commands").glob("*.json")))

    def test_nonempty_scope_requires_exact_authority_binding(self):
        before = self.kernel.head()
        with self.assertRaises(AuthorizationError):
            self.kernel.publish_task_package(
                "unscoped-output",
                {"write_scope": ["src/component"]},
                assignment_id="worker-unscoped-output",
                authority_ref=AUTHORITY,
            )
        self.assertEqual(self.kernel.head(), before)
        self.assertNotIn("unscoped-output", self.kernel.read_state()["tasks"])

    def test_input_reference_type_is_bound_to_immutable_object(self):
        self.kernel.publish_artifact("input-artifact", "v1", {}, authority_ref=AUTHORITY)
        artifact_ref = dict(self.kernel.read_state()["artifacts"]["input-artifact"]["object_ref"])
        artifact_ref["object_type"] = "task-package"
        command = self.kernel._command_for(
            "publish_task_package",
            {
                "task_id": "wrong-ref-type",
                "package": {
                    "task_id": "wrong-ref-type",
                    "attempt_id": "attempt-wrong-ref-type",
                    "assignment": {"role": "worker", "assignment_id": "worker-wrong-ref-type"},
                    "input_refs": [artifact_ref],
                    "write_scope": [],
                    "acceptance": [],
                    "freshness": {"epoch_id": "epoch-0001", "created_at_revision": 3},
                    "stop_conditions": [],
                    "invalidated": False,
                    "stop_requested": False,
                    "output_path": None,
                    "status": "ready",
                },
                "assignment": {"role": "worker", "assignment_id": "worker-wrong-ref-type"},
                "input_refs": [artifact_ref],
                "sibling_group": None,
            },
            authority_ref=AUTHORITY,
            input_refs=[artifact_ref],
        )
        before = self.kernel.head()
        with self.assertRaises(IntegrityBlockedError):
            self.kernel.apply(command)
        self.assertEqual(self.kernel.head(), before)

    def test_runtime_task_package_and_transaction_graph_delta_are_strict(self):
        command = self.kernel._command_for(
            "publish_task_package",
            {
                "task_id": "numeric-attempt",
                "package": {
                    "task_id": "numeric-attempt",
                    "attempt_id": 1,
                    "assignment": {"role": "worker", "assignment_id": "worker-numeric"},
                    "input_refs": [],
                    "write_scope": [],
                    "acceptance": [],
                    "freshness": {"epoch_id": "epoch-0001", "created_at_revision": 2},
                    "stop_conditions": [],
                    "invalidated": False,
                    "stop_requested": False,
                    "output_path": None,
                    "status": "ready",
                },
                "assignment": {"role": "worker", "assignment_id": "worker-numeric"},
                "input_refs": [],
                "sibling_group": None,
            },
            authority_ref=AUTHORITY,
        )
        with self.assertRaises(CommandValidationError):
            self.kernel.apply(command)
        self.kernel.publish_artifact("valid", "v1", {"ok": True}, authority_ref=AUTHORITY)
        head = self.kernel.head()
        tx = json.loads((self.kernel.transactions_dir / (head["transaction_digest"][7:] + ".json")).read_text())
        self.assertIn("graph_delta", tx)
        self.assertEqual(tx["graph_delta"]["revision"], 2)

    def test_direct_review_finding_must_be_complete_and_canonical(self):
        self.kernel.publish_task_package("review-input", {}, authority_ref=AUTHORITY)
        self.kernel.claim_task("review-input", assignment_id="worker-review-input", authority_ref=AUTHORITY)
        self.kernel.accept_task_result(
            "review-input", {"status": "success"}, worker_assignment_id="worker-review-input", authority_ref=AUTHORITY
        )
        task_ref = self.kernel.read_state()["tasks"]["review-input"]["package_ref"]
        self.kernel.open_review_epoch(
            "review-input-epoch", task_ref, reviewer_assignment_id="reviewer-input", authority_ref=AUTHORITY
        )
        command = self.kernel._command_for(
            "open_review",
            {
                "review_id": "incomplete-review",
                "candidate_task_id": "review-input",
                "findings": [{"finding_id": "missing-fingerprint", "requirement_ref": "R1", "description": "bad", "severity": "major", "blocking": True}],
                "reviewer_assignment_id": "reviewer-input",
                "fresh_epoch_id": "review-input-epoch",
                "review_kind": "initial",
                "target_finding_id": None,
            },
            authority_ref=AUTHORITY,
        )
        before = self.kernel.head()
        with self.assertRaises(CommandValidationError):
            self.kernel.apply(command)
        self.assertEqual(self.kernel.head(), before)

    def test_result_requires_live_lease_and_closed_lifecycle_rejects_mutation(self):
        self.kernel.publish_task_package("leased", {}, authority_ref=AUTHORITY)
        with self.assertRaises(ResultAcceptanceError):
            self.kernel.accept_task_result(
                "leased", {"status": "success"}, worker_assignment_id="worker-leased", authority_ref=AUTHORITY
            )
        self.kernel.claim_task("leased", assignment_id="worker-leased", authority_ref=AUTHORITY)
        self.kernel.accept_task_result(
            "leased", {"status": "success"}, worker_assignment_id="worker-leased", authority_ref=AUTHORITY
        )
        self.kernel.close_epoch(authority_ref=AUTHORITY)
        self.kernel.close_group(authority_ref=AUTHORITY)
        with self.assertRaises(LifecycleClosedError):
            self.kernel.publish_artifact("after-close", "v1", {}, authority_ref=AUTHORITY)
        self.assertEqual(self.kernel.ready_tasks(), [])
        with self.assertRaises(LifecycleClosedError):
            self.kernel.open_epoch("epoch-after-group", self.kernel.read_state()["epoch"]["bundle_ref"], authority_ref=AUTHORITY)

    def test_review_context_is_registered_and_object_bound(self):
        self.kernel.publish_task_package("candidate", {}, assignment_id="worker-candidate", authority_ref=AUTHORITY)
        self.kernel.claim_task("candidate", assignment_id="worker-candidate", authority_ref=AUTHORITY)
        self.kernel.accept_task_result(
            "candidate", {"status": "partial"}, worker_assignment_id="worker-candidate", authority_ref=AUTHORITY
        )
        task_ref = self.kernel.read_state()["tasks"]["candidate"]["package_ref"]
        with self.assertRaises(ReviewProvenanceError):
            self.kernel.open_review_epoch(
                "unregistered", task_ref, reviewer_assignment_id=None, authority_ref=AUTHORITY
            )
        self.kernel.open_review_epoch(
            "review-epoch", task_ref, reviewer_assignment_id="reviewer-1", authority_ref=AUTHORITY
        )
        self.kernel.open_review(
            "review", "candidate", [{"finding_id": "finding", "requirement_ref": "R1", "description": "bad", "severity": "major", "blocking": True}],
            reviewer_assignment_id="reviewer-1", fresh_epoch_id="review-epoch", authority_ref=AUTHORITY,
        )
        self.kernel.open_review_epoch("validator-epoch", task_ref, reviewer_assignment_id="validator-1", authority_ref=AUTHORITY)
        self.kernel.validate_findings("review", [{"candidate_id": "finding", "disposition": "required", "reason": "mandatory", "materiality": "material", "requirement_ref": "R1", "permitted_fix_scope": ["candidate"]}], validator_assignment_id="validator-1", fresh_epoch_id="validator-epoch", authority_ref=AUTHORITY)
        state = self.kernel.read_state()
        finding = state["findings"]["finding"]
        self.assertEqual(finding["state"], "open")
        self.kernel.accept_resolution_claim(
            "finding", ["fixed"], worker_assignment_id="worker-candidate", authority_ref=AUTHORITY
        )
        resolution_ref = self.kernel.read_state()["findings"]["finding"]["resolution_ref"]
        self.kernel.open_review_epoch(
            "fresh-review-epoch", resolution_ref, reviewer_assignment_id="reviewer-2", authority_ref=AUTHORITY
        )
        self.kernel.open_review(
            "fresh-review", "candidate", [], reviewer_assignment_id="reviewer-2", fresh_epoch_id="fresh-review-epoch",
            review_kind="closure", target_finding_id="finding", authority_ref=AUTHORITY,
        )
        self.kernel.accept_finding_closure(
            "finding", [resolution_ref], reviewer_assignment_id="reviewer-2", fresh_epoch_id="fresh-review-epoch",
            review_id="fresh-review", authority_ref=AUTHORITY,
        )
        finding = self.kernel.read_state()["findings"]["finding"]
        self.assertEqual(finding["state"], "resolved")
        self.assertNotIn("closed", finding)

    def test_pointer_reader_validates_active_target_and_follows_kernel(self):
        fixtures = ROOT / "agent-workflows" / "tests" / "fixtures" / "a6r"
        source_dir = self.root / "pointer-fixtures"
        source_dir.mkdir()
        copied = {}
        for name in ("legacy-run.json", "legacy-bundle.json", "legacy-worker-report.json"):
            target = source_dir / name
            value = json.loads((fixtures / name).read_text())
            if name == "legacy-run.json":
                value["aliases"] = ["fix-002"]
                value["current_epoch"]["closed_at_revision"] = 11
            elif name == "legacy-bundle.json":
                value.update({"group_id": "bootstrap", "aliases": ["fix-002"], "state_revision": 11})
                value["context_epoch"].update({"group_id": "bootstrap", "status": "closed", "closed_at_revision": 11})
            else:
                value.update({"run_id": "legacy-a6", "group_id": "bootstrap", "status": "done", "aliases": ["fix-002"], "state_revision": 11})
            target.write_text(json.dumps(value, sort_keys=True))
            copied[name] = target
        source_digests = {name: LegacyReader().read_with_digest(path)["source_digest"] for name, path in (("run", copied["legacy-run.json"]), ("bundle", copied["legacy-bundle.json"]), ("worker_report", copied["legacy-worker-report.json"]))}
        converted = LegacyConverter().convert(
            copied["legacy-run.json"], copied["legacy-bundle.json"], copied["legacy-worker-report.json"],
            destination=self.root / "converted-pointer", run_id="fix-002-converted", authority_ref=AUTHORITY,
            expected_source_revision=11, expected_source_digests=source_digests,
        )
        pointer = PointerCutover(self.root / "active-pointer.json")
        old = {"revision": 11, "digest": "sha256:" + "c" * 64}
        new = converted["new_head"]
        source = converted["source_digests"]
        authority = {
            "status": "approved",
            "scopes": ["migration_cutover", "migration_rollback"],
            "migration_approval": True,
            "approval_ref": "approval-migration-1",
            "proposal_digest": pointer.proposal_digest(new, source, old),
            "run_id": converted["run_id"],
            "role": "orchestrator",
            "assignment_id": "orchestrator",
        }
        pointer.cutover(new, source_digests=source, old_pointer=old, expected_old_pointer=old, authority_ref=authority)
        self.assertEqual(NewReader().read(pointer=pointer)["revision"], converted["kernel"].head()["revision"])
        bad = pointer.read()
        bad["new_pointer"]["role"] = "worker"
        bad["pointer_digest"] = PointerCutover._pointer_digest(bad)
        pointer.path.write_text(json.dumps(bad))
        with self.assertRaises(MigrationError):
            NewReader().read(self.kernel, pointer=pointer)


if __name__ == "__main__":
    unittest.main()
