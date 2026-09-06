import copy
import hashlib
import json
import os
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
    DAGCycleError,
    IntegrityBlockedError,
    LifecycleClosedError,
    StaleHeadError,
)
from ai_agent_workflow.migration import (  # noqa: E402
    LegacyConverter,
    MigrationError,
    PointerCutover,
)
from ai_agent_workflow.schema_validation import SchemaValidationError, validate_document  # noqa: E402


AUTHORITY = {"status": "approved", "scopes": ["*"]}
OBJECTIVE = {"path": "objectives/v001.md", "version": "v001", "digest": "a" * 64}


def _raw_digest(path):
    return "sha256:" + hashlib.sha256(Path(path).read_bytes()).hexdigest()


class A6RFix005Tests(unittest.TestCase):
    def setUp(self):
        self.tempdir = tempfile.TemporaryDirectory()
        self.root = Path(self.tempdir.name)
        self.kernel = ControlKernel(self.root / "kernel", "fix-005")
        self.kernel.entry(OBJECTIVE, authority_ref=AUTHORITY, aliases=["fix-005"])

    def tearDown(self):
        self.tempdir.cleanup()

    def _write_sources(self, name, marker=None):
        source_dir = self.root / ("sources-" + name)
        source_dir.mkdir()
        run = {
            "schema": "ai-agent-run/v0",
            "run_id": "legacy-a6",
            "workflow_version": "manual-bootstrap/v1",
            "status": "paused_after_epoch",
            "state_revision": 11,
            "aliases": ["legacy-a6"],
            "objective_ref": copy.deepcopy(OBJECTIVE),
            "current_group": {"id": "bootstrap", "status": "open"},
            "current_epoch": {
                "id": "epoch-0002",
                "status": "closed",
                "clear_before_next": True,
                "closed_at_revision": 11,
            },
        }
        bundle = {
            "schema": "artifact-bundle/v1",
            "bundle_id": "epoch-0002-a6",
            "version": "v1",
            "run_id": "legacy-a6",
            "workflow_version": "manual-bootstrap/v1",
            "group_id": "bootstrap",
            "aliases": ["legacy-a6"],
            "state_revision": 11,
            "context_epoch": {
                "id": "epoch-0002",
                "group_id": "bootstrap",
                "status": "closed",
                "clear_before_next": True,
                "closed_at_revision": 11,
            },
            "canonical_artifacts": [],
        }
        report = {
            "schema": "agent-worker-report/v1",
            "work_id": "legacy-a6",
            "run_id": "legacy-a6",
            "group_id": "bootstrap",
            "context_epoch": "epoch-0002",
            "status": "done",
            "state_revision": 11,
        }
        if marker is not None:
            # These fields are durable provenance but do not change the
            # identity tuple.  They make the two source snapshots distinct.
            run["snapshot_marker"] = marker
            bundle["snapshot_marker"] = marker
            report["snapshot_marker"] = marker
        paths = {
            "run": source_dir / "run.json",
            "bundle": source_dir / "bundle.json",
            "worker_report": source_dir / "worker-report.json",
        }
        for key, value in (("run", run), ("bundle", bundle), ("worker_report", report)):
            paths[key].write_text(json.dumps(value, sort_keys=True))
        digests = {key: _raw_digest(path) for key, path in paths.items()}
        return paths, digests

    def _convert(self, name, marker=None):
        paths, digests = self._write_sources(name, marker=marker)
        return LegacyConverter().convert(
            paths["run"],
            paths["bundle"],
            paths["worker_report"],
            destination=self.root / (name + "-kernel"),
            run_id=name,
            authority_ref=AUTHORITY,
            expected_source_revision=11,
            expected_source_digests=digests,
        )

    @staticmethod
    def _full_pointer(result):
        state = result["kernel"].read_state()
        return {
            **copy.deepcopy(result["new_head"]),
            "status": state["status"],
            "source_digests": copy.deepcopy(result["source_digests"]),
            "source_revision": result["source_revision"],
            "group_id": state["group"]["id"],
            "epoch_id": state["epoch"]["id"],
            "aliases": list(state["metadata"]["aliases"]),
        }

    @staticmethod
    def _cutover_authority(pointer, new_result, old_pointer):
        proposal = pointer.proposal_digest(
            new_result["new_head"], new_result["source_digests"], old_pointer
        )
        return {
            "status": "approved",
            "scopes": ["migration_cutover", "migration_rollback"],
            "migration_approval": True,
            "approval_ref": "approval-fix-005",
            "proposal_digest": proposal,
            "run_id": new_result["run_id"],
            "role": "orchestrator",
            "assignment_id": "orchestrator",
        }

    def test_complete_command_is_validated_before_any_persistence(self):
        before = self.kernel.head()
        command = self.kernel.make_command(
            "publish_artifact",
            {
                "artifact_id": "strict",
                "version": "v1",
                "value": {"ok": True},
                "kind": "artifact",
                "path": None,
            },
            authority_ref=AUTHORITY,
            idempotency_key="strict-command",
        )
        del command["expected_head"]["transaction_digest"]
        with self.assertRaisesRegex(CommandValidationError, "expected_head"):
            self.kernel.apply(command)
        self.assertEqual(self.kernel.head(), before)
        self.assertEqual(len(list(self.kernel.transactions_dir.glob("*.json"))), 1)
        self.assertEqual(list(self.kernel.inbox_dir.rglob("*.json")), [])

        command = self.kernel.make_command(
            "publish_artifact",
            {
                "artifact_id": "strict-nested",
                "version": "v1",
                "value": {"ok": True},
                "kind": "artifact",
                "path": None,
            },
            authority_ref=AUTHORITY,
            idempotency_key="strict-nested-command",
        )
        command["payload"]["unrecognized"] = True
        with self.assertRaisesRegex(CommandValidationError, "unsupported fields: unrecognized"):
            self.kernel.apply(command)
        self.assertEqual(self.kernel.head(), before)

    def test_all_documented_edge_endpoints_are_exact_and_task_to_task_is_blocked(self):
        nodes = {
            "artifact:a": "artifact",
            "work-product:w": "work-product",
            "task:t": "task",
            "review:r": "review",
            "finding:f": "finding",
            "authority:auth": "authority",
            "approval:approval": "approval",
        }
        edges = [
            {"from": "artifact:a", "to": "task:t", "type": "requires"},
            {"from": "work-product:w", "to": "task:t", "type": "requires"},
            {"from": "task:t", "to": "artifact:a", "type": "produces"},
            {"from": "task:t", "to": "work-product:w", "type": "produces"},
            {"from": "review:r", "to": "finding:f", "type": "produces"},
            {"from": "authority:auth", "to": "task:t", "type": "authorizes"},
            {"from": "approval:approval", "to": "task:t", "type": "authorizes"},
            {"from": "review:r", "to": "finding:f", "type": "verdict-for"},
            {"from": "artifact:a", "to": "task:t", "type": "converges"},
            {"from": "work-product:w", "to": "task:t", "type": "converges"},
        ]
        for edge in edges:
            # The documented endpoint table is checked one edge at a time;
            # combining every legal direction would intentionally create a
            # cycle in this otherwise exhaustive fixture.
            self.kernel._validate_edges([edge], nodes)
        with self.assertRaisesRegex(DAGCycleError, "Task-to-Task"):
            self.kernel._validate_edges(
                [{"from": "task:t", "to": "task:other", "type": "requires"}],
                {**nodes, "task:other": "task"},
            )
        edge_schema = json.loads(
            (ROOT / "agent-workflows" / "schemas" / "dag-edge-v1.schema.json").read_text()
        )
        with self.assertRaises(SchemaValidationError):
            validate_document(
                {"from": "task:t", "to": "task:other", "type": "requires"},
                edge_schema,
            )
        with self.assertRaisesRegex(IntegrityBlockedError, "DAG edge violates documented produces semantics"):
            self.kernel._validate_edges(
                [{"from": "artifact:a", "to": "task:t", "type": "produces"}], nodes
            )

    def test_live_authority_expiry_and_expected_head_block_readiness_and_claim(self):
        self.kernel.publish_task_package(
            "authority-bound", {}, assignment_id="worker-authority", authority_ref=AUTHORITY
        )
        self.assertIn("authority-bound", self.kernel.ready_tasks())
        expired_state = self.kernel.read_state()
        expired_state["authority"] = {
            **AUTHORITY,
            "expires_at": "2000-01-01T00:00:00Z",
        }
        self.assertNotIn("authority-bound", self.kernel._ready_tasks(expired_state))
        before = self.kernel.head()
        with self.assertRaisesRegex(AuthorizationError, "expired"):
            self.kernel.claim_task(
                "authority-bound",
                assignment_id="worker-authority",
                authority_ref=expired_state["authority"],
            )
        self.assertEqual(self.kernel.head(), before)

        expected_head = self.kernel.head()
        bound_authority = {
            **AUTHORITY,
            "approval_ref": "approval-head-binding",
            "expected_head": {
                "revision": expected_head["revision"],
                "transaction_digest": expected_head["transaction_digest"],
                "digest": expected_head["transaction_digest"],
            },
        }
        self.kernel.publish_task_package(
            "stale-authority",
            {},
            assignment_id="worker-stale-authority",
            authority_ref=bound_authority,
        )
        self.kernel.publish_artifact("advance", "v1", {"ok": True}, authority_ref=AUTHORITY)
        before = self.kernel.head()
        with self.assertRaisesRegex(AuthorizationError, "expected HEAD is stale"):
            self.kernel.claim_task(
                "stale-authority",
                assignment_id="worker-stale-authority",
                authority_ref=bound_authority,
            )
        self.assertEqual(self.kernel.head(), before)

    def test_needs_decision_and_replan_are_explicit_lifecycle_transitions(self):
        self.kernel.publish_task_package(
            "decision", {}, assignment_id="worker-decision", authority_ref=AUTHORITY
        )
        self.kernel.claim_task("decision", assignment_id="worker-decision", authority_ref=AUTHORITY)
        self.kernel.accept_task_result(
            "decision",
            {"status": "needs_decision"},
            worker_assignment_id="worker-decision",
            authority_ref=AUTHORITY,
        )
        state = self.kernel.read_state()
        self.assertEqual(state["tasks"]["decision"]["status"], "needs_decision")
        self.assertEqual(state["tasks"]["decision"]["result_state"], "result_submitted")
        before = self.kernel.head()
        with self.assertRaisesRegex(LifecycleClosedError, "needs_decision"):
            self.kernel.close_epoch(authority_ref=AUTHORITY)
        self.assertEqual(self.kernel.head(), before)

        self.kernel.replan_task(
            "decision", "needs human decision", authority_ref=AUTHORITY
        )
        state = self.kernel.read_state()
        task = state["tasks"]["decision"]
        self.assertTrue(task["stop_requested"])
        self.assertTrue(task["freshness"]["invalidated"])
        self.assertTrue(task["replan_requested"])
        self.assertEqual(task["replan_reason"], "needs human decision")

    def test_projection_forgery_and_extra_files_are_rebuilt_from_canonical_head(self):
        self.kernel.publish_artifact("projection-source", "v1", {"ok": True}, authority_ref=AUTHORITY)
        projection = self.kernel.projection_dir
        (projection / "status.json").write_text("{\"forged\": true}\n")
        (projection / "untrusted.json").write_text("{}\n")
        manifest_path = projection / "projection-manifest.json"
        manifest = json.loads(manifest_path.read_text())
        manifest["files"]["status.json"] = "sha256:" + "0" * 64
        manifest_path.write_text(json.dumps(manifest, sort_keys=True))

        state = self.kernel.read_state()
        self.assertEqual(state["revision"], self.kernel.head()["revision"])
        self.assertFalse((projection / "untrusted.json").exists())
        status = json.loads((projection / "status.json").read_text())
        self.assertEqual(status["source_head"]["transaction_digest"], self.kernel.head()["transaction_digest"])
        self.assertNotIn("forged", status)

    def test_public_migration_requires_physical_attestation_and_no_mapping_only_write(self):
        result = self._convert("attestation")
        tx_path = result["kernel"]._transaction_path(
            result["migration_transaction"]["transaction_digest"]
        )
        command = json.loads(tx_path.read_text())["command"]
        direct = ControlKernel(self.root / "direct", "direct")
        direct.entry(OBJECTIVE, authority_ref=AUTHORITY, aliases=["direct"])
        command["run_id"] = "direct"
        command["expected_head"] = {
            "revision": direct.head()["revision"],
            "transaction_digest": direct.head()["transaction_digest"],
            "digest": direct.head()["transaction_digest"],
        }
        del command["payload"]["migration"]["source_attestation"]
        before = direct.head()
        with self.assertRaisesRegex(CommandValidationError, r"migrate_legacy\.migration is incomplete: missing source_attestation"):
            direct.apply(command)
        self.assertEqual(direct.head(), before)
        self.assertEqual(len(list(direct.transactions_dir.glob("*.json"))), 1)

        command = json.loads(tx_path.read_text())["command"]
        command["run_id"] = "direct"
        command["expected_head"] = {
            "revision": direct.head()["revision"],
            "transaction_digest": direct.head()["transaction_digest"],
            "digest": direct.head()["transaction_digest"],
        }
        unrelated = self.root / "unrelated.json"
        unrelated.write_text("{}\n")
        command["payload"]["migration"]["source_attestation"]["run"]["path"] = str(unrelated)
        command["payload"]["migration"]["source_attestation"]["run"]["raw_digest"] = _raw_digest(unrelated)
        with self.assertRaisesRegex(CommandValidationError, "value does not match physical source"):
            direct.apply(command)
        self.assertEqual(direct.head(), before)

    def test_complete_old_target_is_physically_validated_before_initial_pointer_mutation(self):
        old_result = self._convert("old-corrupt", marker="old")
        new_result = self._convert("new-candidate", marker="new")
        old_pointer = self._full_pointer(old_result)
        # The old pointer is complete, so initial cutover must open and verify
        # the named target rather than trusting its self-reported HEAD tuple.
        (old_result["kernel"].projection_dir / "status.json").unlink()
        pointer = PointerCutover(self.root / "active-pointer.json")
        authority = self._cutover_authority(pointer, new_result, old_pointer)
        with self.assertRaisesRegex(MigrationError, "projection"):
            pointer.cutover(
                new_result["new_head"],
                source_digests=new_result["source_digests"],
                old_pointer=old_pointer,
                expected_old_pointer=old_pointer,
                authority_ref=authority,
            )
        self.assertFalse(pointer.path.exists())

    def test_rollback_uses_the_old_target_source_binding_when_revisions_differ(self):
        old_result = self._convert("old-source", marker="old")
        new_result = self._convert("new-source", marker="new")
        self.assertNotEqual(old_result["source_digests"], new_result["source_digests"])
        old_pointer = self._full_pointer(old_result)
        pointer = PointerCutover(self.root / "different-sources-pointer.json")
        authority = self._cutover_authority(pointer, new_result, old_pointer)
        cutover = pointer.cutover(
            new_result["new_head"],
            source_digests=new_result["source_digests"],
            old_pointer=old_pointer,
            expected_old_pointer=old_pointer,
            authority_ref=authority,
        )
        self.assertEqual(cutover["active"], "new")
        rolled_back = pointer.rollback(authority_ref=authority, expected_active="new")
        self.assertEqual(rolled_back["active"], "old")
        self.assertEqual(rolled_back["rollback_preserved_candidate"], cutover["new_pointer"])

    def test_future_epoch_context_is_rejected_without_state_mutation(self):
        forged = self.kernel.read_state()
        forged["epoch_contexts"]["future-epoch"] = {
            "id": "future-epoch",
            "status": "open",
            "group_id": "bootstrap",
            "boundary_reason": "forged",
            "started_at_revision": forged["revision"] + 1,
            "input_ref": None,
        }
        with self.assertRaisesRegex(IntegrityBlockedError, "start revision"):
            self.kernel._validate_state(forged, load_objects=False)
        self.assertNotIn("future-epoch", self.kernel.read_state()["epoch_contexts"])


if __name__ == "__main__":
    unittest.main()
