import copy
import hashlib
import json
import os
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
    BudgetError,
    CommandValidationError,
    ControlKernel,
    DAGCycleError,
    IntegrityBlockedError,
    ObjectValidationError,
    ResultAcceptanceError,
    StaleHeadError,
)
from ai_agent_workflow.migration import LegacyConverter, MigrationError, PointerCutover  # noqa: E402
from ai_agent_workflow.schema_validation import SchemaValidationError, validate_document  # noqa: E402


AUTHORITY = {"status": "approved", "scopes": ["*"]}
OBJECTIVE = {"path": "objectives/v001.md", "version": "v001", "digest": "a" * 64}


def _raw_digest(path):
    return "sha256:" + hashlib.sha256(Path(path).read_bytes()).hexdigest()


def _write_migration_sources(root):
    source_dir = Path(root) / "copied-a6"
    source_dir.mkdir(parents=True)
    run = {
        "schema": "ai-agent-run/v0",
        "run_id": "legacy-fix-006",
        "workflow_version": "manual-bootstrap/v1",
        "status": "paused_after_epoch",
        "state_revision": 11,
        "aliases": ["legacy-fix-006"],
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
        "bundle_id": "epoch-0002-fix-006",
        "version": "v1",
        "run_id": "legacy-fix-006",
        "workflow_version": "manual-bootstrap/v1",
        "group_id": "bootstrap",
        "aliases": ["legacy-fix-006"],
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
        "work_id": "legacy-fix-006",
        "run_id": "legacy-fix-006",
        "group_id": "bootstrap",
        "context_epoch": "epoch-0002",
        "status": "done",
        "state_revision": 11,
    }
    paths = {
        "run": source_dir / "run.json",
        "bundle": source_dir / "bundle.json",
        "worker_report": source_dir / "worker-report.json",
    }
    for key, value in (("run", run), ("bundle", bundle), ("worker_report", report)):
        paths[key].write_text(json.dumps(value, sort_keys=True))
    return paths, {key: _raw_digest(path) for key, path in paths.items()}


class A6RFix006Tests(unittest.TestCase):
    def setUp(self):
        self.tempdir = tempfile.TemporaryDirectory()
        self.root = Path(self.tempdir.name)
        self.kernel = ControlKernel(self.root / "kernel", "fix-006")
        self.kernel.entry(OBJECTIVE, authority_ref=AUTHORITY, aliases=["fix-006"])

    def tearDown(self):
        self.tempdir.cleanup()

    def test_compound_secret_keys_are_filtered_before_object_or_inbox_write(self):
        before_head = self.kernel.head()
        before_objects = sorted(self.kernel.objects_dir.glob("*.json"))
        before_transactions = sorted(self.kernel.transactions_dir.glob("*.json"))
        values = (
            {"foo_token_count": 1},
            {"credential_suffix": "x"},
            {"api_key_value": "x"},
            {"nested": ({"private_reasoning": "x"},)},
            {"tasks": {"foo_token_count": "opaque"}},
        )
        for index, value in enumerate(values):
            with self.subTest(value=value):
                with self.assertRaisesRegex(AuthorizationError, "non-durable"):
                    self.kernel.publish_artifact(
                        "secret-%s" % index, "v1", value, authority_ref=AUTHORITY
                    )
        command = self.kernel.make_command(
            "publish_artifact",
            {
                "artifact_id": "inbox-secret",
                "version": "v1",
                "value": {"foo_tokencount": 1},
                "kind": "artifact",
                "path": None,
            },
            authority_ref=AUTHORITY,
        )
        with self.assertRaisesRegex(AuthorizationError, "non-durable"):
            self.kernel.submit_command(command)
        self.assertEqual(self.kernel.head(), before_head)
        self.assertEqual(sorted(self.kernel.objects_dir.glob("*.json")), before_objects)
        self.assertEqual(sorted(self.kernel.transactions_dir.glob("*.json")), before_transactions)
        self.assertEqual(list(self.kernel.inbox_dir.rglob("*.json")), [])

        budget = {
            "target": 200000,
            "normal_limit": 300000,
            "absolute_limit": 500000,
            "token_status": "exact",
            "token_count": 42,
        }
        self.kernel.publish_artifact(
            "schema-owned-budget", "v1", {"context_budget": budget}, authority_ref=AUTHORITY
        )
        stored = self.kernel.read_state()["artifacts"]["schema-owned-budget"]["object_ref"]
        self.assertEqual(self.kernel.read_object(stored)["payload"]["payload"]["context_budget"], budget)
        with self.assertRaisesRegex(BudgetError, "schema-owned"):
            self.kernel.publish_artifact(
                "bad-budget", "v1", {"context_budget": {**budget, "foo_token_count": 1}}, authority_ref=AUTHORITY
            )

    def test_exact_edge_table_is_shared_by_runtime_graph_delta_and_schema(self):
        nodes = {
            "artifact:a": "artifact",
            "work-product:w": "work-product",
            "task:t": "task",
            "review:r": "review",
            "finding:f": "finding",
            "authority:a": "authority",
            "approval:p": "approval",
        }
        valid = (
            ("artifact:a", "task:t", "requires"),
            ("work-product:w", "task:t", "requires"),
            ("task:t", "artifact:a", "produces"),
            ("task:t", "work-product:w", "produces"),
            ("review:r", "finding:f", "produces"),
            ("authority:a", "task:t", "authorizes"),
            ("approval:p", "task:t", "authorizes"),
            ("review:r", "finding:f", "verdict-for"),
            ("artifact:a", "task:t", "converges"),
            ("work-product:w", "task:t", "converges"),
        )
        for source, target, edge_type in valid:
            self.kernel._validate_edges(
                [{"from": source, "to": target, "type": edge_type}], nodes, exact=True
            )
        invalid = (
            ("review:r", "task:t", "requires", "DAG edge violates documented requires semantics"),
            ("authority:a", "artifact:a", "authorizes", "DAG edge violates documented authorizes semantics"),
            ("review:r", "task:t", "verdict-for", "DAG edge violates documented verdict-for semantics"),
            ("review:r", "artifact:a", "converges", "DAG edge violates documented converges semantics"),
        )
        for source, target, edge_type, message in invalid:
            with self.subTest(edge=(source, target, edge_type)):
                with self.assertRaisesRegex(IntegrityBlockedError, message):
                    self.kernel._validate_edges(
                        [{"from": source, "to": target, "type": edge_type}], nodes, exact=True
                    )
        with self.assertRaisesRegex(DAGCycleError, "Task-to-Task execution edges"):
            self.kernel._validate_edges(
                [{"from": "task:t", "to": "task:u", "type": "requires"}],
                {**nodes, "task:u": "task"},
                exact=True,
            )
        with self.assertRaisesRegex(IntegrityBlockedError, "forbidden Task-to-Task edge"):
            self.kernel._validate_graph_delta(
                {
                    "revision": 2,
                    "added_nodes": [],
                    "removed_nodes": [],
                    "added_edges": [{"from": "task:t", "to": "task:u", "type": "requires"}],
                    "removed_edges": [],
                    "changed_artifacts": [],
                    "changed_tasks": [],
                    "changed_reviews": [],
                    "changed_findings": [],
                }
            )
        schema = json.loads((ROOT / "agent-workflows/schemas/dag-edge-v1.schema.json").read_text())
        with self.assertRaises(SchemaValidationError):
            validate_document({"from": "task:t", "to": "task:u", "type": "requires"}, schema)
        validate_document({"from": "work-product:w", "to": "task:t", "type": "requires"}, schema)

    def test_expected_authority_head_blocks_ready_and_claim_without_advancing_head(self):
        expected = self.kernel.head()
        authority = {
            **AUTHORITY,
            "approval_ref": "head-approval",
            "expected_head": {
                "revision": expected["revision"],
                "transaction_digest": expected["transaction_digest"],
                "digest": expected["transaction_digest"],
            },
        }
        self.kernel.publish_task_package(
            "head-bound", {}, assignment_id="worker-head-bound", authority_ref=authority
        )
        # The authority's expected HEAD is the publication parent.  Once the
        # package transaction itself is the current HEAD, a caller must not
        # treat that older expectation as a live capability.
        self.kernel.publish_artifact("advance-head", "v1", {"ok": True}, authority_ref=AUTHORITY)
        self.assertNotIn("head-bound", self.kernel.ready_tasks())
        before = self.kernel.head()
        with self.assertRaisesRegex(AuthorizationError, "authority expected HEAD is stale"):
            self.kernel.claim_task(
                "head-bound", assignment_id="worker-head-bound", authority_ref=authority
            )
        self.assertEqual(self.kernel.head(), before)

    def test_invalidation_reaches_downstream_and_quarantines_live_late_results(self):
        self.kernel.publish_task_package("producer", {}, assignment_id="worker-producer", authority_ref=AUTHORITY)
        self.kernel.claim_task("producer", assignment_id="worker-producer", authority_ref=AUTHORITY)
        self.kernel.accept_task_result(
            "producer", {"status": "success"}, worker_assignment_id="worker-producer", authority_ref=AUTHORITY
        )
        producer_ref = self.kernel.read_state()["tasks"]["producer"]["result_ref"]
        self.kernel.publish_task_package(
            "consumer", {}, assignment_id="worker-consumer", input_refs=[producer_ref], authority_ref=AUTHORITY
        )
        self.assertIn("consumer", self.kernel.ready_tasks())
        self.kernel.invalidate_task("producer", "producer output is stale", authority_ref=AUTHORITY)
        state = self.kernel.read_state()
        self.assertTrue(state["tasks"]["producer"]["invalidated"])
        self.assertTrue(state["tasks"]["consumer"]["invalidated"])
        self.assertIn("consumer", state["tasks"]["producer"]["dependency_closure"])
        self.assertNotIn("consumer", self.kernel.ready_tasks())
        before = self.kernel.head()
        with self.assertRaisesRegex(ResultAcceptanceError, "late task result is quarantined"):
            self.kernel.accept_task_result(
                "producer", {"status": "success"}, worker_assignment_id="worker-producer", authority_ref=AUTHORITY
            )
        self.assertEqual(self.kernel.head(), before)

        live = ControlKernel(self.root / "live", "live")
        live.entry(OBJECTIVE, authority_ref=AUTHORITY)
        live.publish_task_package("running", {}, assignment_id="worker-running", authority_ref=AUTHORITY)
        live.claim_task("running", assignment_id="worker-running", authority_ref=AUTHORITY)
        live.invalidate_task("running", "cancel running work", authority_ref=AUTHORITY)
        state = live.read_state()
        self.assertEqual(state["tasks"]["running"]["status"], "running")
        self.assertTrue(state["leases"]["running"]["quarantine"])
        with self.assertRaisesRegex(ResultAcceptanceError, "late task result is quarantined"):
            live.accept_task_result(
                "running", {"status": "success"}, worker_assignment_id="worker-running", authority_ref=AUTHORITY
            )
        live.release_task("running", assignment_id="worker-running", authority_ref=AUTHORITY)
        self.assertEqual(live.read_state()["tasks"]["running"]["status"], "invalidated")

    def test_replan_compiles_a_fresh_attempt_and_never_uses_a_future_revision(self):
        self.kernel.publish_task_package("retry", {}, assignment_id="worker-retry", authority_ref=AUTHORITY)
        before = self.kernel.head()
        self.kernel.replan_task("retry", "retry after explicit stop", authority_ref=AUTHORITY)
        state = self.kernel.read_state()
        replacement_id = state["tasks"]["retry"]["replacement_task_id"]
        self.assertIsInstance(replacement_id, str)
        self.assertNotEqual(replacement_id, "retry")
        replacement = state["tasks"][replacement_id]
        self.assertEqual(replacement["status"], "ready")
        self.assertEqual(replacement["freshness"]["created_at_revision"], state["revision"])
        self.assertLessEqual(replacement["freshness"]["created_at_revision"], state["revision"])
        self.assertGreater(state["revision"], before["revision"])

    def test_closed_boundary_objects_and_schema_are_reverse_bound(self):
        self.kernel.close_epoch(
            acceptance_evidence=["fix-006 boundary"],
            authority_ref=AUTHORITY,
            token_status="unavailable",
        )
        state = self.kernel.read_state()
        bundle_ref = state["epoch"]["bundle_ref"]
        checkpoint_ref = state["epoch"]["checkpoint_ref"]
        bundle = self.kernel.read_object(bundle_ref)["payload"]
        checkpoint = self.kernel.read_object(checkpoint_ref)["payload"]
        validate_document(bundle, json.loads((ROOT / "agent-workflows/schemas/artifact-bundle-v1.schema.json").read_text()))
        validate_document(checkpoint, json.loads((ROOT / "agent-workflows/schemas/checkpoint-v1.schema.json").read_text()))
        forged = copy.deepcopy(state)
        forged["epoch"]["bundle_ref"] = forged["entry_object_ref"]
        with self.assertRaisesRegex(IntegrityBlockedError, "closed Epoch bundle reference has the wrong object type"):
            self.kernel._validate_state(forged, load_objects=True)
        forged_bundle = copy.deepcopy(bundle)
        forged_bundle["run_id"] = "another-run"
        with self.assertRaisesRegex(ObjectValidationError, "state_ref is not bound to its closure"):
            # The schema cannot establish the cross-object identity, so use
            # the runtime object validator for the reverse-binding assertion.
            self.kernel._validate_object_payload("artifact-bundle", forged_bundle)

    def test_migration_command_cannot_cross_bind_raw_source_digest(self):
        paths, digests = _write_migration_sources(self.root)
        result = LegacyConverter().convert(
            paths["run"], paths["bundle"], paths["worker_report"],
            destination=self.root / "converted", run_id="converted",
            authority_ref=AUTHORITY, expected_source_revision=11,
            expected_source_digests=digests,
        )
        tx_path = result["kernel"]._transaction_path(result["migration_transaction"]["transaction_digest"])
        command = json.loads(tx_path.read_text())["command"]
        direct = ControlKernel(self.root / "direct", "direct")
        direct.entry(OBJECTIVE, authority_ref=AUTHORITY)
        command["run_id"] = "direct"
        head = direct.head()
        command["expected_head"] = {
            "revision": head["revision"],
            "transaction_digest": head["transaction_digest"],
            "digest": head["transaction_digest"],
        }
        migration = command["payload"]["migration"]
        fake = "sha256:" + "f" * 64
        migration["source_digests"]["run"] = fake
        migration["source_snapshot"]["source_digests"]["run"] = fake
        before = direct.head()
        with self.assertRaisesRegex(CommandValidationError, "raw digest does not match migration.source_digests.run"):
            direct.apply(command)
        self.assertEqual(direct.head(), before)
        self.assertEqual(len(list(direct.transactions_dir.glob("*.json"))), 1)

    def test_cutover_schema_and_runtime_reject_unsafe_pointer_paths_and_nested_shape(self):
        schema = json.loads((ROOT / "agent-workflows/schemas/cutover-pointer-v1.schema.json").read_text())
        digest = "sha256:" + "a" * 64
        new = {
            "schema": "dag-head/v1", "run_id": "new", "workflow_version": "manual-bootstrap/v1",
            "graph_version": "artifact-task-dag/v1", "role": "orchestrator", "revision": 1,
            "state_revision": 1, "transaction_digest": digest, "digest": digest, "kernel_path": "/tmp/kernel",
        }
        document = {
            "schema": "cutover-pointer/v1", "active": "new", "old_pointer": None,
            "new_pointer": new, "source_digests": {"run": digest, "bundle": digest, "worker_report": digest},
            "history_preserved": True, "proposal_digest": digest, "approval_ref": "approval",
            "authority": {"status": "approved", "scopes": ["migration_cutover"], "migration_approval": True, "approval_ref": "approval", "proposal_digest": digest, "run_id": "new", "role": "orchestrator", "assignment_id": "orchestrator"},
            "reader_pointers": {"old": None, "new": copy.deepcopy(new)}, "pointer_digest": digest,
        }
        validate_document(document, schema)
        malformed = copy.deepcopy(document)
        malformed["reader_pointers"]["new"].pop("role")
        with self.assertRaises(SchemaValidationError):
            validate_document(malformed, schema)
        malformed = copy.deepcopy(document)
        malformed["new_pointer"]["kernel_path"] = "/tmp/../kernel"
        with self.assertRaises(SchemaValidationError):
            validate_document(malformed, schema)
        with self.assertRaisesRegex(MigrationError, "traversal or an empty path segment"):
            from ai_agent_workflow.migration import NewReader
            NewReader._validate_head_pointer({**new, "kernel_path": "/tmp/../kernel"})
        malformed = copy.deepcopy(document)
        malformed["reader_pointers"]["new"]["unexpected"] = True
        with self.assertRaises(SchemaValidationError):
            validate_document(malformed, schema)

    def test_runtime_path_contract_matches_schema_for_task_and_refs(self):
        before = self.kernel.head()
        with self.assertRaisesRegex(CommandValidationError, "empty segment"):
            self.kernel.publish_task_package(
                "trailing", {"output_path": "src/"}, assignment_id="worker-trailing", authority_ref=AUTHORITY
            )
        self.assertEqual(self.kernel.head(), before)
        task_schema = json.loads((ROOT / "agent-workflows/schemas/task-package-v1.schema.json").read_text())
        package = {
            "task_id": "trailing", "attempt_id": "attempt-trailing",
            "assignment": {"role": "worker", "assignment_id": "worker-trailing"},
            "input_refs": [], "write_scope": [], "acceptance": [],
            "freshness": {"epoch_id": "epoch-0001", "created_at_revision": 2},
            "stop_conditions": [], "invalidated": False, "stop_requested": False,
            "output_path": "src/", "status": "ready",
        }
        with self.assertRaises(SchemaValidationError):
            validate_document(package, task_schema)


if __name__ == "__main__":
    unittest.main()
