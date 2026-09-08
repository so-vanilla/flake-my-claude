import json
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
    BudgetError,
    CommandValidationError,
    ControlKernel,
    DAGCycleError,
    InjectedCrash,
    IntegrityBlockedError,
    KernelError,
    StaleHeadError,
    route_lifecycle,
)
from ai_agent_workflow.migration import LegacyConverter, LegacyReader, MigrationError, NewReader, PointerCutover  # noqa: E402


class A6R_EvidenceTests(unittest.TestCase):
    def setUp(self):
        self.tempdir = tempfile.TemporaryDirectory()
        self.root = Path(self.tempdir.name)
        self.authority = {"status": "approved", "scopes": ["*"]}
        self.objective = {"path": "objectives/v001.md", "version": "v001", "digest": "a" * 64}
        self.kernel = ControlKernel(self.root, "evidence")
        self.kernel.entry(self.objective, authority_ref=self.authority)

    def tearDown(self):
        self.tempdir.cleanup()

    def test_g1_authority_scope_and_protected_field(self):
        with self.assertRaises(CommandValidationError):
            self.kernel.publish_artifact("empty-authority", "v1", {}, authority_ref={})
        with self.assertRaises(AuthorizationError):
            self.kernel.publish_artifact("unscoped-authority", "v1", {}, authority_ref={"status": "approved"})
        with self.assertRaises(AuthorizationError):
            self.kernel.publish_artifact("blocked", "v1", {}, authority_ref={"status": "candidate", "scopes": ["publish_artifact"]})
        command = self.kernel._command_for("publish_artifact", {"artifact_id": "protected", "version": "v1", "value": {}, "kind": "artifact", "path": None}, authority_ref=self.authority)
        command["protected_fields"] = ["objective"]
        command["authority_ref"] = {"status": "approved", "scopes": ["*"]}
        with self.assertRaises(AuthorizationError):
            self.kernel.apply(command)
        command["authority_ref"] = {"status": "approved", "scopes": ["*"], "protected_fields": ["objective"]}
        with self.assertRaises(AuthorizationError):
            self.kernel.apply(command)
        with self.assertRaises(AuthorizationError):
            self.kernel.publish_artifact(
                "expired",
                "v1",
                {},
                authority_ref={"status": "approved", "scopes": ["*"], "expires_at": "2000-01-01T00:00:00Z"},
            )
        with self.assertRaises(AuthorizationError):
            self.kernel.publish_artifact(
                "expired-status",
                "v1",
                {},
                authority_ref={"status": "expired", "approved": ["old"], "scopes": ["*"]},
            )
        with self.assertRaises(AuthorizationError):
            self.kernel.publish_artifact(
                "other-run",
                "v1",
                {},
                authority_ref={"status": "approved", "scopes": ["*"], "run_id": "other"},
            )

    def test_g2_single_writer_g3_fresh_resume_g5_exclusion_g6_routing(self):
        with self.assertRaises(CommandValidationError):
            self.kernel.publish_task_package("secret-task", {"secret": "not durable"}, assignment_id="worker-secret", authority_ref=self.authority)
        self.kernel.publish_task_package("task", {"acceptance": ["ok"]}, assignment_id="worker", authority_ref=self.authority)
        self.kernel.claim_task("task", assignment_id="worker", authority_ref=self.authority)
        with self.assertRaises(AuthorizationError):
            self.kernel.accept_task_result(
                "task",
                {"status": "success", "private_reasoning": "must stay outside durable state"},
                worker_assignment_id="worker",
                authority_ref=self.authority,
            )
        with self.assertRaises(AuthorizationError):
            self.kernel.submit_task_result(
                "task",
                {"status": "partial"},
                worker_assignment_id="worker",
                authority_ref={**self.authority, "private_reasoning": "must stay outside inbox"},
            )
        one_off = route_lifecycle("one-off")
        self.assertFalse(one_off["managed"])
        managed = route_lifecycle("managed", root=self.root, run_id="managed")
        self.assertTrue(managed["managed"])
        env = dict(__import__("os").environ)
        env["PYTHONPATH"] = str(SRC)
        code = "from ai_agent_workflow.control_kernel import ControlKernel; print(ControlKernel(%r, 'evidence').resume()['revision'])" % str(self.root)
        result = subprocess.run([sys.executable, "-c", code], env=env, capture_output=True, text=True, check=True)
        self.assertEqual(result.stdout.strip(), "3")

    def test_g7_budget_states_and_g8_faults_are_explicit(self):
        self.kernel.close_epoch(token_status="exact", token_count=200000, authority_ref=self.authority)
        self.assertEqual(self.kernel.read_state()["context_budget"]["token_status"], "exact")
        self.kernel.close_group(authority_ref=self.authority)
        self.assertEqual(self.kernel.read_state()["group"]["status"], "closed")
        # A new fixture covers estimated/unavailable without sharing a closed
        # Epoch.  Values are never inferred when telemetry is unavailable.
        other = ControlKernel(self.root / "budget", "budget")
        other.entry(self.objective, authority_ref=self.authority)
        other.close_epoch(token_status="estimated", token_count=12, authority_ref=self.authority)
        self.assertEqual(other.read_state()["context_budget"]["token_status"], "estimated")
        blocked = ControlKernel(self.root / "fault", "fault")
        blocked.entry(self.objective, authority_ref=self.authority)
        with self.assertRaises(InjectedCrash):
            with blocked.fault("after_publish_before_head"):
                blocked.publish_artifact("orphan", "v1", {}, authority_ref=self.authority)
        self.assertEqual(blocked.head()["revision"], 1)
        report = blocked.recover()
        self.assertTrue(report["head_unchanged"])
        self.assertTrue(report["orphan_transactions"])

    def test_g7_boundary_values_are_recorded_or_stopped(self):
        for count in (200000, 300000):
            bounded = ControlKernel(self.root / ("budget-%s" % count), "bounded-%s" % count)
            bounded.entry(self.objective, authority_ref=self.authority)
            bounded.close_epoch(token_status="exact", token_count=count, authority_ref=self.authority)
            self.assertEqual(bounded.read_state()["context_budget"]["token_count"], count)
        for count in (300001, 500000, 500001):
            stopped = ControlKernel(self.root / ("stop-%s" % count), "stop-%s" % count)
            stopped.entry(self.objective, authority_ref=self.authority)
            with self.assertRaises(BudgetError):
                stopped.close_epoch(token_status="exact", token_count=count, authority_ref=self.authority)
            self.assertEqual(stopped.head()["revision"], 1)

    def test_migration_old_new_reader_pointer_cutover_and_rollback(self):
        fixtures = ROOT / "tests" / "fixtures" / "a6r"
        source_dir = self.root / "migration-fixtures"
        source_dir.mkdir()
        legacy_run = source_dir / "legacy-run.json"
        legacy_bundle = source_dir / "legacy-bundle.json"
        legacy_report = source_dir / "legacy-worker-report.json"
        run_value = json.loads((fixtures / "legacy-run.json").read_text())
        run_value.update({"aliases": ["legacy-a6"]})
        run_value["current_epoch"].update({"closed_at_revision": 11})
        bundle_value = json.loads((fixtures / "legacy-bundle.json").read_text())
        bundle_value.update({"group_id": "bootstrap", "aliases": ["legacy-a6"], "state_revision": 11})
        bundle_value["context_epoch"].update({"group_id": "bootstrap", "status": "closed", "closed_at_revision": 11})
        report_value = json.loads((fixtures / "legacy-worker-report.json").read_text())
        report_value.update({"run_id": "legacy-a6", "group_id": "bootstrap", "status": "done", "aliases": ["legacy-a6"], "state_revision": 11})
        for path, value in ((legacy_run, run_value), (legacy_bundle, bundle_value), (legacy_report, report_value)):
            path.write_text(json.dumps(value, sort_keys=True))
        old_bytes = legacy_run.read_bytes()
        converter = LegacyConverter()
        expected_digests = {
            "run": LegacyReader().read_with_digest(legacy_run)["source_digest"],
            "bundle": LegacyReader().read_with_digest(legacy_bundle)["source_digest"],
            "worker_report": LegacyReader().read_with_digest(legacy_report)["source_digest"],
        }
        result = converter.convert(legacy_run, legacy_bundle, legacy_report, destination=self.root / "converted", run_id="converted", authority_ref=self.authority, expected_source_revision=11, expected_source_digests=expected_digests)
        self.assertEqual(legacy_run.read_bytes(), old_bytes)
        self.assertTrue(result["history_preserved"])
        self.assertEqual(result["source_revision"], 11)
        self.assertEqual(NewReader().read(result["kernel"])["revision"], 2)
        self.assertEqual(LegacyReader().read(legacy_run)["state_revision"], 11)
        pointer = PointerCutover(self.root / "active-pointer.json")
        old_state = result["kernel"].read_state()
        old = {
            **result["new_head"],
            "status": old_state["status"],
            "source_digests": dict(result["source_digests"]),
            "source_revision": result["source_revision"],
            "group_id": old_state["group"]["id"],
            "epoch_id": old_state["epoch"]["id"],
            "aliases": list(old_state["metadata"]["aliases"]),
        }
        authority = dict(self.authority, scopes=["migration_cutover", "migration_rollback"], migration_approval=True, approval_ref="approval-test", run_id="converted", role="orchestrator", assignment_id="orchestrator")
        authority["proposal_digest"] = pointer.proposal_digest(result["new_head"], result["source_digests"], old)
        cut = pointer.cutover(result["new_head"], source_digests=result["source_digests"], old_pointer=old, expected_old_pointer=old, authority_ref=authority)
        self.assertEqual(cut["active"], "new")
        rolled = pointer.rollback(authority_ref=authority, expected_active="new")
        self.assertEqual(rolled["active"], "old")
        self.assertEqual(rolled["rollback_preserved_candidate"], cut["new_pointer"])
        self.assertEqual(pointer.read()["source_digests"], result["source_digests"])
        with self.assertRaises(MigrationError):
            converter.convert(legacy_run, legacy_bundle, legacy_report, destination=fixtures / "unsafe-copy", run_id="unsafe", authority_ref=self.authority)

    def test_dag_cycle_and_input_digest_guards(self):
        with self.assertRaises(IntegrityBlockedError):
            self.kernel.publish_task_package("guard", {}, assignment_id="worker-guard", input_refs=[{"digest": "sha256:" + "d" * 64}], authority_ref=self.authority)
        self.kernel.publish_task_package("a", {}, authority_ref=self.authority)
        a_ref = self.kernel.read_state()["tasks"]["a"]["package_ref"]
        self.kernel.publish_task_package("b", {}, input_refs=[a_ref], authority_ref=self.authority)
        state = self.kernel.read_state()
        self.assertIn("task:a", state["nodes"])

    def test_scope_guard_rejects_escape_and_result_outside_assignment(self):
        with self.assertRaises(CommandValidationError):
            self.kernel.publish_task_package(
                "scoped",
                {"write_scope": ["../outside"]},
                assignment_id="worker-scoped",
                authority_ref=self.authority,
            )
        self.kernel.publish_task_package(
            "scoped",
            {"write_scope": ["src/component"]},
            assignment_id="worker-scoped",
            authority_ref={**self.authority, "write_scopes": ["src/component"]},
        )
        self.kernel.claim_task("scoped", assignment_id="worker-scoped", authority_ref=self.authority)
        with self.assertRaises(AuthorizationError):
            self.kernel.accept_task_result(
                "scoped",
                {"status": "success", "write_paths": ["tests/unassigned.py"]},
                worker_assignment_id="worker-scoped",
                authority_ref=self.authority,
            )

    def test_readiness_leases_and_explicit_cycle_guard(self):
        self.kernel.publish_task_package("leased", {}, assignment_id="worker-leased", authority_ref=self.authority)
        self.assertIn("leased", self.kernel.ready_tasks())
        self.kernel.claim_task("leased", assignment_id="worker-leased", authority_ref=self.authority)
        self.assertNotIn("leased", self.kernel.ready_tasks())
        self.kernel.release_task("leased", assignment_id="worker-leased", authority_ref=self.authority)
        self.assertIn("leased", self.kernel.ready_tasks())
        with self.assertRaises(DAGCycleError):
            self.kernel._validate_edges([
                {"from": "task:a", "to": "task:b", "type": "requires"},
                {"from": "task:b", "to": "task:a", "type": "requires"},
            ])

    def test_digest_input_ref_compiles_to_artifact_readiness_edge(self):
        self.kernel.publish_artifact("input", "v1", {"ready": True}, authority_ref=self.authority)
        artifact_ref = self.kernel.read_state()["artifacts"]["input"]["object_ref"]
        self.kernel.publish_task_package("dependent", {"acceptance": ["uses input"]}, input_refs=[artifact_ref], authority_ref=self.authority)
        self.assertIn("dependent", self.kernel.ready_tasks())
        state = self.kernel.read_state()
        self.assertIn({"from": "artifact:input", "to": "task:dependent", "type": "requires"}, state["edges"])


if __name__ == "__main__":
    unittest.main()
