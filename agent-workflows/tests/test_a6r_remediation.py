import hashlib
import json
import os
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SRC = ROOT / "agent-workflows" / "src"
RUNROOT = ROOT / ".local" / "agent" / "runs" / "2026-09-01-ai-agent-workflow-rebuild"
sys.path.insert(0, str(SRC))

from ai_agent_workflow.control_kernel import (  # noqa: E402
    AuthorizationError,
    CommandValidationError,
    ControlKernel,
    DuplicateCommandError,
    InjectedCrash,
    IntegrityBlockedError,
    KernelError,
    ResultAcceptanceError,
    ReviewProvenanceError,
    StaleHeadError,
    canonical_digest,
)
from ai_agent_workflow.migration import (  # noqa: E402
    LegacyConverter,
    LegacyReader,
    MigrationError,
    NewReader,
    PointerCutover,
)
from ai_agent_workflow.schema_validation import SchemaValidationError, validate_document  # noqa: E402


AUTHORITY = {"status": "approved", "scopes": ["*"]}
OBJECTIVE = {"path": "objectives/v001.md", "version": "v001", "digest": "a" * 64}


def _raw_digest(path):
    return "sha256:" + hashlib.sha256(Path(path).read_bytes()).hexdigest()


class A6RRemediationTests(unittest.TestCase):
    def setUp(self):
        self.tempdir = tempfile.TemporaryDirectory()
        self.root = Path(self.tempdir.name)
        self.kernel = ControlKernel(self.root, "remediation")
        self.kernel.entry(OBJECTIVE, authority_ref=AUTHORITY)

    def tearDown(self):
        self.tempdir.cleanup()

    def test_invalid_command_is_rejected_before_any_head_or_projection_change(self):
        before = self.kernel.head()
        command = self.kernel.make_command(
            "publish_artifact",
            {"artifact_id": "invalid", "version": "v1", "value": {}},
            authority_ref=AUTHORITY,
            idempotency_key="invalid-command",
        )
        del command["command_id"]
        with self.assertRaises(CommandValidationError):
            self.kernel.apply(command)
        self.assertEqual(self.kernel.head(), before)
        self.assertEqual(sorted(self.kernel.transactions_dir.glob("*.json")), [
            self.kernel._transaction_path(before["transaction_digest"])
        ])

    def test_mutating_wrapper_requires_explicit_authority(self):
        before = self.kernel.head()
        with self.assertRaises(AuthorizationError):
            self.kernel.publish_artifact("no-authority", "v1", {"ok": True})
        self.assertEqual(self.kernel.head(), before)

    def test_projection_manifest_detects_partial_publish_and_rebuilds_all_views(self):
        self.kernel.publish_artifact("before", "v1", {"ok": True}, authority_ref=AUTHORITY)
        command = self.kernel.make_command(
            "publish_artifact",
            {"artifact_id": "after", "version": "v1", "value": {"ok": True}, "kind": "artifact", "path": None},
            authority_ref=AUTHORITY,
            idempotency_key="partial-projection",
        )
        self.kernel.set_fault("during_projection")
        with self.assertRaises(InjectedCrash):
            self.kernel.apply(command)
        self.kernel.set_fault(None)
        self.assertEqual(self.kernel.head()["revision"], 3)
        state = self.kernel.read_state()
        self.assertEqual(state["revision"], 3)
        manifest = json.loads((self.kernel.projection_dir / "projection-manifest.json").read_text())
        self.assertEqual(manifest["source_head"]["transaction_digest"], self.kernel.head()["transaction_digest"])
        for name, digest in manifest["files"].items():
            self.assertEqual(_raw_digest(self.kernel.projection_dir / name), digest)
        for name in ("run.json", "status.json", "ready.json", "plan.yaml"):
            self.assertEqual(
                json.loads((self.kernel.projection_dir / name).read_text())
                .get("source_head", {})
                .get("transaction_digest"),
                self.kernel.head()["transaction_digest"],
            )
        events = [json.loads(line) for line in (self.kernel.projection_dir / "events.jsonl").read_text().splitlines()]
        self.assertEqual(events[-1]["transaction_digest"], self.kernel.head()["transaction_digest"])

    def test_fresh_process_corruption_checks_block_without_head_advance(self):
        env = dict(os.environ)
        env["PYTHONPATH"] = str(SRC)
        for kind in ("object", "parent", "head"):
            root = self.root / ("fresh-" + kind)
            kernel = ControlKernel(root, "corrupt")
            kernel.entry(OBJECTIVE, authority_ref=AUTHORITY)
            kernel.publish_artifact("stable", "v1", {"ok": True}, authority_ref=AUTHORITY)
            before = kernel.head()
            if kind == "object":
                object_path = next(path for path in kernel.objects_dir.glob("*.json") if path.name != before["transaction_digest"][7:] + ".json")
                object_path.write_text("{}")
            elif kind == "parent":
                current = json.loads((kernel.transactions_dir / (before["transaction_digest"][7:] + ".json")).read_text())
                parent = current["parent"]["transaction_digest"]
                (kernel.transactions_dir / (parent[7:] + ".json")).unlink()
            else:
                head_value = json.loads(kernel.head_path.read_text())
                head_value["revision"] = 999
                kernel.head_path.write_text(json.dumps(head_value))
            code = (
                "from ai_agent_workflow.control_kernel import ControlKernel, IntegrityBlockedError\n"
                "try:\n ControlKernel(%r, 'corrupt').read_state()\n"
                "except IntegrityBlockedError:\n print('blocked')\n"
                "else: raise SystemExit('corruption was not blocked')"
            ) % str(root)
            result = subprocess.run([sys.executable, "-c", code], env=env, capture_output=True, text=True)
            self.assertEqual(result.returncode, 0, "%s: %s" % (kind, result.stderr))
            self.assertEqual(result.stdout.strip(), "blocked", kind)
            if kind == "head":
                self.assertEqual(json.loads(kernel.head_path.read_text())["revision"], 999)
            else:
                self.assertEqual(kernel.head()["transaction_digest"], before["transaction_digest"])

    def test_artifact_bundle_matches_frozen_schema_shape_and_digest_contract(self):
        self.kernel.publish_artifact("spec", "v1", {"ok": True}, authority_ref=AUTHORITY)
        state = self.kernel.close_epoch(
            acceptance_evidence=["bundle shape"], authority_ref=AUTHORITY, token_status="unavailable"
        )
        bundle_ref = state["epoch"]["bundle_ref"]
        bundle = self.kernel.read_object(bundle_ref)["payload"]
        required = {
            "schema", "bundle_id", "version", "run_id", "workflow_version", "group_id",
            "context_epoch", "canonical_artifacts", "acceptance_evidence", "approved_decisions",
            "unresolved_items", "invalidated_artifacts", "next_inputs", "context_budget", "digest",
        }
        self.assertTrue(required.issubset(bundle))
        self.assertRegex(bundle["digest"], r"^[0-9a-f]{64}$")
        self.assertEqual(bundle["digest"], canonical_digest({k: v for k, v in bundle.items() if k != "digest"})[7:])
        self.assertEqual(bundle["context_epoch"]["group_id"], "bootstrap")
        self.assertTrue(bundle["context_epoch"]["boundary_reason"])
        for artifact in bundle["canonical_artifacts"]:
            self.assertTrue(artifact["path"])
            self.assertRegex(artifact["digest"], r"^[0-9a-f]{64}$")

    def test_durable_exclusion_covers_json_serializable_tuples(self):
        with self.assertRaises(CommandValidationError):
            self.kernel.publish_task_package(
                "tuple-secret",
                {"metadata": ({"private_reasoning": "never durable"},)},
                authority_ref=AUTHORITY,
            )

    def test_finding_closure_requires_real_fresh_review_package_and_single_state_truth(self):
        self.kernel.publish_task_package("candidate", {}, assignment_id="worker-candidate", authority_ref=AUTHORITY)
        self.kernel.claim_task("candidate", assignment_id="worker-candidate", authority_ref=AUTHORITY)
        self.kernel.accept_task_result(
            "candidate", {"status": "partial"}, worker_assignment_id="worker-candidate", authority_ref=AUTHORITY
        )
        candidate_ref = self.kernel.read_state()["tasks"]["candidate"]["package_ref"]
        self.kernel.open_review_epoch("epoch-review", candidate_ref, reviewer_assignment_id="reviewer-initial", authority_ref=AUTHORITY)
        with self.assertRaises(AuthorizationError):
            self.kernel.open_review(
                "review-unregistered-epoch",
                "candidate",
                [],
                reviewer_assignment_id="reviewer-initial",
                fresh_epoch_id="arbitrary-epoch",
                authority_ref=AUTHORITY,
            )
        self.kernel.open_review(
            "review-initial",
            "candidate",
            [{"finding_id": "finding-remediate", "requirement_ref": "R1", "description": "missing", "severity": "major", "blocking": True}],
            reviewer_assignment_id="reviewer-initial",
            fresh_epoch_id="epoch-review",
            authority_ref=AUTHORITY,
        )
        self.kernel.open_review_epoch("epoch-validator", candidate_ref, reviewer_assignment_id="validator-initial", authority_ref=AUTHORITY)
        self.kernel.validate_findings("review-initial", [{"candidate_id": "finding-remediate", "disposition": "required", "reason": "mandatory", "materiality": "material", "requirement_ref": "R1", "permitted_fix_scope": ["candidate"]}], validator_assignment_id="validator-initial", fresh_epoch_id="epoch-validator", authority_ref=AUTHORITY)
        self.kernel.accept_resolution_claim(
            "finding-remediate", ["fixed"], worker_assignment_id="worker-candidate", authority_ref=AUTHORITY
        )
        resolution_ref = self.kernel.read_state()["findings"]["finding-remediate"]["resolution_ref"]
        self.kernel.open_review_epoch("epoch-rereview", resolution_ref, reviewer_assignment_id="reviewer-fresh", authority_ref=AUTHORITY)
        with self.assertRaises(AuthorizationError):
            self.kernel.accept_finding_closure(
                "finding-remediate",
                ["arbitrary"],
                reviewer_assignment_id="reviewer-fresh",
                fresh_epoch_id="epoch-rereview",
                review_id="not-a-review-package",
                authority_ref=AUTHORITY,
            )
        self.kernel.open_review(
            "review-rereview",
            "candidate",
            [],
            reviewer_assignment_id="reviewer-fresh",
            fresh_epoch_id="epoch-rereview",
            review_kind="closure",
            target_finding_id="finding-remediate",
            authority_ref=AUTHORITY,
        )
        with self.assertRaises(AuthorizationError):
            self.kernel.submit_finding_closure(
                "finding-remediate",
                [{"private_reasoning": "not durable"}],
                reviewer_assignment_id="reviewer-fresh",
                fresh_epoch_id="epoch-rereview",
                review_id="review-rereview",
            )
        self.kernel.submit_finding_closure(
            "finding-remediate",
            [resolution_ref],
            reviewer_assignment_id="reviewer-fresh",
            fresh_epoch_id="epoch-rereview",
            review_id="review-rereview",
        )
        self.kernel.accept_finding_closure(
            "finding-remediate",
            [resolution_ref],
            reviewer_assignment_id="reviewer-fresh",
            fresh_epoch_id="epoch-rereview",
            review_id="review-rereview",
            authority_ref=AUTHORITY,
        )
        finding = self.kernel.read_state()["findings"]["finding-remediate"]
        self.assertNotIn("closed", finding)
        self.assertEqual(finding["state"], "resolved")
        self.assertEqual(self.kernel.read_state()["verdicts"]["review-initial"]["verdict"], "pass")

    def test_orphan_transaction_is_not_a_duplicate_and_can_be_retried(self):
        command = self.kernel.make_command(
            "publish_artifact",
            {"artifact_id": "orphan-retry", "version": "v1", "value": {"ok": True}, "kind": "artifact", "path": None},
            authority_ref=AUTHORITY,
            idempotency_key="orphan-retry",
        )
        self.kernel.set_fault("after_publish_before_head")
        with self.assertRaises(InjectedCrash):
            self.kernel.apply(command)
        self.kernel.set_fault(None)
        self.assertEqual(self.kernel.head()["revision"], 1)
        recovery = self.kernel.recover()
        self.assertTrue(recovery["head_unchanged"])
        self.assertTrue(recovery["orphan_transactions"])
        self.kernel.apply(command)
        self.assertEqual(self.kernel.head()["revision"], 2)
        self.assertIn("orphan-retry", self.kernel.read_state()["artifacts"])

    def test_readiness_has_freshness_stop_lease_and_result_overwrite_guards(self):
        with self.assertRaises(ReviewProvenanceError):
            self.kernel.publish_task_package(
                "stale",
                {"freshness": {"epoch_id": "old-epoch", "created_at_revision": 1}},
                assignment_id="worker-stale",
                authority_ref=AUTHORITY,
            )
        self.kernel.publish_task_package(
            "stopped",
            {"stop_requested": True},
            assignment_id="worker-stopped",
            authority_ref=AUTHORITY,
        )
        self.kernel.publish_task_package("ready", {}, assignment_id="worker-ready", authority_ref=AUTHORITY)
        self.assertEqual(self.kernel.ready_tasks(), ["ready"])
        self.kernel.claim_task("ready", assignment_id="worker-ready", authority_ref=AUTHORITY)
        self.assertNotIn("ready", self.kernel.ready_tasks())
        self.kernel.accept_task_result(
            "ready", {"status": "success"}, worker_assignment_id="worker-ready", authority_ref=AUTHORITY
        )
        with self.assertRaises(ResultAcceptanceError):
            self.kernel.accept_task_result(
                "ready", {"status": "success", "different": True}, worker_assignment_id="worker-ready", authority_ref=AUTHORITY
            )

    def test_real_a6_conversion_binds_source_revision_and_digests(self):
        source_dir = self.root / "copied-a6-source"
        source_dir.mkdir()
        # Use a copied actual-shape fixture so the full suite never depends on
        # mutable Run/report state or whichever remediation revision happens
        # to be present in the working tree.
        fixtures = ROOT / "agent-workflows" / "tests" / "fixtures" / "a6r"
        run_value = json.loads((fixtures / "legacy-run.json").read_text())
        run_value["aliases"] = ["ai-agent-workflow-rebuild"]
        run_value["current_epoch"].update({"status": "closed", "clear_before_next": True, "closed_at_revision": 11})
        bundle_value = json.loads((fixtures / "legacy-bundle.json").read_text())
        bundle_value.update({"group_id": "bootstrap", "aliases": ["ai-agent-workflow-rebuild"], "state_revision": 11})
        bundle_value["context_epoch"].update({"group_id": "bootstrap", "status": "closed", "clear_before_next": True, "closed_at_revision": 11})
        report_value = json.loads((fixtures / "legacy-worker-report.json").read_text())
        report_value.update({"run_id": "legacy-a6", "group_id": "bootstrap", "status": "done", "aliases": ["ai-agent-workflow-rebuild"], "state_revision": 11})
        source_paths = {"run": run_value, "bundle": bundle_value, "worker_report": report_value}
        copied = {}
        for name, value in source_paths.items():
            target = source_dir / (name + ".json")
            target.write_text(json.dumps(value, sort_keys=True))
            copied[name] = target
        run = copied["run"]
        bundle = copied["bundle"]
        report = copied["worker_report"]
        source_bytes = {path: path.read_bytes() for path in (run, bundle, report)}
        source_digests = {"run": _raw_digest(run), "bundle": _raw_digest(bundle), "worker_report": _raw_digest(report)}
        result = LegacyConverter().convert(
            run,
            bundle,
            report,
            destination=self.root / "real-a6-copy",
            run_id="real-a6-copy",
            authority_ref=AUTHORITY,
            expected_source_revision=11,
            expected_source_digests=source_digests,
        )
        self.assertEqual(result["source_revision"], 11)
        self.assertEqual(result["source_digests"], source_digests)
        self.assertEqual({path: path.read_bytes() for path in (run, bundle, report)}, source_bytes)
        state = result["kernel"].read_state()
        self.assertEqual(state["objective_ref"]["path"], "objectives/v001.md")
        self.assertEqual(NewReader().read(result["kernel"])["revision"], 2)
        validate_document(
            state["migration"],
            json.loads((ROOT / "agent-workflows" / "schemas" / "legacy-migration-v1.schema.json").read_text()),
        )
        with self.assertRaises(MigrationError):
            LegacyConverter().convert(
                run,
                bundle,
                report,
                destination=self.root / "missing-authority",
                run_id="missing-authority",
                expected_source_revision=11,
                expected_source_digests=source_digests,
            )
        bad = dict(source_digests, run="sha256:" + "0" * 64)
        with self.assertRaises(MigrationError):
            LegacyConverter().convert(
                run,
                bundle,
                report,
                destination=self.root / "bad-digest",
                run_id="bad-digest",
                authority_ref=AUTHORITY,
                expected_source_revision=11,
                expected_source_digests=bad,
            )
        with self.assertRaises(MigrationError):
            LegacyConverter().convert(
                run,
                bundle,
                report,
                destination=source_dir / "unsafe-copy",
                run_id="unsafe-copy",
                authority_ref=AUTHORITY,
                expected_source_revision=11,
                expected_source_digests=source_digests,
            )

    def test_cutover_requires_migration_approval_proposal_and_expected_pointer_cas(self):
        pointer = PointerCutover(self.root / "pointer.json")
        old = {"revision": 11, "digest": "sha256:" + "c" * 64}
        new = {
            "schema": "dag-head/v1",
            "run_id": "remediation",
            "workflow_version": "manual-bootstrap/v1",
            "graph_version": "artifact-task-dag/v1",
            "role": "orchestrator",
            "revision": 2,
            "state_revision": 2,
            "transaction_digest": "sha256:" + "d" * 64,
            "kernel_path": str(self.root / "new-kernel"),
        }
        new["digest"] = canonical_digest({key: value for key, value in new.items() if key != "kernel_path"})
        source = {"run": "sha256:" + "e" * 64, "bundle": "sha256:" + "f" * 64, "worker_report": "sha256:" + "0" * 64}
        proposal = pointer.proposal_digest(new, source, old)
        authority = {
            "status": "approved",
            "scopes": ["migration_cutover", "migration_rollback"],
            "migration_approval": True,
            "approval_ref": "approval-migration-1",
            "proposal_digest": proposal,
            "run_id": "remediation",
            "role": "orchestrator",
            "assignment_id": "orchestrator",
        }
        with self.assertRaises(MigrationError):
            pointer.cutover(new, source_digests=source, old_pointer=old)
        # A syntactically approved proposal is still rejected before pointer
        # publication when the physical target has no kernel/HEAD/projection
        # and therefore cannot prove its source binding.
        with self.assertRaises(MigrationError):
            pointer.cutover(
                new,
                source_digests=source,
                old_pointer=old,
                expected_old_pointer=old,
                authority_ref=authority,
            )
        self.assertFalse(pointer.path.exists())

    def test_gate_fixture_has_command_exit_status_and_evidence_paths(self):
        fixture = json.loads((ROOT / "agent-workflows" / "tests" / "fixtures" / "a6r" / "gates-v3.json").read_text())
        self.assertEqual([gate["id"] for gate in fixture["gates"]], ["G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8"])
        for gate in fixture["gates"]:
            self.assertTrue(gate["command"])
            self.assertEqual(gate["exit_status"], 0)
            self.assertTrue(gate["evidence_paths"])
            for path in gate["evidence_paths"]:
                self.assertTrue((ROOT / "agent-workflows" / path).exists(), path)
        self.assertIn("v2_limitation", fixture)

    def test_emitted_objects_and_envelopes_conform_to_local_schemas(self):
        schema_dir = ROOT / "agent-workflows" / "schemas"
        registry = {
            path.name: json.loads(path.read_text())
            for path in schema_dir.glob("*.schema.json")
        }
        command = self.kernel.make_command(
            "publish_artifact",
            {"artifact_id": "schema-artifact", "version": "v1", "value": {"ok": True}, "kind": "artifact", "path": None},
            authority_ref=AUTHORITY,
        )
        validate_document(command, registry["dag-command-v1.schema.json"], registry)
        self.kernel.apply(command)
        self.kernel.publish_task_package("schema-task", {}, authority_ref=AUTHORITY)
        self.kernel.claim_task("schema-task", assignment_id="worker-schema-task", authority_ref=AUTHORITY)
        self.kernel.accept_task_result(
            "schema-task", {"status": "partial"}, worker_assignment_id="worker-schema-task", authority_ref=AUTHORITY
        )
        schema_task_ref = self.kernel.read_state()["tasks"]["schema-task"]["package_ref"]
        self.kernel.open_review_epoch("epoch-schema-review", schema_task_ref, reviewer_assignment_id="reviewer-schema", authority_ref=AUTHORITY)
        self.kernel.open_review(
            "schema-review",
            "schema-task",
            [],
            reviewer_assignment_id="reviewer-schema",
            fresh_epoch_id="epoch-schema-review",
            authority_ref=AUTHORITY,
        )
        state = self.kernel.read_state()
        head = self.kernel.head()
        validate_document(state, registry["dag-state-v1.schema.json"], registry)
        transaction = json.loads((self.kernel.transactions_dir / (head["transaction_digest"][7:] + ".json")).read_text())
        validate_document(head, registry["dag-head-v1.schema.json"], registry)
        validate_document(transaction, registry["dag-transaction-v1.schema.json"], registry)
        for ref in state["object_refs"].values():
            object_value = self.kernel.read_object(ref)
            validate_document(object_value, registry["dag-object-v1.schema.json"], registry)
            if object_value["object_type"] == "task-package":
                validate_document(object_value["payload"], registry["task-package-v1.schema.json"], registry)
            if object_value["object_type"] == "review-package":
                validate_document(object_value["payload"], registry["review-package-v1.schema.json"], registry)
            if object_value["object_type"] == "finding":
                validate_document(object_value["payload"], registry["finding-v1.schema.json"], registry)
        self.kernel.close_epoch(authority_ref=AUTHORITY)
        state = self.kernel.read_state()
        bundle = self.kernel.read_object(state["epoch"]["bundle_ref"])["payload"]
        validate_document(bundle, registry["artifact-bundle-v1.schema.json"], registry)
        for finding in state["findings"].values():
            validate_document(finding, registry["finding-v1.schema.json"], registry)
        manifest = json.loads((self.kernel.projection_dir / "projection-manifest.json").read_text())
        validate_document(manifest, registry["projection-manifest-v1.schema.json"], registry)


if __name__ == "__main__":
    unittest.main()
