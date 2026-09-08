import hashlib
import json
import shutil
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
    IntegrityBlockedError,
    LifecycleClosedError,
    ReviewProvenanceError,
)
from ai_agent_workflow.migration import LegacyConverter, LegacyReader, MigrationError, NewReader, PointerCutover  # noqa: E402


AUTHORITY = {"status": "approved", "scopes": ["*"]}
OBJECTIVE = {"path": "objectives/v001.md", "version": "v001", "digest": "a" * 64}


def _raw_digest(path):
    return "sha256:" + hashlib.sha256(Path(path).read_bytes()).hexdigest()


class A6RFix003Tests(unittest.TestCase):
    def setUp(self):
        self.tempdir = tempfile.TemporaryDirectory()
        self.root = Path(self.tempdir.name)
        self.kernel = ControlKernel(self.root / "kernel", "fix-003")
        self.kernel.entry(OBJECTIVE, authority_ref=AUTHORITY, aliases=["fix-003"])

    def tearDown(self):
        self.tempdir.cleanup()

    def _prepare_finding(self):
        self.kernel.publish_task_package("candidate", {}, assignment_id="worker-candidate", authority_ref=AUTHORITY)
        self.kernel.claim_task("candidate", assignment_id="worker-candidate", authority_ref=AUTHORITY)
        self.kernel.accept_task_result(
            "candidate", {"status": "partial"}, worker_assignment_id="worker-candidate", authority_ref=AUTHORITY
        )
        task_ref = self.kernel.read_state()["tasks"]["candidate"]["package_ref"]
        self.kernel.open_review_epoch("epoch-initial", task_ref, reviewer_assignment_id="reviewer-initial", authority_ref=AUTHORITY)
        self.kernel.open_review(
            "review-initial",
            "candidate",
            [{"finding_id": "finding-a", "requirement_ref": "R1", "description": "missing", "severity": "major", "blocking": True}],
            reviewer_assignment_id="reviewer-initial",
            fresh_epoch_id="epoch-initial",
            authority_ref=AUTHORITY,
        )
        self.kernel.open_review_epoch("epoch-validator", task_ref, reviewer_assignment_id="validator-initial", authority_ref=AUTHORITY)
        self.kernel.validate_findings("review-initial", [{"candidate_id": "finding-a", "disposition": "required", "reason": "mandatory", "materiality": "material", "requirement_ref": "R1", "permitted_fix_scope": ["candidate"]}], validator_assignment_id="validator-initial", fresh_epoch_id="epoch-validator", authority_ref=AUTHORITY)
        self.kernel.accept_resolution_claim(
            "finding-a", ["fixed"], worker_assignment_id="worker-candidate", authority_ref=AUTHORITY
        )
        resolution_ref = self.kernel.read_state()["findings"]["finding-a"]["resolution_ref"]
        self.kernel.open_review_epoch("epoch-closure", resolution_ref, reviewer_assignment_id="reviewer-fresh", authority_ref=AUTHORITY)
        self.kernel.open_review(
            "review-closure",
            "candidate",
            [],
            reviewer_assignment_id="reviewer-fresh",
            fresh_epoch_id="epoch-closure",
            review_kind="closure",
            target_finding_id="finding-a",
            authority_ref=AUTHORITY,
        )
        return resolution_ref

    def test_task_output_path_is_bound_even_when_task_scope_is_authorized(self):
        authority = {"status": "approved", "scopes": ["*"], "write_scopes": ["allowed"]}
        before = self.kernel.head()
        with self.assertRaises(AuthorizationError):
            self.kernel.publish_task_package(
                "out-of-scope-output",
                {"write_scope": ["allowed"], "output_path": "outside/result.json"},
                assignment_id="worker-output",
                authority_ref=authority,
            )
        self.assertEqual(self.kernel.head(), before)
        self.assertNotIn("out-of-scope-output", self.kernel.read_state()["tasks"])

    def test_epoch_close_rejects_live_lease_and_running_task(self):
        self.kernel.publish_task_package("running", {}, assignment_id="worker-running", authority_ref=AUTHORITY)
        self.kernel.claim_task("running", assignment_id="worker-running", authority_ref=AUTHORITY)
        before = self.kernel.head()
        with self.assertRaises(LifecycleClosedError):
            self.kernel.close_epoch(authority_ref=AUTHORITY)
        self.assertEqual(self.kernel.head(), before)
        self.assertIn("running", self.kernel.read_state()["leases"])

    def test_closure_requires_exact_bound_resolution_reference_and_is_single_assignment(self):
        resolution_ref = self._prepare_finding()
        forged = dict(resolution_ref)
        forged["digest"] = "sha256:" + "0" * 64
        with self.assertRaises(ReviewProvenanceError):
            self.kernel.accept_finding_closure(
                "finding-a",
                [forged],
                reviewer_assignment_id="reviewer-fresh",
                fresh_epoch_id="epoch-closure",
                review_id="review-closure",
                authority_ref=AUTHORITY,
            )
        self.kernel.accept_finding_closure(
            "finding-a",
            [resolution_ref],
            reviewer_assignment_id="reviewer-fresh",
            fresh_epoch_id="epoch-closure",
            review_id="review-closure",
            authority_ref=AUTHORITY,
        )
        with self.assertRaises(ReviewProvenanceError):
            self.kernel.accept_finding_closure(
                "finding-a",
                [resolution_ref],
                reviewer_assignment_id="reviewer-fresh",
                fresh_epoch_id="epoch-closure",
                review_id="review-closure",
                authority_ref=AUTHORITY,
            )

    def test_conservative_durable_filter_covers_auth_cookie_context_and_opaque_text(self):
        forbidden = (
            "Authorization",
            "api_key",
            "access-key",
            "cookie_header",
            "session_id",
            "refresh",
            "bearer",
            "private_context",
            "model_thoughts",
            "raw_tool_output",
        )
        for key in forbidden:
            with self.subTest(key=key):
                before = self.kernel.head()
                with self.assertRaises(AuthorizationError):
                    self.kernel.publish_artifact(
                        "deny-" + key.replace("_", "-"),
                        "v1",
                        {key: "must not persist"},
                        authority_ref=AUTHORITY,
                    )
                self.assertEqual(self.kernel.head(), before)
        with self.assertRaises(AuthorizationError):
            ControlKernel._ensure_durable_payload({"text": "opaque private_reasoning transcript"}, "opaque")
        with self.assertRaises(AuthorizationError):
            ControlKernel._ensure_durable_payload({"text": "access_token=redacted"}, "opaque")
        with self.assertRaises(AuthorizationError):
            ControlKernel._ensure_durable_payload({"text": "authorization.yaml"}, "opaque")
        # A path member is structured provenance, while the same string in
        # an opaque text member remains denied.
        ControlKernel._ensure_durable_payload({"path": "authorization.yaml"}, "path")

    def test_artifact_claimed_id_must_match_immutable_digest(self):
        self.kernel.publish_artifact("artifact-a", "v1", {"a": True}, authority_ref=AUTHORITY)
        self.kernel.publish_artifact("artifact-b", "v1", {"b": True}, authority_ref=AUTHORITY)
        ref = dict(self.kernel.read_state()["artifacts"]["artifact-a"]["object_ref"])
        ref["artifact_id"] = "artifact-b"
        with self.assertRaises(IntegrityBlockedError):
            self.kernel.publish_task_package(
                "forged-input",
                {"input_refs": [ref]},
                assignment_id="worker-forged",
                input_refs=[ref],
                authority_ref=AUTHORITY,
            )
        self.assertNotIn("forged-input", self.kernel.read_state()["tasks"])

    def test_node_claim_must_match_immutable_artifact_record(self):
        self.kernel.publish_artifact("artifact-a", "v1", {"a": True}, authority_ref=AUTHORITY)
        self.kernel.publish_artifact("artifact-b", "v1", {"b": True}, authority_ref=AUTHORITY)
        ref = dict(self.kernel.read_state()["artifacts"]["artifact-a"]["object_ref"])
        ref["node_id"] = "artifact:artifact-b"
        with self.assertRaises(IntegrityBlockedError):
            self.kernel.publish_task_package(
                "forged-node-input",
                {"input_refs": [ref]},
                assignment_id="worker-forged-node",
                input_refs=[ref],
                authority_ref=AUTHORITY,
            )

    def test_documented_edge_kinds_have_runtime_semantics(self):
        nodes = {
            "artifact:a": "artifact",
            "task:t": "task",
            "review:r": "review",
            "finding:f": "finding",
            "authority:auth": "authority",
            "approval:ap": "approval",
        }
        edges = [
            {"from": "artifact:a", "to": "task:t", "type": "requires"},
            {"from": "task:t", "to": "artifact:out", "type": "produces"},
            {"from": "authority:auth", "to": "task:t", "type": "authorizes"},
            {"from": "review:r", "to": "finding:f", "type": "verdict-for"},
            {"from": "artifact:a", "to": "task:t", "type": "converges"},
        ]
        nodes["artifact:out"] = "artifact"
        self.kernel._validate_edges(edges, nodes)

    def test_actual_frozen_a6_sources_convert_without_mutating_copies(self):
        source_dir = self.root / "a6-source"
        source_dir.mkdir()
        # Keep the regression hermetic.  This is an actual-shape copy of the
        # frozen A6 fixtures, not a read of the mutable Run/report tree.
        fixtures = ROOT / "agent-workflows" / "tests" / "fixtures" / "a6r"
        run_value = json.loads((fixtures / "legacy-run.json").read_text())
        run_value["aliases"] = ["hermetic-a6"]
        run_value["current_group"]["status"] = "open"
        run_value["current_epoch"].update({"status": "closed", "clear_before_next": True, "closed_at_revision": 11})
        bundle_value = json.loads((fixtures / "legacy-bundle.json").read_text())
        bundle_value.update({"group_id": "bootstrap", "aliases": ["hermetic-a6"], "state_revision": 11})
        bundle_value["context_epoch"].update({"group_id": "bootstrap", "status": "closed", "closed_at_revision": 11})
        report_value = json.loads((fixtures / "legacy-worker-report.json").read_text())
        report_value.update({"run_id": "legacy-a6", "group_id": "bootstrap", "status": "done", "aliases": ["hermetic-a6"], "state_revision": 11})
        sources = {"run": run_value, "bundle": bundle_value, "worker_report": report_value}
        copied = {}
        for key, value in sources.items():
            target = source_dir / (key + ".json")
            target.write_text(json.dumps(value, sort_keys=True))
            copied[key] = target
        source_bytes = {key: path.read_bytes() for key, path in copied.items()}
        source_digests = {key: _raw_digest(path) for key, path in copied.items()}
        result = LegacyConverter().convert(
            copied["run"],
            copied["bundle"],
            copied["worker_report"],
            destination=self.root / "converted",
            run_id="converted-a6",
            authority_ref=AUTHORITY,
            expected_source_revision=11,
            expected_source_digests=source_digests,
        )
        state = result["kernel"].read_state()
        self.assertEqual(state["migration"]["source_bindings"]["group_id"], "bootstrap")
        self.assertEqual(state["migration"]["source_bindings"]["epoch_id"], "epoch-0002")
        self.assertEqual(state["migration"]["source_bindings"]["source_revision"], 11)
        self.assertEqual(state["migration"]["source_bindings"]["source_status"], "open")
        self.assertEqual(state["metadata"]["aliases"], state["migration"]["source_bindings"]["aliases"])
        self.assertEqual(state["group"]["id"], "bootstrap")
        self.assertEqual(state["epoch"]["id"], "epoch-0002")
        self.assertEqual(state["epoch"]["status"], "closed")
        self.assertEqual(state["status"], "paused_after_epoch")
        for ref in state["migration"]["source_refs"]:
            value = result["kernel"].read_object(ref)["payload"]["value"]
            self.assertNotIn("text", value)
        self.assertEqual({key: path.read_bytes() for key, path in copied.items()}, source_bytes)
        self.assertEqual(NewReader().read(result["kernel"])["revision"], 2)

        # The frozen Markdown report carries ``work_id`` rather than the
        # newer JSON ``run_id`` spelling.  It is still a binding claim; a
        # copied report with a different work_id must not be silently attached
        # to the Run selected by the Run/Bundle pair.
        bad_report = source_dir / "bad-worker-report.json"
        bad_value = LegacyReader().read(copied["worker_report"])
        bad_value["work_id"] = "different-run"
        bad_report.write_text(json.dumps(bad_value, sort_keys=True))
        bad_digests = dict(source_digests, worker_report=_raw_digest(bad_report))
        with self.assertRaises(MigrationError):
            LegacyConverter().convert(
                copied["run"],
                copied["bundle"],
                bad_report,
                destination=self.root / "mismatched-report",
                run_id="mismatched-report",
                authority_ref=AUTHORITY,
                expected_source_revision=11,
                expected_source_digests=bad_digests,
            )

    def test_cutover_rejects_missing_target_or_incomplete_projection_before_pointer_write(self):
        pointer = PointerCutover(self.root / "pointer.json")
        old = {"revision": 1, "digest": "sha256:" + "c" * 64}
        new = {
            "schema": "dag-head/v1",
            "run_id": "fix-003",
            "workflow_version": "manual-bootstrap/v1",
            "graph_version": "artifact-task-dag/v1",
            "role": "orchestrator",
            "revision": 2,
            "state_revision": 2,
            "transaction_digest": "sha256:" + "d" * 64,
            "kernel_path": str(self.root / "missing-target"),
        }
        new["digest"] = "sha256:" + hashlib.sha256(
            json.dumps({key: value for key, value in new.items() if key != "kernel_path"}, sort_keys=True, separators=(",", ":")).encode()
        ).hexdigest()
        source = {"run": "sha256:" + "e" * 64, "bundle": "sha256:" + "f" * 64, "worker_report": "sha256:" + "0" * 64}
        proposal = pointer.proposal_digest(new, source, old)
        authority = {
            "status": "approved",
            "scopes": ["migration_cutover"],
            "migration_approval": True,
            "approval_ref": "approval-fix-003",
            "proposal_digest": proposal,
            "run_id": "fix-003",
            "role": "orchestrator",
            "assignment_id": "orchestrator",
        }
        with self.assertRaises(MigrationError):
            pointer.cutover(new, source_digests=source, old_pointer=old, expected_old_pointer=old, authority_ref=authority)
        self.assertFalse(pointer.path.exists())

    def test_skill_declares_complete_revision_and_input_provenance_contract(self):
        skill = (ROOT / "agent-workflows" / "skills" / "bootstrap-migrate-control-kernel" / "SKILL.md").read_text()
        for term in ("version", "owner", "Group", "objective", "schema", "creator", "freshness", "revision 19"):
            self.assertIn(term, skill)

    def test_complete_command_shape_rejects_before_projection_side_effect(self):
        status_path = self.kernel.projection_dir / "status.json"
        status_path.unlink()
        before = self.kernel.head()
        command = self.kernel.make_command(
            "publish_artifact",
            {"artifact_id": "shape-only", "version": "v1", "value": {}, "kind": "artifact", "path": None},
            authority_ref=AUTHORITY,
        )
        command["expected_head"]["unexpected"] = True
        with self.assertRaises(CommandValidationError):
            self.kernel.apply(command)
        self.assertEqual(self.kernel.head(), before)
        self.assertFalse(status_path.exists())

    def test_direct_review_rejects_non_schema_finding_provenance(self):
        self.kernel.publish_task_package("strict-candidate", {}, assignment_id="worker-strict", authority_ref=AUTHORITY)
        self.kernel.claim_task("strict-candidate", assignment_id="worker-strict", authority_ref=AUTHORITY)
        self.kernel.accept_task_result(
            "strict-candidate", {"status": "partial"}, worker_assignment_id="worker-strict", authority_ref=AUTHORITY
        )
        candidate_ref = self.kernel.read_state()["tasks"]["strict-candidate"]["package_ref"]
        self.kernel.open_review_epoch(
            "strict-review-epoch", candidate_ref, reviewer_assignment_id="reviewer-strict", authority_ref=AUTHORITY
        )
        command = self.kernel._command_for(
            "open_review",
            {
                "review_id": "strict-review",
                "candidate_task_id": "strict-candidate",
                "findings": [{
                    "finding_id": "strict-finding",
                    "fingerprint": "sha256:" + "a" * 64,
                    "requirement_ref": "R1",
                    "description": "schema check",
                    "evidence": [],
                    "severity": "major",
                    "blocking": True,
                    "state": "open",
                    "provenance": {"unregistered": True},
                }],
                "reviewer_assignment_id": "reviewer-strict",
                "fresh_epoch_id": "strict-review-epoch",
                "review_kind": "initial",
                "target_finding_id": None,
            },
            authority_ref=AUTHORITY,
        )
        before = self.kernel.head()
        with self.assertRaises(CommandValidationError):
            self.kernel.apply(command)
        self.assertEqual(self.kernel.head(), before)
        self.assertNotIn("strict-review", self.kernel.read_state()["reviews"])

    def test_direct_migration_requires_bound_source_records(self):
        digest = "sha256:" + "b" * 64
        command = self.kernel._command_for(
            "migrate_legacy",
            {
                "migration": {
                    "source_revision": 1,
                    "source_digests": {"run": digest, "bundle": digest, "worker_report": digest},
                    "source_bindings": {
                        "source_run_id": "fix-003",
                        "group_id": "bootstrap",
                        "epoch_id": "epoch-0001",
                        "aliases": ["fix-003"],
                        "source_revision": 1,
                        "run_status": "open",
                        "source_status": "active",
                        "epoch_status": "closed",
                    },
                    "run": {},
                    "bundle": {},
                    "worker_report": {},
                    "field_mapping": {},
                    "artifacts": {},
                }
            },
            authority_ref=AUTHORITY,
        )
        before = self.kernel.head()
        with self.assertRaises(CommandValidationError):
            self.kernel.apply(command)
        self.assertEqual(self.kernel.head(), before)


if __name__ == "__main__":
    unittest.main()
