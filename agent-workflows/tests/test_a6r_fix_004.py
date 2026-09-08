import copy
import hashlib
import json
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch


ROOT = Path(__file__).resolve().parents[2]
SRC = ROOT / "agent-workflows" / "src"
sys.path.insert(0, str(SRC))

from ai_agent_workflow import migration as migration_module  # noqa: E402
from ai_agent_workflow.control_kernel import (  # noqa: E402
    AuthorizationError,
    ControlKernel,
    IntegrityBlockedError,
    InjectedCrash,
    ReviewProvenanceError,
    StaleHeadError,
)
from ai_agent_workflow.migration import LegacyConverter, LegacyReader, MigrationError, PointerCutover  # noqa: E402
from ai_agent_workflow.schema_validation import SchemaValidationError, validate_document  # noqa: E402


AUTHORITY = {"status": "approved", "scopes": ["*"]}
OBJECTIVE = {"path": "objectives/v001.md", "version": "v001", "digest": "a" * 64}


def _raw_digest(path):
    return "sha256:" + hashlib.sha256(Path(path).read_bytes()).hexdigest()


def _write_hermetic_actual_a6_sources(root, *, state_revision, status):
    """Write a stable copy of the actual A6 field shapes into a test tempdir."""

    source_dir = Path(root) / ("sources-%s" % state_revision)
    source_dir.mkdir(parents=True)
    run = {
        "schema": "ai-agent-run/v0",
        "run_id": "hermetic-a6",
        "workflow_version": "manual-bootstrap/v1",
        "status": status,
        "state_revision": state_revision,
        "aliases": ["hermetic-a6"],
        "objective_ref": copy.deepcopy(OBJECTIVE),
        "current_group": {"id": "bootstrap", "status": status},
        "current_epoch": {
            "id": "epoch-0002",
            "status": "closed",
            "clear_before_next": True,
            "closed_at_revision": 11,
        },
        "trajectory_correction": {"migration_source": {"source_state_revision": 11}},
    }
    bundle = {
        "schema": "artifact-bundle/v1",
        "bundle_id": "epoch-0002-a6",
        "version": "v1",
        "run_id": "hermetic-a6",
        "workflow_version": "manual-bootstrap/v1",
        "group_id": "bootstrap",
        "aliases": ["hermetic-a6"],
        "state_revision": state_revision,
        "context_epoch": {
            "id": "epoch-0002",
            "group_id": "bootstrap",
            "status": "closed",
            "clear_before_next": True,
            "closed_at_revision": 11,
        },
        "canonical_artifacts": [],
    }
    report = """---
schema: agent-worker-report/v1
work_id: hermetic-a6
agent_id: hermetic-worker
ticket_id: hermetic-ticket
status: done
---

This body is deliberately outside the durable context.
"""
    run_path = source_dir / "run.yaml"
    bundle_path = source_dir / "bundle.yaml"
    report_path = source_dir / "worker-report.md"
    run_path.write_text(json.dumps(run, sort_keys=True))
    bundle_path.write_text(json.dumps(bundle, sort_keys=True))
    report_path.write_text(report)
    paths = {"run": run_path, "bundle": bundle_path, "worker_report": report_path}
    digests = {key: _raw_digest(path) for key, path in paths.items()}
    return paths, digests


class A6RFix004Tests(unittest.TestCase):
    def setUp(self):
        self.tempdir = tempfile.TemporaryDirectory()
        self.root = Path(self.tempdir.name)
        self.kernel = ControlKernel(self.root / "kernel", "fix-004")
        self.kernel.entry(OBJECTIVE, authority_ref=AUTHORITY, aliases=["fix-004"])

    def tearDown(self):
        self.tempdir.cleanup()

    def _prepare_closure(self):
        self.kernel.publish_task_package("candidate", {}, assignment_id="worker-candidate", authority_ref=AUTHORITY)
        self.kernel.claim_task("candidate", assignment_id="worker-candidate", authority_ref=AUTHORITY)
        self.kernel.accept_task_result(
            "candidate", {"status": "partial"}, worker_assignment_id="worker-candidate", authority_ref=AUTHORITY
        )
        task_ref = self.kernel.read_state()["tasks"]["candidate"]["package_ref"]
        self.kernel.open_review_epoch(
            "epoch-initial", task_ref, reviewer_assignment_id="reviewer-initial", authority_ref=AUTHORITY
        )
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
        self.kernel.open_review_epoch(
            "epoch-closure", resolution_ref, reviewer_assignment_id="reviewer-fresh", authority_ref=AUTHORITY
        )
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
        self.kernel.accept_finding_closure(
            "finding-a",
            [resolution_ref],
            reviewer_assignment_id="reviewer-fresh",
            fresh_epoch_id="epoch-closure",
            review_id="review-closure",
            authority_ref=AUTHORITY,
        )
        return resolution_ref

    def test_migration_uses_immutable_snapshot_when_source_mutates_at_publish(self):
        paths, source_digests = _write_hermetic_actual_a6_sources(
            self.root, state_revision=21, status="a6r_fix_003_re_review_rejected_fix_004_authorized"
        )
        destination = self.root / "converted"
        original_bytes = paths["run"].read_bytes()
        original_replace = migration_module.os.replace
        mutated = {"value": False}

        def mutate_before_destination_publish(source, target):
            if Path(target).resolve() == destination.resolve() and not mutated["value"]:
                changed = json.loads(paths["run"].read_text())
                changed["state_revision"] = 999
                paths["run"].write_text(json.dumps(changed, sort_keys=True))
                mutated["value"] = True
            return original_replace(source, target)

        with patch.object(migration_module.os, "replace", side_effect=mutate_before_destination_publish):
            result = LegacyConverter().convert(
                paths["run"],
                paths["bundle"],
                paths["worker_report"],
                destination=destination,
                run_id="converted",
                authority_ref=AUTHORITY,
                expected_source_revision=11,
                expected_source_digests=source_digests,
            )
        self.assertTrue(mutated["value"])
        self.assertNotEqual(paths["run"].read_bytes(), original_bytes)
        state = result["kernel"].read_state()
        self.assertEqual(state["migration"]["source_digests"]["run"], source_digests["run"])
        source_object = next(
            result["kernel"].read_object(ref)
            for ref in state["migration"]["source_refs"]
            if ref["object_type"] == "legacy-run"
        )
        self.assertEqual(source_object["payload"]["value"]["state_revision"], 21)
        self.assertEqual(result["source_snapshot"]["mode"], "immutable")

    def test_actual_a6_shape_test_is_hermetic_across_legitimate_status_and_revision(self):
        for revision, status in ((21, "a6r_fix_003_re_review_rejected_fix_004_authorized"), (22, "paused_after_epoch")):
            with self.subTest(revision=revision, status=status):
                paths, source_digests = _write_hermetic_actual_a6_sources(
                    self.root, state_revision=revision, status=status
                )
                result = LegacyConverter().convert(
                    paths["run"],
                    paths["bundle"],
                    paths["worker_report"],
                    destination=self.root / ("converted-%s" % revision),
                    run_id="converted-%s" % revision,
                    authority_ref=AUTHORITY,
                    expected_source_revision=11,
                    expected_source_digests=source_digests,
                )
                bindings = result["kernel"].read_state()["migration"]["source_bindings"]
                self.assertEqual(bindings["source_revision"], 11)
                self.assertEqual(bindings["source_status"], status)

    def test_finding_verdict_reverse_binds_review_epoch_finding_resolution_and_package(self):
        resolution_ref = self._prepare_closure()
        state = self.kernel.read_state()
        verdict = state["verdicts"]["review-closure"]
        for key in (
            "review_id",
            "finding_id",
            "reviewer_assignment_id",
            "fresh_epoch_id",
            "resolution_ref",
            "evidence_refs",
            "closure_package_ref",
        ):
            self.assertIn(key, verdict)
        self.assertEqual(verdict["review_id"], "review-closure")
        self.assertEqual(verdict["finding_id"], "finding-a")
        self.assertEqual(verdict["resolution_ref"], resolution_ref)
        self.assertEqual(verdict["evidence_refs"], [resolution_ref])
        self.assertEqual(verdict["closure_package_ref"], state["reviews"]["review-closure"]["package_ref"])
        verdict_object = self.kernel.read_object(state["findings"]["finding-a"]["closure_ref"])
        self.assertEqual(verdict_object["payload"]["resolution_ref"], resolution_ref)
        self.assertEqual(verdict_object["payload"]["closure_package_ref"], verdict["closure_package_ref"])

        for mutation in (
            {"review_id": "unregistered-review"},
            {"fresh_epoch_id": "epoch-not-registered"},
            {"finding_id": "finding-other"},
        ):
            forged = copy.deepcopy(state)
            forged["verdicts"]["review-closure"].update(mutation)
            with self.subTest(mutation=mutation):
                with self.assertRaises(IntegrityBlockedError):
                    self.kernel._validate_state(forged, load_objects=False)
        forged = copy.deepcopy(state)
        for key in (
            "finding_id", "reviewer_assignment_id", "fresh_epoch_id", "resolution_ref",
            "evidence_refs", "closure_package_ref",
        ):
            forged["verdicts"]["review-closure"].pop(key)
        with self.assertRaisesRegex(IntegrityBlockedError, "full Verdict binding"):
            self.kernel._validate_state(forged, load_objects=False)
        forged = copy.deepcopy(state)
        forged["reviews"]["review-initial"]["findings"][0]["fingerprint"] = "sha256:" + ("0" * 64)
        with self.assertRaisesRegex(IntegrityBlockedError, "fingerprint"):
            self.kernel._validate_state(forged, load_objects=False)

    def test_future_task_freshness_is_rejected_before_ready_or_claim(self):
        before = self.kernel.head()
        with self.assertRaises(StaleHeadError):
            self.kernel.publish_task_package(
                "future-task",
                {"freshness": {"epoch_id": "epoch-0001", "created_at_revision": 999}},
                assignment_id="worker-future",
                authority_ref=AUTHORITY,
            )
        self.assertEqual(self.kernel.head(), before)
        self.assertNotIn("future-task", self.kernel.read_state()["tasks"])

    def test_genesis_head_can_only_be_created_by_the_orchestrator(self):
        genesis = ControlKernel(self.root / "genesis", "genesis")
        initial_state = genesis._empty_state(
            OBJECTIVE,
            "manual-bootstrap/v1",
            "bootstrap",
            "epoch-0001",
            AUTHORITY,
            ["genesis"],
            [],
        )
        command = genesis._make_command(
            "entry",
            actor_role="worker",
            assignment_id="worker-genesis",
            authority_ref=AUTHORITY,
            payload={"objective_ref": copy.deepcopy(OBJECTIVE)},
            idempotency_key="entry:genesis",
        )
        command["workflow_version"] = "manual-bootstrap/v1"
        with self.assertRaisesRegex(AuthorizationError, "Orchestrator"):
            genesis._commit(command, genesis._entry_reducer, initial=True, initial_state=initial_state)
        self.assertFalse(genesis.path.exists())

    def test_recovery_reports_a_published_genesis_transaction_without_adopting_it(self):
        genesis = ControlKernel(self.root / "orphan-genesis", "orphan-genesis")
        with self.assertRaises(InjectedCrash):
            with genesis.fault("after_publish_before_head"):
                genesis.entry(OBJECTIVE, authority_ref=AUTHORITY, aliases=["orphan-genesis"])
        self.assertFalse(genesis.head_path.exists())
        recovery = genesis.recover()
        self.assertTrue(recovery["head_unchanged"])
        self.assertEqual(len(recovery["orphan_transactions"]), 1)
        self.assertTrue(all(item.startswith("sha256:") for item in recovery["orphan_transactions"]))

    def test_wildcard_and_concrete_scopes_compete_in_both_directions(self):
        scoped_authority = {"status": "approved", "scopes": ["*"], "write_scopes": ["*"]}
        self.kernel.publish_task_package(
            "wild-first", {"write_scope": ["*"]}, assignment_id="worker-wild-first", authority_ref=scoped_authority
        )
        self.kernel.publish_task_package(
            "concrete-second", {"write_scope": ["src/component"]}, assignment_id="worker-concrete-second", authority_ref=scoped_authority
        )
        self.kernel.claim_task("wild-first", assignment_id="worker-wild-first", authority_ref=scoped_authority)
        self.assertNotIn("concrete-second", self.kernel.ready_tasks())
        self.kernel.release_task("wild-first", assignment_id="worker-wild-first", authority_ref=scoped_authority)
        self.kernel.claim_task("concrete-second", assignment_id="worker-concrete-second", authority_ref=scoped_authority)
        self.assertNotIn("wild-first", self.kernel.ready_tasks())

    def test_bare_auth_phrase_is_denied_in_opaque_and_structured_durable_values(self):
        values = (
            {"text": "temporary auth"},
            {"message": "AUTH"},
            {"nested": {"note": "temporary authentication"}},
            {"tuple": ({"text": "temporary auth"},)},
        )
        for value in values:
            with self.subTest(value=value):
                with self.assertRaises(AuthorizationError):
                    self.kernel.publish_artifact(
                        "bare-auth-%s" % len(self.kernel.read_state()["artifacts"]),
                        "v1",
                        value,
                        authority_ref=AUTHORITY,
                    )

    def _converted_result(self, name):
        paths, source_digests = _write_hermetic_actual_a6_sources(self.root / name, state_revision=21, status="source")
        return LegacyConverter().convert(
            paths["run"],
            paths["bundle"],
            paths["worker_report"],
            destination=self.root / (name + "-kernel"),
            run_id=name,
            authority_ref=AUTHORITY,
            expected_source_revision=11,
            expected_source_digests=source_digests,
        )

    def test_rollback_requires_a_physical_old_target_before_pointer_mutation(self):
        old_result = self._converted_result("old-target")
        new_result = self._converted_result("new-target")
        old_head = old_result["new_head"]
        old = {
            "revision": old_head["revision"],
            "digest": old_head["transaction_digest"],
        }
        pointer = PointerCutover(self.root / "pointer.json")
        new_head = new_result["new_head"]
        source_digests = new_result["source_digests"]
        proposal = pointer.proposal_digest(new_head, source_digests, old)
        authority = {
            "status": "approved",
            "scopes": ["migration_cutover", "migration_rollback"],
            "migration_approval": True,
            "approval_ref": "approval-fix-004",
            "proposal_digest": proposal,
            "run_id": "new-target",
            "role": "orchestrator",
            "assignment_id": "orchestrator",
        }
        pointer.cutover(
            new_head,
            source_digests=source_digests,
            old_pointer=old,
            expected_old_pointer=old,
            authority_ref=authority,
        )
        before = pointer.read()
        with self.assertRaises(MigrationError):
            pointer.rollback(authority_ref=authority, expected_active="new")
        self.assertEqual(pointer.read(), before)

    def test_task_schema_rejects_absolute_output_path_like_runtime(self):
        schema = json.loads((ROOT / "agent-workflows" / "schemas" / "task-package-v1.schema.json").read_text())
        package = {
            "task_id": "absolute-output",
            "attempt_id": "attempt-absolute-output",
            "assignment": {"role": "worker", "assignment_id": "worker-absolute-output"},
            "input_refs": [],
            "write_scope": ["src"],
            "acceptance": [],
            "freshness": {"epoch_id": "epoch-0001", "created_at_revision": 2},
            "stop_conditions": [],
            "invalidated": False,
            "stop_requested": False,
            "output_path": "/outside/result.json",
            "status": "ready",
        }
        with self.assertRaises(SchemaValidationError):
            validate_document(package, schema)

    def test_skill_contains_catalog_provenance_purpose_and_advice_contract(self):
        skill = (ROOT / "agent-workflows" / "skills" / "bootstrap-migrate-control-kernel" / "SKILL.md").read_text()
        for term in (
            "revision `21`",
            "Run",
            "objective",
            "sub-objective",
            "contribution",
            "purpose audit",
            "aligned",
            "uncertain",
            "diverged",
            "next Skill",
            "alternative",
            "clear",
            "model advice",
            "failure",
            "recovery",
            "creator",
            "freshness",
            "expected raw source digest",
        ):
            self.assertIn(term, skill)


if __name__ == "__main__":
    unittest.main()
