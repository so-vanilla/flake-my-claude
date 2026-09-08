import hashlib
import json
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SRC = ROOT / "agent-workflows" / "src"

import sys

sys.path.insert(0, str(SRC))

from ai_agent_workflow.a7 import HandoffError, SelfHostHandoff  # noqa: E402
from ai_agent_workflow.control_kernel import ControlKernel  # noqa: E402
from ai_agent_workflow.migration import (  # noqa: E402
    LegacyConverter,
    MigrationError,
    PointerCutover,
    _safe_legacy_value,
)
from ai_agent_workflow.schema_validation import validate_document  # noqa: E402


AUTHORITY = {"status": "approved", "scopes": ["*"]}
ACCEPTED_SOURCE_DIGEST = "sha256:" + "a" * 64


def _raw_digest(path):
    return "sha256:" + hashlib.sha256(Path(path).read_bytes()).hexdigest()


class A7_SelfHostHandoffTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary.name)
        fixture_root = ROOT / "agent-workflows" / "tests" / "fixtures" / "a6r"
        source = self.root / "a6-source"
        source.mkdir()
        values = {
            "run": json.loads((fixture_root / "legacy-run.json").read_text()),
            "bundle": json.loads((fixture_root / "legacy-bundle.json").read_text()),
            "worker_report": json.loads((fixture_root / "legacy-worker-report.json").read_text()),
        }
        values["run"]["aliases"] = ["ai-agent-workflow-rebuild"]
        values["run"]["current_group"]["status"] = "open"
        values["run"]["current_epoch"].update(
            {"status": "closed", "clear_before_next": True, "closed_at_revision": 11}
        )
        values["bundle"].update(
            {
                "group_id": "bootstrap",
                "aliases": ["ai-agent-workflow-rebuild"],
                "state_revision": 11,
            }
        )
        values["bundle"]["context_epoch"].update(
            {
                "group_id": "bootstrap",
                "status": "closed",
                "clear_before_next": True,
                "closed_at_revision": 11,
            }
        )
        values["worker_report"].update(
            {
                "run_id": "legacy-a6",
                "group_id": "bootstrap",
                "status": "done",
                "aliases": ["ai-agent-workflow-rebuild"],
                "state_revision": 11,
            }
        )
        paths = {}
        for name, value in values.items():
            path = source / (name + ".json")
            path.write_text(json.dumps(value, sort_keys=True))
            paths[name] = path
        converted = LegacyConverter().convert(
            paths["run"],
            paths["bundle"],
            paths["worker_report"],
            destination=self.root / "kernel",
            authority_ref=AUTHORITY,
            expected_source_revision=11,
            expected_source_digests={name: _raw_digest(path) for name, path in paths.items()},
        )
        self.legacy_paths = paths
        self.source_digests = {name: _raw_digest(path) for name, path in paths.items()}
        self.converted = converted
        self.kernel = converted["kernel"]
        self.objective = values["run"]["objective_ref"]

    def tearDown(self):
        self.temporary.cleanup()

    def _write_current_sources(self):
        source = self.root / "current-source"
        source.mkdir()
        checkpoint = {
            "schema": "ai-agent-checkpoint/v0",
            "checkpoint_id": "cp-0042-a7-preactivation-authorized",
            "run_id": "legacy-a6",
            "workflow_version": "manual-bootstrap/v1",
            "group_id": "bootstrap",
            "skill_id": "bootstrap-self-host-handoff",
            "state_revision": 47,
        }
        checkpoint_path = source / "checkpoint.json"
        checkpoint_path.write_text(json.dumps(checkpoint, sort_keys=True))
        checkpoint_digest = _raw_digest(checkpoint_path)
        run = {
            "schema": "ai-agent-run/v0",
            "run_id": "legacy-a6",
            "workflow_version": "manual-bootstrap/v1",
            "status": "a7_pre_activation_authorized_current_state_gap_analysis",
            "state_revision": 47,
            "objective": {
                "proposed_path": self.objective["path"],
                "proposed_version": self.objective["version"],
                "proposed_digest": self.objective["digest"],
                "approval_status": self.objective.get("approval_status"),
            },
            "aliases": ["ai-agent-workflow-rebuild"],
            "current_group": {"id": "bootstrap", "status": "open"},
            "context_epoch": {
                "id": "epoch-0002",
                "group_id": "bootstrap",
                "status": "closed",
                "clear_before_next": True,
                "closed_at_revision": 11,
            },
            "last_checkpoint": {
                "id": checkpoint["checkpoint_id"],
                "path": "checkpoints/042-a7-preactivation-authorized.md",
                "state_revision": 47,
                "checkpoint_digest": checkpoint_digest,
            },
        }
        evidence = {
            "schema": "a6r-operational-evidence/v1",
            "run_id": "legacy-a6",
            "workflow_version": "manual-bootstrap/v1",
            "status": "passed",
            "accepted_source_digest": ACCEPTED_SOURCE_DIGEST,
            "gates": {"G%s" % index: {"status": "pass"} for index in range(1, 9)},
            "limitations": ["arbitrary physical power loss remains a residual risk"],
        }
        approval = {
            "schema": "ai-agent-authority/v1",
            "authority_id": "a7-preactivation-authorization",
            "run_id": "legacy-a6",
            "workflow_version": "manual-bootstrap/v1",
            "decision": {"migration_approval": True, "a7_self_host_handoff": "authorized"},
        }
        result = {}
        for name, value in (("run", run), ("evidence", evidence), ("approval", approval)):
            path = source / (name + ".json")
            path.write_text(json.dumps(value, sort_keys=True))
            result[name] = path
        result["checkpoint"] = checkpoint_path
        return self._write_rehearsal_receipt(result)

    def _write_rehearsal_receipt(self, sources, accepted_source_digest=None):
        accepted = accepted_source_digest or ACCEPTED_SOURCE_DIGEST
        old_target = {"path": "/isolated/a6-old-target", "digest": "sha256:" + "b" * 64}
        new_target = {"path": "/isolated/a6r-new-target", "digest": "sha256:" + "c" * 64}
        receipt = {
            "schema": "a7-migration-cutover-rollback-rehearsal/v1",
            "run_id": "legacy-a6",
            "workflow_version": "manual-bootstrap/v1",
            "accepted_source_digest": accepted,
            "physical_old_target": old_target,
            "physical_new_target": new_target,
            "migrated_head": self.kernel.head(),
            "expected_pointer_cas": {
                "pointer_path": "/isolated/a7-active-pointer.json",
                "expected_old_target": old_target,
                "expected_new_target": new_target,
            },
        }
        path = sources["run"].parent / "rehearsal.json"
        path.write_text(json.dumps(receipt, sort_keys=True))
        return {**sources, "rehearsal": path}

    @staticmethod
    def _full_pointer(converted):
        state = converted["kernel"].read_state()
        return {
            **converted["new_head"],
            "status": state["status"],
            "source_digests": dict(converted["source_digests"]),
            "source_revision": converted["source_revision"],
            "group_id": state["group"]["id"],
            "epoch_id": state["epoch"]["id"],
            "aliases": list(state["metadata"]["aliases"]),
        }

    def test_safe_legacy_projection_matches_contextual_reference_path_admission(self):
        marker_path = "artifacts/a7-finding-validation-001-authorization.yaml"
        projected = _safe_legacy_value(
            {
                "authority_path": marker_path,
                "path": marker_path,
                "audit_note": "temporary auth value",
                "context_budget": {
                    "target": 200000,
                    "normal_limit": 300000,
                    "absolute_limit": 500000,
                    "token_status": "unavailable",
                    "token_count": None,
                },
            }
        )
        self.assertEqual(projected["authority_path"], "[legacy opaque text omitted]")
        self.assertEqual(projected["path"], marker_path)
        self.assertEqual(projected["audit_note"], "[legacy opaque text omitted]")
        self.assertEqual(projected["context_budget"]["token_status"], "unavailable")
        self.assertIsNone(projected["context_budget"]["token_count"])
        ControlKernel._ensure_durable_payload(projected, "legacy projection")

    def test_latest_manual_revision_is_preserved_separately_from_a6_epoch_revision(self):
        sources = self._write_current_sources()
        handoff = SelfHostHandoff(self.kernel)
        package = handoff.prepare(
            sources,
            destination=self.root / "handoff-package",
            expected_state_revision=47,
            accepted_source_digest=ACCEPTED_SOURCE_DIGEST,
        )
        receipt = handoff.adopt(
            package["manifest_path"],
            expected_manifest_digest=package["manifest_digest"],
            authority_ref=AUTHORITY,
        )

        self.assertEqual(receipt["run_id"], "legacy-a6")
        self.assertEqual(receipt["objective_ref"]["version"], "v001")
        self.assertEqual(receipt["manual_state_revision"], 47)
        self.assertEqual(receipt["manual_epoch_closed_at_revision"], 11)
        self.assertNotEqual(receipt["kernel_state_revision"], 47)
        self.assertFalse(receipt["next_group_can_begin"])

        schema_root = ROOT / "agent-workflows" / "schemas"
        validate_document(
            receipt,
            json.loads((schema_root / "a7-self-host-status-v1.schema.json").read_text()),
        )

        self.kernel.close_epoch(
            acceptance_evidence=["A7 handoff package"],
            next_inputs=["contracts-and-schema"],
            clear_before_next=True,
            authority_ref=AUTHORITY,
        )
        self.kernel.close_group(
            acceptance_evidence=["A7 handoff artifact"],
            next_inputs=["contracts-and-schema"],
            next_group="contracts-and-schema",
            authority_ref=AUTHORITY,
        )

        fresh_kernel = ControlKernel(self.kernel.path)
        fresh_kernel.run_id = "legacy-a6"
        fresh = SelfHostHandoff(fresh_kernel).status()
        self.assertEqual(fresh["manual_state_revision"], 47)
        self.assertEqual(fresh["checkpoint_id"], "cp-0042-a7-preactivation-authorized")
        self.assertEqual(fresh["manifest_digest"], package["manifest_digest"])
        self.assertTrue(fresh["next_group_can_begin"])
        self.assertEqual(fresh["next_group"], "contracts-and-schema")
        validate_document(
            fresh,
            json.loads((schema_root / "a7-self-host-status-v1.schema.json").read_text()),
        )

    def test_closed_a7_target_can_cut_over_and_roll_back_with_frozen_a6_provenance(self):
        sources = self._write_current_sources()
        package = SelfHostHandoff(self.kernel).prepare(
            sources,
            destination=self.root / "cutover-handoff-package",
            expected_state_revision=47,
            accepted_source_digest=ACCEPTED_SOURCE_DIGEST,
        )
        SelfHostHandoff(self.kernel).adopt(
            package["manifest_path"],
            expected_manifest_digest=package["manifest_digest"],
            authority_ref=AUTHORITY,
        )
        self.kernel.close_epoch(
            acceptance_evidence=["A7 handoff package"],
            next_inputs=["contracts-and-schema"],
            clear_before_next=True,
            authority_ref=AUTHORITY,
        )
        self.kernel.close_group(
            acceptance_evidence=["A7 handoff artifact"],
            next_inputs=["contracts-and-schema"],
            next_group="contracts-and-schema",
            authority_ref=AUTHORITY,
        )

        old = LegacyConverter().convert(
            self.legacy_paths["run"],
            self.legacy_paths["bundle"],
            self.legacy_paths["worker_report"],
            destination=self.root / "physical-old-kernel",
            run_id="physical-old",
            authority_ref=AUTHORITY,
            expected_source_revision=11,
            expected_source_digests=self.source_digests,
        )
        old_pointer = self._full_pointer(old)
        new_head = {**self.kernel.head(), "kernel_path": str(self.kernel.path)}
        pointer = PointerCutover(self.root / "active-pointer.json")
        proposal = pointer.proposal_digest(new_head, self.converted["source_digests"], old_pointer)
        authority = {
            "status": "approved",
            "scopes": ["migration_cutover", "migration_rollback"],
            "migration_approval": True,
            "approval_ref": "a7-cutover-rehearsal",
            "proposal_digest": proposal,
            "run_id": "legacy-a6",
            "role": "orchestrator",
            "assignment_id": "orchestrator",
        }
        cutover = pointer.cutover(
            new_head,
            source_digests=self.converted["source_digests"],
            old_pointer=old_pointer,
            expected_old_pointer=old_pointer,
            authority_ref=authority,
        )
        self.assertEqual(cutover["active"], "new")
        self.assertEqual(pointer.rollback(authority_ref=authority, expected_active="new")["active"], "old")

        bad_source = dict(self.converted["source_digests"])
        bad_source["run"] = "sha256:" + "0" * 64
        rejected_pointer = PointerCutover(self.root / "bad-source-pointer.json")
        bad_authority = dict(
            authority,
            proposal_digest=rejected_pointer.proposal_digest(new_head, bad_source, old_pointer),
        )
        with self.assertRaisesRegex(MigrationError, "source digest binding"):
            rejected_pointer.cutover(
                new_head,
                source_digests=bad_source,
                old_pointer=old_pointer,
                expected_old_pointer=old_pointer,
                authority_ref=bad_authority,
            )
        self.assertFalse(rejected_pointer.path.exists())

        stale_pointer = PointerCutover(self.root / "stale-cas-pointer.json")
        stale_old = {"revision": old_pointer["revision"] + 1, "digest": old_pointer["digest"]}
        stale_authority = dict(
            authority,
            proposal_digest=stale_pointer.proposal_digest(new_head, self.converted["source_digests"], old_pointer),
        )
        with self.assertRaisesRegex(MigrationError, "expected old pointer"):
            stale_pointer.cutover(
                new_head,
                source_digests=self.converted["source_digests"],
                old_pointer=old_pointer,
                expected_old_pointer=stale_old,
                authority_ref=stale_authority,
            )
        self.assertFalse(stale_pointer.path.exists())

    def test_prepare_rejects_absent_null_empty_or_non_string_manual_status_before_publication(self):
        sources = self._write_current_sources()
        original = json.loads(sources["run"].read_text())
        for label, value in (("absent", None), ("null", None), ("empty", ""), ("number", 47)):
            with self.subTest(label=label):
                run = dict(original)
                if label == "absent":
                    run.pop("status")
                else:
                    run["status"] = value
                sources["run"].write_text(json.dumps(run, sort_keys=True))
                handoff = SelfHostHandoff(self.kernel)
                destination = self.root / ("invalid-status-" + label)
                before_head = self.kernel.head()
                with self.assertRaises(HandoffError):
                    handoff.prepare(
                        sources,
                        destination=destination,
                        expected_state_revision=47,
                        accepted_source_digest=ACCEPTED_SOURCE_DIGEST,
                    )
                self.assertFalse(destination.exists())
                self.assertEqual(self.kernel.head(), before_head)

    def test_prepare_requires_explicit_accepted_source_and_raw_bound_rehearsal_receipt(self):
        accepted_source_digest = ACCEPTED_SOURCE_DIGEST
        sources = self._write_rehearsal_receipt(
            self._write_current_sources(), accepted_source_digest
        )
        package = SelfHostHandoff(self.kernel).prepare(
            sources,
            destination=self.root / "bound-rehearsal-package",
            expected_state_revision=47,
            accepted_source_digest=accepted_source_digest,
        )
        manifest = json.loads(package["manifest_path"].read_text())
        self.assertEqual(manifest["accepted_source_digest"], accepted_source_digest)
        self.assertEqual(manifest["rehearsal_receipt_digest"], _raw_digest(sources["rehearsal"]))
        self.assertEqual(manifest["sources"]["rehearsal"]["digest"], _raw_digest(sources["rehearsal"]))

    def test_prepare_rejects_unbound_accepted_source_or_rehearsal_before_publication(self):
        sources = self._write_current_sources()
        originals = {
            name: json.loads(path.read_text())
            for name, path in sources.items()
            if name != "checkpoint"
        }
        cases = {
            "missing-rehearsal": lambda values, candidate: candidate.pop("rehearsal"),
            "malformed-accepted-source": lambda values, candidate: values["evidence"].update({"accepted_source_digest": "not-a-digest"}),
            "mismatched-evidence-source": lambda values, candidate: values["evidence"].update({"accepted_source_digest": "sha256:" + "d" * 64}),
            "mismatched-rehearsal-source": lambda values, candidate: values["rehearsal"].update({"accepted_source_digest": "sha256:" + "d" * 64}),
            "mismatched-rehearsal-run": lambda values, candidate: values["rehearsal"].update({"run_id": "another-run"}),
            "mismatched-rehearsal-head": lambda values, candidate: values["rehearsal"]["migrated_head"].update({"revision": 99}),
            "mismatched-pointer-cas": lambda values, candidate: values["rehearsal"]["expected_pointer_cas"].update({"expected_new_target": {"path": "/isolated/other", "digest": "sha256:" + "e" * 64}}),
        }
        for label, mutate in cases.items():
            with self.subTest(label=label):
                values = json.loads(json.dumps(originals))
                candidate = dict(sources)
                mutate(values, candidate)
                for name, value in values.items():
                    if name in candidate:
                        candidate[name].write_text(json.dumps(value, sort_keys=True))
                destination = self.root / ("rejected-" + label)
                before_head = self.kernel.head()
                with self.assertRaises(HandoffError):
                    SelfHostHandoff(self.kernel).prepare(
                        candidate,
                        destination=destination,
                        expected_state_revision=47,
                        accepted_source_digest=ACCEPTED_SOURCE_DIGEST,
                    )
                self.assertFalse(destination.exists())
                self.assertEqual(self.kernel.head(), before_head)

    def test_retry_after_epoch_open_completes_without_opening_another_epoch(self):
        sources = self._write_current_sources()
        handoff = SelfHostHandoff(self.kernel)
        package = handoff.prepare(
            sources,
            destination=self.root / "retry-package",
            expected_state_revision=47,
            accepted_source_digest=ACCEPTED_SOURCE_DIGEST,
        )
        migrated = self.kernel.read_state()
        self.kernel.open_epoch(
            "epoch-0003-a7-self-host-handoff",
            migrated["epoch"]["bundle_ref"],
            authority_ref=AUTHORITY,
            idempotency_key="simulated-interrupted-a7-open",
        )

        receipt = handoff.adopt(
            package["manifest_path"],
            expected_manifest_digest=package["manifest_digest"],
            authority_ref=AUTHORITY,
        )

        self.assertEqual(receipt["kernel_state_revision"], 4)
        self.assertEqual(self.kernel.read_state()["epoch"]["id"], "epoch-0003-a7-self-host-handoff")

    def test_manifest_status_and_skill_are_schema_owned(self):
        sources = self._write_current_sources()
        handoff = SelfHostHandoff(self.kernel)
        package = handoff.prepare(
            sources,
            destination=self.root / "schema-package",
            expected_state_revision=47,
            accepted_source_digest=ACCEPTED_SOURCE_DIGEST,
        )
        receipt = handoff.adopt(
            package["manifest_path"],
            expected_manifest_digest=package["manifest_digest"],
            authority_ref=AUTHORITY,
        )
        schema_root = ROOT / "agent-workflows" / "schemas"
        validate_document(
            json.loads(package["manifest_path"].read_text()),
            json.loads((schema_root / "a7-handoff-package-v1.schema.json").read_text()),
        )
        validate_document(
            receipt,
            json.loads((schema_root / "a7-self-host-status-v1.schema.json").read_text()),
        )
        skill = (
            ROOT
            / "agent-workflows"
            / "skills"
            / "bootstrap-self-host-handoff"
            / "SKILL.md"
        ).read_text()
        for required in (
            "docs/plans/ai-agent-workflow-step-catalog.md",
            "same run ID",
            "manual_state_revision",
            "G1-G8",
            "accepted-source identity",
            "migration/cutover/rollback rehearsal receipt",
            "expected-pointer CAS",
            "next-Group readiness",
            "pointer-only",
            "rollback",
            "Do not activate",
        ):
            self.assertIn(required, skill)


if __name__ == "__main__":
    unittest.main()
