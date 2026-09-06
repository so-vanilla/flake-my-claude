import copy
import hashlib
import json
import sys
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "src"))

from ai_agent_workflow.bootstrap_lifecycle import (  # noqa: E402
    BootstrapLifecycleV1,
    CompositionContractError,
)
from ai_agent_workflow.closure_protocol import SharedClosureProtocolV1  # noqa: E402
from ai_agent_workflow.control_kernel import (  # noqa: E402
    ControlKernel,
    DuplicateCommandError,
    IntegrityBlockedError,
    KernelError,
    StaleHeadError,
)
from ai_agent_workflow.section_control_plane import (  # noqa: E402
    SectionContractError,
    SectionControlPlaneV1,
    open_section,
)


class GroupManifestCompositionTests(unittest.TestCase):
    def setUp(self):
        self.fixture = json.loads(
            (ROOT / "tests" / "fixtures" / "s1" / "lifecycle-cases.json").read_text()
        )

    def test_two_physical_manifests_map_all_canonical_ids_exactly_once(self):
        composition = BootstrapLifecycleV1(ROOT).load_group_manifests()

        self.assertEqual(composition["schema"], "s1-group-composition/v1")
        self.assertEqual(composition["qualified_ids"], self.fixture["qualified_ids"])
        self.assertEqual(len(composition["qualified_ids"]), len(set(composition["qualified_ids"])))
        self.assertEqual(
            composition["retained_receipt_digests"], self.fixture["retained_receipts"]
        )

    def test_manifest_boundary_rejects_unknown_duplicate_bare_and_cross_bound_entries(self):
        bootstrap = json.loads((ROOT / "groups" / "bootstrap.json").read_text())
        shared = json.loads((ROOT / "groups" / "shared-closure.json").read_text())
        cases = []
        unknown = copy.deepcopy(bootstrap)
        unknown["contracts"][0]["qualified_id"] = "group.A.A0"
        cases.append((unknown, shared))
        duplicate = copy.deepcopy(bootstrap)
        duplicate["contracts"][-1] = copy.deepcopy(duplicate["contracts"][0])
        cases.append((duplicate, shared))
        bare = copy.deepcopy(shared)
        bare["selectors"][0] = "F1"
        cases.append((bootstrap, bare))
        selector_mismatch = copy.deepcopy(bootstrap)
        selector_mismatch["contracts"][0]["selector_ref"]["selector"] = "BootstrapContractsV1.plan_workspace"
        cases.append((selector_mismatch, shared))
        source_mismatch = copy.deepcopy(bootstrap)
        source_mismatch["contracts"][0]["source_ref"]["digest"] = "sha256:" + "0" * 64
        cases.append((source_mismatch, shared))
        cross_bound = copy.deepcopy(bootstrap)
        cross_bound["contracts"][5]["receipt_ref"] = copy.deepcopy(bootstrap["contracts"][7]["receipt_ref"])
        cases.append((cross_bound, shared))

        for bootstrap_value, shared_value in cases:
            with self.subTest(bootstrap=bootstrap_value, shared=shared_value):
                with self.assertRaises(CompositionContractError):
                    BootstrapLifecycleV1(ROOT).load_group_manifests(bootstrap_value, shared_value)

    def test_lifecycle_fixture_is_strict_and_contains_no_acceptance_claim(self):
        self.assertEqual(
            set(self.fixture),
            {"schema", "run_id", "workflow_version", "section", "groups", "qualified_ids", "retained_receipts"},
        )
        self.assertEqual(self.fixture["schema"], "s1-lifecycle-cases/v1")
        self.assertNotIn("accepted", self.fixture)
        self.assertNotIn("actual_a7", self.fixture)


class LifecycleIntegrationTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.store_root = Path(self.temp.name)
        self.fixture = json.loads(
            (ROOT / "tests" / "fixtures" / "s1" / "lifecycle-cases.json").read_text()
        )
        self.authority = {
            "approved": True,
            "scopes": [
                "entry", "publish_artifact", "close_epoch", "close_group", "open_epoch",
                "source", "open_section", "open_group", "publish_task_package", "claim_task",
                "release_task", "replan_task", "accept_task_result",
            ],
            "write_scopes": ["source"],
            "protected_fields": ["section", "group", "epoch", "ready", "head"],
            "human_receipt": "receipt-s1-fixture",
            "execution_class": "candidate-generic",
        }
        self.kernel = ControlKernel(self.store_root, self.fixture["run_id"])
        self.kernel.entry(
            {"path": "objectives/s1-fixture.md", "version": "v1", "digest": "a" * 64},
            workflow_version=self.fixture["workflow_version"],
            group_id="Bootstrap",
            epoch_id="Bootstrap-E1",
            authority_ref=self.authority,
        )
        self.source_refs = {
            "s0": self._attest_sources(
                "s0", [{"id": "S0.C1", "section_id": "S0", "group_id": "contracts-and-schema", "epoch_id": "S0-E1"}]
            ),
            "s1": self._attest_sources(
                "s1", [{"id": "S1.A1", "section_id": "S1", "group_id": "Bootstrap", "epoch_id": "S1-E1"}]
            ),
        }
        self.kernel.close_epoch(authority_ref=self.authority)
        self.kernel.close_group(next_group="contracts-and-schema", authority_ref=self.authority)
        self._open_and_accept_s0()
        self.lifecycle = BootstrapLifecycleV1(ROOT)
        self.protocol = SharedClosureProtocolV1()
        self.closure_refs = None

    def tearDown(self):
        self.temp.cleanup()

    def _attest_sources(self, label, tasks):
        values = {
            "plan": ("plan-" + label).encode(),
            "catalog": json.dumps(tasks, sort_keys=True, separators=(",", ":"), ensure_ascii=True).encode(),
            "checkpoint": ("checkpoint-" + label).encode(),
            "bundle": ("bundle-" + label).encode(),
            "lifecycle_prerequisite": ("lifecycle-" + label).encode(),
            "closed_group_receipt": ("receipt-" + label).encode(),
        }
        refs = {}
        for kind, content in values.items():
            path = self.store_root / ("source-" + label + "-" + kind)
            path.write_bytes(content)
            digest = "sha256:" + hashlib.sha256(content).hexdigest()
            self.kernel.publish_artifact(
                "attest-" + label + "-" + kind,
                "v1",
                {"ref_kind": kind, "source_digest": digest, "source_path": str(path)},
                kind="s0-source-attestation",
                authority_ref=self.authority,
            )
            refs[kind] = digest
        return refs

    def _open_and_accept_s0(self):
        document = json.loads((ROOT / "tests" / "fixtures" / "s0" / "contract-closed-bootstrap.json").read_text())
        refs = self.source_refs["s0"]
        head = self.kernel.head()
        document["expected_head"] = {"revision": head["revision"], "transaction_digest": head["transaction_digest"]}
        document["authority"].update({"expected_head": copy.deepcopy(document["expected_head"])})
        document["closed_bootstrap"].update({
            "run_id": self.fixture["run_id"],
            "plan_digest": refs["plan"],
            "catalog_digest": refs["catalog"],
            "checkpoint_ref": {"checkpoint_id": "checkpoint-s0", "digest": refs["checkpoint"]},
            "bundle_ref": {"digest": refs["bundle"]},
            "lifecycle_prerequisite": {"kind": "closed-bootstrap", "group_id": "Bootstrap", "receipt_digest": refs["lifecycle_prerequisite"]},
            "closed_group_receipt": {"group_id": "Bootstrap", "digest": refs["closed_group_receipt"]},
        })
        document["plan"]["digest"] = refs["plan"]
        document["catalog"]["digest"] = refs["catalog"]
        open_section(self.kernel, SectionControlPlaneV1().compile_registry(document)["open_section"])
        self.kernel.publish_artifact(
            "section-result-S0-contracts",
            "v1",
            {"schema": "section-accepted-result/v1", "run_id": self.fixture["run_id"], "section_id": "S0", "group_id": "contracts-and-schema", "result": "passed"},
            kind="section-accepted-result",
            authority_ref=self.authority,
        )
        evidence = self.kernel.read_state()["artifacts"]["section-result-S0-contracts"]["digest"]
        self.kernel.close_epoch(authority_ref=self.authority)
        self.kernel.close_group(next_group="S1", acceptance_evidence=[evidence], authority_ref=self.authority)

    def _s1_plan(self):
        state = self.kernel.read_state()
        parent = state["metadata"]["section_control"]
        head = self.kernel.head()
        refs = self.source_refs["s1"]
        expected_head = {"revision": head["revision"], "transaction_digest": head["transaction_digest"]}
        return {
            "schema": "section-plan/v1",
            "section": copy.deepcopy(self.fixture["section"]),
            "groups": [{"id": "Bootstrap", "depends_on": [], "first_epoch": "S1-E1", "first_frontier": ["S1.A1"]}],
            "accepted_section": {
                "run_id": self.fixture["run_id"], "workflow_version": self.fixture["workflow_version"],
                "section_id": "S0", "status": "paused_after_group", "ready": [], "next_section": "S1", "clear_boundary": True,
                "plan_digest": refs["plan"], "catalog_digest": refs["catalog"],
                "checkpoint_ref": copy.deepcopy(state["group"]["checkpoint_ref"]), "bundle_ref": copy.deepcopy(state["group"]["bundle_ref"]),
                "lifecycle_prerequisite": {"kind": "accepted-section", "section_id": "S0", "receipt_digest": parent["accepted_section_receipt"]["digest"]},
                "accepted_section_receipt": copy.deepcopy(parent["accepted_section_receipt"]),
                "section_history": copy.deepcopy(parent.get("section_history", [])),
            },
            "expected_head": expected_head,
            "authority": {
                "approved": True, "scopes": ["source", "open_section", "open_group"],
                "protected_fields": ["section", "group", "epoch", "ready"],
                "human_receipt": "receipt-s1-fixture", "expected_head": copy.deepcopy(expected_head),
                "execution_class": "candidate-generic",
            },
            "plan": {"digest": refs["plan"]},
            "catalog": {"digest": refs["catalog"], "tasks": [{"id": "S1.A1", "section_id": "S1", "group_id": "Bootstrap", "epoch_id": "S1-E1"}]},
            "idempotency_key": "open-s1-bootstrap",
        }

    @staticmethod
    def _head(kernel):
        head = kernel.head()
        return {"revision": head["revision"], "transaction_digest": head["transaction_digest"]}

    def _closure_authority(self, operation):
        return {
            "approved": True,
            "scopes": ["source", operation],
            "write_scopes": ["source"],
            "protected_fields": ["head", "group", "epoch", "ready"],
            "human_receipt": "receipt-s1-fixture",
            "expected_head": self._head(self.kernel),
            "assignment_id": "shared-closure-protocol",
            "run_id": self.fixture["run_id"],
            "workflow_version": self.fixture["workflow_version"],
        }

    def _ensure_closure_refs(self):
        if self.closure_refs is not None:
            return self.closure_refs
        refs = []
        for number in range(1, 6):
            artifact_id = "closure-result-F%d" % number
            self.kernel.publish_artifact(
                artifact_id,
                "v1",
                {"selector": "group.F.F%d" % number, "status": "compiled"},
                kind="closure-result",
                authority_ref=self.authority,
            )
            refs.append({"selector": "group.F.F%d" % number, "digest": self.kernel.read_state()["artifacts"][artifact_id]["digest"]})
        self.closure_refs = refs
        return refs

    def _close_epoch_with_f6(self, label):
        refs = self._ensure_closure_refs()
        head = self._head(self.kernel)
        authority = self._closure_authority("close_epoch")
        compiled = self.protocol.compile(
            "write_checkpoint",
            "group.F.F6",
            {
                "run_id": self.fixture["run_id"],
                "workflow_version": self.fixture["workflow_version"],
                "group_id": self.kernel.read_state()["group"]["id"],
                "idempotency_key": "f6-close-" + label,
                "closure_refs": refs,
            },
            authority,
            head,
        )
        return self.lifecycle.apply_closure_result(self.kernel, compiled)

    def _publish_section_result(self, artifact_id, group_id):
        self.kernel.publish_artifact(
            artifact_id,
            "v1",
            {"schema": "section-accepted-result/v1", "run_id": self.fixture["run_id"], "section_id": "S1", "group_id": group_id, "result": "passed"},
            kind="section-accepted-result",
            authority_ref=self.authority,
        )
        return self.kernel.read_state()["artifacts"][artifact_id]["digest"]

    def _close_group_with_f7(self, label, evidence_digest):
        head = self._head(self.kernel)
        checkpoint = self.kernel.read_state()["epoch"]["checkpoint_ref"]
        authority = self._closure_authority("close_group")
        compiled = self.protocol.compile(
            "clear_boundary",
            "group.F.F7",
            {
                "run_id": self.fixture["run_id"],
                "workflow_version": self.fixture["workflow_version"],
                "group_id": self.kernel.read_state()["group"]["id"],
                "idempotency_key": "f7-close-" + label,
                "close_receipt": {"status": "accepted", "head": head, "clear_before_next": True, "digest": evidence_digest},
                "checkpoint_ref": {"digest": checkpoint["digest"]},
            },
            authority,
            head,
        )
        return self.lifecycle.apply_closure_result(self.kernel, compiled)

    def _synthetic_group_plan(self, refs):
        state = self.kernel.read_state()
        section = state["metadata"]["section_control"]
        head = self._head(self.kernel)
        return {
            "schema": "section-plan/v1",
            "section": copy.deepcopy(self.fixture["section"]),
            "groups": [{"id": "Synthetic", "depends_on": ["Bootstrap"], "first_epoch": "S1-E3", "first_frontier": ["S1.Z1"]}],
            "closed_bootstrap": {
                "run_id": self.fixture["run_id"], "workflow_version": self.fixture["workflow_version"],
                "group_id": "Bootstrap", "status": "paused_after_group", "ready": [], "next_group": "Synthetic", "clear_boundary": True,
                "plan_digest": refs["plan"], "catalog_digest": refs["catalog"],
                "checkpoint_ref": {"checkpoint_id": "checkpoint-s1-bootstrap", "digest": refs["checkpoint"]},
                "bundle_ref": {"digest": refs["bundle"]},
                "lifecycle_prerequisite": {"kind": "accepted-section", "section_id": "S1", "receipt_digest": section["accepted_section_receipt"]["digest"]},
                "closed_group_receipt": {"group_id": "Bootstrap", "digest": refs["closed_group_receipt"]},
            },
            "expected_head": head,
            "authority": {
                "approved": True, "scopes": ["source", "open_section", "open_group"],
                "protected_fields": ["section", "group", "epoch", "ready"],
                "human_receipt": "receipt-s1-fixture", "expected_head": copy.deepcopy(head),
                "execution_class": "candidate-generic",
            },
            "plan": {"digest": refs["plan"]},
            "catalog": {"digest": refs["catalog"], "tasks": [{"id": "S1.Z1", "section_id": "S1", "group_id": "Synthetic", "epoch_id": "S1-E3"}]},
            "idempotency_key": "open-s1-synthetic",
        }

    def test_opens_s1_from_the_canonical_accepted_s0_parent_in_one_transaction(self):
        before = self.kernel.head()

        receipt = self.lifecycle.open_s1(self.kernel, self._s1_plan())

        state = self.kernel.read_state()
        self.assertEqual(receipt["revision"], before["revision"] + 1)
        self.assertEqual(state["metadata"]["section_control"]["section_id"], "S1")
        self.assertEqual(state["group"]["id"], "Bootstrap")
        self.assertEqual([(item["position"], item["section_id"]) for item in state["metadata"]["section_control"]["section_history"]], [(0, "S0")])

    def test_s1_open_rejects_missing_wrong_or_forged_s0_parent_without_mutation(self):
        before_head = self.kernel.head()
        before_state = self.kernel.read_state()
        cases = []
        missing_parent = self._s1_plan()
        missing_parent.pop("accepted_section")
        cases.append((missing_parent, SectionContractError))
        wrong_parent = self._s1_plan()
        wrong_parent["accepted_section"]["section_id"] = "S9"
        cases.append((wrong_parent, SectionContractError))
        forged_digest = self._s1_plan()
        forged_digest["accepted_section"]["accepted_section_receipt"]["digest"] = "sha256:" + "0" * 64
        forged_digest["accepted_section"]["lifecycle_prerequisite"]["receipt_digest"] = "sha256:" + "0" * 64
        cases.append((forged_digest, SectionContractError))

        for plan, error in cases:
            with self.subTest(error=error.__name__):
                with self.assertRaises(error):
                    self.lifecycle.open_s1(self.kernel, plan)
                self.assertEqual(self.kernel.head(), before_head)
                self.assertEqual(self.kernel.read_state(), before_state)

    def test_s1_open_retry_conflict_and_stale_bindings_preserve_canonical_state(self):
        plan = self._s1_plan()
        before_head = self.kernel.head()
        before_state = self.kernel.read_state()
        stale_authority = copy.deepcopy(plan)
        stale_authority["authority"]["expected_head"]["revision"] -= 1
        with self.assertRaises(SectionContractError):
            self.lifecycle.open_s1(self.kernel, stale_authority)
        self.assertEqual(self.kernel.head(), before_head)
        self.assertEqual(self.kernel.read_state(), before_state)

        first = self.lifecycle.open_s1(self.kernel, plan)
        committed_head = self.kernel.head()
        committed_state = self.kernel.read_state()
        self.assertEqual(
            self.lifecycle.open_s1(self.kernel, copy.deepcopy(plan)),
            {
                "duplicate": True,
                "transaction_digest": first["transaction_digest"],
                "revision": first["revision"],
            },
        )
        changed_payload = copy.deepcopy(plan)
        changed_payload["authority"]["human_receipt"] = "receipt-s1-changed"
        with self.assertRaises(DuplicateCommandError):
            self.lifecycle.open_s1(self.kernel, changed_payload)
        stale_head = copy.deepcopy(plan)
        stale_head["idempotency_key"] = "open-s1-bootstrap-stale"
        with self.assertRaises(StaleHeadError):
            self.lifecycle.open_s1(self.kernel, stale_head)
        self.assertEqual(self.kernel.head(), committed_head)
        self.assertEqual(self.kernel.read_state(), committed_state)

    def test_group_close_refuses_an_open_s1_epoch_without_mutation(self):
        self.lifecycle.open_s1(self.kernel, self._s1_plan())
        before_head = self.kernel.head()
        before_state = self.kernel.read_state()

        with self.assertRaisesRegex(KernelError, "Group requires a closed Epoch"):
            self.kernel.close_group(
                next_group="Synthetic",
                authority_ref=self.authority,
                idempotency_key="refuse-open-s1-epoch",
            )

        self.assertEqual(self.kernel.head(), before_head)
        self.assertEqual(self.kernel.read_state(), before_state)

    def test_f6_f7_drive_multiple_epochs_second_group_and_cold_resume(self):
        self.lifecycle.open_s1(self.kernel, self._s1_plan())
        synthetic_refs = self._attest_sources(
            "synthetic", [{"id": "S1.Z1", "section_id": "S1", "group_id": "Synthetic", "epoch_id": "S1-E3"}]
        )
        self.kernel.publish_artifact(
            "A-history",
            "v1",
            {"qualified_ids": ["group.A.A1", "group.A.A6", "group.A.A6R", "group.A.A7"]},
            kind="bootstrap-history",
            authority_ref=self.authority,
        )
        self._close_epoch_with_f6("bootstrap-e1")
        first_checkpoint = copy.deepcopy(self.kernel.read_state()["epoch"]["checkpoint_ref"])
        self.kernel.open_epoch("S1-E2", first_checkpoint, authority_ref=self.authority, idempotency_key="open-s1-e2")
        bootstrap_evidence = self._publish_section_result("section-result-S1-Bootstrap", "Bootstrap")
        self._close_epoch_with_f6("bootstrap-e2")
        self._close_group_with_f7("bootstrap", bootstrap_evidence)

        retained_history = copy.deepcopy(self.kernel.read_state()["artifacts"]["A-history"])
        self.lifecycle.open_next_group(self.kernel, self._synthetic_group_plan(synthetic_refs))
        synthetic_evidence = self._publish_section_result("section-result-S1-Synthetic", "Synthetic")
        self._close_epoch_with_f6("synthetic-e3")
        self._close_group_with_f7("synthetic", synthetic_evidence)

        self.assertEqual(self.kernel.read_state()["artifacts"]["A-history"], retained_history)
        expected_head = self._head(self.kernel)
        self.kernel.projection_dir.joinpath("status.json").write_text("forged")
        self.kernel.projection_dir.joinpath("ready.json").unlink()
        cold = ControlKernel(self.store_root, self.fixture["run_id"])
        resumed = self.lifecycle.resume(cold, expected_head)
        self.assertEqual(resumed["head"]["revision"], expected_head["revision"])
        self.assertEqual(resumed["section_id"], "S1")
        self.assertEqual(resumed["group"]["id"], "Synthetic")
        self.assertTrue(resumed["claims"]["source_transition_fixture_passed"])
        self.assertTrue(self.kernel.projection_dir.joinpath("ready.json").is_file())


if __name__ == "__main__":
    unittest.main()
