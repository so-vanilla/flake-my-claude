import copy
import hashlib
import json
import sys
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "src"))

from ai_agent_workflow.control_kernel import (  # noqa: E402
    AuthorizationError,
    CommandValidationError,
    ControlKernel,
    DuplicateCommandError,
    InjectedCrash,
    IntegrityBlockedError,
    LifecycleClosedError,
    StaleHeadError,
    canonical_digest,
)
from ai_agent_workflow.section_control_plane import (  # noqa: E402
    SectionControlPlaneV1,
    open_group,
    open_section,
    resume,
    status,
)
from ai_agent_workflow.schema_validation import SchemaValidationError  # noqa: E402


class SectionControlPlaneKernelTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name)
        self.kernel = ControlKernel(self.root, "run-fixture")
        self.authority = {
            "approved": True,
            "scopes": ["entry", "publish_artifact", "close_epoch", "close_group", "source", "open_section", "open_group"],
            "protected_fields": ["section", "group", "epoch", "ready"],
            "human_receipt": "receipt-fixture",
            "execution_class": "candidate-generic",
        }
        self.kernel.entry({"path": "objectives/source-fixture.md", "version": "v1", "digest": "a" * 64}, workflow_version="workflow/v1", group_id="Bootstrap", epoch_id="Bootstrap-E1", authority_ref=self.authority)
        self.source_refs = {
            "section": self._attest_sources("section", [{"id": "S0.C1", "section_id": "S0", "group_id": "contracts-and-schema", "epoch_id": "S0-E1"}]),
            "later": self._attest_sources("later", [{"id": "S0.D1", "section_id": "S0", "group_id": "Planning", "epoch_id": "S0-E2"}]),
            "s1": self._attest_sources("s1", [{"id": "S1.A1", "section_id": "S1", "group_id": "analysis", "epoch_id": "S1-E1"}]),
            "s2": self._attest_sources("s2", [{"id": "S2.A1", "section_id": "S2", "group_id": "analysis", "epoch_id": "S2-E1"}]),
        }
        wrong_bytes = b"wrong-kind-source"
        self.wrong_kind_path = self.root / "source-wrong-kind"
        self.wrong_kind_path.write_bytes(wrong_bytes)
        self.wrong_kind_digest = "sha256:" + hashlib.sha256(wrong_bytes).hexdigest()
        self.kernel.publish_artifact("attest-wrong-kind", "v1", {"ref_kind": "bundle", "source_digest": self.wrong_kind_digest, "source_path": str(self.wrong_kind_path)}, kind="s0-source-attestation", authority_ref=self.authority)
        self.kernel.close_epoch(authority_ref=self.authority)
        self.kernel.close_group(next_group="contracts-and-schema", authority_ref=self.authority)

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
            path = self.root / ("source-" + label + "-" + kind)
            path.write_bytes(content)
            digest = "sha256:" + hashlib.sha256(content).hexdigest()
            self.kernel.publish_artifact(
                "attest-" + label + "-" + kind, "v1",
                {"ref_kind": kind, "source_digest": digest, "source_path": str(path)},
                kind="s0-source-attestation", authority_ref=self.authority,
            )
            refs[kind] = digest
        return refs

    def _command(self, *, later=False, canonical_receipt=True):
        name = "contract-open-group.json" if later else "contract-closed-bootstrap.json"
        fixture = json.loads((ROOT / "tests" / "fixtures" / "s0" / name).read_text())
        head = self.kernel.head()
        fixture["expected_head"] = {"revision": head["revision"], "transaction_digest": head["transaction_digest"]}
        fixture["authority"]["expected_head"] = copy.deepcopy(fixture["expected_head"])
        fixture["closed_bootstrap"]["run_id"] = "run-fixture"
        refs = copy.deepcopy(self.source_refs["later" if later else "section"])
        if later and canonical_receipt:
            refs["lifecycle_prerequisite"] = self.kernel.read_state()["metadata"]["section_control"]["accepted_section_receipt"]["digest"]
        fixture["closed_bootstrap"]["plan_digest"] = refs["plan"]
        fixture["closed_bootstrap"]["catalog_digest"] = refs["catalog"]
        fixture["closed_bootstrap"]["checkpoint_ref"]["digest"] = refs["checkpoint"]
        fixture["closed_bootstrap"]["bundle_ref"]["digest"] = refs["bundle"]
        fixture["closed_bootstrap"]["lifecycle_prerequisite"]["receipt_digest"] = refs["lifecycle_prerequisite"]
        fixture["closed_bootstrap"]["closed_group_receipt"]["digest"] = refs["closed_group_receipt"]
        fixture["plan"]["digest"] = refs["plan"]
        fixture["catalog"]["digest"] = refs["catalog"]
        if later:
            fixture["closed_bootstrap"]["group_id"] = "contracts-and-schema"
            fixture["closed_bootstrap"]["closed_group_receipt"]["group_id"] = "contracts-and-schema"
            fixture["closed_bootstrap"]["next_group"] = "Planning"
            fixture["groups"][0]["depends_on"] = ["contracts-and-schema"]
        return (SectionControlPlaneV1().compile_open_group(fixture) if later else SectionControlPlaneV1().compile_registry(fixture)["open_section"])

    def _accepted_result(self, *, section_id="S0", group_id="contracts-and-schema", result="passed"):
        artifact_id = "section-result-%s-%s" % (section_id, group_id)
        self.kernel.publish_artifact(
            artifact_id,
            "v1",
            {
                "schema": "section-accepted-result/v1",
                "run_id": "run-fixture",
                "section_id": section_id,
                "group_id": group_id,
                "result": result,
            },
            kind="section-accepted-result",
            authority_ref=self.authority,
        )
        return self.kernel.read_state()["artifacts"][artifact_id]["digest"]

    def _accept_current_section(self, *, section_id, group_id, next_section):
        evidence = self._accepted_result(section_id=section_id, group_id=group_id)
        self.kernel.close_epoch(authority_ref=self.authority)
        self.kernel.close_group(
            next_group=next_section,
            acceptance_evidence=[evidence],
            authority_ref=self.authority,
        )
        return self.kernel.read_state()

    def _successor_command(self, target_section):
        """Compile a successor command using only the current canonical HEAD."""
        position = int(target_section[1:])
        current = self.kernel.read_state()
        prior = current["metadata"]["section_control"]
        head = self.kernel.head()
        refs = self.source_refs[target_section.lower()]
        fixture = json.loads((ROOT / "tests" / "fixtures" / "s1" / "contract-accepted-s0.json").read_text())
        fixture["section"] = {"id": target_section, "workflow_id": "workflow-main", "version": "v1"}
        fixture["groups"] = [{"id": "analysis", "depends_on": [], "first_epoch": target_section + "-E1", "first_frontier": [target_section + ".A1"]}]
        fixture["accepted_section"] = {
            "run_id": "run-fixture",
            "workflow_version": "workflow/v1",
            "section_id": "S%d" % (position - 1),
            "status": "paused_after_group",
            "ready": [],
            "next_section": target_section,
            "clear_boundary": True,
            "plan_digest": refs["plan"],
            "catalog_digest": refs["catalog"],
            "checkpoint_ref": current["group"]["checkpoint_ref"],
            "bundle_ref": current["group"]["bundle_ref"],
            "lifecycle_prerequisite": {"kind": "accepted-section", "section_id": "S%d" % (position - 1), "receipt_digest": prior["accepted_section_receipt"]["digest"]},
            "accepted_section_receipt": prior["accepted_section_receipt"],
            "section_history": copy.deepcopy(prior.get("section_history", [])),
        }
        fixture["expected_head"] = {"revision": head["revision"], "transaction_digest": head["transaction_digest"]}
        fixture["authority"]["expected_head"] = copy.deepcopy(fixture["expected_head"])
        fixture["plan"]["digest"] = refs["plan"]
        fixture["catalog"] = {"digest": refs["catalog"], "tasks": [{"id": target_section + ".A1", "section_id": target_section, "group_id": "analysis", "epoch_id": target_section + "-E1"}]}
        fixture["idempotency_key"] = "open-" + target_section.lower() + "-analysis"
        return SectionControlPlaneV1().compile_registry(fixture)["open_section"]

    def _accepted_s0(self):
        open_section(self.kernel, self._command())
        return self._accept_current_section(section_id="S0", group_id="contracts-and-schema", next_section="S1")

    def _accepted_s0_s1_s2(self):
        self._accepted_s0()
        open_section(self.kernel, self._successor_command("S1"))
        self._accept_current_section(section_id="S1", group_id="analysis", next_section="S2")
        open_section(self.kernel, self._successor_command("S2"))

    def _same_section_s1_group_command(self):
        self._accepted_s0()
        open_section(self.kernel, self._successor_command("S1"))
        refs = self._attest_sources(
            "s1-review",
            [{"id": "S1.R1", "section_id": "S1", "group_id": "review", "epoch_id": "S1-E2"}],
        )
        self._accept_current_section(section_id="S1", group_id="analysis", next_section="review")
        state = self.kernel.read_state()
        section = state["metadata"]["section_control"]
        head = self.kernel.head()
        plan = {
            "schema": "section-plan/v1",
            "section": {"id": "S1", "workflow_id": "workflow-main", "version": "v1"},
            "groups": [{
                "id": "review", "depends_on": ["analysis"], "first_epoch": "S1-E2",
                "first_frontier": ["S1.R1"],
            }],
            "closed_bootstrap": {
                "run_id": "run-fixture", "workflow_version": "workflow/v1",
                "group_id": "analysis", "status": "paused_after_group", "ready": [],
                "next_group": "review", "clear_boundary": True,
                "plan_digest": refs["plan"], "catalog_digest": refs["catalog"],
                "checkpoint_ref": {
                    "checkpoint_id": "checkpoint-s1-analysis",
                    "digest": refs["checkpoint"],
                },
                "bundle_ref": {"digest": refs["bundle"]},
                "lifecycle_prerequisite": {
                    "kind": "accepted-section", "section_id": "S1",
                    "receipt_digest": section["accepted_section_receipt"]["digest"],
                },
                "closed_group_receipt": {"group_id": "analysis", "digest": refs["closed_group_receipt"]},
            },
            "expected_head": {"revision": head["revision"], "transaction_digest": head["transaction_digest"]},
            "authority": {
                "approved": True, "scopes": ["source", "open_section", "open_group"],
                "protected_fields": ["section", "group", "epoch", "ready"],
                "human_receipt": "receipt-fixture",
                "expected_head": {"revision": head["revision"], "transaction_digest": head["transaction_digest"]},
                "execution_class": "candidate-generic",
            },
            "plan": {"digest": refs["plan"]},
            "catalog": {"digest": refs["catalog"], "tasks": [{
                "id": "S1.R1", "section_id": "S1", "group_id": "review", "epoch_id": "S1-E2",
            }]},
            "idempotency_key": "open-s1-review",
        }
        return SectionControlPlaneV1().compile_open_group(plan)

    def _replace_current_transaction_state_for_test(self, mutate):
        """Install a digest-consistent forged HEAD transaction for a read-path test."""
        head = self.kernel.head()
        transaction = json.loads(self.kernel._transaction_path(head["transaction_digest"]).read_text())
        mutate(transaction["state"])
        parent = transaction["parent"]
        parent_state = json.loads(
            self.kernel._transaction_path(parent["transaction_digest"]).read_text()
        )["state"]
        transaction["graph_delta"] = self.kernel._graph_delta(parent_state, transaction["state"])
        transaction["digest"] = canonical_digest({key: value for key, value in transaction.items() if key != "digest"})
        self.kernel._transaction_path(transaction["digest"]).write_text(
            json.dumps(transaction, sort_keys=True, separators=(",", ":"), ensure_ascii=True) + "\n"
        )
        forged_head = copy.deepcopy(head)
        forged_head["transaction_digest"] = transaction["digest"]
        forged_head["digest"] = canonical_digest({key: value for key, value in forged_head.items() if key != "digest"})
        self.kernel.head_path.write_text(
            json.dumps(forged_head, sort_keys=True, separators=(",", ":"), ensure_ascii=True) + "\n"
        )

    def _assert_fresh_resume_blocks_history_substitution(self, field):
        self._accepted_s0_s1_s2()

        def substitute(state):
            history = state["metadata"]["section_control"]["section_history"]
            if field == "accepted_receipt_ref":
                history[0][field], history[1][field] = (
                    copy.deepcopy(history[1][field]),
                    copy.deepcopy(history[0][field]),
                )
            else:
                history[0][field] = copy.deepcopy(history[1][field])

        self._replace_current_transaction_state_for_test(substitute)
        forged_projection = '{"forged":true}\n'
        status_path = self.kernel.projection_dir / "status.json"
        status_path.write_text(forged_projection)
        fresh = ControlKernel(self.root, "run-fixture")
        with self.assertRaisesRegex(IntegrityBlockedError, "section history accepted receipt payload is not bound"):
            fresh.resume()
        self.assertEqual(status_path.read_text(), forged_projection)

    def test_fresh_resume_blocks_cross_section_history_receipt_substitution(self):
        self._assert_fresh_resume_blocks_history_substitution("accepted_receipt_ref")

    def test_fresh_resume_blocks_cross_section_history_bundle_substitution(self):
        self._assert_fresh_resume_blocks_history_substitution("bundle_ref")

    def test_fresh_resume_blocks_cross_section_history_checkpoint_substitution(self):
        self._assert_fresh_resume_blocks_history_substitution("checkpoint_ref")

    def test_fresh_resume_blocks_catalogued_historical_receipt_as_current_accepted_section(self):
        self._accepted_s0()
        open_section(self.kernel, self._successor_command("S1"))
        self._accept_current_section(section_id="S1", group_id="analysis", next_section="S2")
        section = self.kernel.read_state()["metadata"]["section_control"]
        historical_receipt = copy.deepcopy(section["section_history"][0]["accepted_receipt_ref"])
        self.assertNotEqual(historical_receipt, section["accepted_section_receipt"])

        def substitute(state):
            state["metadata"]["section_control"]["accepted_section_receipt"] = historical_receipt

        self._replace_current_transaction_state_for_test(substitute)
        forged_projection = '{"forged":true}\n'
        status_path = self.kernel.projection_dir / "status.json"
        status_path.write_text(forged_projection)
        fresh = ControlKernel(self.root, "run-fixture")
        with self.assertRaisesRegex(
            IntegrityBlockedError,
            "current accepted section receipt payload is not bound",
        ):
            fresh.resume()
        self.assertEqual(status_path.read_text(), forged_projection)

    def test_fresh_resume_blocks_catalogued_noncanonical_parent_accepted_receipt(self):
        self._accepted_s0()
        open_section(self.kernel, self._successor_command("S1"))
        self._accept_current_section(section_id="S1", group_id="analysis", next_section="S2")
        section = self.kernel.read_state()["metadata"]["section_control"]
        canonical_parent = copy.deepcopy(section["parent_accepted_section_receipt"])
        noncanonical_parent = copy.deepcopy(section["accepted_section_receipt"])
        self.assertEqual(canonical_parent, section["section_history"][-1]["accepted_receipt_ref"])
        self.assertNotEqual(noncanonical_parent, canonical_parent)

        def substitute(state):
            state["metadata"]["section_control"]["parent_accepted_section_receipt"] = noncanonical_parent

        self._replace_current_transaction_state_for_test(substitute)
        forged_projection = '{"forged":true}\n'
        status_path = self.kernel.projection_dir / "status.json"
        status_path.write_text(forged_projection)
        fresh = ControlKernel(self.root, "run-fixture")
        with self.assertRaisesRegex(
            IntegrityBlockedError,
            "parent accepted section receipt payload is not bound",
        ):
            fresh.resume()
        self.assertEqual(status_path.read_text(), forged_projection)

    def test_compiled_accepted_s0_successor_opens_s1_and_archives_only_canonical_history(self):
        self._accepted_s0()
        command = self._successor_command("S1")
        before = self.kernel.head()
        predecessor = self.kernel.read_state()
        predecessor_section = predecessor["metadata"]["section_control"]

        receipt = open_section(self.kernel, command)

        state = self.kernel.read_state()
        self.assertEqual(receipt["revision"], before["revision"] + 1)
        self.assertEqual(state["metadata"]["section_control"]["section_id"], "S1")
        self.assertEqual(state["metadata"]["section_control"]["groups"], {"analysis": {"epoch_id": "S1-E1", "frontier": ["S1.A1"]}})
        self.assertEqual(state["metadata"]["section_control"]["section_history"], [{
            "position": 0, "section_id": "S0",
            "accepted_receipt_ref": predecessor_section["accepted_section_receipt"],
            "bundle_ref": predecessor["group"]["bundle_ref"],
            "checkpoint_ref": predecessor["group"]["checkpoint_ref"],
        }])
        self.assertEqual(self.kernel.ready_tasks(), ["S1.A1"])

    def test_successor_rejections_leave_head_state_and_ready_unchanged(self):
        self._accepted_s0()
        command = self._successor_command("S1")
        before, state, ready = self.kernel.head(), self.kernel.read_state(), self.kernel.ready_tasks()
        cases = []
        stale = copy.deepcopy(command); stale["expected_head"]["revision"] -= 1; stale["authority_ref"]["expected_head"] = copy.deepcopy(stale["expected_head"]); cases.append(stale)
        wrong_receipt = copy.deepcopy(command); wrong_receipt["input_refs"][-1]["digest"] = self.source_refs["s1"]["lifecycle_prerequisite"]; wrong_receipt["payload"]["preconditions"]["accepted_section_receipt_digest"] = wrong_receipt["input_refs"][-1]["digest"]; cases.append(wrong_receipt)
        skip = copy.deepcopy(command); skip["payload"]["section"]["id"] = "S2"; cases.append(skip)
        reopen = copy.deepcopy(command); reopen["payload"]["section"]["id"] = "S0"; cases.append(reopen)
        for value in cases:
            with self.subTest(value=value["payload"]["section"]["id"]):
                with self.assertRaises((AuthorizationError, CommandValidationError, LifecycleClosedError, SchemaValidationError, StaleHeadError)):
                    open_section(self.kernel, value)
                self.assertEqual(self.kernel.head(), before)
                self.assertEqual(self.kernel.read_state(), state)
                self.assertEqual(self.kernel.ready_tasks(), ready)

    def test_successor_exact_retry_projection_rebuild_and_cold_process_are_canonical(self):
        self._accepted_s0()
        command = self._successor_command("S1")
        first = open_section(self.kernel, command)
        self.assertEqual(open_section(self.kernel, copy.deepcopy(command)), {"duplicate": True, "transaction_digest": first["transaction_digest"], "revision": first["revision"]})
        changed = copy.deepcopy(command)
        changed["payload"]["first_frontier"] = ["S1.B1"]
        with self.assertRaises(DuplicateCommandError):
            open_section(self.kernel, changed)
        self.kernel.projection_dir.joinpath("status.json").unlink()
        current = self.kernel.head()
        expected = resume(self.kernel, {"revision": current["revision"], "transaction_digest": current["transaction_digest"]})
        cold = ControlKernel(self.root, "run-fixture")
        self.assertEqual(resume(cold, {"revision": current["revision"], "transaction_digest": current["transaction_digest"]}), expected)

    def test_successor_forged_projection_rebuilds_and_canonical_transaction_corruption_blocks(self):
        self._accepted_s0()
        open_section(self.kernel, self._successor_command("S1"))
        head = self.kernel.head()
        expected = resume(self.kernel, {"revision": head["revision"], "transaction_digest": head["transaction_digest"]})
        self.kernel.projection_dir.joinpath("status.json").write_text("forged")
        self.assertEqual(resume(self.kernel, {"revision": head["revision"], "transaction_digest": head["transaction_digest"]}), expected)
        self.kernel._transaction_path(head["transaction_digest"]).write_text("{}")
        with self.assertRaises(IntegrityBlockedError):
            ControlKernel(self.root, "run-fixture").read_state()

    def test_representative_s1_to_s2_preserves_s0_and_s1_history(self):
        self._accepted_s0_s1_s2()
        history = self.kernel.read_state()["metadata"]["section_control"]["section_history"]
        self.assertEqual([(item["position"], item["section_id"]) for item in history], [(0, "S0"), (1, "S1")])

    def test_direct_s1_same_section_open_group_preserves_history_and_kernel_controls(self):
        command = self._same_section_s1_group_command()
        before = self.kernel.head()
        before_state = self.kernel.read_state()
        prior = copy.deepcopy(before_state["metadata"]["section_control"])
        stale = copy.deepcopy(command)
        stale["expected_head"]["revision"] -= 1
        stale["authority_ref"]["expected_head"] = copy.deepcopy(stale["expected_head"])
        wrong_receipt = copy.deepcopy(command)
        for ref in wrong_receipt["input_refs"]:
            if ref["kind"] == "closed_group_receipt":
                ref["digest"] = "sha256:" + "9" * 64
        wrong_receipt["payload"]["preconditions"]["closed_group_receipt_digest"] = "sha256:" + "9" * 64
        for rejected in (stale, wrong_receipt):
            with self.assertRaises((AuthorizationError, SchemaValidationError, StaleHeadError)):
                self.kernel.apply(rejected)
            self.assertEqual(self.kernel.head(), before)
            self.assertEqual(self.kernel.read_state(), before_state)
        with self.kernel.fault("after_publish_before_head"):
            with self.assertRaises(InjectedCrash):
                self.kernel.apply(command)
        self.assertEqual(self.kernel.head(), before)
        self.assertEqual(self.kernel.read_state(), before_state)
        self.assertTrue(self.kernel.recover()["orphan_transactions"])

        receipt = self.kernel.apply(command)
        state = self.kernel.read_state()
        self.assertEqual(receipt["revision"], before["revision"] + 1)
        self.assertEqual(state["metadata"]["section_control"]["groups"], {
            "analysis": {"epoch_id": "S1-E1", "frontier": ["S1.A1"]},
            "review": {"epoch_id": "S1-E2", "frontier": ["S1.R1"]},
        })
        self.assertEqual(state["metadata"]["section_control"]["section_history"], prior["section_history"])
        self.assertEqual(
            state["metadata"]["section_control"]["parent_accepted_section_receipt"],
            prior["accepted_section_receipt"],
        )
        self.assertEqual(self.kernel.ready_tasks(), ["S1.R1"])
        self.assertEqual(
            self.kernel.apply(copy.deepcopy(command)),
            {"duplicate": True, "transaction_digest": receipt["transaction_digest"], "revision": receipt["revision"]},
        )
        changed = copy.deepcopy(command)
        changed["payload"]["first_frontier"] = ["S1.A1"]
        with self.assertRaises(DuplicateCommandError):
            self.kernel.apply(changed)
        head = self.kernel.head()
        expected = resume(self.kernel, {"revision": head["revision"], "transaction_digest": head["transaction_digest"]})
        self.kernel.projection_dir.joinpath("status.json").write_text("forged")
        self.assertEqual(resume(self.kernel, {"revision": head["revision"], "transaction_digest": head["transaction_digest"]}), expected)
        self.assertEqual(
            resume(ControlKernel(self.root, "run-fixture"), {"revision": head["revision"], "transaction_digest": head["transaction_digest"]}),
            expected,
        )

    def test_open_section_is_one_atomic_head_transition_with_only_first_frontier_ready(self):
        command = self._command()
        before = self.kernel.head()

        receipt = open_section(self.kernel, command)

        self.assertEqual(receipt["revision"], before["revision"] + 1)
        state = self.kernel.read_state()
        self.assertEqual(state["group"], {"id": "contracts-and-schema", "status": "open", "next_group": None})
        self.assertEqual(state["epoch"]["id"], "S0-E1")
        self.assertEqual(self.kernel.ready_tasks(), ["S0.C1"])
        current_head = self.kernel.head()
        view = status(self.kernel, {"revision": current_head["revision"], "transaction_digest": current_head["transaction_digest"]})
        self.assertEqual(view["section_id"], "S0")
        self.assertFalse(view["claims"]["source_transition_fixture_passed"])
        self.assertFalse(view["claims"]["actual_a7"])
        self.assertFalse(view["claims"]["activation"])
        self.assertFalse(view["claims"]["full_ready"])

    def test_rejects_bad_section_transition_without_advancing_head_or_ready(self):
        command = self._command()
        before = self.kernel.head()
        cases = []
        stale = copy.deepcopy(command); stale["expected_head"]["revision"] -= 1; stale["authority_ref"]["expected_head"] = copy.deepcopy(stale["expected_head"]); cases.append((StaleHeadError, stale))
        wrong_actor = copy.deepcopy(command); wrong_actor["actor"]["role"] = "worker"; cases.append((SchemaValidationError, wrong_actor))
        forged = copy.deepcopy(command); forged["input_refs"][0]["digest"] = "sha256:" + "9" * 64; cases.append((SchemaValidationError, forged))
        wrong_scope = copy.deepcopy(command); wrong_scope["scope"] = ["source", "other"]; cases.append((AuthorizationError, wrong_scope))
        wrong_execution = copy.deepcopy(command); wrong_execution["authority_ref"]["execution_class"] = "pilot"; cases.append((AuthorizationError, wrong_execution))
        insufficient_protection = copy.deepcopy(command); insufficient_protection["protected_fields"] = []; insufficient_protection["authority_ref"]["protected_fields"] = []; cases.append((AuthorizationError, insufficient_protection))
        unknown_section = copy.deepcopy(command); unknown_section["payload"]["section"]["id"] = "S9"; cases.append((CommandValidationError, unknown_section))
        unknown_group = copy.deepcopy(command); unknown_group["payload"]["group"]["id"] = "unknown"; cases.append((LifecycleClosedError, unknown_group))
        for error, value in cases:
            with self.subTest(error=error.__name__):
                with self.assertRaises((error, SchemaValidationError)): open_section(self.kernel, value)
                self.assertEqual(self.kernel.head(), before)
                self.assertEqual(self.kernel.ready_tasks(), [])

    def test_rejects_self_consistent_unattested_source_references_before_head_mutation(self):
        command = self._command()
        fake = copy.deepcopy(command)
        fake_refs = {
            "plan": "sha256:" + "1" * 64,
            "catalog": "sha256:" + "2" * 64,
            "checkpoint": "sha256:" + "3" * 64,
            "bundle": "sha256:" + "4" * 64,
            "lifecycle_prerequisite": "sha256:" + "5" * 64,
            "closed_group_receipt": "sha256:" + "6" * 64,
        }
        for ref in fake["input_refs"]:
            ref["digest"] = fake_refs[ref["kind"]]
        for field, kind in {
            "plan_digest": "plan", "catalog_digest": "catalog", "checkpoint_digest": "checkpoint",
            "bundle_digest": "bundle", "lifecycle_prerequisite_digest": "lifecycle_prerequisite",
            "closed_group_receipt_digest": "closed_group_receipt",
        }.items():
            fake["payload"]["preconditions"][field] = fake_refs[kind]
        before = self.kernel.head()
        before_state, before_status = self.kernel.read_state(), self.kernel.resume()
        with self.assertRaises(AuthorizationError):
            open_section(self.kernel, fake)
        self.assertEqual(self.kernel.head(), before)
        self.assertEqual(self.kernel.read_state(), before_state)
        self.assertEqual(self.kernel.resume(), before_status)
        self.assertEqual(self.kernel.ready_tasks(), [])

    def test_rejects_single_self_consistent_and_wrong_kind_source_reference(self):
        command = self._command()
        before = self.kernel.head()
        before_state, before_status = self.kernel.read_state(), self.kernel.resume()
        single = copy.deepcopy(command)
        single["input_refs"][0]["digest"] = "sha256:" + "7" * 64
        single["payload"]["preconditions"]["plan_digest"] = single["input_refs"][0]["digest"]
        wrong_kind = copy.deepcopy(command)
        wrong_kind["input_refs"][0]["digest"] = self.wrong_kind_digest
        wrong_kind["payload"]["preconditions"]["plan_digest"] = self.wrong_kind_digest
        for value in (single, wrong_kind):
            with self.subTest(value=value["input_refs"][0]["digest"]):
                with self.assertRaises(AuthorizationError):
                    open_section(self.kernel, value)
                self.assertEqual(self.kernel.head(), before)
                self.assertEqual(self.kernel.read_state(), before_state)
                self.assertEqual(self.kernel.resume(), before_status)
                self.assertEqual(self.kernel.ready_tasks(), [])

    def test_open_group_rejects_a_physical_but_noncanonical_accepted_section_receipt(self):
        open_section(self.kernel, self._command())
        evidence = self._accepted_result()
        self.kernel.close_epoch(authority_ref=self.authority)
        self.kernel.close_group(next_group="Planning", acceptance_evidence=[evidence], authority_ref=self.authority)
        command = self._command(later=True)
        forged = copy.deepcopy(command)
        forged_digest = self.source_refs["later"]["lifecycle_prerequisite"]
        for ref in forged["input_refs"]:
            if ref["kind"] == "lifecycle_prerequisite":
                ref["digest"] = forged_digest
        forged["payload"]["preconditions"]["lifecycle_prerequisite_digest"] = forged_digest
        before = self.kernel.head()
        before_state, before_status = self.kernel.read_state(), self.kernel.resume()
        with self.assertRaises(AuthorizationError):
            open_group(self.kernel, forged)
        self.assertEqual(self.kernel.head(), before)
        self.assertEqual(self.kernel.read_state(), before_state)
        self.assertEqual(self.kernel.resume(), before_status)
        self.assertEqual(self.kernel.ready_tasks(), [])

    def test_exact_retry_is_canonical_and_changed_payload_conflicts(self):
        command = self._command()
        first = open_section(self.kernel, command)
        second = open_section(self.kernel, copy.deepcopy(command))
        self.assertEqual(second, {"duplicate": True, "transaction_digest": first["transaction_digest"], "revision": first["revision"]})
        changed = copy.deepcopy(command)
        changed["payload"]["first_frontier"] = ["S0.D1"]
        with self.assertRaises(DuplicateCommandError): open_section(self.kernel, changed)

    def test_pre_head_fault_leaves_no_visible_half_open_section_and_recovery_reports_orphan(self):
        command = self._command()
        before = self.kernel.head()
        with self.kernel.fault("after_publish_before_head"):
            with self.assertRaises(InjectedCrash): open_section(self.kernel, command)
        self.assertEqual(self.kernel.head(), before)
        self.assertEqual(self.kernel.ready_tasks(), [])
        self.assertTrue(self.kernel.recover()["orphan_transactions"])

    def test_open_group_requires_accepted_section_and_rebuilds_public_status(self):
        open_section(self.kernel, self._command())
        # The source fixture models the accepted child boundary; close the first
        # child to give the generic kernel an actual paused-after-group state.
        evidence = self._accepted_result()
        self.kernel.close_epoch(authority_ref=self.authority)
        self.kernel.close_group(next_group="Planning", acceptance_evidence=[evidence], authority_ref=self.authority)
        accepted_head = self.kernel.head()
        accepted = status(self.kernel, {"revision": accepted_head["revision"], "transaction_digest": accepted_head["transaction_digest"]})
        self.assertTrue(accepted["claims"]["source_transition_fixture_passed"])
        self.assertNotEqual(accepted["transition_evidence"]["receipt_digest"], accepted_head["transaction_digest"])
        command = self._command(later=True)
        open_group(self.kernel, command)
        self.assertEqual(self.kernel.read_state()["group"]["id"], "Planning")
        self.assertIn("parent_accepted_section_receipt", self.kernel.read_state()["metadata"]["section_control"])
        self.kernel.projection_dir.joinpath("status.json").write_text("forged")
        current_head = self.kernel.head()
        self.assertEqual(resume(self.kernel, {"revision": current_head["revision"], "transaction_digest": current_head["transaction_digest"]})["ready"], ["S0.D1"])

    def test_open_section_is_pending_until_digest_bound_close_acceptance_and_empty_close_cannot_open_group(self):
        open_section(self.kernel, self._command())
        current_head = self.kernel.head()
        pending = status(self.kernel, {"revision": current_head["revision"], "transaction_digest": current_head["transaction_digest"]})
        self.assertFalse(pending["claims"]["source_transition_fixture_passed"])
        self.assertEqual(pending["transition_evidence"], {"state": "pending"})
        self.kernel.close_epoch(authority_ref=self.authority)
        with self.assertRaises(AuthorizationError):
            self.kernel.close_group(next_group="Planning", authority_ref=self.authority)

    def test_close_group_rejects_nonexistent_acceptance_evidence_without_minting_a_receipt(self):
        open_section(self.kernel, self._command())
        self.kernel.close_epoch(authority_ref=self.authority)
        before = self.kernel.head()
        before_state, before_status = self.kernel.read_state(), self.kernel.resume()

        with self.assertRaises(AuthorizationError):
            self.kernel.close_group(
                next_group="Planning",
                acceptance_evidence=["sha256:" + "d" * 64],
                authority_ref=self.authority,
            )

        self.assertEqual(self.kernel.head(), before)
        self.assertEqual(self.kernel.read_state(), before_state)
        self.assertEqual(self.kernel.resume(), before_status)
        self.assertEqual(self.kernel.ready_tasks(), [])
        self.assertNotIn("accepted_section_receipt", self.kernel.read_state()["metadata"]["section_control"])

    def test_close_group_rejects_reachable_but_semantically_wrong_acceptance_evidence(self):
        open_section(self.kernel, self._command())
        self.kernel.close_epoch(authority_ref=self.authority)
        before = self.kernel.head()
        before_state, before_status = self.kernel.read_state(), self.kernel.resume()

        with self.assertRaises(AuthorizationError):
            self.kernel.close_group(
                next_group="Planning",
                acceptance_evidence=[self.kernel.read_state()["entry_object_ref"]["digest"]],
                authority_ref=self.authority,
            )

        self.assertEqual(self.kernel.head(), before)
        self.assertEqual(self.kernel.read_state(), before_state)
        self.assertEqual(self.kernel.resume(), before_status)
        self.assertEqual(self.kernel.ready_tasks(), [])
        self.assertNotIn("accepted_section_receipt", self.kernel.read_state()["metadata"]["section_control"])

    def test_close_group_rejects_reachable_unrelated_artifact_as_acceptance_evidence(self):
        open_section(self.kernel, self._command())
        self.kernel.close_epoch(authority_ref=self.authority)
        before = self.kernel.head()
        before_state, before_status = self.kernel.read_state(), self.kernel.resume()

        with self.assertRaises(AuthorizationError):
            self.kernel.close_group(
                next_group="Planning",
                acceptance_evidence=[self.source_refs["section"]["plan"]],
                authority_ref=self.authority,
            )

        self.assertEqual(self.kernel.head(), before)
        self.assertEqual(self.kernel.read_state(), before_state)
        self.assertEqual(self.kernel.resume(), before_status)
        self.assertEqual(self.kernel.ready_tasks(), [])
        self.assertNotIn("accepted_section_receipt", self.kernel.read_state()["metadata"]["section_control"])

    def test_close_group_accepts_current_run_reachable_section_result_with_matching_identity_and_passed_result(self):
        open_section(self.kernel, self._command())
        evidence = self._accepted_result()
        self.kernel.close_epoch(authority_ref=self.authority)

        self.kernel.close_group(next_group="Planning", acceptance_evidence=[evidence], authority_ref=self.authority)

        receipt = self.kernel.read_state()["metadata"]["section_control"]["accepted_section_receipt"]
        self.assertEqual(self.kernel.read_object(receipt)["payload"]["payload"]["acceptance_evidence"], [evidence])


if __name__ == "__main__":
    unittest.main()
