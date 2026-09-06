import copy
import json
import tempfile
import unittest
from pathlib import Path

from ai_agent_workflow.closure_protocol import ClosureProtocolError, SharedClosureProtocolV1
from ai_agent_workflow.control_kernel import ControlKernel, IntegrityBlockedError, StaleHeadError
from ai_agent_workflow.schema_validation import SchemaValidationError, validate_document


HEAD = {"revision": 8, "transaction_digest": "sha256:" + "c" * 64}
AUTHORITY = {"approved": True, "expected_head": HEAD}
COMMAND_AUTHORITY = {
    "approved": True,
    "scopes": ["source", "close_epoch", "close_group"],
    "write_scopes": ["source"],
    "protected_fields": ["head", "group", "epoch", "ready"],
    "human_receipt": "shared-closure-authority-receipt",
    "expected_head": HEAD,
    "assignment_id": "shared-closure-protocol",
    "run_id": "run-fixture",
    "workflow_version": "workflow/v1",
}
FIXTURE_PATH = Path(__file__).resolve().parent / "fixtures" / "s1" / "shared-closure-cases.json"
CASES = json.loads(FIXTURE_PATH.read_text(encoding="utf-8"))


class SharedClosureProtocolTests(unittest.TestCase):
    def test_finding_kernel_authority_is_compatible_for_f6_and_f7_without_apply(self):
        protocol = SharedClosureProtocolV1()
        refs = [
            {"selector": "group.F.F%d" % number, "digest": "sha256:" + str(number) * 64}
            for number in range(1, 6)
        ]
        checkpoint = protocol.write_checkpoint(
            "group.F.F6",
            {
                "run_id": "run-fixture",
                "workflow_version": "workflow/v1",
                "group_id": "analysis",
                "closure_refs": refs,
                "idempotency_key": "close-epoch-fixture",
            },
            COMMAND_AUTHORITY,
            HEAD,
        )["result"]["command"]
        clear = protocol.clear_boundary(
            "group.F.F7",
            {
                "run_id": "run-fixture",
                "workflow_version": "workflow/v1",
                "group_id": "analysis",
                "idempotency_key": "close-group-fixture",
                "checkpoint_ref": {"digest": "sha256:" + "e" * 64},
                "close_receipt": {
                    "status": "accepted",
                    "head": HEAD,
                    "clear_before_next": True,
                    "digest": "sha256:" + "f" * 64,
                },
            },
            COMMAND_AUTHORITY,
            HEAD,
        )["result"]["command"]

        with tempfile.TemporaryDirectory() as directory:
            kernel = ControlKernel(directory, run_id="run-fixture")
            for command, operation in ((checkpoint, "close_epoch"), (clear, "close_group")):
                kernel._validate_command_shape(command, verify_physical_attestation=False)
                kernel._authority_ok(
                    command["authority_ref"], operation, protected_fields=command["protected_fields"]
                )
                kernel._validate_scope(command["scope"], command["authority_ref"], label="command.scope")
                self.assertEqual(command["actor"], {"role": "orchestrator", "assignment_id": "shared-closure-protocol"})
                self.assertEqual(command["expected_head"], HEAD)
                self.assertEqual(command["authority_ref"]["expected_head"], HEAD)
                self.assertEqual(command["authority_ref"]["protected_fields"], command["protected_fields"])
                self.assertEqual(command["scope"], ["source"])

        for mutation in (
            {"scopes": ["source"]},
            {"write_scopes": []},
            {"protected_fields": ["group", "epoch", "ready"]},
            {"human_receipt": ""},
            {"expected_head": {"revision": 7, "transaction_digest": HEAD["transaction_digest"]}},
            {"assignment_id": "some-other-actor"},
        ):
            unsafe = copy.deepcopy(COMMAND_AUTHORITY)
            unsafe.update(mutation)
            with self.subTest(mutation=mutation), self.assertRaises(ClosureProtocolError):
                protocol.write_checkpoint(
                    "group.F.F6",
                    {
                        "run_id": "run-fixture",
                        "workflow_version": "workflow/v1",
                        "group_id": "analysis",
                        "closure_refs": refs,
                        "idempotency_key": "close-epoch-fixture",
                    },
                    unsafe,
                    HEAD,
                )

    def test_finding_f5_rejects_stale_or_independently_mismatched_advice_bindings(self):
        protocol = SharedClosureProtocolV1()
        arbitrary = "sha256:" + "1" * 64
        reproduced = {
            "workflow_ref": {"id": "workflow-main", "digest": arbitrary},
            "state_ref": {"digest": arbitrary},
            "checkpoint_ref": {"digest": arbitrary},
            "input_refs": [{"digest": arbitrary}],
            "clear_boundary": True,
            "next_group": "not-bound-to-any-plan",
            "prerequisites": {"workflow": True, "state": True, "checkpoint": True, "input": True, "clear": True},
        }
        with self.assertRaises(ClosureProtocolError):
            protocol.advise_next_group("group.F.F5", reproduced, AUTHORITY, HEAD)

        control = copy.deepcopy(CASES["F5"]["control"])
        result = protocol.advise_next_group("group.F.F5", control, AUTHORITY, HEAD)
        self.assertEqual(result["result"]["advice"], {"schema": "next-group-advice/v1", **control})
        mutations = {
            "stale-head": lambda value: value["expected_head"].update(revision=7),
            "workflow-mismatch": lambda value: value["future_plan_ref"].update(workflow_digest="sha256:" + "0" * 64),
            "state-mismatch": lambda value: value["state_ref"].update(revision=7),
            "checkpoint-mismatch": lambda value: value["checkpoint_ref"].update(state_digest="sha256:" + "0" * 64),
            "input-mismatch": lambda value: value["future_plan_ref"].update(input_digests=["sha256:" + "0" * 64]),
            "clear-boundary-mismatch": lambda value: value["future_plan_ref"].update(clear_boundary=False),
            "future-plan-mismatch": lambda value: value["future_plan_ref"].update(checkpoint_digest="sha256:" + "0" * 64),
            "next-group-mismatch": lambda value: value.update(next_group="some-other-group"),
        }
        self.assertEqual(set(mutations), set(CASES["F5"]["unsafe"]))
        for name, mutate in mutations.items():
            unsafe = copy.deepcopy(control)
            mutate(unsafe)
            with self.subTest(name=name), self.assertRaises(ClosureProtocolError):
                protocol.advise_next_group("group.F.F5", unsafe, AUTHORITY, HEAD)

    def test_finding_f4_rejects_dag_bypass_and_invalid_future_order(self):
        protocol = SharedClosureProtocolV1()
        control = copy.deepcopy(CASES["F4"]["control"])
        reproduced = copy.deepcopy(control)
        reproduced["proposed_plan"]["future_frontier"] = ["not-in-dag"]
        with self.assertRaises(ClosureProtocolError):
            protocol.replan_future("group.F.F4", reproduced, AUTHORITY, HEAD)

        result = protocol.replan_future("group.F.F4", control, AUTHORITY, HEAD)
        self.assertEqual(result["result"]["future_frontier"], ["next-a", "next-b"])
        mutations = {
            "missing-dag": lambda value: value.pop("dag"),
            "malformed-dag": lambda value: value["dag"].update(nodes="not-a-node-list"),
            "stale-dag": lambda value: value["dag"]["expected_head"].update(revision=7),
            "frontier-node-absent": lambda value: value["proposed_plan"].update(future_frontier=["absent"]),
            "invalid-graph-order": lambda value: value["proposed_plan"].update(future_frontier=["next-b", "next-a"]),
            "rewrite-accepted-history": lambda value: value["proposed_plan"].update(accepted_history=[]),
        }
        self.assertEqual(set(mutations), set(CASES["F4"]["unsafe"]))
        for name, mutate in mutations.items():
            unsafe = copy.deepcopy(control)
            mutate(unsafe)
            with self.subTest(name=name), self.assertRaises(ClosureProtocolError):
                protocol.replan_future("group.F.F4", unsafe, AUTHORITY, HEAD)

    def test_finding_result_schema_rejects_empty_cross_selector_and_multi_result_shapes(self):
        schema = json.loads(
            (Path(__file__).resolve().parents[1] / "schemas" / "closure-operation-result-v1.schema.json").read_text()
        )
        empty = {
            "schema": "closure-operation-result/v1",
            "selector": "group.F.F1",
            "status": "compiled",
            "expected_head": HEAD,
            "input_refs": [],
            "result": {},
        }
        with self.assertRaises(SchemaValidationError):
            validate_document(empty, schema)

        controls = self._schema_result_controls()
        self.assertEqual(set(controls), {"group.F.F%d" % number for number in range(1, 9)})
        for selector, document in controls.items():
            with self.subTest(selector=selector):
                validate_document(document, schema)
        cross = copy.deepcopy(controls["group.F.F1"])
        cross["selector"] = "group.F.F2"
        with self.assertRaises(SchemaValidationError):
            validate_document(cross, schema)
        multi = copy.deepcopy(controls["group.F.F1"])
        multi["result"]["artifact_inventory"] = []
        with self.assertRaises(SchemaValidationError):
            validate_document(multi, schema)
        missing_command = copy.deepcopy(controls["group.F.F6"])
        missing_command["result"] = {}
        with self.assertRaises(SchemaValidationError):
            validate_document(missing_command, schema)

    def test_finding_f8_normalizes_only_documented_upstream_failures(self):
        class KernelFailure:
            def __init__(self, error):
                self.error = error

            def resume(self, **kwargs):
                raise self.error

        protocol = SharedClosureProtocolV1()
        for error in (IntegrityBlockedError("corrupt transaction parent"), StaleHeadError("expected HEAD is stale")):
            with self.subTest(error=type(error).__name__), self.assertRaisesRegex(ClosureProtocolError, "resume"):
                protocol.resume("group.F.F8", {"identifier": "run-fixture", "kernel": KernelFailure(error)}, AUTHORITY, HEAD)
        with self.assertRaises(RuntimeError):
            protocol.resume(
                "group.F.F8",
                {"identifier": "run-fixture", "kernel": KernelFailure(RuntimeError("programming error"))},
                AUTHORITY,
                HEAD,
            )

    @staticmethod
    def _schema_result_controls():
        command = {
            "schema": "dag-command/v1",
            "command_id": "closure-command",
            "command_type": "close_epoch",
            "run_id": "run-fixture",
            "expected_head": HEAD,
            "workflow_version": "workflow/v1",
            "graph_version": "artifact-task-dag/v1",
            "actor": {"role": "orchestrator", "assignment_id": "shared-closure-protocol"},
            "authority_ref": COMMAND_AUTHORITY,
            "input_refs": [{"kind": "group.F.F1", "digest": "sha256:" + "1" * 64}],
            "idempotency_key": "closure-command",
            "protected_fields": ["head", "group", "epoch", "ready"],
            "scope": ["source"],
            "payload": {
                "acceptance_evidence": [],
                "approved_decisions": [],
                "unresolved_items": [],
                "invalidated_artifacts": [],
                "next_inputs": [],
                "context_budget": {"target": 200000, "normal_limit": 300000, "absolute_limit": 500000, "token_status": "unavailable", "token_count": None},
                "clear_before_next": True,
                "boundary_reason": "shared-closure-checkpoint",
            },
        }
        base = {"schema": "closure-operation-result/v1", "status": "compiled", "expected_head": HEAD, "input_refs": []}
        results = {
            "group.F.F1": {"alignment": "aligned"},
            "group.F.F2": {"artifact_inventory": []},
            "group.F.F3": {"decision_candidates": [], "unapproved_items": []},
            "group.F.F4": {"future_frontier": ["next-a"]},
            "group.F.F5": {"advice": copy.deepcopy(CASES["F5"]["control"]) | {"schema": "next-group-advice/v1"}},
            "group.F.F6": {"command": copy.deepcopy(command)},
            "group.F.F7": {"command": copy.deepcopy(command)},
            "group.F.F8": {"resume": {"identifier": "run-fixture", "head": HEAD, "section_id": "S0", "status": "active"}},
        }
        results["group.F.F7"]["command"]["command_type"] = "close_group"
        results["group.F.F7"]["command"]["payload"] = {
            "acceptance_evidence": [],
            "approved_decisions": [],
            "unresolved_items": [],
            "invalidated_artifacts": [],
            "next_inputs": [],
            "next_group": None,
            "boundary_reason": "shared-closure-clear",
        }
        return {selector: dict(base, selector=selector, result=result) for selector, result in results.items()}

    def test_f1_audit_group_purpose_requires_bound_objective_evidence(self):
        protocol = SharedClosureProtocolV1()
        inputs = {
            "objective": {"id": "objective-1", "digest": "sha256:" + "a" * 64, "status": "accepted"},
            "subobjective": {"id": "subobjective-1", "objective_digest": "sha256:" + "a" * 64},
            "group_result": {"objective_digest": "sha256:" + "a" * 64, "outcome": "aligned"},
        }
        original = copy.deepcopy(inputs)

        result = protocol.audit_group_purpose("group.F.F1", inputs, AUTHORITY, HEAD)

        self.assertEqual(result["schema"], "closure-operation-result/v1")
        self.assertEqual(result["selector"], "group.F.F1")
        self.assertEqual(result["result"]["alignment"], "aligned")
        self.assertEqual(inputs, original)
        with self.assertRaisesRegex(ClosureProtocolError, "objective"):
            protocol.audit_group_purpose("group.F.F1", {"group_result": inputs["group_result"]}, AUTHORITY, HEAD)

    def test_f2_collect_group_artifacts_keeps_all_classifications_and_required_entries(self):
        protocol = SharedClosureProtocolV1()
        inputs = {
            "artifacts": [
                {"id": "result", "digest": "sha256:" + "1" * 64, "classification": "canonical"},
                {"id": "review", "digest": "sha256:" + "2" * 64, "classification": "partial"},
                {"id": "finding", "digest": "sha256:" + "3" * 64, "classification": "unverified"},
            ],
            "required_artifact_ids": ["result", "review", "finding"],
        }
        original = copy.deepcopy(inputs)

        result = protocol.collect_group_artifacts("group.F.F2", inputs, AUTHORITY, HEAD)

        self.assertEqual([item["classification"] for item in result["result"]["artifact_inventory"]], ["canonical", "partial", "unverified"])
        self.assertEqual(inputs, original)
        with self.assertRaisesRegex(ClosureProtocolError, "required artifact"):
            protocol.collect_group_artifacts("group.F.F2", {"artifacts": inputs["artifacts"], "required_artifact_ids": ["missing"]}, AUTHORITY, HEAD)

    def test_f3_extract_decision_candidates_never_promotes_unapproved_material(self):
        protocol = SharedClosureProtocolV1()
        inputs = {"events": [
            {"id": "d1", "digest": "sha256:" + "4" * 64, "kind": "decision", "approved": True},
            {"id": "p1", "digest": "sha256:" + "5" * 64, "kind": "proposal", "approved": False},
            {"id": "o1", "digest": "sha256:" + "6" * 64, "kind": "observation", "approved": False},
            {"id": "a1", "digest": "sha256:" + "7" * 64, "kind": "assumption", "approved": False},
        ]}
        original = copy.deepcopy(inputs)

        result = protocol.extract_decision_candidates("group.F.F3", inputs, AUTHORITY, HEAD)

        self.assertEqual([item["id"] for item in result["result"]["decision_candidates"]], ["d1"])
        self.assertEqual([item["id"] for item in result["result"]["unapproved_items"]], ["p1", "o1", "a1"])
        self.assertEqual(inputs, original)
        with self.assertRaisesRegex(ClosureProtocolError, "promotion"):
            protocol.extract_decision_candidates("group.F.F3", {"events": inputs["events"], "promote_unapproved": True}, AUTHORITY, HEAD)

    def test_f4_replan_future_changes_only_the_future_frontier(self):
        protocol = SharedClosureProtocolV1()
        inputs = copy.deepcopy(CASES["F4"]["control"])
        original = copy.deepcopy(inputs)

        result = protocol.replan_future("group.F.F4", inputs, AUTHORITY, HEAD)

        self.assertEqual(result["result"]["future_frontier"], ["next-a", "next-b"])
        self.assertEqual(inputs, original)
        unsafe = copy.deepcopy(inputs)
        unsafe["proposed_plan"]["scope"] = "expanded"
        with self.assertRaisesRegex(ClosureProtocolError, "future frontier"):
            protocol.replan_future("group.F.F4", unsafe, AUTHORITY, HEAD)

    def test_f5_advise_next_group_binds_all_prerequisites(self):
        protocol = SharedClosureProtocolV1()
        inputs = copy.deepcopy(CASES["F5"]["control"])
        original = copy.deepcopy(inputs)

        result = protocol.advise_next_group("group.F.F5", inputs, AUTHORITY, HEAD)

        self.assertEqual(result["result"]["advice"]["next_group"], "analysis-next")
        self.assertEqual(inputs, original)
        unsafe = copy.deepcopy(inputs)
        unsafe["future_plan_ref"]["next_group"] = "different"
        with self.assertRaisesRegex(ClosureProtocolError, "next.group"):
            protocol.advise_next_group("group.F.F5", unsafe, AUTHORITY, HEAD)

    def test_f6_write_checkpoint_compiles_only_a_complete_close_epoch_command(self):
        protocol = SharedClosureProtocolV1()
        refs = [{"selector": "group.F.F%d" % number, "digest": "sha256:" + str(number) * 64} for number in range(1, 6)]
        inputs = {"run_id": "run-fixture", "workflow_version": "workflow/v1", "group_id": "analysis", "closure_refs": refs, "idempotency_key": "close-epoch-fixture"}
        original = copy.deepcopy(inputs)

        result = protocol.write_checkpoint("group.F.F6", inputs, COMMAND_AUTHORITY, HEAD)

        self.assertEqual(result["result"]["command"]["command_type"], "close_epoch")
        self.assertEqual(result["result"]["command"]["expected_head"], HEAD)
        self.assertEqual(inputs, original)
        with self.assertRaisesRegex(ClosureProtocolError, "F1-F5"):
            protocol.write_checkpoint("group.F.F6", dict(inputs, closure_refs=refs[:-1]), COMMAND_AUTHORITY, HEAD)

    def test_f7_clear_boundary_refuses_preclose_or_stale_receipts(self):
        protocol = SharedClosureProtocolV1()
        inputs = {
            "run_id": "run-fixture", "workflow_version": "workflow/v1", "group_id": "analysis", "idempotency_key": "close-group-fixture",
            "checkpoint_ref": {"digest": "sha256:" + "e" * 64},
            "close_receipt": {"status": "accepted", "head": HEAD, "clear_before_next": True, "digest": "sha256:" + "f" * 64},
        }
        original = copy.deepcopy(inputs)

        result = protocol.clear_boundary("group.F.F7", inputs, COMMAND_AUTHORITY, HEAD)

        self.assertEqual(result["result"]["command"]["command_type"], "close_group")
        self.assertEqual(inputs, original)
        stale = copy.deepcopy(inputs)
        stale["close_receipt"]["head"]["revision"] = 7
        with self.assertRaisesRegex(ClosureProtocolError, "stale"):
            protocol.clear_boundary("group.F.F7", stale, COMMAND_AUTHORITY, HEAD)

    def test_f8_resume_reads_only_a_valid_s0_resume_head_chain(self):
        class KernelDouble:
            def __init__(self, document):
                self.document = document
                self.calls = []

            def resume(self, **kwargs):
                self.calls.append(kwargs)
                return copy.deepcopy(self.document)

        status = {"schema": "section-status/v1", "section_id": "S0", "status": "active", "group": {"id": "analysis", "status": "open"}, "epoch": {"id": "S0-E1", "status": "open", "group_id": "analysis", "boundary_reason": "resume", "clear_before_next": False}, "ready": ["S0.A1"], "head": HEAD, "claims": {"source_transition_fixture_passed": False, "actual_a7": False, "activation": False, "full_ready": False}, "transition_evidence": {"state": "pending"}}
        kernel = KernelDouble(status)
        protocol = SharedClosureProtocolV1()

        result = protocol.resume("group.F.F8", {"identifier": "run-fixture", "kernel": kernel}, AUTHORITY, HEAD)

        self.assertEqual(result["result"]["resume"]["head"], HEAD)
        self.assertEqual(kernel.calls, [{"expected_revision": 8, "expected_head_digest": HEAD["transaction_digest"]}])
        corrupt = KernelDouble(dict(status, schema="corrupt/v1"))
        with self.assertRaises(ClosureProtocolError):
            protocol.resume("group.F.F8", {"identifier": "run-fixture", "kernel": corrupt}, AUTHORITY, HEAD)

    def test_shared_manifest_and_dispatch_reject_bare_f_aliases(self):
        schema_path = Path(__file__).resolve().parents[1] / "schemas" / "shared-closure-manifest-v1.schema.json"
        manifest = {"schema": "shared-closure-manifest/v1", "group_id": "agent-workflows.group.F", "source": {"path": "agent-workflows/src/ai_agent_workflow/closure_protocol.py", "version": "v1"}, "selectors": ["group.F.F%d" % number for number in range(1, 9)]}
        validate_document(manifest, json.loads(schema_path.read_text()))
        with self.assertRaises(SchemaValidationError):
            validate_document(dict(manifest, selectors=["F%d" % number for number in range(1, 9)]), json.loads(schema_path.read_text()))
        with self.assertRaisesRegex(ClosureProtocolError, "qualified"):
            SharedClosureProtocolV1().compile("resume", "F8", {"identifier": "run", "kernel": object()}, AUTHORITY, HEAD)


if __name__ == "__main__":
    unittest.main()
