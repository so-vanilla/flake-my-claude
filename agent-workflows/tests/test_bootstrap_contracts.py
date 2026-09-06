import copy
import inspect
import json
import sys
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "src"))
CASES = json.loads((ROOT / "tests/fixtures/s1/bootstrap-cases.json").read_text())
HEAD = CASES["expected_head"]
AUTHORITY = CASES["authority"]
VALID_CALLS = [
    ("classify_environment", "group.A.A1", "valid_inventory"),
    ("plan_workspace", "group.A.A2", "valid_workspace"),
    ("design_rollback", "group.A.A3", "valid_rollback"),
    ("initialize_run_plan", "group.A.A4", "valid_run_plan"),
    ("plan_legacy_detach", "group.A.A5", "valid_legacy_detach"),
]


class A1_EnvironmentInventoryTests(unittest.TestCase):
    def test_unknown_owner_is_refused_without_guessing(self):
        from ai_agent_workflow.bootstrap_contracts import BootstrapContractsV1

        inputs = copy.deepcopy(CASES["valid_inventory"])
        inputs["entries"].append({
            "path": "unknown/runtime-state", "class": "unmanaged", "owner": "unknown",
            "source": "inspection", "retention": "preserve", "inspection": "manual",
        })
        result = BootstrapContractsV1().compile(
            "classify_environment", "group.A.A1", inputs, AUTHORITY, HEAD
        )

        self.assertEqual("refused", result["outcome"]["status"])
        self.assertEqual("unknown-owner", result["outcome"]["reason_code"])
        self.assertEqual("unknown", result["artifact"]["entries"][1]["owner"])


class A2_WorkspaceIsolationTests(unittest.TestCase):
    def test_foreign_dirty_path_overlapping_write_scope_is_refused(self):
        from ai_agent_workflow.bootstrap_contracts import BootstrapContractsV1

        inputs = copy.deepcopy(CASES["valid_workspace"])
        inputs["dirty_paths"] = ["agent-workflows/src/ai_agent_workflow/bootstrap_contracts.py"]
        inputs["write_scope"] = ["agent-workflows/src/ai_agent_workflow/bootstrap_contracts.py"]
        result = BootstrapContractsV1().compile(
            "plan_workspace", "group.A.A2", inputs, AUTHORITY, HEAD
        )

        self.assertEqual("refused", result["outcome"]["status"])
        self.assertEqual("foreign-diff-overlap", result["outcome"]["reason_code"])


class A3_RollbackDesignTests(unittest.TestCase):
    def test_secret_backup_request_is_refused(self):
        from ai_agent_workflow.bootstrap_contracts import BootstrapContractsV1

        inputs = copy.deepcopy(CASES["valid_rollback"])
        inputs["backup_paths"] = [".env.production"]
        inputs["secret_paths"] = [".env.production"]
        result = BootstrapContractsV1().compile(
            "design_rollback", "group.A.A3", inputs, AUTHORITY, HEAD
        )

        self.assertEqual("refused", result["outcome"]["status"])
        self.assertEqual("secret-backup", result["outcome"]["reason_code"])


class A4_RunInitializationTests(unittest.TestCase):
    def test_candidate_objective_remains_unapproved(self):
        from ai_agent_workflow.bootstrap_contracts import BootstrapContractsV1

        inputs = copy.deepcopy(CASES["valid_run_plan"])
        inputs["objective"]["approval_status"] = "candidate"
        result = BootstrapContractsV1().compile(
            "initialize_run_plan", "group.A.A4", inputs, AUTHORITY, HEAD
        )

        self.assertEqual("refused", result["outcome"]["status"])
        self.assertEqual("objective-not-approved", result["outcome"]["reason_code"])
        self.assertEqual("candidate", result["artifact"]["details"]["objective_status"])


class A5_LegacyDetachTests(unittest.TestCase):
    def test_missing_physical_non_use_authority_is_refused(self):
        from ai_agent_workflow.bootstrap_contracts import BootstrapContractsV1

        inputs = copy.deepcopy(CASES["valid_legacy_detach"])
        del inputs["non_use_authority"]
        result = BootstrapContractsV1().compile(
            "plan_legacy_detach", "group.A.A5", inputs, AUTHORITY, HEAD
        )

        self.assertEqual("refused", result["outcome"]["status"])
        self.assertEqual("missing-non-use-authority", result["outcome"]["reason_code"])
        self.assertEqual("legacy-detach-plan", result["artifact"]["kind"])


class BootstrapContractControlsTests(unittest.TestCase):
    def test_public_seams_are_deterministic_non_mutating_and_exactly_qualified(self):
        from ai_agent_workflow.bootstrap_contracts import BootstrapContractsV1

        source = BootstrapContractsV1()
        results = []
        for operation, qualified_id, fixture_name in VALID_CALLS:
            inputs = copy.deepcopy(CASES[fixture_name])
            before = copy.deepcopy(inputs)
            first = source.compile(operation, qualified_id, inputs, AUTHORITY, HEAD)
            self.assertEqual(before, inputs)
            self.assertEqual(
                first,
                source.compile(operation, qualified_id, copy.deepcopy(inputs), AUTHORITY, HEAD),
            )
            self.assertEqual(qualified_id, first["qualified_id"])
            self.assertEqual("completed", first["outcome"]["status"])
            self.assertEqual(HEAD, first["expected_head"])
            self.assertEqual(AUTHORITY, first["authority_ref"])
            self.assertTrue(all(ref["path"] != "candidate-generic" for ref in first["input_refs"]))
            results.append(first)
        self.assertEqual({"group.A.A1", "group.A.A2", "group.A.A3", "group.A.A4", "group.A.A5"}, {item["qualified_id"] for item in results})

    def test_schema_rejects_an_unsupported_nested_property(self):
        from ai_agent_workflow.bootstrap_contracts import BootstrapContractsV1
        from ai_agent_workflow.schema_validation import SchemaValidationError, validate_document

        document = BootstrapContractsV1().compile(
            "classify_environment", "group.A.A1", CASES["valid_inventory"], AUTHORITY, HEAD
        )
        document["artifact"]["unexpected"] = True
        schema = json.loads((ROOT / "schemas/bootstrap-artifact-v1.schema.json").read_text())
        with self.assertRaises(SchemaValidationError):
            validate_document(document, schema)


class BootstrapValidatedFindingReproductions(unittest.TestCase):
    def test_s1_a_br_001_exposes_the_prescribed_dispatcher_and_physical_provenance(self):
        from ai_agent_workflow.bootstrap_contracts import BootstrapContractError, BootstrapContractsV1
        from ai_agent_workflow.schema_validation import validate_document

        source = BootstrapContractsV1()
        self.assertEqual(
            ["operation", "qualified_id", "inputs", "authority", "expected_head"],
            list(inspect.signature(source.compile).parameters),
        )
        schema = json.loads((ROOT / "schemas/bootstrap-artifact-v1.schema.json").read_text())
        for operation, qualified_id, fixture_name in VALID_CALLS:
            inputs = copy.deepcopy(CASES[fixture_name])
            result = source.compile(operation, qualified_id, inputs, AUTHORITY, HEAD)
            validate_document(result, schema)
            self.assertEqual(operation, result["operation"])
            self.assertEqual(qualified_id, result["qualified_id"])
            self.assertTrue(result["input_refs"])
            self.assertEqual(HEAD, result["expected_head"])
            self.assertEqual(AUTHORITY, result["authority_ref"])
        with self.assertRaisesRegex(BootstrapContractError, "unknown Bootstrap operation"):
            source.compile("unknown", "group.A.A1", CASES["valid_inventory"], AUTHORITY, HEAD)
        with self.assertRaisesRegex(BootstrapContractError, "requires qualified selector"):
            source.compile("classify_environment", "group.A.A5", CASES["valid_inventory"], AUTHORITY, HEAD)
        with self.assertRaisesRegex(BootstrapContractError, "approved source authority"):
            source.compile("classify_environment", "group.A.A1", CASES["valid_inventory"], {}, HEAD)
        with self.assertRaisesRegex(BootstrapContractError, "expected_head is malformed"):
            source.compile("classify_environment", "group.A.A1", CASES["valid_inventory"], AUTHORITY, {})
        non_physical = copy.deepcopy(CASES["valid_inventory"])
        non_physical["input_refs"][0]["path"] = "candidate-generic"
        refusal = source.compile("classify_environment", "group.A.A1", non_physical, AUTHORITY, HEAD)
        self.assertEqual({"status": "refused", "reason_code": "invalid-input"}, refusal["outcome"])
        validate_document(refusal, schema)

    def test_s1_a_br_002_refuses_incomplete_a2_through_a5_bindings(self):
        from ai_agent_workflow.bootstrap_contracts import BootstrapContractsV1

        source = BootstrapContractsV1()
        results = [
            source.compile("plan_workspace", "group.A.A2", {
                "base_ref": "e5b87e08", "dirty_paths": [], "write_scope": [],
                "local_inputs": [".local/agent/input.json"], "rollback_ref": "refs/heads/main",
            }, AUTHORITY, HEAD),
            source.compile("design_rollback", "group.A.A3", {
                "restore_sources": ["external:unverified"], "backup_paths": [], "secret_paths": [],
            }, AUTHORITY, HEAD),
            source.compile("initialize_run_plan", "group.A.A4", {
                "objective": {"path": "objectives/v001.md", "approval_status": "approved"},
                "workspace_plan_ref": "sha256:" + "z" * 64,
            }, AUTHORITY, HEAD),
            source.compile("plan_legacy_detach", "group.A.A5", {
                "inventory_ref": "", "rollback_ref": "", "a6_input_refs": [],
                "non_use_authority": {"path": "receipts/non-use.json", "digest": "sha256:" + "d" * 64},
            }, AUTHORITY, HEAD),
        ]

        self.assertEqual(["refused"] * 4, [item["outcome"]["status"] for item in results])
        controls = [
            source.compile(operation, qualified_id, CASES[fixture_name], AUTHORITY, HEAD)
            for operation, qualified_id, fixture_name in VALID_CALLS[1:]
        ]
        self.assertEqual(["completed"] * 4, [item["outcome"]["status"] for item in controls])
        duplicate_a6 = copy.deepcopy(CASES["valid_legacy_detach"])
        duplicate_a6["a6_input_refs"].append(copy.deepcopy(duplicate_a6["a6_input_refs"][0]))
        duplicate_result = source.compile(
            "plan_legacy_detach", "group.A.A5", duplicate_a6, AUTHORITY, HEAD
        )
        self.assertEqual({"status": "refused", "reason_code": "invalid-input"}, duplicate_result["outcome"])

    def test_s1_a_br_003_rejects_cross_bound_artifact_and_outcome(self):
        from ai_agent_workflow.bootstrap_contracts import BootstrapContractsV1
        from ai_agent_workflow.schema_validation import SchemaValidationError, validate_document

        source = BootstrapContractsV1()
        schema = json.loads((ROOT / "schemas/bootstrap-artifact-v1.schema.json").read_text())
        valid = [
            source.compile(operation, qualified_id, CASES[fixture_name], AUTHORITY, HEAD)
            for operation, qualified_id, fixture_name in VALID_CALLS
        ]
        kinds = ["inventory", "workspace-plan", "rollback-plan", "run-plan", "legacy-detach-plan"]
        for index, document in enumerate(valid):
            validate_document(document, schema)
            for other_index, (_, other_id, _) in enumerate(VALID_CALLS):
                if other_index != index:
                    forged = copy.deepcopy(document)
                    forged["qualified_id"] = other_id
                    with self.assertRaises(SchemaValidationError):
                        validate_document(forged, schema)
            for other_kind in kinds:
                if other_kind != document["artifact"]["kind"]:
                    forged = copy.deepcopy(document)
                    forged["artifact"]["kind"] = other_kind
                    with self.assertRaises(SchemaValidationError):
                        validate_document(forged, schema)
        completed_refusal = copy.deepcopy(valid[0])
        completed_refusal["outcome"] = {"status": "completed", "reason_code": "unknown-owner"}
        missing_payload = copy.deepcopy(valid[0])
        del missing_payload["artifact"]["entries"]
        for forged in [completed_refusal, missing_payload]:
            with self.assertRaises(SchemaValidationError):
                validate_document(forged, schema)

    def test_s1_a_br_004_returns_typed_refusals_for_unsupported_nested_values(self):
        from ai_agent_workflow.bootstrap_contracts import BootstrapContractsV1
        from ai_agent_workflow.schema_validation import validate_document

        source = BootstrapContractsV1()
        schema = json.loads((ROOT / "schemas/bootstrap-artifact-v1.schema.json").read_text())
        results = []
        for operation, qualified_id, fixture_name in VALID_CALLS:
            inputs = copy.deepcopy(CASES[fixture_name])
            marker = object()
            inputs["unsupported"] = {"nested": [marker]}
            result = source.compile(operation, qualified_id, inputs, AUTHORITY, HEAD)
            self.assertIs(marker, inputs["unsupported"]["nested"][0])
            self.assertEqual(result, source.compile(operation, qualified_id, inputs, AUTHORITY, HEAD))
            validate_document(result, schema)
            results.append(result)
        self.assertEqual(["refused"] * 5, [item["outcome"]["status"] for item in results])
        self.assertEqual(["invalid-input"] * 5, [item["outcome"]["reason_code"] for item in results])

        class ExplodingMapping(dict):
            def items(self):
                raise RuntimeError("unrelated-programming-error")

        with self.assertRaisesRegex(RuntimeError, "unrelated-programming-error"):
            source.compile("classify_environment", "group.A.A1", ExplodingMapping(), AUTHORITY, HEAD)


if __name__ == "__main__":
    unittest.main()
