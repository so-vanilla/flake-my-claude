import copy
import hashlib
import json
import sys
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "src"))
sys.path.insert(0, str(ROOT / "tests"))

from ai_agent_workflow.execution_group import ExecutionGroupV1  # noqa: E402
from ai_agent_workflow.schema_validation import SchemaValidationError, validate_document  # noqa: E402
from test_execution_group import HEAD, authority, inputs  # noqa: E402


def raw(path):
    return "sha256:" + hashlib.sha256((ROOT.parent / path).read_bytes()).hexdigest()


def load(relative):
    return json.loads((ROOT / relative).read_text())


class ExecutionGroupStaticBindingTests(unittest.TestCase):
    def test_candidate_and_refusal_schemas_reject_unknown_keys(self):
        contract_schema = load("schemas/execution-group-v1.schema.json")
        candidate = ExecutionGroupV1().compile("group.E.E1", inputs(), authority(), HEAD)
        validate_document(candidate, contract_schema, contract_schema["$defs"])
        unknown_candidate = copy.deepcopy(candidate); unknown_candidate["transition_authority"] = True
        with self.assertRaises(SchemaValidationError):
            validate_document(unknown_candidate, contract_schema, contract_schema["$defs"])
        refusal = ExecutionGroupV1().compile("group.E.E1", inputs(expected_head={**HEAD, "revision": 8}), authority(), HEAD)
        validate_document(refusal, contract_schema, contract_schema["$defs"])
        unknown_refusal = copy.deepcopy(refusal); unknown_refusal["transition_authority"] = True
        with self.assertRaises(SchemaValidationError):
            validate_document(unknown_refusal, contract_schema, contract_schema["$defs"])

    def test_exact_manifest_pairs_and_all_physical_digests(self):
        manifest = load("groups/execution.json")
        manifest_schema = load("schemas/group-e-manifest-v1.schema.json")
        validate_document(manifest, manifest_schema, manifest_schema["$defs"])
        self.assertEqual("skill", manifest["implementation_kind"])
        self.assertEqual(["group.E.E%d" % number for number in range(1, 11)], [item["qualified_id"] for item in manifest["contracts"]])
        for item in manifest["contracts"]:
            for reference in ("source_ref", "selector_ref", "receipt_ref"):
                with self.subTest(contract=item["qualified_id"], reference=reference):
                    self.assertEqual(raw(item[reference]["path"]), item[reference]["digest"])
        local_swap = copy.deepcopy(manifest); local_swap["contracts"][0]["local_id"] = "E2"
        with self.assertRaises(SchemaValidationError):
            validate_document(local_swap, manifest_schema, manifest_schema["$defs"])
        path_swap = copy.deepcopy(manifest); path_swap["contracts"][0]["source_ref"]["path"] = manifest["contracts"][1]["source_ref"]["path"]
        with self.assertRaises(SchemaValidationError):
            validate_document(path_swap, manifest_schema, manifest_schema["$defs"])
        digest_mutation = copy.deepcopy(manifest); digest_mutation["contracts"][0]["source_ref"]["digest"] = "sha256:" + "0" * 64
        validate_document(digest_mutation, manifest_schema, manifest_schema["$defs"])
        self.assertNotEqual(raw(digest_mutation["contracts"][0]["source_ref"]["path"]), digest_mutation["contracts"][0]["source_ref"]["digest"])

    def test_qualified_evidence_and_execution_templates_are_complete(self):
        evidence_schema = load("schemas/qualified-contract-acceptance-v1.schema.json")
        names = ["execution-preflight", "dispatch-task", "execute-small-loop", "review-task-spec", "review-task-quality", "validate-review-findings", "fix-and-rereview", "converge-parallel-batch", "verify-whole-change", "arbitrate-exception"]
        for number, name in enumerate(names, start=1):
            evidence = load("evidence/contracts/E%d.json" % number)
            with self.subTest(evidence=number):
                validate_document(evidence, evidence_schema, evidence_schema.get("$defs", {}))
                self.assertEqual("group.E.E%d" % number, evidence["contract_id"])
                self.assertEqual(name, evidence["contract_name"])
                self.assertEqual("skill", evidence["implementation_kind"])
                self.assertEqual(raw(evidence["implementation_ref"]["path"]), evidence["implementation_ref"]["digest"])
        required_templates = ["task-package.md", "review-package.md", "finding-validation.md", "whole-verification.md", "exception-recommendation.md"]
        for filename in required_templates:
            with self.subTest(template=filename):
                content = (ROOT / "templates" / "execution" / filename).read_text()
                self.assertTrue(content.startswith("# "))
                self.assertGreater(len(content.strip()), 120)


if __name__ == "__main__":
    unittest.main()
