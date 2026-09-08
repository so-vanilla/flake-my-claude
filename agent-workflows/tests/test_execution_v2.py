import copy
import hashlib
import json
import sys
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "src"))

from ai_agent_workflow.execution_v2 import (  # noqa: E402
    EvidenceFinalizer,
    ExecutionClosureBuilder,
    FindingValidator,
    IssuanceWatermarkCutoverPlanner,
    ReceiptAggregator,
    RegressionFrontier,
    V2ContractError,
)
from ai_agent_workflow.schema_validation import SchemaValidationError, validate_document  # noqa: E402


DIGESTS = {letter: "sha256:" + letter * 64 for letter in "abcdef"}


def execution_input():
    return {
        "schema": "execution-package-input/v2",
        "package_id": "package-001",
        "contract_version": "workflow-execution/v2",
        "workspace_identity": "/workspace",
        "candidate_ref": {"id": "candidate-001", "digest": DIGESTS["a"]},
        "test_refs": [{"id": "test-b", "digest": DIGESTS["c"]}, {"id": "test-a", "digest": DIGESTS["b"]}],
        "fixture_refs": [],
        "schema_refs": [{"id": "schema", "digest": DIGESTS["d"]}],
        "config_refs": [],
        "lock_refs": [{"id": "lock", "digest": DIGESTS["e"]}],
        "toolchain": {"executable_digest": DIGESTS["f"], "identity": "python-3.13"},
        "command": {"argv": ["python3", "-m", "unittest", "test_one"], "cwd": "/workspace"},
        "environment": {"LANG": "C.UTF-8"},
        "isolation": {"cwd": "/workspace", "temporary_namespace": "tmp-001", "output_namespace": "out-001"},
        "resource_claims": {"read_paths": ["src"], "write_paths": [], "exclusive_resources": []},
        "supervision": {"timeout_seconds": 300, "grace_seconds": 5, "signals": ["TERM", "KILL"], "heartbeat_seconds": 5, "terminal_publication_seconds": 5},
        "external_input_refs": [],
    }


def candidate(closure):
    return {
        "schema": "artifact-candidate/v1",
        "candidate_id": "candidate-001",
        "candidate_digest": DIGESTS["a"],
        "execution_closure_digest": closure["closure_digest"],
        "regression_inventory": ["test-c", "test-a", "test-b"],
        "frozen": True,
    }


def shards():
    return [
        {"shard_id": "shard-b", "members": ["test-b"], "command": {"argv": ["python3", "-m", "unittest", "test-b"]}, "resource_claims": {"read_paths": ["tmp/data"], "write_paths": [], "exclusive_resources": []}, "isolation": {"cwd": "/workspace/b", "temporary_namespace": "tmp-b", "output_namespace": "out-b"}},
        {"shard_id": "shard-c", "members": ["test-c"], "command": {"argv": ["python3", "-m", "unittest", "test-c"]}, "resource_claims": {"read_paths": [], "write_paths": [], "exclusive_resources": ["network"]}, "isolation": {"cwd": "/workspace/c", "temporary_namespace": "tmp-c", "output_namespace": "out-c"}},
        {"shard_id": "shard-a", "members": ["test-a"], "command": {"argv": ["python3", "-m", "unittest", "test-a"]}, "resource_claims": {"read_paths": [], "write_paths": ["tmp"], "exclusive_resources": []}, "isolation": {"cwd": "/workspace/a", "temporary_namespace": "tmp-a", "output_namespace": "out-a"}},
    ]


def object_digest(value):
    encoded = json.dumps(value, sort_keys=True, separators=(",", ":")).encode()
    return "sha256:" + hashlib.sha256(encoded).hexdigest()


def receipt(shard, closure, status="passed"):
    value = {
        "schema": "command-receipt/v1",
        "receipt_id": "receipt-" + shard["shard_id"],
        "shard_id": shard["shard_id"],
        "candidate_digest": DIGESTS["a"],
        "execution_closure_digest": shard["execution_closure_ref"]["digest"],
        "idempotency_key": "run-" + shard["shard_id"],
        "payload_digest": object_digest({"shard": shard["shard_id"]}),
        "status": status,
        "coverage": list(shard["members"]),
        "capture_state": {"stdout": "complete", "stderr": "complete"},
        "terminal": True,
    }
    value["receipt_digest"] = object_digest(value)
    return value


def review(axis, finding_id):
    return {
        "schema": "ordinary-review-report/v1",
        "report_id": "review-" + axis,
        "axis": axis,
        "actor_id": "actor-" + axis,
        "context_epoch_id": "epoch-" + axis,
        "package_digest": DIGESTS["b"] if axis == "architecture-safety" else DIGESTS["c"],
        "candidate_digest": DIGESTS["a"],
        "aggregate_digest": DIGESTS["d"],
        "findings": [{"finding_id": finding_id, "fingerprint": "same-root", "severity": "major", "summary": "same material concern"}],
    }


def issued_inventory(candidate_ref):
    value = {
        "schema": "issued-lineage-inventory/v1",
        "inventory_id": "issued-001",
        "candidate_ref": copy.deepcopy(candidate_ref),
        "sequence_domain": {"first": 8, "watermark": 9},
        "roots": [
            {"package_id": "root-8", "sequence": 8, "contract_version": "workflow-execution/v1", "terminal": False, "terminal_ref": {"id": "state-root-8", "digest": DIGESTS["b"]}, "descendants": [{"package_id": "replacement-8", "terminal": True, "terminal_ref": {"id": "receipt-replacement-8", "digest": DIGESTS["c"]}}]},
            {"package_id": "root-9", "sequence": 9, "contract_version": "workflow-execution/v1", "terminal": True, "terminal_ref": {"id": "receipt-root-9", "digest": DIGESTS["d"]}, "descendants": []},
        ],
    }
    value["inventory_digest"] = object_digest(value)
    return value


class ExecutionV2Tests(unittest.TestCase):
    def test_execution_closure_is_canonical_and_fails_closed_on_missing_identity(self):
        builder = ExecutionClosureBuilder()
        first = builder.freeze(execution_input())
        reordered = execution_input()
        reordered["test_refs"].reverse()
        self.assertEqual(first, builder.freeze(reordered))
        self.assertEqual(first["schema"], "execution-package-closure/v2")
        self.assertRegex(first["closure_digest"], r"^sha256:[0-9a-f]{64}$")
        self.assertNotIn("authority", first)

        changed = execution_input()
        changed["environment"]["LANG"] = "en_US.UTF-8"
        self.assertNotEqual(first["closure_digest"], builder.freeze(changed)["closure_digest"])
        for missing in ("candidate_ref", "toolchain", "command", "isolation", "supervision"):
            invalid = execution_input()
            invalid.pop(missing)
            with self.subTest(missing=missing), self.assertRaises(V2ContractError):
                builder.freeze(invalid)

    def test_regression_partition_is_complete_disjoint_and_admission_is_deterministic(self):
        closure = ExecutionClosureBuilder().freeze(execution_input())
        frontier = RegressionFrontier()
        plan = frontier.plan(candidate(closure), closure, shards())
        self.assertEqual([item["shard_id"] for item in plan["shards"]], ["shard-a", "shard-b", "shard-c"])
        self.assertEqual([item["shard_id"] for item in frontier.admit_wave(plan)], ["shard-a", "shard-c"])
        self.assertEqual(plan["shards"][0]["execution_closure"]["command"]["argv"][-1], "test-a")
        self.assertEqual(plan["shards"][0]["execution_closure_ref"]["digest"], plan["shards"][0]["execution_closure"]["closure_digest"])
        self.assertIn({"id": "parent-execution-closure", "digest": closure["closure_digest"]}, plan["shards"][0]["execution_closure"]["external_input_refs"])

        for claim in ("/workspace/tmp", "../tmp", "src/../tmp", "src//tmp", "./src"):
            alias = shards()
            alias[0]["resource_claims"]["read_paths"] = [claim]
            with self.subTest(claim=claim), self.assertRaises(V2ContractError):
                frontier.plan(candidate(closure), closure, alias)

        overlapping = shards()
        overlapping[1]["members"] = ["test-a", "test-c"]
        with self.assertRaises(V2ContractError):
            frontier.plan(candidate(closure), closure, overlapping)
        unknown_claim = shards()
        unknown_claim[0]["resource_claims"]["gpu"] = True
        with self.assertRaises(V2ContractError):
            frontier.plan(candidate(closure), closure, unknown_claim)
        wrong_closure = copy.deepcopy(closure)
        wrong_closure["closure_digest"] = DIGESTS["b"]
        with self.assertRaises(V2ContractError):
            frontier.plan(candidate(closure), wrong_closure, shards())

    def test_receipt_aggregation_requires_exact_terminal_coverage_and_safe_reuse(self):
        closure = ExecutionClosureBuilder().freeze(execution_input())
        candidate_value = candidate(closure)
        plan = RegressionFrontier().plan(candidate_value, closure, shards())
        receipts = [receipt(shard, closure) for shard in reversed(plan["shards"])]
        aggregate = ReceiptAggregator().aggregate(candidate_value, plan, receipts + [copy.deepcopy(receipts[0])])
        self.assertTrue(aggregate["terminal_complete"])
        self.assertTrue(aggregate["accepted"])
        self.assertEqual(aggregate["coverage"], ["test-a", "test-b", "test-c"])
        self.assertEqual(aggregate["reused_receipt_ids"], [receipts[0]["receipt_id"]])

        changed_same_id = copy.deepcopy(receipts[0])
        changed_same_id["payload_digest"] = DIGESTS["f"]
        changed_same_id["receipt_digest"] = object_digest({key: value for key, value in changed_same_id.items() if key != "receipt_digest"})
        with self.assertRaises(V2ContractError):
            ReceiptAggregator().aggregate(candidate_value, plan, receipts + [changed_same_id])
        incomplete = receipts[:-1]
        with self.assertRaises(V2ContractError):
            ReceiptAggregator().aggregate(candidate_value, plan, incomplete)
        wrong_closure = copy.deepcopy(receipts)
        wrong_closure[0].pop("execution_closure_digest")
        with self.assertRaises(V2ContractError):
            ReceiptAggregator().aggregate(candidate_value, plan, wrong_closure)

    def test_finding_validation_is_complete_deduplicated_and_advisory_only(self):
        reviews = [review("architecture-safety", "A-1"), review("integration-operability", "B-1")]
        report = FindingValidator().validate(
            reviews,
            [{"fingerprint": "same-root", "classification": "deliberate-design", "materiality": "material", "proposed_scope": []}],
            {"remaining_seconds": 120, "review_round": 2, "product_fix_attempt": 0},
        )
        self.assertTrue(report["advisory_only"])
        self.assertEqual(report["source_finding_ids"], ["A-1", "B-1"])
        self.assertEqual(len(report["dispositions"]), 1)
        self.assertNotIn("transition_authority", report)
        with self.assertRaises(V2ContractError):
            FindingValidator().validate(reviews, [], {"remaining_seconds": 120, "review_round": 2, "product_fix_attempt": 0})
        same_actor = copy.deepcopy(reviews)
        same_actor[1]["actor_id"] = same_actor[0]["actor_id"]
        with self.assertRaises(V2ContractError):
            FindingValidator().validate(same_actor, [{"fingerprint": "same-root", "classification": "duplicate", "materiality": "material", "proposed_scope": []}], {"remaining_seconds": 120, "review_round": 2, "product_fix_attempt": 0})

    def test_finalization_requires_every_known_complete_accepted_branch(self):
        closure = ExecutionClosureBuilder().freeze(execution_input())
        candidate_value = candidate(closure)
        plan = RegressionFrontier().plan(candidate_value, closure, shards())
        aggregate = ReceiptAggregator().aggregate(candidate_value, plan, [receipt(shard, closure) for shard in plan["shards"]])
        reviews = [review("architecture-safety", "A-1"), review("integration-operability", "B-1")]
        for item in reviews:
            item["aggregate_digest"] = aggregate["aggregate_digest"]
        dispositions = FindingValidator().validate(reviews, [{"fingerprint": "same-root", "classification": "deliberate-design", "materiality": "material", "proposed_scope": []}], {"remaining_seconds": 120, "review_round": 2, "product_fix_attempt": 0})
        branches = [
            {"branch_id": "regression", "state": "accepted", "complete": True, "terminal_ref": {"id": "aggregate", "digest": aggregate["aggregate_digest"]}},
            {"branch_id": "review", "state": "accepted", "complete": True, "terminal_ref": {"id": "disposition", "digest": dispositions["disposition_digest"]}},
        ]
        inventory = {
            "schema": "declared-branch-inventory/v1",
            "inventory_id": "branches-001",
            "candidate_ref": {"id": candidate_value["candidate_id"], "digest": candidate_value["candidate_digest"]},
            "branches": [{"branch_id": item["branch_id"], "terminal_ref": copy.deepcopy(item["terminal_ref"])} for item in branches],
        }
        inventory["inventory_digest"] = object_digest(inventory)
        inventory_ref = {"id": inventory["inventory_id"], "digest": inventory["inventory_digest"]}
        finalized = EvidenceFinalizer().finalize(candidate_value, aggregate, dispositions, inventory_ref, inventory, branches)
        self.assertEqual(finalized["acceptance"], {"known": True, "complete": True, "accepted": True})
        for supplied in (branches[:-1], branches + [{"branch_id": "surplus", "state": "accepted", "complete": True, "terminal_ref": {"id": "aggregate", "digest": aggregate["aggregate_digest"]}}]):
            with self.assertRaises(V2ContractError):
                EvidenceFinalizer().finalize(candidate_value, aggregate, dispositions, inventory_ref, inventory, supplied)
        substituted = copy.deepcopy(branches)
        substituted[0]["terminal_ref"] = copy.deepcopy(branches[1]["terminal_ref"])
        with self.assertRaises(V2ContractError):
            EvidenceFinalizer().finalize(candidate_value, aggregate, dispositions, inventory_ref, inventory, substituted)
        required = copy.deepcopy(dispositions)
        required["dispositions"][0]["classification"] = "required"
        required["disposition_digest"] = object_digest({key: value for key, value in required.items() if key != "disposition_digest"})
        with self.assertRaises(V2ContractError):
            EvidenceFinalizer().finalize(candidate_value, aggregate, required, inventory_ref, inventory, branches)

    def test_cutover_binds_head_lease_watermark_outstanding_set_and_old_lineage(self):
        head = {"revision": 9, "transaction_digest": DIGESTS["a"]}
        lease = {"schema": "issuance-lease/v1", "lease_id": "lease-9", "status": "active", "expected_head": copy.deepcopy(head)}
        candidate_ref = {"id": "candidate-001", "digest": DIGESTS["a"]}
        inventory = issued_inventory(candidate_ref)
        inventory_ref = {"id": inventory["inventory_id"], "digest": inventory["inventory_digest"]}
        planner = IssuanceWatermarkCutoverPlanner()
        plan = planner.plan(head, copy.deepcopy(head), lease, "workflow-execution/v1", "workflow-execution/v2", candidate_ref, inventory_ref, inventory)
        self.assertEqual(plan["outstanding_old_package_ids"], ["root-8"])
        self.assertEqual(plan["issuance_watermark"], 9)
        self.assertEqual(plan["selection_rule"]["later_new_roots"], "workflow-execution/v2")
        stale = copy.deepcopy(head)
        stale["revision"] = 10
        with self.assertRaises(V2ContractError):
            planner.plan(head, stale, lease, "workflow-execution/v1", "workflow-execution/v2", candidate_ref, inventory_ref, inventory)
        gapped = issued_inventory(candidate_ref)
        gapped["sequence_domain"]["first"] = 7
        gapped["inventory_digest"] = object_digest({key: value for key, value in gapped.items() if key != "inventory_digest"})
        with self.assertRaises(V2ContractError):
            planner.plan(head, head, lease, "workflow-execution/v1", "workflow-execution/v2", candidate_ref, inventory_ref, gapped)
        nonterminal_descendant = issued_inventory(candidate_ref)
        nonterminal_descendant["roots"][0]["descendants"][0]["terminal"] = False
        nonterminal_descendant["inventory_digest"] = object_digest({key: value for key, value in nonterminal_descendant.items() if key != "inventory_digest"})
        with self.assertRaises(V2ContractError):
            planner.plan(head, head, lease, "workflow-execution/v1", "workflow-execution/v2", candidate_ref, inventory_ref, nonterminal_descendant)
        omitted = issued_inventory(candidate_ref)
        omitted["roots"] = omitted["roots"][1:]
        omitted["sequence_domain"]["first"] = 9
        omitted["inventory_digest"] = object_digest({key: value for key, value in omitted.items() if key != "inventory_digest"})
        with self.assertRaises(V2ContractError):
            planner.plan(head, head, lease, "workflow-execution/v1", "workflow-execution/v2", candidate_ref, inventory_ref, omitted)
        wrong_candidate = {"id": "candidate-002", "digest": DIGESTS["b"]}
        with self.assertRaises(V2ContractError):
            planner.plan(head, head, lease, "workflow-execution/v1", "workflow-execution/v2", wrong_candidate, inventory_ref, inventory)
        stale_lease = copy.deepcopy(lease)
        stale_lease["status"] = "closed"
        with self.assertRaises(V2ContractError):
            planner.plan(head, head, stale_lease, "workflow-execution/v1", "workflow-execution/v2", candidate_ref, inventory_ref, inventory)

    def test_public_artifacts_satisfy_strict_source_schemas(self):
        closure = ExecutionClosureBuilder().freeze(execution_input())
        candidate_value = candidate(closure)
        plan = RegressionFrontier().plan(candidate_value, closure, shards())
        aggregate = ReceiptAggregator().aggregate(candidate_value, plan, [receipt(shard, closure) for shard in plan["shards"]])
        reviews = [review("architecture-safety", "A-1"), review("integration-operability", "B-1")]
        for item in reviews:
            item["aggregate_digest"] = aggregate["aggregate_digest"]
        disposition = FindingValidator().validate(reviews, [{"fingerprint": "same-root", "classification": "deliberate-design", "materiality": "material", "proposed_scope": []}], {"remaining_seconds": 120, "review_round": 2, "product_fix_attempt": 0})
        branches = [
            {"branch_id": "regression", "state": "accepted", "complete": True, "terminal_ref": {"id": "aggregate", "digest": aggregate["aggregate_digest"]}},
            {"branch_id": "review", "state": "accepted", "complete": True, "terminal_ref": {"id": "disposition", "digest": disposition["disposition_digest"]}},
        ]
        inventory = {"schema": "declared-branch-inventory/v1", "inventory_id": "branches-001", "candidate_ref": {"id": candidate_value["candidate_id"], "digest": candidate_value["candidate_digest"]}, "branches": [{"branch_id": item["branch_id"], "terminal_ref": copy.deepcopy(item["terminal_ref"])} for item in branches]}
        inventory["inventory_digest"] = object_digest(inventory)
        inventory_ref = {"id": inventory["inventory_id"], "digest": inventory["inventory_digest"]}
        finalization = EvidenceFinalizer().finalize(candidate_value, aggregate, disposition, inventory_ref, inventory, branches)
        head = {"revision": 9, "transaction_digest": DIGESTS["a"]}
        candidate_ref = {"id": "candidate-001", "digest": DIGESTS["a"]}
        issued = issued_inventory(candidate_ref)
        issued_ref = {"id": issued["inventory_id"], "digest": issued["inventory_digest"]}
        cutover = IssuanceWatermarkCutoverPlanner().plan(head, head, {"schema": "issuance-lease/v1", "lease_id": "lease-9", "status": "active", "expected_head": head}, "workflow-execution/v1", "workflow-execution/v2", candidate_ref, issued_ref, issued)
        artifacts = {
            "execution-package-closure-v2.schema.json": closure,
            "artifact-candidate-v1.schema.json": candidate_value,
            "regression-shard-plan-v1.schema.json": plan,
            "command-receipt-v1.schema.json": receipt(plan["shards"][0], closure),
            "receipt-aggregate-v1.schema.json": aggregate,
            "finding-disposition-v1.schema.json": disposition,
            "evidence-finalization-v1.schema.json": finalization,
            "workflow-cutover-v1.schema.json": cutover,
        }
        registry = {path.name: json.loads(path.read_text()) for path in (ROOT / "schemas").glob("*.schema.json")}
        for filename, artifact in artifacts.items():
            with self.subTest(filename=filename):
                schema = json.loads((ROOT / "schemas" / filename).read_text())
                self.assertFalse(schema["additionalProperties"])
                validate_document(artifact, schema, registry)
                unknown = copy.deepcopy(artifact)
                unknown["transition_authority"] = True
                with self.assertRaises(SchemaValidationError):
                    validate_document(unknown, schema, registry)


if __name__ == "__main__":
    unittest.main()
