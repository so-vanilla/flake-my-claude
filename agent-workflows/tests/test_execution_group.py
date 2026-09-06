import copy
import hashlib
import json
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "src"))
sys.path.insert(0, str(ROOT / "tests"))
from ai_agent_workflow.execution_group import ArtifactCandidateBuilder, ExecutionGroupV1  # noqa: E402
from test_execution_v2_orchestrator import advisory as orchestrator_advice, command as orchestrator_command, state as orchestrator_state  # noqa: E402
from test_persistent_receipts import package as persistent_package, policy as persistent_policy  # noqa: E402
from test_execution_v2 import execution_input, candidate as v2_candidate, shards as v2_shards, receipt as v2_receipt, review as v2_review, object_digest  # noqa: E402
from ai_agent_workflow.execution_v2 import ExecutionClosureBuilder, RegressionFrontier, ReceiptAggregator, FindingValidator  # noqa: E402
from ai_agent_workflow.schema_validation import SchemaValidationError, validate_document  # noqa: E402


def digest(value):
    return "sha256:" + hashlib.sha256(json.dumps(value, sort_keys=True, separators=(",", ":")).encode()).hexdigest()


def raw(path):
    return "sha256:" + hashlib.sha256((ROOT.parent / path).read_bytes()).hexdigest()


def schema(name):
    return json.loads((ROOT / "schemas" / name).read_text())


def validate_execution(document):
    value = schema("execution-group-v1.schema.json")
    validate_document(document, value, value["$defs"])


HEAD = {"revision": 7, "transaction_digest": "sha256:" + "a" * 64}


def authority(**overrides):
    value = {"authority_id": "authority-1", "actor_id": "orchestrator", "role": "orchestrator", "assignment_id": "assignment-1", "scope_ref": {"path": "scope-1", "digest": "sha256:" + "f" * 64}, "epoch_id": "epoch-1", "lease_id": "lease-1", "idempotency_key": "key-1", "budget": {"seconds": 100, "review_round": 0, "product_fix_attempts": 0}, "expected_head": HEAD}
    value.update(overrides); return value


def inputs(**overrides):
    physical_paths = ["agent-workflows/skills/implementation-readiness-review/SKILL.md", "agent-workflows/groups/planning.json", "agent-workflows/catalog.yaml", "agent-workflows/groups/required-only-feedback-execution-policy-v1.json"]
    kinds = ["readiness-approval", "task-dag", "workspace-receipt", "git-state-receipt"]
    value = {"expected_head": HEAD, "loop_level": "artifact", "readiness_checks": {key: True for key in ("authority", "expected_head", "lease", "dependencies", "conflicts", "blockers", "purpose", "budget", "workspace", "git")}, "authority_ref": {"authority_id": "authority-1", "role": "orchestrator", "assignment_id": "assignment-1", "lease_id": "lease-1", "scope_ref": {"path": "scope-1", "digest": "sha256:" + "f" * 64}}, "required_only_policy_ref": {"path": "agent-workflows/groups/required-only-feedback-execution-policy-v1.json", "digest": raw("agent-workflows/groups/required-only-feedback-execution-policy-v1.json")}, "physical_refs": [{"kind": kind, "path": path, "version": "v1", "digest": raw(path), "creator": "planner", "revision": 1, "fresh": True} for kind, path in zip(kinds, physical_paths)]}
    value.update(overrides); return value


def review(axis, actor, epoch, findings):
    package = "b" if axis == "architecture-safety" else "e"
    return {"schema": "ordinary-review-report/v1", "report_id": "report-" + axis, "axis": axis, "actor_id": actor, "context_epoch_id": epoch, "package_digest": "sha256:" + package * 64, "candidate_digest": "sha256:" + "c" * 64, "aggregate_digest": "sha256:" + "d" * 64, "findings": findings}


def execution_package_source():
    d = "sha256:" + "a" * 64
    return {"schema": "execution-package-input/v2", "package_id": "package-e2", "contract_version": "workflow-execution/v2", "workspace_identity": "/workspace", "candidate_ref": {"id": "candidate", "digest": d}, "test_refs": [{"id": "test", "digest": d}], "fixture_refs": [], "schema_refs": [{"id": "schema", "digest": d}], "config_refs": [], "lock_refs": [{"id": "lock", "digest": d}], "toolchain": {"executable_digest": d, "identity": "python"}, "command": {"argv": ["python"], "cwd": "/workspace"}, "environment": {}, "isolation": {"cwd": "/workspace", "temporary_namespace": "tmp", "output_namespace": "out"}, "resource_claims": {"read_paths": ["src"], "write_paths": [], "exclusive_resources": []}, "supervision": {"timeout_seconds": 1, "grace_seconds": 1, "signals": ["TERM"], "heartbeat_seconds": 1, "terminal_publication_seconds": 1}, "external_input_refs": []}


def frozen_binding(): return {key: "sha256:" + letter * 64 for key, letter in zip(("candidate_digest", "aggregate_digest", "spec_digest", "closure_digest", "budget_digest"), "abcde")}
def detailed_finding(): return {"finding_id": "f", "fingerprint": "fp", "background": "background", "as_is": "as-is", "to_be": "to-be", "gap": "gap", "requirement_refs": ["R1"], "evidence_refs": ["E1"], "severity": "major", "blocking_proposal": "blocking", "owner_proposal": "owner"}
def sibling(): return [{"id": "sibling", "digest": "sha256:" + "a" * 64, "status": "accepted"}]
def siblings_for(join): return [{"id": item["terminal_ref"]["id"], "digest": item["terminal_ref"]["digest"], "status": "accepted"} for item in ExecutionGroupV1.v2_join(join)["finalization"]["branches"]]
def verification_receipts():
    candidate, closure = "sha256:" + "a" * 64, "sha256:" + "b" * 64
    values = {}
    for name in ("integration", "e2e", "regression", "objective"):
        receipt = {"schema": "whole-verification-receipt/v1", "category": name, "candidate_digest": candidate, "closure_digest": closure, "command": {"argv": [name]}, "environment": {"isolated": True}, "capture": {"state": "complete"}, "terminal": True, "status": "passed"}
        receipt["receipt_digest"] = digest(receipt); values[name] = receipt
    return values


def v2_join_inputs(required=False):
    closure = ExecutionClosureBuilder().freeze(execution_input()); candidate = v2_candidate(closure)
    plan = RegressionFrontier().plan(candidate, closure, v2_shards()); receipts = [v2_receipt(item, closure) for item in plan["shards"]]
    aggregate = ReceiptAggregator().aggregate(candidate, plan, receipts); reviews = [v2_review("architecture-safety", "A"), v2_review("integration-operability", "B")]
    for item in reviews: item["aggregate_digest"] = aggregate["aggregate_digest"]
    classification = "required" if required else "deliberate-design"; dispositions = [{"fingerprint": "same-root", "classification": classification, "materiality": "material", "proposed_scope": []}]
    validated = FindingValidator().validate(reviews, dispositions, {"remaining_seconds": 1, "review_round": 1, "product_fix_attempt": 0})
    branches = [{"branch_id": "regression", "state": "accepted", "complete": True, "terminal_ref": {"id": "aggregate", "digest": aggregate["aggregate_digest"]}}, {"branch_id": "review", "state": "accepted", "complete": True, "terminal_ref": {"id": "disposition", "digest": validated["disposition_digest"]}}]
    inventory = {"schema": "declared-branch-inventory/v1", "inventory_id": "branches", "candidate_ref": {"id": candidate["candidate_id"], "digest": candidate["candidate_digest"]}, "branches": [{"branch_id": b["branch_id"], "terminal_ref": b["terminal_ref"]} for b in branches]}; inventory["inventory_digest"] = object_digest(inventory)
    return {"candidate": candidate, "plan": plan, "receipts": receipts, "reviews": reviews, "dispositions": dispositions, "observed_budget": {"remaining_seconds": 1, "review_round": 1, "product_fix_attempt": 0}, "inventory_ref": {"id": inventory["inventory_id"], "digest": inventory["inventory_digest"]}, "inventory": inventory, "branches": branches}


class E1_ExecutionPreflightTests(unittest.TestCase):
    def test_fresh_closed_preflight_and_stale_head_budget_refusal(self):
        result = ExecutionGroupV1().compile("group.E.E1", inputs(), authority(), HEAD)
        self.assertEqual(result["status"], "ready")
        validate_execution(result)
        self.assertEqual(ExecutionGroupV1().compile("group.E.E1", inputs(expected_head={**HEAD, "revision": 8}), authority(), HEAD)["schema"], "execution-group-refusal/v1")
        self.assertEqual(ExecutionGroupV1().compile("group.E.E1", inputs(), authority(budget={"seconds": 0, "review_round": 0, "product_fix_attempts": 0}), HEAD)["schema"], "execution-group-refusal/v1")


class E2_DispatchTaskTests(unittest.TestCase):
    def test_exact_e1_and_singular_luna_package(self):
        preflight = ExecutionGroupV1().compile("group.E.E1", inputs(), authority(), HEAD)
        result = ExecutionGroupV1().compile("group.E.E2", inputs(preflight=preflight, preflight_digest=preflight["candidate_digest"], execution_package_input=execution_package_source(), assigned_role="worker", assigned_worker="worker-1", output_path="out/task", write_scope=["src"], non_goals=["none"], acceptance=["test"], stop_conditions=["budget"]), authority(), HEAD)
        self.assertEqual(result["package"]["model"], "gpt-5.6-luna")
        validate_execution(result)
        self.assertEqual(ExecutionGroupV1().compile("group.E.E2", inputs(preflight={}), authority(), HEAD)["schema"], "execution-group-refusal/v1")


class E3_ExecuteSmallLoopTests(unittest.TestCase):
    def test_candidate_and_scope_retry_recovery_boundary(self):
        bound = authority(idempotency_key="receipt-key")
        preflight = ExecutionGroupV1().compile("group.E.E1", inputs(), bound, HEAD)
        package = ExecutionGroupV1().compile("group.E.E2", inputs(preflight=preflight, preflight_digest=preflight["candidate_digest"], execution_package_input=execution_package_source(), assigned_role="worker", assigned_worker="worker-1", output_path="out/task", write_scope=["src"], non_goals=["none"], acceptance=["test"], stop_conditions=["budget"]), bound, HEAD)["package"]
        with tempfile.TemporaryDirectory() as root:
            runner_package, runner_policy = ArtifactCandidateBuilder.runner_inputs(package)
            recovery = ArtifactCandidateBuilder.verify_terminal_reuse(root, runner_package, runner_policy)
            changed = copy.deepcopy(runner_package); changed["execution_closure"]["command"]["argv"] = ["changed"]
            ArtifactCandidateBuilder.refuse_ambiguous_replacement(root, runner_package, changed, runner_policy)
        receipt = recovery["terminal_receipt"]
        result = ExecutionGroupV1().compile("group.E.E3", inputs(package=package, terminal="DONE", changed_paths=["src/a.py"], receipt=receipt, recovery_receipt=recovery, task_id="t"), bound, HEAD)
        self.assertTrue(result["artifact_candidate"]["frozen"]); self.assertTrue(result["recovery"]["terminal_reused"])
        validate_execution(result)
        forged = copy.deepcopy(recovery); forged["spawn_count"] = 2
        self.assertEqual(ExecutionGroupV1().compile("group.E.E3", inputs(package=package, terminal="DONE", changed_paths=["src/a.py"], receipt=receipt, recovery_receipt=forged, task_id="t"), bound, HEAD)["schema"], "execution-group-refusal/v1")
        self.assertEqual(ExecutionGroupV1().compile("group.E.E3", inputs(package=package, terminal="DONE", write_scope=["src"], changed_paths=["other/a"]), authority(), HEAD)["schema"], "execution-group-refusal/v1")


class E4_ReviewTaskSpecTests(unittest.TestCase):
    def test_fresh_spec_reviewer(self):
        result = ExecutionGroupV1().compile("group.E.E4", inputs(axis="architecture-safety", actor_id="spec", reviewer_epoch_id="review-epoch-spec", other_reviewer_epoch_id="review-epoch-quality", worker_actor_id="worker", other_reviewer_actor_id="quality", frozen_binding=frozen_binding(), findings=[detailed_finding()]), authority(), HEAD)
        self.assertEqual(result["status"], "reviewed")
        validate_execution(result)
        self.assertEqual(ExecutionGroupV1().compile("group.E.E4", inputs(axis="architecture-safety", actor_id="worker", reviewer_epoch_id="review-epoch-worker", other_reviewer_epoch_id="review-epoch-other", worker_actor_id="worker", other_reviewer_actor_id="quality", frozen_binding=frozen_binding(), findings=[detailed_finding()]), authority(), HEAD)["schema"], "execution-group-refusal/v1")


class E5_ReviewTaskQualityTests(unittest.TestCase):
    def test_fresh_quality_reviewer(self):
        result = ExecutionGroupV1().compile("group.E.E5", inputs(axis="integration-operability", actor_id="quality", reviewer_epoch_id="review-epoch-quality", other_reviewer_epoch_id="review-epoch-spec", worker_actor_id="worker", other_reviewer_actor_id="spec", frozen_binding=frozen_binding(), findings=[detailed_finding()]), authority(), HEAD)
        self.assertEqual(result["status"], "reviewed")
        validate_execution(result)


class E6_ValidateReviewFindingsTests(unittest.TestCase):
    def test_required_only_routes_and_coverage(self):
        finding = {"finding_id": "f1", "fingerprint": "fp1", "severity": "major", "summary": "gap"}
        reviews = [review("architecture-safety", "spec", "epoch-spec", [finding]), review("integration-operability", "quality", "epoch-quality", [])]
        base = inputs(reviews=reviews, dispositions=[{"fingerprint": "fp1", "classification": "required", "materiality": "material", "proposed_scope": ["src"]}], observed_budget={"remaining_seconds": 9, "review_round": 0, "product_fix_attempt": 0})
        result = ExecutionGroupV1().compile("group.E.E6", base, authority(), HEAD)
        self.assertEqual(result["next"], "E7")
        validate_execution(result)
        no_required = copy.deepcopy(base); no_required["dispositions"][0]["classification"] = "too-minor"
        self.assertEqual(ExecutionGroupV1().compile("group.E.E6", no_required, authority(), HEAD)["next"], "E8")
        missing = copy.deepcopy(base); missing["dispositions"] = []
        self.assertEqual(ExecutionGroupV1().compile("group.E.E6", missing, authority(), HEAD)["schema"], "execution-group-refusal/v1")


class E7_FixAndRereviewTests(unittest.TestCase):
    def test_required_only_and_fresh_e6(self):
        advice = orchestrator_advice()
        outer_head = {"revision": 9, "transaction_digest": "sha256:" + "a" * 64}
        bound = authority(assignment_id="root-orchestrator", lease_id="lease-new", expected_head=outer_head)
        outer_ref = {"authority_id": bound["authority_id"], "role": bound["role"], "assignment_id": bound["assignment_id"], "lease_id": bound["lease_id"], "scope_ref": bound["scope_ref"]}
        result = ExecutionGroupV1().compile("group.E.E7", inputs(expected_head=outer_head, authority_ref=outer_ref, orchestrator_validated=True, advice="required", orchestrator_command=orchestrator_command(advice), observed_state=orchestrator_state(advice)), bound, outer_head)
        self.assertEqual(result["next"], "E6")
        validate_execution(result)
        self.assertEqual(ExecutionGroupV1().compile("group.E.E7", inputs(orchestrator_validated=False, advice="required"), authority(), HEAD)["schema"], "execution-group-refusal/v1")


class E8_ConvergeParallelBatchTests(unittest.TestCase):
    def test_open_required_and_incomplete_rejected(self):
        join = v2_join_inputs()
        result = ExecutionGroupV1().compile("group.E.E8", inputs(v2_join=join, sibling_refs=siblings_for(join), complete=True, conflicting=False, open_required=False), authority(), HEAD)
        self.assertEqual(result["status"], "converged")
        validate_execution(result)
        self.assertEqual(ExecutionGroupV1().compile("group.E.E8", inputs(v2_join=join, sibling_refs=siblings_for(join), complete=True, conflicting=False, open_required=True), authority(), HEAD)["schema"], "execution-group-refusal/v1")


class E9_VerifyWholeChangeTests(unittest.TestCase):
    def test_open_required_and_budget_are_stops(self):
        result = ExecutionGroupV1().compile("group.E.E9", inputs(candidate_digest="sha256:" + "a" * 64, closure_digest="sha256:" + "b" * 64, verification_receipts=verification_receipts(), complete=True, open_required=False), authority(), HEAD)
        self.assertEqual(result["status"], "verified")
        validate_execution(result)
        self.assertEqual(ExecutionGroupV1().compile("group.E.E9", inputs(candidate_digest="sha256:" + "a" * 64, closure_digest="sha256:" + "b" * 64, verification_receipts=verification_receipts(), complete=True, open_required=True), authority(), HEAD)["schema"], "execution-group-refusal/v1")


class E10_ArbitrateExceptionTests(unittest.TestCase):
    def test_exception_terminal_only(self):
        bound = authority(); terminal = {"terminal_id": "terminal", "reason": "budget", "type": "stopped-budget", "expected_head": HEAD, "authority_ref": {"authority_id": bound["authority_id"], "lease_id": bound["lease_id"], "assignment_id": bound["assignment_id"]}}
        terminal["terminal_digest"] = digest(terminal); ref = {"id": "terminal", "digest": terminal["terminal_digest"]}; options = [{"id": "stop", "tradeoff": "tradeoff", "next_action": "ask", "human_required": True}]
        result = ExecutionGroupV1().compile("group.E.E10", inputs(normal_loop_terminal=True, exception=True, terminal=terminal, terminal_ref=ref, options=options, human_gate=True, reopen_authorized=False), bound, HEAD)
        self.assertEqual(result["status"], "advisory-options")
        validate_execution(result)
        self.assertEqual(ExecutionGroupV1().compile("group.E.E10", inputs(normal_loop_terminal=False, exception=True, terminal=terminal, terminal_ref=ref, options=options, human_gate=True, reopen_authorized=False), bound, HEAD)["schema"], "execution-group-refusal/v1")


class ExecutionGroupAddendumTests(unittest.TestCase):
    def test_candidate_levels_invalidation_and_late_rejection(self):
        candidate = ArtifactCandidateBuilder().build({"task_id": "t", "candidate_digest": "sha256:" + "d" * 64, "receipt_digest": "sha256:" + "e" * 64, "changed_paths": [], "terminal": "DONE"}, {"terminal": True, "status": "passed", "receipt_digest": "sha256:" + "e" * 64})
        self.assertTrue(candidate["non_authorizing"])
        for level in ("artifact", "section", "workflow"):
            join = v2_join_inputs(); self.assertEqual(ExecutionGroupV1().compile("group.E.E8", inputs(loop_level=level, v2_join=join, sibling_refs=siblings_for(join), complete=True, conflicting=False, open_required=False), authority(), HEAD)["loop_level"], level)
        invalidated = ExecutionGroupV1.invalidate("a", {"a": [], "b": ["a"], "c": []}, ["a", "b", "c"], {"bound_input_digest": "old", "current_input_digest": "new"})
        self.assertEqual(invalidated["invalidated"], ["a", "b"]); self.assertEqual(invalidated["preserved"], ["c"]); self.assertEqual(invalidated["late_result"], "rejected")


if __name__ == "__main__": unittest.main()
