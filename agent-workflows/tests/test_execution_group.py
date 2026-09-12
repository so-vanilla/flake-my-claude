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
from ai_agent_workflow.macos_task_process import MacOSTaskProcessBroker, MacOSTaskProcessError  # noqa: E402
from test_execution_v2_orchestrator import advisory as orchestrator_advice, command as orchestrator_command, state as orchestrator_state  # noqa: E402
from test_persistent_receipts import package as persistent_package, policy as persistent_policy  # noqa: E402
from test_execution_v2 import execution_input, candidate as v2_candidate, shards as v2_shards, receipt as v2_receipt, review as v2_review, object_digest, workflow_loop_request  # noqa: E402
from ai_agent_workflow.bounded_read_scope import observe_read_scope  # noqa: E402
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


class _ParentVerifiedBroker(MacOSTaskProcessBroker):
    """Narrow E3 seam; broker sealing itself is covered in its own tests."""

    def __init__(self, receipt, expected_ref):
        self.receipt = copy.deepcopy(receipt)
        self.expected_ref = copy.deepcopy(expected_ref)

    def verify_for_e3(self, package, receipt_ref):
        if receipt_ref != self.expected_ref or receipt_ref.get("schema") != "macos-task-process-receipt-ref/v2":
            raise MacOSTaskProcessError("E3 isolation receipt ref is not parent-broker-owned")
        return copy.deepcopy(self.receipt)


def practical_e2(workspace):
    source = execution_package_source()
    source["workspace_identity"] = str(workspace)
    source["command"] = {"argv": [sys.executable, "-c", "pass"], "cwd": str(workspace)}
    source["isolation"]["cwd"] = str(workspace)
    source["resource_claims"] = {"read_paths": ["src"], "write_paths": ["out"], "exclusive_resources": []}
    bound = authority(idempotency_key="practical-v2-key")
    preflight = ExecutionGroupV1().compile("group.E.E1", inputs(), bound, HEAD)
    values = inputs(
        preflight=preflight, preflight_digest=preflight["candidate_digest"],
        execution_package_input=source, assigned_role="worker", assigned_worker="worker-v2",
        output_path="out/task", write_scope=["out"], non_goals=["external mutation"],
        acceptance=["parent verified v2"], stop_conditions=["scope escape"],
        task_process_mode="macos-positive-allowlist",
        broker_state_root=str(workspace / ".agent-workflow/runtime-e-broker"),
        system_read_roots=[str(Path("/usr/bin").resolve())],
        runtime_read_roots=[str(Path(sys.executable).resolve().parent)],
    )
    result = ExecutionGroupV1().compile("group.E.E2", values, bound, HEAD)
    return source, bound, values, result


def practical_e3_inputs(package, root):
    runner, policy = ArtifactCandidateBuilder.runner_inputs(package)
    recovery = ArtifactCandidateBuilder.verify_terminal_reuse(root, runner, policy)
    terminal = recovery["terminal_receipt"]
    release = package["task_process_release"]
    ref = {
        "schema": "macos-task-process-receipt-ref/v2", "receipt_id": "sha256:" + "b" * 64,
        "receipt_digest": "sha256:" + "c" * 64, "broker_id": "broker-v2",
        "threat_profile_digest": release["threat_profile_digest"],
    }
    verified = {
        "schema": "macos-task-process-receipt/v2", "threat_profile": copy.deepcopy(release["threat_profile"]),
        "threat_profile_digest": release["threat_profile_digest"],
        "state_writer_boundary": copy.deepcopy(release["state_writer_boundary"]),
        "limitations_acknowledged": True, "terminal_receipt_digest": terminal["receipt_digest"],
    }
    values = inputs(
        package=package, terminal="DONE", changed_paths=["out/result"], receipt=terminal,
        recovery_receipt=recovery, isolation_receipt_ref=ref, task_id="practical-v2-task",
    )
    return values, verified, ref


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

    def test_macos_dispatch_emits_exact_v2_and_rejects_caller_safety_labels(self):
        with tempfile.TemporaryDirectory(prefix="execution-e2-v2-") as temporary:
            workspace = Path(temporary).resolve()
            (workspace / "src").mkdir(); (workspace / "out").mkdir()
            _, bound, values, result = practical_e2(workspace)
            self.assertEqual(result["package"]["task_process_release"]["schema"], "macos-task-process-release/v2")
            self.assertEqual(result["package"]["task_process_release"]["threat_profile"]["approval_digest"], "sha256:2f0ae19fac7b90778d1b5713c51071f1ab7e96ce179237a1e5892cf320dfaefb")
            for field, value in (
                ("threat_profile", {}), ("limitations_acknowledged", True),
                ("arbitrary_same_uid_atomicity", True), ("publication_sigkill_atomicity", True),
            ):
                with self.subTest(field=field):
                    forged = copy.deepcopy(values); forged[field] = value
                    refused = ExecutionGroupV1().compile("group.E.E2", forged, bound, HEAD)
                    self.assertEqual(refused["schema"], "execution-group-refusal/v1")
                    self.assertIn("caller-authored practical safety labels", refused["reason"])


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
        read_receipt = observe_read_scope(package["read_scope"], ["src/a.py"], observation_complete=True)
        result = ExecutionGroupV1().compile("group.E.E3", inputs(package=package, terminal="DONE", changed_paths=["src/a.py"], receipt=receipt, recovery_receipt=recovery, read_scope_receipt=read_receipt, task_id="t"), bound, HEAD)
        self.assertTrue(result["artifact_candidate"]["frozen"]); self.assertTrue(result["recovery"]["terminal_reused"])
        self.assertFalse(result["artifact_candidate"]["read_scope_receipt"]["os_isolation_enforced"])
        self.assertFalse(result["artifact_candidate"]["read_isolation_acceptance"])
        self.assertEqual(result["artifact_candidate"]["isolation_status"], "rehearsal-only")
        self.assertEqual(result["artifact_candidate"]["artifact_digest"], digest({key: value for key, value in result["artifact_candidate"].items() if key != "artifact_digest"}))
        validate_execution(result)
        forged = copy.deepcopy(recovery); forged["spawn_count"] = 2
        self.assertEqual(ExecutionGroupV1().compile("group.E.E3", inputs(package=package, terminal="DONE", changed_paths=["src/a.py"], receipt=receipt, recovery_receipt=forged, task_id="t"), bound, HEAD)["schema"], "execution-group-refusal/v1")
        self.assertEqual(ExecutionGroupV1().compile("group.E.E3", inputs(package=package, terminal="DONE", write_scope=["src"], changed_paths=["other/a"]), authority(), HEAD)["schema"], "execution-group-refusal/v1")

    def test_parent_verified_practical_v2_and_profile_ref_fail_closed(self):
        with tempfile.TemporaryDirectory(prefix="execution-e3-v2-") as temporary:
            workspace = Path(temporary).resolve(); (workspace / "src").mkdir(); (workspace / "out").mkdir()
            _, bound, _, e2 = practical_e2(workspace)
            self.assertEqual(e2["status"], "issued")
            package = e2["package"]
            with tempfile.TemporaryDirectory(prefix="execution-e3-receipt-") as receipt_root:
                values, verified, ref = practical_e3_inputs(package, receipt_root)
            result = ExecutionGroupV1().compile("group.E.E3", values, bound, HEAD, trusted_isolation_broker=_ParentVerifiedBroker(verified, ref))
            self.assertEqual(result["status"], "submitted")
            self.assertTrue(result["artifact_candidate"]["read_isolation_acceptance"])
            self.assertEqual(result["artifact_candidate"]["isolation_status"], "os-enforced")
            self.assertEqual(result["artifact_candidate"]["read_scope_receipt"], verified)
            validate_execution(result)

            mutations = (
                lambda item: item["threat_profile"].__setitem__("approval_digest", "sha256:" + "0" * 64),
                lambda item: item["threat_profile"]["accepted_residuals"].pop(),
                lambda item: item["state_writer_boundary"].__setitem__("arbitrary_same_uid_atomicity", True),
                lambda item: item["state_writer_boundary"].__setitem__("publication_sigkill_atomicity", True),
                lambda item: item.__setitem__("limitations_acknowledged", False),
            )
            for mutate in mutations:
                forged = copy.deepcopy(verified); mutate(forged)
                refused = ExecutionGroupV1().compile("group.E.E3", values, bound, HEAD, trusted_isolation_broker=_ParentVerifiedBroker(forged, ref))
                self.assertEqual(refused["schema"], "execution-group-refusal/v1")

            mixed = copy.deepcopy(ref); mixed.pop("threat_profile_digest"); mixed["schema"] = "macos-task-process-receipt-ref/v1"
            refused = ExecutionGroupV1().compile("group.E.E3", {**values, "isolation_receipt_ref": mixed}, bound, HEAD, trusted_isolation_broker=_ParentVerifiedBroker(verified, ref))
            self.assertEqual(refused["schema"], "execution-group-refusal/v1")

    def test_historical_v1_cannot_be_relabelled_as_practical_e3(self):
        with tempfile.TemporaryDirectory(prefix="execution-e3-v1-") as temporary:
            workspace = Path(temporary).resolve(); (workspace / "src").mkdir(); (workspace / "out").mkdir()
            _, bound, _, e2 = practical_e2(workspace)
            package = copy.deepcopy(e2["package"])
            release = package["task_process_release"]
            release["schema"] = "macos-task-process-release/v1"
            release.pop("threat_profile"); release.pop("threat_profile_digest"); release.pop("state_writer_boundary")
            release["release_digest"] = digest({key: value for key, value in release.items() if key != "release_digest"})
            with tempfile.TemporaryDirectory(prefix="execution-e3-v1-receipt-") as receipt_root:
                values, verified, ref = practical_e3_inputs(e2["package"], receipt_root)
            values["package"] = package
            v1_ref = {key: value for key, value in ref.items() if key != "threat_profile_digest"}; v1_ref["schema"] = "macos-task-process-receipt-ref/v1"
            values["isolation_receipt_ref"] = v1_ref
            refused = ExecutionGroupV1().compile("group.E.E3", values, bound, HEAD, trusted_isolation_broker=_ParentVerifiedBroker(verified, ref))
            self.assertEqual(refused["schema"], "execution-group-refusal/v1")
            self.assertIn("legacy-unprofiled", refused["reason"])


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

    def test_workflow_loop_zero_finding_routes_to_mechanical_completion(self):
        result = ExecutionGroupV1().compile("group.E.E6", inputs(workflow_loop=workflow_loop_request()), authority(), HEAD)
        self.assertEqual("E8", result["next"])
        self.assertTrue(result["validation"]["validator"]["skipped"])
        validate_execution(result)

    def test_workflow_loop_required_finding_routes_to_repair_batch(self):
        request = workflow_loop_request(required_finding=True)
        result = ExecutionGroupV1().compile("group.E.E6", inputs(workflow_loop=request), authority(), HEAD)
        self.assertEqual("E7", result["next"])
        self.assertEqual("loop-repair-batch-plan/v1", result["validation"]["repair_batch_plan"]["schema"])
        validate_execution(result)

    def test_workflow_loop_e8_emits_non_authorizing_mechanical_close(self):
        result = ExecutionGroupV1().compile("group.E.E8", inputs(workflow_loop=workflow_loop_request()), authority(), HEAD)
        self.assertEqual("converged", result["status"])
        self.assertTrue(result["validation"]["validator"]["skipped"])
        self.assertEqual("loop-machine-decision-receipt/v1", result["aggregate"]["machine_decision_receipt"]["schema"])
        validate_execution(result)


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
