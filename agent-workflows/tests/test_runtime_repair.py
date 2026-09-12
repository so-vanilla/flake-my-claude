"""Focused staged-CAS tests for the post-E6 practical repair protocol."""
import copy
import hashlib
import json
import shutil
import subprocess
import sys
import unittest
from pathlib import Path
from tempfile import TemporaryDirectory

ROOT = Path(__file__).resolve().parents[1]
sys.path[:0] = [str(ROOT / "src"), str(ROOT / "tests"), str(ROOT / "examples" / "support-report")]

from ai_agent_workflow.control_kernel import ControlKernel  # noqa: E402
from ai_agent_workflow.execution_group import (  # noqa: E402
    ArtifactCandidateBuilder,
    ExecutionGroupV1,
)
from ai_agent_workflow.inception_runtime import InceptionRuntime  # noqa: E402
from ai_agent_workflow.loop_contracts import canonical_digest  # noqa: E402
from ai_agent_workflow.macos_task_process import MacOSTaskProcessBroker  # noqa: E402
from ai_agent_workflow.runtime_execution import RuntimeExecution  # noqa: E402
from ai_agent_workflow.runtime_repair import (  # noqa: E402
    ACCEPTED_RESIDUAL,
    TRUST_PROFILE,
    RuntimeRepairCoordinator,
    RuntimeRepairError,
    WorkflowLoopRepairCoordinator,
    assert_production_adoptable,
)
from test_execution_group import HEAD  # noqa: E402
from test_execution_group import authority as compiler_authority
from test_execution_group import inputs as compiler_inputs
from test_runtime_execution import (  # noqa: E402
    RuntimeExecutionIntegrationTests,
    digest_bytes,
)

AXES = ("architecture/safety", "integration/operability/time/dotfiles")
LOOP_AXES = ("architecture-safety", "integration-operability")
LOOP_DIGEST = "sha256:" + "b" * 64


class WorkflowLoopRepairProtocolTests(unittest.TestCase):
    """Focused tests for the additive workflow-loop/v1 repair adapter."""

    def setUp(self):
        self._temporary = TemporaryDirectory()
        root = Path(self._temporary.name)
        self.kernel = ControlKernel(root, "loop-repair")
        self.authority = {
            "status": "approved",
            "scopes": ["*"],
            "fixture_identity": {
                "schema": "canonical-fixture-identity/v1",
                "run_id": "loop-repair",
                "namespace": "fixture:loop-repair",
                "approval_scope": "fixture-only",
            },
        }
        self.identity = {
            "schema": "loop-work-identity/v1",
            "work_lineage_id": "lineage-repair",
            "logical_task_id": "task-repair",
            "phase": "E3",
            "scope_revision": "scope-r1",
            "requirements_digest": LOOP_DIGEST,
            "predecessor_ref": None,
        }
        self.kernel.entry(
            {"path": "objectives/loop.md", "version": "v001", "digest": "c" * 64},
            authority_ref=self.authority,
            loop_control={"identity": self.identity, "history": []},
        )
        self.coordinator = WorkflowLoopRepairCoordinator(kernel=self.kernel, run_id="loop-repair")

    def tearDown(self):
        self._temporary.cleanup()

    @staticmethod
    def _finding(identifier="F1"):
        return {
            "finding_id": identifier,
            "fingerprint": "fingerprint-" + identifier,
            "classification": "required",
            "candidate_digest": LOOP_DIGEST,
            "batch_key": "root-a",
            "root_cause": "same-root",
            "write_scope": ["src/a.py"],
            "verification": ["test-a"],
            "depends_on": [],
            "conflicts_with": [],
            "resolution_conditions": ["test-a passes"],
        }

    @staticmethod
    def _evidence():
        value = {
            "schema": "loop-evidence-record/v1",
            "evidence_id": "evidence-1",
            "evidence_digest": "",
            "candidate_digest": LOOP_DIGEST,
            "spec_digest": LOOP_DIGEST,
            "source_digest": LOOP_DIGEST,
            "dependency_digest": LOOP_DIGEST,
            "environment_digest": LOOP_DIGEST,
            "check_definition_digest": LOOP_DIGEST,
            "coverage": ["R1"],
            "status": "pass",
        }
        value["evidence_digest"] = canonical_digest(
            {key: item for key, item in value.items() if key != "evidence_digest"}
        )
        return value

    def test_durable_reserve_running_accept_and_explicit_unknown_recovery(self):
        reserved = self.coordinator.reserve({"command_id": "command-initial"})
        self.assertEqual("reserved", reserved["event"]["status"])
        self.assertEqual(3, reserved["policy"]["additional_iteration_limit"])
        self.assertEqual(1, reserved["policy"]["technical_retry_limit"])
        self.assertNotIn("review_budget", self.kernel.read_state())
        replayed = self.coordinator.reserve({"command_id": "command-initial"})
        self.assertEqual(reserved["revision"], replayed["revision"])

        running = self.coordinator.mark_running({"event_id": "event-command-initial"})
        self.assertEqual("running", running["event"]["status"])
        unknown = self.coordinator.mark_execution_unknown({"event_id": "event-command-initial"})
        self.assertEqual("recovery-required", unknown["loop_control"]["outcome"])
        with self.assertRaises(RuntimeRepairError):
            self.coordinator.accept_result(
                {"event_id": "event-command-initial", "result_ref": {"id": "late", "digest": LOOP_DIGEST}}
            )
        recovered = self.coordinator.recover_execution(
            {
                "event_id": "event-command-initial",
                "resolution": "retry",
                "evidence_ref": {"id": "recovery-proof", "digest": LOOP_DIGEST},
                "retry_command_id": "command-retry",
                "retry_event_id": "event-command-retry",
            }
        )
        self.assertEqual("technical-retry", recovered["event"]["kind"])
        self.assertEqual(1, recovered["event"]["attempt"])

    def test_repair_batch_and_fresh_two_axis_delta_review(self):
        finding = self._finding()
        batch_result = self.coordinator.repair_batch(
            {
                "findings": [finding],
                "candidate_digest": LOOP_DIGEST,
                "resolutions": [
                    {
                        "finding_id": "F1",
                        "status": "resolved",
                        "evidence_refs": [{"id": "fix-proof", "digest": LOOP_DIGEST}],
                        "unresolved_conditions": [],
                    }
                ],
            }
        )
        self.assertTrue(batch_result["complete"])

        candidate = {
            "candidate_ref": {"id": "candidate", "digest": LOOP_DIGEST},
            "spec_ref": {"id": "spec", "digest": LOOP_DIGEST},
            "dependency_refs": [{"id": "dependency", "digest": LOOP_DIGEST}],
            "environment_ref": {"id": "environment", "digest": LOOP_DIGEST},
            "source_paths": ["src/a.py"],
        }
        requirements = [{"requirement_id": "R1", "requirement_ref": {"id": "R1", "digest": LOOP_DIGEST}, "scope": ["src/a.py"]}]
        prior = [{
            "finding_id": "F1", "finding_ref": {"id": "F1", "digest": LOOP_DIGEST},
            "status": "resolved", "scope": ["src/a.py"],
            "resolution_ref": {"id": "resolution-F1", "digest": LOOP_DIGEST},
        }]
        impact = {
            "known": True, "changed_paths": ["src/a.py"],
            "affected_requirements": ["R1"], "affected_interfaces": [], "affected_tests": ["test-a"],
        }
        package_set = self.coordinator.build_review_packages(
            candidate,
            requirements,
            prior,
            impact,
            assignments={
                "architecture-safety": {"assignment_id": "a", "actor_id": "actor-a", "context_epoch": "epoch-a"},
                "integration-operability": {"assignment_id": "b", "actor_id": "actor-b", "context_epoch": "epoch-b"},
            },
        )
        self.assertEqual({"delta"}, {item["mode"] for item in package_set["packages"]})
        results = [
            {
                "schema": "loop-review-assessment/v1", "review_id": "review-" + axis,
                "axis": axis, "actor_id": package["assignment"]["actor_id"],
                "context_epoch": package["assignment"]["context_epoch"],
                "candidate_digest": LOOP_DIGEST, "package_digest": package["package_digest"],
                "coverage": ["R1"], "completed": True, "unevaluated": [], "finding_refs": [],
            }
            for axis, package in ((item["axis"], item) for item in package_set["packages"])
        ]
        accepted = self.coordinator.accept_review_packages(package_set, results)
        self.assertTrue(accepted["complete"])

    def test_strict_zero_finding_completion_issues_machine_receipt_only(self):
        evidence = self._evidence()
        request = {
            "schema": "workflow-loop/v1",
            "identity": self.identity,
            "candidate_digest": LOOP_DIGEST,
            "package_digest": LOOP_DIGEST,
            "requirements": [{
                "schema": "loop-requirement-assessment/v1", "requirement_id": "R1",
                "status": "pass", "scope": ["src/a.py"],
                "evidence_refs": [{"id": evidence["evidence_id"], "digest": evidence["evidence_digest"]}],
            }],
            "reviews": [
                {"schema": "loop-review-assessment/v1", "review_id": "review-a", "axis": "architecture-safety", "actor_id": "actor-a", "context_epoch": "epoch-a", "candidate_digest": LOOP_DIGEST, "package_digest": LOOP_DIGEST, "coverage": ["R1"], "completed": True, "unevaluated": [], "finding_refs": []},
                {"schema": "loop-review-assessment/v1", "review_id": "review-b", "axis": "integration-operability", "actor_id": "actor-b", "context_epoch": "epoch-b", "candidate_digest": LOOP_DIGEST, "package_digest": LOOP_DIGEST, "coverage": ["R1"], "completed": True, "unevaluated": [], "finding_refs": []},
            ],
            "evidence": [evidence],
            "findings": [],
            "remaining_seconds": 0,
            "wall_clock_minutes": 0,
        }
        result = self.coordinator.complete(request, receipt_id="machine-repair-1")
        self.assertTrue(result["validator"]["skipped"])
        self.assertEqual("loop-machine-decision-receipt/v1", result["machine_decision_receipt"]["schema"])
        self.assertFalse(result["machine_decision_receipt"]["human_approval"])
        self.assertTrue(self.coordinator.classify_completion(request)["completed"])



class ProductionAdoptionGateTests(unittest.TestCase):
    def test_gate_binds_real_claim_to_the_same_run_identity(self):
        rehearsal_identity = {"run_id": "fixture", "namespace": "rehearsal:fixture", "mode": "rehearsal"}
        rehearsal = {
            "schema": "runtime-practical-repair-final/v1",
            "runtime_identity": rehearsal_identity,
            "execution_claim": {"mode": "rehearsal", "authoritative": False, "production_closure": False},
        }
        with self.assertRaises(RuntimeRepairError):
            assert_production_adoptable(rehearsal, rehearsal_identity)
        altered = copy.deepcopy(rehearsal)
        altered["execution_claim"] = {"mode": "real", "authoritative": True, "production_closure": True}
        with self.assertRaises(RuntimeRepairError):
            assert_production_adoptable(altered, rehearsal_identity)
        real_identity = {"run_id": "fixture", "namespace": "project:fixture", "mode": "real"}
        real = copy.deepcopy(altered)
        real["runtime_identity"] = real_identity
        self.assertIsNone(assert_production_adoptable(real, real_identity))


@unittest.skipUnless(
    sys.platform == "darwin" and shutil.which("sandbox-exec"), "macOS sandbox-exec only"
)
class RuntimeRepairProtocolTests(unittest.TestCase):
    package = RuntimeExecutionIntegrationTests.package
    stage_inputs = RuntimeExecutionIntegrationTests.stage_inputs
    _required_e7 = RuntimeExecutionIntegrationTests._required_e7
    _e7_json_ref = RuntimeExecutionIntegrationTests._e7_json_ref

    def setUp(self):
        RuntimeExecutionIntegrationTests.setUp(self)
        subprocess.run(["git", "init", "-q", str(self.project)], check=True, timeout=10)
        subprocess.run(["git", "-C", str(self.project), "config", "user.name", "Runtime Repair Test"], check=True, timeout=10)
        subprocess.run(["git", "-C", str(self.project), "config", "user.email", "runtime-repair@example.invalid"], check=True, timeout=10)

    def git_commit(self, message):
        subprocess.run(["git", "-C", str(self.project), "add", "--", "sla_report/core.py"], check=True, timeout=10)
        subprocess.run(["git", "-C", str(self.project), "commit", "-q", "-m", message], check=True, timeout=10)
        return subprocess.run(
            ["git", "-C", str(self.project), "rev-parse", "HEAD"],
            check=True, capture_output=True, text=True, timeout=10,
        ).stdout.strip()

    def physical_json(self, name, value):
        path = self.project / ".local/agent/support-report-trial/repair" / name
        path.parent.mkdir(parents=True, exist_ok=True)
        raw = json.dumps(value, sort_keys=True, separators=(",", ":")).encode("utf-8")
        path.write_bytes(raw)
        return {"path": str(path.relative_to(self.project)), "digest": "sha256:" + hashlib.sha256(raw).hexdigest()}

    def kernel_value(self, coordinator, ref):
        return coordinator._kernel_value(ref, "test fixture")

    def cas(self, coordinator, result):
        state = self.kernel_value(coordinator, result["attempt_state_ref"])
        return {
            "attempt_id": result["attempt_id"],
            "nonce": state["nonce"],
            "previous_state_ref": result["attempt_state_ref"],
            "expected_head": result["head"],
        }

    def verification_plan(self, label, broker_root):
        verification_output = "sla_report/verification-" + label + ".txt"
        script = self.project / ".local/agent/support-report-trial" / ("verify-" + label + ".py")
        script.write_text(
            "import pathlib, sys\n"
            "text = pathlib.Path(sys.argv[1]).read_text(encoding='utf-8')\n"
            "raise SystemExit(0 if '# repaired runtime output' in text else 19)\n",
            encoding="utf-8",
        )
        python = str(Path(sys.executable).resolve(strict=True))
        evidence_digest = digest_bytes(script)
        package_source = {
            "schema": "execution-package-input/v2",
            "package_id": "repair-verification-" + label,
            "contract_version": "workflow-execution/v2",
            "workspace_identity": str(self.project),
            "candidate_ref": {"id": "repair-verifier-" + label, "digest": evidence_digest},
            "test_refs": [{"id": "repair-verifier", "digest": evidence_digest}],
            "fixture_refs": [],
            "schema_refs": [{"id": "repair-verifier-schema", "digest": evidence_digest}],
            "config_refs": [],
            "lock_refs": [{"id": "python-runtime", "digest": digest_bytes(python)}],
            "toolchain": {"executable_digest": digest_bytes(python), "identity": "python3"},
            "command": {"argv": [python, "-I", "-S", str(script), str(self.project / "sla_report/core.py")], "cwd": str(self.project)},
            "environment": {},
            "isolation": {"cwd": str(self.project), "temporary_namespace": "repair-tmp", "output_namespace": "repair-out"},
            "resource_claims": {"read_paths": ["sla_report"], "write_paths": [verification_output], "exclusive_resources": []},
            "supervision": {"timeout_seconds": 10, "grace_seconds": 1, "signals": ["TERM", "KILL"], "heartbeat_seconds": 1, "terminal_publication_seconds": 1},
            "external_input_refs": [],
        }
        bound = compiler_authority(idempotency_key="repair-verification-" + label)
        preflight = ExecutionGroupV1().compile("group.E.E1", compiler_inputs(), bound, HEAD)
        python_root = str(Path("/Library/Developer/CommandLineTools").resolve(strict=True))
        system_roots = [path for path in ("/System", "/usr/lib", "/usr/share", "/usr/bin", "/bin", "/dev") if Path(path).is_dir()]
        e2 = ExecutionGroupV1().compile(
            "group.E.E2",
            compiler_inputs(
                preflight=preflight,
                preflight_digest=preflight["candidate_digest"],
                execution_package_input=package_source,
                assigned_role="worker",
                assigned_worker="repair-verifier-" + label,
                output_path=verification_output,
                write_scope=[verification_output],
                non_goals=["mutation"],
                acceptance=["post-fix marker exists"],
                stop_conditions=["marker absent"],
                task_process_mode="macos-positive-allowlist",
                broker_state_root=str(broker_root),
                system_read_roots=system_roots,
                runtime_read_roots=[python_root],
            ),
            bound,
            HEAD,
        )
        self.assertEqual(e2.get("status"), "issued", e2)
        runner, policy = ArtifactCandidateBuilder.runner_inputs(e2["package"])
        allowed = self.project / ".local/agent/support-report-trial/objective.json"
        return self.physical_json("plan-" + label + ".json", {
            "execution_package": e2["package"],
            "runner_package": runner,
            "policy": policy,
            "probes": [
                {"operation": "content-read", "path": str(allowed), "expected": "allowed"},
                {"operation": "content-read", "path": str(self.sibling / "secret.txt"), "expected": "denied"},
            ],
        })

    def required_frontier(self):
        package = self.package()
        stages = self.stage_inputs(package, required=True)
        stages.pop("E7")
        runtime = RuntimeExecution(self.project, "support-report")
        broker_root = self.project / ".agent-workflow/runtime-e-broker"
        broker = MacOSTaskProcessBroker(broker_root, capability=b"i" * 32, boot_id="repair-initial")
        python_root = str(Path("/Library/Developer/CommandLineTools").resolve(strict=True))
        system_roots = [path for path in ("/System", "/usr/lib", "/usr/share", "/usr/bin", "/bin", "/dev") if Path(path).is_dir()]
        allowed = self.project / ".local/agent/support-report-trial/objective.json"
        try:
            result = runtime.execute(
                "parse-select", package, worker_assignment_id="runtime-worker", broker=broker,
                probes=[
                    {"operation": "content-read", "path": str(allowed), "expected": "allowed"},
                    {"operation": "content-read", "path": str(self.sibling / "secret.txt"), "expected": "denied"},
                ],
                system_read_roots=system_roots,
                runtime_read_roots=[python_root],
                stage_inputs=stages,
                changed_paths=["sla_report/core.py"],
            )
        finally:
            broker.close()
        self.assertEqual(result["status"], "repair_required")
        return result

    @staticmethod
    def assignment(prefix, axis=None):
        value = {
            "assignment_id": prefix + "-assignment",
            "actor_id": prefix + "-actor",
            "host_task_id": prefix + "-host",
            "parent_task_id": "repair-parent",
        }
        if axis is not None:
            value["axis"] = axis
        return value

    def begin_attempt(self):
        initial = self.required_frontier()
        baseline_commit = self.git_commit("runtime repair baseline")
        coordinator = RuntimeRepairCoordinator(self.project, "support-report")
        focused_root = self.project / ".agent-workflow/repair-focused-broker"
        whole_root = self.project / ".agent-workflow/repair-whole-broker"
        focused_plan = self.verification_plan("focused", focused_root)
        whole_plan = self.verification_plan("whole", whole_root)
        worker_assignment = self.assignment("repair-worker")
        # Native resolution is owned by the live Task assignment.  The fresh
        # repair Worker has a new actor/host package while retaining that
        # Kernel assignment identity.
        worker_assignment["assignment_id"] = "runtime-worker"
        begin_inputs = {
            "task_id": initial["task_id"],
            "finding_id": initial["finding_id"],
            "initial_e6_ref": initial["initial_e6_ref"],
            "expected_head": initial["head"],
            "worker_assignment": worker_assignment,
            "focused_plan_ref": focused_plan,
            "whole_plan_ref": whole_plan,
        }
        begun = coordinator.begin(begin_inputs)
        begun["fixture_baseline_commit"] = baseline_commit
        return coordinator, begun, begin_inputs, focused_root, whole_root

    def role_material(self, coordinator, current, assignment, package_ref, role, axis, findings=None, dispositions=None, observed=None):
        current_cas = self.cas(coordinator, current)
        report = {
            "schema": "repair-role-output/v1",
            "attempt_id": current_cas["attempt_id"],
            "nonce": current_cas["nonce"],
            "role_package_ref": package_ref,
            "predecessor_state_ref": current_cas["previous_state_ref"],
            "assignment_id": assignment["assignment_id"],
            "actor_id": assignment["actor_id"],
            "host_task_id": assignment["host_task_id"],
        }
        if axis is not None:
            report["axis"] = axis
        if findings is not None:
            report["findings"] = copy.deepcopy(findings)
        if dispositions is not None:
            report["dispositions"] = copy.deepcopy(dispositions)
        report_ref = self.physical_json("output-%s-%s.json" % (role, current_cas["previous_state_ref"]["digest"][7:15]), report)
        receipt = {
            "schema": "procedural-role-receipt/v1",
            "trust_profile": TRUST_PROFILE,
            "attested_by": "trusted-test-parent",
            "parent_task_id": assignment["parent_task_id"],
            "host_task_id": assignment["host_task_id"],
            "assignment_id": assignment["assignment_id"],
            "actor_id": assignment["actor_id"],
            "role": role,
            "axis": axis,
            "attempt_id": current_cas["attempt_id"],
            "nonce": current_cas["nonce"],
            "role_package_ref": package_ref,
            "predecessor_state_ref": current_cas["previous_state_ref"],
            "predecessor_head": current_cas["expected_head"],
            "started_at": "2026-09-07T00:00:00+00:00",
            "ended_at": "2026-09-07T00:00:01+00:00",
            "terminal_state": "completed",
            "output_ref": report_ref,
            "observed_changed_paths": list(observed or []),
        }
        receipt_ref = self.physical_json("receipt-%s-%s.json" % (role, current_cas["previous_state_ref"]["digest"][7:15]), receipt)
        return report_ref, receipt_ref

    def accept_worker(self, coordinator, begun, *, mutate=True):
        state = self.kernel_value(coordinator, begun["attempt_state_ref"])
        authority = self.kernel_value(coordinator, state["attempt_authority_ref"])
        if mutate:
            (self.project / "sla_report/core.py").write_text("# repaired runtime output\n", encoding="utf-8")
            self.git_commit("runtime repair candidate")
        report_ref, receipt_ref = self.role_material(
            coordinator, begun, authority["worker_assignment"], authority["worker_package_ref"],
            "repair-worker", None, observed=["sla_report/core.py"],
        )
        values = {
            **self.cas(coordinator, begun),
            "worker_report_ref": report_ref,
            "post_fix_candidate_request": {"candidate_id": "post-fix-candidate"},
            "changed_paths": ["sla_report/core.py"],
            "procedural_role_receipt": receipt_ref,
        }
        return coordinator.accept_worker(values), values

    def test_split_protocol_cas_cold_resume_partial_finalize_and_residual(self):
        coordinator, begun, begin_inputs, focused_root, whole_root = self.begin_attempt()
        begin_state = self.kernel_value(coordinator, begun["attempt_state_ref"])
        authority = self.kernel_value(coordinator, begin_state["attempt_authority_ref"])
        self.assertRegex(begun["attempt_id"], r"^e7-[0-9a-f]{32}$")
        self.assertRegex(begin_state["nonce"], r"^[0-9a-f]{64}$")
        self.assertEqual(begin_state["trust_profile"], TRUST_PROFILE)
        self.assertEqual(begin_state["accepted_residual"], ACCEPTED_RESIDUAL)
        self.assertEqual(authority["permitted_fix_scope"], ["sla_report/core.py"])
        self.assertEqual(authority["git_baseline"]["commit"], begun["fixture_baseline_commit"])
        self.assertEqual(RuntimeRepairCoordinator(self.project, "support-report").status()["attempt_state_ref"], begun["attempt_state_ref"])
        self.assertEqual(coordinator.begin(begin_inputs)["attempt_state_ref"], begun["attempt_state_ref"])

        with self.assertRaises(RuntimeRepairError):
            coordinator.run_focused(self.cas(coordinator, begun), object())
        wrong = self.cas(coordinator, begun)
        wrong["nonce"] = "0" * 64
        with self.assertRaises(RuntimeRepairError):
            coordinator.accept_worker({**wrong, "worker_report_ref": {}, "post_fix_candidate_request": {}, "changed_paths": ["sla_report/core.py"], "procedural_role_receipt": {}})

        worker, worker_inputs = self.accept_worker(coordinator, begun)
        worker_state = self.kernel_value(coordinator, worker["attempt_state_ref"])
        self.assertNotEqual(
            worker_state["accepted_refs"]["candidate"]["git_candidate"]["tree"],
            authority["git_baseline"]["tree"],
        )
        self.assertEqual(coordinator.accept_worker(worker_inputs)["attempt_state_ref"], worker["attempt_state_ref"])
        changed_replay = copy.deepcopy(worker_inputs)
        changed_replay["post_fix_candidate_request"] = {"candidate_id": "different-candidate"}
        with self.assertRaises(RuntimeRepairError):
            coordinator.accept_worker(changed_replay)
        self.assertEqual(RuntimeRepairCoordinator(self.project, "support-report").status()["status"], "worker-accepted")

        focused_broker = MacOSTaskProcessBroker(focused_root, capability=b"f" * 32, boot_id="repair-focused")
        try:
            focused = coordinator.run_focused(self.cas(coordinator, worker), focused_broker)
        finally:
            focused_broker.close()
        self.assertEqual(focused["status"], "focused-accepted")
        self.assertEqual(RuntimeRepairCoordinator(self.project, "support-report").status()["next_action"], "reviews-issue")

        review_assignments = [self.assignment("review-a", AXES[0]), self.assignment("review-b", AXES[1])]
        reviews = coordinator.issue_reviews({**self.cas(coordinator, focused), "assignments": review_assignments})
        review_state = self.kernel_value(coordinator, reviews["attempt_state_ref"])
        packages = review_state["accepted_refs"]["review_package_refs"]
        forbidden = {"expected_verdict", "expected_empty_findings", "passed", "caller_authored_terminal_status", "suggested_validator_disposition"}
        for package_ref in packages.values():
            self.assertFalse(forbidden & set(self.kernel_value(coordinator, package_ref)))

        current = reviews
        for assignment in review_assignments:
            axis = assignment["axis"]
            report_ref, receipt_ref = self.role_material(coordinator, current, assignment, packages[axis], "reviewer", axis, findings=[])
            current = coordinator.accept_review({
                **self.cas(coordinator, current),
                "role_package_ref": packages[axis],
                "report_ref": report_ref,
                "procedural_role_receipt": receipt_ref,
            })
            self.assertEqual(RuntimeRepairCoordinator(self.project, "support-report").status()["attempt_state_ref"], current["attempt_state_ref"])
        self.assertEqual(current["status"], "reviews-accepted")

        validator_assignment = self.assignment("validator")
        validator = coordinator.issue_validator({**self.cas(coordinator, current), "assignment": validator_assignment})
        validator_state = self.kernel_value(coordinator, validator["attempt_state_ref"])
        validator_package = validator_state["accepted_refs"]["validator_package_ref"]
        self.assertEqual(self.kernel_value(coordinator, validator_package)["candidate_findings"], [])
        decision_ref, validator_receipt_ref = self.role_material(
            coordinator, validator, validator_assignment, validator_package,
            "finding-validator", None, dispositions=[],
        )
        validated = coordinator.accept_validator({
            **self.cas(coordinator, validator),
            "decision_ref": decision_ref,
            "procedural_role_receipt": validator_receipt_ref,
        })
        self.assertEqual(validated["status"], "validator-accepted")

        whole_broker = MacOSTaskProcessBroker(whole_root, capability=b"w" * 32, boot_id="repair-whole")
        try:
            whole = coordinator.run_whole(self.cas(coordinator, validated), whole_broker)
        finally:
            whole_broker.close()
        with self.assertRaises(RuntimeRepairError):
            coordinator.finalize({**self.cas(coordinator, whole), "passed": True})
        cold = RuntimeRepairCoordinator(self.project, "support-report")
        self.assertEqual(cold.status()["status"], "whole-accepted")

        original_publish = coordinator._publish_canonical
        crashed = {"done": False}

        def crash_after_e7(artifact_id, *args, **kwargs):
            result = original_publish(artifact_id, *args, **kwargs)
            if artifact_id == "runtime-E7" and not crashed["done"]:
                crashed["done"] = True
                raise RuntimeError("simulated parent interruption after E7 publication")
            return result

        coordinator._publish_canonical = crash_after_e7
        with self.assertRaisesRegex(RuntimeError, "simulated parent interruption"):
            coordinator.finalize(self.cas(coordinator, whole))
        interrupted = RuntimeRepairCoordinator(self.project, "support-report")
        interrupted_status = interrupted.status()
        self.assertEqual(interrupted_status["status"], "finalizing")
        interrupted_state = interrupted.kernel.read_state()
        self.assertIn("runtime-E7", interrupted_state["artifacts"])
        self.assertNotIn("runtime-E9", interrupted_state["artifacts"])
        self.assertIsNone(interrupted_state["tasks"]["parse-select"]["result_ref"])
        self.assertIsNone(interrupted_state["findings"]["runtime-required"].get("closure_ref"))

        closed = interrupted.finalize(self.cas(interrupted, interrupted_status))
        self.assertEqual(closed["status"], "closed")
        final_state = interrupted.kernel.read_state()
        self.assertFalse(final_state["leases"])
        self.assertEqual(final_state["tasks"]["parse-select"]["status"], "succeeded")
        self.assertIsNotNone(final_state["findings"]["runtime-required"]["closure_ref"])
        self.assertEqual(set(InceptionRuntime(self.project, "support-report").records("E")), {"E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9"})
        result_object = interrupted.kernel.read_object(final_state["tasks"]["parse-select"]["result_ref"])["payload"]["result"]
        self.assertEqual(result_object["trust_profile"], TRUST_PROFILE)
        self.assertEqual(result_object["accepted_residual"], ACCEPTED_RESIDUAL)
        rehearsal_claim = {"mode": "rehearsal", "authoritative": False, "production_closure": False}
        self.assertEqual(result_object["execution_claim"], rehearsal_claim)
        e9 = interrupted._kernel_value(final_state["artifacts"]["runtime-E9"]["object_ref"], "test final E9")
        self.assertEqual(e9["compiled"]["execution_claim"], rehearsal_claim)
        final_evidence = e9["inputs"]["native_kernel_refs"]
        self.assertEqual(final_evidence["execution_claim"], rehearsal_claim)
        with self.assertRaisesRegex(RuntimeRepairError, "only a real Run identity"):
            assert_production_adoptable(final_evidence, interrupted.runtime.identity)

        real_final = copy.deepcopy(final_evidence)
        real_identity = copy.deepcopy(interrupted.runtime.identity)
        real_identity["mode"] = "real"
        real_final["execution_claim"] = {"mode": "real", "authoritative": True, "production_closure": True}
        real_final["runtime_identity"] = real_identity
        self.assertIsNone(assert_production_adoptable(real_final, real_identity))

        altered_rehearsal = copy.deepcopy(final_evidence)
        altered_rehearsal["execution_claim"] = real_final["execution_claim"]
        with self.assertRaisesRegex(RuntimeRepairError, "only a real Run identity"):
            assert_production_adoptable(altered_rehearsal, interrupted.runtime.identity)

    def test_unchanged_physical_candidate_is_rejected_without_resolution(self):
        coordinator, begun, _, _, _ = self.begin_attempt()
        with self.assertRaisesRegex(RuntimeRepairError, "did not change|unchanged"):
            self.accept_worker(coordinator, begun, mutate=False)
        state = coordinator.kernel.read_state()
        self.assertIsNone(state["findings"]["runtime-required"]["resolution_ref"])
        self.assertIsNone(state["tasks"]["parse-select"]["result_ref"])
        self.assertIn("parse-select", state["leases"])


if __name__ == "__main__":
    unittest.main()
