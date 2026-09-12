"""Actual persisted D-to-E runtime boundary through the macOS broker."""
import copy
import hashlib
import io
import json
import shutil
import sys
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
EXAMPLE = ROOT / "examples" / "support-report"
sys.path[:0] = [str(ROOT / "src"), str(ROOT / "tests"), str(EXAMPLE)]

from ai_agent_workflow.control_kernel import ControlKernel
from ai_agent_workflow.execution_v2 import ExecutionClosureBuilder, RegressionFrontier
from ai_agent_workflow.inception_runtime import InceptionRuntime
from ai_agent_workflow.loop_contracts import canonical_digest
from ai_agent_workflow.macos_task_process import MacOSTaskProcessBroker
from ai_agent_workflow.runtime_execution import RuntimeExecution, RuntimeExecutionError
from runtime_trial import audited_close, initialize, plan_group, put


def digest_bytes(path):
    return "sha256:" + hashlib.sha256(Path(path).read_bytes()).hexdigest()


@unittest.skipUnless(
    sys.platform == "darwin" and shutil.which("sandbox-exec"), "macOS sandbox-exec only"
)
class RuntimeExecutionIntegrationTests(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix="runtime-e-")
        self.addCleanup(temporary.cleanup)
        root = Path(temporary.name).resolve()
        self.project = root / "selected-project"
        self.sibling = root / "confidential-sibling"
        self.project.mkdir()
        self.sibling.mkdir()
        (self.sibling / "secret.txt").write_text("must-not-be-readable", encoding="utf-8")
        with redirect_stdout(io.StringIO()):
            initialize(self.project)
            audited_close(self.project, "B compiler outputs bind the approved rehearsal objective")
            InceptionRuntime(self.project, "support-report").advance()
            plan_group(self.project, "C")
            audited_close(self.project, "C outcome system remains bound to the objective")
            InceptionRuntime(self.project, "support-report").advance()
            plan_group(self.project, "D")
            audited_close(self.project, "D plan is ready for one bounded implementation Task")
            InceptionRuntime(self.project, "support-report").advance()

    def package(self):
        state = InceptionRuntime(self.project, "support-report").state
        scope = state["metadata"]["operational_task_grant"]["tasks"]["parse-select"]["write_scope"]
        for relative in scope:
            path = self.project / relative
            path.parent.mkdir(parents=True, exist_ok=True)
            path.touch()
        adapter = self.project / ".local/agent/support-report-trial/runtime-e-task.py"
        output = self.project / "sla_report/core.py"
        allowed = self.project / ".local/agent/support-report-trial/objective.json"
        adapter.write_text(
            """import json, sys
allowed, sibling, output = sys.argv[1:]
with open(allowed, encoding='utf-8') as stream:
    objective = json.load(stream)
try:
    open(sibling, encoding='utf-8').read()
except OSError:
    denied = True
else:
    denied = False
try:
    with open(output, encoding='utf-8') as stream:
        existing = stream.read()
except OSError:
    existing = ''
if '# repaired runtime output' not in existing:
    with open(output, 'w', encoding='utf-8') as stream:
        stream.write('# bounded runtime output\\n')
if not objective.get('objective') or not denied:
    raise SystemExit(23)
""",
            encoding="utf-8",
        )
        python = str(Path(sys.executable).resolve(strict=True))
        objective_digest = state["objective_ref"]["digest"]
        return {
            "schema": "execution-package-input/v2",
            "package_id": "runtime-e-parse-select",
            "contract_version": "workflow-execution/v2",
            "workspace_identity": str(self.project),
            "candidate_ref": {"id": "approved-objective", "digest": objective_digest},
            "test_refs": [{"id": "runtime-e-adapter", "digest": digest_bytes(adapter)}],
            "fixture_refs": [],
            "schema_refs": [{"id": "runtime-e-task", "digest": digest_bytes(adapter)}],
            "config_refs": [],
            "lock_refs": [{"id": "python-runtime", "digest": digest_bytes(python)}],
            "toolchain": {"executable_digest": digest_bytes(python), "identity": "python3"},
            "command": {
                "argv": [python, "-I", "-S", str(adapter), str(allowed), str(self.sibling / "secret.txt"), str(output)],
                "cwd": str(self.project),
            },
            "environment": {},
            "isolation": {"cwd": str(self.project), "temporary_namespace": "tmp", "output_namespace": "out"},
            "resource_claims": {"read_paths": [".local/agent"], "write_paths": scope, "exclusive_resources": []},
            "supervision": {"timeout_seconds": 10, "grace_seconds": 1, "signals": ["TERM", "KILL"], "heartbeat_seconds": 1, "terminal_publication_seconds": 1},
            "external_input_refs": [],
        }

    def _e7_json_ref(self, name, value):
        path = self.project / ".local/agent/support-report-trial/e7" / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps(value, sort_keys=True, separators=(",", ":")), encoding="utf-8")
        return {"path": str(path.relative_to(self.project)), "digest": digest_bytes(path)}

    def _required_e7(self, package, prior_candidate, prior_closure, base_plan):
        finding_id = "runtime-required"
        output_digest = "sha256:" + hashlib.sha256(b"# bounded runtime output\n").hexdigest()
        candidate_doc = {
            "schema": "runtime-e7-post-fix-candidate/v1", "finding_id": finding_id, "candidate_id": "runtime-e7-post-fix",
            "prior_candidate_digest": prior_candidate["candidate_digest"], "permitted_fix_scope": ["sla_report/core.py"],
            "artifacts": [{"path": "sla_report/core.py", "digest": output_digest}],
        }
        candidate_ref = self._e7_json_ref("post-fix-candidate.json", candidate_doc)
        closure_package = {key: copy.deepcopy(value) for key, value in prior_closure.items() if key != "closure_digest"}
        closure_package["schema"] = "execution-package-input/v2"
        closure_package["candidate_ref"] = {"id": candidate_doc["candidate_id"], "digest": candidate_ref["digest"]}
        post_closure = ExecutionClosureBuilder().freeze(closure_package)
        post_candidate = {
            "schema": "artifact-candidate/v1", "candidate_id": candidate_doc["candidate_id"], "candidate_digest": candidate_ref["digest"],
            "execution_closure_digest": post_closure["closure_digest"], "regression_inventory": prior_candidate["regression_inventory"], "frozen": True,
        }
        shard_inputs = [{key: copy.deepcopy(shard[key]) for key in ("shard_id", "members", "command", "resource_claims", "isolation")} for shard in base_plan["shards"]]
        post_plan = RegressionFrontier().plan(post_candidate, post_closure, shard_inputs)
        worker_doc = {
            "schema": "runtime-e7-worker-result/v1", "finding_id": finding_id, "worker_assignment_id": "runtime-worker",
            "post_fix_candidate_digest": candidate_ref["digest"], "permitted_fix_scope": ["sla_report/core.py"],
            "changed_paths": ["sla_report/core.py"], "status": "passed",
        }
        worker_ref = self._e7_json_ref("worker-result.json", worker_doc)
        focused_refs = []
        for shard in post_plan["shards"]:
            receipt = {
                "schema": "command-receipt/v1", "receipt_id": "runtime-e7-" + shard["shard_id"], "shard_id": shard["shard_id"],
                "candidate_digest": candidate_ref["digest"], "execution_closure_digest": shard["execution_closure_ref"]["digest"],
                "idempotency_key": "runtime-e7-focused-" + shard["shard_id"], "payload_digest": output_digest, "status": "passed",
                "coverage": copy.deepcopy(shard["members"]), "capture_state": {"stdout": "complete", "stderr": "complete"}, "terminal": True,
            }
            receipt["receipt_digest"] = "sha256:" + hashlib.sha256(json.dumps(receipt, sort_keys=True, separators=(",", ":")).encode()).hexdigest()
            focused_refs.append(self._e7_json_ref("focused-" + shard["shard_id"] + ".json", {
                "schema": "runtime-e7-focused-regression/v1", "finding_id": finding_id, "post_fix_candidate_digest": candidate_ref["digest"],
                "worker_result_digest": worker_ref["digest"], "receipt": receipt,
            }))
        focused_digests = sorted(item["digest"] for item in focused_refs)
        review_refs = []
        for axis, actor, epoch in (
            ("architecture-safety", "fresh-spec-rereviewer", "fresh-spec-rereview-epoch"),
            ("integration-operability", "fresh-quality-rereviewer", "fresh-quality-rereview-epoch"),
        ):
            review_refs.append(self._e7_json_ref("review-" + axis + ".json", {
                "schema": "runtime-e7-fresh-review/v1", "finding_id": finding_id, "axis": axis, "actor_id": actor,
                "reviewer_epoch_id": epoch, "post_fix_candidate_digest": candidate_ref["digest"], "worker_result_digest": worker_ref["digest"],
                "focused_receipt_digests": focused_digests, "verdict": "pass", "findings": [],
            }))
        verification_refs = {}
        for category in ("integration", "e2e", "regression", "objective"):
            value = {
                "schema": "whole-verification-receipt/v1", "category": category, "candidate_digest": candidate_ref["digest"],
                "closure_digest": post_closure["closure_digest"], "command": {"argv": [category]}, "environment": {"isolated": True},
                "capture": {"state": "complete"}, "terminal": True, "status": "passed",
            }
            value["receipt_digest"] = "sha256:" + hashlib.sha256(json.dumps(value, sort_keys=True, separators=(",", ":")).encode()).hexdigest()
            verification_refs[category] = self._e7_json_ref("verification-" + category + ".json", value)
        return {
            "post_fix_candidate_ref": candidate_ref, "worker_result_ref": worker_ref, "focused_receipt_refs": focused_refs,
            "fresh_review_refs": review_refs, "verification_receipt_refs": verification_refs,
        }

    def stage_inputs(self, package, *, required=False):
        closure = ExecutionClosureBuilder().freeze(package)
        candidate = {
            "schema": "artifact-candidate/v1",
            "candidate_id": closure["candidate_ref"]["id"],
            "candidate_digest": closure["candidate_ref"]["digest"],
            "execution_closure_digest": closure["closure_digest"],
            "regression_inventory": ["runtime-e-regression"],
            "frozen": True,
        }
        shard = {
            "shard_id": "runtime-e-shard",
            "members": ["runtime-e-regression"],
            "command": {"argv": copy.deepcopy(package["command"]["argv"])},
            "resource_claims": {"read_paths": [], "write_paths": [], "exclusive_resources": []},
            "isolation": {"cwd": package["command"]["cwd"], "temporary_namespace": "verify-tmp", "output_namespace": "verify-out"},
        }
        plan = RegressionFrontier().plan(candidate, closure, [shard])
        receipt = {
            "schema": "command-receipt/v1",
            "receipt_id": "runtime-e-receipt",
            "shard_id": "runtime-e-shard",
            "candidate_digest": candidate["candidate_digest"],
            "execution_closure_digest": plan["shards"][0]["execution_closure_ref"]["digest"],
            "idempotency_key": "runtime-e-regression",
            "payload_digest": digest_bytes(package["command"]["argv"][3]),
            "status": "passed",
            "coverage": ["runtime-e-regression"],
            "capture_state": {"stdout": "complete", "stderr": "complete"},
            "terminal": True,
        }
        receipt["receipt_digest"] = "sha256:" + hashlib.sha256(json.dumps(receipt, sort_keys=True, separators=(",", ":")).encode()).hexdigest()
        findings = []
        dispositions = []
        e7 = None
        if required:
            requirement = "R-runtime-e"
            gap = "The fixture requires one bounded repair before acceptance."
            severity = "major"
            fingerprint = "sha256:" + hashlib.sha256(json.dumps({"requirement_ref": requirement, "description": gap, "severity": severity}, sort_keys=True, separators=(",", ":")).encode()).hexdigest()
            findings = [{"finding_id": "runtime-required", "fingerprint": fingerprint, "background": "Review found one material fixture gap.", "as_is": "The initial candidate lacks the marker.", "to_be": "The repair evidence binds the marker.", "gap": gap, "requirement_refs": [requirement], "evidence_refs": ["fixture-review"], "severity": severity, "blocking_proposal": "blocking", "owner_proposal": "runtime-worker"}]
            dispositions = [{"fingerprint": fingerprint, "classification": "required", "materiality": "material", "proposed_scope": ["sla_report/core.py"]}]
            e7 = self._required_e7(package, candidate, closure, plan)
        verification = {}
        for category in ("integration", "e2e", "regression", "objective"):
            value = {"schema": "whole-verification-receipt/v1", "category": category, "candidate_digest": candidate["candidate_digest"], "closure_digest": closure["closure_digest"], "command": {"argv": [category]}, "environment": {"isolated": True}, "capture": {"state": "complete"}, "terminal": True, "status": "passed"}
            value["receipt_digest"] = "sha256:" + hashlib.sha256(json.dumps(value, sort_keys=True, separators=(",", ":")).encode()).hexdigest()
            verification[category] = value
        result = {
            "E4": {"axis": "architecture-safety", "actor_id": "spec-reviewer", "reviewer_epoch_id": "spec-review-epoch", "other_reviewer_epoch_id": "quality-review-epoch", "worker_actor_id": "runtime-worker", "other_reviewer_actor_id": "quality-reviewer", "findings": findings},
            "E5": {"axis": "integration-operability", "actor_id": "quality-reviewer", "reviewer_epoch_id": "quality-review-epoch", "other_reviewer_epoch_id": "spec-review-epoch", "worker_actor_id": "runtime-worker", "other_reviewer_actor_id": "spec-reviewer", "findings": []},
            "E6": {"dispositions": dispositions, "observed_budget": {"remaining_seconds": 100, "review_round": 0, "product_fix_attempt": 0}},
            "E8": {"candidate": candidate, "plan": plan, "receipts": [receipt]},
            "E9": {"candidate_digest": candidate["candidate_digest"], "closure_digest": closure["closure_digest"], "verification_receipts": verification, "complete": True, "open_required": False},
        }
        if e7 is not None:
            result["E7"] = e7
        return result

    def test_native_task_reviews_validators_e_closure_and_cold_resume(self):
        package = self.package()
        runtime = RuntimeExecution(self.project, "support-report")
        broker_root = self.project / ".agent-workflow/runtime-e-broker"
        broker = MacOSTaskProcessBroker(broker_root, capability=b"r" * 32, boot_id="runtime-e-test")
        python_root = str(Path("/Library/Developer/CommandLineTools").resolve(strict=True))
        system_roots = [path for path in ("/System", "/usr/lib", "/usr/share", "/usr/bin", "/bin", "/dev") if Path(path).is_dir()]

        changed = copy.deepcopy(package)
        changed["resource_claims"]["write_paths"] = ["outside"]
        with self.assertRaises(RuntimeExecutionError):
            runtime.execute("parse-select", changed, worker_assignment_id="runtime-worker", broker=broker, probes=[], system_read_roots=system_roots, runtime_read_roots=[python_root], stage_inputs=self.stage_inputs(package), changed_paths=["outside"])

        allowed = self.project / ".local/agent/support-report-trial/objective.json"
        result = runtime.execute(
            "parse-select",
            package,
            worker_assignment_id="runtime-worker",
            broker=broker,
            probes=[
                {"operation": "content-read", "path": str(allowed), "expected": "allowed"},
                {"operation": "enumerate", "path": str(allowed.parent), "expected": "allowed"},
                {"operation": "content-read", "path": str(self.sibling / "secret.txt"), "expected": "denied"},
                {"operation": "stat", "path": str(self.sibling / "secret.txt"), "expected": "denied"},
                {"operation": "enumerate", "path": str(self.sibling), "expected": "denied"},
            ],
            system_read_roots=system_roots,
            runtime_read_roots=[python_root],
            stage_inputs=self.stage_inputs(package),
            changed_paths=["sla_report/core.py"],
        )
        self.assertEqual(result["next_action"], "close-group-with-audit")
        state = runtime.kernel.read_state()
        self.assertEqual(state["tasks"]["parse-select"]["status"], "succeeded")
        self.assertFalse(state["leases"])
        self.assertEqual(set(state["reviews"]), {"runtime-e4", "runtime-e5"})
        self.assertEqual(set(state["finding_validations"]), {"runtime-e4", "runtime-e5"})
        self.assertEqual(set(InceptionRuntime(self.project, "support-report").records("E")), {"E1", "E2", "E3", "E4", "E5", "E6", "E8", "E9"})

        records = InceptionRuntime(self.project, "support-report").records("E")
        e1_frontier = records["E1"]["value"]["inputs"]["persisted_frontier"]
        self.assertTrue(all(e1_frontier["readiness_receipt"]["checks"].values()))
        self.assertEqual(e1_frontier["d8_ref"], InceptionRuntime(self.project, "support-report").records("D")["D8"]["ref"])
        e2_closure = records["E2"]["value"]["compiled"]["package"]["execution_closure"]
        e3_candidate = records["E3"]["value"]["compiled"]["artifact_candidate"]
        for stage in ("E4", "E5"):
            binding = records[stage]["value"]["compiled"]["report"]["binding"]
            self.assertEqual(binding["candidate_digest"], e3_candidate["candidate_digest"])
            self.assertEqual(binding["closure_digest"], e2_closure["closure_digest"])
        finalization = records["E8"]["value"]["compiled"]["finalization"]
        self.assertEqual(finalization["candidate_ref"]["digest"], e3_candidate["candidate_digest"])
        self.assertEqual(finalization["execution_closure_digest"], e2_closure["closure_digest"])
        self.assertEqual(records["E9"]["value"]["previous_ref"], records["E8"]["ref"])

        current = InceptionRuntime(self.project, "support-report")
        records = current.records("E")
        audit = {
            "group_id": "E",
            "objective_digest": current.state["objective_ref"]["digest"],
            "alignment": "aligned",
            "artifact_refs": [records[key]["ref"] for key in ("E1", "E2", "E3", "E4", "E5", "E6", "E8", "E9")],
            "reviewer": "runtime-e-boundary-test",
            "rationale": "Native task, macOS broker, two reviews, validators, convergence, and verification are persisted.",
        }
        closed = current.close(put(self.project, "E-audit.json", audit))
        self.assertEqual(
            {key: closed["group"][key] for key in ("id", "next_group", "status")},
            {"id": "E", "next_group": "H", "status": "closed"},
        )
        cold = InceptionRuntime(self.project, "support-report")
        cold_group = cold.status()["group"]
        self.assertEqual(
            {key: cold_group[key] for key in ("id", "next_group", "status")},
            {"id": "E", "next_group": "H", "status": "closed"},
        )
        self.assertEqual(cold.status()["head"], closed["head"])

    def test_cross_candidate_semantic_inputs_refuse_before_task_publication(self):
        package = self.package()
        runtime = RuntimeExecution(self.project, "support-report")
        broker = MacOSTaskProcessBroker(self.project / ".agent-workflow/runtime-e-broker", capability=b"n" * 32, boot_id="runtime-e-negative")
        base = self.stage_inputs(package)

        foreign_package = copy.deepcopy(package)
        foreign_package["candidate_ref"]["digest"] = "sha256:" + "f" * 64
        foreign = self.stage_inputs(foreign_package)
        aggregate = copy.deepcopy(base)
        aggregate["E8"]["aggregate"] = {"aggregate_digest": "sha256:" + "a" * 64}
        report = copy.deepcopy(base)
        report["E6"]["reviews"] = []
        disposition = copy.deepcopy(base)
        disposition["E6"]["dispositions"] = [{"fingerprint": "foreign-root", "classification": "required", "materiality": "material", "proposed_scope": []}]
        closure = copy.deepcopy(base)
        closure["E9"]["closure_digest"] = "sha256:" + "e" * 64
        verification = copy.deepcopy(base)
        item = verification["E9"]["verification_receipts"]["integration"]
        item["candidate_digest"] = "sha256:" + "d" * 64
        item["receipt_digest"] = "sha256:" + hashlib.sha256(json.dumps({key: value for key, value in item.items() if key != "receipt_digest"}, sort_keys=True, separators=(",", ":")).encode()).hexdigest()

        for name, supplied in (("candidate", foreign), ("aggregate", aggregate), ("report", report), ("disposition", disposition), ("closure", closure), ("verification", verification)):
            with self.subTest(name=name), self.assertRaises(RuntimeExecutionError):
                runtime.execute("parse-select", package, worker_assignment_id="runtime-worker", broker=broker, probes=[], system_read_roots=[], runtime_read_roots=[], stage_inputs=supplied, changed_paths=["sla_report/core.py"])
            state = runtime.kernel.read_state()
            self.assertNotIn("parse-select", state["tasks"])
            self.assertFalse(state["leases"])
            self.assertFalse(InceptionRuntime(self.project, "support-report").records("E"))

    def test_post_release_semantic_refusal_releases_and_terminalizes_live_task(self):
        package = self.package()
        runtime = RuntimeExecution(self.project, "support-report")
        broker = MacOSTaskProcessBroker(self.project / ".agent-workflow/runtime-e-broker", capability=b"t" * 32, boot_id="runtime-e-terminal")
        stages = self.stage_inputs(package)
        stages["E4"]["frozen_binding"] = {key: "sha256:" + letter * 64 for key, letter in zip(("candidate_digest", "aggregate_digest", "spec_digest", "closure_digest", "budget_digest"), "abcde")}
        python_root = str(Path("/Library/Developer/CommandLineTools").resolve(strict=True))
        system_roots = [path for path in ("/System", "/usr/lib", "/usr/share", "/usr/bin", "/bin", "/dev") if Path(path).is_dir()]
        with self.assertRaises(RuntimeExecutionError):
            runtime.execute("parse-select", package, worker_assignment_id="runtime-worker", broker=broker, probes=[], system_read_roots=system_roots, runtime_read_roots=[python_root], stage_inputs=stages, changed_paths=["sla_report/core.py"])
        state = runtime.kernel.read_state()
        self.assertFalse(state["leases"])
        self.assertTrue(state["tasks"]["parse-select"]["invalidated"])
        self.assertEqual(state["tasks"]["parse-select"]["status"], "invalidated")
        self.assertIsNone(state["tasks"]["parse-select"]["result_ref"])
        self.assertFalse(InceptionRuntime(self.project, "support-report").records("E"))


    def test_one_required_finding_stops_at_persisted_split_repair_frontier(self):
        package = self.package()
        runtime = RuntimeExecution(self.project, "support-report")
        broker = MacOSTaskProcessBroker(self.project / ".agent-workflow/runtime-e-broker", capability=b"q" * 32, boot_id="runtime-e-required")
        python_root = str(Path("/Library/Developer/CommandLineTools").resolve(strict=True))
        system_roots = [path for path in ("/System", "/usr/lib", "/usr/share", "/usr/bin", "/bin", "/dev") if Path(path).is_dir()]
        allowed = self.project / ".local/agent/support-report-trial/objective.json"
        stages = self.stage_inputs(package, required=True)
        stages.pop("E7")
        result = runtime.execute(
            "parse-select", package, worker_assignment_id="runtime-worker", broker=broker,
            probes=[
                {"operation": "content-read", "path": str(allowed), "expected": "allowed"},
                {"operation": "content-read", "path": str(self.sibling / "secret.txt"), "expected": "denied"},
            ],
            system_read_roots=system_roots, runtime_read_roots=[python_root],
            stage_inputs=stages, changed_paths=["sla_report/core.py"],
        )
        self.assertEqual(result["status"], "repair_required")
        self.assertEqual(result["next_operation"], "begin")
        self.assertEqual(result["finding_id"], "runtime-required")
        self.assertEqual(result["stages"], ["E1", "E2", "E3", "E4", "E5", "E6-initial"])
        state = runtime.kernel.read_state()
        finding = state["findings"]["runtime-required"]
        self.assertEqual(finding["state"], "open")
        self.assertIsNone(finding.get("resolution_ref"))
        self.assertIsNone(finding.get("closure_ref"))
        self.assertIsNone(state["tasks"]["parse-select"]["result_ref"])
        self.assertIn("parse-select", state["leases"])
        self.assertEqual({item["validator_assignment_id"] for item in state["finding_validations"].values()}, {"runtime-e-validator"})
        records = InceptionRuntime(self.project, "support-report").records("E")
        self.assertEqual(set(records), {"E1", "E2", "E3", "E4", "E5"})
        self.assertIn("execution-E6-initial", state["artifacts"])
        for forbidden in ("runtime-E6", "runtime-E7", "runtime-E8", "runtime-E9"):
            self.assertNotIn(forbidden, state["artifacts"])

        current = InceptionRuntime(self.project, "support-report")
        with self.assertRaisesRegex(Exception, "E7 may be omitted only after E6 routes directly to E8"):
            current.close(put(self.project, "E-incomplete-audit.json", {
                "group_id": "E", "objective_digest": current.state["objective_ref"]["digest"],
                "alignment": "aligned", "artifact_refs": [], "reviewer": "runtime-e-test",
                "rationale": "A required split repair has not closed.",
            }))

    def test_real_one_shot_e7_rejects_before_task_publication(self):
        package = self.package()
        runtime = RuntimeExecution(self.project, "support-report")
        broker = MacOSTaskProcessBroker(self.project / ".agent-workflow/runtime-e-broker", capability=b"c" * 32, boot_id="runtime-e-claim-only")
        stages = self.stage_inputs(package, required=True)
        python_root = str(Path("/Library/Developer/CommandLineTools").resolve(strict=True))
        system_roots = [path for path in ("/System", "/usr/lib", "/usr/share", "/usr/bin", "/bin", "/dev") if Path(path).is_dir()]
        allowed = self.project / ".local/agent/support-report-trial/objective.json"
        with self.assertRaisesRegex(RuntimeExecutionError, "cannot consume caller-supplied one-shot E7"):
            runtime.execute(
                "parse-select", package, worker_assignment_id="runtime-worker", broker=broker, probes=[
                    {"operation": "content-read", "path": str(allowed), "expected": "allowed"},
                    {"operation": "content-read", "path": str(self.sibling / "secret.txt"), "expected": "denied"},
                ],
                system_read_roots=system_roots, runtime_read_roots=[python_root], stage_inputs=stages,
                changed_paths=["sla_report/core.py"],
            )
        state = runtime.kernel.read_state()
        self.assertNotIn("parse-select", state["tasks"])
        self.assertFalse(state["leases"])
        self.assertFalse(InceptionRuntime(self.project, "support-report").records("E"))



class WorkflowLoopExecutionTests(unittest.TestCase):
    """Exercise the explicit loop-control path without a process broker."""

    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix="workflow-loop-runtime-")
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)
        self.authority = {"status": "approved", "scopes": ["*"]}
        self.identity = {
            "schema": "loop-work-identity/v1",
            "work_lineage_id": "lineage-runtime",
            "logical_task_id": "task-1",
            "phase": "E3",
            "scope_revision": "scope-1",
            "requirements_digest": "sha256:" + "a" * 64,
            "predecessor_ref": None,
        }
        self.kernel = ControlKernel(self.root, "workflow-loop-run")
        self.kernel.entry(
            {"path": "objectives/loop.md", "version": "v1", "digest": "sha256:" + "b" * 64},
            authority_ref=self.authority,
            loop_control={"identity": self.identity, "history": []},
        )
        self.runtime = RuntimeExecution(kernel=self.kernel)

    def request(self, **changes):
        value = {"schema": "workflow-loop/v1", "identity": copy.deepcopy(self.identity), "phase": "E3"}
        value.update(copy.deepcopy(changes))
        return value

    def result_ref(self, suffix):
        return {"id": "result-" + suffix, "digest": "sha256:" + suffix * 64}

    def completion_bundle(self, candidate, package):
        evidence = {
            "schema": "loop-evidence-record/v1",
            "evidence_id": "evidence-1",
            "evidence_digest": "",
            "candidate_digest": candidate,
            "spec_digest": "sha256:" + "1" * 64,
            "source_digest": "sha256:" + "2" * 64,
            "dependency_digest": "sha256:" + "3" * 64,
            "environment_digest": "sha256:" + "4" * 64,
            "check_definition_digest": "sha256:" + "5" * 64,
            "coverage": ["R1"],
            "status": "pass",
        }
        evidence["evidence_digest"] = canonical_digest(
            {key: value for key, value in evidence.items() if key != "evidence_digest"}
        )
        requirement = {
            "schema": "loop-requirement-assessment/v1",
            "requirement_id": "R1",
            "status": "pass",
            "scope": ["src/a.py"],
            "evidence_refs": [{"id": "evidence-1", "digest": evidence["evidence_digest"]}],
        }
        reviews = [
            {
                "schema": "loop-review-assessment/v1",
                "review_id": "review-" + axis,
                "axis": axis,
                "actor_id": actor,
                "context_epoch": epoch,
                "candidate_digest": candidate,
                "package_digest": package,
                "coverage": ["R1"],
                "completed": True,
                "unevaluated": [],
                "finding_refs": [],
            }
            for axis, actor, epoch in (
                ("architecture-safety", "reviewer-a", "epoch-a"),
                ("integration-operability", "reviewer-b", "epoch-b"),
            )
        ]
        return {
            "identity": copy.deepcopy(self.identity),
            "candidate_digest": candidate,
            "package_digest": package,
            "requirements": [requirement],
            "reviews": reviews,
            "evidence": [evidence],
            "findings": [],
        }, requirement, reviews, evidence

    def test_dispatch_reserves_before_accepting_and_ignores_legacy_progress_controls(self):
        result = self.runtime.execute_loop(
            "task-1",
            self.request(
                remaining_seconds=0,
                review_budget={"rounds": 0},
                deadline="expired",
                budget_digest="sha256:" + "d" * 64,
            ),
            result_ref=self.result_ref("c"),
            process_timeout=2.0,
        )
        self.assertEqual("continue", result["outcome"])
        self.assertEqual(1, result["counters"]["initial"])
        self.assertEqual(0, result["counters"]["additional_iterations"])
        self.assertEqual(0, result["counters"]["technical_retries"])
        self.assertEqual(2.0, result["process_timeout"]["process_timeout"])
        self.assertEqual(
            {"budget_digest", "deadline", "remaining_seconds", "review_budget"},
            set(result["ignored_progress_fields"]),
        )
        event = result["history"][-1]
        self.assertEqual("evaluated", event["status"])
        self.assertEqual("initial", event["kind"])

    def test_scope_rename_does_not_reset_counter_and_technical_retry_is_finite(self):
        first = self.runtime.execute_loop("task-1", self.request(), result_ref=self.result_ref("c"))
        renamed = self.request()
        renamed["identity"]["scope_revision"] = "renamed-scope"
        renamed["command_id"] = "renamed-scope-next-iteration"
        second = self.runtime.execute_loop(
            "task-1", renamed, result_ref=self.result_ref("d"),
        )
        self.assertEqual(1, second["counters"]["additional_iterations"])
        self.assertEqual(1, second["counters"]["initial"])
        self.assertEqual(first["identity"], second["identity"])

        retry = self.runtime.execute_loop(
            "task-1",
            self.request(iteration_kind="technical-retry"),
            result_ref=self.result_ref("e"),
        )
        self.assertEqual(1, retry["counters"]["technical_retries"])
        self.assertEqual("execution-failed", retry["outcome"])
        self.assertTrue(retry["terminal_recorded"])
        blocked = self.runtime.execute_loop(
            "task-1",
            self.request(iteration_kind="technical-retry"),
            result_ref=self.result_ref("f"),
        )
        self.assertEqual("execution-failed", blocked["outcome"])
        self.assertEqual(retry["terminal_ref"], blocked["terminal_ref"])

    def test_additional_iteration_limit_is_terminal_and_durable(self):
        outcomes = []
        for suffix in ("c", "d", "e", "f"):
            result = self.runtime.execute_loop(
                "task-1",
                self.request(command_id="iteration-" + suffix),
                result_ref=self.result_ref(suffix),
            )
            outcomes.append(result["outcome"])
        self.assertEqual(["continue", "continue", "continue", "iteration-limit"], outcomes)
        self.assertTrue(result["limit_exhausted"])
        self.assertTrue(result["terminal_recorded"])
        self.assertEqual("iteration-limit", self.kernel.read_state()["loop_control"]["terminal_record"]["outcome"])

    def test_needs_input_is_saved_and_resume_evidence_does_not_reset_the_counter(self):
        stopped = self.runtime.execute_loop("task-1", self.request())
        self.assertEqual("needs-input", stopped["outcome"])
        self.assertTrue(stopped["terminal_recorded"])
        terminal_ref = stopped["terminal_ref"]
        resumed = self.runtime.execute_loop(
            "task-1",
            self.request(
                resume_evidence_ref={"id": "input-1", "digest": "sha256:" + "9" * 64}
            ),
            result_ref=self.result_ref("c"),
        )
        self.assertEqual("continue", resumed["outcome"])
        self.assertEqual(1, resumed["counters"]["initial"])
        state = self.kernel.read_state()["loop_control"]
        self.assertIsNone(state["terminal_record"])
        self.assertEqual("loop-terminal-record", state["control_refs"][0]["object_type"])
        self.assertEqual("loop-resume-record", state["control_refs"][1]["object_type"])
        self.assertEqual(terminal_ref["digest"], state["control_refs"][0]["digest"])

    def test_completed_decision_is_saved_with_current_assessments(self):
        candidate = "sha256:" + "c" * 64
        package = "sha256:" + "d" * 64
        bundle, requirement, reviews, evidence = self.completion_bundle(candidate, package)
        result = self.runtime.execute_loop(
            "task-1",
            self.request(completion_request=bundle),
            result_ref={"id": "candidate-result", "digest": candidate},
        )
        self.assertEqual("completed", result["outcome"])
        self.assertTrue(result["terminal_recorded"])
        terminal = self.kernel.read_state()["loop_control"]["terminal_record"]
        self.assertEqual([requirement], terminal["requirements"])
        self.assertEqual(reviews, terminal["reviews"])
        self.assertEqual([evidence], terminal["evidence"])

    def test_same_request_is_exactly_once_and_does_not_redispatch(self):
        calls = []

        def dispatch(_package):
            calls.append("called")
            return self.result_ref("c")

        request = self.request()
        first = self.runtime.execute_loop("task-1", request, dispatcher=dispatch)
        replay = self.runtime.execute_loop("task-1", request, dispatcher=dispatch)
        self.assertEqual(["called"], calls)
        self.assertEqual(1, len(replay["history"]))
        self.assertEqual(first["event"], replay["event"])

    def test_implicit_command_ignores_evaluation_and_legacy_sidecar_changes(self):
        calls = []

        def dispatch(_package):
            calls.append("called")
            return self.result_ref("c")

        first = self.runtime.execute_loop(
            "task-1", self.request(remaining_seconds=0), dispatcher=dispatch
        )
        replay = self.runtime.execute_loop(
            "task-1",
            self.request(remaining_seconds=999, review_budget={"rounds": 99}),
            dispatcher=dispatch,
        )
        self.assertEqual(["called"], calls)
        self.assertEqual(1, len(replay["history"]))
        self.assertEqual(first["event"], replay["event"])

    def test_replayed_command_rejects_a_different_result_without_terminalizing(self):
        request = self.request(command_id="stable-command")
        self.runtime.execute_loop("task-1", request, result_ref=self.result_ref("c"))
        with self.assertRaisesRegex(RuntimeExecutionError, "different result"):
            self.runtime.execute_loop(
                "task-1", request, result_ref=self.result_ref("d")
            )
        state = self.kernel.read_state()["loop_control"]
        self.assertIsNone(state["terminal_record"])
        self.assertEqual(1, len(self.runtime.loop_status()["history"]))

    def test_explicit_command_rejects_changed_completion_payload(self):
        candidate = "sha256:" + "c" * 64
        package = "sha256:" + "d" * 64
        request = self.request(command_id="stable-command")
        self.runtime.execute_loop(
            "task-1",
            request,
            result_ref={"id": "candidate-result", "digest": candidate},
        )
        bundle, _, _, _ = self.completion_bundle(candidate, package)
        changed = self.request(
            command_id="stable-command", completion_request=bundle
        )
        with self.assertRaisesRegex(RuntimeExecutionError, "different request payload"):
            self.runtime.execute_loop(
                "task-1",
                changed,
                result_ref={"id": "candidate-result", "digest": candidate},
            )
        self.assertIsNone(self.kernel.read_state()["loop_control"]["terminal_record"])

    def test_foreign_lineage_is_a_refusal_not_a_durable_run_outcome(self):
        request = self.request()
        request["identity"]["work_lineage_id"] = "foreign-lineage"
        with self.assertRaisesRegex(RuntimeExecutionError, "work lineage"):
            self.runtime.execute_loop(
                "task-1", request, result_ref=self.result_ref("c")
            )
        state = self.kernel.read_state()["loop_control"]
        self.assertIsNone(state["terminal_record"])
        self.assertEqual([], self.runtime.loop_status()["history"])

    def test_malformed_supplied_result_is_a_non_mutating_refusal(self):
        before = self.kernel.read_state()
        with self.assertRaisesRegex(RuntimeExecutionError, "sha256 digest"):
            self.runtime.execute_loop(
                "task-1",
                self.request(),
                result_ref={"id": "bad-result", "digest": "not-a-digest"},
            )
        after = self.kernel.read_state()
        self.assertEqual(before["revision"], after["revision"])
        self.assertEqual([], self.runtime.loop_status()["history"])
        self.assertIsNone(after["loop_control"]["terminal_record"])

    def test_malformed_completion_is_a_non_mutating_refusal(self):
        before = self.kernel.read_state()
        candidate = "sha256:" + "c" * 64
        package = "sha256:" + "d" * 64
        bundle, _, _, _ = self.completion_bundle(candidate, package)
        bundle["requirements"] = "not-a-list"
        with self.assertRaisesRegex(RuntimeExecutionError, "completion input is invalid"):
            self.runtime.execute_loop(
                "task-1",
                self.request(completion_request=bundle),
                result_ref={"id": "candidate-result", "digest": candidate},
            )
        after = self.kernel.read_state()
        self.assertEqual(before["revision"], after["revision"])
        self.assertEqual([], self.runtime.loop_status()["history"])
        self.assertIsNone(after["loop_control"]["terminal_record"])

    def test_incomplete_completion_is_a_non_mutating_refusal(self):
        before = self.kernel.read_state()
        candidate = "sha256:" + "c" * 64
        for completion_request in ({}, {"requirements": "bad"}):
            with self.subTest(completion_request=completion_request):
                with self.assertRaisesRegex(
                    RuntimeExecutionError, "completion input is invalid"
                ):
                    self.runtime.execute_loop(
                        "task-1",
                        self.request(completion_request=completion_request),
                        result_ref={"id": "candidate-result", "digest": candidate},
                    )
                after = self.kernel.read_state()
                self.assertEqual(before["revision"], after["revision"])
                self.assertEqual([], self.runtime.loop_status()["history"])
                self.assertIsNone(after["loop_control"]["terminal_record"])

    def test_rejected_completed_terminal_is_not_reported_as_completed(self):
        candidate = "sha256:" + "c" * 64
        package = "sha256:" + "d" * 64
        bundle, _, _, _ = self.completion_bundle(candidate, package)
        result = self.runtime.execute_loop(
            "task-1",
            self.request(completion_request=bundle),
            result_ref={"id": "different-result", "digest": "sha256:" + "e" * 64},
        )
        self.assertEqual("execution-failed", result["outcome"])
        self.assertEqual("completed", result["requested_outcome"])
        self.assertFalse(result["terminal_recorded"])
        self.assertFalse(result["durable"])
        self.assertIn("does not match", result["terminal_error"])
        self.assertIsNone(self.kernel.read_state()["loop_control"]["terminal_record"])

    def test_unknown_dispatch_blocks_redispatch_until_recovery(self):
        calls = []

        def ambiguous(_package):
            calls.append("called")
            raise RuntimeError("broker disconnected")

        unknown = self.runtime.execute_loop("task-1", self.request(), dispatcher=ambiguous)
        self.assertEqual("recovery-required", unknown["outcome"])
        self.assertTrue(unknown["execution_unknown"])
        blocked = self.runtime.execute_loop(
            "task-1", self.request(command_id="redispatch"), dispatcher=lambda _: calls.append("retry"),
        )
        self.assertEqual("recovery-required", blocked["outcome"])
        self.assertEqual(["called"], calls)
        self.assertEqual("execution-unknown", blocked["history"][-1]["status"])

    def test_explicit_ambiguous_dispatch_status_is_execution_unknown(self):
        result = self.runtime.execute_loop(
            "task-1",
            self.request(),
            dispatcher=lambda _package: {"status": "unknown"},
        )
        self.assertEqual("recovery-required", result["outcome"])
        self.assertTrue(result["execution_unknown"])
        self.assertEqual("execution-unknown", result["history"][-1]["status"])

    def test_unknown_state_remains_visible_when_terminal_recording_fails(self):
        with patch.object(
            self.kernel,
            "record_loop_outcome",
            side_effect=RuntimeError("terminal storage unavailable"),
        ):
            result = self.runtime.execute_loop(
                "task-1",
                self.request(),
                dispatcher=lambda _package: {"status": "unknown"},
            )
        self.assertEqual("recovery-required", result["outcome"])
        self.assertTrue(result["execution_unknown"])
        self.assertTrue(result["needs_recovery"])
        self.assertFalse(result["terminal_recorded"])
        self.assertFalse(result["durable"])
        self.assertTrue(self.kernel.read_state()["loop_control"]["recovery_required"])

    def test_integration_return_restores_task_counter_and_keeps_integration_counter(self):
        self.runtime.execute_loop("task-1", self.request(), result_ref=self.result_ref("c"))
        integration_identity = copy.deepcopy(self.identity)
        integration_identity.update({"logical_task_id": "integration-1", "phase": "E8", "scope_revision": "scope-e8"})
        integration = self.runtime.execute_loop(
            "integration-1",
            {"schema": "workflow-loop/v1", "identity": integration_identity, "phase": "E8"},
            integration=True,
            result_ref=self.result_ref("d"),
        )
        self.assertEqual("E8", integration["identity"]["phase"])
        self.assertEqual(1, integration["counters"]["initial"])
        returned_identity = copy.deepcopy(self.identity)
        returned_identity["scope_revision"] = "scope-return"
        returned = self.runtime.execute_loop(
            "task-1",
            {
                "schema": "workflow-loop/v1",
                "identity": returned_identity,
                "phase": "E3",
                "iteration_kind": "integration-return",
            },
            result_ref=self.result_ref("e"),
        )
        self.assertEqual("E3", returned["identity"]["phase"])
        self.assertEqual(1, returned["counters"]["initial"])
        self.assertEqual(1, returned["counters"]["additional_iterations"])
        self.assertEqual("E8", self.kernel.read_state()["loop_control"]["archives"][0]["identity"]["phase"])

    def test_project_runtime_requires_the_accepted_operational_task_grant(self):
        self.runtime.runtime = object()
        with self.assertRaisesRegex(RuntimeExecutionError, "D8/D10 operational grant"):
            self.runtime.execute_loop("renamed-task", self.request(logical_task_id="renamed-task"), result_ref=self.result_ref("c"))


if __name__ == "__main__":
    unittest.main()
