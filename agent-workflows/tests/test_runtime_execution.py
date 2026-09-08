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

ROOT = Path(__file__).resolve().parents[1]
EXAMPLE = ROOT / "examples" / "support-report"
sys.path[:0] = [str(ROOT / "src"), str(ROOT / "tests"), str(EXAMPLE)]

from ai_agent_workflow.inception_runtime import InceptionRuntime  # noqa: E402
from ai_agent_workflow.macos_task_process import MacOSTaskProcessBroker  # noqa: E402
from ai_agent_workflow.runtime_execution import RuntimeExecution, RuntimeExecutionError  # noqa: E402
from ai_agent_workflow.execution_v2 import ExecutionClosureBuilder, RegressionFrontier  # noqa: E402
from runtime_trial import audited_close, initialize, plan_group, put  # noqa: E402


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
        output = self.project / "sla_report/core.py"
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


if __name__ == "__main__":
    unittest.main()
