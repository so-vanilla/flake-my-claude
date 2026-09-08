"""Operational approval and Group transitions retain real/mock provenance."""
import copy
import hashlib
import json
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "src"))
from ai_agent_workflow.control_kernel import ControlKernel, KernelError
from ai_agent_workflow.schema_validation import validate_document
from ai_agent_workflow.planning_system import PlanningSystemV1


class RuntimeApprovalTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.project = Path(self.temporary.name).resolve()
        self.path = self.project / "objective.md"
        self.path.write_text("A concrete approved outcome.\n")
        self.base = json.loads((ROOT / "tests/fixtures/s2/objective/lifecycle/adapter-positive.json").read_text())

    def setup_run(self, mode="real"):
        self.identity = {"schema": "project-local-run-identity/v1", "run_id": "runtime-run", "namespace": "project:runtime-run", "approval_scope": "project-local", "project_root": str(self.project), "mode": mode}
        self.kernel = ControlKernel(self.project, "runtime-run")
        self.kernel.entry({"path": "intake.json", "version": "intake", "digest": "sha256:" + "d" * 64}, group_id="B", epoch_id="B-01", authority_ref={"approved": True, "scopes": ["entry"], "runtime_identity": self.identity})
        payload = copy.deepcopy(self.base["payload"])
        candidate, approval = payload["candidate_ref"], payload["approval"]
        candidate.update(path=str(self.path), version="v001", namespace=self.identity["namespace"], digest="sha256:" + hashlib.sha256(self.path.read_bytes()).hexdigest())
        payload["prior_objective"]["version"] = "intake"
        source = "mock" if mode == "rehearsal" else "human"
        approval.update({k: self.identity[k] for k in ("run_id", "namespace", "approval_scope", "project_root", "mode")})
        approval.update(candidate_digest=candidate["digest"], candidate_version="v001", prior_objective_version="intake")
        approval["actor"]["kind"] = source
        receipt = approval["receipt"]
        receipt.update({k: approval[k] for k in ("run_id", "namespace", "approval_scope", "project_root", "mode", "candidate_digest", "candidate_version", "prior_objective_version")})
        receipt.update(source=source, schema=source + "-approval-receipt/v1", candidate_path=str(self.path), candidate_namespace=self.identity["namespace"])
        authority = copy.deepcopy(self.base["authority"])
        authority.update({k: approval[k] for k in ("run_id", "namespace", "approval_scope", "project_root", "mode")})
        authority.update(human_receipt=copy.deepcopy(receipt), write_scopes=[candidate["namespace"]])
        self.payload, self.authority = payload, authority

    def command(self):
        return self.kernel.make_command("approve_objective", self.payload, authority_ref=self.authority, protected_fields=["objective_ref"], scope=[self.identity["namespace"]], idempotency_key="runtime-approval-v001")

    def test_real_approval_cold_resume(self):
        self.setup_run()
        approval_schema = json.loads((ROOT / "schemas/objective-approval-v1.schema.json").read_text())
        command_schema = json.loads((ROOT / "schemas/dag-command-v1.schema.json").read_text())
        validate_document(self.command(), command_schema, approval_schema["$defs"])
        self.kernel.apply(self.command())
        cold = ControlKernel(self.project, "runtime-run")
        self.assertEqual(cold.resume()["objective_ref"]["version"], "v001")
        state = cold.read_state()
        self.assertEqual(len(state["objective_approvals"]), 1)
        self.assertEqual(state["objective_history"][0]["version"], "intake")
        self.assertEqual(cold.read_object(state["objective_ref"]["approval_ref"])["payload"]["receipt"]["source"], "human")

    def test_mock_is_retained_and_cannot_become_real(self):
        self.setup_run("rehearsal")
        approval_schema = json.loads((ROOT / "schemas/objective-approval-v1.schema.json").read_text())
        command_schema = json.loads((ROOT / "schemas/dag-command-v1.schema.json").read_text())
        validate_document(self.command(), command_schema, approval_schema["$defs"])
        self.kernel.apply(self.command())
        state = ControlKernel(self.project, "runtime-run").read_state()
        self.assertEqual(self.kernel.read_object(state["objective_ref"]["approval_ref"])["payload"]["receipt"]["source"], "mock")
        state["metadata"]["runtime_identity"]["mode"] = "real"
        with self.assertRaises(KernelError):
            self.kernel._validate_state(state)

    def test_forged_bindings_fail_without_mutation(self):
        self.setup_run()
        pristine = self.command()
        for location, key, value in [
            ("receipt", "source", "mock"), ("receipt", "candidate_digest", "sha256:" + "f" * 64),
            ("approval", "project_root", "/wrong-project"), ("approval", "namespace", "project:foreign"),
            ("approval", "mode", "rehearsal"), ("candidate", "digest", "sha256:" + "f" * 64),
        ]:
            command = copy.deepcopy(pristine)
            target = command["payload"]["approval"]["receipt"] if location == "receipt" else command["payload"]["approval"] if location == "approval" else command["payload"]["candidate_ref"]
            target[key] = value
            before = self.kernel.head()
            with self.subTest(location=location, key=key), self.assertRaises(KernelError):
                self.kernel.apply(command)
            self.assertEqual(before, self.kernel.head())

    def transition_authority(self):
        return {"approved": True, "scopes": ["open_operational_group"], "runtime_identity": self.identity, "run_id": "runtime-run", "write_scopes": [self.identity["namespace"]], "protected_fields": ["group", "epoch", "ready"], "human_receipt": self.payload["approval"]["receipt"]}

    def execution_run(self, mode):
        self.setup_run(mode)
        self.kernel.apply(self.command())
        close = {"approved": True, "scopes": ["close_epoch", "close_group", "publish_artifact"], "human_receipt": self.payload["approval"]["receipt"]}
        for next_group in ("C", "D"):
            self.kernel.close_epoch(authority_ref=close)
            self.kernel.close_group(next_group=next_group, authority_ref=close)
            self.kernel.open_operational_group(next_group, next_group + "-01", self.kernel.read_state()["group"]["bundle_ref"], authority_ref=self.transition_authority())
        ref = {"path": str(self.path), "version": "v001", "digest": self.payload["candidate_ref"]["digest"]}
        auth = {"authority_ref": ref, "namespace": "project:runtime-run", "scope": "candidate-generic", "owner_ref": ref}
        task = {"task_id": "planned", "files": ["app.py"], "interface": "observable parser", "inputs": [ref], "outputs": ["app.py"], "checks": ["result verified"], "stop": "scope exhausted", "report_path": "reports/planned.json", "parent_outcome_contribution": "outcome"}
        brief = {"task_id": "planned", "purpose_ref": ref, "task": "parser", "interface": "observable parser", "write_scope": ["app.py"], "checks": ["result verified"], "stop": "scope exhausted", "report_path": "reports/planned.json", "exploration_refs": [ref]}
        for local, extra in (("D8", {"tasks": [task]}), ("D10", {"briefs": [brief]}), ("D12", {"evidence_refs": [ref]})):
            inputs = {"input_refs": [ref], **extra}
            head = self.kernel.head()
            compiled = PlanningSystemV1().compile("group.D." + local, inputs, auth, {key: head[key] for key in ("revision", "transaction_digest")})
            self.kernel.publish_artifact("runtime-" + local, "v1", {"qualified_id": "group.D." + local, "runtime_identity": self.identity, "objective_digest": ref["digest"], "inputs": inputs, "compiled": compiled}, authority_ref=close)
        self.kernel.close_epoch(authority_ref=close)
        self.kernel.close_group(next_group="E", acceptance_evidence=[self.kernel.read_state()["artifacts"]["runtime-D12"]["digest"]], authority_ref=close)
        self.kernel.open_operational_group("E", "E-01", self.kernel.read_state()["group"]["bundle_ref"], authority_ref=self.transition_authority())

    def assert_execution_scope(self, mode):
        self.execution_run(mode)
        authority = {"approved": True, "scopes": ["publish_task_package", "claim_task"], "write_scopes": ["app.py", "reports/planned.json", "unrelated.py"]}
        self.kernel.publish_task_package("planned", {"write_scope": ["app.py", "reports/planned.json"], "output_path": "reports/planned.json"}, assignment_id="worker", authority_ref=authority)
        self.kernel.claim_task("planned", assignment_id="worker", authority_ref=authority)
        self.assertEqual(ControlKernel(self.project, "runtime-run").read_state()["tasks"]["planned"]["status"], "running")
        self.kernel.publish_task_package("unplanned", {"write_scope": ["unrelated.py"], "output_path": "unrelated.py"}, assignment_id="other", authority_ref=authority)
        with self.assertRaises(KernelError):
            self.kernel.claim_task("unplanned", assignment_id="other", authority_ref=authority)
        with self.assertRaises(KernelError):
            self.kernel.publish_task_package("escape", {"write_scope": ["../outside.py"]}, assignment_id="escape", authority_ref=authority)

    def test_real_execution_is_limited_to_closed_d_plan(self):
        self.assert_execution_scope("real")

    def test_mock_execution_is_limited_to_closed_d_plan(self):
        self.assert_execution_scope("rehearsal")

    def test_pre_d_task_cannot_be_claimed(self):
        self.setup_run()
        authority = {"approved": True, "scopes": ["publish_task_package", "claim_task"], "write_scopes": ["app.py"]}
        self.kernel.publish_task_package("early", {"write_scope": ["app.py"], "output_path": "app.py"}, assignment_id="worker", authority_ref=authority)
        with self.assertRaises(KernelError):
            self.kernel.claim_task("early", assignment_id="worker", authority_ref=authority)

    def test_group_transition_cold_resume_and_refusals(self):
        self.setup_run()
        self.kernel.apply(self.command())
        close_auth = {"approved": True, "scopes": ["close_epoch", "close_group"], "human_receipt": self.payload["approval"]["receipt"]}
        self.kernel.close_epoch(authority_ref=close_auth)
        self.kernel.close_group(next_group="C", authority_ref=close_auth)
        ref = self.kernel.read_state()["group"]["bundle_ref"]
        for group, epoch, candidate_ref in [("D", "D-01", ref), ("C", "B-01", ref), ("C", "C-01", {**ref, "digest": "sha256:" + "f" * 64})]:
            with self.assertRaises(KernelError):
                self.kernel.open_operational_group(group, epoch, candidate_ref, authority_ref=self.transition_authority())
        command = self.kernel.make_command("open_operational_group", {"group_id": "C", "epoch_id": "C-01", "input_ref": ref}, authority_ref=self.transition_authority(), input_refs=[ref], protected_fields=["group", "epoch", "ready"], scope=[self.identity["namespace"]])
        self.kernel.open_operational_group("C", "C-01", ref, authority_ref=self.transition_authority())
        self.assertEqual(ControlKernel(self.project, "runtime-run").resume()["group"]["id"], "C")
        with self.assertRaises(KernelError):
            self.kernel.apply(command)
        with self.assertRaises(KernelError):
            self.kernel.open_operational_group("C", "C-02", ref, authority_ref=self.transition_authority())


if __name__ == "__main__":
    unittest.main()
