"""Receipt-driven B7 adoption exercises the compiler and actual Kernel."""
import copy
from datetime import datetime, timedelta, timezone
import hashlib
import json
from pathlib import Path
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "src"))
from ai_agent_workflow.control_kernel import ControlKernel, KernelError
from ai_agent_workflow.runtime_approval import RuntimeApprovalError, adopt_approved_objective, approval_context


class RuntimeApprovalAdapterTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.project = Path(self.temporary.name).resolve()
        (self.project / "docs").mkdir()

    def write(self, name, value, version="v1"):
        path = self.project / "docs" / name
        raw = (json.dumps(value, sort_keys=True) + "\n").encode()
        path.write_bytes(raw)
        return {"path": str(path), "version": version, "digest": "sha256:" + hashlib.sha256(raw).hexdigest()}

    def arguments(self, mode="real"):
        args = {"intake_ref": self.write("intake.json", {"request": "A complete sample"}, "intake"),
                "candidate_ref": self.write("candidate.json", {"objective": "Sample passes an observable check"}, "v001"),
                "proposal_ref": self.write("proposal.json", {"choice": "selected by supplied receipt"}),
                "actor_ref": self.write("actor.json", {"actor_id": "sample-owner", "source": "mock" if mode == "rehearsal" else "human"}),
                "mode": mode}
        context = approval_context(self.project, "sample", **args)
        self.assertNotIn("decision", context)
        self.assertNotIn("explicit", context)
        self.assertFalse((self.project / ".local").exists())
        receipt = {**context, "receipt_id": "sample-receipt", "decision": "approve", "explicit": True, "issued_at": datetime.now(timezone.utc).isoformat()}
        args["receipt_ref"] = self.write("receipt.json", receipt)
        args["preapproval_steps"] = self.steps(args)
        return args

    def steps(self, args):
        ref = {"path": "docs/candidate.json", "digest": args["candidate_ref"]["digest"], "selector": "candidate"}
        base = {"input_refs": [ref], "candidate_ref": ref, "version": "v001"}
        result = []
        for n in range(1, 7):
            values = copy.deepcopy(base)
            if n == 4:
                values.update(material_unknowns=[], inquiry_complete=True, resolution_reason="Supplied physical candidate resolves sample scope", resolution_refs=[ref])
            if n == 5:
                values["options"] = [{"tradeoff": "less scope"}, {"tradeoff": "more scope"}]
            result.append(("group.B.B%d" % n, values))
        return result

    def test_real_receipt_approval_and_identical_cold_retry(self):
        args = self.arguments()
        kernel = adopt_approved_objective(self.project, "sample", **args)
        state = kernel.read_state()
        self.assertEqual(len(state["objective_approvals"]), 1)
        self.assertIn("runtime-B7", state["artifacts"])
        self.assertEqual(state["review_budget"]["max_rounds"], 2)
        self.assertEqual(state["review_budget"]["max_attempts_per_finding"], 5)
        head = kernel.head()
        retry = adopt_approved_objective(self.project, "sample", **args)
        self.assertEqual(head, retry.head())
        self.assertEqual(retry.resume()["objective_ref"]["version"], "v001")

    def test_missing_incomplete_or_out_of_order_steps_never_register_run(self):
        args = self.arguments()
        complete = args.pop("preapproval_steps")
        invalid = [None, [], complete[:-1], list(reversed(complete)), complete[:-1] + [["group.B.B6", None]]]
        for steps in invalid:
            with self.subTest(steps=steps), self.assertRaises(RuntimeApprovalError):
                adopt_approved_objective(self.project, "sample", **args, preapproval_steps=steps)
            self.assertIsNone(ControlKernel(self.project, "sample").head())
            self.assertFalse((self.project / ".local").exists())
        with self.assertRaises(RuntimeApprovalError):
            adopt_approved_objective(self.project, "sample", **args)
        self.assertIsNone(ControlKernel(self.project, "sample").head())
        self.assertFalse((self.project / ".local").exists())

    def test_project_traversal_is_refused_before_registration(self):
        args = self.arguments()
        # Existing bytes plus a lexical project/.. spelling used to pass the
        # containment predicate. No external file needs to be created.
        for name in ("intake_ref", "candidate_ref", "proposal_ref", "actor_ref", "receipt_ref"):
            changed = copy.deepcopy(args)
            original = Path(changed[name]["path"])
            changed[name]["path"] = str(self.project / ".." / self.project.name / original.relative_to(self.project))
            with self.subTest(name=name), self.assertRaises(RuntimeApprovalError):
                adopt_approved_objective(self.project, "sample", **changed)
            self.assertIsNone(ControlKernel(self.project, "sample").head())
            self.assertFalse((self.project / ".local").exists())
        changed = copy.deepcopy(args)
        changed["preapproval_steps"][0][1]["input_refs"][0]["path"] = str(self.project / ".." / self.project.name / "docs/candidate.json")
        with self.assertRaises(RuntimeApprovalError):
            adopt_approved_objective(self.project, "sample", **changed)
        self.assertIsNone(ControlKernel(self.project, "sample").head())
        self.assertFalse((self.project / ".local").exists())

    def test_rehearsal_executes_all_b_compilers_and_preserves_mock(self):
        args = self.arguments("rehearsal")
        args["preapproval_steps"] = self.steps(args)
        kernel = adopt_approved_objective(self.project, "sample", **args)
        state = kernel.read_state()
        self.assertEqual(set(state["artifacts"]), {"runtime-B%d" % n for n in range(1, 8)})
        approval = kernel.read_object(state["objective_ref"]["approval_ref"])["payload"]
        self.assertEqual(approval["receipt"]["source"], "mock")
        revisions = [kernel.read_object(state["artifacts"]["runtime-B%d" % n]["object_ref"])["payload"]["payload"]["compiled"]["expected_head"]["revision"] for n in range(1, 8)]
        self.assertEqual(revisions, list(range(1, 8)))
        before = kernel.head()
        args["mode"] = "real"
        with self.assertRaises(RuntimeApprovalError):
            adopt_approved_objective(self.project, "sample", **args)
        self.assertEqual(before, kernel.head())

    def test_bad_receipt_and_compiler_refusal_create_no_run(self):
        args = self.arguments()
        receipt = json.loads(Path(args["receipt_ref"]["path"]).read_text())
        receipt["explicit"] = False
        args["receipt_ref"] = self.write("receipt.json", receipt)
        with self.assertRaises((RuntimeApprovalError, KernelError)):
            adopt_approved_objective(self.project, "sample", **args)
        self.assertIsNone(ControlKernel(self.project, "sample").head())
        receipt["explicit"] = True
        args["receipt_ref"] = self.write("receipt.json", receipt)
        args["preapproval_steps"] = self.steps(args)
        args["preapproval_steps"][4][1]["selected_option"] = "invented-approval"
        with self.assertRaises(RuntimeApprovalError):
            adopt_approved_objective(self.project, "sample", **args)
        self.assertIsNone(ControlKernel(self.project, "sample").head())

    def test_stale_physical_input_and_changed_retry_are_refused(self):
        args = self.arguments()
        kernel = adopt_approved_objective(self.project, "sample", **args)
        before = kernel.head()
        with self.assertRaises(RuntimeApprovalError):
            adopt_approved_objective(self.project, "sample", **args, budget_seconds=999)
        Path(args["proposal_ref"]["path"]).write_text("changed")
        with self.assertRaises(RuntimeApprovalError):
            adopt_approved_objective(self.project, "sample", **args)
        self.assertEqual(before, kernel.head())

    def test_future_or_expired_receipt_never_registers_run(self):
        args = self.arguments()
        receipt = json.loads(Path(args["receipt_ref"]["path"]).read_text())
        for delta in (timedelta(hours=1), timedelta(hours=-1)):
            receipt["issued_at"] = (datetime.now(timezone.utc) + delta).isoformat()
            args["receipt_ref"] = self.write("receipt.json", receipt)
            with self.assertRaises(RuntimeApprovalError):
                adopt_approved_objective(self.project, "sample", **args)
            self.assertIsNone(ControlKernel(self.project, "sample").head())


if __name__ == "__main__":
    unittest.main()
