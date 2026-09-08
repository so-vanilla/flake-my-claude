import copy
import hashlib
import json
import os
from pathlib import Path
import shutil
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]
SRC = ROOT / "agent-workflows" / "src"
if str(SRC) not in os.sys.path:
    os.sys.path.insert(0, str(SRC))

from ai_agent_workflow.s2_evidence import S2EvidenceCompiler, verify_s2_source_evidence


IDS = [*(f"group.B.B{i}" for i in range(1, 8)), *(f"group.C.C{i}" for i in range(1, 7))]


class S2EvidenceTests(unittest.TestCase):
    def setUp(self):
        self.root = Path(tempfile.mkdtemp())
        self.addCleanup(shutil.rmtree, self.root, ignore_errors=True)

    def write(self, path, value):
        target = self.root / path
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(json.dumps(value, sort_keys=True), encoding="utf-8")
        return {"path": path, "digest": "sha256:" + hashlib.sha256(target.read_bytes()).hexdigest()}

    def acceptance(self, contract_id, selector, *, schema="qualified-contract-acceptance/v1", kind="skill", status="passed"):
        return {
            "schema": schema,
            "contract_id": contract_id,
            "contract_name": contract_id + " acceptance",
            "implementation_kind": kind,
            "implementation_ref": {"path": "agent-workflows/tests/example.py", "digest": "sha256:" + "c" * 64},
            "test_ref": {"selector": selector},
            "status": status,
        }

    def inputs(self):
        refs = {}
        for index, item in enumerate(IDS):
            selector = item.replace("group.", "") + "Tests"
            ref = self.write(f"evidence/{index}.json", self.acceptance(item, selector))
            refs[item] = {**ref, "selector": selector}
        receipts = {
            "s0_authority": {"path": "agent-workflows/manifests/s0-source-transition-current-authority.json", "digest": "sha256:" + "a" * 64},
            "s1_authority": {"path": "agent-workflows/manifests/s1-source-authority.json", "digest": "sha256:" + "b" * 64},
            "rotation_receipt": {"receipt_id": "S2-U-R-U-001", "digest": "sha256:8eeb198dd2a3592733c4e028767ee9d07450ea9a2f3e5df3323260d4268a6fe2"},
            "closure": {"required": 0, "needs_user": 0, "incomplete": 0, "unknown": 0, "integrity": 0, "stopped": 0},
        }
        counts = {"named_contracts": {"accepted": 29, "total": 60}, "profile_steps": {"accepted": 0, "total": 23}, "claim_vector": {"current_run_objective_approved": False, "actual_a7_handoff_complete": False, "migration_complete": False, "activation_complete": False, "source_wide_integration_complete": False, "full_workflow_ready": False}}
        receipts["counts_receipt"] = self.write("evidence/counts.json", counts)
        return refs, receipts, counts

    def actual_inputs(self):
        refs, receipts, counts = self.inputs()
        contracts = ROOT / "agent-workflows" / "evidence" / "contracts"
        for item in IDS:
            name = item.rsplit(".", 1)[1]
            source = contracts / f"{name}.json"
            path = f"agent-workflows/evidence/contracts/{name}.json"
            target = self.root / path
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(source, target)
            document = json.loads(target.read_text(encoding="utf-8"))
            refs[item] = {
                "path": path,
                "digest": "sha256:" + hashlib.sha256(target.read_bytes()).hexdigest(),
                "selector": document["test_ref"]["selector"],
            }
        return refs, receipts, counts

    def rewrite_acceptance(self, refs, item, **changes):
        path = self.root / refs[item]["path"]
        document = json.loads(path.read_text(encoding="utf-8"))
        document.update(changes)
        path.write_text(json.dumps(document, sort_keys=True), encoding="utf-8")
        refs[item]["digest"] = "sha256:" + hashlib.sha256(path.read_bytes()).hexdigest()

    def test_physical_thirteen_produces_candidate_but_never_fixed_acceptance(self):
        refs, receipts, counts = self.inputs()
        before = copy.deepcopy((refs, receipts, counts))
        result = S2EvidenceCompiler().compile(self.root, refs, receipts, counts)
        self.assertTrue(result["candidate"])
        self.assertFalse(result["section_accepted"])
        self.assertFalse(result["fixed_anchor"])
        self.assertEqual((refs, receipts, counts), before)
        self.assertFalse(verify_s2_source_evidence(self.root))

    def test_current_actual_b_and_c_acceptances_bind_contract_and_test_selector(self):
        refs, receipts, counts = self.actual_inputs()
        result = S2EvidenceCompiler().compile(self.root, refs, receipts, counts)
        self.assertTrue(result["candidate"])
        self.assertEqual(result["accepted_refs"], refs)

    def test_actual_acceptance_shape_rejects_unbound_or_non_accepting_documents(self):
        for change in ("contract_id", "cross_selector", "schema", "kind", "status", "stale", "path", "digest", "selector"):
            with self.subTest(change=change):
                refs, receipts, counts = self.actual_inputs()
                if change == "contract_id": self.rewrite_acceptance(refs, IDS[0], contract_id=IDS[1])
                if change == "cross_selector":
                    self.rewrite_acceptance(refs, IDS[0], test_ref={"selector": refs[IDS[1]]["selector"]})
                if change == "schema": self.rewrite_acceptance(refs, IDS[0], schema="other/v1")
                if change == "kind": self.rewrite_acceptance(refs, IDS[0], implementation_kind="other")
                if change == "status": self.rewrite_acceptance(refs, IDS[0], status="failed")
                if change == "stale":
                    path = self.root / refs[IDS[0]]["path"]
                    path.write_bytes(path.read_bytes() + b"\n")
                if change == "path": refs[IDS[1]]["path"] = refs[IDS[0]]["path"]
                if change == "digest": refs[IDS[1]]["digest"] = refs[IDS[0]]["digest"]
                if change == "selector": refs[IDS[1]]["selector"] = refs[IDS[0]]["selector"]
                with self.assertRaises(ValueError): S2EvidenceCompiler().compile(self.root, refs, receipts, counts)

    def test_duplicate_digest_selector_open_claims_and_bad_counts_are_refused(self):
        for change in ("digest", "selector", "open", "live", "count", "self"):
            with self.subTest(change=change):
                refs, receipts, counts = self.inputs()
                if change == "digest": refs[IDS[1]]["digest"] = refs[IDS[0]]["digest"]
                if change == "selector": refs[IDS[1]]["selector"] = refs[IDS[0]]["selector"]
                if change == "open": receipts["closure"]["required"] = 1
                if change == "live": counts["claim_vector"]["activation_complete"] = True
                if change == "count": counts["named_contracts"]["accepted"] = 30
                if change == "self": refs[IDS[0]]["path"] = "agent-workflows/manifests/s2-source-authority.json"
                with self.assertRaises(ValueError): S2EvidenceCompiler().compile(self.root, refs, receipts, counts)

    def test_duplicate_json_member_is_refused(self):
        refs, receipts, counts = self.inputs()
        path = self.root / refs[IDS[0]]["path"]
        path.write_text('{"qualified_id":"group.B.B1","qualified_id":"group.B.B1","selector":"B.B1Tests"}', encoding="utf-8")
        refs[IDS[0]]["digest"] = "sha256:" + hashlib.sha256(path.read_bytes()).hexdigest()
        with self.assertRaises(ValueError): S2EvidenceCompiler().compile(self.root, refs, receipts, counts)

    def test_root_escape_and_symlink_are_refused(self):
        refs, receipts, counts = self.inputs()
        refs[IDS[0]]["path"] = "../outside.json"
        with self.assertRaises(ValueError): S2EvidenceCompiler().compile(self.root, refs, receipts, counts)
        refs, receipts, counts = self.inputs()
        link = self.root / "linked"
        link.symlink_to(self.root / "evidence", target_is_directory=True)
        refs[IDS[0]]["path"] = "linked/0.json"
        with self.assertRaises(ValueError): S2EvidenceCompiler().compile(self.root, refs, receipts, counts)
