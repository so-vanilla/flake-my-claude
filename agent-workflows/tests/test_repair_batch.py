import copy
import unittest

from ai_agent_workflow.repair_batch import (
    RepairBatchError,
    assess_batch_resolution,
    plan_fix_batches,
)

DIGEST = "sha256:" + "a" * 64


def finding(identifier, *, batch="root-a", root="cause-a", depends=(), conflicts=(), classification="required"):
    return {
        "finding_id": identifier,
        "fingerprint": "fingerprint-" + identifier,
        "classification": classification,
        "candidate_digest": DIGEST,
        "batch_key": batch,
        "root_cause": root,
        "write_scope": ["src/a.py"],
        "verification": ["test-a"],
        "depends_on": list(depends),
        "conflicts_with": list(conflicts),
        "resolution_conditions": ["test passes for " + identifier],
    }


class RepairBatchTests(unittest.TestCase):
    def test_three_compatible_required_findings_share_one_round(self):
        plan = plan_fix_batches([finding("F1"), finding("F2"), finding("F3")])
        self.assertEqual(1, len(plan["batches"]))
        self.assertEqual(["F1", "F2", "F3"], plan["batches"][0]["finding_ids"])

    def test_non_required_findings_are_retained_but_not_dispatched(self):
        plan = plan_fix_batches([finding("F1"), finding("F2", classification="duplicate")])
        self.assertEqual(["F1"], plan["batches"][0]["finding_ids"])
        self.assertEqual(["F2"], plan["non_required_finding_ids"])

    def test_conflicts_dependencies_and_different_roots_cannot_hide_in_one_batch(self):
        for values in (
            [finding("F1", conflicts=("F2",)), finding("F2")],
            [finding("F1", depends=("F2",)), finding("F2")],
            [finding("F1"), finding("F2", root="cause-b")],
        ):
            with self.subTest(values=values), self.assertRaises(RepairBatchError):
                plan_fix_batches(values)

    def test_separate_dependency_is_preserved_between_batches(self):
        plan = plan_fix_batches([
            finding("F1", batch="one", depends=("F2",)),
            finding("F2", batch="two"),
        ])
        batches = {item["finding_ids"][0]: item for item in plan["batches"]}
        self.assertEqual([batches["F2"]["batch_id"]], batches["F1"]["depends_on_batches"])

    def test_required_finding_cannot_depend_on_non_required_finding(self):
        with self.assertRaises(RepairBatchError):
            plan_fix_batches([finding("F1", depends=("F2",)), finding("F2", classification="defer")])

    def test_dependency_cycle_is_rejected(self):
        with self.assertRaises(RepairBatchError):
            plan_fix_batches([finding("F1", batch="one", depends=("F2",)), finding("F2", batch="two", depends=("F1",))])

    def test_resolution_never_closes_an_unreported_finding(self):
        batch = plan_fix_batches([finding("F1"), finding("F2"), finding("F3")])["batches"][0]
        results = [
            {"finding_id": item, "status": "resolved", "evidence_refs": [{"id": "e-" + item, "digest": DIGEST}], "unresolved_conditions": []}
            for item in ("F1", "F2")
        ] + [{"finding_id": "F3", "status": "open", "evidence_refs": [], "unresolved_conditions": ["test fails"]}]
        resolution = assess_batch_resolution(batch, results)
        self.assertFalse(resolution["complete"])
        self.assertEqual(["F3"], resolution["open_finding_ids"])
        with self.assertRaises(RepairBatchError):
            assess_batch_resolution(batch, results[:-1])

    def test_resolution_requires_physical_ref_and_binds_batch_candidate_and_conditions(self):
        batch = plan_fix_batches([finding("F1")])["batches"][0]
        with self.assertRaises(RepairBatchError):
            assess_batch_resolution(
                batch,
                [{"finding_id": "F1", "status": "resolved", "evidence_refs": [{"digest": DIGEST}], "unresolved_conditions": []}],
            )
        result = assess_batch_resolution(
            batch,
            [{"finding_id": "F1", "status": "resolved", "evidence_refs": [{"id": "fix-proof", "digest": DIGEST}], "unresolved_conditions": []}],
        )
        finding_result = result["finding_results"][0]
        self.assertEqual(DIGEST, result["batch_ref"]["candidate_digest"])
        self.assertEqual(DIGEST, finding_result["evidence_binding"]["candidate_digest"])
        self.assertEqual(batch["resolution_conditions"]["F1"], finding_result["evidence_binding"]["resolution_conditions"])

    def test_resolution_cannot_close_with_any_unresolved_condition(self):
        batch = plan_fix_batches([finding("F1")])["batches"][0]
        with self.assertRaises(RepairBatchError):
            assess_batch_resolution(
                batch,
                [{"finding_id": "F1", "status": "resolved", "evidence_refs": [{"id": "fix-proof", "digest": DIGEST}], "unresolved_conditions": ["unbound"]}],
            )

    def test_tampered_batch_is_rejected(self):
        batch = plan_fix_batches([finding("F1")])["batches"][0]
        tampered = copy.deepcopy(batch)
        tampered["write_scope"].append("src/escape.py")
        with self.assertRaises(RepairBatchError):
            assess_batch_resolution(tampered, [])


if __name__ == "__main__":
    unittest.main()
