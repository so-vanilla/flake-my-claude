from __future__ import annotations

import copy
import json
import unittest
from pathlib import Path

from ai_agent_workflow.completion import (
    CLASSIFICATION_SCHEMA,
    MACHINE_DECISION_RECEIPT_SCHEMA,
    CompletionClassificationError,
    MachineDecisionReceiptError,
    classify_completion,
    create_machine_decision_receipt,
    validate_machine_decision_receipt,
)
from ai_agent_workflow.loop_contracts import canonical_digest

ROOT = Path(__file__).resolve().parent
FIXTURE = json.loads((ROOT / "fixtures" / "loop-control-v1" / "contract-cases.json").read_text())
DIGEST = "sha256:" + "b" * 64


def _evidence(candidate: str = DIGEST, *, evidence_id: str = "evidence-1", status: str = "pass") -> dict:
    value = {
        "schema": "loop-evidence-record/v1",
        "evidence_id": evidence_id,
        "evidence_digest": "",
        "candidate_digest": candidate,
        "spec_digest": DIGEST,
        "source_digest": DIGEST,
        "dependency_digest": DIGEST,
        "environment_digest": DIGEST,
        "check_definition_digest": DIGEST,
        "coverage": ["R1"],
        "status": status,
    }
    value["evidence_digest"] = canonical_digest({key: item for key, item in value.items() if key != "evidence_digest"})
    return value


def _requirement(evidence: dict) -> dict:
    return {
        "schema": "loop-requirement-assessment/v1",
        "requirement_id": "R1",
        "status": "pass",
        "scope": ["src/a.py"],
        "evidence_refs": [{"id": evidence["evidence_id"], "digest": evidence["evidence_digest"]}],
    }


def _review(axis: str, *, candidate: str = DIGEST, package: str = DIGEST, actor: str, context: str, **extra) -> dict:
    return {
        "schema": "loop-review-assessment/v1",
        "review_id": "review-" + axis,
        "axis": axis,
        "actor_id": actor,
        "context_epoch": context,
        "candidate_digest": candidate,
        "package_digest": package,
        "coverage": ["R1"],
        "completed": True,
        "unevaluated": [],
        "finding_refs": [],
        **extra,
    }


class CompletionClassificationTests(unittest.TestCase):
    def setUp(self) -> None:
        self.identity = copy.deepcopy(FIXTURE["identity"])
        self.evidence = _evidence()
        self.request = {
            "identity": self.identity,
            "candidate_digest": DIGEST,
            "package_digest": DIGEST,
            "requirements": [_requirement(self.evidence)],
            "reviews": [
                _review("architecture-safety", actor="reviewer-a", context="epoch-a"),
                _review("integration-operability", actor="reviewer-b", context="epoch-b"),
            ],
            "evidence": [self.evidence],
            "findings": [],
        }

    def test_completed_requires_all_gates_and_is_non_mutating(self):
        original = copy.deepcopy(self.request)
        result = classify_completion(self.request)
        self.assertEqual(CLASSIFICATION_SCHEMA, result["schema"])
        self.assertEqual("completed", result["outcome"])
        self.assertTrue(result["completed"])
        self.assertEqual(original, self.request)
        self.assertFalse(result["human_approval"])
        self.assertFalse(result["d5_selection"])
        self.assertFalse(result["objective_achievement"])

    def test_empty_findings_do_not_complete_without_the_other_gates(self):
        request = copy.deepcopy(self.request)
        request["requirements"] = []
        request["reviews"] = []
        request["evidence"] = []
        result = classify_completion(request)
        self.assertEqual("needs-input", result["outcome"])
        self.assertFalse(result["completed"])
        self.assertIn("no-required-assessments", result["reason_codes"])

    def test_reviews_must_share_candidate_and_package_and_be_fresh(self):
        stale = copy.deepcopy(self.request)
        stale["reviews"][0]["candidate_digest"] = "sha256:" + "c" * 64
        self.assertEqual("needs-input", classify_completion(stale)["outcome"])
        stale = copy.deepcopy(self.request)
        stale["reviews"][1]["package_digest"] = "sha256:" + "c" * 64
        self.assertEqual("needs-input", classify_completion(stale)["outcome"])
        same_actor = copy.deepcopy(self.request)
        same_actor["reviews"][1]["actor_id"] = "reviewer-a"
        self.assertFalse(classify_completion(same_actor)["checks"]["reviews_fresh"])
        same_context = copy.deepcopy(self.request)
        same_context["reviews"][1]["context_epoch"] = "epoch-a"
        self.assertFalse(classify_completion(same_context)["checks"]["reviews_fresh"])

        incomplete_coverage = copy.deepcopy(self.request)
        incomplete_coverage["required_requirement_ids"] = ["R1", "R2"]
        second = copy.deepcopy(incomplete_coverage["requirements"][0])
        second["requirement_id"] = "R2"
        incomplete_coverage["requirements"].append(second)
        result = classify_completion(incomplete_coverage)
        self.assertFalse(result["completed"])
        self.assertFalse(result["checks"]["reviews_covered"])

        narrowed = copy.deepcopy(incomplete_coverage)
        narrowed["required_requirement_ids"] = ["R1"]
        with self.assertRaises(CompletionClassificationError):
            classify_completion(narrowed)

    def test_unknown_contradiction_and_open_required_findings_block(self):
        unknown = copy.deepcopy(self.request)
        unknown["mandatory_unknowns"] = ["scope-boundary"]
        self.assertFalse(classify_completion(unknown)["completed"])
        contradiction = copy.deepcopy(self.request)
        contradiction["contradictions"] = ["review-vs-evidence"]
        self.assertFalse(classify_completion(contradiction)["completed"])
        finding = copy.deepcopy(self.request)
        finding["reviews"][0]["finding_refs"] = [{"id": "finding-1", "digest": DIGEST}]
        finding["findings"] = [{"finding_id": "finding-1", "required": True, "status": "open"}]
        result = classify_completion(finding)
        self.assertFalse(result["completed"])
        self.assertFalse(result["checks"]["no_open_required_findings"])

    def test_requirement_reference_digest_must_bind_the_current_evidence(self):
        stale_ref = copy.deepcopy(self.request)
        stale_ref["requirements"][0]["evidence_refs"][0]["digest"] = "sha256:" + "c" * 64
        result = classify_completion(stale_ref)
        self.assertFalse(result["completed"])
        self.assertFalse(result["checks"]["requirements_have_evidence"])

    def test_non_completed_outcomes_require_explicit_blocker(self):
        for outcome in ("stalled", "iteration-limit", "execution-failed", "recovery-required"):
            request = copy.deepcopy(self.request)
            request["blockers"] = [{"outcome": outcome, "reason": "fixture blocker"}]
            self.assertEqual(outcome, classify_completion(request)["outcome"])
        incomplete = copy.deepcopy(self.request)
        incomplete["reviews"] = []
        self.assertEqual("needs-input", classify_completion(incomplete)["outcome"])

    def test_malformed_contract_values_fail_closed(self):
        malformed = copy.deepcopy(self.request)
        malformed["reviews"][0]["unevaluated"] = ["R1"]
        with self.assertRaises(CompletionClassificationError):
            classify_completion(malformed)
        malformed = copy.deepcopy(self.request)
        malformed["evidence"][0]["candidate_digest"] = "sha256:" + "c" * 64
        # The evidence record digest now no longer binds the record.
        with self.assertRaises(CompletionClassificationError):
            classify_completion(malformed)


class MachineDecisionReceiptTests(unittest.TestCase):
    def test_receipt_is_distinct_and_canonically_digest_bound(self):
        evidence = _evidence()
        request = {
            "identity": FIXTURE["identity"],
            "candidate_digest": DIGEST,
            "package_digest": DIGEST,
            "requirements": [_requirement(evidence)],
            "reviews": [
                _review("architecture-safety", actor="reviewer-a", context="epoch-a"),
                _review("integration-operability", actor="reviewer-b", context="epoch-b"),
            ],
            "evidence": [evidence],
            "findings": [],
        }
        classification = classify_completion(request)
        receipt = create_machine_decision_receipt(classification, "machine-receipt-1")
        self.assertEqual(MACHINE_DECISION_RECEIPT_SCHEMA, receipt["schema"])
        self.assertTrue(receipt["non_authorizing"])
        self.assertEqual("deterministic-machine", receipt["producer"]["kind"])
        self.assertFalse(receipt["human_approval"])
        self.assertFalse(receipt["d5_selection"])
        self.assertFalse(receipt["objective_achievement"])
        self.assertEqual(receipt, validate_machine_decision_receipt(receipt))
        tampered = copy.deepcopy(receipt)
        tampered["classification"]["outcome"] = "needs-input"
        with self.assertRaises(MachineDecisionReceiptError):
            validate_machine_decision_receipt(tampered)

    def test_receipt_rejects_wrong_expected_candidate_or_package(self):
        evidence = _evidence()
        request = {
            "identity": FIXTURE["identity"], "candidate_digest": DIGEST, "package_digest": DIGEST,
            "requirements": [_requirement(evidence)],
            "reviews": [_review("architecture-safety", actor="reviewer-a", context="epoch-a"), _review("integration-operability", actor="reviewer-b", context="epoch-b")],
            "evidence": [evidence], "findings": [],
        }
        receipt = create_machine_decision_receipt(classify_completion(request))
        with self.assertRaises(MachineDecisionReceiptError):
            validate_machine_decision_receipt(receipt, expected_candidate_digest="sha256:" + "c" * 64)

    def test_receipt_builder_rejects_a_forged_completed_classification(self):
        evidence = _evidence()
        request = {
            "identity": FIXTURE["identity"], "candidate_digest": DIGEST,
            "package_digest": DIGEST, "requirements": [_requirement(evidence)],
            "reviews": [
                _review("architecture-safety", actor="reviewer-a", context="epoch-a"),
                _review("integration-operability", actor="reviewer-b", context="epoch-b"),
            ],
            "evidence": [evidence], "findings": [],
        }
        forged = classify_completion(request)
        forged["source_request"]["requirements"] = []
        with self.assertRaises(MachineDecisionReceiptError):
            create_machine_decision_receipt(forged)


if __name__ == "__main__":
    unittest.main()
