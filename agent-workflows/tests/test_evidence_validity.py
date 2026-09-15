import unittest

from ai_agent_workflow.evidence_validity import (
    EvidenceValidityError,
    assess_evidence,
    bind_reused_evidence,
)
from ai_agent_workflow.loop_contracts import canonical_digest


def digest(letter):
    return "sha256:" + letter * 64


def current(**changes):
    value = {
        "candidate_digest": digest("a"),
        "spec_digest": digest("b"),
        "source_digest": digest("c"),
        "dependency_digest": digest("d"),
        "environment_digest": digest("e"),
        "check_definition_digest": digest("f"),
        "required_coverage": ["R1", "R2"],
    }
    value.update(changes)
    return value


def receipt(**changes):
    inputs = current()
    value = {
        "schema": "loop-evidence-record/v1",
        "evidence_id": "evidence-1",
        "evidence_digest": "",
        "candidate_digest": inputs["candidate_digest"],
        "spec_digest": inputs["spec_digest"],
        "source_digest": inputs["source_digest"],
        "dependency_digest": inputs["dependency_digest"],
        "environment_digest": inputs["environment_digest"],
        "check_definition_digest": inputs["check_definition_digest"],
        "coverage": ["R1", "R2"],
        "status": "pass",
    }
    value.update(changes)
    value["evidence_digest"] = canonical_digest({key: item for key, item in value.items() if key != "evidence_digest"})
    return value


class EvidenceValidityTests(unittest.TestCase):
    def test_matching_receipt_is_valid_without_rebinding(self):
        result = assess_evidence(receipt(), current(), {"known": True, "invalidated_dimensions": [], "impacted_coverage": []})
        self.assertEqual("valid", result["status"])
        self.assertFalse(result["rebind_required"])
        self.assertEqual(["R1", "R2"], result["reusable_coverage"])

    def test_candidate_change_can_rebind_when_inputs_and_impact_are_known(self):
        result = assess_evidence(receipt(), current(candidate_digest=digest("9")), {"known": True, "invalidated_dimensions": [], "impacted_coverage": []})
        self.assertEqual("valid", result["status"])
        self.assertTrue(result["rebind_required"])
        rebound = bind_reused_evidence(result, {"id": "old-evidence", "digest": receipt()["evidence_digest"]}, digest("9"))
        self.assertEqual(digest("9"), rebound["candidate_digest"])
        self.assertEqual("loop-evidence-reuse/v1", rebound["schema"])

    def test_source_change_invalidates_only_known_impacted_coverage(self):
        result = assess_evidence(
            receipt(),
            current(source_digest=digest("8")),
            {"known": True, "invalidated_dimensions": ["source"], "impacted_coverage": ["R2"]},
        )
        self.assertEqual("invalid", result["status"])
        self.assertEqual(["R1"], result["reusable_coverage"])
        self.assertEqual(["R2"], result["rerun_coverage"])

    def test_spec_or_environment_change_invalidates_full_required_set(self):
        for field, dimension in (("spec_digest", "spec"), ("environment_digest", "environment")):
            with self.subTest(field=field):
                result = assess_evidence(
                    receipt(),
                    current(**{field: digest("7")}),
                    {"known": True, "invalidated_dimensions": [dimension], "impacted_coverage": []},
                )
                self.assertEqual("invalid", result["status"])
                self.assertEqual(["R1", "R2"], result["rerun_coverage"])

    def test_unknown_impact_fails_closed_and_mismatch_claim_must_be_exact(self):
        result = assess_evidence(
            receipt(), current(candidate_digest=digest("6")),
            {"known": False, "invalidated_dimensions": [], "impacted_coverage": []},
        )
        self.assertEqual("unknown", result["status"])
        self.assertEqual(["R1", "R2"], result["rerun_coverage"])
        with self.assertRaises(EvidenceValidityError):
            assess_evidence(
                receipt(), current(source_digest=digest("5")),
                {"known": True, "invalidated_dimensions": [], "impacted_coverage": []},
            )

    def test_tampered_receipt_and_invalid_rebind_are_rejected(self):
        broken = receipt()
        broken["coverage"] = ["R1"]
        with self.assertRaises(EvidenceValidityError):
            assess_evidence(broken, current(), {"known": True, "invalidated_dimensions": [], "impacted_coverage": []})
        valid = assess_evidence(receipt(), current(), {"known": True, "invalidated_dimensions": [], "impacted_coverage": []})
        with self.assertRaises(EvidenceValidityError):
            bind_reused_evidence(valid, {"id": "wrong", "digest": digest("0")}, digest("a"))


if __name__ == "__main__":
    unittest.main()
