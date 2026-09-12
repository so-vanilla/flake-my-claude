import copy
import json
import unittest
from pathlib import Path

from ai_agent_workflow.loop_contracts import (
    LoopContractError,
    canonical_digest,
    counter_identity,
    phase_policy,
    validate_evidence_record,
    validate_iteration_event,
    validate_requirement_assessment,
    validate_resume_record,
    validate_review_assessment,
    validate_terminal_record,
    validate_work_identity,
)
from ai_agent_workflow.schema_validation import SchemaValidationError, validate_document

ROOT = Path(__file__).resolve().parent
FIXTURE = json.loads((ROOT / "fixtures" / "loop-control-v1" / "contract-cases.json").read_text())
DIGEST = "sha256:" + "b" * 64


class LoopContractTests(unittest.TestCase):
    def identity(self):
        return copy.deepcopy(FIXTURE["identity"])

    def test_fixture_phase_policies_match_the_shared_limits(self):
        for case in FIXTURE["policy_examples"]:
            policy = phase_policy(case["phase"])
            self.assertEqual(case["policy_id"], policy["policy_id"])
            self.assertEqual(case["limit"], policy["additional_iteration_limit"])
        with self.assertRaises(LoopContractError):
            phase_policy("A6")
        with self.assertRaises(LoopContractError):
            phase_policy("E2")

    def test_counter_identity_survives_scope_revision(self):
        original = self.identity()
        changed = self.identity()
        changed["scope_revision"] = "scope-r2"
        changed["requirements_digest"] = DIGEST
        self.assertEqual(counter_identity(original), counter_identity(changed))

    def test_identity_and_iteration_events_are_strict_and_non_mutating(self):
        identity = self.identity()
        self.assertEqual(identity, validate_work_identity(identity))
        event = {
            "schema": "loop-iteration-event/v1",
            "event_id": "event-001",
            "command_id": "command-001",
            "identity": identity,
            "kind": "initial",
            "status": "reserved",
            "attempt": 0,
            "predecessor_ref": None,
            "result_ref": None,
        }
        original = copy.deepcopy(event)
        self.assertEqual(event, validate_iteration_event(event))
        self.assertEqual(original, event)
        bad = copy.deepcopy(event)
        bad["attempt"] = 1
        with self.assertRaises(LoopContractError):
            validate_iteration_event(bad)

    def test_requirement_and_review_contracts_fail_closed(self):
        requirement = {
            "schema": "loop-requirement-assessment/v1",
            "requirement_id": "R1",
            "status": "pass",
            "scope": ["src/a.py"],
            "evidence_refs": [{"id": "evidence-1", "digest": DIGEST}],
        }
        validate_requirement_assessment(requirement)
        with self.assertRaises(LoopContractError):
            validate_requirement_assessment(dict(requirement, evidence_refs=[]))
        review = {
            "schema": "loop-review-assessment/v1",
            "review_id": "review-1",
            "axis": "architecture-safety",
            "actor_id": "reviewer-a",
            "context_epoch": "epoch-a",
            "candidate_digest": DIGEST,
            "package_digest": DIGEST,
            "coverage": ["R1"],
            "completed": True,
            "unevaluated": [],
            "finding_refs": [],
        }
        validate_review_assessment(review)
        with self.assertRaises(LoopContractError):
            validate_review_assessment(dict(review, unevaluated=["R1"]))

    def test_evidence_digest_binds_all_validity_inputs(self):
        evidence = {
            "schema": "loop-evidence-record/v1",
            "evidence_id": "evidence-1",
            "evidence_digest": "",
            "candidate_digest": DIGEST,
            "spec_digest": DIGEST,
            "source_digest": DIGEST,
            "dependency_digest": DIGEST,
            "environment_digest": DIGEST,
            "check_definition_digest": DIGEST,
            "coverage": ["R1"],
            "status": "pass",
        }
        evidence["evidence_digest"] = canonical_digest({key: value for key, value in evidence.items() if key != "evidence_digest"})
        validate_evidence_record(evidence)
        changed = copy.deepcopy(evidence)
        changed["environment_digest"] = "sha256:" + "c" * 64
        with self.assertRaises(LoopContractError):
            validate_evidence_record(changed)

    def test_terminal_and_resume_records_preserve_stop_context(self):
        terminal = {
            "schema": "loop-terminal-record/v1",
            "terminal_id": "terminal-1",
            "identity": self.identity(),
            "outcome": "needs-input",
            "reason": "a required choice is missing",
            "candidate_ref": None,
            "requirements": [],
            "reviews": [],
            "evidence": [],
            "open_items": ["choose the supported option"],
            "resume_ref": {"id": "question-1", "digest": DIGEST},
            "non_authorizing": True,
        }
        self.assertEqual(validate_terminal_record(terminal), terminal)
        resume = {
            "schema": "loop-resume-record/v1",
            "resume_id": "resume-1",
            "identity": self.identity(),
            "terminal_ref": {"id": "terminal-1", "digest": DIGEST},
            "reason": "the required choice is now available",
            "evidence_ref": {"id": "answer-1", "digest": DIGEST},
            "non_authorizing": True,
        }
        self.assertEqual(validate_resume_record(resume), resume)
        with self.assertRaises(LoopContractError):
            validate_terminal_record(dict(terminal, open_items=[]))
        with self.assertRaises(LoopContractError):
            validate_resume_record(dict(resume, non_authorizing=False))

        registry = {
            path.name: json.loads(path.read_text())
            for path in (ROOT.parent / "schemas").glob("*.schema.json")
        }
        validate_document(terminal, registry["loop-terminal-record-v1.schema.json"], registry)
        forged_completed = dict(
            terminal,
            outcome="completed",
            open_items=[],
            resume_ref=None,
        )
        with self.assertRaises(SchemaValidationError):
            validate_document(
                forged_completed,
                registry["loop-terminal-record-v1.schema.json"],
                registry,
            )


if __name__ == "__main__":
    unittest.main()
