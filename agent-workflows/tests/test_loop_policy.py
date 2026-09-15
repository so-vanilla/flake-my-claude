import copy
import json
import unittest
from pathlib import Path

from ai_agent_workflow.loop_policy import (
    count_additional_iterations,
    count_counters,
    count_technical_retries,
    decide_loop_outcome,
    decide_next,
    detect_progress,
    evaluate_loop,
    is_stalled,
    iteration_allowed,
    phase_limits,
)

ROOT = Path(__file__).resolve().parent
FIXTURE = json.loads((ROOT / "fixtures" / "loop-control-v1" / "contract-cases.json").read_text())
DIGEST = "sha256:" + "b" * 64


class LoopPolicyTests(unittest.TestCase):
    def identity(self, *, task="task-001", scope="scope-r1", requirements=DIGEST):
        value = copy.deepcopy(FIXTURE["identity"])
        value["logical_task_id"] = task
        value["scope_revision"] = scope
        value["requirements_digest"] = requirements
        return value

    def event(self, number, kind, *, identity=None, status="reserved", attempt=None):
        identity = copy.deepcopy(identity or self.identity())
        if attempt is None:
            attempt = 0 if kind == "initial" else number
        return {
            "schema": "loop-iteration-event/v1",
            "event_id": f"event-{number:03d}",
            "command_id": f"command-{number:03d}",
            "identity": identity,
            "kind": kind,
            "status": status,
            "attempt": attempt,
            "predecessor_ref": None if kind == "initial" else {
                "id": f"event-{max(0, number - 1):03d}",
                "digest": DIGEST,
            },
            "result_ref": None,
        }

    def complete_reviews(self):
        return [
            {"axis": "architecture-safety", "completed": True, "unevaluated": []},
            {"axis": "integration-operability", "completed": True, "unevaluated": []},
        ]

    def complete_requirements(self):
        return [{"requirement_id": "R1", "status": "pass"}]

    def complete_evidence(self):
        return [{"evidence_id": "evidence-1", "status": "pass"}]

    def test_phase_limits_follow_fixed_fixture(self):
        for case in FIXTURE["policy_examples"]:
            policy = phase_limits(case["phase"])
            self.assertEqual(case["policy_id"], policy["policy_id"])
            self.assertEqual(case["limit"], policy["additional_iteration_limit"])

    def test_initial_attempt_is_not_counted(self):
        events = [
            self.event(1, "initial"),
            self.event(2, "improvement"),
            self.event(3, "technical-retry"),
        ]
        self.assertEqual(1, count_additional_iterations(events))
        self.assertEqual(1, count_technical_retries(events))

    def test_technical_retry_and_content_iteration_limits_are_independent(self):
        events = [self.event(1, "technical-retry")]
        self.assertFalse(iteration_allowed("E4", events, kind="technical-retry"))
        self.assertTrue(iteration_allowed("E4", events, kind="improvement"))

    def test_scope_candidate_and_session_renames_do_not_reset_counter(self):
        first = self.identity(scope="scope-r1")
        second = self.identity(scope="scope-r2", requirements="sha256:" + "c" * 64)
        events = [
            {"event": self.event(1, "initial", identity=first), "candidate_digest": DIGEST, "session_id": "s1"},
            {"event": self.event(2, "improvement", identity=first), "candidate_digest": DIGEST, "session_id": "s1"},
            {"event": self.event(3, "improvement", identity=second), "candidate_digest": "sha256:" + "d" * 64, "session_id": "s2"},
        ]
        self.assertEqual(2, count_additional_iterations(events, first))
        self.assertEqual(2, count_additional_iterations(events, second))

    def test_integration_return_charges_only_supplied_state_entries(self):
        task = self.identity(task="task-001")
        integration = self.identity(task="integration-001")
        one_entry = [self.event(1, "integration-return", identity=task)]
        two_entries = [
            self.event(1, "integration-return", identity=task),
            self.event(2, "integration-return", identity=integration),
        ]
        self.assertEqual(1, count_additional_iterations(one_entry))
        self.assertEqual(2, count_additional_iterations(two_entries))
        self.assertEqual(1, count_counters(two_entries)[("lineage-001", "task-001", "E3-E7")]["additional_iterations"])
        self.assertEqual(1, count_counters(two_entries)[("lineage-001", "integration-001", "E3-E7")]["additional_iterations"])

        # A state projection may carry the same wire event under two explicit
        # counter entries; the wrapper identity makes the separate charge
        # unambiguous and is still pure/read-only.
        wire_event = self.event(9, "integration-return", identity=task)
        projected = {
            "state_entries": [
                {"identity": task, "events": [wire_event]},
                {"identity": integration, "events": [wire_event]},
            ]
        }
        self.assertEqual(2, count_additional_iterations(projected))

    def test_replayed_event_or_command_does_not_double_charge_one_counter(self):
        original = self.event(1, "improvement")
        same_event = copy.deepcopy(original)
        same_command = copy.deepcopy(original)
        same_command["event_id"] = "event-002"
        events = [original, same_event, same_command]
        self.assertEqual(1, count_additional_iterations(events))

    def test_progress_and_stall_are_deterministic(self):
        self.assertTrue(detect_progress({"status": "running"}, {"status": "evaluated"}))
        self.assertFalse(detect_progress({"status": "running"}, {"status": "running"}))
        history = [{"progressed": True}, {"progressed": False}, {"progressed": False}]
        self.assertFalse(is_stalled(history, stall_window=3))
        self.assertTrue(is_stalled(history, stall_window=2))
        self.assertTrue(is_stalled({"stalled": True}))

    def test_decisions_cover_terminal_outcomes_and_continue(self):
        self.assertEqual("continue", decide_loop_outcome("E4", events=[self.event(1, "initial")]))
        self.assertEqual(
            "completed",
            decide_loop_outcome(
                "E4",
                requirements=self.complete_requirements(),
                reviews=self.complete_reviews(),
                evidence=self.complete_evidence(),
            ),
        )
        self.assertNotEqual(
            "completed",
            decide_loop_outcome("E4", completed=True, requirements=[{"status": "fail"}]),
        )
        self.assertEqual("needs-input", decide_loop_outcome("E4", input_required=True))
        self.assertEqual("stalled", decide_loop_outcome("E4", stalled=True))
        self.assertEqual("stalled", decide_loop_outcome("E4", progress=False))
        exhausted = [self.event(number, "improvement") for number in range(1, 4)]
        self.assertEqual("iteration-limit", decide_loop_outcome("E4", events=exhausted))
        self.assertEqual("execution-failed", decide_loop_outcome("E4", execution_status="failed"))
        unknown = self.event(1, "improvement", status="execution-unknown")
        self.assertEqual("recovery-required", decide_loop_outcome("E4", events=[unknown]))
        retry = self.event(1, "technical-retry", status="execution-unknown")
        self.assertEqual("recovery-required", decide_loop_outcome("E4", events=[retry]))

    def test_evaluate_loop_returns_counter_evidence_without_mutating_input(self):
        events = [self.event(1, "initial"), self.event(2, "improvement")]
        original = copy.deepcopy(events)
        result = evaluate_loop("E4", events=events)
        self.assertEqual("continue", result["outcome"])
        self.assertEqual(1, result["additional_iterations"])
        self.assertEqual(2, result["additional_iterations_remaining"])
        self.assertEqual(original, events)

    def test_decide_next_accepts_state_assessment_and_profile_without_writing(self):
        state = {"events": [self.event(1, "initial")], "phase": "E4"}
        original = copy.deepcopy(state)
        decision = decide_next(state, {"progress": True}, {"phase": "E4"})
        self.assertEqual("continue", decision["outcome"])
        self.assertEqual(original, state)


if __name__ == "__main__":
    unittest.main()
