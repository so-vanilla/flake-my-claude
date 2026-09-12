import copy
import json
import sys
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "src"))

from ai_agent_workflow.loop_state import (
    CommandConflictError,
    EventConflictError,
    ExecutionUnknownError,
    HistoryValidationError,
    LoopStateController,
    RecoveryRequiredError,
    ResultRejectedError,
    RevisionConflictError,
    accept_result,
    build_iteration_event,
    derive_counters,
    event_ref,
    mark_execution_unknown,
    recover_execution,
    reduce_history,
    reserve_iteration,
    validate_history,
)

FIXTURE = json.loads(
    (ROOT / "tests" / "fixtures" / "loop-control-v1" / "contract-cases.json").read_text()
)
DIGEST = "sha256:" + "b" * 64


class FakeLoopAdapter:
    """A deliberately tiny CAS boundary; it is not part of the production module."""

    def __init__(self):
        self.revision = 0
        self.history = []
        self.calls = []

    def snapshot(self):
        return {"revision": self.revision, "history": copy.deepcopy(self.history)}

    def compare_and_swap(self, *, expected_revision, history):
        self.calls.append({"expected_revision": expected_revision, "history": copy.deepcopy(history)})
        if expected_revision != self.revision:
            raise RevisionConflictError("stale fake revision")
        self.history = copy.deepcopy(list(history))
        self.revision += 1
        return {"accepted": True, "revision": self.revision, "history": copy.deepcopy(self.history)}


class LoopStateTests(unittest.TestCase):
    def identity(self, **changes):
        value = copy.deepcopy(FIXTURE["identity"])
        value.update(changes)
        return value

    def initial(self, *, command_id="command-001", event_id="event-001", identity=None):
        return build_iteration_event(
            identity or self.identity(),
            command_id,
            event_id=event_id,
            kind="initial",
            attempt=0,
        )

    def improvement(self, previous, *, command_id="command-002", event_id="event-002", identity=None):
        value = identity or previous["identity"]
        return build_iteration_event(
            value,
            command_id,
            event_id=event_id,
            kind="improvement",
            attempt=previous["attempt"] + 1,
            predecessor_ref=event_ref(previous),
        )

    def test_reduction_is_pure_and_counter_scope_survives_revision_changes(self):
        event = self.initial()
        original = copy.deepcopy(event)
        state = reduce_history([event])

        self.assertEqual(event, original)
        self.assertEqual(state["status"], "reserved")
        self.assertEqual(state["counters"]["additional_iterations"], 0)
        self.assertEqual(
            state["counter_key"],
            {
                "work_lineage_id": "lineage-001",
                "logical_task_id": "task-001",
                "policy_id": "E3-E7",
            },
        )
        changed = copy.deepcopy(event)
        changed["identity"]["scope_revision"] = "scope-r2"
        changed["identity"]["requirements_digest"] = DIGEST
        self.assertEqual(derive_counters([event])["counter_key"], derive_counters([changed])["counter_key"])

    def test_empty_identity_bound_reduction_preserves_counter_contract(self):
        state = reduce_history([], identity=self.identity())

        self.assertEqual(
            state["counter_key"],
            {
                "work_lineage_id": "lineage-001",
                "logical_task_id": "task-001",
                "policy_id": "E3-E7",
            },
        )
        self.assertEqual(state["policy"]["additional_iteration_limit"], 3)

    def test_duplicate_command_and_event_are_idempotent_but_payload_reuse_is_rejected(self):
        event = self.initial()
        duplicate = reserve_iteration([event], copy.deepcopy(event))
        self.assertTrue(duplicate["idempotent"])
        self.assertEqual(len(duplicate["history"]), 1)

        changed = copy.deepcopy(event)
        changed["status"] = "running"
        with self.assertRaises((CommandConflictError, EventConflictError)):
            validate_history([event, changed])

        different_event = copy.deepcopy(event)
        different_event["event_id"] = "event-other"
        different_event["kind"] = "improvement"
        different_event["attempt"] = 1
        different_event["predecessor_ref"] = event_ref(event)
        different_event["command_id"] = event["command_id"]
        with self.assertRaises(CommandConflictError):
            validate_history([event, different_event])

    def test_broken_predecessor_and_counter_key_drift_fail_closed(self):
        first = self.initial()
        broken = self.improvement(first)
        broken["predecessor_ref"] = {"id": "missing", "digest": DIGEST}
        with self.assertRaises(HistoryValidationError):
            validate_history([first, broken])

        drifted = self.improvement(
            first,
            identity=self.identity(phase="D5"),
        )
        with self.assertRaises(HistoryValidationError):
            validate_history([first, drifted])

    def test_reservation_precedes_result_and_result_is_idempotent(self):
        result_ref = {"id": "result-001", "digest": DIGEST}
        with self.assertRaises(ResultRejectedError):
            accept_result([], "event-001", result_ref)

        reserved = reserve_iteration([], self.initial())
        accepted = accept_result(reserved["history"], "event-001", result_ref)
        self.assertTrue(accepted["accepted"])
        self.assertEqual(accepted["status"], "evaluated")
        replay = accept_result(accepted["history"], "event-001", result_ref)
        self.assertTrue(replay["idempotent"])
        self.assertEqual(replay["history"], accepted["history"])

    def test_unknown_execution_blocks_dispatch_until_explicit_recovery(self):
        reserved = reserve_iteration([], self.initial())
        unknown = mark_execution_unknown(reserved["history"], "event-001")
        self.assertTrue(unknown["recovery_required"])
        self.assertEqual(unknown["outcome"], FIXTURE["terminal_outcomes"][-1])

        with self.assertRaises(RecoveryRequiredError):
            recover_execution(unknown["history"], "event-001", resolution="retry")
        with self.assertRaises(RecoveryRequiredError):
            reserve_iteration(unknown["history"], self.improvement(unknown["history"][0]))

        recovered = recover_execution(
            unknown["history"],
            "event-001",
            resolution="retry",
            evidence_ref={"id": "recovery-proof", "digest": DIGEST},
            retry_command_id="command-retry",
            retry_event_id="event-retry",
        )
        self.assertTrue(recovered["recovered"])
        self.assertFalse(recovered["recovery_required"])
        self.assertEqual(recovered["history"][-1]["kind"], "technical-retry")

    def test_controller_commits_reservation_before_dispatch_and_marks_unknown(self):
        adapter = FakeLoopAdapter()
        controller = LoopStateController(adapter)
        observations = []

        def dispatch(event):
            observations.append((event["status"], adapter.history[-1]["status"], adapter.revision))
            return {"accepted": True}

        state = controller.reserve_before_dispatch(dispatch, self.initial())
        self.assertEqual(observations, [("reserved", "reserved", 1)])
        self.assertTrue(state["dispatch_invoked"])
        self.assertEqual(adapter.calls[0]["expected_revision"], 0)

        adapter = FakeLoopAdapter()
        controller = LoopStateController(adapter)

        def crashing_dispatch(_event):
            raise RuntimeError("worker disconnected")

        with self.assertRaises(ExecutionUnknownError):
            controller.reserve_before_dispatch(crashing_dispatch, self.initial())
        self.assertEqual(adapter.history[-1]["status"], "execution-unknown")
        self.assertEqual(adapter.revision, 2)
        self.assertEqual([call["expected_revision"] for call in adapter.calls], [0, 1])

    def test_controller_surfaces_compare_and_swap_conflict_without_local_state(self):
        adapter = FakeLoopAdapter()
        controller = LoopStateController(adapter)
        adapter.revision = 4
        with self.assertRaises(RevisionConflictError):
            # The candidate is calculated from revision 4, then the fake is
            # changed before publication by a competing writer.
            original = adapter.compare_and_swap

            def stale(**kwargs):
                adapter.revision = 5
                return original(**kwargs)

            adapter.compare_and_swap = stale
            controller.reserve(self.initial())
        self.assertEqual(adapter.history, [])


if __name__ == "__main__":
    unittest.main()
