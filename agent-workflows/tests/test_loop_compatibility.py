import copy
import unittest

from ai_agent_workflow.loop_compatibility import (
    LoopCompatibilityError,
    plan_legacy_transition,
)
from ai_agent_workflow.loop_contracts import canonical_digest
from ai_agent_workflow.loop_state import reduce_history

DIGEST = "sha256:" + "a" * 64


def identity():
    return {
        "schema": "loop-work-identity/v1", "work_lineage_id": "lineage-1",
        "logical_task_id": "task-1", "phase": "E4", "scope_revision": "r1",
        "requirements_digest": DIGEST, "predecessor_ref": None,
    }


def state(rounds=0, attempts=None, terminal=None):
    return {
        "schema": "dag-state/v1", "run_id": "run-1",
        "review_budget": {"version": "v1", "deadline": "2026-09-12T00:00:00+00:00", "max_rounds": 2, "max_attempts_per_finding": 5, "rounds_used": rounds, "finding_attempts": attempts or {}},
        "budget_terminal": terminal, "metadata": {}, "authority": {"scope": "unchanged"},
    }


def source_ref(value):
    return {"id": "legacy-state", "digest": canonical_digest(value)}


def event(identifier, kind, attempt, predecessor=None):
    return {
        "schema": "loop-iteration-event/v1", "event_id": identifier, "command_id": "command-" + identifier,
        "identity": identity(), "kind": kind, "status": "evaluated", "attempt": attempt,
        "predecessor_ref": predecessor, "result_ref": {"id": "result-" + identifier, "digest": DIGEST},
    }


class LoopCompatibilityTests(unittest.TestCase):
    def test_pristine_legacy_state_can_plan_without_using_its_deadline(self):
        legacy = state()
        original = copy.deepcopy(legacy)
        plan = plan_legacy_transition(legacy, [], identity(), source_ref(legacy))
        self.assertEqual("transition-ready", plan["status"])
        self.assertEqual("preserved-read-only-not-used-for-progress", plan["proposed_loop_state"]["legacy_budget_disposition"])
        self.assertEqual("loop-control-migration-candidate/v1", plan["proposed_loop_state"]["schema"])
        self.assertEqual(original, legacy)

    def test_current_top_level_loop_state_is_detected_without_rewriting(self):
        current = state()
        current.pop("review_budget")
        current.pop("budget_terminal")
        reduced = reduce_history([], identity=identity())
        current["loop_control"] = {
            "schema": "loop-control-state/v1", "identity": identity(), "event_refs": [],
            "archives": [], "counter_identity": reduced["counter_identity"],
            "counters": reduced["counters"], "status": reduced["status"],
            "outcome": reduced["outcome"], "terminal_outcome": reduced["terminal_outcome"],
            "recovery_required": reduced["recovery_required"],
            "dispatch_allowed": reduced["dispatch_allowed"], "recovery": None,
            "control_refs": [], "terminal_ref": None, "terminal_record": None,
        }
        plan = plan_legacy_transition(current, [], identity(), source_ref(current))

        self.assertEqual("already-current", plan["status"])
        self.assertEqual(current["loop_control"], plan["proposed_loop_state"])

        malformed = copy.deepcopy(current)
        malformed["loop_control"] = {"schema": "loop-control-state/v1", "identity": identity()}
        refused = plan_legacy_transition(malformed, [], identity(), source_ref(malformed))
        self.assertEqual("migration-refused", refused["status"])
        self.assertEqual("malformed-current-loop-control", refused["reason"])

    def test_active_legacy_counter_without_bound_history_is_refused_not_reset(self):
        legacy = state(rounds=1, attempts={"F1": 1})
        plan = plan_legacy_transition(legacy, [], identity(), source_ref(legacy))
        self.assertEqual("migration-refused", plan["status"])
        self.assertEqual("insufficient-iteration-history", plan["reason"])
        malformed = state(rounds=1, attempts={"F1": "bad"})
        refused = plan_legacy_transition(malformed, [], identity(), source_ref(malformed))
        self.assertEqual("malformed-legacy-review-history", refused["reason"])

    def test_exact_history_maps_counts_and_survives_scope_revision(self):
        legacy = state(rounds=1, attempts={"F1": 1})
        first = event("e0", "initial", 0)
        second = event("e1", "improvement", 1, {"id": "e0", "digest": canonical_digest(first)})
        plan = plan_legacy_transition(legacy, [first, second], identity(), source_ref(legacy))
        self.assertEqual(1, plan["proposed_loop_state"]["additional_iterations_used"])
        changed = identity(); changed["scope_revision"] = "r2"; changed["requirements_digest"] = "sha256:" + "b" * 64
        plan = plan_legacy_transition(legacy, [first, second], changed, source_ref(legacy))
        self.assertEqual(1, plan["proposed_loop_state"]["additional_iterations_used"])

    def test_terminal_or_old_manual_state_requires_explicit_handling(self):
        legacy = state(terminal={"reason": "budget"})
        self.assertEqual("legacy-terminal-requires-human-decision", plan_legacy_transition(legacy, [], identity(), source_ref(legacy))["reason"])
        old = {"schema": "ai-agent-run-state/v1", "state_revision": 1}
        self.assertEqual("unsupported-legacy-state", plan_legacy_transition(old, [], identity(), source_ref(old))["reason"])

    def test_source_ref_and_history_identity_fail_closed(self):
        legacy = state()
        with self.assertRaises(LoopCompatibilityError):
            plan_legacy_transition(legacy, [], identity(), {"id": "legacy", "digest": DIGEST})
        wrong = event("e0", "initial", 0)
        wrong["identity"]["logical_task_id"] = "other"
        with self.assertRaises(LoopCompatibilityError):
            plan_legacy_transition(legacy, [wrong], identity(), source_ref(legacy))

        broken = event("e1", "improvement", 1, {"id": "missing", "digest": DIGEST})
        with self.assertRaises(LoopCompatibilityError):
            plan_legacy_transition(legacy, [broken], identity(), source_ref(legacy))


if __name__ == "__main__":
    unittest.main()
