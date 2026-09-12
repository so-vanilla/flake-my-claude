"""Project-local adoption, semantic steps, refusal and cold Group boundaries."""
import copy
import unittest
from pathlib import Path
from unittest.mock import Mock, call, patch

import test_runtime_approval as approval_helpers
from ai_agent_workflow.control_kernel import StaleHeadError
from ai_agent_workflow.inception_cli import InceptionError
from ai_agent_workflow.inception_runtime import (
    _REPAIR_ACTIONS,
    SOURCE,
    InceptionRuntime,
    execute_group_e,
    execute_repair_e,
    file_ref,
)
from ai_agent_workflow.runtime_approval import adopt_approved_objective


class InceptionRuntimeTests(unittest.TestCase):
    setUp = approval_helpers.RuntimeApprovalAdapterTests.setUp
    write = approval_helpers.RuntimeApprovalAdapterTests.write
    arguments = approval_helpers.RuntimeApprovalAdapterTests.arguments
    steps = approval_helpers.RuntimeApprovalAdapterTests.steps

    def prepare(self):
        self.args = self.arguments("rehearsal")
        self.args["preapproval_steps"] = self.steps(self.args)
        adopt_approved_objective(self.project, "sample", **self.args)
        self.runtime = InceptionRuntime(self.project, "sample")
        self.close_group("B")
        self.runtime.advance()
        return self.runtime

    def close_group(self, group):
        records = self.runtime.records(group)
        refs = [records[group + str(n)]["ref"] for n in range(1, len(records) + 1)]
        audit = self.write("audit-" + group + ".json", {"group_id": group, "objective_digest": self.runtime.state["objective_ref"]["digest"], "alignment": "aligned", "artifact_refs": refs, "reviewer": "sample-reviewer", "rationale": "Every recorded compiler accepted its bound semantic input."})
        return self.runtime.close(audit)

    def c1(self):
        actor = self.args["actor_ref"]
        owner = {"kind": "system", "stable_id": "sample-owner", "role": "objective-owner", "path": actor["path"], "digest": actor["digest"]}
        ref = self.args["candidate_ref"]
        return {"outcome_map": {"schema": "outcome-map/v1", "outcomes": [{"outcome_id": "observable", "achieved_state": "The outcome is observable", "why_required": "Objective evidence", "objective_contribution": ["main"], "exclusion_conditions": ["No unrelated output"], "owner_ref": owner, "acceptance_predicate_refs": [ref]}], "coverage_refs": [ref]}, "required_contributions": ["main"]}

    def step(self, short, values):
        return self.runtime.step(
            f"group.{short[0]}.{short}", values, actor_ref=self.args["actor_ref"]
        )

    def complete_c(self, stop_before_validation=False):
        self.step("C1", self.c1())
        graph = {"schema": "outcome-dependency-graph/v1", "node_ids": ["observable"], "edges": [], "joins": [], "runtime_checks": ["unknown_endpoint", "orphan", "cycle", "ownerless_join"]}
        self.step("C2", {"dependency_graph": graph})
        self.step("C3", {"measurement_plan": {"schema": "measurement-plan/v1", "outcome_id": "observable", "strategy": "direct_metric", "rationale": "Read the observable source"}, "gaming_guard_ref": self.args["candidate_ref"]})
        target = {"schema": "target-set/v1", "outcome_id": "observable", "target_ref": self.args["candidate_ref"]}
        self.step("C4", {"target_set": target, "target": {name: "documented" for name in ("unit", "formula", "source", "frequency", "window", "guard")}})
        self.step("C5", {"target_set": target, "observation": {"schema": "measurement-observation/v1", "observation_id": "baseline", "status": "unavailable", "value": None, "reason": {"code": "stale"}}})
        records = self.runtime.records("C")
        trace = {"objective": self.args["candidate_ref"], **{key: records[step]["value"]["compiled"]["output"]["payload"] for key, step in (("outcome_map", "C1"), ("dependency_graph", "C2"), ("measurement_plan", "C3"), ("target_set", "C4"), ("baseline", "C5"))}}
        if not stop_before_validation:
            self.step("C6", {"trace": trace})
        return trace

    def test_actual_b_closure_and_cold_c_frontier(self):
        self.prepare()
        cold = InceptionRuntime(self.project, "sample")
        self.assertEqual(cold.status()["next_id"], "group.C.C1")
        self.assertEqual(cold.status()["progress_control"], "iteration-and-evidence")
        self.assertEqual(cold.state["loop_control"]["identity"]["phase"], "C")
        self.assertEqual(len(cold.state["loop_control"]["archives"]), 1)
        self.assertEqual(cold.status()["objective_approval_source"], "mock")
        self.assertEqual(len(cold.state["metadata"]["operational_group_history"]), 1)

    def test_advance_replay_repairs_interrupted_loop_phase_rotation(self):
        self.args = self.arguments("rehearsal")
        self.args["preapproval_steps"] = self.steps(self.args)
        adopt_approved_objective(self.project, "sample", **self.args)
        self.runtime = InceptionRuntime(self.project, "sample")
        self.close_group("B")
        authority = {
            "approved": True,
            "scopes": ["open_operational_group"],
            "runtime_identity": self.runtime.identity,
            "run_id": self.runtime.state["run_id"],
            "write_scopes": [self.runtime.identity["namespace"]],
            "protected_fields": ["group", "epoch", "ready"],
            "human_receipt": self.runtime.approval["receipt"],
        }
        self.runtime.kernel.open_operational_group(
            "C",
            "C-01",
            self.runtime.state["group"]["bundle_ref"],
            authority_ref=authority,
        )
        cold = InceptionRuntime(self.project, "sample")

        status = cold.advance()

        self.assertEqual(status["group"]["id"], "C")
        self.assertEqual(status["loop_control"]["identity"]["phase"], "C")

    def test_c_compiler_refusal_does_not_advance(self):
        runtime = self.prepare()
        before = runtime.kernel.head()
        values = self.c1()
        values["outcome_map"]["outcomes"][0]["achieved_state"] = "Implement a file"
        with self.assertRaises(InceptionError):
            self.step("C1", values)
        self.assertEqual(before, runtime.kernel.head())
        self.assertEqual(runtime.status()["next_id"], "group.C.C1")

    def test_wrong_frontier_actor_source_and_stale_file_refuse(self):
        runtime = self.prepare()
        before = runtime.kernel.head()
        with self.assertRaises(InceptionError):
            self.step("C2", {})
        alternate = self.write("other-actor.json", {"actor_id": "sample-owner", "source": "human"})
        with self.assertRaises(InceptionError):
            runtime.step("group.C.C1", self.c1(), actor_ref=alternate)
        Path(self.args["candidate_ref"]["path"]).write_text("Changed objective")
        with self.assertRaises(InceptionError):
            self.step("C1", self.c1())
        self.assertEqual(before, runtime.kernel.head())

    def test_c2_cannot_substitute_unrelated_outcomes(self):
        runtime = self.prepare()
        self.step("C1", self.c1())
        before = runtime.kernel.head()
        with self.assertRaises(InceptionError):
            self.step("C2", {"dependency_graph": {"node_ids": ["unrelated"]}})
        self.assertEqual(before, runtime.kernel.head())

    def test_complete_c_closure_and_d_compiler_refusal(self):
        self.prepare()
        self.complete_c()
        self.close_group("C")
        cold = InceptionRuntime(self.project, "sample")
        self.assertEqual(cold.status()["group"]["status"], "closed")
        cold.advance()
        before = cold.kernel.head()
        with self.assertRaises(InceptionError):
            cold.step("group.D.D1", {"domain": "unresolved"}, actor_ref=self.args["actor_ref"])
        self.assertEqual(before, cold.kernel.head())
        self.assertEqual(cold.status()["next_id"], "group.D.D1")

    def test_incomplete_group_close_and_open_group_advance_refuse(self):
        runtime = self.prepare()
        before = runtime.kernel.head()
        with self.assertRaises(InceptionError):
            runtime.close(self.args["candidate_ref"])
        with self.assertRaises(InceptionError):
            runtime.advance()
        self.assertEqual(before, runtime.kernel.head())

    def test_d1_real_compiler_and_missing_predecessor_refusal(self):
        self.prepare()
        self.complete_c()
        self.close_group("C")
        self.runtime.advance()
        manifest = file_ref(SOURCE / "agent-workflows/workflows/feature-bounded.json")
        self.step("D1", {"domain": "software", "workflow_manifest_ref": manifest, "input_refs": [self.args["candidate_ref"]]})
        self.assertEqual(self.runtime.state["loop_control"]["identity"]["phase"], "D1")
        before = self.runtime.kernel.head()
        with self.assertRaises(InceptionError):
            self.step("D2", {"input_refs": [self.args["candidate_ref"]]})
        self.assertEqual(before, self.runtime.kernel.head())
        self.assertEqual(InceptionRuntime(self.project, "sample").status()["next_id"], "group.D.D2")

    def test_c6_substituted_trace_refuses_without_advancing(self):
        self.prepare()
        trace = self.complete_c(stop_before_validation=True)
        trace["baseline"]["reason"] = "incomparable_condition"
        before = self.runtime.kernel.head()
        with self.assertRaises(InceptionError):
            self.step("C6", {"trace": trace})
        self.assertEqual(before, self.runtime.kernel.head())
        self.assertEqual(self.runtime.status()["next_id"], "group.C.C6")

    def test_wrong_full_group_audit_cannot_publish_or_close(self):
        self.prepare()
        self.complete_c()
        audit = self.write("wrong-audit.json", {"group_id": "C", "objective_digest": self.args["candidate_ref"]["digest"], "alignment": "aligned", "artifact_refs": [], "reviewer": "reviewer", "rationale": "Incomplete artifact coverage"})
        before = self.runtime.kernel.head()
        with self.assertRaises(InceptionError):
            self.runtime.close(audit)
        self.assertEqual(before, self.runtime.kernel.head())

    def test_interleaved_real_transaction_rejects_stale_publication(self):
        runtime = self.prepare()
        runtime.status()
        runtime.kernel.publish_artifact("interleaved-evidence", "v1", {"result": "physical evidence"}, authority_ref={"approved": True, "scopes": ["publish_artifact"]})
        before = runtime.kernel.head()
        with self.assertRaises(StaleHeadError):
            runtime._publish("stale-candidate", {"result": "old compilation"}, "runtime-skill")
        self.assertEqual(before, runtime.kernel.head())

    def test_repair_e_dispatches_all_ten_closed_actions(self):
        cas = {
            "attempt_id": "e7-" + "a" * 32,
            "nonce": "b" * 64,
            "previous_state_ref": {"id": "state", "digest": "sha256:" + "c" * 64},
            "expected_head": {"revision": 7, "transaction_digest": "sha256:" + "d" * 64},
        }
        method_names = {
            "begin": "begin",
            "worker": "accept_worker",
            "reviews-issue": "issue_reviews",
            "reviews-accept": "accept_review",
            "validator-issue": "issue_validator",
            "validator-accept": "accept_validator",
            "finalize": "finalize",
        }
        self.assertEqual(
            tuple(_REPAIR_ACTIONS),
            ("status", "begin", "worker", "focused", "reviews-issue", "reviews-accept",
             "validator-issue", "validator-accept", "whole", "finalize"),
        )
        with patch("ai_agent_workflow.runtime_repair.RuntimeRepairCoordinator") as coordinator_type, \
                patch("ai_agent_workflow.inception_runtime._repair_broker") as broker_factory:
            coordinator = coordinator_type.return_value
            coordinator.status.return_value = {"status": "not-ready"}
            for name in method_names.values():
                getattr(coordinator, name).return_value = {"status": name}
            coordinator.run_focused.return_value = {"status": "focused-accepted"}
            coordinator.run_whole.return_value = {"status": "whole-accepted"}
            broker_factory.return_value = Mock()

            self.assertEqual(
                execute_repair_e(self.project, "sample", "status", {"task_id": "task-1"}),
                {"status": "not-ready"},
            )
            for action, method_name in method_names.items():
                payload = cas if action == "finalize" else {"action_marker": action}
                result = execute_repair_e(self.project, "sample", action, payload)
                self.assertEqual(result["status"], method_name)
                getattr(coordinator, method_name).assert_called_once_with(payload)
            for action, method_name in (("focused", "run_focused"), ("whole", "run_whole")):
                broker_factory.reset_mock()
                broker_factory.return_value = Mock()
                result = execute_repair_e(self.project, "sample", action, cas)
                self.assertEqual(result["status"], action + "-accepted")
                getattr(coordinator, method_name).assert_called_once_with(cas, broker_factory.return_value)
                broker_factory.return_value.close.assert_called_once_with()

    def test_execute_e_routes_explicit_loop_envelope_without_legacy_broker_inputs(self):
        request = {
            "schema": "workflow-loop/v1",
            "identity": {"logical_task_id": "task-1"},
            "phase": "E3",
        }
        result_ref = {"id": "result-1", "digest": "sha256:" + "a" * 64}
        with patch("ai_agent_workflow.runtime_execution.RuntimeExecution") as runtime_type, \
                patch("ai_agent_workflow.macos_task_process.MacOSTaskProcessBroker") as broker_type:
            runtime_type.return_value.execute_loop.return_value = {"outcome": "continue"}
            result = execute_group_e(
                self.project,
                "sample",
                {"task_id": "task-1", "workflow_loop": request, "result_ref": result_ref},
            )
        self.assertEqual("continue", result["outcome"])
        runtime_type.return_value.execute_loop.assert_called_once_with(
            "task-1",
            request,
            result_ref=result_ref,
            phase="E3",
            integration=False,
            process_timeout=None,
            metric_events=None,
        )
        broker_type.assert_not_called()

    def test_execute_e_routes_canonical_top_level_loop_request(self):
        request = {
            "schema": "workflow-loop/v1",
            "identity": {"logical_task_id": "task-1"},
            "phase": "E3",
            "result_ref": {"id": "result-1", "digest": "sha256:" + "a" * 64},
        }
        with patch("ai_agent_workflow.runtime_execution.RuntimeExecution") as runtime_type, \
                patch("ai_agent_workflow.macos_task_process.MacOSTaskProcessBroker") as broker_type:
            runtime_type.return_value.execute_loop.return_value = {"outcome": "continue"}
            result = execute_group_e(self.project, "sample", request)
        self.assertEqual("continue", result["outcome"])
        runtime_type.return_value.execute_loop.assert_called_once_with(
            "task-1",
            request,
            result_ref=request["result_ref"],
            phase="E3",
            integration=False,
            process_timeout=None,
            metric_events=None,
        )
        broker_type.assert_not_called()

    def test_execute_e_loop_envelope_rejects_legacy_or_secret_sidecars(self):
        base = {
            "task_id": "task-1",
            "workflow_loop": {"schema": "workflow-loop/v1"},
        }
        for extra in (
            {"stage_inputs": {}},
            {"loop_request": {}},
            {"broker": {"capability": "caller-secret"}},
        ):
            with self.subTest(extra=extra), self.assertRaises(InceptionError):
                execute_group_e(self.project, "sample", {**base, **extra})
        with self.assertRaisesRegex(InceptionError, "legacy broker inputs"):
            execute_group_e(
                self.project,
                "sample",
                {
                    "schema": "workflow-loop/v1",
                    "identity": {"logical_task_id": "task-1"},
                    "stage_inputs": {},
                },
            )
        with self.assertRaisesRegex(InceptionError, "cannot mix"):
            execute_group_e(
                self.project,
                "sample",
                {
                    "schema": "workflow-loop/v1",
                    "identity": {"logical_task_id": "task-1"},
                    "workflow_loop": {"schema": "workflow-loop/v1"},
                },
            )

    def test_repair_e_routes_workflow_loop_actions_to_the_explicit_v1_coordinator(self):
        actions = (
            ("loop-status", "status"),
            ("loop-policy", "loop-policy"),
            ("loop-phase-transition", "phase-transition"),
            ("reserve", "reserve"),
            ("loop-reserve", "reserve"),
            ("loop-running", "running"),
            ("loop-accept-result", "accept-result"),
            ("loop-execution-unknown", "execution-unknown"),
            ("loop-recover", "recover"),
            ("loop-repair-batch", "repair-batch"),
            ("loop-review-packages", "review-packages"),
            ("loop-review-results", "review-results"),
            ("loop-evidence-validity", "evidence-validity"),
            ("loop-completion", "completion"),
        )
        with patch("ai_agent_workflow.runtime_repair.WorkflowLoopRepairCoordinator") as loop_type, \
                patch("ai_agent_workflow.runtime_repair.RuntimeRepairCoordinator") as legacy_type:
            coordinator = loop_type.return_value
            coordinator.dispatch.side_effect = lambda name, payload: {
                "action": name, "payload": payload,
            }
            for index, (external, canonical) in enumerate(actions):
                payload = {"schema": "workflow-loop/v1", "marker": f"loop-{index}"}
                result = execute_repair_e(self.project, "sample", external, payload)
                self.assertEqual(canonical, result["action"])
                self.assertNotIn("schema", result["payload"])
                self.assertEqual(payload["marker"], result["payload"]["marker"])
            legacy_type.assert_not_called()
            self.assertEqual(len(actions), coordinator.dispatch.call_count)

    def test_repair_e_detects_loop_control_runs_and_preserves_replay_cas_inputs(self):
        first = {
            "command_id": "loop-command-1",
            "event_id": "loop-event-1",
            "attempt": 0,
            "expected_revision": 7,
            "idempotency_key": "loop-reserve:loop-command-1",
        }
        with patch("ai_agent_workflow.inception_runtime._has_loop_control_run", return_value=True), \
                patch("ai_agent_workflow.runtime_repair.WorkflowLoopRepairCoordinator") as loop_type, \
                patch("ai_agent_workflow.runtime_repair.RuntimeRepairCoordinator") as legacy_type:
            coordinator = loop_type.return_value
            coordinator.dispatch.side_effect = lambda name, payload: {
                "action": name, "payload": payload,
            }
            first_result = execute_repair_e(self.project, "sample", "reserve", first)
            second_result = execute_repair_e(self.project, "sample", "reserve", copy.deepcopy(first))
            self.assertEqual(first_result, second_result)
            coordinator.dispatch.assert_has_calls([call("reserve", first), call("reserve", first)])
            legacy_type.assert_not_called()

    def test_repair_e_rejects_caller_authority_claims_on_workflow_loop_route(self):
        with patch("ai_agent_workflow.runtime_repair.WorkflowLoopRepairCoordinator") as loop_type:
            for payload in (
                {"schema": "workflow-loop/v1", "passed": True},
                {"schema": "workflow-loop/v1", "broker": {"capability": "caller"}},
            ):
                with self.subTest(payload=payload), self.assertRaises(InceptionError):
                    execute_repair_e(self.project, "sample", "loop-completion", payload)
            loop_type.assert_not_called()

    def test_repair_e_rejects_secret_result_and_terminal_claims_before_broker(self):
        cas = {
            "attempt_id": "e7-" + "a" * 32,
            "nonce": "b" * 64,
            "previous_state_ref": {"id": "state", "digest": "sha256:" + "c" * 64},
            "expected_head": {"revision": 7, "transaction_digest": "sha256:" + "d" * 64},
        }
        rejected = (
            {**cas, "broker_secret": "caller-secret"},
            {**cas, "nested": {"hmac": "caller-mac"}},
            {**cas, "passed": True},
            {**cas, "terminal_receipt": {"status": "passed"}},
            {**cas, "status": "closed"},
            {**cas, "broker": {"capability": "caller"}},
        )
        with patch("ai_agent_workflow.runtime_repair.RuntimeRepairCoordinator"), \
                patch("ai_agent_workflow.inception_runtime._repair_broker") as broker_factory:
            for payload in rejected:
                with self.subTest(payload=payload), self.assertRaises(InceptionError):
                    execute_repair_e(self.project, "sample", "focused", payload)
            broker_factory.assert_not_called()
            with self.assertRaisesRegex(InceptionError, "only attempt identity and CAS"):
                execute_repair_e(self.project, "sample", "whole", {key: value for key, value in cas.items() if key != "expected_head"})
        with self.assertRaisesRegex(InceptionError, "unsupported"):
            execute_repair_e(self.project, "sample", "close", cas)

    def test_repair_e_constructs_and_closes_internal_broker_even_on_failure(self):
        cas = {
            "attempt_id": "e7-" + "a" * 32,
            "nonce": "b" * 64,
            "previous_state_ref": {"id": "state", "digest": "sha256:" + "c" * 64},
            "expected_head": {"revision": 7, "transaction_digest": "sha256:" + "d" * 64},
        }
        with patch("ai_agent_workflow.runtime_repair.RuntimeRepairCoordinator") as coordinator_type, \
                patch("ai_agent_workflow.macos_task_process.MacOSTaskProcessBroker") as broker_type:
            coordinator_type.return_value.run_focused.side_effect = InceptionError("probe failed")
            broker = broker_type.return_value
            with self.assertRaisesRegex(InceptionError, "probe failed"):
                execute_repair_e(self.project, "sample", "focused", cas)
            broker.close.assert_called_once_with()
            root = broker_type.call_args.args[0]
            self.assertTrue(str(root).startswith(str(self.project / ".agent-workflow/state/runtime-e-repair")))
            self.assertNotIn(cas["attempt_id"], str(root))
            self.assertEqual(len(broker_type.call_args.kwargs["capability"]), 32)
            self.assertEqual(len(broker_type.call_args.kwargs["boot_id"]), 32)

    def test_repair_status_is_cold_read_only_and_does_not_invent_completion(self):
        runtime = self.prepare()
        before = runtime.kernel.head()
        first = execute_repair_e(self.project, "sample", "status", {})
        cold = InceptionRuntime(self.project, "sample")
        second = execute_repair_e(self.project, "sample", "status", {})
        self.assertEqual(first, second)
        self.assertEqual(first["status"], "not-ready")
        self.assertIsNone(first["next_action"])
        self.assertNotIn(first["status"], {"closed", "passed", "complete"})
        self.assertEqual(before, cold.kernel.head())
        self.assertEqual(before, InceptionRuntime(self.project, "sample").kernel.head())


if __name__ == "__main__":
    unittest.main()
