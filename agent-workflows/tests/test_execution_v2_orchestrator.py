import copy
import hashlib
import json
import sys
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "src"))

from ai_agent_workflow.execution_v2_orchestrator import (
    DAGOrchestrator,
    OrchestratorContractError,
)
from ai_agent_workflow.loop_contracts import canonical_digest
from ai_agent_workflow.loop_state import build_iteration_event, event_ref
from ai_agent_workflow.schema_validation import (
    SchemaValidationError,
    validate_document,
)

DIGESTS = {letter: "sha256:" + letter * 64 for letter in "abcdef"}
LOOP_DIGEST = DIGESTS["b"]
LOOP_IDENTITY = {
    "schema": "loop-work-identity/v1",
    "work_lineage_id": "lineage-001",
    "logical_task_id": "task-001",
    "phase": "E4",
    "scope_revision": "scope-r1",
    "requirements_digest": LOOP_DIGEST,
    "predecessor_ref": None,
}


def loop_history(*kinds, status="reserved"):
    history = []
    for index, kind in enumerate(kinds):
        history.append(
            build_iteration_event(
                LOOP_IDENTITY,
                f"loop-command-{index}",
                kind=kind,
                attempt=index,
                event_id=f"loop-event-{index}",
                status=status if index == len(kinds) - 1 else "reserved",
                predecessor_ref=event_ref(history[-1]) if history else None,
            )
        )
    return history


def loop_command(phase="E4", **values):
    return {"schema": "workflow-loop/v1", "phase": phase, **values}


def repair_finding(identifier, *, batch="root-a", root="cause-a"):
    return {
        "finding_id": identifier,
        "fingerprint": "fingerprint-" + identifier,
        "classification": "required",
        "candidate_digest": LOOP_DIGEST,
        "batch_key": batch,
        "root_cause": root,
        "write_scope": ["src/a.py"],
        "verification": ["test-a"],
        "depends_on": [],
        "conflicts_with": [],
        "resolution_conditions": ["test passes for " + identifier],
    }


def review_candidate():
    return {
        "candidate_ref": {"id": "candidate", "digest": LOOP_DIGEST},
        "spec_ref": {"id": "spec", "digest": LOOP_DIGEST},
        "dependency_refs": [{"id": "dependency", "digest": LOOP_DIGEST}],
        "environment_ref": {"id": "environment", "digest": LOOP_DIGEST},
        "source_paths": ["src/a.py", "src/b.py"],
    }


def review_requirements():
    return [
        {"requirement_id": "R1", "requirement_ref": {"id": "R1", "digest": LOOP_DIGEST}, "scope": ["src/a.py"]},
        {"requirement_id": "R2", "requirement_ref": {"id": "R2", "digest": LOOP_DIGEST}, "scope": ["src/b.py"]},
    ]


def object_digest(value):
    encoded = json.dumps(value, sort_keys=True, separators=(",", ":")).encode()
    return "sha256:" + hashlib.sha256(encoded).hexdigest()


def head(revision=9):
    return {"revision": revision, "transaction_digest": DIGESTS["a"]}


def advisory(classification="required", observed_round=99, observed_attempt=99):
    value = {
        "schema": "finding-disposition/v1",
        "candidate_digest": DIGESTS["b"],
        "receipt_aggregate_digest": DIGESTS["c"],
        "review_refs": [
            {"id": "review-architecture", "digest": DIGESTS["d"]},
            {"id": "review-operability", "digest": DIGESTS["e"]},
        ],
        "source_finding_ids": ["finding-a"],
        "dispositions": [
            {
                "fingerprint": "root-cause-a",
                "source_finding_ids": ["finding-a"],
                "classification": classification,
                "materiality": "material",
                "proposed_scope": ["src/owned.py"],
            }
        ],
        "observed_budget": {
            "remaining_seconds": 1,
            "review_round": observed_round,
            "product_fix_attempt": observed_attempt,
        },
        "advisory_only": True,
    }
    value["disposition_digest"] = object_digest(value)
    return value


def event(sequence, parent_digest, event_type, payload):
    value = {
        "schema": "orchestrator-history-event/v2",
        "sequence": sequence,
        "parent_digest": parent_digest,
        "event_type": event_type,
        "contract_version": "workflow-execution/v2",
        "payload": copy.deepcopy(payload),
    }
    value["event_digest"] = object_digest(value)
    return value


def history_for(advice):
    first = event(
        0,
        None,
        "review-round-opened",
        {"candidate_digest": advice["candidate_digest"], "review_round": 1},
    )
    second = event(
        1,
        first["event_digest"],
        "advisory-accepted",
        {
            "candidate_digest": advice["candidate_digest"],
            "disposition_digest": advice["disposition_digest"],
        },
    )
    return [first, second]


def chained_history(advice, review_rounds=1, product_attempts=0):
    values = []
    parent = None
    for review_round in range(1, review_rounds + 1):
        item = event(
            len(values),
            parent,
            "review-round-opened",
            {"candidate_digest": advice["candidate_digest"], "review_round": review_round},
        )
        values.append(item)
        parent = item["event_digest"]
    for _ in range(product_attempts):
        item = event(
            len(values),
            parent,
            "product-fix-authorized",
            {
                "candidate_digest": advice["candidate_digest"],
                "disposition_digest": advice["disposition_digest"],
                "finding_fingerprint": "root-cause-a",
            },
        )
        values.append(item)
        parent = item["event_digest"]
    item = event(
        len(values),
        parent,
        "advisory-accepted",
        {
            "candidate_digest": advice["candidate_digest"],
            "disposition_digest": advice["disposition_digest"],
        },
    )
    values.append(item)
    return values


def lease(expected_head=None, lease_id="lease-new"):
    return {
        "schema": "orchestrator-lease/v2",
        "lease_id": lease_id,
        "status": "active",
        "holder_assignment_id": "root-orchestrator",
        "contract_version": "workflow-execution/v2",
        "expected_head": copy.deepcopy(expected_head or head()),
        "unaccepted_dispatch_ids": ["dispatch-old-b", "dispatch-old-a"],
    }


def command(advice, remaining_seconds=100):
    return {
        "schema": "orchestrator-command/v2",
        "command_id": "command-001",
        "operation": "evaluate-advice",
        "actor": {"role": "orchestrator", "assignment_id": "root-orchestrator"},
        "expected_head": head(),
        "contract_version": "workflow-execution/v2",
        "lease_id": "lease-new",
        "source_ref": {"id": "advice-001", "digest": advice["disposition_digest"]},
        "remaining_seconds": remaining_seconds,
        "allowances": {
            "command_timeout_seconds": 20,
            "grace_seconds": 5,
            "terminal_publication_seconds": 5,
            "affected_regression_seconds": 20,
            "round_two_reviews_seconds": 10,
            "validator_seconds": 10,
            "parent_seconds": 10,
        },
        "replacement_budget": None,
    }


def reopen_command(terminal):
    return {
        "schema": "orchestrator-command/v2",
        "command_id": "command-reopen-001",
        "operation": "reopen-stopped-budget",
        "actor": {"role": "orchestrator", "assignment_id": "root-orchestrator"},
        "expected_head": head(10),
        "contract_version": "workflow-execution/v2",
        "lease_id": "lease-reopen",
        "source_ref": {"id": terminal["terminal_id"], "digest": terminal["terminal_digest"]},
        "remaining_seconds": 0,
        "allowances": {key: 0 for key in command(advisory())["allowances"]},
        "replacement_budget": {"version": "budget-v2", "value_seconds": 200},
    }


def state(advice):
    return {
        "current_head": head(),
        "active_lease": lease(),
        "advisory_disposition": advice,
        "immutable_history": history_for(advice),
        "prior_terminal": None,
    }


def append_event(values, event_type, payload):
    item = event(
        len(values),
        values[-1]["event_digest"] if values else None,
        event_type,
        payload,
    )
    values.append(item)
    return item


class ExecutionV2OrchestratorTests(unittest.TestCase):
    def test_required_advice_compiles_one_bounded_fix_from_history_counters(self):
        advice = advisory(observed_round=99, observed_attempt=99)
        result = DAGOrchestrator().compile(command(advice), state(advice))

        self.assertEqual(result["schema"], "orchestrator-transition/v2")
        self.assertEqual(result["kind"], "fix-dispatch")
        self.assertTrue(result["non_mutating"])
        self.assertEqual(result["derived_counters"]["review_round"], 1)
        self.assertEqual(
            result["derived_counters"]["product_fix_attempts"],
            [{"fingerprint": "root-cause-a", "count": 0}],
        )
        task = result["dispatch_authority"]["task"]
        self.assertEqual(task["product_fix_attempt"], 1)
        self.assertEqual(task["next_review_round"], 2)
        self.assertEqual(task["budget_seconds"], 80)
        self.assertEqual(task["write_scope"], ["src/owned.py"])
        self.assertNotIn("head_update", result)

    def test_budget_or_counter_exhaustion_compiles_non_dispatch_stopped_budget(self):
        advice = advisory()
        cases = [
            ("budget-insufficient", 79, 1, 0),
            ("review-round-exhausted", 100, 2, 0),
            ("product-fix-attempt-exhausted", 100, 1, 5),
        ]
        for reason, remaining, rounds, attempts in cases:
            observed = state(advice)
            observed["immutable_history"] = chained_history(advice, rounds, attempts)
            with self.subTest(reason=reason):
                result = DAGOrchestrator().compile(
                    command(advice, remaining_seconds=remaining), observed
                )
                self.assertEqual(result["schema"], "stopped-budget/v1")
                self.assertEqual(result["reason"], reason)
                self.assertTrue(result["terminal"])
                self.assertTrue(result["non_dispatch"])
                self.assertIsNone(result["dispatch_authority"])
                self.assertEqual(result["lease_closure"], {"lease_id": "lease-new", "status": "closed"})
                self.assertEqual(
                    result["revoked_dispatch_ids"],
                    ["dispatch-old-a", "dispatch-old-b"],
                )

    def test_role_staleness_and_changed_replay_fail_closed_while_exact_replay_reuses(self):
        advice = advisory()
        request = command(advice)
        observed = state(advice)
        result = DAGOrchestrator().compile(request, observed)

        replay_state = copy.deepcopy(observed)
        append_event(
            replay_state["immutable_history"],
            "transition-compiled",
            {
                "command_id": request["command_id"],
                "command_digest": object_digest(request),
                "disposition_digest": advice["disposition_digest"],
                "result": result,
            },
        )
        self.assertEqual(DAGOrchestrator().compile(request, replay_state), result)

        changed = copy.deepcopy(request)
        changed["remaining_seconds"] += 1
        with self.assertRaises(OrchestratorContractError):
            DAGOrchestrator().compile(changed, replay_state)

        for role in ("worker", "reviewer", "validator"):
            wrong_role = copy.deepcopy(request)
            wrong_role["actor"]["role"] = role
            with self.subTest(role=role), self.assertRaises(OrchestratorContractError):
                DAGOrchestrator().compile(wrong_role, observed)

        stale = copy.deepcopy(request)
        stale["expected_head"]["revision"] += 1
        with self.assertRaises(OrchestratorContractError):
            DAGOrchestrator().compile(stale, observed)

        missing_acceptance = copy.deepcopy(observed)
        missing_acceptance["immutable_history"] = missing_acceptance["immutable_history"][:-1]
        with self.assertRaises(OrchestratorContractError):
            DAGOrchestrator().compile(request, missing_acceptance)

        tampered_history = copy.deepcopy(observed)
        tampered_history["immutable_history"][0]["payload"]["review_round"] = 2
        with self.assertRaises(OrchestratorContractError):
            DAGOrchestrator().compile(request, tampered_history)

    def test_product_attempt_counter_is_scoped_to_candidate_and_disposition_lineage(self):
        advice = advisory()
        values = []
        append_event(
            values,
            "review-round-opened",
            {"candidate_digest": advice["candidate_digest"], "review_round": 1},
        )
        append_event(
            values,
            "product-fix-authorized",
            {
                "candidate_digest": DIGESTS["f"],
                "disposition_digest": DIGESTS["f"],
                "finding_fingerprint": "root-cause-a",
            },
        )
        for retry_class in (
            "test-fixture-correction",
            "command-or-capture-retry",
            "package-or-report-correction",
        ):
            append_event(
                values,
                retry_class,
                {
                    "candidate_digest": advice["candidate_digest"],
                    "disposition_digest": advice["disposition_digest"],
                },
            )
        append_event(
            values,
            "advisory-accepted",
            {
                "candidate_digest": advice["candidate_digest"],
                "disposition_digest": advice["disposition_digest"],
            },
        )
        observed = state(advice)
        observed["immutable_history"] = values
        result = DAGOrchestrator().compile(command(advice), observed)
        self.assertEqual(
            result["derived_counters"]["product_fix_attempts"],
            [{"fingerprint": "root-cause-a", "count": 0}],
        )
        self.assertEqual(
            result["derived_counters"]["non_product_retry_classes"],
            {
                "test_fixture_correction": 1,
                "command_or_capture_retry": 1,
                "package_or_report_correction": 1,
            },
        )

    def test_unknown_retry_class_is_rejected_instead_of_silently_dropped(self):
        advice = advisory()
        observed = state(advice)
        append_event(
            observed["immutable_history"],
            "network-retry",
            {"candidate_digest": advice["candidate_digest"], "disposition_digest": advice["disposition_digest"]},
        )
        with self.assertRaises(OrchestratorContractError):
            DAGOrchestrator().compile(command(advice), observed)

    def test_reopen_requires_terminal_head_new_lease_and_versioned_replacement_budget(self):
        advice = advisory()
        stopped = DAGOrchestrator().compile(
            command(advice, remaining_seconds=1), state(advice)
        )
        observed = state(advice)
        append_event(
            observed["immutable_history"],
            "stopped-budget",
            {"terminal_digest": stopped["terminal_digest"]},
        )
        observed.update(
            {
                "current_head": head(10),
                "active_lease": lease(head(10), "lease-reopen"),
                "advisory_disposition": None,
                "prior_terminal": stopped,
            }
        )
        request = reopen_command(stopped)
        result = DAGOrchestrator().compile(request, observed)
        self.assertEqual(result["schema"], "orchestrator-transition/v2")
        self.assertEqual(result["kind"], "reopen")
        self.assertIsNone(result["dispatch_authority"])
        self.assertEqual(result["lease_action"]["lease_id"], "lease-reopen")
        self.assertEqual(result["reopen"]["prior_terminal_digest"], stopped["terminal_digest"])
        self.assertEqual(
            result["reopen"]["replacement_budget"],
            {"version": "budget-v2", "value_seconds": 200},
        )

        for mutation in ("terminal", "old-lease", "budget"):
            invalid_state = copy.deepcopy(observed)
            invalid_request = copy.deepcopy(request)
            if mutation == "terminal":
                invalid_request["source_ref"]["digest"] = DIGESTS["f"]
            elif mutation == "old-lease":
                invalid_state["active_lease"] = lease(head(10), stopped["lease_closure"]["lease_id"])
                invalid_request["lease_id"] = stopped["lease_closure"]["lease_id"]
            else:
                invalid_request["replacement_budget"] = None
            with self.subTest(mutation=mutation), self.assertRaises(OrchestratorContractError):
                DAGOrchestrator().compile(invalid_request, invalid_state)

    def test_outputs_and_ordinary_review_satisfy_strict_source_schemas(self):
        advice = advisory()
        transition = DAGOrchestrator().compile(command(advice), state(advice))
        stopped = DAGOrchestrator().compile(
            command(advice, remaining_seconds=1), state(advice)
        )
        reopened_state = state(advice)
        append_event(
            reopened_state["immutable_history"],
            "stopped-budget",
            {"terminal_digest": stopped["terminal_digest"]},
        )
        reopened_state.update(
            {
                "current_head": head(10),
                "active_lease": lease(head(10), "lease-reopen"),
                "advisory_disposition": None,
                "prior_terminal": stopped,
            }
        )
        reopened = DAGOrchestrator().compile(
            reopen_command(stopped), reopened_state
        )
        review = {
            "schema": "ordinary-review-report/v1",
            "report_id": "review-architecture",
            "axis": "architecture-safety",
            "actor_id": "reviewer-a",
            "context_epoch_id": "epoch-a",
            "package_digest": DIGESTS["a"],
            "candidate_digest": DIGESTS["b"],
            "aggregate_digest": DIGESTS["c"],
            "findings": [
                {
                    "finding_id": "finding-a",
                    "fingerprint": "root-cause-a",
                    "severity": "major",
                    "summary": "material contract gap",
                }
            ],
        }
        artifacts = {
            "orchestrator-transition-v2.schema.json": transition,
            "stopped-budget-v1.schema.json": stopped,
            "ordinary-review-report-v1.schema.json": review,
        }
        for filename, artifact in artifacts.items():
            with self.subTest(filename=filename):
                schema = json.loads((ROOT / "schemas" / filename).read_text())
                self.assertFalse(schema["additionalProperties"])
                validate_document(artifact, schema)
                unknown = copy.deepcopy(artifact)
                unknown["transition_authority"] = True
                with self.assertRaises(SchemaValidationError):
                    validate_document(unknown, schema)
        transition_schema = json.loads(
            (ROOT / "schemas/orchestrator-transition-v2.schema.json").read_text()
        )
        validate_document(reopened, transition_schema)
        invalid_review = copy.deepcopy(review)
        invalid_review["findings"][0]["implementation_hint"] = "private detail"
        with self.assertRaises(SchemaValidationError):
            validate_document(
                invalid_review,
                json.loads((ROOT / "schemas/ordinary-review-report-v1.schema.json").read_text()),
            )

    def test_workflow_loop_route_ignores_v2_time_fields(self):
        history = loop_history("initial")
        state = {
            "events": history,
            "remaining_seconds": 0,
            "allowances": {"command_timeout_seconds": 0},
            "replacement_budget": {"version": "ignored", "value_seconds": 1},
        }
        original = copy.deepcopy(state)
        result = DAGOrchestrator().compile(loop_command(), state)
        changed = copy.deepcopy(state)
        changed.update(
            {
                "remaining_seconds": 999999,
                "allowances": {"parent_seconds": 999999},
                "replacement_budget": {"version": "different", "value_seconds": 999999},
            }
        )
        changed_result = DAGOrchestrator().compile(loop_command(), changed)
        self.assertEqual("continue", result["outcome"])
        self.assertEqual(result, changed_result)
        self.assertNotIn("budget", result)
        self.assertEqual(original, state)

    def test_nested_workflow_loop_request_keeps_canonical_completion_projection(self):
        evidence = {
            "schema": "loop-evidence-record/v1",
            "evidence_id": "evidence-nested",
            "evidence_digest": "",
            "candidate_digest": LOOP_DIGEST,
            "spec_digest": LOOP_DIGEST,
            "source_digest": LOOP_DIGEST,
            "dependency_digest": LOOP_DIGEST,
            "environment_digest": LOOP_DIGEST,
            "check_definition_digest": LOOP_DIGEST,
            "coverage": ["R1"],
            "status": "pass",
        }
        evidence["evidence_digest"] = canonical_digest(
            {key: value for key, value in evidence.items() if key != "evidence_digest"}
        )
        request = {
            "schema": "workflow-loop/v1",
            "identity": LOOP_IDENTITY,
            "candidate_digest": LOOP_DIGEST,
            "package_digest": LOOP_DIGEST,
            "requirements": [{
                "schema": "loop-requirement-assessment/v1",
                "requirement_id": "R1",
                "status": "pass",
                "scope": ["src/a.py"],
                "evidence_refs": [{"id": evidence["evidence_id"], "digest": evidence["evidence_digest"]}],
            }],
            "reviews": [
                {
                    "schema": "loop-review-assessment/v1",
                    "review_id": "review-nested-a",
                    "axis": "architecture-safety",
                    "actor_id": "actor-a",
                    "context_epoch": "epoch-a",
                    "candidate_digest": LOOP_DIGEST,
                    "package_digest": LOOP_DIGEST,
                    "coverage": ["R1"],
                    "completed": True,
                    "unevaluated": [],
                    "finding_refs": [],
                },
                {
                    "schema": "loop-review-assessment/v1",
                    "review_id": "review-nested-b",
                    "axis": "integration-operability",
                    "actor_id": "actor-b",
                    "context_epoch": "epoch-b",
                    "candidate_digest": LOOP_DIGEST,
                    "package_digest": LOOP_DIGEST,
                    "coverage": ["R1"],
                    "completed": True,
                    "unevaluated": [],
                    "finding_refs": [],
                },
            ],
            "evidence": [evidence],
            "findings": [],
        }
        result = DAGOrchestrator().compile(
            {"schema": "workflow-loop/v1", "completion_request": request, "remaining_seconds": 0},
            {},
        )
        self.assertEqual("completed", result["outcome"])
        self.assertTrue(result["completion"]["completed"])

    def test_explicit_state_entries_charge_each_supplied_counter_identity(self):
        wire_event = loop_history("initial", "integration-return")[-1]
        integration_identity = copy.deepcopy(LOOP_IDENTITY)
        integration_identity.update({"logical_task_id": "integration-001", "scope_revision": "scope-r2"})
        result = DAGOrchestrator().compile(
            loop_command(),
            {
                "state_entries": [
                    {"identity": LOOP_IDENTITY, "events": [wire_event]},
                    {"identity": integration_identity, "events": [wire_event]},
                ]
            },
        )
        self.assertEqual("continue", result["outcome"])
        self.assertEqual(2, result["counters"]["additional_iterations"])
        self.assertEqual(1, result["counters"]["additional_iterations_remaining"])

    def test_workflow_loop_reads_phase_limits_from_loop_contract(self):
        expected = {
            "D5": 1,
            "D6": 2,
            "D12": 2,
            "D6-D12": 2,
            "E4": 3,
            "E3-E7": 3,
            "E8": 2,
            "E8-E9": 2,
        }
        for phase, limit in expected.items():
            with self.subTest(phase=phase):
                result = DAGOrchestrator().compile(loop_command(phase), {})
                self.assertEqual(limit, result["policy"]["additional_iteration_limit"])
                self.assertEqual(1, result["policy"]["technical_retry_limit"])

    def test_compatible_required_findings_share_one_repair_round_and_delta_review(self):
        assignment = {
            "architecture-safety": {
                "assignment_id": "assignment-architecture",
                "actor_id": "actor-architecture",
                "context_epoch": "epoch-architecture",
            },
            "integration-operability": {
                "assignment_id": "assignment-integration",
                "actor_id": "actor-integration",
                "context_epoch": "epoch-integration",
            },
        }
        state = {
            "repair_findings": [repair_finding("F1"), repair_finding("F2")],
            "candidate": review_candidate(),
            "review_requirements": review_requirements(),
            "prior_findings": [
                {
                    "finding_id": "F1",
                    "finding_ref": {"id": "F1", "digest": LOOP_DIGEST},
                    "status": "required",
                    "scope": ["src/a.py"],
                    "resolution_ref": None,
                }
            ],
            "impact": {
                "known": True,
                "changed_paths": ["src/a.py"],
                "affected_requirements": ["R1"],
                "affected_interfaces": [],
                "affected_tests": ["test-a"],
            },
            "review_assignments": assignment,
        }
        result = DAGOrchestrator().compile(loop_command(), state)
        self.assertEqual("continue", result["outcome"])
        self.assertEqual(["F1", "F2"], result["repair_plan"]["batches"][0]["finding_ids"])
        self.assertEqual(2, len(result["review_packages"]))
        self.assertEqual({"delta"}, {item["mode"] for item in result["review_packages"]})
        self.assertEqual(["src/a.py"], result["review_packages"][0]["read_scope"])

    def test_unknown_impact_scope_is_a_hard_failure(self):
        state = {
            "repair_findings": [repair_finding("F1")],
            "candidate": review_candidate(),
            "review_requirements": review_requirements(),
            "prior_findings": [],
            "impact": {
                "known": True,
                "changed_paths": ["src/escape.py"],
                "affected_requirements": ["R1"],
                "affected_interfaces": [],
                "affected_tests": [],
            },
            "review_assignments": {
                "architecture-safety": {
                    "assignment_id": "assignment-architecture",
                    "actor_id": "actor-architecture",
                    "context_epoch": "epoch-architecture",
                }
            },
        }
        result = DAGOrchestrator().compile(loop_command(), state)
        self.assertEqual("execution-failed", result["outcome"])
        self.assertTrue(result["hard_failure"])
        self.assertIn("changed_paths", result["error"])

    def test_strict_zero_finding_terminal_requires_all_current_gates(self):
        evidence = {
            "schema": "loop-evidence-record/v1",
            "evidence_id": "evidence-1",
            "evidence_digest": "",
            "candidate_digest": LOOP_DIGEST,
            "spec_digest": LOOP_DIGEST,
            "source_digest": LOOP_DIGEST,
            "dependency_digest": LOOP_DIGEST,
            "environment_digest": LOOP_DIGEST,
            "check_definition_digest": LOOP_DIGEST,
            "coverage": ["R1"],
            "status": "pass",
        }
        evidence["evidence_digest"] = canonical_digest(
            {key: value for key, value in evidence.items() if key != "evidence_digest"}
        )
        completion = {
            "identity": LOOP_IDENTITY,
            "candidate_digest": LOOP_DIGEST,
            "package_digest": LOOP_DIGEST,
            "requirements": [{
                "schema": "loop-requirement-assessment/v1",
                "requirement_id": "R1",
                "status": "pass",
                "scope": ["src/a.py"],
                "evidence_refs": [{"id": "evidence-1", "digest": evidence["evidence_digest"]}],
            }],
            "reviews": [
                {
                    "schema": "loop-review-assessment/v1",
                    "review_id": "review-architecture",
                    "axis": "architecture-safety",
                    "actor_id": "actor-a",
                    "context_epoch": "epoch-a",
                    "candidate_digest": LOOP_DIGEST,
                    "package_digest": LOOP_DIGEST,
                    "coverage": ["R1"],
                    "completed": True,
                    "unevaluated": [],
                    "finding_refs": [],
                },
                {
                    "schema": "loop-review-assessment/v1",
                    "review_id": "review-integration",
                    "axis": "integration-operability",
                    "actor_id": "actor-b",
                    "context_epoch": "epoch-b",
                    "candidate_digest": LOOP_DIGEST,
                    "package_digest": LOOP_DIGEST,
                    "coverage": ["R1"],
                    "completed": True,
                    "unevaluated": [],
                    "finding_refs": [],
                },
            ],
            "evidence": [evidence],
            "findings": [],
        }
        complete = DAGOrchestrator().compile(loop_command(completion_request=completion), {})
        incomplete = copy.deepcopy(completion)
        incomplete["reviews"] = incomplete["reviews"][:1]
        pending = DAGOrchestrator().compile(loop_command(completion_request=incomplete), {})
        self.assertEqual("completed", complete["outcome"])
        self.assertEqual("needs-input", pending["outcome"])
        self.assertFalse(pending["completion"]["completed"])

    def test_phase_limit_retry_and_unknown_execution_are_explicit(self):
        limited = DAGOrchestrator().compile(
            loop_command(), {"events": loop_history("initial", "improvement", "improvement", "improvement")}
        )
        d5_limited = DAGOrchestrator().compile(
            loop_command("D5"), {"events": loop_history("initial", "improvement")}
        )
        retry = DAGOrchestrator().compile(
            loop_command(), {"events": loop_history("initial", "technical-retry")}
        )
        unknown = DAGOrchestrator().compile(
            loop_command(), {"events": loop_history("initial", status="execution-unknown")}
        )
        self.assertEqual("iteration-limit", limited["outcome"])
        self.assertTrue(limited["limit_exhausted"])
        self.assertEqual("iteration-limit", d5_limited["outcome"])
        self.assertEqual("execution-failed", retry["outcome"])
        self.assertTrue(retry["hard_failure"])
        self.assertEqual("recovery-required", unknown["outcome"])
        self.assertTrue(unknown["execution_unknown"])
        self.assertTrue(unknown["needs_recovery"])


if __name__ == "__main__":
    unittest.main()
