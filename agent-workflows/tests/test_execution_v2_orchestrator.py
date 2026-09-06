import copy
import hashlib
import json
import sys
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "src"))

from ai_agent_workflow.execution_v2_orchestrator import (  # noqa: E402
    DAGOrchestrator,
    OrchestratorContractError,
)
from ai_agent_workflow.schema_validation import (  # noqa: E402
    SchemaValidationError,
    validate_document,
)


DIGESTS = {letter: "sha256:" + letter * 64 for letter in "abcdef"}


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


if __name__ == "__main__":
    unittest.main()
