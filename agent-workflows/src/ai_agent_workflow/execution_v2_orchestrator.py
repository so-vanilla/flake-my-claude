"""Pure Workflow Execution V2 Orchestrator transition compiler.

The public Interface derives immutable transition candidates.  It never writes HEAD,
persists an artifact, executes a command, or mutates a supplied value.
"""

from __future__ import annotations

import copy
import hashlib
import json
import re
from collections.abc import Iterable, Mapping, Sequence
from typing import Any, ClassVar

from .completion import CompletionClassificationError, classify_completion
from .loop_contracts import LoopContractError
from .loop_policy import (
    LoopPolicyError,
    count_counters,
    decide_next,
    phase_limits,
)
from .loop_state import derive_counters, reduce_history, validate_history
from .repair_batch import RepairBatchError, assess_batch_resolution, plan_fix_batches
from .review_packages import (
    ReviewPackageError,
    accept_review_result,
    build_review_package,
)

_DIGEST = re.compile(r"^sha256:[0-9a-f]{64}$")
_IDENTIFIER = re.compile(r"^[A-Za-z0-9][A-Za-z0-9._:/-]{0,127}$")
_CONTRACT_VERSION = "workflow-execution/v2"
_MAX_REVIEW_ROUND = 2
_MAX_PRODUCT_FIX_ATTEMPTS = 5
_ALLOWANCE_KEYS = {
    "command_timeout_seconds",
    "grace_seconds",
    "terminal_publication_seconds",
    "affected_regression_seconds",
    "round_two_reviews_seconds",
    "validator_seconds",
    "parent_seconds",
}
_LOOP_SCHEMA = "workflow-loop/v1"
_LOOP_RESULT_SCHEMA = "workflow-loop-transition/v1"
_LOOP_PHASES = {
    "B", "C", "D1-D4", "D5", "D6-D12", "E3-E7", "E8-E9", "G-H",
}
_LOOP_REVIEW_AXES = ("architecture-safety", "integration-operability")
_LOOP_PHASE_ALIASES = {
    "D1-D4": "D1",
    "D6-D12": "D6",
    "E3-E7": "E3",
    "E8-E9": "E8",
    "G-H": "G",
}
_LOOP_COMPLETION_IGNORED_FIELDS = {
    "schema",
    "budget",
    "observed_budget",
    "remaining_seconds",
    "wall_clock_minutes",
    "allowances",
    "replacement_budget",
    "reopen_budget",
    "review_round",
    "product_fix_attempt",
    "product_fix_attempts",
    "task_budgets",
    "current_inputs",
    "change_impact",
    "evidence_assessments",
    "required_checks",
    "required_coverage",
    "repair_findings",
    "repair_batch_findings",
    "required_findings",
    "review_package_inputs",
    "delta_review_inputs",
    "review_candidate",
    "review_requirements",
    "package_requirements",
    "prior_findings",
    "impact",
    "assignments",
    "review_assignments",
    "review_mode",
    "review_results",
    "rereview_results",
    "batch_resolutions",
    "resolutions",
    "candidate",
    "receipt_id",
}


class OrchestratorContractError(ValueError):
    """The requested transition is stale, unauthorized, or internally incomplete."""


def _canonical_digest(value: Mapping[str, Any]) -> str:
    encoded = json.dumps(
        value, sort_keys=True, separators=(",", ":"), ensure_ascii=False
    ).encode("utf-8")
    return "sha256:" + hashlib.sha256(encoded).hexdigest()


def _mapping(value: Any, label: str, keys: Iterable[str]) -> dict[str, Any]:
    expected = set(keys)
    if not isinstance(value, Mapping) or set(value) != expected:
        raise OrchestratorContractError(
            f"{label} must contain exactly {sorted(expected)}"
        )
    return dict(value)


def _identifier(value: Any, label: str) -> str:
    if not isinstance(value, str) or _IDENTIFIER.fullmatch(value) is None:
        raise OrchestratorContractError(f"{label} is not a valid identifier")
    return value


def _digest(value: Any, label: str) -> str:
    if not isinstance(value, str) or _DIGEST.fullmatch(value) is None:
        raise OrchestratorContractError(f"{label} is not a sha256 digest")
    return value


def _head(value: Any, label: str) -> dict[str, Any]:
    result = _mapping(value, label, {"revision", "transaction_digest"})
    if (
        not isinstance(result["revision"], int)
        or isinstance(result["revision"], bool)
        or result["revision"] < 0
    ):
        raise OrchestratorContractError(f"{label} revision is invalid")
    _digest(result["transaction_digest"], f"{label}.transaction_digest")
    return copy.deepcopy(result)


class DAGOrchestrator:
    """Compile one immutable transition candidate through a pure Interface."""

    _COMMAND_KEYS: ClassVar[set[str]] = {
        "schema",
        "command_id",
        "operation",
        "actor",
        "expected_head",
        "contract_version",
        "lease_id",
        "source_ref",
        "remaining_seconds",
        "allowances",
        "replacement_budget",
    }
    _STATE_KEYS: ClassVar[set[str]] = {
        "current_head",
        "active_lease",
        "advisory_disposition",
        "immutable_history",
        "prior_terminal",
    }

    def compile(
        self, command: Mapping[str, Any], observed_state: Mapping[str, Any]
    ) -> dict[str, Any]:
        """Return an immutable, non-mutating transition candidate or refuse."""

        if isinstance(command, Mapping) and command.get("schema") == _LOOP_SCHEMA:
            return self.compile_workflow_loop(command, observed_state)

        command_value = _mapping(command, "command", self._COMMAND_KEYS)
        state = _mapping(observed_state, "observed_state", self._STATE_KEYS)
        if command_value["schema"] != "orchestrator-command/v2":
            raise OrchestratorContractError("command schema is invalid")
        _identifier(command_value["command_id"], "command_id")
        if command_value["operation"] not in {"evaluate-advice", "reopen-stopped-budget"}:
            raise OrchestratorContractError("command operation is unsupported")
        actor = _mapping(command_value["actor"], "actor", {"role", "assignment_id"})
        if actor["role"] != "orchestrator":
            raise OrchestratorContractError("only the orchestrator role may compile transitions")
        _identifier(actor["assignment_id"], "actor.assignment_id")
        if command_value["contract_version"] != _CONTRACT_VERSION:
            raise OrchestratorContractError("contract version is not Workflow Execution V2")
        expected_head = _head(command_value["expected_head"], "expected_head")
        current_head = _head(state["current_head"], "current_head")
        if expected_head != current_head:
            raise OrchestratorContractError("expected HEAD is stale")

        lease = self._lease(state["active_lease"], expected_head, actor, command_value)
        history = self._history(state["immutable_history"])
        if command_value["operation"] == "reopen-stopped-budget":
            return self._compile_reopen(command_value, state, expected_head, lease, history)
        advisory = self._advisory(state["advisory_disposition"])
        source_ref = _mapping(command_value["source_ref"], "source_ref", {"id", "digest"})
        _identifier(source_ref["id"], "source_ref.id")
        if _digest(source_ref["digest"], "source_ref.digest") != advisory["disposition_digest"]:
            raise OrchestratorContractError("command does not bind the advisory disposition")
        self._require_advisory_acceptance(history, advisory)
        replayed = self._replayed_result(history, command_value, advisory)
        if replayed is not None:
            return replayed

        review_round = self._review_round(history, advisory["candidate_digest"])
        attempts = [
            {
                "fingerprint": item["fingerprint"],
                "count": self._product_attempts(
                    history,
                    item["fingerprint"],
                    advisory["candidate_digest"],
                    advisory["disposition_digest"],
                ),
            }
            for item in advisory["dispositions"]
            if item["classification"] == "required"
        ]
        non_product_retries = self._non_product_retries(
            history,
            advisory["candidate_digest"],
            advisory["disposition_digest"],
        )
        required = sorted(
            (
                item
                for item in advisory["dispositions"]
                if item["classification"] == "required"
            ),
            key=lambda item: item["fingerprint"],
        )
        if not required:
            raise OrchestratorContractError("advisory has no canonical required disposition")
        allowances = self._allowances(command_value["allowances"])
        required_seconds = sum(allowances.values())
        remaining_seconds = command_value["remaining_seconds"]
        if (
            not isinstance(remaining_seconds, int)
            or isinstance(remaining_seconds, bool)
            or remaining_seconds < 0
        ):
            raise OrchestratorContractError("remaining budget is invalid")

        stop_reason = None
        if review_round >= _MAX_REVIEW_ROUND:
            stop_reason = "review-round-exhausted"
        elif any(item["count"] >= _MAX_PRODUCT_FIX_ATTEMPTS for item in attempts):
            stop_reason = "product-fix-attempt-exhausted"
        elif remaining_seconds < required_seconds:
            stop_reason = "budget-insufficient"
        if stop_reason is not None:
            return self._stopped_budget(
                command_value,
                expected_head,
                source_ref,
                lease,
                review_round,
                attempts,
                non_product_retries,
                remaining_seconds,
                required_seconds,
                stop_reason,
            )

        finding = required[0]
        attempt = next(
            item["count"] for item in attempts if item["fingerprint"] == finding["fingerprint"]
        ) + 1
        task = {
            "schema": "bounded-fix-task/v2",
            "task_id": f"fix-{hashlib.sha256(finding['fingerprint'].encode()).hexdigest()[:12]}-attempt-{attempt}",
            "finding_fingerprint": finding["fingerprint"],
            "source_finding_ids": sorted(finding["source_finding_ids"]),
            "write_scope": sorted(finding["proposed_scope"]),
            "product_fix_attempt": attempt,
            "next_review_round": review_round + 1,
            "budget_seconds": required_seconds,
            "allowances": allowances,
            "expected_head": expected_head,
            "contract_version": _CONTRACT_VERSION,
        }
        authority = {"status": "issued", "task": task}
        authority["authority_digest"] = _canonical_digest(authority)
        result = {
            "schema": "orchestrator-transition/v2",
            "transition_id": "transition-" + command_value["command_id"],
            "kind": "fix-dispatch",
            "contract_version": _CONTRACT_VERSION,
            "expected_head": expected_head,
            "source_disposition_ref": copy.deepcopy(source_ref),
            "derived_counters": {
                "review_round": review_round,
                "product_fix_attempts": attempts,
                "non_product_retry_classes": non_product_retries,
            },
            "budget": {
                "remaining_seconds": remaining_seconds,
                "required_seconds": required_seconds,
            },
            "lease_action": {
                "lease_id": lease["lease_id"],
                "status": "active",
                "revoked_dispatch_ids": [],
            },
            "dispatch_authority": authority,
            "reopen": None,
            "non_mutating": True,
        }
        result["transition_digest"] = _canonical_digest(result)
        return result

    def compile_workflow_loop(
        self,
        command: Mapping[str, Any],
        observed_state: Mapping[str, Any] | None = None,
    ) -> dict[str, Any]:
        """Compile the explicit ``workflow-loop/v1`` transition path.

        This path is intentionally separate from the V2 command compiler.
        It derives a pure decision from the supplied loop state and evidence;
        time allocations and the V2 reopen shape are not inputs to progress.
        """

        return _compile_workflow_loop(command, observed_state)

    @staticmethod
    def _compile_reopen(
        command: Mapping[str, Any],
        state: Mapping[str, Any],
        expected_head: Mapping[str, Any],
        lease: Mapping[str, Any],
        history: list[dict[str, Any]],
    ) -> dict[str, Any]:
        terminal = DAGOrchestrator._terminal(state["prior_terminal"])
        if state["advisory_disposition"] is not None:
            raise OrchestratorContractError("reopen must not substitute current advisory state")
        source_ref = _mapping(command["source_ref"], "source_ref", {"id", "digest"})
        if source_ref != {
            "id": terminal["terminal_id"],
            "digest": terminal["terminal_digest"],
        }:
            raise OrchestratorContractError("reopen does not bind the prior terminal")
        matches = [
            item for item in history
            if item["event_type"] == "stopped-budget"
            and item["payload"] == {"terminal_digest": terminal["terminal_digest"]}
        ]
        if len(matches) != 1:
            raise OrchestratorContractError("prior terminal is not present exactly once in history")
        if lease["lease_id"] == terminal["lease_closure"]["lease_id"]:
            raise OrchestratorContractError("reopen requires a new lease")
        replacement = _mapping(
            command["replacement_budget"],
            "replacement_budget",
            {"version", "value_seconds"},
        )
        _identifier(replacement["version"], "replacement_budget.version")
        if (
            not isinstance(replacement["value_seconds"], int)
            or isinstance(replacement["value_seconds"], bool)
            or replacement["value_seconds"] < 1
        ):
            raise OrchestratorContractError("replacement budget value must be positive")
        if command["remaining_seconds"] != 0 or any(
            DAGOrchestrator._allowances(command["allowances"]).values()
        ):
            raise OrchestratorContractError("reopen command must use only its replacement budget")
        result = {
            "schema": "orchestrator-transition/v2",
            "transition_id": "transition-" + command["command_id"],
            "kind": "reopen",
            "contract_version": _CONTRACT_VERSION,
            "expected_head": copy.deepcopy(expected_head),
            "source_disposition_ref": copy.deepcopy(terminal["source_disposition_ref"]),
            "derived_counters": copy.deepcopy(terminal["derived_counters"]),
            "budget": {
                "remaining_seconds": replacement["value_seconds"],
                "required_seconds": 0,
            },
            "lease_action": {
                "lease_id": lease["lease_id"],
                "status": "active",
                "revoked_dispatch_ids": [],
            },
            "dispatch_authority": None,
            "reopen": {
                "prior_terminal_digest": terminal["terminal_digest"],
                "replacement_budget": copy.deepcopy(replacement),
            },
            "non_mutating": True,
        }
        result["transition_digest"] = _canonical_digest(result)
        return result

    @staticmethod
    def _stopped_budget(
        command: Mapping[str, Any],
        expected_head: Mapping[str, Any],
        source_ref: Mapping[str, Any],
        lease: Mapping[str, Any],
        review_round: int,
        attempts: list[dict[str, Any]],
        non_product_retries: Mapping[str, int],
        remaining_seconds: int,
        required_seconds: int,
        reason: str,
    ) -> dict[str, Any]:
        result = {
            "schema": "stopped-budget/v1",
            "terminal_id": "stopped-" + command["command_id"],
            "contract_version": _CONTRACT_VERSION,
            "expected_head": copy.deepcopy(expected_head),
            "source_disposition_ref": copy.deepcopy(source_ref),
            "reason": reason,
            "derived_counters": {
                "review_round": review_round,
                "product_fix_attempts": copy.deepcopy(attempts),
                "non_product_retry_classes": copy.deepcopy(non_product_retries),
            },
            "budget": {
                "remaining_seconds": remaining_seconds,
                "required_seconds": required_seconds,
            },
            "lease_closure": {"lease_id": lease["lease_id"], "status": "closed"},
            "revoked_dispatch_ids": sorted(lease["unaccepted_dispatch_ids"]),
            "dispatch_authority": None,
            "non_dispatch": True,
            "terminal": True,
            "reopen_requirements": {
                "prior_terminal_digest": True,
                "expected_head": True,
                "new_lease": True,
                "replacement_budget_version_and_value": True,
            },
        }
        result["terminal_digest"] = _canonical_digest(result)
        return result

    @staticmethod
    def _terminal(value: Any) -> dict[str, Any]:
        terminal = _mapping(
            value,
            "prior_terminal",
            {
                "schema", "terminal_id", "contract_version", "expected_head",
                "source_disposition_ref", "reason", "derived_counters", "budget",
                "lease_closure", "revoked_dispatch_ids", "dispatch_authority",
                "non_dispatch", "terminal", "reopen_requirements", "terminal_digest",
            },
        )
        if (
            terminal["schema"] != "stopped-budget/v1"
            or terminal["contract_version"] != _CONTRACT_VERSION
            or terminal["terminal"] is not True
            or terminal["non_dispatch"] is not True
            or terminal["dispatch_authority"] is not None
        ):
            raise OrchestratorContractError("prior terminal is not stopped-budget/v1")
        _identifier(terminal["terminal_id"], "terminal_id")
        supplied = _digest(terminal["terminal_digest"], "terminal_digest")
        unsigned = {
            key: copy.deepcopy(item)
            for key, item in terminal.items()
            if key != "terminal_digest"
        }
        if _canonical_digest(unsigned) != supplied:
            raise OrchestratorContractError("prior terminal digest does not match")
        return copy.deepcopy(terminal)

    @staticmethod
    def _allowances(value: Any) -> dict[str, int]:
        allowances = _mapping(value, "allowances", _ALLOWANCE_KEYS)
        if any(
            not isinstance(item, int) or isinstance(item, bool) or item < 0
            for item in allowances.values()
        ):
            raise OrchestratorContractError("allowances must be non-negative integers")
        return {key: allowances[key] for key in sorted(allowances)}

    @staticmethod
    def _lease(value: Any, expected_head: Mapping[str, Any], actor: Mapping[str, Any], command: Mapping[str, Any]) -> dict[str, Any]:
        lease = _mapping(
            value,
            "active_lease",
            {
                "schema",
                "lease_id",
                "status",
                "holder_assignment_id",
                "contract_version",
                "expected_head",
                "unaccepted_dispatch_ids",
            },
        )
        if lease["schema"] != "orchestrator-lease/v2" or lease["status"] != "active":
            raise OrchestratorContractError("orchestrator lease is not active")
        if lease["lease_id"] != command["lease_id"]:
            raise OrchestratorContractError("command lease identity is stale")
        if lease["holder_assignment_id"] != actor["assignment_id"]:
            raise OrchestratorContractError("lease is held by a different assignment")
        if lease["contract_version"] != _CONTRACT_VERSION:
            raise OrchestratorContractError("lease contract version is stale")
        if _head(lease["expected_head"], "lease.expected_head") != expected_head:
            raise OrchestratorContractError("lease expected HEAD is stale")
        dispatches = lease["unaccepted_dispatch_ids"]
        if not isinstance(dispatches, list) or len(dispatches) != len(set(dispatches)):
            raise OrchestratorContractError("lease dispatch inventory is malformed")
        for item in dispatches:
            _identifier(item, "unaccepted_dispatch_id")
        lease["unaccepted_dispatch_ids"] = sorted(dispatches)
        return copy.deepcopy(lease)

    @staticmethod
    def _advisory(value: Any) -> dict[str, Any]:
        advisory = _mapping(
            value,
            "advisory_disposition",
            {
                "schema",
                "candidate_digest",
                "receipt_aggregate_digest",
                "review_refs",
                "source_finding_ids",
                "dispositions",
                "observed_budget",
                "advisory_only",
                "disposition_digest",
            },
        )
        if advisory["schema"] != "finding-disposition/v1" or advisory["advisory_only"] is not True:
            raise OrchestratorContractError("source disposition is not advisory finding-disposition/v1")
        _digest(advisory["candidate_digest"], "advisory.candidate_digest")
        _digest(advisory["receipt_aggregate_digest"], "advisory.receipt_aggregate_digest")
        supplied_digest = _digest(advisory["disposition_digest"], "advisory.disposition_digest")
        unsigned = {key: copy.deepcopy(item) for key, item in advisory.items() if key != "disposition_digest"}
        if _canonical_digest(unsigned) != supplied_digest:
            raise OrchestratorContractError("advisory disposition digest does not match")
        if not isinstance(advisory["dispositions"], list):
            raise OrchestratorContractError("advisory dispositions must be a list")
        normalized: list[dict[str, Any]] = []
        for index, item in enumerate(advisory["dispositions"]):
            disposition = _mapping(
                item,
                f"disposition[{index}]",
                {"fingerprint", "source_finding_ids", "classification", "materiality", "proposed_scope"},
            )
            if disposition["classification"] not in {
                "required", "duplicate", "invalid", "deliberate-design", "downstream-only",
                "too-minor", "test-evidence-debt", "needs-user",
            }:
                raise OrchestratorContractError("advisory classification is invalid")
            if not isinstance(disposition["fingerprint"], str) or not disposition["fingerprint"]:
                raise OrchestratorContractError("disposition fingerprint is missing")
            for field in ("source_finding_ids", "proposed_scope"):
                if not isinstance(disposition[field], list) or any(not isinstance(entry, str) or not entry for entry in disposition[field]):
                    raise OrchestratorContractError(f"disposition {field} is malformed")
            normalized.append(copy.deepcopy(disposition))
        advisory["dispositions"] = normalized
        return copy.deepcopy(advisory)

    @staticmethod
    def _history(value: Any) -> list[dict[str, Any]]:
        if not isinstance(value, list):
            raise OrchestratorContractError("immutable history must be a list")
        result: list[dict[str, Any]] = []
        parent = None
        for index, item in enumerate(value):
            current = _mapping(
                item,
                f"history[{index}]",
                {"schema", "sequence", "parent_digest", "event_type", "contract_version", "payload", "event_digest"},
            )
            if current["schema"] != "orchestrator-history-event/v2" or current["sequence"] != index:
                raise OrchestratorContractError("immutable history sequence is invalid")
            if current["parent_digest"] != parent or current["contract_version"] != _CONTRACT_VERSION:
                raise OrchestratorContractError("immutable history chain or contract version is invalid")
            supplied = _digest(current["event_digest"], "history.event_digest")
            unsigned = {key: copy.deepcopy(entry) for key, entry in current.items() if key != "event_digest"}
            if _canonical_digest(unsigned) != supplied:
                raise OrchestratorContractError("immutable history event digest does not match")
            if not isinstance(current["payload"], Mapping):
                raise OrchestratorContractError("immutable history payload is malformed")
            result.append(copy.deepcopy(current))
            parent = supplied
        return result

    @staticmethod
    def _require_advisory_acceptance(history: list[dict[str, Any]], advisory: Mapping[str, Any]) -> None:
        matches = [
            item for item in history
            if item["event_type"] == "advisory-accepted"
            and item["payload"] == {
                "candidate_digest": advisory["candidate_digest"],
                "disposition_digest": advisory["disposition_digest"],
            }
        ]
        if len(matches) != 1:
            raise OrchestratorContractError("advisory disposition is not accepted exactly once")

    @staticmethod
    def _review_round(history: list[dict[str, Any]], candidate_digest: str) -> int:
        rounds = sorted(
            item["payload"].get("review_round")
            for item in history
            if item["event_type"] == "review-round-opened"
            and item["payload"].get("candidate_digest") == candidate_digest
        )
        if not rounds or rounds != list(range(1, len(rounds) + 1)):
            raise OrchestratorContractError("review round history is not contiguous")
        return len(rounds)

    @staticmethod
    def _product_attempts(
        history: list[dict[str, Any]],
        fingerprint: str,
        candidate_digest: str,
        disposition_digest: str,
    ) -> int:
        return sum(
            1 for item in history
            if item["event_type"] == "product-fix-authorized"
            and item["payload"].get("finding_fingerprint") == fingerprint
            and item["payload"].get("candidate_digest") == candidate_digest
            and item["payload"].get("disposition_digest") == disposition_digest
        )

    @staticmethod
    def _non_product_retries(
        history: list[dict[str, Any]],
        candidate_digest: str,
        disposition_digest: str,
    ) -> dict[str, int]:
        retry_events = {
            "test_fixture_correction": "test-fixture-correction",
            "command_or_capture_retry": "command-or-capture-retry",
            "package_or_report_correction": "package-or-report-correction",
        }
        unknown = sorted({
            item["event_type"] for item in history
            if item["event_type"].endswith("-retry")
            and item["event_type"] not in retry_events.values()
        })
        if unknown:
            raise OrchestratorContractError(
                f"immutable history contains unknown retry classes: {', '.join(unknown)}"
            )
        return {
            counter: sum(
                1
                for item in history
                if item["event_type"] == event_type
                and item["payload"].get("candidate_digest") == candidate_digest
                and item["payload"].get("disposition_digest") == disposition_digest
            )
            for counter, event_type in retry_events.items()
        }

    @staticmethod
    def _replayed_result(
        history: list[dict[str, Any]],
        command: Mapping[str, Any],
        advisory: Mapping[str, Any],
    ) -> Any:
        matches = [
            item
            for item in history
            if item["event_type"] == "transition-compiled"
            and item["payload"].get("command_id") == command["command_id"]
        ]
        if not matches:
            return None
        if len(matches) != 1:
            raise OrchestratorContractError("transition replay history is ambiguous")
        payload = _mapping(
            matches[0]["payload"],
            "transition replay payload",
            {"command_id", "command_digest", "disposition_digest", "result"},
        )
        if payload["command_digest"] != _canonical_digest(command):
            raise OrchestratorContractError("same command identity has changed payload")
        if payload["disposition_digest"] != advisory["disposition_digest"]:
            raise OrchestratorContractError("replay disposition identity changed")
        result = payload["result"]
        if not isinstance(result, Mapping):
            raise OrchestratorContractError("replayed result is malformed")
        if result.get("schema") == "orchestrator-transition/v2":
            digest_field = "transition_digest"
        elif result.get("schema") == "stopped-budget/v1":
            digest_field = "terminal_digest"
        else:
            raise OrchestratorContractError("replayed result schema is invalid")
        supplied = _digest(result.get(digest_field), "replayed result digest")
        unsigned = {
            key: copy.deepcopy(value)
            for key, value in result.items()
            if key != digest_field
        }
        if _canonical_digest(unsigned) != supplied:
            raise OrchestratorContractError("replayed result digest does not match")
        return copy.deepcopy(result)

def _loop_values(
    command: Mapping[str, Any], observed_state: Mapping[str, Any] | None,
) -> dict[str, Any]:
    """Join the v1 request and observation without retaining either input."""

    if not isinstance(command, Mapping) or command.get("schema") != _LOOP_SCHEMA:
        raise OrchestratorContractError("workflow-loop command schema is invalid")
    if observed_state is not None and not isinstance(observed_state, Mapping):
        raise OrchestratorContractError("workflow-loop observed state must be a mapping")

    values = copy.deepcopy(dict(command))
    for field in ("payload", "state", "assessment", "request"):
        nested = values.pop(field, None)
        if isinstance(nested, Mapping):
            values.update(copy.deepcopy(dict(nested)))
        elif nested is not None:
            raise OrchestratorContractError(f"workflow-loop {field} must be a mapping")
    if observed_state is not None:
        values.update(copy.deepcopy(dict(observed_state)))
    return values


def _loop_explicit_payload(values: Mapping[str, Any]) -> dict[str, Any]:
    """Unwrap a nested loop request without allowing envelope fields through."""

    explicit = values.get("completion_request", values.get("completion"))
    if explicit is None:
        return {}
    if not isinstance(explicit, Mapping):
        raise OrchestratorContractError(
            "workflow-loop completion request must be a mapping"
        )
    payload = copy.deepcopy(dict(explicit))
    for _ in range(2):
        if payload.get("schema") == _LOOP_SCHEMA:
            payload.pop("schema", None)
        nested = None
        for field in ("request", "completion_request", "completion", "assessment", "payload"):
            if field in payload:
                nested = payload.pop(field)
                if nested is None:
                    continue
                if not isinstance(nested, Mapping):
                    raise OrchestratorContractError(
                        "workflow-loop request payload must be a mapping"
                    )
                supplied = copy.deepcopy(dict(nested))
                for key, value in payload.items():
                    supplied.setdefault(key, copy.deepcopy(value))
                payload = supplied
                break
        else:
            break
    return payload


def _loop_value(values: Mapping[str, Any], *fields: str) -> Any:
    """Read a top-level value, then the equivalent nested-loop sidecar."""

    for field in fields:
        if field in values and values[field] is not None:
            return values[field]
    payload = _loop_explicit_payload(values)
    for field in fields:
        if field in payload and payload[field] is not None:
            return payload[field]
    return None


def _loop_candidate_digest(values: Mapping[str, Any]) -> str | None:
    digest = _loop_value(values, "candidate_digest")
    if isinstance(digest, str):
        return digest
    candidate = _loop_value(values, "candidate", "review_candidate")
    if isinstance(candidate, Mapping):
        digest = candidate.get("candidate_digest")
        if digest is None and isinstance(candidate.get("candidate_ref"), Mapping):
            digest = candidate["candidate_ref"].get("digest")
    return digest if isinstance(digest, str) else None


def _loop_phase(values: Mapping[str, Any]) -> tuple[str, dict[str, Any]]:
    candidate = values.get("profile", values.get("phase"))
    if candidate is None:
        identity = values.get("identity")
        if isinstance(identity, Mapping):
            candidate = identity.get("phase")
    if candidate is None:
        payload = _loop_explicit_payload(values)
        identity = payload.get("identity")
        if isinstance(identity, Mapping):
            candidate = identity.get("phase")
        if candidate is None:
            candidate = payload.get("profile", payload.get("phase"))
    if isinstance(candidate, Mapping):
        candidate = candidate.get("phase")
    if not isinstance(candidate, str):
        raise OrchestratorContractError("workflow-loop phase is required")
    candidate = _LOOP_PHASE_ALIASES.get(candidate, candidate)
    try:
        policy = phase_limits(candidate)
    except (LoopContractError, LoopPolicyError) as error:
        raise OrchestratorContractError(str(error)) from error
    if policy["policy_id"] not in _LOOP_PHASES:
        raise OrchestratorContractError("workflow-loop phase policy is unsupported")
    return policy["phase"], policy


def _loop_history(values: Mapping[str, Any]) -> list[dict[str, Any]]:
    raw = values.get(
        "events",
        values.get(
            "history",
            values.get(
                "iteration_events", values.get("state_entries", values.get("entries", []))
            ),
        ),
    )
    if raw is None:
        return []
    if isinstance(raw, Mapping):
        if "state_entries" not in raw and "entries" not in raw:
            raise OrchestratorContractError("workflow-loop history must be a sequence")
        raw = raw.get("state_entries", raw.get("entries"))
    if not isinstance(raw, Sequence) or isinstance(raw, (str, bytes)):
        raise OrchestratorContractError("workflow-loop history must be a sequence")
    return copy.deepcopy(list(raw))


def _loop_projection_events(value: Any) -> list[Mapping[str, Any]]:
    """Flatten explicit state-entry projections for status/counter facts."""

    if isinstance(value, Mapping):
        if value.get("schema") == "loop-iteration-event/v1":
            return [value]
        for field in ("event", "events", "iteration_events", "state_entries", "entries"):
            if field in value:
                return _loop_projection_events(value[field])
        return []
    if isinstance(value, Sequence) and not isinstance(value, (str, bytes)):
        result: list[Mapping[str, Any]] = []
        for item in value:
            result.extend(_loop_projection_events(item))
        return result
    return []


def _loop_state_projection(history: Sequence[Any]) -> bool:
    return any(
        isinstance(item, Mapping)
        and any(field in item for field in ("identity", "event", "events", "iteration_events", "entries"))
        and item.get("schema") != "loop-iteration-event/v1"
        for item in history
    )


def _loop_counter_summary(
    history: Sequence[Any],
) -> tuple[list[Mapping[str, Any]], dict[str, Any]]:
    """Validate a linear history or count an explicit multi-entry projection."""

    if not _loop_state_projection(history):
        validated = validate_history(history)
        return validated, derive_counters(validated)

    counts = count_counters(history)
    events = _loop_projection_events(history)
    by_kind = {kind: 0 for kind in ("initial", "improvement", "integration-return", "technical-retry")}
    for event in events:
        kind = event.get("kind")
        if kind in by_kind:
            by_kind[kind] += 1
    additional = sum(item["additional_iterations"] for item in counts.values())
    retries = sum(item["technical_retries"] for item in counts.values())
    initial = by_kind["initial"]
    return list(history), {
        "counter_key": None,
        "policy": None,
        "total": initial + additional + retries,
        "initial": initial,
        "additional_iterations": additional,
        "technical_retries": retries,
        "by_kind": by_kind,
        "highest_attempt": max((event.get("attempt", -1) for event in events), default=-1),
        "next_attempt": max((event.get("attempt", -1) for event in events), default=-1) + 1,
        "counter_entries": list(copy.deepcopy(counts).values()),
    }


def _loop_completion_request(
    values: Mapping[str, Any],
) -> tuple[dict[str, Any] | None, bool]:
    explicit = values.get("completion_request", values.get("completion"))
    request = _loop_explicit_payload(values)
    if "required_requirement_ids" not in request and "required_coverage" in request:
        request["required_requirement_ids"] = copy.deepcopy(request["required_coverage"])
    for field in _LOOP_COMPLETION_IGNORED_FIELDS:
        request.pop(field, None)

    candidate_digest = _loop_candidate_digest(values)
    package = values.get("package")
    package_digest = values.get("package_digest")
    if package_digest is None and isinstance(package, Mapping):
        package_digest = package.get("package_digest")

    aliases = {
        "identity": values.get("identity"),
        "candidate_digest": candidate_digest,
        "package_digest": package_digest,
        "requirements": values.get(
            "completion_requirements",
            values.get("requirement_assessments", values.get("requirements")),
        ),
        "required_requirement_ids": values.get("required_requirement_ids"),
        "reviews": values.get(
            "completion_reviews",
            values.get("review_assessments", values.get("reviews")),
        ),
        "evidence": values.get(
            "completion_evidence",
            values.get("evidence_records", values.get("evidence")),
        ),
        "findings": values.get(
            "completion_findings", values.get("findings", values.get("required_findings")),
        ),
    }
    for key, value in aliases.items():
        if key not in request and value is not None:
            request[key] = copy.deepcopy(value)
    for key in (
        "open_required_findings", "mandatory_unknowns", "unknowns", "contradictions",
        "blockers", "current_context_epoch", "worker_actor_id", "worker_context_epoch",
        "human_approval", "d5_selection", "d5_selected", "objective_achievement",
        "objective_achieved",
    ):
        if key in values and key not in request:
            request[key] = copy.deepcopy(values[key])

    # A bare execution state with no completion-shaped inputs must remain a
    # loop decision.  Once a strict completion projection is supplied, missing
    # fields are intentionally left for the completion classifier to reject.
    completion_markers = (
        "completion_request", "completion", "completion_requirements",
        "requirement_assessments", "completion_reviews", "review_assessments",
        "completion_evidence", "evidence_records", "required_requirement_ids",
    )
    strict_projection = any(marker in values for marker in completion_markers)
    strict_projection = strict_projection or any(
        isinstance(values.get(field), list)
        and any(
            isinstance(item, Mapping)
            and str(item.get("schema", "")).startswith("loop-")
            for item in values[field]
        )
        for field in ("requirements", "reviews", "evidence")
    )
    if not strict_projection and explicit is None:
        return None, False
    return request, True


def _loop_completion(values: Mapping[str, Any]) -> tuple[dict[str, Any] | None, str | None]:
    request, requested = _loop_completion_request(values)
    if not requested or request is None:
        return None, None
    try:
        return classify_completion(request), None
    except CompletionClassificationError as error:
        return None, str(error)


_REPAIR_FINDING_KEYS = {
    "finding_id", "fingerprint", "classification", "candidate_digest", "batch_key",
    "root_cause", "write_scope", "verification", "depends_on", "conflicts_with",
    "resolution_conditions",
}


def _loop_repair_findings(values: Mapping[str, Any]) -> tuple[list[Mapping[str, Any]], bool]:
    payload = _loop_explicit_payload(values)
    for field in ("repair_findings", "required_findings", "findings"):
        raw = values.get(field, payload.get(field))
        if raw is None:
            continue
        if not isinstance(raw, list):
            raise RepairBatchError(field + " must be a list")
        if field != "findings" or any(
            isinstance(item, Mapping) and _REPAIR_FINDING_KEYS.issubset(item)
            for item in raw
        ):
            return copy.deepcopy(raw), True
        return [], False
    return [], False


def _loop_repair_plan(
    values: Mapping[str, Any],
) -> tuple[dict[str, Any] | None, str | None]:
    try:
        findings, requested = _loop_repair_findings(values)
        if not requested:
            return None, None
        plan = plan_fix_batches(findings)
        candidate_digest = _loop_candidate_digest(values)
        if (
            plan.get("candidate_digest") is not None
            and candidate_digest is not None
            and plan["candidate_digest"] != candidate_digest
        ):
            raise RepairBatchError("repair batch candidate does not match the current candidate")
        return plan, None
    except RepairBatchError as error:
        return None, str(error)


def _loop_resolutions(
    values: Mapping[str, Any], plan: Mapping[str, Any] | None,
) -> tuple[list[dict[str, Any]], str | None]:
    if plan is None:
        return [], None
    raw = _loop_value(values, "batch_resolutions", "resolutions")
    if raw is None:
        return [], None
    results: list[dict[str, Any]] = []
    try:
        for batch in plan["batches"]:
            supplied = raw.get(batch["batch_id"]) if isinstance(raw, Mapping) else raw
            if supplied is None:
                continue
            if isinstance(raw, Mapping) and not isinstance(supplied, Sequence):
                supplied = [supplied]
            results.append(assess_batch_resolution(batch, supplied))
    except (RepairBatchError, TypeError) as error:
        return [], str(error)
    return results, None


def _loop_assignments(values: Mapping[str, Any]) -> dict[str, dict[str, Any]]:
    raw = _loop_value(values, "review_assignments", "assignments")
    if raw is None:
        return {}
    result: dict[str, dict[str, Any]] = {}
    if isinstance(raw, Mapping):
        if {"assignment_id", "actor_id", "context_epoch"}.issubset(raw):
            return {axis: copy.deepcopy(dict(raw)) for axis in _LOOP_REVIEW_AXES}
        for axis in _LOOP_REVIEW_AXES:
            if axis in raw:
                assignment = raw[axis]
                if not isinstance(assignment, Mapping):
                    raise ReviewPackageError("review assignment must be a mapping")
                result[axis] = copy.deepcopy(dict(assignment))
        unknown = set(raw) - set(_LOOP_REVIEW_AXES)
        if unknown:
            raise ReviewPackageError("review assignment names an unsupported axis")
        return result
    if not isinstance(raw, list):
        raise ReviewPackageError("review assignments must be a mapping or list")
    for item in raw:
        if not isinstance(item, Mapping) or "axis" not in item:
            raise ReviewPackageError("review assignment list item must name an axis")
        axis = item["axis"]
        if axis not in _LOOP_REVIEW_AXES:
            raise ReviewPackageError("review assignment names an unsupported axis")
        assignment = {key: copy.deepcopy(value) for key, value in item.items() if key != "axis"}
        result[axis] = assignment
    return result


def _loop_review_packages(
    values: Mapping[str, Any],
) -> tuple[list[dict[str, Any]], list[dict[str, Any]], str | None]:
    try:
        assignments = _loop_assignments(values)
        raw_results = _loop_value(values, "review_results", "rereview_results")
        candidate = _loop_value(values, "review_candidate", "candidate")
        requirements = _loop_value(values, "review_requirements", "package_requirements")
        prior_findings = _loop_value(values, "prior_findings") or []
        impact = _loop_value(values, "impact")
        if not assignments:
            if raw_results is not None:
                raise ReviewPackageError("review results require issued review packages")
            return [], [], None
        if candidate is None or requirements is None or impact is None:
            raise ReviewPackageError("candidate, review requirements, and impact are required")
        mode = _loop_value(values, "review_mode")
        if mode is None:
            changed_paths = impact.get("changed_paths") if isinstance(impact, Mapping) else None
            mode = "delta" if prior_findings or changed_paths else "initial"
        packages = [
            build_review_package(
                candidate,
                requirements,
                prior_findings,
                impact,
                requested_mode=mode,
                axis=axis,
                assignment=assignments[axis],
            )
            for axis in _LOOP_REVIEW_AXES
            if axis in assignments
        ]
        if raw_results is None:
            return packages, [], None
        if isinstance(raw_results, Mapping):
            raw_results = list(raw_results.values())
        if not isinstance(raw_results, list):
            raise ReviewPackageError("review results must be a list or mapping")
        by_axis = {package["axis"]: package for package in packages}
        accepted = []
        seen_axes = set()
        for result in raw_results:
            if not isinstance(result, Mapping) or result.get("axis") not in by_axis:
                raise ReviewPackageError("review result does not identify an issued package")
            if result["axis"] in seen_axes:
                raise ReviewPackageError("review results repeat an issued package")
            seen_axes.add(result["axis"])
            accepted.append(accept_review_result(by_axis[result["axis"]], result))
        return packages, accepted, None
    except ReviewPackageError as error:
        return [], [], str(error)


def _loop_policy_state(
    values: Mapping[str, Any], history: Sequence[Mapping[str, Any]],
    completion: Mapping[str, Any] | None, has_next_hypothesis: bool,
) -> dict[str, Any]:
    state: dict[str, Any] = {"events": copy.deepcopy(list(history))}
    for field in (
        "progress", "previous", "current", "stalled", "stall_window", "needs_input",
        "input_required", "recovery_required", "completed", "complete", "outcome",
        "mandatory_unknowns", "contradictions", "next_hypothesis",
    ):
        if field in values:
            state[field] = copy.deepcopy(values[field])
    if has_next_hypothesis and not state.get("next_hypothesis"):
        state["next_hypothesis"] = {"source": "validated-repair-batch"}
    if values.get("hard_failure") is True:
        state["execution_status"] = "failed"
    elif values.get("execution_unknown") is True or values.get("needs_recovery") is True:
        state["execution_status"] = "execution-unknown"
    elif "execution_status" in values:
        state["execution_status"] = copy.deepcopy(values["execution_status"])
    elif "execution" in values:
        state["execution_status"] = copy.deepcopy(values["execution"])
    if completion is not None:
        for field in ("requirements", "reviews", "evidence"):
            if field in completion:
                state[field] = copy.deepcopy(completion[field])
    return state


def _loop_result(
    phase: str,
    policy: Mapping[str, Any],
    decision: Mapping[str, Any],
    counters: Mapping[str, Any],
    *,
    outcome: str,
    reason: str,
    completion: Mapping[str, Any] | None = None,
    repair_plan: Mapping[str, Any] | None = None,
    resolutions: Sequence[Mapping[str, Any]] = (),
    review_packages: Sequence[Mapping[str, Any]] = (),
    accepted_reviews: Sequence[Mapping[str, Any]] = (),
    error: str | None = None,
) -> dict[str, Any]:
    additional_limit = int(policy["additional_iteration_limit"])
    retry_limit = int(policy["technical_retry_limit"])
    additional = int(counters.get("additional_iterations", 0))
    retries = int(counters.get("technical_retries", 0))
    return {
        "schema": _LOOP_RESULT_SCHEMA,
        "phase": phase,
        "policy": copy.deepcopy(dict(policy)),
        "outcome": outcome,
        "reason": reason,
        "counters": {
            "initial": int(counters.get("initial", 0)),
            "additional_iterations": additional,
            "technical_retries": retries,
            "additional_iteration_limit": additional_limit,
            "technical_retry_limit": retry_limit,
            "additional_iterations_remaining": max(0, additional_limit - additional),
            "technical_retries_remaining": max(0, retry_limit - retries),
        },
        "counter_entries": copy.deepcopy(counters.get("counter_entries", [])),
        "progressed": decision.get("progressed"),
        "stalled": bool(decision.get("stalled")),
        "completion": copy.deepcopy(completion),
        "repair_plan": copy.deepcopy(repair_plan),
        "batch_resolutions": copy.deepcopy(list(resolutions)),
        "review_packages": copy.deepcopy(list(review_packages)),
        "accepted_reviews": copy.deepcopy(list(accepted_reviews)),
        "hard_failure": outcome == "execution-failed",
        "limit_exhausted": outcome == "iteration-limit",
        "execution_unknown": outcome == "recovery-required",
        "needs_recovery": outcome == "recovery-required",
        "needs_input": outcome == "needs-input",
        "non_mutating": True,
        "error": error,
    }


def _compile_workflow_loop(
    command: Mapping[str, Any], observed_state: Mapping[str, Any] | None,
) -> dict[str, Any]:
    values = _loop_values(command, observed_state)
    phase, policy = _loop_phase(values)
    try:
        history = _loop_history(values)
        validated_history, counters = _loop_counter_summary(history)
        if not _loop_state_projection(history):
            reduced = reduce_history(validated_history)
            if reduced["recovery_required"]:
                values["needs_recovery"] = True
    except LoopContractError as error:
        empty_decision = {"progressed": None, "stalled": False}
        return _loop_result(
            phase,
            policy,
            empty_decision,
            {"initial": 0, "additional_iterations": 0, "technical_retries": 0},
            outcome="execution-failed",
            reason="loop history is invalid",
            error=str(error),
        )

    repair_plan, repair_error = _loop_repair_plan(values)
    resolutions, resolution_error = _loop_resolutions(values, repair_plan)
    packages, accepted_reviews, review_error = _loop_review_packages(values)
    if accepted_reviews:
        values = copy.deepcopy(values)
        values["completion_reviews"] = accepted_reviews
    completion, completion_error = _loop_completion(values)
    operation_error = completion_error or repair_error or resolution_error or review_error

    policy_state = _loop_policy_state(
        values,
        validated_history,
        completion,
        bool(repair_plan and repair_plan.get("batches")),
    )
    try:
        decision = decide_next(policy_state, profile=phase)
    except (LoopContractError, LoopPolicyError) as error:
        operation_error = operation_error or str(error)
        decision = {"outcome": "execution-failed", "progressed": None, "stalled": False}

    if operation_error:
        return _loop_result(
            phase,
            policy,
            decision,
            counters,
            outcome="execution-failed",
            reason="workflow-loop operation failed its contract checks",
            completion=completion,
            repair_plan=repair_plan,
            resolutions=resolutions,
            review_packages=packages,
            accepted_reviews=accepted_reviews,
            error=operation_error,
        )

    base_outcome = decision["outcome"]
    outcome = base_outcome
    reason = decision["reason"]
    has_batches = bool(repair_plan and repair_plan.get("batches"))
    review_results_supplied = _loop_value(values, "review_results", "rereview_results") is not None
    review_axes = {item["axis"] for item in accepted_reviews}
    package_axes = {item["axis"] for item in packages}
    incomplete_review_receipts = bool(
        review_results_supplied
        and packages
        and (len(accepted_reviews) != len(packages) or review_axes != package_axes)
    )
    if base_outcome in {"recovery-required", "execution-failed", "iteration-limit"}:
        outcome = base_outcome
        reason = decision["reason"]
    elif (
        completion is not None
        and completion["outcome"] == "completed"
        and (not packages or len(accepted_reviews) == len(packages))
    ):
        outcome = "completed"
        reason = "all required conditions, reviews, and current evidence pass"
    elif completion is not None and completion["outcome"] == "completed":
        outcome = "needs-input"
        reason = "fresh review results are required before completion"
    elif incomplete_review_receipts:
        outcome = "needs-input"
        reason = "all issued fresh review results are required before continuation"
    elif has_batches:
        outcome = "continue"
        reason = "compatible required findings are available as one or more repair batches"
    elif completion is not None:
        outcome = completion["outcome"]
        reason = "completion gates remain unresolved"

    return _loop_result(
        phase,
        policy,
        decision,
        counters,
        outcome=outcome,
        reason=reason,
        completion=completion,
        repair_plan=repair_plan,
        resolutions=resolutions,
        review_packages=packages,
        accepted_reviews=accepted_reviews,
    )


__all__ = ["DAGOrchestrator", "OrchestratorContractError"]
