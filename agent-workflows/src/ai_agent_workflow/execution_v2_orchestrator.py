"""Pure Workflow Execution V2 Orchestrator transition compiler.

The public Interface derives immutable transition candidates.  It never writes HEAD,
persists an artifact, executes a command, or mutates a supplied value.
"""

from __future__ import annotations

import copy
import hashlib
import json
import re
from collections.abc import Mapping
from typing import Any, Dict, Iterable, List


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


class OrchestratorContractError(ValueError):
    """The requested transition is stale, unauthorized, or internally incomplete."""


def _canonical_digest(value: Mapping[str, Any]) -> str:
    encoded = json.dumps(
        value, sort_keys=True, separators=(",", ":"), ensure_ascii=False
    ).encode("utf-8")
    return "sha256:" + hashlib.sha256(encoded).hexdigest()


def _mapping(value: Any, label: str, keys: Iterable[str]) -> Dict[str, Any]:
    expected = set(keys)
    if not isinstance(value, Mapping) or set(value) != expected:
        raise OrchestratorContractError(
            "%s must contain exactly %s" % (label, sorted(expected))
        )
    return dict(value)


def _identifier(value: Any, label: str) -> str:
    if not isinstance(value, str) or _IDENTIFIER.fullmatch(value) is None:
        raise OrchestratorContractError("%s is not a valid identifier" % label)
    return value


def _digest(value: Any, label: str) -> str:
    if not isinstance(value, str) or _DIGEST.fullmatch(value) is None:
        raise OrchestratorContractError("%s is not a sha256 digest" % label)
    return value


def _head(value: Any, label: str) -> Dict[str, Any]:
    result = _mapping(value, label, {"revision", "transaction_digest"})
    if (
        not isinstance(result["revision"], int)
        or isinstance(result["revision"], bool)
        or result["revision"] < 0
    ):
        raise OrchestratorContractError("%s revision is invalid" % label)
    _digest(result["transaction_digest"], "%s.transaction_digest" % label)
    return copy.deepcopy(result)


class DAGOrchestrator:
    """Compile one immutable transition candidate through a pure Interface."""

    _COMMAND_KEYS = {
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
    _STATE_KEYS = {
        "current_head",
        "active_lease",
        "advisory_disposition",
        "immutable_history",
        "prior_terminal",
    }

    def compile(
        self, command: Mapping[str, Any], observed_state: Mapping[str, Any]
    ) -> Dict[str, Any]:
        """Return an immutable, non-mutating transition candidate or refuse."""

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
            "task_id": "fix-%s-attempt-%d"
            % (hashlib.sha256(finding["fingerprint"].encode()).hexdigest()[:12], attempt),
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

    @staticmethod
    def _compile_reopen(
        command: Mapping[str, Any],
        state: Mapping[str, Any],
        expected_head: Mapping[str, Any],
        lease: Mapping[str, Any],
        history: List[Dict[str, Any]],
    ) -> Dict[str, Any]:
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
        attempts: List[Dict[str, Any]],
        non_product_retries: Mapping[str, int],
        remaining_seconds: int,
        required_seconds: int,
        reason: str,
    ) -> Dict[str, Any]:
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
    def _terminal(value: Any) -> Dict[str, Any]:
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
    def _allowances(value: Any) -> Dict[str, int]:
        allowances = _mapping(value, "allowances", _ALLOWANCE_KEYS)
        if any(
            not isinstance(item, int) or isinstance(item, bool) or item < 0
            for item in allowances.values()
        ):
            raise OrchestratorContractError("allowances must be non-negative integers")
        return {key: allowances[key] for key in sorted(allowances)}

    @staticmethod
    def _lease(value: Any, expected_head: Mapping[str, Any], actor: Mapping[str, Any], command: Mapping[str, Any]) -> Dict[str, Any]:
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
    def _advisory(value: Any) -> Dict[str, Any]:
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
        normalized: List[Dict[str, Any]] = []
        for index, item in enumerate(advisory["dispositions"]):
            disposition = _mapping(
                item,
                "disposition[%d]" % index,
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
                    raise OrchestratorContractError("disposition %s is malformed" % field)
            normalized.append(copy.deepcopy(disposition))
        advisory["dispositions"] = normalized
        return copy.deepcopy(advisory)

    @staticmethod
    def _history(value: Any) -> List[Dict[str, Any]]:
        if not isinstance(value, list):
            raise OrchestratorContractError("immutable history must be a list")
        result: List[Dict[str, Any]] = []
        parent = None
        for index, item in enumerate(value):
            current = _mapping(
                item,
                "history[%d]" % index,
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
    def _require_advisory_acceptance(history: List[Dict[str, Any]], advisory: Mapping[str, Any]) -> None:
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
    def _review_round(history: List[Dict[str, Any]], candidate_digest: str) -> int:
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
        history: List[Dict[str, Any]],
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
        history: List[Dict[str, Any]],
        candidate_digest: str,
        disposition_digest: str,
    ) -> Dict[str, int]:
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
            raise OrchestratorContractError("immutable history contains unknown retry classes: %s" % ", ".join(unknown))
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
        history: List[Dict[str, Any]],
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


__all__ = ["DAGOrchestrator", "OrchestratorContractError"]
