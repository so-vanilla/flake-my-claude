"""Pure contracts for Workflow Execution V2.

The public Interfaces in this module validate and derive immutable artifacts.  They
do not execute commands, persist receipts, or grant workflow transition authority.
"""

from __future__ import annotations

import copy
import hashlib
import json
import posixpath
import re
from collections.abc import Mapping, Sequence
from typing import Any, Dict, Iterable, List

from .completion import (
    CompletionError,
    classify_completion,
    create_machine_decision_receipt,
)
from .evidence_validity import EvidenceValidityError, assess_evidence
from .repair_batch import RepairBatchError, plan_fix_batches
from .review_packages import ReviewPackageError, build_review_package


_DIGEST = re.compile(r"^sha256:[0-9a-f]{64}$")
_IDENTIFIER = re.compile(r"^[A-Za-z0-9][A-Za-z0-9._:/-]{0,127}$")
_LOOP_SCHEMA = "workflow-loop/v1"
_LOOP_AXES = {"architecture-safety", "integration-operability"}
_LOOP_IGNORED_BUDGET_FIELDS = {
    "budget", "observed_budget", "remaining_seconds", "wall_clock_minutes",
    "review_round", "product_fix_attempt", "product_fix_attempts", "task_budgets",
}
_LOOP_SIDECAR_FIELDS = {
    "current_inputs", "change_impact", "required_checks", "required_coverage",
    "repair_findings", "repair_batch_findings", "required_findings",
    "review_package_inputs", "delta_review_inputs", "review_candidate",
    "review_requirements", "package_requirements", "prior_findings", "impact",
    "assignments", "review_assignments", "review_mode", "review_results",
    "rereview_results", "batch_resolutions", "resolutions", "receipt_id",
}


class V2ContractError(ValueError):
    """A V2 artifact is incomplete, ambiguous, stale, or internally inconsistent."""


def _canonical_digest(value: Mapping[str, Any]) -> str:
    encoded = json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=False).encode("utf-8")
    return "sha256:" + hashlib.sha256(encoded).hexdigest()


def _mapping(value: Any, label: str, keys: Iterable[str]) -> Dict[str, Any]:
    expected = set(keys)
    if not isinstance(value, Mapping) or set(value) != expected:
        raise V2ContractError("%s must contain exactly %s" % (label, sorted(expected)))
    return dict(value)


def _identifier(value: Any, label: str) -> str:
    if not isinstance(value, str) or _IDENTIFIER.fullmatch(value) is None:
        raise V2ContractError("%s is not a valid identifier" % label)
    return value


def _digest(value: Any, label: str) -> str:
    if not isinstance(value, str) or _DIGEST.fullmatch(value) is None:
        raise V2ContractError("%s is not a sha256 digest" % label)
    return value


def _ref(value: Any, label: str) -> Dict[str, str]:
    ref = _mapping(value, label, {"id", "digest"})
    return {"id": _identifier(ref["id"], "%s.id" % label), "digest": _digest(ref["digest"], "%s.digest" % label)}


def _refs(value: Any, label: str, *, nonempty: bool = False) -> List[Dict[str, str]]:
    if not isinstance(value, list) or (nonempty and not value):
        raise V2ContractError("%s must be %sa list" % (label, "a non-empty " if nonempty else ""))
    result = [_ref(item, "%s[%d]" % (label, index)) for index, item in enumerate(value)]
    ids = [item["id"] for item in result]
    if len(ids) != len(set(ids)):
        raise V2ContractError("%s contains duplicate identities" % label)
    return sorted(result, key=lambda item: item["id"])


class ExecutionClosureBuilder:
    """Freeze every declared behavior-affecting input behind one digest."""

    _KEYS = {
        "schema", "package_id", "contract_version", "candidate_ref", "test_refs",
        "fixture_refs", "schema_refs", "config_refs", "lock_refs", "toolchain",
        "command", "environment", "isolation", "resource_claims", "supervision",
        "external_input_refs", "workspace_identity",
    }

    def freeze(self, package: Mapping[str, Any]) -> Dict[str, Any]:
        value = _mapping(package, "execution package", self._KEYS)
        if value["schema"] != "execution-package-input/v2":
            raise V2ContractError("execution package schema must be execution-package-input/v2")
        toolchain = _mapping(value["toolchain"], "toolchain", {"executable_digest", "identity"})
        command = _mapping(value["command"], "command", {"argv", "cwd"})
        isolation = _mapping(value["isolation"], "isolation", {"cwd", "temporary_namespace", "output_namespace"})
        workspace_identity = _workspace_identity(value["workspace_identity"])
        claims = _resource_claims(value["resource_claims"], workspace_identity)
        supervision = _mapping(value["supervision"], "supervision", {"timeout_seconds", "grace_seconds", "signals", "heartbeat_seconds", "terminal_publication_seconds"})
        if not isinstance(command["argv"], list) or not command["argv"] or any(not isinstance(item, str) or not item for item in command["argv"]):
            raise V2ContractError("command.argv must be a non-empty string list")
        for label in ("cwd",):
            if not isinstance(command[label], str) or not command[label]:
                raise V2ContractError("command.%s must be non-empty" % label)
        for label in ("cwd", "temporary_namespace", "output_namespace"):
            if not isinstance(isolation[label], str) or not isolation[label]:
                raise V2ContractError("isolation.%s must be non-empty" % label)
        if command["cwd"] != isolation["cwd"]:
            raise V2ContractError("command and isolation cwd must match")
        _workspace_path(command["cwd"], workspace_identity, "command.cwd")
        _digest(toolchain["executable_digest"], "toolchain.executable_digest")
        _identifier(toolchain["identity"], "toolchain.identity")
        environment = value["environment"]
        if not isinstance(environment, Mapping) or any(not isinstance(key, str) or not key or not isinstance(item, str) for key, item in environment.items()):
            raise V2ContractError("environment must contain string keys and values")
        for field in ("timeout_seconds", "grace_seconds", "heartbeat_seconds", "terminal_publication_seconds"):
            if not isinstance(supervision[field], int) or isinstance(supervision[field], bool) or supervision[field] < 1:
                raise V2ContractError("supervision.%s must be a positive integer" % field)
        if not isinstance(supervision["signals"], list) or not supervision["signals"] or any(item not in {"TERM", "KILL", "INT"} for item in supervision["signals"]):
            raise V2ContractError("supervision.signals is invalid")

        closure = {
            "schema": "execution-package-closure/v2",
            "package_id": _identifier(value["package_id"], "package_id"),
            "contract_version": _identifier(value["contract_version"], "contract_version"),
            "workspace_identity": workspace_identity,
            "candidate_ref": _ref(value["candidate_ref"], "candidate_ref"),
            "test_refs": _refs(value["test_refs"], "test_refs", nonempty=True),
            "fixture_refs": _refs(value["fixture_refs"], "fixture_refs"),
            "schema_refs": _refs(value["schema_refs"], "schema_refs", nonempty=True),
            "config_refs": _refs(value["config_refs"], "config_refs"),
            "lock_refs": _refs(value["lock_refs"], "lock_refs", nonempty=True),
            "toolchain": copy.deepcopy(toolchain),
            "command": copy.deepcopy(command),
            "environment": {key: environment[key] for key in sorted(environment)},
            "isolation": copy.deepcopy(isolation),
            "resource_claims": claims,
            "supervision": copy.deepcopy(supervision),
            "external_input_refs": _refs(value["external_input_refs"], "external_input_refs"),
        }
        closure["closure_digest"] = _canonical_digest(closure)
        return closure


def _validate_closure(value: Any) -> Dict[str, Any]:
    keys = ExecutionClosureBuilder._KEYS - {"schema"} | {"schema", "closure_digest"}
    closure = _mapping(value, "execution closure", keys)
    if closure["schema"] != "execution-package-closure/v2":
        raise V2ContractError("execution closure schema is invalid")
    _digest(closure["closure_digest"], "closure_digest")
    package = {key: copy.deepcopy(item) for key, item in closure.items() if key != "closure_digest"}
    package["schema"] = "execution-package-input/v2"
    rebuilt = ExecutionClosureBuilder().freeze(package)
    if rebuilt != closure:
        raise V2ContractError("execution closure digest does not match its members")
    return rebuilt


def _candidate(value: Any) -> Dict[str, Any]:
    candidate = _mapping(value, "candidate", {"schema", "candidate_id", "candidate_digest", "execution_closure_digest", "regression_inventory", "frozen"})
    if candidate["schema"] != "artifact-candidate/v1" or candidate["frozen"] is not True:
        raise V2ContractError("candidate must be a frozen artifact-candidate/v1")
    _identifier(candidate["candidate_id"], "candidate_id")
    _digest(candidate["candidate_digest"], "candidate_digest")
    _digest(candidate["execution_closure_digest"], "execution_closure_digest")
    inventory = candidate["regression_inventory"]
    if not isinstance(inventory, list) or not inventory or any(not isinstance(item, str) or not item for item in inventory):
        raise V2ContractError("regression inventory must be a non-empty string list")
    if len(inventory) != len(set(inventory)):
        raise V2ContractError("regression inventory contains duplicates")
    candidate["regression_inventory"] = sorted(inventory)
    return copy.deepcopy(candidate)


class RegressionFrontier:
    """Validate a complete partition and admit one deterministic safe frontier."""

    def plan(self, candidate: Mapping[str, Any], closure: Mapping[str, Any], shards: Sequence[Mapping[str, Any]]) -> Dict[str, Any]:
        candidate_value = _candidate(candidate)
        closure_value = _validate_closure(closure)
        if candidate_value["execution_closure_digest"] != closure_value["closure_digest"]:
            raise V2ContractError("candidate and execution closure identities do not match")
        if candidate_value["candidate_digest"] != closure_value["candidate_ref"]["digest"]:
            raise V2ContractError("candidate digest is not the closure candidate")
        if not isinstance(shards, Sequence) or isinstance(shards, (str, bytes)) or not shards:
            raise V2ContractError("shards must be a non-empty sequence")
        normalized = [self._shard(item, index, closure_value) for index, item in enumerate(shards)]
        shard_ids = [item["shard_id"] for item in normalized]
        if len(shard_ids) != len(set(shard_ids)):
            raise V2ContractError("shard identities must be unique")
        memberships = [member for shard in normalized for member in shard["members"]]
        if len(memberships) != len(set(memberships)):
            raise V2ContractError("shard membership overlaps")
        if sorted(memberships) != candidate_value["regression_inventory"]:
            raise V2ContractError("shards must exactly cover the regression inventory")
        namespaces = [(item["isolation"]["temporary_namespace"], item["isolation"]["output_namespace"]) for item in normalized]
        if len(namespaces) != len(set(namespaces)):
            raise V2ContractError("shard isolation namespaces must be unique")
        plan = {
            "schema": "regression-shard-plan/v1",
            "candidate_ref": {"id": candidate_value["candidate_id"], "digest": candidate_value["candidate_digest"]},
            "execution_closure_digest": closure_value["closure_digest"],
            "execution_closure": closure_value,
            "regression_inventory": candidate_value["regression_inventory"],
            "shards": sorted(normalized, key=lambda item: item["shard_id"]),
        }
        plan["plan_digest"] = _canonical_digest(plan)
        return plan

    def admit_wave(self, plan: Mapping[str, Any]) -> List[Dict[str, Any]]:
        value = self._validate_plan(plan)
        admitted: List[Dict[str, Any]] = []
        for shard in value["shards"]:
            if all(self._compatible(shard["resource_claims"], item["resource_claims"]) for item in admitted):
                admitted.append(copy.deepcopy(shard))
        return admitted

    @staticmethod
    def _shard(value: Any, index: int, base_closure: Mapping[str, Any] | None = None) -> Dict[str, Any]:
        input_keys = {"shard_id", "members", "command", "resource_claims", "isolation"}
        planned_keys = input_keys | {"execution_closure", "execution_closure_ref"}
        shard = _mapping(value, "shard[%d]" % index, input_keys if base_closure is not None else planned_keys)
        members = shard["members"]
        if not isinstance(members, list) or not members or any(not isinstance(item, str) or not item for item in members):
            raise V2ContractError("shard members must be a non-empty string list")
        if len(members) != len(set(members)):
            raise V2ContractError("one shard contains duplicate members")
        isolation = _mapping(shard["isolation"], "shard isolation", {"cwd", "temporary_namespace", "output_namespace"})
        if any(not isinstance(item, str) or not item for item in isolation.values()):
            raise V2ContractError("shard isolation values must be non-empty strings")
        command = _mapping(shard["command"], "shard command", {"argv"})
        if not isinstance(command["argv"], list) or not command["argv"] or any(not isinstance(item, str) or not item for item in command["argv"]):
            raise V2ContractError("shard command argv must be a non-empty string list")
        if base_closure is not None:
            workspace = base_closure["workspace_identity"]
            claims = _resource_claims(shard["resource_claims"], workspace)
            _workspace_path(isolation["cwd"], workspace, "shard isolation cwd")
            package = {key: copy.deepcopy(item) for key, item in base_closure.items() if key != "closure_digest"}
            package["schema"] = "execution-package-input/v2"
            package["package_id"] = "%s:%s" % (base_closure["package_id"], _identifier(shard["shard_id"], "shard_id"))
            package["command"] = {"argv": copy.deepcopy(command["argv"]), "cwd": isolation["cwd"]}
            package["isolation"] = copy.deepcopy(isolation)
            package["resource_claims"] = copy.deepcopy(claims)
            if any(item["id"] == "parent-execution-closure" for item in package["external_input_refs"]):
                raise V2ContractError("external_input_refs reserves parent-execution-closure for shard derivation")
            package["external_input_refs"] = package["external_input_refs"] + [{"id": "parent-execution-closure", "digest": base_closure["closure_digest"]}]
            execution = ExecutionClosureBuilder().freeze(package)
        else:
            execution = _validate_closure(shard["execution_closure"])
            claims = _resource_claims(shard["resource_claims"], execution["workspace_identity"])
            ref = _ref(shard["execution_closure_ref"], "shard execution_closure_ref")
            if ref != {"id": execution["package_id"], "digest": execution["closure_digest"]}:
                raise V2ContractError("shard execution closure ref is invalid")
            if execution["command"] != {"argv": command["argv"], "cwd": isolation["cwd"]} or execution["isolation"] != isolation or execution["resource_claims"] != claims:
                raise V2ContractError("shard execution closure does not match effective shard inputs")
        return {"shard_id": _identifier(shard["shard_id"], "shard_id"), "members": sorted(members), "command": copy.deepcopy(command), "resource_claims": claims, "isolation": copy.deepcopy(isolation), "execution_closure": execution, "execution_closure_ref": {"id": execution["package_id"], "digest": execution["closure_digest"]}}

    @classmethod
    def _validate_plan(cls, value: Any) -> Dict[str, Any]:
        plan = _mapping(value, "shard plan", {"schema", "candidate_ref", "execution_closure_digest", "execution_closure", "regression_inventory", "shards", "plan_digest"})
        if plan["schema"] != "regression-shard-plan/v1":
            raise V2ContractError("shard plan schema is invalid")
        _ref(plan["candidate_ref"], "candidate_ref")
        _digest(plan["execution_closure_digest"], "execution_closure_digest")
        parent_closure = _validate_closure(plan["execution_closure"])
        if parent_closure["closure_digest"] != plan["execution_closure_digest"] or parent_closure["candidate_ref"] != plan["candidate_ref"]:
            raise V2ContractError("shard plan parent closure identity is invalid")
        _digest(plan["plan_digest"], "plan_digest")
        inventory = plan["regression_inventory"]
        if not isinstance(inventory, list) or not inventory or any(not isinstance(item, str) or not item for item in inventory) or len(inventory) != len(set(inventory)):
            raise V2ContractError("shard plan inventory is invalid")
        if not isinstance(plan["shards"], list) or not plan["shards"]:
            raise V2ContractError("shard plan must contain shards")
        normalized = [cls._shard(item, index) for index, item in enumerate(plan["shards"])]
        for shard in normalized:
            execution = shard["execution_closure"]
            if execution["candidate_ref"] != plan["candidate_ref"] or {"id": "parent-execution-closure", "digest": plan["execution_closure_digest"]} not in execution["external_input_refs"]:
                raise V2ContractError("shard execution closure does not bind plan candidate and parent closure")
        for index, shard in enumerate(normalized):
            shard_input = {key: copy.deepcopy(shard[key]) for key in ("shard_id", "members", "command", "resource_claims", "isolation")}
            if cls._shard(shard_input, index, parent_closure) != shard:
                raise V2ContractError("shard execution closure is not the exact parent-derived closure")
        memberships = [member for shard in normalized for member in shard["members"]]
        namespaces = [(item["isolation"]["temporary_namespace"], item["isolation"]["output_namespace"]) for item in normalized]
        if len({item["shard_id"] for item in normalized}) != len(normalized) or len(memberships) != len(set(memberships)) or sorted(memberships) != sorted(inventory) or len(namespaces) != len(set(namespaces)):
            raise V2ContractError("shard plan is not a complete disjoint isolated partition")
        if normalized != sorted(normalized, key=lambda item: item["shard_id"]) or inventory != sorted(inventory):
            raise V2ContractError("shard plan is not canonically ordered")
        unsigned = {key: copy.deepcopy(item) for key, item in plan.items() if key != "plan_digest"}
        if _canonical_digest(unsigned) != plan["plan_digest"]:
            raise V2ContractError("shard plan digest does not match its content")
        return copy.deepcopy(plan)

    @staticmethod
    def _compatible(left: Mapping[str, List[str]], right: Mapping[str, List[str]]) -> bool:
        if set(left["exclusive_resources"]) & set(right["exclusive_resources"]):
            return False
        for written in left["write_paths"]:
            if any(_paths_overlap(written, item) for item in right["read_paths"] + right["write_paths"]):
                return False
        for written in right["write_paths"]:
            if any(_paths_overlap(written, item) for item in left["read_paths"] + left["write_paths"]):
                return False
        return True


def _paths_overlap(left: str, right: str) -> bool:
    left = left.rstrip("/")
    right = right.rstrip("/")
    return left == right or left.startswith(right + "/") or right.startswith(left + "/")


class ReceiptAggregator:
    """Join complete closure-bound terminal receipts without executing commands."""

    _RECEIPT_KEYS = {
        "schema", "receipt_id", "shard_id", "candidate_digest",
        "execution_closure_digest", "idempotency_key", "payload_digest", "status",
        "coverage", "capture_state", "terminal", "receipt_digest",
    }

    def aggregate(self, candidate: Mapping[str, Any], plan: Mapping[str, Any], receipts: Sequence[Mapping[str, Any]]) -> Dict[str, Any]:
        candidate_value = _candidate(candidate)
        plan_value = RegressionFrontier._validate_plan(plan)
        if plan_value["candidate_ref"] != {"id": candidate_value["candidate_id"], "digest": candidate_value["candidate_digest"]}:
            raise V2ContractError("receipt plan does not bind the candidate")
        if plan_value["execution_closure_digest"] != candidate_value["execution_closure_digest"]:
            raise V2ContractError("receipt plan closure identity does not bind the candidate")
        if not isinstance(receipts, Sequence) or isinstance(receipts, (str, bytes)) or not receipts:
            raise V2ContractError("receipts must be a non-empty sequence")
        unique: Dict[str, Dict[str, Any]] = {}
        keys: Dict[str, Dict[str, Any]] = {}
        reused: List[str] = []
        for index, receipt in enumerate(receipts):
            value = self._receipt(receipt, index)
            for identity, seen in ((value["receipt_id"], unique), (value["idempotency_key"], keys)):
                if identity in seen:
                    if seen[identity] != value:
                        raise V2ContractError("same receipt identity has a changed payload")
                    reused.append(value["receipt_id"])
                    break
            else:
                unique[value["receipt_id"]] = value
                keys[value["idempotency_key"]] = value
        shards = {item["shard_id"]: item for item in plan_value["shards"]}
        by_shard: Dict[str, Dict[str, Any]] = {}
        for value in unique.values():
            shard = shards.get(value["shard_id"])
            if shard is None or value["shard_id"] in by_shard:
                raise V2ContractError("receipts must bind every shard exactly once")
            if value["candidate_digest"] != candidate_value["candidate_digest"] or value["execution_closure_digest"] != shard["execution_closure_ref"]["digest"]:
                raise V2ContractError("receipt closure identity does not match the candidate")
            if sorted(value["coverage"]) != sorted(shard["members"]):
                raise V2ContractError("receipt coverage does not match its shard")
            by_shard[value["shard_id"]] = value
        if set(by_shard) != set(shards):
            raise V2ContractError("receipt set is not terminally complete")
        coverage = sorted(item for value in by_shard.values() for item in value["coverage"])
        if coverage != candidate_value["regression_inventory"] or len(coverage) != len(set(coverage)):
            raise V2ContractError("receipt coverage is incomplete or overlapping")
        refs = [
            {"receipt_id": value["receipt_id"], "receipt_digest": value["receipt_digest"], "shard_id": value["shard_id"], "execution_closure_ref": copy.deepcopy(shards[value["shard_id"]]["execution_closure_ref"]), "status": value["status"]}
            for value in sorted(by_shard.values(), key=lambda item: item["shard_id"])
        ]
        aggregate = {
            "schema": "receipt-aggregate/v1",
            "candidate_ref": copy.deepcopy(plan_value["candidate_ref"]),
            "execution_closure_digest": candidate_value["execution_closure_digest"],
            "shard_plan_digest": plan_value["plan_digest"],
            "receipt_refs": refs,
            "coverage": coverage,
            "terminal_complete": True,
            "accepted": all(item["status"] == "passed" for item in by_shard.values()),
            "reused_receipt_ids": sorted(set(reused)),
        }
        aggregate["aggregate_digest"] = _canonical_digest(aggregate)
        return aggregate

    @classmethod
    def _receipt(cls, value: Any, index: int) -> Dict[str, Any]:
        receipt = _mapping(value, "receipt[%d]" % index, cls._RECEIPT_KEYS)
        if receipt["schema"] != "command-receipt/v1" or receipt["terminal"] is not True:
            raise V2ContractError("receipt must be a terminal command-receipt/v1")
        for field in ("receipt_id", "shard_id", "idempotency_key"):
            _identifier(receipt[field], "receipt.%s" % field)
        for field in ("candidate_digest", "execution_closure_digest", "payload_digest", "receipt_digest"):
            _digest(receipt[field], "receipt.%s" % field)
        if receipt["status"] not in {"passed", "failed", "timed_out", "interrupted"}:
            raise V2ContractError("receipt status is not terminal")
        coverage = receipt["coverage"]
        if not isinstance(coverage, list) or not coverage or any(not isinstance(item, str) or not item for item in coverage) or len(coverage) != len(set(coverage)):
            raise V2ContractError("receipt coverage is malformed")
        capture = _mapping(receipt["capture_state"], "receipt.capture_state", {"stdout", "stderr"})
        if any(item not in {"complete", "truncated"} for item in capture.values()):
            raise V2ContractError("receipt capture is incomplete or corrupt")
        unsigned = {key: copy.deepcopy(item) for key, item in receipt.items() if key != "receipt_digest"}
        if _canonical_digest(unsigned) != receipt["receipt_digest"]:
            raise V2ContractError("receipt digest does not match its payload")
        return copy.deepcopy(receipt)


class FindingValidator:
    """Validate two independent review axes and emit disposition advice only."""

    _CLASSES = {"required", "duplicate", "invalid", "deliberate-design", "downstream-only", "too-minor", "test-evidence-debt", "needs-user"}

    def validate(
        self,
        reviews: Sequence[Mapping[str, Any]] | Mapping[str, Any],
        dispositions: Sequence[Mapping[str, Any]] | None = None,
        observed_budget: Mapping[str, Any] | None = None,
    ) -> Dict[str, Any]:
        # Keep the historical three-argument disposition API intact while
        # providing a discoverable one-argument workflow-loop seam.
        if (
            dispositions is None
            and observed_budget is None
            and isinstance(reviews, Mapping)
            and (
                reviews.get("schema") == _LOOP_SCHEMA
                or any(key in reviews for key in ("workflow_loop", "loop_request", "completion_request"))
            )
        ):
            return self.validate_workflow_loop(reviews)
        if dispositions is None or observed_budget is None:
            raise V2ContractError("legacy FindingValidator requires reviews, dispositions, and observed_budget")
        if not isinstance(reviews, Sequence) or isinstance(reviews, (str, bytes)) or len(reviews) != 2:
            raise V2ContractError("exactly two review reports are required")
        normalized = [self._review(item, index) for index, item in enumerate(reviews)]
        if {item["axis"] for item in normalized} != {"architecture-safety", "integration-operability"}:
            raise V2ContractError("both required review axes must be present")
        for field in ("actor_id", "context_epoch_id", "package_digest", "report_id"):
            if len({item[field] for item in normalized}) != 2:
                raise V2ContractError("review reports require distinct %s values" % field)
        if len({item["candidate_digest"] for item in normalized}) != 1 or len({item["aggregate_digest"] for item in normalized}) != 1:
            raise V2ContractError("review reports must bind the same candidate and aggregate")
        budget = _mapping(observed_budget, "observed budget", {"remaining_seconds", "review_round", "product_fix_attempt"})
        if any(not isinstance(budget[field], int) or isinstance(budget[field], bool) or budget[field] < 0 for field in budget):
            raise V2ContractError("observed budget values must be non-negative integers")
        findings: Dict[str, List[Dict[str, Any]]] = {}
        finding_ids: List[str] = []
        for review in normalized:
            for finding in review["findings"]:
                if finding["finding_id"] in finding_ids:
                    raise V2ContractError("finding ids must be unique across axes")
                finding_ids.append(finding["finding_id"])
                findings.setdefault(finding["fingerprint"], []).append(finding)
        if not isinstance(dispositions, Sequence) or isinstance(dispositions, (str, bytes)):
            raise V2ContractError("dispositions must be a sequence")
        supplied: Dict[str, Dict[str, Any]] = {}
        for index, item in enumerate(dispositions):
            value = _mapping(item, "disposition[%d]" % index, {"fingerprint", "classification", "materiality", "proposed_scope"})
            fingerprint = _identifier(value["fingerprint"], "disposition fingerprint")
            if fingerprint in supplied or fingerprint not in findings:
                raise V2ContractError("disposition fingerprints must cover findings exactly once")
            if value["classification"] not in self._CLASSES or value["materiality"] not in {"material", "non-material"}:
                raise V2ContractError("disposition classification or materiality is invalid")
            scope = value["proposed_scope"]
            if not isinstance(scope, list) or any(not isinstance(path, str) or not path for path in scope) or len(scope) != len(set(scope)):
                raise V2ContractError("proposed scope must be a unique string list")
            supplied[fingerprint] = {
                "fingerprint": fingerprint,
                "source_finding_ids": sorted(finding["finding_id"] for finding in findings[fingerprint]),
                "classification": value["classification"],
                "materiality": value["materiality"],
                "proposed_scope": sorted(scope),
            }
        if set(supplied) != set(findings):
            raise V2ContractError("every deduplicated finding requires one disposition")
        report = {
            "schema": "finding-disposition/v1",
            "candidate_digest": normalized[0]["candidate_digest"],
            "receipt_aggregate_digest": normalized[0]["aggregate_digest"],
            "review_refs": sorted(
                ({"id": item["report_id"], "digest": _canonical_digest(item)} for item in normalized),
                key=lambda item: item["id"],
            ),
            "source_finding_ids": sorted(finding_ids),
            "dispositions": [supplied[key] for key in sorted(supplied)],
            "observed_budget": copy.deepcopy(budget),
            "advisory_only": True,
        }
        report["disposition_digest"] = _canonical_digest(report)
        return report

    @staticmethod
    def _review(value: Any, index: int) -> Dict[str, Any]:
        review = _mapping(value, "review[%d]" % index, {"schema", "report_id", "axis", "actor_id", "context_epoch_id", "package_digest", "candidate_digest", "aggregate_digest", "findings"})
        if review["schema"] != "ordinary-review-report/v1" or review["axis"] not in {"architecture-safety", "integration-operability"}:
            raise V2ContractError("review report schema or axis is invalid")
        for field in ("report_id", "actor_id", "context_epoch_id"):
            _identifier(review[field], "review.%s" % field)
        for field in ("package_digest", "candidate_digest", "aggregate_digest"):
            _digest(review[field], "review.%s" % field)
        if not isinstance(review["findings"], list):
            raise V2ContractError("review findings must be a list")
        normalized = []
        for position, item in enumerate(review["findings"]):
            finding = _mapping(item, "review finding", {"finding_id", "fingerprint", "severity", "summary"})
            _identifier(finding["finding_id"], "finding_id")
            _identifier(finding["fingerprint"], "fingerprint")
            if finding["severity"] not in {"critical", "major", "minor"} or not isinstance(finding["summary"], str) or not finding["summary"]:
                raise V2ContractError("review finding is invalid")
            normalized.append(copy.deepcopy(finding))
        review["findings"] = sorted(normalized, key=lambda item: item["finding_id"])
        return copy.deepcopy(review)

    def validate_workflow_loop(self, request: Mapping[str, Any], *, receipt_id: str | None = None) -> dict[str, Any]:
        """Validate the workflow-loop/v1 review/completion path.

        ``validate`` is the historical E6 disposition seam and intentionally
        retains its observed-budget argument.  This method is the additive
        workflow-loop/v1 seam: it never consumes that budget and only omits a
        Validator when the mechanical zero-finding proof is complete.
        """
        return WorkflowLoopValidator().validate(request, receipt_id=receipt_id)

    # A short alias keeps callers that use the loop terminology independent of
    # the historical E6 class name.
    validate_loop = validate_workflow_loop


def _loop_payload(value: Any) -> tuple[dict[str, Any], bool]:
    """Unwrap the additive loop envelope while preserving caller bytes."""
    if not isinstance(value, Mapping):
        raise V2ContractError("workflow-loop request must be a mapping")
    envelope = copy.deepcopy(dict(value))
    nested = False
    for key in ("workflow_loop", "loop_request", "completion_request"):
        if key in envelope:
            candidate = envelope.pop(key)
            if not isinstance(candidate, Mapping):
                raise V2ContractError("workflow-loop request is malformed")
            supplied = copy.deepcopy(dict(candidate))
            for sidecar in _LOOP_SIDECAR_FIELDS:
                if sidecar in envelope and sidecar not in supplied:
                    supplied[sidecar] = copy.deepcopy(envelope[sidecar])
            envelope = supplied
            nested = True
            break
    if envelope.get("schema") == _LOOP_SCHEMA:
        nested = True
        envelope.pop("schema", None)
        for key in ("request", "completion_request", "completion", "assessment", "payload"):
            if key not in envelope:
                continue
            supplied = envelope.pop(key)
            if supplied is None:
                continue
            if not isinstance(supplied, Mapping):
                raise V2ContractError("workflow-loop request payload is malformed")
            # A canonical completion request is the authoritative projection;
            # command-level phase/history fields must not leak into it.
            projected = copy.deepcopy(dict(supplied))
            for sidecar in _LOOP_SIDECAR_FIELDS:
                if sidecar in envelope and sidecar not in projected:
                    projected[sidecar] = copy.deepcopy(envelope[sidecar])
            envelope = projected
            break
    return envelope, nested


def _loop_completion_request(value: Mapping[str, Any]) -> dict[str, Any]:
    """Project loop inputs onto the strict completion classifier contract."""
    result = copy.deepcopy(dict(value))
    # These values may be present in a larger execution status projection, but
    # they are deliberately not allowed to decide loop progress.
    for key in _LOOP_IGNORED_BUDGET_FIELDS:
        result.pop(key, None)
    result.pop("schema", None)
    result.pop("current_inputs", None)
    result.pop("change_impact", None)
    result.pop("evidence_assessments", None)
    result.pop("required_checks", None)
    result.pop("required_coverage", None)
    for key in (
        "repair_findings", "repair_batch_findings", "required_findings",
        "review_package_inputs", "delta_review_inputs", "review_candidate",
        "review_requirements", "package_requirements", "prior_findings", "impact",
        "assignments", "review_assignments", "review_mode", "review_results",
        "rereview_results", "batch_resolutions", "resolutions", "candidate",
        "receipt_id",
    ):
        result.pop(key, None)
    return result


def _loop_evidence_current(
    value: Mapping[str, Any],
    completion_request: dict[str, Any],
) -> list[str]:
    """Check optional full evidence validity inputs without using time budgets."""
    current = value.get("current_inputs")
    if current is None:
        return []
    impact = value.get("change_impact", {"known": True, "invalidated_dimensions": [], "impacted_coverage": []})
    evidence = completion_request.get("evidence", [])
    invalid: list[str] = []
    for item in evidence:
        try:
            assessment = assess_evidence(item, current, impact)
        except EvidenceValidityError:
            invalid.append(str(item.get("evidence_id", "unknown")) if isinstance(item, Mapping) else "unknown")
            continue
        if assessment["status"] != "valid":
            invalid.append(assessment["evidence_id"])
    return sorted(set(invalid))


def _loop_required_coverage(value: Mapping[str, Any], completion_request: dict[str, Any]) -> list[str]:
    required = value.get("required_checks", value.get("required_coverage"))
    if required is None:
        required = completion_request.get("required_requirement_ids")
    if required is None:
        requirements = completion_request.get("requirements", [])
        return [item["requirement_id"] for item in requirements if isinstance(item, Mapping) and isinstance(item.get("requirement_id"), str)]
    if not isinstance(required, list) or not required or any(not isinstance(item, str) or not item for item in required) or len(required) != len(set(required)):
        raise V2ContractError("workflow-loop required coverage is malformed")
    completion_request["required_requirement_ids"] = copy.deepcopy(required)
    return sorted(required)


def _loop_coverage_blockers(value: Mapping[str, Any], completion_request: dict[str, Any]) -> list[str]:
    required = set(_loop_required_coverage(value, completion_request))
    if not required:
        return []
    evidence_coverage = {
        item
        for evidence in completion_request.get("evidence", [])
        if isinstance(evidence, Mapping)
        for item in evidence.get("coverage", [])
        if isinstance(item, str)
    }
    blockers = ["evidence:" + item for item in sorted(required - evidence_coverage)]
    for review in completion_request.get("reviews", []):
        if not isinstance(review, Mapping):
            continue
        coverage = set(review.get("coverage", [])) if isinstance(review.get("coverage"), list) else set()
        blockers.extend("review:" + item for item in sorted(required - coverage))
    return blockers


def _loop_completion_projection(value: Mapping[str, Any]) -> dict[str, Any]:
    """Build the strict completion projection and add explicit freshness stops."""
    completion_request = _loop_completion_request(value)
    invalid_evidence = _loop_evidence_current(value, completion_request)
    coverage_blockers = _loop_coverage_blockers(value, completion_request)
    blockers = invalid_evidence + coverage_blockers
    if blockers:
        mandatory = (
            list(completion_request.get("mandatory_unknowns", []))
            if isinstance(completion_request.get("mandatory_unknowns"), list)
            else []
        )
        completion_request["mandatory_unknowns"] = sorted(
            set(mandatory + ["current-evidence:" + item for item in blockers])
        )
    return completion_request


def _loop_finding_is_required(value: Mapping[str, Any]) -> bool:
    disposition = value.get("classification", value.get("disposition"))
    state = value.get("state", value.get("status"))
    required = value.get("required") is True or disposition in {"required", "open-required", "needs-user"} or state in {"needs-input", "unknown"}
    closed = value.get("closed") is True or state in {"closed", "resolved", "accepted", "fixed", "superseded"}
    return bool(required and not closed)


def _loop_repair_findings(value: Mapping[str, Any], candidate_digest: str) -> list[dict[str, Any]]:
    supplied = value.get(
        "repair_findings",
        value.get("repair_batch_findings", value.get("required_findings")),
    )
    if supplied is None:
        supplied = value.get("findings", [])
    if not isinstance(supplied, list):
        raise V2ContractError("workflow-loop repair findings must be a list")
    result: list[dict[str, Any]] = []
    for raw in supplied:
        if not isinstance(raw, Mapping) or not _loop_finding_is_required(raw):
            continue
        if {"finding_id", "fingerprint", "classification", "candidate_digest", "batch_key", "root_cause", "write_scope", "verification", "depends_on", "conflicts_with", "resolution_conditions"}.issubset(raw):
            canonical = copy.deepcopy(dict(raw))
            if canonical["candidate_digest"] != candidate_digest:
                raise V2ContractError("required Finding belongs to another candidate")
            result.append(canonical)
            continue
        finding_id = raw.get("finding_id", raw.get("id"))
        fingerprint = raw.get("fingerprint", finding_id)
        scope = raw.get("write_scope", raw.get("scope"))
        verification = raw.get("verification", raw.get("verification_scope"))
        if not isinstance(finding_id, str) or not isinstance(fingerprint, str) or not isinstance(scope, list) or not scope or not isinstance(verification, list) or not verification:
            raise V2ContractError("required Finding lacks explicit repair scope or verification")
        result.append({
            "finding_id": finding_id,
            "fingerprint": fingerprint,
            "classification": "required",
            "candidate_digest": candidate_digest,
            "batch_key": raw.get("batch_key"),
            "root_cause": raw.get("root_cause", finding_id),
            "write_scope": copy.deepcopy(scope),
            "verification": copy.deepcopy(verification),
            "depends_on": copy.deepcopy(raw.get("depends_on", [])),
            "conflicts_with": copy.deepcopy(raw.get("conflicts_with", [])),
            "resolution_conditions": copy.deepcopy(raw.get("resolution_conditions", ["fresh evidence and independent delta review"])),
        })
    return result


def _loop_declared_repair_ids(value: Mapping[str, Any]) -> list[str]:
    """Expose explicit repair blockers before completion classification."""
    supplied = value.get(
        "repair_findings",
        value.get("repair_batch_findings", value.get("required_findings")),
    )
    if supplied is None:
        return []
    if not isinstance(supplied, list):
        raise V2ContractError("workflow-loop repair findings must be a list")
    identifiers = []
    for item in supplied:
        if isinstance(item, Mapping) and _loop_finding_is_required(item):
            finding_id = item.get("finding_id", item.get("id"))
            if isinstance(finding_id, str) and finding_id:
                identifiers.append(finding_id)
    return sorted(set(identifiers))


def _loop_delta_packages(value: Mapping[str, Any], required_findings: Sequence[Mapping[str, Any]]) -> list[dict[str, Any]]:
    supplied = value.get("review_package_inputs", value.get("delta_review_inputs"))
    if supplied is None and any(
        key in value
        for key in (
            "review_candidate", "review_requirements", "package_requirements",
            "prior_findings", "impact", "review_assignments", "assignments",
        )
    ):
        supplied = {
            "candidate": value.get("review_candidate", value.get("candidate")),
            "requirements": value.get("review_requirements", value.get("package_requirements")),
            "prior_findings": value.get("prior_findings"),
            "impact": value.get("impact"),
            "assignments": value.get("review_assignments", value.get("assignments")),
        }
    if supplied is None:
        return []
    if not isinstance(supplied, Mapping):
        raise V2ContractError("workflow-loop delta review inputs are malformed")
    supplied = copy.deepcopy(dict(supplied))
    for alias, canonical in (
        ("review_candidate", "candidate"),
        ("review_requirements", "requirements"),
        ("package_requirements", "requirements"),
        ("findings", "prior_findings"),
        ("change_impact", "impact"),
        ("review_assignments", "assignments"),
    ):
        if canonical not in supplied and alias in supplied:
            supplied[canonical] = supplied[alias]
    required = {"candidate", "requirements", "prior_findings", "impact"}
    if not required.issubset(supplied):
        raise V2ContractError("workflow-loop delta review inputs are incomplete")
    assignments = supplied.get("assignments")
    if assignments is None:
        axis = supplied.get("axis")
        assignment = supplied.get("assignment")
        if axis is None or assignment is None:
            raise V2ContractError("workflow-loop delta review assignment is missing")
        assignments = {axis: assignment}
    if isinstance(assignments, Mapping) and {"assignment_id", "actor_id", "context_epoch"}.issubset(assignments):
        assignments = {axis: copy.deepcopy(dict(assignments)) for axis in sorted(_LOOP_AXES)}
    elif isinstance(assignments, list):
        normalized: dict[str, Any] = {}
        for item in assignments:
            if not isinstance(item, Mapping) or "axis" not in item:
                raise V2ContractError("workflow-loop delta review assignment is malformed")
            axis = item["axis"]
            normalized[axis] = {key: copy.deepcopy(item[key]) for key in item if key != "axis"}
        assignments = normalized
    if not isinstance(assignments, Mapping) or not assignments:
        raise V2ContractError("workflow-loop delta review assignments are malformed")
    packages: list[dict[str, Any]] = []
    for axis in sorted(assignments):
        if axis not in _LOOP_AXES:
            raise V2ContractError("workflow-loop delta review axis is unsupported")
        try:
            package = build_review_package(
                supplied["candidate"],
                supplied["requirements"],
                supplied["prior_findings"],
                supplied["impact"],
                requested_mode="delta",
                axis=axis,
                assignment=assignments[axis],
            )
        except ReviewPackageError as error:
            raise V2ContractError(str(error)) from error
        if package["candidate"]["candidate_ref"]["digest"] != required_findings[0]["candidate_digest"]:
            raise V2ContractError("delta review package belongs to another candidate")
        packages.append(package)
    return packages


class WorkflowLoopValidator:
    """Join v1 reviews and evidence without silently invoking an LLM."""

    def validate(self, request: Mapping[str, Any], *, receipt_id: str | None = None) -> dict[str, Any]:
        payload, _ = _loop_payload(request)
        receipt_id = receipt_id or payload.get("receipt_id")
        completion_request = _loop_completion_projection(payload)
        declared_repair_ids = _loop_declared_repair_ids(payload)
        if declared_repair_ids:
            existing = completion_request.get("open_required_findings")
            if existing is None:
                existing = []
            if isinstance(existing, list):
                completion_request["open_required_findings"] = sorted(
                    set(existing + declared_repair_ids)
                )
        try:
            classification = classify_completion(completion_request)
        except CompletionError as error:
            raise V2ContractError(str(error)) from error

        reviews = completion_request.get("reviews", [])
        finding_refs = [
            ref
            for review in reviews
            if isinstance(review, Mapping)
            for ref in review.get("finding_refs", [])
            if isinstance(review.get("finding_refs"), list)
        ]
        findings = completion_request.get("findings", [])
        declared_repair_entries = any(
            isinstance(payload.get(key), list) and bool(payload.get(key))
            for key in ("repair_findings", "repair_batch_findings", "required_findings")
        )
        zero_findings = not findings and not finding_refs and not declared_repair_entries
        strict_zero = bool(
            zero_findings
            and classification["outcome"] == "completed"
            and all(classification["checks"].values())
        )
        result: dict[str, Any] = {
            "schema": _LOOP_SCHEMA,
            "candidate_digest": classification["candidate_digest"],
            "package_digest": classification["package_digest"],
            "reviews": copy.deepcopy(reviews),
            "classification": copy.deepcopy(classification),
            "mechanical_completion": copy.deepcopy(classification),
            "validator": {
                "kind": "deterministic-zero-finding" if strict_zero else "llm-validator-required",
                "skipped": strict_zero,
                "required": not strict_zero,
                "reason": "all-required-checks-pass-and-findings-empty" if strict_zero else "mechanical-zero-finding-proof-incomplete",
            },
            "non_authorizing": True,
        }
        if strict_zero:
            try:
                result["machine_decision_receipt"] = create_machine_decision_receipt(classification, receipt_id)
            except CompletionError as error:
                raise V2ContractError(str(error)) from error

        required_findings = _loop_repair_findings(payload, classification["candidate_digest"])
        if required_findings:
            try:
                result["repair_batch_plan"] = plan_fix_batches(required_findings)
            except RepairBatchError as error:
                raise V2ContractError(str(error)) from error
            result["delta_review_packages"] = _loop_delta_packages(payload, required_findings)
            result["next"] = "repair-and-delta-rereview"
        elif strict_zero:
            result["next"] = "complete"
        else:
            result["next"] = "validator-required"
        unsigned = copy.deepcopy(result)
        result["result_digest"] = _canonical_digest(unsigned)
        return result

    compile = validate


class MechanicalCompletion:
    """Public mechanical completion facade for workflow-loop/v1 callers."""

    def classify(self, request: Mapping[str, Any]) -> dict[str, Any]:
        payload, _ = _loop_payload(request)
        completion_request = _loop_completion_projection(payload)
        declared_repair_ids = _loop_declared_repair_ids(payload)
        if declared_repair_ids:
            existing = completion_request.get("open_required_findings")
            if existing is None:
                existing = []
            if isinstance(existing, list):
                completion_request["open_required_findings"] = sorted(
                    set(existing + declared_repair_ids)
                )
        return classify_completion(completion_request)

    def evaluate(self, request: Mapping[str, Any], *, receipt_id: str | None = None) -> dict[str, Any]:
        return WorkflowLoopValidator().validate(request, receipt_id=receipt_id)

    def complete(self, request: Mapping[str, Any], *, receipt_id: str | None = None) -> dict[str, Any]:
        return self.evaluate(request, receipt_id=receipt_id)

    compile = evaluate


# Descriptive aliases keep the additive seam discoverable without changing the
# historical E6 class or its validate signature.
LoopReviewValidator = WorkflowLoopValidator
WorkflowLoopCompletion = MechanicalCompletion


def validate_workflow_loop(request: Mapping[str, Any], *, receipt_id: str | None = None) -> dict[str, Any]:
    """Validate the additive workflow-loop/v1 review/validator path."""
    return WorkflowLoopValidator().validate(request, receipt_id=receipt_id)


def mechanical_completion(request: Mapping[str, Any], *, receipt_id: str | None = None) -> dict[str, Any]:
    """Return a non-authorizing mechanical completion decision and receipt."""
    return WorkflowLoopValidator().validate(request, receipt_id=receipt_id)


mechanical_complete = mechanical_completion


class EvidenceFinalizer:
    """Mechanically assemble a close set only from fully accepted terminals."""

    def finalize(self, candidate: Mapping[str, Any], aggregate: Mapping[str, Any], dispositions: Mapping[str, Any], declared_inventory_ref: Mapping[str, Any], declared_inventory: Mapping[str, Any], branches: Sequence[Mapping[str, Any]]) -> Dict[str, Any]:
        candidate_value = _candidate(candidate)
        aggregate_value = self._aggregate(aggregate)
        disposition_value = self._dispositions(dispositions)
        if aggregate_value["candidate_ref"] != {"id": candidate_value["candidate_id"], "digest": candidate_value["candidate_digest"]}:
            raise V2ContractError("aggregate does not bind finalization candidate")
        if aggregate_value["execution_closure_digest"] != candidate_value["execution_closure_digest"]:
            raise V2ContractError("aggregate closure does not bind finalization candidate")
        if disposition_value["candidate_digest"] != candidate_value["candidate_digest"] or disposition_value["receipt_aggregate_digest"] != aggregate_value["aggregate_digest"]:
            raise V2ContractError("disposition report does not bind candidate and aggregate")
        if not aggregate_value["terminal_complete"] or not aggregate_value["accepted"]:
            raise V2ContractError("receipt aggregate is not completely accepted")
        if any(item["classification"] in {"required", "needs-user"} for item in disposition_value["dispositions"]):
            raise V2ContractError("open required or needs-user disposition prevents finalization")
        inventory = _mapping(declared_inventory, "declared branch inventory", {"schema", "inventory_id", "candidate_ref", "branches", "inventory_digest"})
        if inventory["schema"] != "declared-branch-inventory/v1" or _canonical_digest({key: copy.deepcopy(item) for key, item in inventory.items() if key != "inventory_digest"}) != inventory["inventory_digest"]:
            raise V2ContractError("declared branch inventory identity is invalid")
        inventory_id = _identifier(inventory["inventory_id"], "declared branch inventory_id")
        _digest(inventory["inventory_digest"], "declared branch inventory_digest")
        if _ref(declared_inventory_ref, "authoritative declared branch inventory ref") != {"id": inventory_id, "digest": inventory["inventory_digest"]}:
            raise V2ContractError("declared branch inventory does not match authoritative ref")
        if _ref(inventory["candidate_ref"], "declared branch candidate_ref") != {"id": candidate_value["candidate_id"], "digest": candidate_value["candidate_digest"]}:
            raise V2ContractError("declared branch inventory does not bind candidate")
        if not isinstance(inventory["branches"], list) or not inventory["branches"]:
            raise V2ContractError("declared branch inventory must be non-empty")
        declared = []
        for index, item in enumerate(inventory["branches"]):
            entry = _mapping(item, "declared branch[%d]" % index, {"branch_id", "terminal_ref"})
            declared.append({"branch_id": _identifier(entry["branch_id"], "declared branch_id"), "terminal_ref": _ref(entry["terminal_ref"], "declared terminal_ref")})
        if len({item["branch_id"] for item in declared}) != len(declared):
            raise V2ContractError("declared branch inventory contains duplicates")
        if not isinstance(branches, Sequence) or isinstance(branches, (str, bytes)) or not branches:
            raise V2ContractError("finalization branches must be non-empty")
        normalized = [self._branch(item, index) for index, item in enumerate(branches)]
        ids = [item["branch_id"] for item in normalized]
        if len(ids) != len(set(ids)) or any(item["state"] != "accepted" or item["complete"] is not True for item in normalized):
            raise V2ContractError("every branch must be a known complete accepted terminal")
        supplied_members = [{"branch_id": item["branch_id"], "terminal_ref": item["terminal_ref"]} for item in normalized]
        if sorted(supplied_members, key=lambda item: item["branch_id"]) != sorted(declared, key=lambda item: item["branch_id"]):
            raise V2ContractError("supplied branches must exactly equal the declared branch inventory")
        terminal_digests = {item["terminal_ref"]["digest"] for item in normalized}
        if not {aggregate_value["aggregate_digest"], disposition_value["disposition_digest"]}.issubset(terminal_digests):
            raise V2ContractError("finalization branches omit required terminal refs")
        result = {
            "schema": "evidence-finalization/v1",
            "candidate_ref": {"id": candidate_value["candidate_id"], "digest": candidate_value["candidate_digest"]},
            "execution_closure_digest": candidate_value["execution_closure_digest"],
            "receipt_aggregate_ref": {"id": "receipt-aggregate", "digest": aggregate_value["aggregate_digest"]},
            "finding_disposition_ref": {"id": "finding-disposition", "digest": disposition_value["disposition_digest"]},
            "declared_branch_inventory_ref": {"id": inventory_id, "digest": inventory["inventory_digest"]},
            "branches": sorted(normalized, key=lambda item: item["branch_id"]),
            "acceptance": {"known": True, "complete": True, "accepted": True},
        }
        result["finalization_digest"] = _canonical_digest(result)
        return result

    @staticmethod
    def _aggregate(value: Any) -> Dict[str, Any]:
        aggregate = _mapping(value, "receipt aggregate", {"schema", "candidate_ref", "execution_closure_digest", "shard_plan_digest", "receipt_refs", "coverage", "terminal_complete", "accepted", "reused_receipt_ids", "aggregate_digest"})
        if aggregate["schema"] != "receipt-aggregate/v1" or _canonical_digest({key: copy.deepcopy(item) for key, item in aggregate.items() if key != "aggregate_digest"}) != aggregate.get("aggregate_digest"):
            raise V2ContractError("receipt aggregate identity is invalid")
        _ref(aggregate["candidate_ref"], "receipt aggregate candidate_ref")
        for field in ("execution_closure_digest", "shard_plan_digest", "aggregate_digest"):
            _digest(aggregate[field], "receipt aggregate %s" % field)
        if aggregate["terminal_complete"] is not True or not isinstance(aggregate["accepted"], bool):
            raise V2ContractError("receipt aggregate terminal state is invalid")
        if not isinstance(aggregate["receipt_refs"], list) or not aggregate["receipt_refs"] or not isinstance(aggregate["coverage"], list) or not aggregate["coverage"]:
            raise V2ContractError("receipt aggregate coverage is invalid")
        return copy.deepcopy(aggregate)

    @staticmethod
    def _dispositions(value: Any) -> Dict[str, Any]:
        report = _mapping(value, "finding disposition", {"schema", "candidate_digest", "receipt_aggregate_digest", "review_refs", "source_finding_ids", "dispositions", "observed_budget", "advisory_only", "disposition_digest"})
        if report["schema"] != "finding-disposition/v1" or report["advisory_only"] is not True or _canonical_digest({key: copy.deepcopy(item) for key, item in report.items() if key != "disposition_digest"}) != report.get("disposition_digest"):
            raise V2ContractError("finding disposition identity is invalid")
        for field in ("candidate_digest", "receipt_aggregate_digest", "disposition_digest"):
            _digest(report[field], "finding disposition %s" % field)
        if not isinstance(report["dispositions"], list):
            raise V2ContractError("finding dispositions must be a list")
        for index, item in enumerate(report["dispositions"]):
            disposition = _mapping(item, "finding disposition[%d]" % index, {"fingerprint", "source_finding_ids", "classification", "materiality", "proposed_scope"})
            if disposition["classification"] not in FindingValidator._CLASSES:
                raise V2ContractError("unknown finding disposition")
        return copy.deepcopy(report)

    @staticmethod
    def _branch(value: Any, index: int) -> Dict[str, Any]:
        branch = _mapping(value, "branch[%d]" % index, {"branch_id", "state", "complete", "terminal_ref"})
        _identifier(branch["branch_id"], "branch_id")
        if branch["state"] not in {"accepted", "required", "needs-user", "incomplete", "unknown"} or not isinstance(branch["complete"], bool):
            raise V2ContractError("branch state is invalid")
        branch["terminal_ref"] = _ref(branch["terminal_ref"], "terminal_ref")
        return copy.deepcopy(branch)


class IssuanceWatermarkCutoverPlanner:
    """Derive a non-mutating atomic-cutover candidate from one current issuance view."""

    def plan(
        self,
        expected_head: Mapping[str, Any],
        current_head: Mapping[str, Any],
        lease: Mapping[str, Any],
        old_contract_version: str,
        new_contract_version: str,
        candidate_ref: Mapping[str, Any],
        issued_inventory_ref: Mapping[str, Any],
        issued_inventory: Mapping[str, Any],
    ) -> Dict[str, Any]:
        expected = self._head(expected_head, "expected_head")
        current = self._head(current_head, "current_head")
        if expected != current:
            raise V2ContractError("expected HEAD is stale")
        lease_value = _mapping(lease, "issuance lease", {"schema", "lease_id", "status", "expected_head"})
        if lease_value["schema"] != "issuance-lease/v1" or lease_value["status"] != "active" or self._head(lease_value["expected_head"], "lease.expected_head") != expected:
            raise V2ContractError("issuance lease is inactive or stale")
        _identifier(lease_value["lease_id"], "lease_id")
        old_version = _identifier(old_contract_version, "old_contract_version")
        new_version = _identifier(new_contract_version, "new_contract_version")
        if old_version == new_version:
            raise V2ContractError("cutover must select a new contract version")
        bound_candidate = _ref(candidate_ref, "cutover candidate_ref")
        inventory = _mapping(issued_inventory, "issued lineage inventory", {"schema", "inventory_id", "candidate_ref", "sequence_domain", "roots", "inventory_digest"})
        if inventory["schema"] != "issued-lineage-inventory/v1" or _canonical_digest({key: copy.deepcopy(item) for key, item in inventory.items() if key != "inventory_digest"}) != inventory["inventory_digest"]:
            raise V2ContractError("issued lineage inventory identity is invalid")
        inventory_id = _identifier(inventory["inventory_id"], "issued inventory_id")
        _digest(inventory["inventory_digest"], "issued inventory_digest")
        if _ref(issued_inventory_ref, "authoritative issued inventory ref") != {"id": inventory_id, "digest": inventory["inventory_digest"]}:
            raise V2ContractError("issued lineage inventory does not match authoritative ref")
        if _ref(inventory["candidate_ref"], "issued inventory candidate_ref") != bound_candidate:
            raise V2ContractError("candidate and issued lineage identities do not match")
        domain = _mapping(inventory["sequence_domain"], "issued sequence domain", {"first", "watermark"})
        if any(not isinstance(domain[field], int) or isinstance(domain[field], bool) or domain[field] < 0 for field in domain) or domain["first"] > domain["watermark"]:
            raise V2ContractError("issued sequence domain is invalid")
        if not isinstance(inventory["roots"], list) or not inventory["roots"]:
            raise V2ContractError("complete issued-root inventory is required")
        normalized = [self._root(item, index) for index, item in enumerate(inventory["roots"])]
        root_ids = [item["package_id"] for item in normalized]
        sequences = [item["sequence"] for item in normalized]
        if len(root_ids) != len(set(root_ids)) or len(sequences) != len(set(sequences)):
            raise V2ContractError("issued roots and sequences must be unique")
        issuance_watermark = domain["watermark"]
        if sorted(sequences) != list(range(domain["first"], issuance_watermark + 1)) or any(item["contract_version"] != old_version for item in normalized):
            raise V2ContractError("watermark must cover the complete old-contract root inventory")
        all_lineage_ids = [member["package_id"] for item in normalized for member in [item["root_member"]] + item["descendants"]]
        if len(all_lineage_ids) != len(set(all_lineage_ids)):
            raise V2ContractError("old-lineage package identity is ambiguous")
        bindings = [
            {"root_package_id": item["package_id"], "package_ids": sorted(member["package_id"] for member in [item["root_member"]] + item["descendants"]), "terminal_refs": sorted((copy.deepcopy(member["terminal_ref"]) for member in [item["root_member"]] + item["descendants"]), key=lambda ref: ref["id"]), "contract_version": old_version}
            for item in sorted(normalized, key=lambda item: item["sequence"])
        ]
        result = {
            "schema": "workflow-cutover/v1",
            "expected_head": expected,
            "lease_ref": {"id": lease_value["lease_id"], "digest": _canonical_digest(copy.deepcopy(lease_value))},
            "candidate_ref": bound_candidate,
            "issued_lineage_inventory_ref": {"id": inventory_id, "digest": inventory["inventory_digest"]},
            "old_contract_version": old_version,
            "new_contract_version": new_version,
            "issuance_watermark": issuance_watermark,
            "outstanding_old_package_ids": sorted(item["package_id"] for item in normalized if not item["terminal"]),
            "old_lineage_bindings": bindings,
            "selection_rule": {"at_or_below_watermark": old_version, "existing_descendants": old_version, "later_new_roots": new_version},
        }
        result["cutover_digest"] = _canonical_digest(result)
        return result

    @staticmethod
    def _head(value: Any, label: str) -> Dict[str, Any]:
        head = _mapping(value, label, {"revision", "transaction_digest"})
        if not isinstance(head["revision"], int) or isinstance(head["revision"], bool) or head["revision"] < 0:
            raise V2ContractError("%s revision is invalid" % label)
        _digest(head["transaction_digest"], "%s.transaction_digest" % label)
        return copy.deepcopy(head)

    @staticmethod
    def _root(value: Any, index: int) -> Dict[str, Any]:
        root = _mapping(value, "issued root[%d]" % index, {"package_id", "sequence", "contract_version", "terminal", "terminal_ref", "descendants"})
        _identifier(root["package_id"], "issued root package_id")
        _identifier(root["contract_version"], "issued root contract_version")
        if not isinstance(root["sequence"], int) or isinstance(root["sequence"], bool) or root["sequence"] < 0 or not isinstance(root["terminal"], bool):
            raise V2ContractError("issued root sequence or terminal state is invalid")
        root_ref = _ref(root["terminal_ref"], "issued root terminal_ref")
        if not isinstance(root["descendants"], list):
            raise V2ContractError("issued descendants must be a list")
        descendants = []
        for position, item in enumerate(root["descendants"]):
            descendant = _mapping(item, "issued descendant[%d]" % position, {"package_id", "terminal", "terminal_ref"})
            if descendant["terminal"] is not True:
                raise V2ContractError("every issued descendant must be terminal")
            descendants.append({"package_id": _identifier(descendant["package_id"], "descendant package_id"), "terminal": True, "terminal_ref": _ref(descendant["terminal_ref"], "descendant terminal_ref")})
        member_ids = [root["package_id"]] + [item["package_id"] for item in descendants]
        if len(member_ids) != len(set(member_ids)):
            raise V2ContractError("issued root lineage contains duplicate members")
        root["root_member"] = {"package_id": root["package_id"], "terminal": root["terminal"], "terminal_ref": root_ref}
        root["descendants"] = sorted(descendants, key=lambda item: item["package_id"])
        return copy.deepcopy(root)


def _workspace_identity(value: Any) -> str:
    if not isinstance(value, str) or not value.startswith("/") or value == "/" or value.endswith("/") or posixpath.normpath(value) != value or "//" in value:
        raise V2ContractError("workspace_identity must be a canonical absolute workspace path")
    return value


def _workspace_path(value: Any, workspace: str, label: str) -> str:
    if not isinstance(value, str) or not value.startswith(workspace + "/") and value != workspace or posixpath.normpath(value) != value or "//" in value:
        raise V2ContractError("%s must be canonical and inside workspace_identity" % label)
    return value


def _resource_claims(value: Any, workspace_identity: str) -> Dict[str, List[str]]:
    claims = _mapping(value, "resource_claims", {"read_paths", "write_paths", "exclusive_resources"})
    result: Dict[str, List[str]] = {}
    for field in ("read_paths", "write_paths", "exclusive_resources"):
        items = claims[field]
        if not isinstance(items, list) or any(not isinstance(item, str) or not item for item in items):
            raise V2ContractError("resource_claims.%s must be a string list" % field)
        if len(items) != len(set(items)):
            raise V2ContractError("resource_claims.%s contains duplicates" % field)
        if field != "exclusive_resources":
            for item in items:
                if item.startswith("/") or item in {".", ".."} or posixpath.normpath(item) != item or item.startswith("../") or "//" in item or item.startswith("./"):
                    raise V2ContractError("resource_claims.%s contains a non-canonical workspace-relative path" % field)
                _workspace_path(workspace_identity + "/" + item, workspace_identity, "resource claim")
        result[field] = sorted(items)
    return result


__all__ = [
    "EvidenceFinalizer", "ExecutionClosureBuilder", "FindingValidator",
    "IssuanceWatermarkCutoverPlanner", "LoopReviewValidator", "MechanicalCompletion",
    "ReceiptAggregator", "RegressionFrontier", "V2ContractError",
    "WorkflowLoopCompletion", "WorkflowLoopValidator", "mechanical_complete",
    "mechanical_completion", "validate_workflow_loop",
]
