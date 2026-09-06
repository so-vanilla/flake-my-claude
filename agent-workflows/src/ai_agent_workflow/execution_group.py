"""Pure source backing for the Group E execution Skills.

This is deliberately a compiler, not an execution engine.  It binds physical
inputs and delegates the narrow deep seams to Workflow Execution V2.
"""
from __future__ import annotations

import copy
import hashlib
import json
import re
import posixpath
from collections.abc import Mapping, Sequence
from pathlib import Path
from typing import Any

from .execution_v2 import (EvidenceFinalizer, ExecutionClosureBuilder, FindingValidator,
                           ReceiptAggregator, V2ContractError)
from .execution_v2_orchestrator import DAGOrchestrator, OrchestratorContractError
from .persistent_receipts import (DeterministicProcessAdapter, DuplicateReceiptConflict,
                                  PersistentReceiptError, PersistentReceiptRunner)

_ROOT = Path(__file__).resolve().parents[2]
_POLICY = "agent-workflows/groups/required-only-feedback-execution-policy-v1.json"
_STAGES = {"group.E.E%d" % number for number in range(1, 11)}
_LEVELS = {"artifact", "section", "workflow"}
_SHA256 = re.compile(r"^sha256:[0-9a-f]{64}$")
_E1_KINDS = {"readiness-approval", "task-dag", "workspace-receipt", "git-state-receipt"}
_E1_CHECKS = {"authority", "expected_head", "lease", "dependencies", "conflicts", "blockers", "purpose", "budget", "workspace", "git"}


class ExecutionGroupRefusal(ValueError):
    """Typed, non-authorizing refusal returned by the facade."""


def _digest(value: Any) -> str:
    return "sha256:" + hashlib.sha256(json.dumps(value, sort_keys=True, separators=(",", ":")).encode()).hexdigest()


def _digest_value(value: Any, label: str) -> str:
    _require(isinstance(value, str) and _SHA256.fullmatch(value) is not None, "%s is not a full sha256 digest" % label)
    return value


def _candidate_digest(value: Mapping[str, Any]) -> str:
    unsigned = {key: copy.deepcopy(item) for key, item in value.items() if key != "candidate_digest"}
    return _digest(unsigned)


def _raw_digest(path: str) -> str:
    try:
        return "sha256:" + hashlib.sha256((_ROOT.parent / path).read_bytes()).hexdigest()
    except OSError as error:
        raise ExecutionGroupRefusal("physical input is unavailable: %s" % path) from error


def _refusal(stage: str, reason: str) -> dict[str, Any]:
    return {"schema": "execution-group-refusal/v1", "qualified_id": stage, "reason": reason, "non_mutating": True}


def _require(condition: bool, reason: str) -> None:
    if not condition:
        raise ExecutionGroupRefusal(reason)


def _head(value: Any) -> dict[str, Any]:
    _require(isinstance(value, Mapping) and set(value) == {"revision", "transaction_digest"}, "expected HEAD is malformed")
    _require(isinstance(value["revision"], int) and not isinstance(value["revision"], bool) and value["revision"] >= 0, "expected HEAD revision is invalid")
    _digest_value(value["transaction_digest"], "expected HEAD digest")
    return dict(value)


def _authority(value: Any, expected_head: Mapping[str, Any]) -> dict[str, Any]:
    keys = {"authority_id", "actor_id", "role", "assignment_id", "scope_ref", "epoch_id", "lease_id", "idempotency_key", "budget", "expected_head"}
    _require(isinstance(value, Mapping) and set(value) == keys, "authority binding is malformed")
    for key in ("authority_id", "actor_id", "role", "assignment_id", "epoch_id", "lease_id", "idempotency_key"):
        _require(isinstance(value[key], str) and value[key], "authority %s is missing" % key)
    _require(isinstance(value["scope_ref"], Mapping) and set(value["scope_ref"]) == {"path", "digest"}, "authority scope ref is malformed")
    _require(isinstance(value["scope_ref"]["path"], str) and value["scope_ref"]["path"], "authority scope path is invalid")
    _digest_value(value["scope_ref"]["digest"], "authority scope digest")
    _require(_head(value["expected_head"]) == expected_head, "authority expected HEAD is stale")
    budget = value["budget"]
    _require(isinstance(budget, Mapping) and set(budget) == {"seconds", "review_round", "product_fix_attempts"}, "finite budget is malformed")
    _require(all(isinstance(budget[k], int) and not isinstance(budget[k], bool) and budget[k] >= 0 for k in budget), "finite budget is invalid")
    _require(budget["seconds"] > 0, "finite budget is empty or expired")
    return copy.deepcopy(dict(value))


def _policy(inputs: Mapping[str, Any]) -> None:
    ref = inputs.get("required_only_policy_ref")
    _require(isinstance(ref, Mapping) and set(ref) == {"path", "digest"}, "canonical required-only policy binding is missing")
    _require(ref["path"] == _POLICY and ref["digest"] == _raw_digest(_POLICY), "canonical required-only policy digest drifted")


def _common(stage: str, inputs: Any, authority: Any, expected_head: Any) -> tuple[dict[str, Any], dict[str, Any]]:
    _require(stage in _STAGES, "unknown Group E selector")
    _require(isinstance(inputs, Mapping), "inputs are malformed")
    head = _head(expected_head)
    bound_authority = _authority(authority, head)
    _require(inputs.get("expected_head") == head, "input expected HEAD is stale")
    _require(inputs.get("authority_ref") == {key: bound_authority[key] for key in ("authority_id", "role", "assignment_id", "lease_id", "scope_ref")}, "input role, assignment, scope, authority, or lease is unbound")
    _require(inputs.get("loop_level") in _LEVELS, "loop level is invalid")
    _policy(inputs)
    return copy.deepcopy(dict(inputs)), bound_authority


class ArtifactCandidateBuilder:
    """Freeze one validated Task result and receipt; grant no lifecycle authority."""

    def build(self, task_result: Mapping[str, Any], receipt: Mapping[str, Any]) -> dict[str, Any]:
        _require(isinstance(task_result, Mapping) and isinstance(receipt, Mapping), "candidate inputs are malformed")
        _require(task_result.get("terminal") in {"DONE", "DONE_WITH_CONCERNS", "NEEDS_CONTEXT", "BLOCKED"}, "task result is not terminal")
        _require(receipt.get("terminal") is True and receipt.get("status") == "passed", "focused receipt is not an accepted terminal")
        _require(isinstance(task_result.get("task_id"), str) and task_result["task_id"], "task result task_id is missing")
        _digest_value(task_result.get("candidate_digest"), "task result candidate_digest")
        _digest_value(task_result.get("receipt_digest"), "task result receipt_digest")
        _digest_value(receipt.get("receipt_digest"), "receipt digest")
        _require(receipt.get("receipt_digest") == task_result["receipt_digest"], "receipt does not bind task result")
        candidate = {"schema": "artifact-candidate/v1", "task_id": task_result["task_id"], "candidate_digest": task_result["candidate_digest"], "receipt_digest": receipt["receipt_digest"], "changed_paths": sorted(task_result.get("changed_paths", [])), "frozen": True, "non_authorizing": True}
        candidate["artifact_digest"] = _digest(candidate)
        return candidate

    @staticmethod
    def runner_inputs(package: Mapping[str, Any]) -> tuple[dict[str, Any], dict[str, Any]]:
        closure = package.get("execution_closure"); ref = package.get("execution_closure_ref")
        _require(isinstance(closure, Mapping) and ref == {"id": closure.get("package_id"), "digest": closure.get("closure_digest")}, "E2 package cannot derive runner identity")
        supervision = closure.get("supervision")
        _require(isinstance(supervision, Mapping), "E2 closure supervision is missing")
        runner = {"schema": "execution-package/v1", "package_id": closure["package_id"], "execution_closure": copy.deepcopy(closure), "execution_closure_ref": copy.deepcopy(ref), "shard_id": "group-e-worker", "coverage": [item["id"] for item in closure["test_refs"]], "idempotency_key": package["idempotency_key"], "nonce": "group-e-nonce", "supervisor_lease": "group-e-lease", "retry_class": "none"}
        policy = {"schema": "supervision-policy/v1", "timeout_seconds": supervision["timeout_seconds"], "heartbeat_seconds": supervision["heartbeat_seconds"], "heartbeat_expiry_seconds": supervision["heartbeat_seconds"] + 1, "grace_seconds": supervision["grace_seconds"], "terminal_publication_seconds": supervision["terminal_publication_seconds"], "output_limit_bytes": 4096, "signals": copy.deepcopy(supervision["signals"]), "sensitivity": "confidential"}
        return runner, policy

    @staticmethod
    def verify_terminal_reuse(root: str | Path, package: Mapping[str, Any], policy: Mapping[str, Any]) -> dict[str, Any]:
        """Test-only deterministic seam: no real command is released."""
        adapter = DeterministicProcessAdapter(); adapter.queue(exit_code=0)
        runner = PersistentReceiptRunner(root, adapter=adapter, boot_id="group-e-test")
        first = runner.run(package, policy); second = runner.run(package, policy)
        _require(first == second and adapter.spawn_count == 1, "persistent receipt terminal reuse failed")
        evidence = {"schema": "e3-recovery-evidence/v1", "terminal_reused": True, "spawn_count": adapter.spawn_count, "idempotency_key": package["idempotency_key"], "closure_digest": package["execution_closure"]["closure_digest"], "terminal_receipt": copy.deepcopy(first)}
        evidence["evidence_digest"] = _digest(evidence)
        return evidence

    @staticmethod
    def refuse_ambiguous_replacement(root: str | Path, package: Mapping[str, Any], changed: Mapping[str, Any], policy: Mapping[str, Any]) -> None:
        adapter = DeterministicProcessAdapter(); adapter.queue(exit_code=0)
        runner = PersistentReceiptRunner(root, adapter=adapter, boot_id="group-e-test")
        runner.run(package, policy)
        try: runner.run(changed, policy)
        except (DuplicateReceiptConflict, PersistentReceiptError): return
        raise ExecutionGroupRefusal("same idempotency key accepted a changed replacement")


class ExecutionGroupV1:
    """One shared Group E candidate/refusal compiler for all three loop levels."""

    def compile(self, qualified_id: str, inputs: Mapping[str, Any], authority: Mapping[str, Any], expected_head: Mapping[str, Any]) -> dict[str, Any]:
        try:
            value, bound = _common(qualified_id, inputs, authority, expected_head)
            handler = getattr(self, "_" + qualified_id.rsplit("E", 1)[1])
            result = handler(value, bound)
            result.update({"schema": "execution-group-candidate/v1", "qualified_id": qualified_id, "loop_level": value["loop_level"], "expected_head": copy.deepcopy(expected_head), "authority_ref": {"authority_id": bound["authority_id"], "lease_id": bound["lease_id"], "epoch_id": bound["epoch_id"]}, "non_mutating": True})
            result["candidate_digest"] = _digest(result)
            return result
        except (ExecutionGroupRefusal, V2ContractError, OrchestratorContractError, PersistentReceiptError) as error:
            return _refusal(qualified_id, str(error))

    @staticmethod
    def v2_join(value: Mapping[str, Any]) -> dict[str, Any]:
        required = {"candidate", "plan", "receipts", "reviews", "dispositions", "observed_budget", "inventory_ref", "inventory", "branches"}
        _require(isinstance(value, Mapping) and set(value) == required, "v2 join inputs are incomplete")
        aggregate = ReceiptAggregator().aggregate(value["candidate"], value["plan"], value["receipts"])
        reviews = copy.deepcopy(value["reviews"])
        _require(all(isinstance(review, Mapping) and review.get("aggregate_digest") == aggregate["aggregate_digest"] for review in reviews), "v2 join review aggregate is not the immutable computed aggregate")
        dispositions = FindingValidator().validate(reviews, value["dispositions"], value["observed_budget"])
        finalized = EvidenceFinalizer().finalize(value["candidate"], aggregate, dispositions, value["inventory_ref"], value["inventory"], value["branches"])
        return {"aggregate": aggregate, "dispositions": dispositions, "finalization": finalized}

    def _1(self, v: Mapping[str, Any], a: Mapping[str, Any]) -> dict[str, Any]:
        checks = v.get("readiness_checks")
        _require(isinstance(checks, Mapping) and set(checks) == _E1_CHECKS and all(item is True for item in checks.values()), "preflight readiness check set is not exact and closed")
        _require(not v.get("open_blockers") and not v.get("resource_conflict") and not v.get("terminal"), "preflight contains a durable stop")
        refs = v.get("physical_refs")
        _require(isinstance(refs, list) and refs, "E1 physical references are missing")
        for ref in refs:
            _require(isinstance(ref, Mapping) and set(ref) == {"kind", "path", "version", "digest", "creator", "revision", "fresh"}, "E1 physical reference is malformed")
            _require(ref["kind"] in _E1_KINDS and isinstance(ref["path"], str) and ref["path"].startswith("agent-workflows/") and posixpath.normpath(ref["path"]) == ref["path"] and ".." not in ref["path"].split("/") and isinstance(ref["version"], str) and ref["version"] and isinstance(ref["creator"], str) and ref["creator"] and ref["fresh"] is True and isinstance(ref["revision"], int) and not isinstance(ref["revision"], bool) and ref["revision"] >= 0, "E1 physical reference is stale")
            _require(ref["digest"] == _raw_digest(ref["path"]), "E1 physical reference digest drifted")
        _require({ref["kind"] for ref in refs} == _E1_KINDS and len(refs) == 4, "E1 must bind exact readiness, DAG, workspace, and Git receipts")
        return {"stage": "E1", "status": "ready", "receipt": {"authority": a["authority_id"], "lease": a["lease_id"], "budget_seconds": a["budget"]["seconds"]}}

    def _2(self, v: Mapping[str, Any], a: Mapping[str, Any]) -> dict[str, Any]:
        preflight = v.get("preflight")
        _require(isinstance(preflight, Mapping) and preflight.get("stage") == "E1" and preflight.get("status") == "ready", "E2 requires exact passing E1")
        _require(preflight.get("qualified_id") == "group.E.E1" and preflight.get("expected_head") == v["expected_head"] and preflight.get("authority_ref") == {"authority_id": a["authority_id"], "lease_id": a["lease_id"], "epoch_id": a["epoch_id"]}, "E2 preflight authority or HEAD is forged")
        _require(_candidate_digest(preflight) == preflight.get("candidate_digest") and v.get("preflight_digest") == preflight.get("candidate_digest"), "E2 preflight digest is forged")
        _require(v.get("assigned_role") == "worker" and isinstance(v.get("assigned_worker"), str) and v["assigned_worker"] and isinstance(v.get("output_path"), str) and v["output_path"], "worker package is not singular")
        for field in ("write_scope", "non_goals", "acceptance", "stop_conditions"):
            _require(isinstance(v.get(field), list) and v[field], "E2 package %s is missing" % field)
        source = v.get("execution_package_input")
        _require(isinstance(source, Mapping), "E2 requires a physical execution package input")
        closure = ExecutionClosureBuilder().freeze(source)
        return {"stage": "E2", "status": "issued", "package": {"model": "gpt-5.6-luna", "effort": "maximum", "assigned_worker": v["assigned_worker"], "output_path": v["output_path"], "loop_level": v["loop_level"], "write_scope": sorted(v["write_scope"]), "non_goals": sorted(v["non_goals"]), "acceptance": sorted(v["acceptance"]), "stop_conditions": sorted(v["stop_conditions"]), "supervision": "bounded", "idempotency_key": a["idempotency_key"], "execution_closure": closure, "execution_closure_ref": {"id": closure["package_id"], "digest": closure["closure_digest"]}}}

    def _3(self, v: Mapping[str, Any], a: Mapping[str, Any]) -> dict[str, Any]:
        package = v.get("package"); changed = v.get("changed_paths", [])
        _require(isinstance(package, Mapping) and package.get("model") == "gpt-5.6-luna" and package.get("idempotency_key") == a["idempotency_key"], "E3 requires immutable E2 package")
        closure = package.get("execution_closure"); closure_ref = package.get("execution_closure_ref")
        _require(isinstance(closure, Mapping) and isinstance(closure_ref, Mapping) and ExecutionClosureBuilder().freeze({**{key: copy.deepcopy(item) for key, item in closure.items() if key not in {"schema", "closure_digest"}}, "schema": "execution-package-input/v2"}) == closure and closure_ref == {"id": closure["package_id"], "digest": closure["closure_digest"]}, "E3 package closure identity drifted")
        _require(v.get("terminal") in {"DONE", "DONE_WITH_CONCERNS", "NEEDS_CONTEXT", "BLOCKED"}, "E3 terminal status is undeclared")
        scope = package.get("write_scope", [])
        _require(isinstance(scope, list) and isinstance(changed, list) and all(any(p == root or p.startswith(root + "/") for root in scope) for p in changed), "worker wrote outside authorized scope")
        if v.get("retry"):
            _require(isinstance(v.get("changed_factor"), str) and v["changed_factor"], "retry lacks a materially changed factor")
        recovery = v.get("recovery_receipt")
        keys = {"schema", "terminal_reused", "spawn_count", "idempotency_key", "closure_digest", "terminal_receipt", "evidence_digest"}
        _require(isinstance(recovery, Mapping) and set(recovery) == keys and recovery.get("schema") == "e3-recovery-evidence/v1" and recovery.get("terminal_reused") is True and recovery.get("spawn_count") == 1 and recovery.get("idempotency_key") == a["idempotency_key"] and recovery.get("closure_digest") == closure["closure_digest"] and _digest({key: copy.deepcopy(item) for key, item in recovery.items() if key != "evidence_digest"}) == recovery["evidence_digest"], "E3 recovery evidence is forged")
        receipt = recovery["terminal_receipt"]
        _require(isinstance(receipt, Mapping) and _digest({key: copy.deepcopy(item) for key, item in receipt.items() if key != "receipt_digest"}) == receipt.get("receipt_digest") and receipt.get("candidate_digest") == closure["candidate_ref"]["digest"] and receipt.get("execution_closure_digest") == closure["closure_digest"] and receipt.get("idempotency_key") == a["idempotency_key"] and v.get("receipt") == receipt, "E3 terminal receipt does not bind E2 package")
        task = {"task_id": v.get("task_id", "task"), "candidate_digest": closure["candidate_ref"]["digest"], "receipt_digest": receipt["receipt_digest"], "changed_paths": changed, "terminal": v["terminal"]}
        candidate = ArtifactCandidateBuilder().build(task, receipt)
        return {"stage": "E3", "status": "submitted", "artifact_candidate": candidate, "recovery": copy.deepcopy(dict(recovery))}

    def _4(self, v: Mapping[str, Any], a: Mapping[str, Any]) -> dict[str, Any]: return self._review(v, a, "architecture-safety", "E4")
    def _5(self, v: Mapping[str, Any], a: Mapping[str, Any]) -> dict[str, Any]: return self._review(v, a, "integration-operability", "E5")

    def _review(self, v: Mapping[str, Any], a: Mapping[str, Any], axis: str, stage: str) -> dict[str, Any]:
        actors = [v.get("actor_id"), v.get("worker_actor_id"), v.get("other_reviewer_actor_id")]
        epochs = [v.get("reviewer_epoch_id"), v.get("other_reviewer_epoch_id"), a["epoch_id"]]
        _require(v.get("axis") == axis and all(isinstance(item, str) and item for item in actors) and len(set(actors)) == len(actors), "review actors are not fresh and distinct")
        _require(all(isinstance(item, str) and item for item in epochs) and len(set(epochs)) == len(epochs), "review Epochs are not fresh and distinct")
        binding = v.get("frozen_binding")
        _require(isinstance(binding, Mapping) and set(binding) == {"candidate_digest", "aggregate_digest", "spec_digest", "closure_digest", "budget_digest"}, "review does not bind the frozen candidate, aggregate, spec, closure, and budget")
        for name, value in binding.items(): _digest_value(value, "review " + name)
        _require(isinstance(v.get("findings"), list), "review findings are malformed")
        required = {"finding_id", "fingerprint", "background", "as_is", "to_be", "gap", "requirement_refs", "evidence_refs", "severity", "blocking_proposal", "owner_proposal"}
        for finding in v["findings"]:
            _require(isinstance(finding, Mapping) and set(finding) == required, "review finding lacks detailed contract fields")
            _require(isinstance(finding["finding_id"], str) and isinstance(finding["fingerprint"], str) and all(isinstance(finding[key], str) and finding[key] for key in ("background", "as_is", "to_be", "gap", "severity", "blocking_proposal", "owner_proposal")), "review finding content is invalid")
            _require(isinstance(finding["requirement_refs"], list) and finding["requirement_refs"] and isinstance(finding["evidence_refs"], list) and finding["evidence_refs"], "review finding references are incomplete")
        report = {"axis": axis, "actor_id": v["actor_id"], "reviewer_epoch_id": v["reviewer_epoch_id"], "binding": copy.deepcopy(binding), "findings": copy.deepcopy(v["findings"])}
        return {"stage": stage, "status": "reviewed", "report": report, "report_digest": _digest(report), "findings": copy.deepcopy(v["findings"])}

    def _6(self, v: Mapping[str, Any], a: Mapping[str, Any]) -> dict[str, Any]:
        reviews, dispositions, budget = v.get("reviews"), v.get("dispositions"), v.get("observed_budget")
        validation = FindingValidator().validate(reviews, dispositions, budget)
        classes = {item["classification"] for item in validation["dispositions"]}
        route = "E7" if "required" in classes else "stop-needs-user" if "needs-user" in classes else "E8"
        return {"stage": "E6", "status": "validated", "validation": validation, "next": route}

    def _7(self, v: Mapping[str, Any], a: Mapping[str, Any]) -> dict[str, Any]:
        _require(v.get("orchestrator_validated") is True and v.get("advice") == "required", "E7 cannot start without orchestrator-validated required advice")
        _require(a["budget"]["review_round"] < 2 and a["budget"]["product_fix_attempts"] < 5, "E7 bounded repair budget is exhausted")
        _require(isinstance(v.get("orchestrator_command"), Mapping) and isinstance(v.get("observed_state"), Mapping), "E7 requires the physical orchestrator command and observed state")
        command, state = v["orchestrator_command"], v["observed_state"]
        _require(state.get("current_head") == v["expected_head"] and command.get("expected_head") == v["expected_head"], "E7 inner current or command HEAD differs from outer HEAD")
        lease = state.get("active_lease", {})
        _require(lease.get("lease_id") == a["lease_id"] and command.get("lease_id") == a["lease_id"] and lease.get("holder_assignment_id") == a["assignment_id"] and command.get("actor", {}).get("assignment_id") == a["assignment_id"], "E7 inner lease or assignment differs from outer authority")
        advisory = state.get("advisory_disposition", {})
        _require(any(item.get("classification") == "required" for item in advisory.get("dispositions", [])) and command.get("source_ref", {}).get("digest") == advisory.get("disposition_digest"), "E7 command lacks required advisory source binding")
        transition = DAGOrchestrator().compile(v["orchestrator_command"], v["observed_state"])
        _require(transition.get("schema") in {"orchestrator-transition/v2", "stopped-budget/v1"}, "E7 delegated transition is invalid")
        return {"stage": "E7", "status": "rereview-required" if transition.get("kind") == "fix-dispatch" else "stopped-budget", "next": "E6" if transition.get("kind") == "fix-dispatch" else "E10", "worker_self_close": False, "orchestrator_transition": transition}

    def _8(self, v: Mapping[str, Any], a: Mapping[str, Any]) -> dict[str, Any]:
        _require(v.get("complete") is True and not v.get("conflicting") and not v.get("open_required"), "E8 rejects incomplete, conflicting, or open-required joins")
        joined = self.v2_join(v.get("v2_join"))
        refs = v.get("sibling_refs")
        expected = [{"id": item["terminal_ref"]["id"], "digest": item["terminal_ref"]["digest"], "status": "accepted"} for item in joined["finalization"]["branches"]]
        _require(isinstance(refs, list) and sorted(refs, key=lambda item: item.get("id", "")) == sorted(expected, key=lambda item: item["id"]), "E8 sibling refs are not the exact finalization branches")
        return {"stage": "E8", "status": "converged", "head_advanced": False, "aggregate": joined["aggregate"], "finalization": joined["finalization"]}

    def _9(self, v: Mapping[str, Any], a: Mapping[str, Any]) -> dict[str, Any]:
        _require(v.get("complete") is True and not v.get("open_required"), "E9 rejects open required findings")
        _require(v.get("budget_exhausted") is not True, "E9 budget exhaustion is a stop")
        inventory = v.get("verification_receipts")
        _require(isinstance(inventory, Mapping) and set(inventory) == {"integration", "e2e", "regression", "objective"}, "E9 verification inventory is incomplete")
        candidate_digest = _digest_value(v.get("candidate_digest"), "E9 candidate digest")
        closure_digest = _digest_value(v.get("closure_digest"), "E9 closure digest")
        for name, receipt in inventory.items():
            keys = {"schema", "category", "candidate_digest", "closure_digest", "command", "environment", "capture", "terminal", "status", "receipt_digest"}
            _require(isinstance(receipt, Mapping) and set(receipt) == keys and receipt.get("schema") == "whole-verification-receipt/v1" and receipt.get("category") == name and receipt.get("candidate_digest") == candidate_digest and receipt.get("closure_digest") == closure_digest and isinstance(receipt.get("command"), Mapping) and isinstance(receipt.get("environment"), Mapping) and isinstance(receipt.get("capture"), Mapping) and receipt.get("terminal") is True and receipt.get("status") == "passed", "E9 %s receipt identity is invalid" % name)
            _require(_digest({key: copy.deepcopy(item) for key, item in receipt.items() if key != "receipt_digest"}) == receipt["receipt_digest"], "E9 %s receipt digest is forged" % name)
        return {"stage": "E9", "status": "verified", "new_findings_route": "E6"}

    def _10(self, v: Mapping[str, Any], a: Mapping[str, Any]) -> dict[str, Any]:
        terminal, terminal_ref = v.get("terminal"), v.get("terminal_ref")
        _require(v.get("normal_loop_terminal") is True and v.get("exception") is True and isinstance(terminal, Mapping), "E10 is exception-only after a physical normal-loop terminal")
        required = {"terminal_id", "reason", "type", "expected_head", "authority_ref", "terminal_digest"}
        _require(set(terminal) == required and terminal.get("expected_head") == v["expected_head"] and terminal.get("authority_ref") == {"authority_id": a["authority_id"], "lease_id": a["lease_id"], "assignment_id": a["assignment_id"]}, "E10 terminal authority or HEAD is invalid")
        _require(_digest({key: copy.deepcopy(item) for key, item in terminal.items() if key != "terminal_digest"}) == terminal["terminal_digest"] and terminal_ref == {"id": terminal["terminal_id"], "digest": terminal["terminal_digest"]}, "E10 terminal reference is forged")
        options = v.get("options")
        _require(isinstance(options, list) and options and all(isinstance(item, Mapping) and set(item) == {"id", "tradeoff", "next_action", "human_required"} and isinstance(item["id"], str) and isinstance(item["tradeoff"], str) and isinstance(item["next_action"], str) and isinstance(item["human_required"], bool) for item in options) and v.get("human_gate") is True and v.get("reopen_authorized") is not True, "E10 options, human gate, or reopen boundary is invalid")
        model = v.get("model_route", {"model": "gpt-5.6-luna", "effort": "maximum"})
        _require(model == {"model": "gpt-5.6-luna", "effort": "maximum"} or (model == {"model": "gpt-5.6-sol", "effort": "high"} and v.get("escalation_authorized") is True and isinstance(v.get("prior_failure_evidence"), str) and v["prior_failure_evidence"]), "E10 model escalation is unauthorized")
        return {"stage": "E10", "status": "advisory-options", "external_authority": False, "model_route": model}

    @staticmethod
    def invalidate(changed_id: str, graph: Mapping[str, Sequence[str]], accepted: Sequence[str], late_result: Mapping[str, Any]) -> dict[str, Any]:
        _require(changed_id in graph, "changed dependency is unknown")
        affected, pending = {changed_id}, [changed_id]
        while pending:
            current = pending.pop()
            for node, dependencies in graph.items():
                if current in dependencies and node not in affected:
                    affected.add(node); pending.append(node)
        _require(late_result.get("bound_input_digest") != late_result.get("current_input_digest"), "late result is not stale")
        return {"invalidated": sorted(affected), "preserved": sorted(set(accepted) - affected), "late_result": "rejected", "non_mutating": True}


__all__ = ["ArtifactCandidateBuilder", "ExecutionGroupRefusal", "ExecutionGroupV1"]
