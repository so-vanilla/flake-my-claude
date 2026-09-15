"""Pure source backing for the Group E execution Skills.

This is deliberately a compiler, not an execution engine.  It binds physical
inputs and delegates the narrow deep seams to Workflow Execution V2.
"""
from __future__ import annotations

import copy
import hashlib
import json
import os
import re
import posixpath
import stat
from collections.abc import Mapping, Sequence
from pathlib import Path
from typing import Any

from .bounded_read_scope import (ReadScopeError, compile_read_scope,
                                 verify_read_scope_receipt)
from .execution_v2 import (EvidenceFinalizer, ExecutionClosureBuilder, FindingValidator,
                           MechanicalCompletion, ReceiptAggregator, V2ContractError,
                           WorkflowLoopValidator)
from .execution_v2_orchestrator import DAGOrchestrator, OrchestratorContractError
from .macos_task_process import (MacOSTaskProcessBroker,
                                 compile_macos_task_process_release)
from .persistent_receipts import (DeterministicProcessAdapter, DuplicateReceiptConflict,
                                  PersistentReceiptError, PersistentReceiptRunner)
from .review_packages import ReviewPackageError, accept_review_result

_ROOT = Path(__file__).resolve().parents[2]
_POLICY = "agent-workflows/groups/required-only-feedback-execution-policy-v1.json"
_STAGES = {"group.E.E%d" % number for number in range(1, 11)}
_LEVELS = {"artifact", "section", "workflow"}
_SHA256 = re.compile(r"^sha256:[0-9a-f]{64}$")
_E1_KINDS = {"readiness-approval", "task-dag", "workspace-receipt", "git-state-receipt"}
_E1_CHECKS = {"authority", "expected_head", "lease", "dependencies", "conflicts", "blockers", "purpose", "budget", "workspace", "git"}
_DECLARED_RUNTIME_SOURCES = {
    "agent-workflows/skills/implementation-readiness-review/SKILL.md",
    "agent-workflows/groups/planning.json",
    "agent-workflows/catalog.yaml",
    _POLICY,
}
_PRACTICAL_APPROVAL_DIGEST = "sha256:2f0ae19fac7b90778d1b5713c51071f1ab7e96ce179237a1e5892cf320dfaefb"
_PRACTICAL_RESIDUALS = [
    "a malicious external process running as the same login UID may rename a project ancestor",
    "SIGKILL of the trusted parent after a publication syscall and before bookkeeping may leave bytes in a detached tree",
]
_CALLER_SAFETY_FIELDS = {
    "threat_profile", "threat_profile_digest", "state_writer_boundary",
    "limitations_acknowledged", "practical_profile_accepted",
    "arbitrary_same_uid_atomicity", "publication_sigkill_atomicity",
}


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


def _regular_contained(project: Path, relative: str) -> Path:
    candidate = project / relative
    try:
        if candidate.resolve(strict=True) != candidate or not candidate.is_file():
            raise ExecutionGroupRefusal("installed source is not one contained regular file: " + relative)
        current = project
        for part in Path(relative).parts:
            current = current / part
            if stat.S_ISLNK(os.lstat(current).st_mode):
                raise ExecutionGroupRefusal("installed source follows a symlink: " + relative)
    except OSError as error:
        raise ExecutionGroupRefusal("installed source is unavailable: " + relative) from error
    return candidate


def _declared_source_path(path: str) -> Path:
    """Resolve only fixed E contract bytes from checkout or installed snapshot."""
    if path not in _DECLARED_RUNTIME_SOURCES:
        raise ExecutionGroupRefusal("undeclared runtime source path: " + str(path))
    installed_layout = (_ROOT.name == "agent-workflows" and _ROOT.parent.name == "runtime"
                        and _ROOT.parent.parent.name == ".agent-workflow")
    checkout = _ROOT.parent / path
    if not installed_layout:
        try:
            if checkout.resolve(strict=True) != checkout or not checkout.is_file():
                raise ExecutionGroupRefusal("declared checkout source is not one regular file: " + path)
        except OSError as error:
            raise ExecutionGroupRefusal("physical input is unavailable: " + path) from error
        return checkout

    project = _ROOT.parent.parent.parent
    destination = (".agents/skills/" + path.removeprefix("agent-workflows/skills/")
                   if path.startswith("agent-workflows/skills/")
                   else ".agent-workflow/runtime/" + path)
    manifest_path = _regular_contained(project, ".agent-workflow/SNAPSHOT-MANIFEST.json")
    try:
        manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
        unsigned = dict(manifest)
        supplied_snapshot_digest = unsigned.pop("snapshot_digest")
        actual_snapshot_digest = "sha256:" + hashlib.sha256(json.dumps(
            unsigned, ensure_ascii=True, sort_keys=True, separators=(",", ":")
        ).encode("utf-8")).hexdigest()
        entry = manifest["files"][destination]
    except (OSError, KeyError, TypeError, json.JSONDecodeError) as error:
        raise ExecutionGroupRefusal("installed snapshot inventory is unavailable for: " + path) from error
    if (not isinstance(entry, Mapping)
            or manifest.get("schema") != "agent-workflow-project-snapshot/v2"
            or supplied_snapshot_digest != actual_snapshot_digest
            or entry != {"source": path, "digest": entry.get("digest"), "mode": "0644"}
            or not re.fullmatch(r"sha256:[0-9a-f]{64}", str(entry.get("digest")))):
        raise ExecutionGroupRefusal("installed snapshot inventory does not bind: " + path)
    physical = _regular_contained(project, destination)
    if ("sha256:" + hashlib.sha256(physical.read_bytes()).hexdigest() != entry["digest"]
            or stat.S_IMODE(os.lstat(physical).st_mode) != 0o644):
        raise ExecutionGroupRefusal("installed snapshot bytes or mode drifted: " + path)
    return physical


def _raw_digest(path: str) -> str:
    try:
        return "sha256:" + hashlib.sha256(_declared_source_path(path).read_bytes()).hexdigest()
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


def _workflow_loop_input(value: Mapping[str, Any]) -> Mapping[str, Any] | None:
    """Return an additive workflow-loop request, if the caller supplied one."""
    for key in ("workflow_loop", "loop_request", "completion_request"):
        if key in value:
            request = value[key]
            _require(isinstance(request, Mapping), "workflow-loop request is malformed")
            return request
    if any(key in value for key in ("review_package", "review_assessment", "review_results")):
        return value
    if value.get("schema") == "workflow-loop/v1":
        return value
    return None


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

    def compile(
        self,
        qualified_id: str,
        inputs: Mapping[str, Any],
        authority: Mapping[str, Any],
        expected_head: Mapping[str, Any],
        *,
        trusted_isolation_broker: MacOSTaskProcessBroker | None = None,
    ) -> dict[str, Any]:
        try:
            value, bound = _common(qualified_id, inputs, authority, expected_head)
            handler = getattr(self, "_" + qualified_id.rsplit("E", 1)[1])
            if qualified_id == "group.E.E3":
                result = handler(value, bound, trusted_isolation_broker)
            else:
                _require(trusted_isolation_broker is None, "trusted isolation broker is valid only at E3")
                result = handler(value, bound)
            result.update({"schema": "execution-group-candidate/v1", "qualified_id": qualified_id, "loop_level": value["loop_level"], "expected_head": copy.deepcopy(expected_head), "authority_ref": {"authority_id": bound["authority_id"], "lease_id": bound["lease_id"], "epoch_id": bound["epoch_id"]}, "non_mutating": True})
            result["candidate_digest"] = _digest(result)
            return result
        except (ExecutionGroupRefusal, ReadScopeError, V2ContractError, OrchestratorContractError, PersistentReceiptError) as error:
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

    @staticmethod
    def validate_workflow_loop(request: Mapping[str, Any], *, receipt_id: str | None = None) -> dict[str, Any]:
        """Compile the additive workflow-loop/v1 review and completion path."""
        return WorkflowLoopValidator().validate(request, receipt_id=receipt_id)

    @staticmethod
    def compile_workflow_loop(request: Mapping[str, Any], *, receipt_id: str | None = None) -> dict[str, Any]:
        """Compatibility name for callers that treat the loop as a compiler."""
        return MechanicalCompletion().evaluate(request, receipt_id=receipt_id)

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
        read_scope = compile_read_scope(closure["workspace_identity"], closure["resource_claims"]["read_paths"])
        mode = v.get("task_process_mode", "unsandboxed-rehearsal")
        _require(mode in {"unsandboxed-rehearsal", "macos-positive-allowlist"}, "E2 task-process mode is invalid")
        _require(not (_CALLER_SAFETY_FIELDS & set(v)), "E2 rejects caller-authored practical safety labels")
        if mode == "macos-positive-allowlist":
            _require(sorted(v["write_scope"]) == closure["resource_claims"]["write_paths"], "sandboxed E2 write scope must equal the execution closure")
            release = compile_macos_task_process_release(
                closure,
                broker_state_root=v.get("broker_state_root"),
                system_read_roots=v.get("system_read_roots"),
                runtime_read_roots=v.get("runtime_read_roots"),
            )
        else:
            _require(not any(key in v for key in ("broker_state_root", "system_read_roots", "runtime_read_roots")), "unsandboxed rehearsal cannot carry a sandbox authority claim")
            release = {
                "schema": "unsandboxed-task-process-release/v1",
                "release_rule": "rehearsal-only",
                "os_isolation_enforced": False,
                "limitation": "no operating-system read isolation; cannot satisfy confidential execution acceptance",
                "execution_closure_ref": {"id": closure["package_id"], "digest": closure["closure_digest"]},
            }
            release["release_digest"] = _digest(release)
        return {"stage": "E2", "status": "issued", "package": {"model": "gpt-5.6-luna", "effort": "maximum", "assigned_worker": v["assigned_worker"], "output_path": v["output_path"], "loop_level": v["loop_level"], "write_scope": sorted(v["write_scope"]), "read_scope": read_scope, "task_process_release": release, "non_goals": sorted(v["non_goals"]), "acceptance": sorted(v["acceptance"]), "stop_conditions": sorted(v["stop_conditions"]), "supervision": "bounded", "idempotency_key": a["idempotency_key"], "execution_closure": closure, "execution_closure_ref": {"id": closure["package_id"], "digest": closure["closure_digest"]}}}

    def _3(
        self,
        v: Mapping[str, Any],
        a: Mapping[str, Any],
        trusted_isolation_broker: MacOSTaskProcessBroker | None,
    ) -> dict[str, Any]:
        package = v.get("package"); changed = v.get("changed_paths", [])
        _require(isinstance(package, Mapping) and package.get("model") == "gpt-5.6-luna" and package.get("idempotency_key") == a["idempotency_key"], "E3 requires immutable E2 package")
        closure = package.get("execution_closure"); closure_ref = package.get("execution_closure_ref")
        _require(isinstance(closure, Mapping) and isinstance(closure_ref, Mapping) and ExecutionClosureBuilder().freeze({**{key: copy.deepcopy(item) for key, item in closure.items() if key not in {"schema", "closure_digest"}}, "schema": "execution-package-input/v2"}) == closure and closure_ref == {"id": closure["package_id"], "digest": closure["closure_digest"]}, "E3 package closure identity drifted")
        expected_read_scope = compile_read_scope(closure["workspace_identity"], closure["resource_claims"]["read_paths"])
        _require(package.get("read_scope") == expected_read_scope, "E3 package read scope drifted")
        release = package.get("task_process_release")
        _require(isinstance(release, Mapping), "E3 task-process release is missing")
        if release.get("schema") == "macos-task-process-release/v2":
            _require(v.get("read_scope_receipt") is None, "E3 rejects caller-authored observations for OS isolation")
            _require(isinstance(trusted_isolation_broker, MacOSTaskProcessBroker), "E3 requires the parent-owned macOS isolation broker")
            read_receipt = trusted_isolation_broker.verify_for_e3(package, v.get("isolation_receipt_ref"))
            profile = read_receipt.get("threat_profile")
            boundary = read_receipt.get("state_writer_boundary")
            _require(
                isinstance(profile, Mapping)
                and set(profile) == {"schema", "profile_id", "approval_digest", "trusted", "adversarial", "accepted_residuals", "guarantees", "not_guaranteed"}
                and profile.get("schema") == "macos-practical-threat-profile/v1"
                and profile.get("profile_id") == "macos-parent-writer-practical/v1"
                and profile.get("approval_digest") == _PRACTICAL_APPROVAL_DIGEST
                and profile.get("accepted_residuals") == _PRACTICAL_RESIDUALS
                and profile.get("not_guaranteed") == _PRACTICAL_RESIDUALS,
                "E3 parent receipt does not bind the approved practical threat profile",
            )
            _require(
                isinstance(boundary, Mapping)
                and set(boundary) == {
                    "writer", "task_child_is_writer", "broker_state_root", "task_write_roots",
                    "task_write_scope_disjoint", "descriptor_anchored", "no_follow",
                    "device_inode_revalidated", "pre_post_attachment_checks", "rollback",
                    "arbitrary_same_uid_atomicity", "publication_sigkill_atomicity",
                }
                and boundary.get("writer") == "trusted-same-uid-parent"
                and boundary.get("task_child_is_writer") is False
                and boundary.get("task_write_scope_disjoint") is True
                and all(boundary.get(key) is True for key in ("descriptor_anchored", "no_follow", "device_inode_revalidated", "pre_post_attachment_checks"))
                and boundary.get("rollback") == "defense-in-depth"
                and boundary.get("arbitrary_same_uid_atomicity") is False
                and boundary.get("publication_sigkill_atomicity") is False
                and read_receipt.get("limitations_acknowledged") is True
                and read_receipt.get("threat_profile_digest") == release.get("threat_profile_digest")
                and read_receipt.get("state_writer_boundary") == release.get("state_writer_boundary"),
                "E3 parent receipt does not bind the practical state-writer boundary and accepted limitations",
            )
            read_isolation_acceptance = True
            isolation_status = "os-enforced"
        elif release.get("schema") == "macos-task-process-release/v1":
            raise ExecutionGroupRefusal("historical macOS v1 evidence is legacy-unprofiled and cannot satisfy practical E3 acceptance")
        else:
            _require(release.get("schema") == "unsandboxed-task-process-release/v1" and release.get("os_isolation_enforced") is False, "E3 task-process release is unrecognized")
            _require(v.get("isolation_receipt_ref") is None and trusted_isolation_broker is None, "unsandboxed rehearsal cannot consume a trusted isolation receipt")
            read_receipt = verify_read_scope_receipt(expected_read_scope, v.get("read_scope_receipt"))
            read_isolation_acceptance = False
            isolation_status = "rehearsal-only"
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
        if read_isolation_acceptance:
            _require(read_receipt.get("terminal_receipt_digest") == receipt.get("receipt_digest"), "trusted isolation receipt does not bind the E3 terminal receipt")
        task = {"task_id": v.get("task_id", "task"), "candidate_digest": closure["candidate_ref"]["digest"], "receipt_digest": receipt["receipt_digest"], "changed_paths": changed, "terminal": v["terminal"]}
        candidate = ArtifactCandidateBuilder().build(task, receipt)
        candidate["read_scope_receipt"] = read_receipt
        candidate["read_isolation_acceptance"] = read_isolation_acceptance
        candidate["isolation_status"] = isolation_status
        candidate["artifact_digest"] = _digest({key: copy.deepcopy(item) for key, item in candidate.items() if key != "artifact_digest"})
        return {"stage": "E3", "status": "submitted", "artifact_candidate": candidate, "recovery": copy.deepcopy(dict(recovery))}

    def _4(self, v: Mapping[str, Any], a: Mapping[str, Any]) -> dict[str, Any]: return self._review(v, a, "architecture-safety", "E4")
    def _5(self, v: Mapping[str, Any], a: Mapping[str, Any]) -> dict[str, Any]: return self._review(v, a, "integration-operability", "E5")

    def _review(self, v: Mapping[str, Any], a: Mapping[str, Any], axis: str, stage: str) -> dict[str, Any]:
        loop_request = _workflow_loop_input(v)
        if loop_request is not None:
            return self._workflow_loop_review(v, loop_request, axis, stage)
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

    @staticmethod
    def _workflow_loop_review(value: Mapping[str, Any], request: Mapping[str, Any], axis: str, stage: str) -> dict[str, Any]:
        review = request.get("review", request.get("review_assessment", request))
        package = request.get("review_package", request.get("package"))
        _require(isinstance(package, Mapping), "workflow-loop review package is missing")
        try:
            accepted = accept_review_result(package, review)
        except ReviewPackageError as error:
            raise ExecutionGroupRefusal(str(error)) from error
        _require(accepted["axis"] == axis, "workflow-loop review axis does not match the Group E stage")
        _require(accepted["completed"] is True, "workflow-loop review is incomplete")
        report = {
            "schema": "workflow-loop-review-report/v1",
            "axis": axis,
            "actor_id": accepted["actor_id"],
            "context_epoch": accepted["context_epoch"],
            "candidate_digest": accepted["candidate_digest"],
            "package_digest": accepted["package_digest"],
            "coverage": copy.deepcopy(accepted["coverage"]),
            "unevaluated": copy.deepcopy(accepted["unevaluated"]),
            "finding_refs": copy.deepcopy(accepted["finding_refs"]),
        }
        return {
            "stage": stage,
            "status": "reviewed",
            "report": report,
            "report_digest": _digest(report),
            "findings": copy.deepcopy(accepted["finding_refs"]),
        }

    def _6(self, v: Mapping[str, Any], a: Mapping[str, Any]) -> dict[str, Any]:
        loop_request = _workflow_loop_input(v)
        if loop_request is not None:
            validation = WorkflowLoopValidator().validate(loop_request, receipt_id=v.get("receipt_id"))
            if validation.get("repair_batch_plan"):
                route = "E7"
            elif validation.get("validator", {}).get("skipped") is True:
                route = "E8"
            else:
                route = "E6"
            return {"stage": "E6", "status": "validated", "validation": validation, "next": route}
        reviews, dispositions, budget = v.get("reviews"), v.get("dispositions"), v.get("observed_budget")
        validation = FindingValidator().validate(reviews, dispositions, budget)
        classes = {item["classification"] for item in validation["dispositions"]}
        route = "E7" if "required" in classes else "stop-needs-user" if "needs-user" in classes else "E8"
        return {"stage": "E6", "status": "validated", "validation": validation, "next": route}

    def _7(self, v: Mapping[str, Any], a: Mapping[str, Any]) -> dict[str, Any]:
        loop_request = _workflow_loop_input(v)
        if loop_request is not None:
            validation = WorkflowLoopValidator().validate(loop_request, receipt_id=v.get("receipt_id"))
            _require(isinstance(validation.get("repair_batch_plan"), Mapping), "workflow-loop E7 requires required Finding repair batches")
            transition = {
                "schema": "workflow-loop-repair/v1",
                "candidate_digest": validation["candidate_digest"],
                "package_digest": validation["package_digest"],
                "repair_batch_plan": copy.deepcopy(validation["repair_batch_plan"]),
                "delta_review_packages": copy.deepcopy(validation.get("delta_review_packages", [])),
                "non_authorizing": True,
            }
            return {"stage": "E7", "status": "rereview-required", "next": "E6", "worker_self_close": False, "orchestrator_transition": transition}
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
        loop_request = _workflow_loop_input(v)
        if loop_request is not None:
            validation = WorkflowLoopValidator().validate(loop_request, receipt_id=v.get("receipt_id"))
            _require(validation.get("validator", {}).get("skipped") is True and validation.get("next") == "complete", "E8 requires strict mechanical completion")
            aggregate = {
                "schema": "workflow-loop-aggregate/v1",
                "candidate_digest": validation["candidate_digest"],
                "package_digest": validation["package_digest"],
                "classification": copy.deepcopy(validation["classification"]),
                "machine_decision_receipt": copy.deepcopy(validation["machine_decision_receipt"]),
                "non_authorizing": True,
            }
            aggregate["aggregate_digest"] = _digest(aggregate)
            finalization = {
                "schema": "workflow-loop-finalization/v1",
                "candidate_digest": validation["candidate_digest"],
                "aggregate_digest": aggregate["aggregate_digest"],
                "completed": True,
                "non_authorizing": True,
            }
            finalization["finalization_digest"] = _digest(finalization)
            return {
                "stage": "E8",
                "status": "converged",
                "head_advanced": False,
                "aggregate": aggregate,
                "finalization": finalization,
                "validation": validation,
            }
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
