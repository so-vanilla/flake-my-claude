"""Post-E6 practical repair protocol for one persisted Group E Finding.

The Codex host and the parent Orchestrator are trusted by the approved
``practical/trusted-parent/v1`` profile.  This module proves causal and
content consistency inside the current Run; it intentionally does not claim
cryptographic authentication of AI principals.
"""
from __future__ import annotations

import copy
import hashlib
import json
import re
import secrets
import subprocess
from collections.abc import Mapping, Sequence
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

from .completion import (
    CompletionError,
    classify_completion,
    create_machine_decision_receipt,
)
from .evidence_validity import EvidenceValidityError, assess_evidence
from .execution_v2 import ExecutionClosureBuilder
from .inception_cli import InceptionError
from .inception_runtime import InceptionRuntime
from .loop_contracts import (
    LOOP_CONTRACT_VERSION,
    REQUIRED_REVIEW_AXES,
    LoopContractError,
    canonical_digest,
    require_digest,
    validate_iteration_event,
    validate_ref,
    validate_work_identity,
)
from .loop_policy import LoopPolicyError, phase_limits
from .loop_state import build_iteration_event, event_ref
from .repair_batch import RepairBatchError, assess_batch_resolution, plan_fix_batches
from .review_packages import (
    ReviewPackageError,
    accept_review_result,
    build_review_package,
)
from .runtime_execution import (  # noqa: F401
    RuntimeExecution,
    RuntimeExecutionError,
    _digest,
    _head,
)

# Keep the historical module attributes available to callers that imported
# the runtime execution helpers through this module, even though the v1
# adapter itself talks directly to the Kernel.
TRUST_PROFILE = "practical/trusted-parent/v1"
ACCEPTED_RESIDUAL = (
    "the trusted Codex host or parent Orchestrator could misrepresent role identity; "
    "runtime detects causal and consistency violations but does not cryptographically authenticate AI principals"
)
_ATTEMPT = re.compile(r"^e7-[0-9a-f]{32}$")
_NONCE = re.compile(r"^[0-9a-f]{64}$")
_STATE_ID = re.compile(r"^repair-e7-(e7-[0-9a-f]{32})-s([0-9]{2})$")
_AXES = ("architecture/safety", "integration/operability/time/dotfiles")
_NEXT = {
    "attempt-issued": "worker",
    "worker-accepted": "focused",
    "focused-accepted": "reviews-issue",
    "reviews-issued": "reviews-accept",
    "review-one-accepted": "reviews-accept",
    "reviews-accepted": "validator-issue",
    "validator-issued": "validator-accept",
    "validator-accepted": "whole",
    "whole-accepted": "finalize",
    "finalizing": "finalize",
    "closed": None,
    "expired": None,
    "required-remains": None,
    "attempts-exhausted": None,
    "invalidated": None,
    "terminal-integrity": None,
}
_TERMINAL = {"expired", "required-remains", "attempts-exhausted", "invalidated", "terminal-integrity"}
_CAS = {"attempt_id", "nonce", "previous_state_ref", "expected_head"}


class RuntimeRepairError(InceptionError):
    """A practical repair transition was refused without inferred success."""


def _execution_claim(mode: str) -> dict[str, Any]:
    authoritative = mode == "real"
    return {
        "mode": mode,
        "authoritative": authoritative,
        "production_closure": authoritative,
    }


def assert_production_adoptable(
    final_record: Mapping[str, Any], runtime_identity: Mapping[str, Any]
) -> None:
    """Fail closed unless a repair final record is production-authoritative.

    A rehearsal may close its own isolated Run.  This separate boundary must
    be called before any consumer adopts that closure as production evidence.
    """
    if not isinstance(final_record, Mapping) or final_record.get("schema") != "runtime-practical-repair-final/v1":
        raise RuntimeRepairError("production adoption requires a practical repair final record")
    if not isinstance(runtime_identity, Mapping) or runtime_identity.get("mode") != "real":
        raise RuntimeRepairError("only a real Run identity may adopt repair completion into production")
    if final_record.get("runtime_identity") != runtime_identity:
        raise RuntimeRepairError("repair final record does not bind the production Run identity")
    if final_record.get("execution_claim") != _execution_claim("real"):
        raise RuntimeRepairError("non-authoritative repair completion cannot be adopted into production")


def _utc(value: str, label: str) -> datetime:
    try:
        parsed = datetime.fromisoformat(value.replace("Z", "+00:00"))
    except (AttributeError, ValueError) as error:
        raise RuntimeRepairError(label + " must be an ISO-8601 timestamp") from error
    if parsed.tzinfo is None:
        raise RuntimeRepairError(label + " must be timezone-aware")
    return parsed.astimezone(timezone.utc)


class RuntimeRepairCoordinator:
    """Resume and advance one practical repair from immutable Run state."""

    def __init__(self, project: str | Path, run_id: str) -> None:
        self.runtime = InceptionRuntime(project, run_id)
        self.kernel = self.runtime.kernel
        self.project = self.runtime.project
        self.run_id = run_id

    def workflow_loop(self):
        """Return the additive workflow-loop/v1 repair adapter.

        The practical E7 methods remain unchanged; callers opt in explicitly
        to the Kernel loop-control protocol through this factory.
        """

        return WorkflowLoopRepairCoordinator(runtime=self.runtime)

    # -- public discovery -------------------------------------------------

    def status(self, task_id=None, finding_id=None):
        attempts = self._attempts(task_id=task_id, finding_id=finding_id)
        if not attempts:
            state = self.kernel.read_state()
            initial = state.get("artifacts", {}).get("execution-E6-initial")
            findings = [
                item for item in state.get("findings", {}).values()
                if item.get("admitted") is True and item.get("validation_disposition") == "required"
                and item.get("closure_ref") is None
                and (task_id is None or item.get("candidate_task_id") == task_id)
                and (finding_id is None or item.get("finding_id") == finding_id)
            ]
            if initial is not None and len(findings) == 1:
                return self._result("required-validated", None, None, "begin", None)
            return self._result("not-ready", None, None, None, "no single persisted required Finding frontier")
        active = [item for item in attempts if item["value"]["state"] not in _TERMINAL | {"closed"}]
        if len(active) > 1:
            raise RuntimeRepairError("more than one active repair attempt exists for the selected Finding")
        chosen = (active or attempts)[-1]
        value = chosen["value"]
        terminal_reason = value.get("terminal_reason")
        if value["state"] not in _TERMINAL | {"closed"} and datetime.now(timezone.utc) >= _utc(value["deadline"], "deadline"):
            return self._result("expired", value["attempt_id"], chosen["ref"], None, "repair deadline expired")
        return self._result(value["state"], value["attempt_id"], chosen["ref"], value["next_action"], terminal_reason)

    # -- begin ------------------------------------------------------------

    def begin(self, inputs):
        allowed = {
            "task_id", "finding_id", "initial_e6_ref", "expected_head", "deadline",
            "worker_assignment", "focused_plan_ref", "whole_plan_ref",
        }
        self._closed_inputs(inputs, allowed, {"task_id", "finding_id", "initial_e6_ref", "expected_head", "worker_assignment", "focused_plan_ref", "whole_plan_ref"}, "begin")
        request_digest = _digest(inputs)
        for item in self._attempts(task_id=inputs["task_id"], finding_id=inputs["finding_id"]):
            if item["value"].get("accepted_refs", {}).get("begin_request_digest") == request_digest:
                value = item["value"]
                return self._result(value["state"], value["attempt_id"], item["ref"], value["next_action"], value.get("terminal_reason"))
        self._expect_head(inputs["expected_head"])
        initial = self._kernel_value(inputs["initial_e6_ref"], "initial E6")
        if initial.get("schema") != "runtime-execution-step/v2" or initial.get("record_id") != "E6-initial":
            raise RuntimeRepairError("begin requires the canonical execution-E6-initial record")
        state = self.kernel.read_state()
        finding = state.get("findings", {}).get(inputs["finding_id"])
        if not isinstance(finding, Mapping) or finding.get("candidate_task_id") != inputs["task_id"]:
            raise RuntimeRepairError("begin Finding does not belong to the selected Task")
        if finding.get("admitted") is not True or finding.get("validation_disposition") != "required" or finding.get("closure_ref") is not None:
            raise RuntimeRepairError("begin requires one open native required Finding")
        existing = [item for item in self._attempts(finding_id=inputs["finding_id"]) if item["value"]["state"] not in _TERMINAL | {"closed"}]
        if existing:
            raise RuntimeRepairError("an active repair attempt already exists for this Finding")
        initial_refs = initial.get("inputs", {}).get("native_kernel_refs", {})
        validation_refs = initial_refs.get("joined_validation_refs")
        if not isinstance(validation_refs, list) or not validation_refs:
            raise RuntimeRepairError("initial E6 does not bind native validation refs")
        prior = initial.get("inputs", {}).get("prior_stage_refs", {})
        e2 = self._kernel_value(prior.get("E2"), "initial E2")
        e3 = self._kernel_value(prior.get("E3"), "initial E3")
        closure = e2.get("compiled", {}).get("package", {}).get("execution_closure")
        candidate = e3.get("compiled", {}).get("artifact_candidate")
        if not isinstance(closure, Mapping) or not isinstance(candidate, Mapping):
            raise RuntimeRepairError("initial E6 lineage lacks candidate or closure")
        scope = finding.get("permitted_fix_scope")
        if not isinstance(scope, list) or not scope:
            validation = initial.get("compiled", {}).get("validation", {})
            roots = [item for item in validation.get("dispositions", []) if item.get("classification") == "required"]
            if len(roots) != 1:
                raise RuntimeRepairError("begin supports exactly one admitted required root")
            scope = roots[0].get("proposed_scope")
        scope = self._normalized_scope(scope)
        git_baseline = self._git_snapshot(scope, "repair baseline")
        worker = self._assignment(inputs["worker_assignment"], "repair-worker", axis=None)
        focused_plan_ref = self._verified_ref(inputs["focused_plan_ref"], "focused plan")
        whole_plan_ref = self._verified_ref(inputs["whole_plan_ref"], "whole plan")
        deadline = _utc(state["review_budget"]["deadline"], "Kernel review deadline")
        if inputs.get("deadline") is not None:
            deadline = min(deadline, _utc(inputs["deadline"], "requested repair deadline"))
        if datetime.now(timezone.utc) >= deadline:
            raise RuntimeRepairError("repair deadline is already exhausted")
        attempt_id = "e7-" + secrets.token_hex(16)
        nonce = secrets.token_hex(32)
        issued_head = copy.deepcopy(inputs["expected_head"])
        role_package = self._role_package(
            attempt_id, nonce, worker, None, issued_head, inputs["finding_id"],
            candidate, closure, scope, deadline, [inputs["initial_e6_ref"], focused_plan_ref, whole_plan_ref],
        )
        worker_ref = self._publish_aux(attempt_id, 0, "begin", "worker-package", role_package, "repair-role-package")
        authority_value = {
            "schema": "repair-attempt-authority/v1", "trust_profile": TRUST_PROFILE,
            "accepted_residual": ACCEPTED_RESIDUAL, "run_id": state["run_id"],
            "task_id": inputs["task_id"], "finding_id": inputs["finding_id"],
            "finding_fingerprint": finding["fingerprint"], "attempt_id": attempt_id,
            "nonce": nonce, "issued_at": datetime.now(timezone.utc).isoformat(),
            "deadline": deadline.isoformat(), "issued_from_head": issued_head,
            "initial_e6_ref": copy.deepcopy(inputs["initial_e6_ref"]),
            "initial_validation_refs": copy.deepcopy(validation_refs),
            "initial_candidate_ref": {"id": candidate.get("candidate_id", "initial-candidate"), "digest": candidate["candidate_digest"]},
            "initial_closure_ref": {"id": "initial-execution-closure", "digest": closure["closure_digest"]},
            "initial_candidate": copy.deepcopy(candidate), "initial_closure": copy.deepcopy(closure),
            "permitted_fix_scope": scope, "worker_package_ref": worker_ref,
            "git_baseline": git_baseline,
            "focused_plan_ref": focused_plan_ref, "whole_plan_ref": whole_plan_ref,
            "review_round": state["review_budget"].get("rounds_used", 0),
            "product_fix_attempt": state["review_budget"].get("finding_attempts", {}).get(inputs["finding_id"], 0) + 1,
            "mode": self.runtime.identity.get("mode", "real"), "worker_assignment": worker,
        }
        authority_ref = self._publish_aux(attempt_id, 0, "begin", "authority", authority_value, "repair-attempt-authority")
        accepted = {
            "begin_request_digest": request_digest, "task_id": inputs["task_id"], "finding_id": inputs["finding_id"],
            "worker_package_ref": worker_ref, "focused_plan_ref": focused_plan_ref, "whole_plan_ref": whole_plan_ref,
            "initial_e6_ref": copy.deepcopy(inputs["initial_e6_ref"]),
        }
        value, ref = self._publish_state(
            attempt_id, nonce, 0, "attempt-issued", None, authority_ref, accepted,
            {"worker": worker}, deadline.isoformat(), authority_value["review_round"],
            authority_value["product_fix_attempt"], "begin",
        )
        return self._result(value["state"], attempt_id, ref, value["next_action"], None)

    # -- bounded transitions ---------------------------------------------

    def accept_worker(self, inputs):
        allowed = _CAS | {"worker_report_ref", "post_fix_candidate_request", "changed_paths", "procedural_role_receipt"}
        self._closed_inputs(inputs, allowed, allowed, "worker")
        replay = self._replay(inputs)
        if replay is not None:
            return replay
        current = self._current(inputs, {"attempt-issued"})
        authority = self._authority_value(current)
        report_ref, report = self._physical_json(inputs["worker_report_ref"], "Worker report")
        role_ref, receipt = self._physical_json(inputs["procedural_role_receipt"], "Worker role receipt")
        self._check_role_receipt(receipt, authority["worker_package_ref"], authority["worker_assignment"], current, report_ref, "repair-worker", None)
        self._bind_report(report, current, authority["worker_package_ref"], authority["worker_assignment"], report_ref)
        paths = self._normalized_scope(inputs["changed_paths"])
        if paths != authority["permitted_fix_scope"]:
            raise RuntimeRepairError("Worker changed paths differ from the exact permitted fix scope")
        if sorted(receipt["observed_changed_paths"]) != paths:
            raise RuntimeRepairError("trusted-parent Worker receipt observed a different write set")
        git_candidate = self._git_snapshot(paths, "post-fix candidate")
        git_baseline = authority.get("git_baseline")
        if not isinstance(git_baseline, Mapping):
            raise RuntimeRepairError("repair authority lacks the Git baseline identity")
        if git_candidate["commit"] == git_baseline.get("commit") or git_candidate["tree"] == git_baseline.get("tree"):
            raise RuntimeRepairError("post-fix Git candidate is unchanged")
        self._git_require_ancestor(git_baseline["commit"], git_candidate["commit"])
        git_changed_paths = self._git_changed_paths(git_baseline["commit"], git_candidate["commit"])
        if git_changed_paths != paths:
            raise RuntimeRepairError("Git candidate changed paths differ from the exact permitted fix scope")
        artifacts = []
        for relative in paths:
            artifacts.append(self._git_artifact(git_candidate["commit"], relative))
        request = inputs["post_fix_candidate_request"]
        if not isinstance(request, Mapping) or set(request) - {"candidate_id"}:
            raise RuntimeRepairError("post-fix candidate request accepts only candidate_id")
        candidate_id = request.get("candidate_id") or current["value"]["attempt_id"] + "-candidate"
        candidate_value = {
            "schema": "runtime-e7-post-fix-candidate/v2", "attempt_id": current["value"]["attempt_id"],
            "finding_id": authority["finding_id"], "candidate_id": candidate_id,
            "prior_candidate_digest": authority["initial_candidate_ref"]["digest"],
            "permitted_fix_scope": paths, "artifacts": artifacts,
            "git_baseline": copy.deepcopy(git_baseline),
            "git_candidate": git_candidate,
            "git_changed_paths": git_changed_paths,
        }
        candidate_value["candidate_digest"] = _digest(candidate_value)
        closure_input = {key: copy.deepcopy(value) for key, value in authority["initial_closure"].items() if key != "closure_digest"}
        closure_input["schema"] = "execution-package-input/v2"
        closure_input["candidate_ref"] = {"id": candidate_id, "digest": candidate_value["candidate_digest"]}
        closure = ExecutionClosureBuilder().freeze(closure_input)
        sequence = current["value"]["sequence"] + 1
        candidate_ref = self._publish_aux(current["value"]["attempt_id"], sequence, "worker", "candidate", candidate_value, "repair-candidate")
        accepted_worker = {
            "schema": "repair-worker-result/v1", "attempt_id": current["value"]["attempt_id"],
            "nonce": current["value"]["nonce"], "finding_id": authority["finding_id"],
            "worker_report_ref": report_ref, "candidate_ref": candidate_ref,
            "closure": closure, "changed_paths": paths, "role_receipt_ref": role_ref,
        }
        worker_result_ref = self._publish_aux(current["value"]["attempt_id"], sequence, "worker", "result", accepted_worker, "repair-worker-result")
        accepted = copy.deepcopy(current["value"]["accepted_refs"])
        accepted.update({"candidate_ref": candidate_ref, "candidate": candidate_value, "closure": closure, "worker_result_ref": worker_result_ref, "worker_report_ref": report_ref, "worker_role_receipt_ref": role_ref})
        return self._advance(current, "worker-accepted", accepted, current["value"]["role_assignments"], "worker", request=inputs)

    def run_focused(self, inputs, broker):
        self._closed_inputs(inputs, _CAS, _CAS, "focused")
        replay = self._replay(inputs)
        if replay is not None:
            return replay
        current = self._current(inputs, {"worker-accepted"})
        authority = self._authority_value(current)
        result = self._run_plan(authority["focused_plan_ref"], broker, "focused")
        if result.get("terminal_receipt", {}).get("status") != "passed":
            raise RuntimeRepairError("focused broker execution did not pass")
        sequence = current["value"]["sequence"] + 1
        result_ref = self._publish_aux(current["value"]["attempt_id"], sequence, "focused", "broker-result", result, "repair-broker-result")
        accepted = copy.deepcopy(current["value"]["accepted_refs"])
        accepted["focused_result_ref"] = result_ref
        worker = authority["worker_assignment"]["assignment_id"]
        kernel_authority = self._kernel_authority(["accept_resolution_claim"])
        self.kernel.accept_resolution_claim(
            authority["finding_id"], [accepted["candidate_ref"], accepted["worker_result_ref"], result_ref],
            worker_assignment_id=worker, authority_ref=kernel_authority,
            idempotency_key="runtime-e7:%s:%02d:focused:resolution" % (current["value"]["attempt_id"], sequence),
        )
        accepted["native_resolution_ref"] = copy.deepcopy(self.kernel.read_state()["findings"][authority["finding_id"]]["resolution_ref"])
        return self._advance(current, "focused-accepted", accepted, current["value"]["role_assignments"], "focused", request=inputs)

    def issue_reviews(self, inputs):
        allowed = _CAS | {"assignments"}
        self._closed_inputs(inputs, allowed, allowed, "reviews-issue")
        replay = self._replay(inputs)
        if replay is not None:
            return replay
        current = self._current(inputs, {"focused-accepted"})
        supplied = inputs["assignments"]
        if not isinstance(supplied, list) or len(supplied) != 2:
            raise RuntimeRepairError("reviews-issue requires exactly two assignments")
        assignments = {item.get("axis"): self._assignment(item, "reviewer", axis=item.get("axis")) for item in supplied if isinstance(item, Mapping)}
        if set(assignments) != set(_AXES):
            raise RuntimeRepairError("reviews-issue requires both exact review axes")
        authority = self._authority_value(current)
        identities = [(item[key]) for item in assignments.values() for key in ("assignment_id", "actor_id", "host_task_id")]
        worker = authority["worker_assignment"]
        if len(identities) != len(set(identities)) or any(value in {worker["assignment_id"], worker["actor_id"], worker["host_task_id"]} for value in identities):
            raise RuntimeRepairError("Worker and Reviewer role identities must be procedurally distinct")
        sequence = current["value"]["sequence"] + 1
        packages = {}
        for axis in _AXES:
            package = self._role_package(
                current["value"]["attempt_id"], current["value"]["nonce"], assignments[axis], current["ref"],
                inputs["expected_head"], authority["finding_id"], current["value"]["accepted_refs"]["candidate"],
                current["value"]["accepted_refs"]["closure"], authority["permitted_fix_scope"],
                _utc(current["value"]["deadline"], "deadline"), [current["value"]["accepted_refs"]["focused_result_ref"]],
            )
            packages[axis] = self._publish_aux(current["value"]["attempt_id"], sequence, "reviews-issue", self._axis_key(axis), package, "repair-role-package")
        accepted = copy.deepcopy(current["value"]["accepted_refs"])
        accepted["review_package_refs"] = packages
        roles = copy.deepcopy(current["value"]["role_assignments"])
        roles["reviewers"] = assignments
        return self._advance(current, "reviews-issued", accepted, roles, "reviews-issue", request=inputs)

    def accept_review(self, inputs):
        allowed = _CAS | {"role_package_ref", "report_ref", "procedural_role_receipt"}
        self._closed_inputs(inputs, allowed, allowed, "reviews-accept")
        replay = self._replay(inputs)
        if replay is not None:
            return replay
        current = self._current(inputs, {"reviews-issued", "review-one-accepted"})
        package = self._kernel_value(inputs["role_package_ref"], "Reviewer role package")
        axis = package.get("axis")
        roles = current["value"]["role_assignments"]
        assignment = roles.get("reviewers", {}).get(axis)
        expected_package = current["value"]["accepted_refs"].get("review_package_refs", {}).get(axis)
        if assignment is None or inputs["role_package_ref"] != expected_package:
            raise RuntimeRepairError("review result does not match an issued role package")
        report_ref, report = self._physical_json(inputs["report_ref"], "Reviewer report")
        role_ref, receipt = self._physical_json(inputs["procedural_role_receipt"], "Reviewer role receipt")
        self._check_role_receipt(receipt, expected_package, assignment, current, report_ref, "reviewer", axis)
        self._bind_report(report, current, expected_package, assignment, report_ref, axis=axis)
        accepted = copy.deepcopy(current["value"]["accepted_refs"])
        reports = accepted.setdefault("accepted_reviews", {})
        if axis in reports and reports[axis]["report_ref"] != report_ref:
            raise RuntimeRepairError("one review axis cannot be replaced with different bytes")
        reports[axis] = {"report_ref": report_ref, "role_receipt_ref": role_ref, "findings": copy.deepcopy(report.get("findings", [])), "assignment": assignment}
        target = "reviews-accepted" if set(reports) == set(_AXES) else "review-one-accepted"
        return self._advance(current, target, accepted, roles, "reviews-accept-" + self._axis_key(axis), request=inputs)

    def issue_validator(self, inputs):
        allowed = _CAS | {"assignment"}
        self._closed_inputs(inputs, allowed, allowed, "validator-issue")
        replay = self._replay(inputs)
        if replay is not None:
            return replay
        current = self._current(inputs, {"reviews-accepted"})
        assignment = self._assignment(inputs["assignment"], "finding-validator", axis=None)
        used = current["value"]["role_assignments"]
        occupied = set()
        for role in [used.get("worker"), *used.get("reviewers", {}).values()]:
            if isinstance(role, Mapping):
                occupied.update(role.get(key) for key in ("assignment_id", "actor_id", "host_task_id"))
        if any(assignment[key] in occupied for key in ("assignment_id", "actor_id", "host_task_id")):
            raise RuntimeRepairError("Validator must be procedurally distinct from Worker and Reviewers")
        authority = self._authority_value(current)
        reports = current["value"]["accepted_refs"]["accepted_reviews"]
        findings = []
        for axis in _AXES:
            findings.extend(copy.deepcopy(reports[axis]["findings"]))
        package = self._role_package(
            current["value"]["attempt_id"], current["value"]["nonce"], assignment, current["ref"], inputs["expected_head"],
            authority["finding_id"], current["value"]["accepted_refs"]["candidate"], current["value"]["accepted_refs"]["closure"],
            authority["permitted_fix_scope"], _utc(current["value"]["deadline"], "deadline"),
            [reports[axis]["report_ref"] for axis in _AXES],
        )
        package["candidate_findings"] = findings
        sequence = current["value"]["sequence"] + 1
        package_ref = self._publish_aux(current["value"]["attempt_id"], sequence, "validator-issue", "package", package, "repair-role-package")
        accepted = copy.deepcopy(current["value"]["accepted_refs"])
        accepted["validator_package_ref"] = package_ref
        roles = copy.deepcopy(used)
        roles["validator"] = assignment
        return self._advance(current, "validator-issued", accepted, roles, "validator-issue", request=inputs)

    def accept_validator(self, inputs):
        allowed = _CAS | {"decision_ref", "procedural_role_receipt"}
        self._closed_inputs(inputs, allowed, allowed, "validator-accept")
        replay = self._replay(inputs)
        if replay is not None:
            return replay
        current = self._current(inputs, {"validator-issued"})
        decision_ref, decision = self._physical_json(inputs["decision_ref"], "Validator decision")
        receipt_ref, receipt = self._physical_json(inputs["procedural_role_receipt"], "Validator role receipt")
        assignment = current["value"]["role_assignments"]["validator"]
        package_ref = current["value"]["accepted_refs"]["validator_package_ref"]
        self._check_role_receipt(receipt, package_ref, assignment, current, decision_ref, "finding-validator", None)
        self._bind_report(decision, current, package_ref, assignment, decision_ref)
        package = self._kernel_value(package_ref, "Validator package")
        candidates = package.get("candidate_findings", [])
        expected = [item.get("finding_id") for item in candidates if isinstance(item, Mapping)]
        dispositions = decision.get("dispositions")
        if not isinstance(dispositions, list) or sorted(item.get("finding_id") for item in dispositions if isinstance(item, Mapping)) != sorted(expected):
            raise RuntimeRepairError("Validator decision does not classify the complete Finding union exactly once")
        classifications = {"required", "defer", "duplicate", "rejected"}
        if any(item.get("classification") not in classifications for item in dispositions):
            raise RuntimeRepairError("Validator classification is outside the closed set")
        accepted = copy.deepcopy(current["value"]["accepted_refs"])
        accepted.update({"validator_decision_ref": decision_ref, "validator_role_receipt_ref": receipt_ref, "validator_dispositions": copy.deepcopy(dispositions)})
        target = "required-remains" if any(item["classification"] == "required" for item in dispositions) else "validator-accepted"
        reason = "Validator retained a current required Finding" if target == "required-remains" else None
        return self._advance(current, target, accepted, current["value"]["role_assignments"], "validator-accept", terminal_reason=reason, request=inputs)

    def run_whole(self, inputs, broker):
        self._closed_inputs(inputs, _CAS, _CAS, "whole")
        replay = self._replay(inputs)
        if replay is not None:
            return replay
        current = self._current(inputs, {"validator-accepted"})
        authority = self._authority_value(current)
        result = self._run_plan(authority["whole_plan_ref"], broker, "whole")
        if result.get("terminal_receipt", {}).get("status") != "passed":
            raise RuntimeRepairError("whole broker execution did not pass")
        sequence = current["value"]["sequence"] + 1
        result_ref = self._publish_aux(current["value"]["attempt_id"], sequence, "whole", "broker-result", result, "repair-broker-result")
        accepted = copy.deepcopy(current["value"]["accepted_refs"])
        accepted["whole_result_ref"] = result_ref
        return self._advance(current, "whole-accepted", accepted, current["value"]["role_assignments"], "whole", request=inputs)

    def finalize(self, inputs):
        self._closed_inputs(inputs, _CAS, _CAS, "finalize")
        replay = self._replay(inputs)
        if replay is not None:
            return replay
        current = self._current(inputs, {"whole-accepted", "finalizing"})
        if current["value"]["state"] == "whole-accepted":
            current_result = self._advance(current, "finalizing", current["value"]["accepted_refs"], current["value"]["role_assignments"], "finalize-open", request=inputs)
            current = self._current({
                "attempt_id": current_result["attempt_id"], "nonce": inputs["nonce"],
                "previous_state_ref": current_result["attempt_state_ref"], "expected_head": current_result["head"],
            }, {"finalizing"})
        authority = self._authority_value(current)
        accepted = copy.deepcopy(current["value"]["accepted_refs"])
        for key in ("candidate_ref", "worker_result_ref", "focused_result_ref", "native_resolution_ref", "validator_decision_ref", "whole_result_ref"):
            if key not in accepted:
                raise RuntimeRepairError("finalizer is missing accepted attempt evidence: " + key)
        kernel_state = self.kernel.read_state()
        finding = kernel_state["findings"].get(authority["finding_id"])
        task = kernel_state["tasks"].get(authority["task_id"])
        if not isinstance(finding, Mapping) or finding.get("resolution_ref") != accepted["native_resolution_ref"]:
            raise RuntimeRepairError("native Finding is not at the accepted resolution frontier")
        if not isinstance(task, Mapping):
            raise RuntimeRepairError("Task is not live at the final repair frontier")

        sequence = current["value"]["sequence"] + 1
        execution_claim = _execution_claim(authority["mode"])
        final_evidence = {
            "schema": "runtime-practical-repair-final/v1", "attempt_id": current["value"]["attempt_id"],
            "attempt_state_ref": current["ref"], "trust_profile": TRUST_PROFILE,
            "accepted_residual": ACCEPTED_RESIDUAL, "candidate_ref": accepted["candidate_ref"],
            "closure": accepted["closure"], "worker_result_ref": accepted["worker_result_ref"],
            "focused_result_ref": accepted["focused_result_ref"], "review_refs": accepted["accepted_reviews"],
            "validator_decision_ref": accepted["validator_decision_ref"], "whole_result_ref": accepted["whole_result_ref"],
            "execution_claim": execution_claim,
            "runtime_identity": copy.deepcopy(self.runtime.identity),
        }
        initial = self._kernel_value(authority["initial_e6_ref"], "initial E6")
        objective_digest = initial["objective_digest"]
        started_head = initial["started_head"]
        frontier = initial["inputs"]["persisted_frontier"]

        def record(stage, compiled, previous):
            value = {
                "schema": "runtime-execution-step/v2", "qualified_id": "group.E." + stage,
                "record_id": stage, "objective_digest": objective_digest,
                "inputs": {
                    "persisted_frontier": copy.deepcopy(frontier),
                    "native_kernel_refs": copy.deepcopy(final_evidence),
                    "prior_stage_refs": {"E6-initial": copy.deepcopy(authority["initial_e6_ref"])},
                },
                "compiled_source_digest": _digest(compiled), "compiled": compiled,
                "previous_ref": copy.deepcopy(previous), "runtime_identity": copy.deepcopy(self.runtime.identity),
                # Deterministic across a crash/retry of the finalizer.  The
                # actual causal time is the immutable attempt issuance time;
                # no retry-local clock value enters canonical record bytes.
                "started_head": copy.deepcopy(started_head), "finished_at": authority["issued_at"],
            }
            return value

        e7_compiled = {
            "schema": "execution-group-result/v1", "qualified_id": "group.E.E7", "status": "accepted",
            "worker_self_close": False, "attempt_id": current["value"]["attempt_id"],
            "output": {"status": "accepted", "kind": "repair-evidence"},
            "execution_claim": execution_claim,
        }
        e7_ref = self._publish_canonical("runtime-E7", record("E7", e7_compiled, authority["initial_e6_ref"]), "runtime-skill", current["value"]["attempt_id"], sequence, "E7")
        e6_compiled = {
            "schema": "execution-group-result/v1", "qualified_id": "group.E.E6", "status": "accepted",
            "next": "E8", "attempt_id": current["value"]["attempt_id"],
            "validation": {"dispositions": [], "source_ref": accepted["validator_decision_ref"]},
            "output": {"status": "accepted", "kind": "validated-disposition"},
            "execution_claim": execution_claim,
        }
        e6_ref = self._publish_canonical("runtime-E6", record("E6", e6_compiled, e7_ref), "runtime-skill", current["value"]["attempt_id"], sequence, "E6")
        e8_compiled = {
            "schema": "execution-group-result/v1", "qualified_id": "group.E.E8", "status": "accepted",
            "finalization": {"candidate_ref": accepted["candidate_ref"], "execution_closure_digest": accepted["closure"]["closure_digest"], "attempt_id": current["value"]["attempt_id"]},
            "output": {"status": "accepted", "kind": "evidence-finalization"},
            "execution_claim": execution_claim,
        }
        e8_ref = self._publish_canonical("runtime-E8", record("E8", e8_compiled, e6_ref), "runtime-skill", current["value"]["attempt_id"], sequence, "E8")
        e9_compiled = {
            "schema": "execution-group-result/v1", "qualified_id": "group.E.E9", "status": "accepted",
            "candidate_digest": accepted["candidate"]["candidate_digest"], "closure_digest": accepted["closure"]["closure_digest"],
            "whole_result_ref": accepted["whole_result_ref"], "complete": True, "open_required": False,
            "output": {"status": "passed", "kind": "whole-verification"},
            "execution_claim": execution_claim,
        }
        e9_ref = self._publish_canonical("runtime-E9", record("E9", e9_compiled, e8_ref), "runtime-skill", current["value"]["attempt_id"], sequence, "E9")

        reviews = accepted["accepted_reviews"]
        kernel_authority = self._kernel_authority(
            ["open_review_epoch", "open_review", "accept_finding_closure", "accept_task_result"],
            write_scopes=authority["permitted_fix_scope"],
        )
        closure_packages = {}
        for axis in _AXES:
            assignment = reviews[axis]["assignment"]
            epoch = "%s-%s-closure" % (current["value"]["attempt_id"], self._axis_key(axis))
            review_id = "%s-%s-closure" % (current["value"]["attempt_id"], self._axis_key(axis))
            self.kernel.open_review_epoch(epoch, accepted["native_resolution_ref"], reviewer_assignment_id=assignment["assignment_id"], authority_ref=kernel_authority, idempotency_key="runtime-e7:%s:%02d:finalize:epoch:%s" % (current["value"]["attempt_id"], sequence, self._axis_key(axis)))
            self.kernel.open_review(review_id, authority["task_id"], [], reviewer_assignment_id=assignment["assignment_id"], fresh_epoch_id=epoch, review_kind="closure", target_finding_id=authority["finding_id"], authority_ref=kernel_authority, idempotency_key="runtime-e7:%s:%02d:finalize:review:%s" % (current["value"]["attempt_id"], sequence, self._axis_key(axis)))
            closure_packages[axis] = copy.deepcopy(self.kernel.read_state()["reviews"][review_id]["package_ref"])
        selected = reviews["architecture/safety"]["assignment"]
        review_id = "%s-%s-closure" % (current["value"]["attempt_id"], self._axis_key("architecture/safety"))
        epoch = "%s-%s-closure" % (current["value"]["attempt_id"], self._axis_key("architecture/safety"))
        latest = self.kernel.read_state()
        if latest["findings"][authority["finding_id"]].get("closure_ref") is None:
            self.kernel.accept_finding_closure(authority["finding_id"], [accepted["native_resolution_ref"]], reviewer_assignment_id=selected["assignment_id"], fresh_epoch_id=epoch, review_id=review_id, authority_ref=kernel_authority, idempotency_key="runtime-e7:%s:%02d:finalize:closure" % (current["value"]["attempt_id"], sequence))
        elif latest["findings"][authority["finding_id"]].get("closed_by") != selected["assignment_id"]:
            raise RuntimeRepairError("native Finding was closed by a different assignment during finalization")
        latest = self.kernel.read_state()
        if latest["tasks"][authority["task_id"]].get("result_ref") is None:
            self.kernel.accept_task_result(authority["task_id"], {
                "status": "success", "changed_paths": authority["permitted_fix_scope"],
                "execution_candidate_digest": accepted["candidate"]["candidate_digest"],
                "repair_attempt_id": current["value"]["attempt_id"], "whole_result_ref": accepted["whole_result_ref"],
                "trust_profile": TRUST_PROFILE, "accepted_residual": ACCEPTED_RESIDUAL,
                "execution_claim": execution_claim,
            }, worker_assignment_id=authority["worker_assignment"]["assignment_id"], authority_ref=kernel_authority, idempotency_key="runtime-e7:%s:%02d:finalize:task" % (current["value"]["attempt_id"], sequence))
        else:
            result_object = self.kernel.read_object(latest["tasks"][authority["task_id"]]["result_ref"])["payload"]
            if result_object.get("result", {}).get("repair_attempt_id") != current["value"]["attempt_id"]:
                raise RuntimeRepairError("Task result belongs to a different repair attempt")
        accepted.update({"runtime_E7_ref": e7_ref, "runtime_E6_ref": e6_ref, "runtime_E8_ref": e8_ref, "runtime_E9_ref": e9_ref, "closure_review_refs": closure_packages, "native_closure_ref": copy.deepcopy(self.kernel.read_state()["findings"][authority["finding_id"]]["closure_ref"]), "task_result_ref": copy.deepcopy(self.kernel.read_state()["tasks"][authority["task_id"]]["result_ref"])})
        close_request = {"attempt_id": current["value"]["attempt_id"], "nonce": current["value"]["nonce"], "previous_state_ref": current["ref"], "expected_head": current["expected_head"]}
        return self._advance(current, "closed", accepted, current["value"]["role_assignments"], "finalize-close", request=close_request)

    # -- immutable-chain helpers -----------------------------------------

    def _git(self, *arguments: str) -> bytes:
        try:
            result = subprocess.run(
                ["git", "-C", str(self.project), *arguments],
                check=False,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                timeout=15,
            )
        except (OSError, subprocess.TimeoutExpired) as error:
            raise RuntimeRepairError("Git candidate identity could not be resolved") from error
        if result.returncode != 0:
            raise RuntimeRepairError("Git candidate identity command failed")
        return result.stdout

    def _git_snapshot(self, scope: Sequence[str], label: str) -> dict[str, str]:
        root = Path(self._git("rev-parse", "--show-toplevel").decode("utf-8", "strict").strip()).resolve()
        if root != self.project:
            raise RuntimeRepairError(label + " must use the project root as the Git worktree root")
        commit = self._git("rev-parse", "--verify", "HEAD^{commit}").decode("ascii", "strict").strip().lower()
        tree = self._git("rev-parse", "--verify", "HEAD^{tree}").decode("ascii", "strict").strip().lower()
        if not commit or not tree or not re.fullmatch(r"[0-9a-f]+", commit + tree):
            raise RuntimeRepairError(label + " has malformed Git object identities")
        dirty = self._git("status", "--porcelain=v1", "-z", "--", *scope)
        if dirty:
            raise RuntimeRepairError(label + " has uncommitted changes in the permitted fix scope")
        return {"commit": commit, "tree": tree}

    def _git_require_ancestor(self, baseline: str, candidate: str) -> None:
        try:
            result = subprocess.run(
                ["git", "-C", str(self.project), "merge-base", "--is-ancestor", baseline, candidate],
                check=False,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                timeout=15,
            )
        except (OSError, subprocess.TimeoutExpired) as error:
            raise RuntimeRepairError("Git candidate ancestry could not be resolved") from error
        if result.returncode != 0:
            raise RuntimeRepairError("post-fix Git candidate is not descended from the repair baseline")

    def _git_changed_paths(self, baseline: str, candidate: str) -> list[str]:
        raw = self._git("diff", "--name-only", "--no-renames", "-z", baseline, candidate, "--")
        paths = [item.decode("utf-8", "surrogateescape") for item in raw.split(b"\0") if item]
        return self._normalized_scope(paths)

    def _git_artifact(self, commit: str, relative: str) -> dict[str, Any]:
        spec = commit + ":" + relative
        try:
            result = subprocess.run(
                ["git", "-C", str(self.project), "cat-file", "blob", spec],
                check=False,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                timeout=15,
            )
        except (OSError, subprocess.TimeoutExpired) as error:
            raise RuntimeRepairError("Git candidate artifact could not be read") from error
        if result.returncode != 0:
            return {"path": relative, "present": False}
        oid = self._git("rev-parse", "--verify", spec).decode("ascii", "strict").strip().lower()
        return {
            "path": relative,
            "present": True,
            "git_object": oid,
            "digest": "sha256:" + hashlib.sha256(result.stdout).hexdigest(),
        }

    def _attempts(self, *, task_id=None, finding_id=None):
        grouped = {}
        state = self.kernel.read_state()
        for artifact_id, item in state.get("artifacts", {}).items():
            match = _STATE_ID.fullmatch(artifact_id)
            if not match:
                continue
            value = self._kernel_value(item["object_ref"], "repair state")
            if value.get("schema") != "repair-attempt-state/v1" or value.get("attempt_id") != match.group(1) or value.get("sequence") != int(match.group(2)):
                raise RuntimeRepairError("repair attempt state identity is inconsistent")
            grouped.setdefault(match.group(1), []).append({"id": artifact_id, "ref": copy.deepcopy(item["object_ref"]), "value": value})
        result = []
        for chain in grouped.values():
            chain.sort(key=lambda item: item["value"]["sequence"])
            authority_ref = chain[0]["value"].get("attempt_authority_ref")
            immutable = None
            previous = None
            for index, item in enumerate(chain):
                value = item["value"]
                if value["sequence"] != index or value.get("previous_state_ref") != previous:
                    raise RuntimeRepairError("repair attempt state chain is not contiguous")
                fields = (value.get("attempt_authority_ref"), value.get("attempt_id"), value.get("nonce"), value.get("deadline"), value.get("review_round"), value.get("product_fix_attempt"))
                if immutable is None:
                    immutable = fields
                elif fields != immutable:
                    raise RuntimeRepairError("repair attempt immutable fields changed across the chain")
                previous = item["ref"]
            authority = self._kernel_value(authority_ref, "repair authority")
            if task_id is not None and authority.get("task_id") != task_id:
                continue
            if finding_id is not None and authority.get("finding_id") != finding_id:
                continue
            result.append(chain[-1])
        result.sort(key=lambda item: (item["value"]["attempt_id"], item["value"]["sequence"]))
        return result

    def _current(self, inputs, allowed_states):
        self._closed_inputs(inputs, set(inputs), _CAS, "repair transition")
        if not _ATTEMPT.fullmatch(inputs["attempt_id"]) or not _NONCE.fullmatch(inputs["nonce"]):
            raise RuntimeRepairError("repair attempt identity is malformed")
        self._expect_head(inputs["expected_head"])
        attempts = self._attempts()
        selected = [item for item in attempts if item["value"]["attempt_id"] == inputs["attempt_id"]]
        if len(selected) != 1:
            raise RuntimeRepairError("repair attempt is missing or ambiguous")
        current = selected[0]
        if current["ref"] != inputs["previous_state_ref"] or current["value"]["nonce"] != inputs["nonce"]:
            raise RuntimeRepairError("repair predecessor state or nonce differs from the current attempt")
        if current["value"]["state"] not in allowed_states:
            raise RuntimeRepairError("repair action is not valid from state " + current["value"]["state"])
        if datetime.now(timezone.utc) >= _utc(current["value"]["deadline"], "deadline"):
            value = current["value"]
            self._publish_state(
                value["attempt_id"], value["nonce"], value["sequence"] + 1, "expired", current["ref"],
                value["attempt_authority_ref"], value["accepted_refs"], value["role_assignments"],
                value["deadline"], value["review_round"], value["product_fix_attempt"], "expire",
                terminal_reason="repair deadline expired", next_action=None,
            )
            raise RuntimeRepairError("repair deadline expired")
        current["expected_head"] = copy.deepcopy(inputs["expected_head"])
        return current

    def _advance(self, current, state, accepted, roles, action, terminal_reason=None, request=None):
        value = current["value"]
        next_action = _NEXT[state]
        accepted = copy.deepcopy(accepted)
        if request is not None:
            accepted.setdefault("request_digests", {})[_digest(request)] = action
        new_value, ref = self._publish_state(
            value["attempt_id"], value["nonce"], value["sequence"] + 1, state, current["ref"],
            value["attempt_authority_ref"], accepted, roles, value["deadline"], value["review_round"],
            value["product_fix_attempt"], action, terminal_reason=terminal_reason, next_action=next_action,
        )
        return self._result(state, value["attempt_id"], ref, next_action, terminal_reason)

    def _replay(self, inputs):
        """Return the committed result for an exact already-consumed request."""
        if not isinstance(inputs, Mapping) or not _CAS <= set(inputs):
            return None
        digest = _digest(inputs)
        selected = [item for item in self._attempts() if item["value"]["attempt_id"] == inputs.get("attempt_id")]
        if len(selected) != 1:
            return None
        value = selected[0]["value"]
        if value.get("nonce") != inputs.get("nonce"):
            return None
        if digest not in value.get("accepted_refs", {}).get("request_digests", {}):
            return None
        return self._result(value["state"], value["attempt_id"], selected[0]["ref"], value["next_action"], value.get("terminal_reason"))

    def _publish_state(self, attempt_id, nonce, sequence, state, previous, authority_ref, accepted, roles, deadline, review_round, product_fix_attempt, action, terminal_reason=None, next_action=None):
        consumed = _head(self.kernel)
        value = {
            "schema": "repair-attempt-state/v1", "attempt_authority_ref": copy.deepcopy(authority_ref),
            "attempt_id": attempt_id, "nonce": nonce, "sequence": sequence, "state": state,
            "previous_state_ref": copy.deepcopy(previous), "consumed_head": consumed,
            "accepted_refs": copy.deepcopy(accepted), "role_assignments": copy.deepcopy(roles),
            "deadline": deadline, "review_round": review_round, "product_fix_attempt": product_fix_attempt,
            "next_action": _NEXT[state] if next_action is None else next_action, "terminal_reason": terminal_reason,
            "trust_profile": TRUST_PROFILE, "accepted_residual": ACCEPTED_RESIDUAL,
        }
        artifact_id = "repair-e7-%s-s%02d" % (attempt_id, sequence)
        ref = self._publish(artifact_id, value, "repair-attempt-state", "runtime-e7:%s:%02d:%s" % (attempt_id, sequence, action))
        return value, ref

    def _publish_aux(self, attempt_id, sequence, action, suffix, value, kind):
        artifact_id = "repair-e7-%s-%02d-%s-%s" % (attempt_id, sequence, action, suffix)
        return self._publish(artifact_id, value, kind, "runtime-e7:%s:%02d:%s:%s" % (attempt_id, sequence, action, suffix))

    def _publish_canonical(self, artifact_id, value, kind, attempt_id, sequence, suffix):
        state = self.kernel.read_state()
        if artifact_id in state.get("artifacts", {}):
            existing = self._kernel_value(state["artifacts"][artifact_id]["object_ref"], artifact_id)
            if existing != value:
                raise RuntimeRepairError(artifact_id + " already exists with different bytes")
            return copy.deepcopy(state["artifacts"][artifact_id]["object_ref"])
        return self._publish(artifact_id, value, kind, "runtime-e7:%s:%02d:finalize:%s" % (attempt_id, sequence, suffix))

    def _publish(self, artifact_id, value, kind, idempotency_key):
        command = self.kernel.make_command(
            "publish_artifact", {"artifact_id": artifact_id, "version": "v1", "value": copy.deepcopy(value), "kind": kind, "path": None},
            authority_ref={"approved": True, "scopes": ["publish_artifact"], "human_receipt": self.runtime.state["objective_ref"]["approval_ref"]["digest"]},
            expected_head=_head(self.kernel), idempotency_key=idempotency_key,
        )
        state = self.kernel.apply(command)
        return copy.deepcopy(state["artifacts"][artifact_id]["object_ref"])

    # -- evidence and validation helpers ---------------------------------

    def _authority_value(self, current):
        value = self._kernel_value(current["value"]["attempt_authority_ref"], "repair authority")
        if value.get("schema") != "repair-attempt-authority/v1" or value.get("attempt_id") != current["value"]["attempt_id"] or value.get("nonce") != current["value"]["nonce"]:
            raise RuntimeRepairError("repair authority does not bind the active attempt")
        return value

    def _kernel_value(self, ref, label):
        if not isinstance(ref, Mapping):
            raise RuntimeRepairError(label + " reference is missing")
        try:
            outer = self.kernel.read_object(ref)
        except Exception as error:
            raise RuntimeRepairError(label + " reference is not a current-Run object") from error
        artifact = outer.get("payload")
        if not isinstance(artifact, Mapping) or "payload" not in artifact:
            raise RuntimeRepairError(label + " is not a published artifact")
        return copy.deepcopy(artifact["payload"])

    def _verified_ref(self, ref, label):
        if not isinstance(ref, Mapping):
            raise RuntimeRepairError(label + " reference is malformed")
        if "path" in ref:
            physical, _ = self._physical_json(ref, label)
            return physical
        self._kernel_value(ref, label)
        return copy.deepcopy(dict(ref))

    def _physical_json(self, supplied, label):
        if not isinstance(supplied, Mapping) or set(supplied) != {"path", "digest"}:
            raise RuntimeRepairError(label + " requires exact path and digest")
        path = Path(supplied["path"])
        if not path.is_absolute():
            path = self.project / path
        try:
            root = self.project.resolve(strict=True)
            lexical_parts = path.relative_to(root).parts
            lexical = root
            for part in lexical_parts:
                lexical = lexical / part
                if lexical.is_symlink():
                    raise RuntimeRepairError(label + " traverses a symlink")
            resolved = path.resolve(strict=True)
            resolved.relative_to(root)
        except (OSError, ValueError) as error:
            raise RuntimeRepairError(label + " escapes the selected project") from error
        if not resolved.is_file():
            raise RuntimeRepairError(label + " is not a regular file")
        raw = resolved.read_bytes()
        actual = "sha256:" + hashlib.sha256(raw).hexdigest()
        if supplied["digest"] != actual:
            raise RuntimeRepairError(label + " digest differs from physical bytes")
        try:
            value = json.loads(raw)
        except (UnicodeDecodeError, json.JSONDecodeError) as error:
            raise RuntimeRepairError(label + " is not JSON") from error
        if not isinstance(value, Mapping):
            raise RuntimeRepairError(label + " must contain an object")
        return copy.deepcopy(dict(supplied)), copy.deepcopy(dict(value))

    def _project_file(self, relative, label):
        if not isinstance(relative, str) or not relative or Path(relative).is_absolute() or any(part in {"", ".", ".."} for part in Path(relative).parts):
            raise RuntimeRepairError(label + " path is not canonical project-relative")
        root = self.project.resolve(strict=True)
        current = root
        for part in Path(relative).parts:
            current = current / part
            if current.is_symlink():
                raise RuntimeRepairError(label + " traverses a symlink")
        try:
            resolved = current.resolve(strict=True)
            resolved.relative_to(root)
        except (OSError, ValueError) as error:
            raise RuntimeRepairError(label + " escapes the selected project") from error
        if not resolved.is_file():
            raise RuntimeRepairError(label + " is not a regular file")
        return resolved

    def _run_plan(self, ref, broker, label):
        if "path" in ref:
            _, plan = self._physical_json(ref, label + " plan")
        else:
            plan = self._kernel_value(ref, label + " plan")
        required = {"execution_package", "runner_package", "policy"}
        if not required <= set(plan) or set(plan) - required - {"probes"}:
            raise RuntimeRepairError(label + " plan is not an executable precommitted broker plan")
        return broker.execute(plan["execution_package"], plan["runner_package"], plan["policy"], probes=plan.get("probes", []))

    def _role_package(self, attempt_id, nonce, assignment, predecessor, head, finding_id, candidate, closure, scope, deadline, refs):
        role_nonce = hashlib.sha256(
            (nonce + "\0" + assignment["assignment_id"] + "\0" + str(predecessor)).encode("utf-8")
        ).hexdigest()
        return {
            "schema": "practical-role-package/v1", "attempt_id": attempt_id, "nonce": nonce,
            "role_package_id": "%s-%s" % (attempt_id, assignment["assignment_id"]),
            "role_nonce": role_nonce, "role": assignment["role"], "axis": assignment.get("axis"),
            "assignment_id": assignment["assignment_id"], "actor_id": assignment["actor_id"], "host_task_id": assignment["host_task_id"],
            "predecessor_state_ref": copy.deepcopy(predecessor), "predecessor_head": copy.deepcopy(head),
            "finding_ref": {"id": finding_id},
            "candidate_ref": {"id": candidate.get("candidate_id", "candidate"), "digest": candidate["candidate_digest"]},
            "closure_ref": {"id": "execution-closure", "digest": closure["closure_digest"]},
            "scope": copy.deepcopy(scope), "deadline": deadline.isoformat(), "input_refs": copy.deepcopy(refs),
        }

    @staticmethod
    def _assignment(value, role, axis):
        if not isinstance(value, Mapping):
            raise RuntimeRepairError("role assignment must be an object")
        required = {"assignment_id", "actor_id", "host_task_id", "parent_task_id"}
        if not required <= set(value) or any(not isinstance(value[key], str) or not value[key] for key in required):
            raise RuntimeRepairError("role assignment lacks nonempty parent/host/actor/assignment identity")
        if axis is not None and value.get("axis") != axis:
            raise RuntimeRepairError("role assignment axis differs from the issued axis")
        return {**{key: value[key] for key in required}, "role": role, "axis": axis}

    def _check_role_receipt(self, receipt, package_ref, assignment, current, output_ref, role, axis):
        required = {"schema", "trust_profile", "attested_by", "parent_task_id", "host_task_id", "assignment_id", "actor_id", "role", "axis", "attempt_id", "nonce", "role_package_ref", "predecessor_state_ref", "predecessor_head", "started_at", "ended_at", "terminal_state", "output_ref", "observed_changed_paths"}
        if set(receipt) != required or receipt.get("schema") != "procedural-role-receipt/v1" or receipt.get("trust_profile") != TRUST_PROFILE:
            raise RuntimeRepairError("procedural role receipt has the wrong closed schema")
        if not isinstance(receipt.get("attested_by"), str) or not receipt["attested_by"]:
            raise RuntimeRepairError("procedural role receipt lacks its trusted-parent attestor")
        expected = {
            "parent_task_id": assignment["parent_task_id"], "host_task_id": assignment["host_task_id"],
            "assignment_id": assignment["assignment_id"], "actor_id": assignment["actor_id"],
            "attempt_id": current["value"]["attempt_id"], "nonce": current["value"]["nonce"],
            "role_package_ref": package_ref, "predecessor_state_ref": current["ref"],
            "predecessor_head": current["expected_head"], "output_ref": output_ref,
        }
        if any(receipt.get(key) != value for key, value in expected.items()) or receipt.get("role") != role or receipt.get("axis") != axis or receipt.get("terminal_state") != "completed":
            raise RuntimeRepairError("procedural role receipt does not bind the issued package and predecessor")
        if _utc(receipt["ended_at"], "role receipt end") < _utc(receipt["started_at"], "role receipt start"):
            raise RuntimeRepairError("procedural role receipt time range is reversed")

    def _bind_report(self, report, current, package_ref, assignment, report_ref, axis=None):
        bindings = {
            "attempt_id": current["value"]["attempt_id"], "nonce": current["value"]["nonce"],
            "role_package_ref": package_ref, "predecessor_state_ref": current["ref"],
            "assignment_id": assignment["assignment_id"], "actor_id": assignment["actor_id"], "host_task_id": assignment["host_task_id"],
        }
        for key, expected in bindings.items():
            if report.get(key) != expected:
                raise RuntimeRepairError("role output does not bind " + key)
        if axis is not None and report.get("axis") != axis:
            raise RuntimeRepairError("Reviewer output axis differs from its package")
        if report.get("output_digest") not in {None, report_ref["digest"]}:
            raise RuntimeRepairError("role output digest does not bind its physical bytes")

    def _kernel_authority(self, scopes, *, write_scopes=()):
        return {
            "approved": True, "scopes": list(scopes), "run_id": self.kernel.read_state()["run_id"],
            "write_scopes": copy.deepcopy(list(write_scopes)),
            "human_receipt": copy.deepcopy(self.runtime.approval["receipt"]),
        }

    @staticmethod
    def _closed_inputs(inputs, allowed, required, label):
        if not isinstance(inputs, Mapping) or set(inputs) - set(allowed) or not set(required) <= set(inputs):
            raise RuntimeRepairError(label + " inputs are incomplete or outside the closed contract")

    @staticmethod
    def _normalized_scope(paths):
        if not isinstance(paths, list) or not paths or any(not isinstance(path, str) or not path for path in paths):
            raise RuntimeRepairError("permitted fix scope must be a nonempty path array")
        normalized = sorted(paths)
        if len(normalized) != len(set(normalized)) or any(Path(path).is_absolute() or any(part in {"", ".", ".."} for part in Path(path).parts) for path in normalized):
            raise RuntimeRepairError("permitted fix scope is not canonical")
        return normalized

    def _expect_head(self, expected):
        if expected != _head(self.kernel):
            raise RuntimeRepairError("repair action expected HEAD is stale")

    @staticmethod
    def _axis_key(axis):
        return "architecture-safety" if axis == "architecture/safety" else "integration-operability-time-dotfiles"

    def _result(self, status, attempt_id, attempt_state_ref, next_action, terminal_reason):
        return {
            "status": status, "attempt_id": attempt_id, "attempt_state_ref": copy.deepcopy(attempt_state_ref),
            "head": _head(self.kernel), "next_action": next_action, "terminal_reason": terminal_reason,
        }


# The historical coordinator above is intentionally left on its practical
# E7 protocol.  Workflow-loop/v1 has a different progress authority: the
# Kernel's immutable loop event history.  Keeping the adapter separate makes
# it impossible for a legacy deadline or review budget to accidentally become
# a counter for the versioned path.
_WORKFLOW_LOOP_TRANSITION_SCHEMA = "workflow-loop-repair-transition/v1"
_WORKFLOW_LOOP_STATUS_SCHEMA = "workflow-loop-repair-status/v1"
_WORKFLOW_LOOP_BATCH_SCHEMA = "workflow-loop-repair-batch-result/v1"
_WORKFLOW_LOOP_REVIEW_SCHEMA = "workflow-loop-review-result-set/v1"
_WORKFLOW_LOOP_FORBIDDEN_PROGRESS_FIELDS = frozenset(
    {
        "budget",
        "deadline",
        "observed_budget",
        "product_fix_attempt",
        "product_fix_attempts",
        "wall_clock_deadline",
        "wall_clock_minutes",
        "review_budget",
        "review_budget_remaining",
        "review_round",
        "iteration_limit",
        "additional_iteration_limit",
        "technical_retry_limit",
        "remaining",
        "remaining_seconds",
        "task_budgets",
    }
)
_WORKFLOW_LOOP_EVENT_KINDS = frozenset(
    {"initial", "improvement", "integration-return", "technical-retry"}
)


class WorkflowLoopRepairCoordinator:
    """Durable workflow-loop/v1 repair boundary.

    This is an additive adapter for callers that already have a v1 Run.  It
    deliberately does not inherit the practical E7 coordinator: all progress
    transitions go through the Control Kernel's loop-control methods and are
    guarded by the Kernel revision (CAS) at each boundary.  Batch planning,
    review-package construction, evidence invalidation, and completion remain
    pure contract operations; only loop events are persisted here.
    """

    contract_version = LOOP_CONTRACT_VERSION
    review_axes = REQUIRED_REVIEW_AXES

    def __init__(
        self,
        project: str | Path | None = None,
        run_id: str | None = None,
        *,
        runtime: Any | None = None,
        kernel: Any | None = None,
    ) -> None:
        if runtime is not None and kernel is not None and getattr(runtime, "kernel", kernel) is not kernel:
            raise RuntimeRepairError("runtime and kernel refer to different Control Kernels")
        if runtime is None and kernel is None:
            if project is None or run_id is None:
                raise RuntimeRepairError("workflow-loop coordinator requires project and run_id")
            runtime = InceptionRuntime(project, run_id)
            kernel = runtime.kernel
        elif kernel is None:
            kernel = getattr(runtime, "kernel", None)
        if kernel is None:
            raise RuntimeRepairError("workflow-loop coordinator requires a Kernel")
        self.runtime = runtime
        self.kernel = kernel
        self.project = Path(project).resolve() if project is not None else getattr(runtime, "project", None)
        self.run_id = run_id or getattr(runtime, "run_id", None) or getattr(kernel, "run_id", None)

    # -- durable loop-control transitions ---------------------------------

    def status(self) -> dict[str, Any]:
        """Return a read-only status projection without consulting a clock."""

        state = self._state()
        loop = self._loop(state)
        identity = self._identity(loop)
        history = self._history(state)
        policy = self._policy(identity)
        latest = history[-1] if history else None
        outcome = loop.get("outcome") or loop.get("terminal_outcome")
        if latest is not None and latest.get("status") == "execution-unknown":
            outcome = "recovery-required"
        return {
            "schema": _WORKFLOW_LOOP_STATUS_SCHEMA,
            "contract_version": self.contract_version,
            "run_id": self.run_id,
            "revision": state.get("revision"),
            "head": self._head(),
            "identity": copy.deepcopy(identity),
            "policy": policy,
            "loop_control": copy.deepcopy(loop),
            "history": copy.deepcopy(history),
            "latest_event": copy.deepcopy(latest),
            "status": loop.get("status", "idle"),
            "outcome": outcome,
            "recovery_required": outcome == "recovery-required" or bool(loop.get("recovery_required")),
            "dispatch_allowed": bool(loop.get("dispatch_allowed", True)) and outcome != "recovery-required",
            "durable": True,
            "non_mutating": True,
        }

    def loop_policy(self, phase: str | None = None) -> dict[str, Any]:
        """Expose the shared phase policy without creating runtime state."""

        if phase is None:
            state = self._state()
            phase = self._identity(self._loop(state))["phase"]
        try:
            return phase_limits(phase)
        except (LoopContractError, LoopPolicyError) as error:
            raise RuntimeRepairError("loop policy is invalid: " + str(error)) from error

    policy = loop_policy

    def reserve(self, inputs: Mapping[str, Any]) -> dict[str, Any]:
        """Durably reserve one event before a worker/tool can be dispatched."""

        raw_inputs: Any = inputs
        if isinstance(inputs, Mapping) and isinstance(inputs.get("event"), Mapping):
            raw_inputs = dict(inputs)
            supplied_event = inputs["event"]
            for field in (
                "command_id", "event_id", "kind", "attempt", "identity", "predecessor_ref"
            ):
                if field not in raw_inputs and field in supplied_event:
                    raw_inputs[field] = supplied_event[field]
        values = self._inputs(
            raw_inputs,
            {
                "identity", "event", "command_id", "event_id", "kind", "attempt",
                "predecessor_ref", "expected_revision", "idempotency_key",
            },
            {"command_id"},
            "reserve",
        )
        state = self._state()
        loop = self._loop(state)
        identity = self._input_identity(values, loop)
        history = self._history(state)
        command_id = values["command_id"]
        requested_event_id = values.get("event_id")
        replay_event = next(
            (
                item
                for item in history
                if item.get("command_id") == command_id
                or (requested_event_id is not None and item.get("event_id") == requested_event_id)
            ),
            None,
        )
        kind = values.get("kind")
        if kind is None:
            kind = replay_event["kind"] if replay_event is not None else ("initial" if not history else "improvement")
        if kind not in _WORKFLOW_LOOP_EVENT_KINDS:
            raise RuntimeRepairError("reserve kind is unsupported")

        supplied_attempt = values.get("attempt")
        if supplied_attempt is None:
            if replay_event is not None:
                supplied_attempt = replay_event["attempt"]
            elif history:
                # Never silently turn a resumed loop into attempt zero.  A
                # caller may derive the next number from durable history and
                # resubmit it explicitly.
                raise RuntimeRepairError("reserve requires an explicit attempt after the initial event")
            supplied_attempt = 0
        if type(supplied_attempt) is not int or supplied_attempt < 0:
            raise RuntimeRepairError("reserve attempt must be a non-negative integer")
        if replay_event is not None:
            if (
                replay_event["identity"] != identity
                or replay_event["kind"] != kind
                or replay_event["attempt"] != supplied_attempt
                or (requested_event_id is not None and replay_event["event_id"] != requested_event_id)
            ):
                raise RuntimeRepairError("reserve replay does not bind the original event")
        elif not history and (kind != "initial" or supplied_attempt != 0):
            raise RuntimeRepairError("the first loop event must be initial attempt zero")
        if replay_event is None and history and (kind == "initial" or supplied_attempt == 0):
            raise RuntimeRepairError("a resumed loop cannot reuse initial attempt zero")

        predecessor = replay_event.get("predecessor_ref") if replay_event is not None else values.get("predecessor_ref")
        if replay_event is None and history:
            expected_predecessor = event_ref(history[-1])
            if predecessor is None:
                predecessor = expected_predecessor
            elif predecessor != expected_predecessor:
                raise RuntimeRepairError("reserve predecessor does not bind the latest durable event")
        elif replay_event is None and predecessor is not None:
            raise RuntimeRepairError("the initial event cannot have a predecessor")

        if replay_event is not None:
            event = copy.deepcopy(replay_event)
        else:
            try:
                event = build_iteration_event(
                    identity,
                    command_id,
                    kind=kind,
                    attempt=supplied_attempt,
                    predecessor_ref=predecessor,
                    event_id=requested_event_id,
                    status="reserved",
                    result_ref=None,
                )
            except (LoopContractError, TypeError, ValueError) as error:
                raise RuntimeRepairError("reserve event is invalid: " + str(error)) from error
        if values.get("event") is not None:
            try:
                supplied_event = validate_iteration_event(values["event"])
            except LoopContractError as error:
                raise RuntimeRepairError("reserve event is invalid: " + str(error)) from error
            if supplied_event != event:
                raise RuntimeRepairError("reserve event does not bind the supplied identity or attempt")

        revision = self._expected_revision(values, state)
        idempotency_key = values.get("idempotency_key", "workflow-loop-reserve:" + command_id)
        if not isinstance(idempotency_key, str) or not idempotency_key:
            raise RuntimeRepairError("reserve idempotency_key must be a non-empty string")
        # The call is intentionally the first durable operation.  No dispatch
        # or candidate acceptance is allowed before this CAS succeeds.
        try:
            result = self.kernel.reserve_loop_event(
                event=event,
                expected_revision=revision,
                idempotency_key=idempotency_key,
            )
        except Exception as error:
            raise RuntimeRepairError("loop reservation was rejected: " + str(error)) from error
        return self._transition("reserve", result, event=event)

    begin = reserve
    reserve_iteration = reserve
    begin_loop = reserve

    def reserve_loop_event(self, inputs: Mapping[str, Any] | None = None, **kwargs: Any) -> dict[str, Any]:
        """Kernel-shaped convenience seam for callers migrating incrementally."""

        values = {} if inputs is None else dict(inputs)
        values.update(kwargs)
        return self.reserve(values)

    def mark_running(self, inputs: Mapping[str, Any]) -> dict[str, Any]:
        """Record running only after a durable reservation exists."""

        values = self._inputs(
            inputs,
            {"event_id", "expected_revision", "idempotency_key"},
            {"event_id"},
            "running",
        )
        state = self._state()
        revision = self._expected_revision(values, state)
        event_id = values["event_id"]
        if not isinstance(event_id, str) or not event_id:
            raise RuntimeRepairError("running event_id is required")
        try:
            result = self.kernel.mark_loop_running(
                event_id,
                expected_revision=revision,
                idempotency_key=values.get("idempotency_key", "workflow-loop-running:" + event_id),
            )
        except Exception as error:
            raise RuntimeRepairError("loop running transition was rejected: " + str(error)) from error
        return self._transition("running", result)

    running = mark_running

    def mark_loop_running(self, event_id: str | Mapping[str, Any], **kwargs: Any) -> dict[str, Any]:
        values = dict(event_id) if isinstance(event_id, Mapping) else {"event_id": event_id}
        values.update(kwargs)
        return self.mark_running(values)

    def accept_result(self, inputs: Mapping[str, Any]) -> dict[str, Any]:
        """Accept a digest-bound result for the latest running/reserved event."""

        values = self._inputs(
            inputs,
            {"event_id", "result_ref", "expected_revision", "idempotency_key"},
            {"event_id", "result_ref"},
            "accept-result",
        )
        try:
            result_ref = validate_ref(values["result_ref"], "result_ref")
        except LoopContractError as error:
            raise RuntimeRepairError(str(error)) from error
        state = self._state()
        revision = self._expected_revision(values, state)
        event_id = values["event_id"]
        if not isinstance(event_id, str) or not event_id:
            raise RuntimeRepairError("accept-result event_id is required")
        try:
            result = self.kernel.accept_loop_result(
                event_id,
                result_ref,
                expected_revision=revision,
                idempotency_key=values.get("idempotency_key"),
            )
        except Exception as error:
            if "unknown" in str(error).lower() or "recovery" in str(error).lower():
                raise RuntimeRepairError("execution outcome is unknown; explicit recovery is required") from error
            raise RuntimeRepairError("loop result was rejected: " + str(error)) from error
        return self._transition("accept-result", result)

    accept = accept_result

    def accept_loop_result(
        self,
        event_id: str | Mapping[str, Any],
        result_ref: Mapping[str, Any] | None = None,
        **kwargs: Any,
    ) -> dict[str, Any]:
        values = dict(event_id) if isinstance(event_id, Mapping) else {"event_id": event_id, "result_ref": result_ref}
        if result_ref is not None and isinstance(event_id, Mapping):
            values.setdefault("result_ref", result_ref)
        values.update(kwargs)
        return self.accept_result(values)

    def mark_execution_unknown(self, inputs: Mapping[str, Any]) -> dict[str, Any]:
        """Persist an ambiguous dispatch result; callers must recover explicitly."""

        values = self._inputs(
            inputs,
            {"event_id", "expected_revision", "idempotency_key"},
            {"event_id"},
            "execution-unknown",
        )
        state = self._state()
        revision = self._expected_revision(values, state)
        event_id = values["event_id"]
        try:
            result = self.kernel.mark_loop_execution_unknown(
                event_id,
                expected_revision=revision,
                idempotency_key=values.get("idempotency_key", "workflow-loop-unknown:" + event_id),
            )
        except Exception as error:
            raise RuntimeRepairError("could not persist execution-unknown: " + str(error)) from error
        return self._transition("execution-unknown", result)

    execution_unknown = mark_execution_unknown

    def mark_loop_execution_unknown(self, event_id: str | Mapping[str, Any], **kwargs: Any) -> dict[str, Any]:
        values = dict(event_id) if isinstance(event_id, Mapping) else {"event_id": event_id}
        values.update(kwargs)
        return self.mark_execution_unknown(values)

    def recover_execution(self, inputs: Mapping[str, Any]) -> dict[str, Any]:
        """Resolve execution-unknown only with an explicit, bound proof."""

        values = self._inputs(
            inputs,
            {
                "event_id", "resolution", "evidence_ref", "result_ref", "retry_event",
                "retry_command_id", "retry_event_id", "expected_revision", "idempotency_key",
            },
            {"event_id", "resolution", "evidence_ref"},
            "recover",
        )
        if values["resolution"] not in {"accept-result", "retry"}:
            raise RuntimeRepairError("recovery requires an explicit accept-result or retry resolution")
        try:
            evidence_ref = validate_ref(values["evidence_ref"], "evidence_ref")
        except LoopContractError as error:
            raise RuntimeRepairError(str(error)) from error
        result_ref = values.get("result_ref")
        if values["resolution"] == "accept-result":
            if result_ref is None:
                raise RuntimeRepairError("accept-result recovery requires result_ref")
            try:
                result_ref = validate_ref(result_ref, "result_ref")
            except LoopContractError as error:
                raise RuntimeRepairError(str(error)) from error
        elif values.get("retry_event") is None and not {
            "retry_command_id", "retry_event_id"
        }.issubset(values):
            raise RuntimeRepairError("retry recovery requires an explicit retry event identity")

        state = self._state()
        revision = self._expected_revision(values, state)
        kwargs: dict[str, Any] = {
            "resolution": values["resolution"],
            "evidence_ref": evidence_ref,
            "result_ref": result_ref,
            "retry_event": copy.deepcopy(values.get("retry_event")),
            "retry_command_id": values.get("retry_command_id"),
            "retry_event_id": values.get("retry_event_id"),
            "expected_revision": revision,
            "idempotency_key": values.get("idempotency_key"),
        }
        try:
            result = self.kernel.recover_loop_execution(values["event_id"], **kwargs)
        except Exception as error:
            raise RuntimeRepairError("explicit execution recovery was rejected: " + str(error)) from error
        return self._transition("recover", result)

    recover = recover_execution

    def recover_loop_execution(self, event_id: str | Mapping[str, Any], **kwargs: Any) -> dict[str, Any]:
        values = dict(event_id) if isinstance(event_id, Mapping) else {"event_id": event_id}
        values.update(kwargs)
        return self.recover_execution(values)

    def transition_phase(self, identity: Mapping[str, Any], *, reason: str = "phase-transition", expected_revision: int | None = None) -> dict[str, Any]:
        """Advance the phase while preserving the Kernel's archived counters."""

        try:
            new_identity = validate_work_identity(identity)
        except LoopContractError as error:
            raise RuntimeRepairError(str(error)) from error
        state = self._state()
        revision = state.get("revision") if expected_revision is None else expected_revision
        if type(revision) is not int or revision < 0:
            raise RuntimeRepairError("phase transition expected_revision is invalid")
        try:
            result = self.kernel.transition_loop_phase(
                new_identity,
                reason=reason,
                expected_revision=revision,
            )
        except Exception as error:
            raise RuntimeRepairError("loop phase transition was rejected: " + str(error)) from error
        return self._transition("phase-transition", result)

    # -- pure repair, review, evidence, and completion seams --------------

    def plan_repair_batch(self, findings: Sequence[Mapping[str, Any]], *, candidate_digest: str | None = None) -> dict[str, Any]:
        if isinstance(findings, (str, bytes)) or not isinstance(findings, Sequence):
            raise RuntimeRepairError("repair findings must be a sequence")
        if candidate_digest is not None:
            try:
                require_digest(candidate_digest, "candidate_digest")
            except LoopContractError as error:
                raise RuntimeRepairError(str(error)) from error
        for index, finding in enumerate(findings):
            if candidate_digest is not None and isinstance(finding, Mapping) and finding.get("candidate_digest") != candidate_digest:
                raise RuntimeRepairError(f"required Finding[{index}] belongs to another candidate")
        try:
            return plan_fix_batches(copy.deepcopy(list(findings)))
        except (RepairBatchError, TypeError, ValueError) as error:
            raise RuntimeRepairError("repair batch is invalid: " + str(error)) from error

    build_repair_batch = plan_repair_batch
    plan_fix_batches = plan_repair_batch

    def resolve_repair_batch(self, batch: Mapping[str, Any], resolutions: Sequence[Mapping[str, Any]]) -> dict[str, Any]:
        try:
            return assess_batch_resolution(copy.deepcopy(dict(batch)), copy.deepcopy(list(resolutions)))
        except (RepairBatchError, TypeError, ValueError) as error:
            raise RuntimeRepairError("Finding resolution is invalid: " + str(error)) from error

    assess_batch_resolution = resolve_repair_batch

    def repair_batch(self, inputs: Mapping[str, Any]) -> dict[str, Any]:
        values = self._inputs(
            inputs,
            {"findings", "repair_findings", "candidate_digest", "batch_resolutions", "resolutions"},
            set(),
            "repair-batch",
        )
        findings = values.get("findings", values.get("repair_findings"))
        if findings is None:
            raise RuntimeRepairError("repair-batch requires findings")
        plan = self.plan_repair_batch(findings, candidate_digest=values.get("candidate_digest"))
        raw_resolutions = values.get("batch_resolutions", values.get("resolutions"))
        resolutions: list[dict[str, Any]] = []
        if raw_resolutions is not None:
            for batch in plan["batches"]:
                if isinstance(raw_resolutions, Mapping):
                    supplied = raw_resolutions.get(batch["batch_id"])
                    if supplied is None:
                        supplied = [
                            raw_resolutions[finding_id]
                            for finding_id in batch["finding_ids"]
                            if finding_id in raw_resolutions
                        ]
                else:
                    supplied = [
                        item
                        for item in raw_resolutions
                        if isinstance(item, Mapping)
                        and item.get("finding_id") in batch["finding_ids"]
                    ]
                if supplied is None:
                    continue
                if isinstance(raw_resolutions, Mapping) and isinstance(supplied, Mapping):
                    supplied = [supplied]
                if not supplied:
                    continue
                resolutions.append(self.resolve_repair_batch(batch, supplied))
        complete = bool(resolutions) and all(item["complete"] for item in resolutions)
        return {
            "schema": _WORKFLOW_LOOP_BATCH_SCHEMA,
            "contract_version": self.contract_version,
            "plan": plan,
            "repair_batch_plan": copy.deepcopy(plan),
            "resolutions": resolutions,
            "batch_resolutions": copy.deepcopy(resolutions),
            "complete": complete,
            "next": "delta-review" if complete else "repair",
            "non_mutating": True,
        }

    repair = repair_batch

    def build_review_packages(
        self,
        candidate: Mapping[str, Any],
        requirements: Sequence[Mapping[str, Any]] | None = None,
        prior_findings: Sequence[Mapping[str, Any]] | None = None,
        impact: Mapping[str, Any] | None = None,
        *,
        assignments: Any = None,
        requested_mode: str = "delta",
        worker_actor_id: str | None = None,
        worker_context_epoch: str | None = None,
        current_context_epoch: str | None = None,
    ) -> dict[str, Any]:
        """Build both independent fresh delta packages for a repair batch."""

        if requirements is None and isinstance(candidate, Mapping) and {
            "candidate", "requirements", "prior_findings", "impact"
        }.issubset(candidate):
            values = dict(candidate)
            self._reject_progress_fields(values, "review packages")
            requirements = values["requirements"]
            prior_findings = values["prior_findings"]
            impact = values["impact"]
            assignments = values.get("assignments", values.get("review_assignments", values.get("assignment")))
            requested_mode = values.get("requested_mode", "delta")
            worker_actor_id = values.get("worker_actor_id")
            worker_context_epoch = values.get("worker_context_epoch")
            current_context_epoch = values.get("current_context_epoch")
            candidate = values["candidate"]
        if requirements is None or prior_findings is None or impact is None or assignments is None:
            raise RuntimeRepairError("fresh delta review requires candidate, requirements, findings, impact, and assignments")
        if requested_mode != "delta":
            raise RuntimeRepairError("repair rereview packages must use delta mode")
        normalized = self._review_assignments(assignments)
        actor_ids = [normalized[axis]["actor_id"] for axis in REQUIRED_REVIEW_AXES]
        contexts = [normalized[axis]["context_epoch"] for axis in REQUIRED_REVIEW_AXES]
        if len(set(actor_ids)) != len(actor_ids) or len(set(contexts)) != len(contexts):
            raise RuntimeRepairError("required review axes must use independent actors and contexts")
        if worker_actor_id is not None and worker_actor_id in actor_ids:
            raise RuntimeRepairError("review actor must differ from the repair Worker")
        if worker_context_epoch is not None and worker_context_epoch in contexts:
            raise RuntimeRepairError("review context must differ from the repair Worker context")
        if current_context_epoch is not None and current_context_epoch in contexts:
            raise RuntimeRepairError("review context must be fresh relative to the current context")
        packages: list[dict[str, Any]] = []
        try:
            for axis in REQUIRED_REVIEW_AXES:
                packages.append(
                    build_review_package(
                        candidate,
                        requirements,
                        prior_findings,
                        impact,
                        requested_mode="delta",
                        axis=axis,
                        assignment=normalized[axis],
                    )
                )
        except (ReviewPackageError, TypeError, ValueError) as error:
            raise RuntimeRepairError("review package is invalid: " + str(error)) from error
        candidate_digest = packages[0]["candidate"]["candidate_ref"]["digest"]
        if any(item["candidate"]["candidate_ref"]["digest"] != candidate_digest for item in packages):
            raise RuntimeRepairError("review packages do not share one candidate")
        unsigned = {"contract_version": self.contract_version, "candidate_digest": candidate_digest, "packages": packages}
        return {
            "schema": "loop-review-package-set/v1",
            **unsigned,
            "review_packages": copy.deepcopy(packages),
            "package_set_digest": canonical_digest(unsigned),
            "non_mutating": True,
        }

    review_packages = build_review_packages
    build_delta_review_packages = build_review_packages
    build_review_package_set = build_review_packages

    def accept_review_packages(self, packages: Any, results: Any) -> dict[str, Any]:
        if isinstance(packages, Mapping) and packages.get("schema") == "loop-review-package-set/v1":
            package_values = packages.get("packages")
            if "review_packages" in packages and packages["review_packages"] != package_values:
                raise RuntimeRepairError("review package aliases disagree")
            expected_set_digest = packages.get("package_set_digest")
            unsigned = {
                "contract_version": packages.get("contract_version"),
                "candidate_digest": packages.get("candidate_digest"),
                "packages": package_values,
            }
            if expected_set_digest != canonical_digest(unsigned):
                raise RuntimeRepairError("review package set digest is stale")
        else:
            package_values = packages
        if not isinstance(package_values, Sequence) or isinstance(package_values, (str, bytes)):
            raise RuntimeRepairError("review packages must be a sequence")
        by_axis = {}
        for package in package_values:
            if not isinstance(package, Mapping) or package.get("axis") not in REQUIRED_REVIEW_AXES:
                raise RuntimeRepairError("review package does not identify a required axis")
            if package.get("mode") not in {"delta", "full-impact-unknown"} or package.get("fresh_review_required") is not True:
                raise RuntimeRepairError("fresh delta or explicit full-impact-unknown review package is required")
            if package["axis"] in by_axis:
                raise RuntimeRepairError("review packages repeat an axis")
            by_axis[package["axis"]] = package
        if set(by_axis) != set(REQUIRED_REVIEW_AXES):
            raise RuntimeRepairError("both required review axes must be supplied")
        candidate_digests = {
            package["candidate"]["candidate_ref"]["digest"]
            for package in by_axis.values()
        }
        if len(candidate_digests) != 1:
            raise RuntimeRepairError("review packages must share one current candidate")
        if (
            isinstance(packages, Mapping)
            and packages.get("candidate_digest") != next(iter(candidate_digests))
        ):
            raise RuntimeRepairError("review package set candidate binding is stale")
        if isinstance(results, Mapping):
            result_values = list(results.values()) if "schema" not in results else [results]
        elif isinstance(results, Sequence) and not isinstance(results, (str, bytes)):
            result_values = list(results)
        else:
            raise RuntimeRepairError("review results must be a sequence or axis mapping")
        accepted = []
        seen = set()
        try:
            for result in result_values:
                if not isinstance(result, Mapping) or result.get("axis") not in by_axis:
                    raise RuntimeRepairError("review result does not identify an issued package")
                axis = result["axis"]
                if axis in seen:
                    raise RuntimeRepairError("review results repeat an axis")
                seen.add(axis)
                accepted.append(accept_review_result(by_axis[axis], result))
        except ReviewPackageError as error:
            raise RuntimeRepairError("review result is stale or incomplete: " + str(error)) from error
        if seen != set(REQUIRED_REVIEW_AXES):
            raise RuntimeRepairError("both fresh review results are required")
        actors = [item["actor_id"] for item in accepted]
        contexts = [item["context_epoch"] for item in accepted]
        if len(set(actors)) != len(actors) or len(set(contexts)) != len(contexts):
            raise RuntimeRepairError("review results must come from independent actors and contexts")
        return {
            "schema": _WORKFLOW_LOOP_REVIEW_SCHEMA,
            "contract_version": self.contract_version,
            "candidate_digest": next(iter(candidate_digests)),
            "package_digests": {axis: by_axis[axis]["package_digest"] for axis in REQUIRED_REVIEW_AXES},
            "reviews": accepted,
            "accepted_reviews": copy.deepcopy(accepted),
            "complete": True,
            "non_mutating": True,
        }

    accept_reviews = accept_review_packages
    accept_delta_reviews = accept_review_packages

    def evidence_validity(self, receipt: Mapping[str, Any], current_inputs: Mapping[str, Any], change_impact: Mapping[str, Any]) -> dict[str, Any]:
        try:
            return assess_evidence(copy.deepcopy(receipt), copy.deepcopy(current_inputs), copy.deepcopy(change_impact))
        except EvidenceValidityError as error:
            raise RuntimeRepairError("evidence validity could not be established: " + str(error)) from error

    assess_evidence = evidence_validity
    assess_evidence_validity = evidence_validity

    def classify_completion(self, request: Mapping[str, Any]) -> dict[str, Any]:
        # A larger status projection may carry legacy budget observations.
        # They are stripped before the pure classifier and therefore cannot
        # influence the result; they are not a progress authority here.
        values = copy.deepcopy(dict(request))
        values.pop("schema", None)
        for field in _WORKFLOW_LOOP_FORBIDDEN_PROGRESS_FIELDS:
            values.pop(field, None)
        try:
            return classify_completion(values)
        except CompletionError as error:
            raise RuntimeRepairError("completion classification was rejected: " + str(error)) from error

    classify = classify_completion

    def complete(self, request: Mapping[str, Any], *, receipt_id: str | None = None) -> dict[str, Any]:
        """Validate the full v1 repair/review path and issue no LLM verdict."""

        try:
            # WorkflowLoopValidator owns the strict-zero-finding predicate and
            # invokes the pure batch/review/evidence seams before completion.
            from .execution_v2 import WorkflowLoopValidator

            result = WorkflowLoopValidator().validate(copy.deepcopy(dict(request)), receipt_id=receipt_id)
        except Exception as error:
            if isinstance(error, (RuntimeRepairError,)):
                raise
            raise RuntimeRepairError("workflow-loop completion was rejected: " + str(error)) from error
        return result

    evaluate_completion = complete
    mechanical_completion = complete
    validate_completion = complete
    completion = complete

    def machine_decision_receipt(self, classification: Mapping[str, Any], receipt_id: str | None = None) -> dict[str, Any]:
        try:
            return create_machine_decision_receipt(copy.deepcopy(dict(classification)), receipt_id)
        except CompletionError as error:
            raise RuntimeRepairError("machine completion receipt was rejected: " + str(error)) from error

    # -- dispatch/read helpers -------------------------------------------

    def dispatch(self, action: str, inputs: Mapping[str, Any]) -> dict[str, Any]:
        actions = {
            "status": lambda values: self.status(),
            "loop-policy": lambda values: self.loop_policy(values.get("phase")),
            "phase-transition": lambda values: self.transition_phase(
                values["identity"],
                reason=values.get("reason", "phase-transition"),
                expected_revision=values.get("expected_revision"),
            ),
            "reserve": self.reserve,
            "begin": self.reserve,
            "running": self.mark_running,
            "mark-running": self.mark_running,
            "accept": self.accept_result,
            "accept-result": self.accept_result,
            "execution-unknown": self.mark_execution_unknown,
            "mark-execution-unknown": self.mark_execution_unknown,
            "recover": self.recover_execution,
            "repair-batch": self.repair_batch,
            "repair": self.repair_batch,
            "review-packages": self.build_review_packages,
            "review-results": lambda values: self.accept_review_packages(
                values.get("packages", values.get("review_packages")),
                values.get("results", values.get("reviews")),
            ),
            "evidence-validity": lambda values: self.evidence_validity(
                values["receipt"], values["current_inputs"], values["change_impact"]
            ),
            "completion": self.complete,
        }
        method = actions.get(action)
        if method is None:
            raise RuntimeRepairError("workflow-loop action is unsupported: " + str(action))
        return method(copy.deepcopy(dict(inputs)))

    def _state(self) -> Mapping[str, Any]:
        reader = getattr(self.kernel, "read_state", None) or getattr(self.kernel, "snapshot", None)
        if not callable(reader):
            raise RuntimeRepairError("workflow-loop Kernel must expose read_state()")
        state = reader()
        if not isinstance(state, Mapping):
            raise RuntimeRepairError("workflow-loop Kernel state is malformed")
        return state

    @staticmethod
    def _loop(state: Mapping[str, Any]) -> Mapping[str, Any]:
        loop = state.get("loop_control")
        if not isinstance(loop, Mapping):
            raise RuntimeRepairError("workflow-loop/v1 requires loop_control state")
        return loop

    @staticmethod
    def _identity(loop: Mapping[str, Any]) -> dict[str, Any]:
        try:
            return validate_work_identity(loop.get("identity"))
        except LoopContractError as error:
            raise RuntimeRepairError("loop identity is invalid: " + str(error)) from error

    def _history(self, state: Mapping[str, Any]) -> list[dict[str, Any]]:
        loop = self._loop(state)
        for key in ("history", "events"):
            value = loop.get(key)
            if isinstance(value, list):
                return copy.deepcopy(value)
        loader = getattr(self.kernel, "_loop_history", None)
        if callable(loader):
            try:
                value = loader(state)
            except Exception as error:
                raise RuntimeRepairError("loop history cannot be loaded: " + str(error)) from error
            if isinstance(value, list):
                return copy.deepcopy(value)
        refs = loop.get("event_refs")
        reader = getattr(self.kernel, "read_object", None)
        if isinstance(refs, list) and callable(reader):
            result = []
            try:
                for ref in refs:
                    result.append(copy.deepcopy(reader(ref)["payload"]))
            except Exception as error:
                raise RuntimeRepairError("loop event history is unavailable: " + str(error)) from error
            return result
        return []

    def _head(self) -> dict[str, Any] | None:
        getter = getattr(self.kernel, "head", None)
        if not callable(getter):
            return None
        value = getter()
        if not isinstance(value, Mapping):
            return None
        digest = value.get("transaction_digest") or value.get("digest")
        if digest is None:
            return copy.deepcopy(dict(value))
        return {"revision": value.get("revision"), "transaction_digest": digest}

    @staticmethod
    def _policy(identity: Mapping[str, Any]) -> dict[str, Any]:
        try:
            return phase_limits(identity["phase"])
        except (LoopContractError, LoopPolicyError, KeyError) as error:
            raise RuntimeRepairError("loop policy is invalid: " + str(error)) from error

    @staticmethod
    def _inputs(inputs: Any, allowed: set[str], required: set[str], label: str) -> dict[str, Any]:
        if not isinstance(inputs, Mapping):
            raise RuntimeRepairError(label + " inputs must be a mapping")
        values = copy.deepcopy(dict(inputs))
        unknown = set(values) - allowed
        if unknown:
            raise RuntimeRepairError(label + " inputs contain unsupported fields: " + ", ".join(sorted(unknown)))
        missing = required - set(values)
        if missing:
            raise RuntimeRepairError(label + " inputs are missing: " + ", ".join(sorted(missing)))
        WorkflowLoopRepairCoordinator._reject_progress_fields(values, label)
        return values

    @staticmethod
    def _reject_progress_fields(values: Any, label: str) -> None:
        if isinstance(values, Mapping):
            forbidden = _WORKFLOW_LOOP_FORBIDDEN_PROGRESS_FIELDS.intersection(values)
            if forbidden:
                raise RuntimeRepairError(label + " may not use wall-clock or review-budget progress controls: " + ", ".join(sorted(forbidden)))

    def _input_identity(self, values: Mapping[str, Any], loop: Mapping[str, Any]) -> dict[str, Any]:
        identity = self._identity(loop)
        if "identity" in values:
            try:
                supplied = validate_work_identity(values["identity"])
            except LoopContractError as error:
                raise RuntimeRepairError("reserve identity is invalid: " + str(error)) from error
            if supplied != identity:
                raise RuntimeRepairError("reserve identity does not match the current loop")
        return identity

    @staticmethod
    def _expected_revision(values: Mapping[str, Any], state: Mapping[str, Any]) -> int:
        revision = values.get("expected_revision", state.get("revision"))
        if type(revision) is not int or revision < 0:
            raise RuntimeRepairError("expected_revision must be a non-negative integer")
        return revision

    def _transition(self, operation: str, result: Any, *, event: Mapping[str, Any] | None = None) -> dict[str, Any]:
        state = result if isinstance(result, Mapping) and isinstance(result.get("loop_control"), Mapping) else self._state()
        loop = self._loop(state)
        history = self._history(state)
        identity = self._identity(loop)
        latest = history[-1] if history else None
        return {
            "schema": _WORKFLOW_LOOP_TRANSITION_SCHEMA,
            "contract_version": self.contract_version,
            "operation": operation,
            "revision": state.get("revision"),
            "head": self._head(),
            "identity": copy.deepcopy(identity),
            "policy": self._policy(identity),
            "loop_control": copy.deepcopy(loop),
            "event": copy.deepcopy(event if event is not None else latest),
            "latest_event": copy.deepcopy(latest),
            "durable": True,
            "non_mutating": True,
        }

    @staticmethod
    def _review_assignments(value: Any) -> dict[str, dict[str, Any]]:
        if isinstance(value, Mapping) and {"assignment_id", "actor_id", "context_epoch"}.issubset(value):
            values = {axis: copy.deepcopy(dict(value)) for axis in REQUIRED_REVIEW_AXES}
        elif isinstance(value, Mapping):
            values = {axis: copy.deepcopy(dict(value[axis])) for axis in REQUIRED_REVIEW_AXES if axis in value}
            if set(value) - set(REQUIRED_REVIEW_AXES):
                raise RuntimeRepairError("review assignments name an unsupported axis")
        elif isinstance(value, Sequence) and not isinstance(value, (str, bytes)):
            values = {}
            for item in value:
                if not isinstance(item, Mapping) or "axis" not in item:
                    raise RuntimeRepairError("review assignment must name an axis")
                axis = item["axis"]
                if axis not in REQUIRED_REVIEW_AXES:
                    raise RuntimeRepairError("review assignment names an unsupported axis")
                values[axis] = {key: copy.deepcopy(item[key]) for key in item if key != "axis"}
        else:
            raise RuntimeRepairError("review assignments must be a mapping or list")
        if set(values) != set(REQUIRED_REVIEW_AXES):
            raise RuntimeRepairError("both required review assignments are required")
        required = {"assignment_id", "actor_id", "context_epoch"}
        if any(set(item) != required for item in values.values()):
            raise RuntimeRepairError("review assignment has an unsupported shape")
        return values


WorkflowLoopRuntimeRepair = WorkflowLoopRepairCoordinator
RuntimeRepairV1 = WorkflowLoopRepairCoordinator
WorkflowLoopRepair = WorkflowLoopRepairCoordinator


__all__ = [
    "ACCEPTED_RESIDUAL",
    "TRUST_PROFILE",
    "RuntimeRepairCoordinator",
    "RuntimeRepairError",
    "RuntimeRepairV1",
    "WorkflowLoopRepair",
    "WorkflowLoopRepairCoordinator",
    "WorkflowLoopRuntimeRepair",
    "assert_production_adoptable",
]
