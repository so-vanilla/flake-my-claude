"""Post-E6 practical repair protocol for one persisted Group E Finding.

The Codex host and the parent Orchestrator are trusted by the approved
``practical/trusted-parent/v1`` profile.  This module proves causal and
content consistency inside the current Run; it intentionally does not claim
cryptographic authentication of AI principals.
"""
from __future__ import annotations

import copy
from datetime import datetime, timezone
import hashlib
import json
from pathlib import Path
import re
import secrets
import subprocess
from typing import Any, Mapping, Sequence

from .execution_v2 import ExecutionClosureBuilder
from .inception_cli import InceptionError
from .inception_runtime import InceptionRuntime
from .runtime_execution import RuntimeExecution, RuntimeExecutionError, _digest, _head


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


__all__ = [
    "RuntimeRepairCoordinator",
    "RuntimeRepairError",
    "TRUST_PROFILE",
    "ACCEPTED_RESIDUAL",
    "assert_production_adoptable",
]
