"""Adopt an explicit physical B7 receipt into a project-local ControlKernel.

The caller supplies the human response (or a labelled rehearsal response).
Preparing context never grants approval, writes a receipt, or creates a Run.
"""
from __future__ import annotations

import copy
from datetime import datetime, timedelta, timezone
import hashlib
import json
from pathlib import Path
import re
from typing import Mapping

from .control_kernel import ControlKernel, canonical_digest
from .inception_cli import create_file, encoded, locked, read_json
from .loop_contracts import LOOP_CONTRACT_VERSION
from .objective_system import ObjectiveSystemV1


class RuntimeApprovalError(ValueError):
    pass


def _physical(project, ref):
    if (not isinstance(ref, Mapping) or set(ref) != {"path", "version", "digest"}
            or not isinstance(ref["version"], str)
            or not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9_.:@/-]*", ref["version"])):
        raise RuntimeApprovalError("physical path/version/digest reference required")
    path = Path(ref["path"])
    if (not path.is_absolute() or ".." in path.parts or path != path.resolve(strict=True)
            or project not in path.parents
            or any(item.is_symlink() for item in (path, *path.parents))
            or not path.is_file()):
        raise RuntimeApprovalError("physical input must be a regular project-local file")
    raw = path.read_bytes()
    if not raw.strip() or ref["digest"] != "sha256:" + hashlib.sha256(raw).hexdigest():
        raise RuntimeApprovalError("physical input is empty or stale")
    return dict(ref)


def approval_context(project, run_id, *, intake_ref, candidate_ref, proposal_ref, actor_ref, mode):
    """Return binding fields only; caller must supply the explicit decision/time."""
    project = Path(project).resolve(strict=True)
    if (not isinstance(run_id, str) or not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9_-]{0,79}", run_id)
            or mode not in {"real", "rehearsal"}):
        raise RuntimeApprovalError("bounded Run ID and real/rehearsal mode required")
    intake, candidate, proposal, actor = [_physical(project, ref) for ref in (intake_ref, candidate_ref, proposal_ref, actor_ref)]
    actor_value = read_json(actor["path"])
    source = "mock" if mode == "rehearsal" else "human"
    if (not isinstance(actor_value, dict) or actor_value.get("source") != source
            or not isinstance(actor_value.get("actor_id"), str)
            or not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9_.:@/-]*", actor_value["actor_id"])):
        raise RuntimeApprovalError("actor source and identifier must match the Run mode")
    if candidate["version"] == intake["version"]:
        raise RuntimeApprovalError("approved objective version must differ from intake version")
    return {
        "schema": source + "-approval-receipt/v1", "approval_id": "approval-" + run_id + "-" + candidate["version"],
        "source": source, "actor_id": actor_value["actor_id"], "run_id": run_id,
        "namespace": "project:" + run_id, "approval_scope": "project-local", "project_root": str(project), "mode": mode,
        "candidate_path": candidate["path"], "candidate_version": candidate["version"],
        "candidate_namespace": "project:" + run_id, "candidate_digest": candidate["digest"],
        "prior_objective_digest": intake["digest"], "prior_objective_version": intake["version"],
        "proposal_digest": proposal["digest"],
    }


def _snapshot(directory, value, raw=None):
    data = raw if raw is not None else encoded(value)
    path = directory / (hashlib.sha256(data).hexdigest() + ".json")
    if path.exists():
        if path.is_symlink() or path.read_bytes() != data:
            raise RuntimeApprovalError("immutable adoption snapshot differs")
    else:
        create_file(path, data)
    return path


def adopt_approved_objective(
    project,
    run_id,
    *,
    intake_ref,
    candidate_ref,
    proposal_ref,
    actor_ref,
    receipt_ref,
    mode,
    legacy_budget_seconds=None,
    budget_seconds=None,
    preapproval_steps=None,
):
    """Validate the supplied receipt, compile B7, then commit actual approval."""
    project = Path(project).resolve(strict=True)
    context = approval_context(project, run_id, intake_ref=intake_ref, candidate_ref=candidate_ref, proposal_ref=proposal_ref, actor_ref=actor_ref, mode=mode)
    receipt_ref = _physical(project, receipt_ref)
    receipt = read_json(receipt_ref["path"])
    if not isinstance(receipt, dict) or any(receipt.get(key) != value for key, value in context.items()):
        raise RuntimeApprovalError("receipt does not bind these exact physical inputs")
    if receipt.get("decision") != "approve" or receipt.get("explicit") is not True:
        raise RuntimeApprovalError("an explicit supplied approval receipt is required")
    if legacy_budget_seconds is not None and budget_seconds is not None:
        raise RuntimeApprovalError("only one explicit legacy budget field is permitted")
    legacy_seconds = budget_seconds if budget_seconds is not None else legacy_budget_seconds
    if legacy_seconds is not None and (
        type(legacy_seconds) is not int or legacy_seconds <= 0
    ):
        raise RuntimeApprovalError("legacy budget must be a positive integer")
    approval = {key: receipt[key] for key in ("approval_id", "run_id", "namespace", "approval_scope", "decision", "candidate_digest", "candidate_version", "prior_objective_digest", "prior_objective_version", "proposal_digest", "project_root", "mode")}
    approval.update(schema="objective-approval/v1", actor={"kind": context["source"], "actor_id": receipt["actor_id"]}, receipt=receipt)
    # Full Kernel receipt validation happens before a genesis transaction exists.
    ControlKernel._validate_objective_approval_payload(approval)
    issued_at = datetime.fromisoformat(receipt["issued_at"].replace("Z", "+00:00"))
    if issued_at > datetime.now(timezone.utc):
        raise RuntimeApprovalError("approval receipt cannot be issued in the future")
    identity = {key: context[key] for key in ("run_id", "namespace", "approval_scope", "project_root", "mode")}
    identity["schema"] = "project-local-run-identity/v1"
    candidate = {**dict(candidate_ref), "namespace": context["namespace"]}
    loop_identity = {
        "schema": "loop-work-identity/v1",
        "work_lineage_id": run_id,
        "logical_task_id": "group-B-purpose",
        "phase": "B",
        "scope_revision": candidate["version"],
        "requirements_digest": candidate["digest"],
        "predecessor_ref": None,
    }
    loop_control = {"identity": loop_identity, "history": []}
    payload = {"candidate_ref": candidate, "prior_objective": {key: intake_ref[key] for key in ("version", "digest")}, "proposal_digest": proposal_ref["digest"], "approval": approval}
    steps = copy.deepcopy(preapproval_steps)
    if (not isinstance(steps, list) or len(steps) != 6
            or any(not isinstance(step, (list, tuple)) or len(step) != 2 or not isinstance(step[1], Mapping) for step in steps)
            or [step[0] for step in steps] != ["group.B.B%d" % number for number in range(1, 7)]):
        raise RuntimeApprovalError("preapproval steps must contain all B1 through B6 inputs in exact order before Run registration")
    def verify_step_refs(value):
        if isinstance(value, Mapping):
            if "path" in value and "digest" in value:
                raw_path = Path(value["path"])
                path = raw_path if raw_path.is_absolute() else project / raw_path
                _physical(project, {"path": str(path), "version": "v1", "digest": value["digest"]})
            for item in value.values():
                verify_step_refs(item)
        elif isinstance(value, (list, tuple)):
            for item in value:
                verify_step_refs(item)
    verify_step_refs(steps)
    progress_control = (
        {"schema": "legacy-review-budget/v1", "budget_seconds": legacy_seconds}
        if legacy_seconds is not None
        else {"schema": LOOP_CONTRACT_VERSION, "loop_control": loop_control}
    )
    adoption = {"intake": dict(intake_ref), "candidate": dict(candidate_ref), "proposal": dict(proposal_ref), "actor": dict(actor_ref), "receipt": dict(receipt_ref), "identity": identity, "progress_control": progress_control, "preapproval_steps": steps}
    adoption_digest = canonical_digest(adoption)
    kernel = ControlKernel(project, run_id)
    deadline = issued_at + timedelta(seconds=legacy_seconds) if legacy_seconds is not None else None
    if kernel.head() is None and deadline is not None and deadline <= datetime.now(timezone.utc):
        raise RuntimeApprovalError("legacy approval budget expired before Run registration")
    kernel._validate_runtime_identity(identity)
    kernel._validate_runtime_approval_binding(identity, approval, candidate)
    directory = project / ".local" / "agent" / "runtime-approvals" / run_id
    if any(path.is_symlink() for path in (directory, *directory.parents)):
        raise RuntimeApprovalError("adoption storage cannot contain symlinks")
    directory.mkdir(parents=True, exist_ok=True)
    with locked(directory):
        artifact_authority = {"approved": True, "scopes": ["publish_artifact"], "write_scopes": ["artifacts"], "run_id": run_id, "human_receipt": receipt}
        def publish_step(qualified_id, inputs, compiled):
            artifact_id = "runtime-" + qualified_id.rsplit(".", 1)[-1]
            value = {"qualified_id": qualified_id, "inputs": inputs, "compiled": compiled, "objective_digest": candidate["digest"]}
            state = kernel.read_state()
            if artifact_id in state["artifacts"]:
                existing_value = kernel.read_object(state["artifacts"][artifact_id]["object_ref"])["payload"]["payload"]
                if existing_value["inputs"] != inputs or existing_value["objective_digest"] != candidate["digest"]:
                    raise RuntimeApprovalError("published Skill input binding differs")
                return
            kernel.publish_artifact(artifact_id, "v1", value, kind="runtime-skill", path="artifacts/" + artifact_id,
                                    authority_ref=artifact_authority, idempotency_key="publish-" + artifact_id)
        if kernel.head() is not None:
            state = kernel.read_state()
            if state["authority"].get("runtime_adoption_digest") != adoption_digest:
                raise RuntimeApprovalError("Run already belongs to a different immutable adoption")
            existing = state["objective_approvals"].get(approval["approval_id"])
            if existing:
                if kernel.read_object(existing["approval_ref"])["payload"] != approval:
                    raise RuntimeApprovalError("existing objective approval differs")
                saved = read_json(directory / "b7-compiled.json")
                publish_step("group.B.B7", saved["inputs"], saved["compiled"])
                return kernel
        refs = {}
        for name in ("intake", "candidate", "proposal", "actor", "receipt"):
            ref = adoption[name]
            _physical(project, ref)
            path = _snapshot(directory, None, Path(ref["path"]).read_bytes())
            refs[name] = {"path": str(path.relative_to(project)), "digest": ref["digest"], "selector": name}
        owner = {"owner_kind": "human" if mode == "real" else "system", "owner_id": context["actor_id"], "authority_ref": refs["actor"]}
        inputs = {"input_refs": list(refs.values()), "candidate_ref": refs["candidate"], "version": candidate["version"], "candidate": candidate,
                  "prior_objective": payload["prior_objective"], "approval_scope": "project-local", "namespace": context["namespace"], "mode": mode,
                  "approval_receipt": {"receipt_ref": refs["receipt"], "source": receipt["source"], "explicit": receipt["explicit"], "decision": receipt["decision"], "actor_ref": owner}}
        compiler_authority = {"owner_ref": owner, "source_refs": list(refs.values())}
        def compile_step(qualified_id, step_inputs, head):
            result = ObjectiveSystemV1().compile(qualified_id, step_inputs, compiler_authority, head)
            if result.get("schema") != "objective-system-artifact/v1":
                raise RuntimeApprovalError(qualified_id + " refused: " + str(result.get("reason")))
            return result
        def compile_b7(head):
            result = ObjectiveSystemV1().compile("group.B.B7", inputs, compiler_authority, head)
            if result.get("schema") != "objective-approval-command/v1":
                raise RuntimeApprovalError("B7 refused approval: " + str(result.get("reason")))
            _snapshot(directory, {"inputs": inputs, "authority": compiler_authority, "compiled": result, "adoption_digest": adoption_digest})
            return result
        if kernel.head() is None:
            for qualified_id, step_inputs in steps:
                compile_step(qualified_id, step_inputs, {"revision": 0, "transaction_digest": None})
            compile_b7({"revision": 0, "transaction_digest": None})
            if deadline is not None and deadline <= datetime.now(timezone.utc):
                raise RuntimeApprovalError("legacy approval budget expired before Run registration")
            entry = {
                "workflow_version": "operational-workflow/v1" if deadline is not None else "operational-workflow/v2",
                "group_id": "B",
                "epoch_id": "B-01",
                "authority_ref": {"approved": True, "scopes": ["entry"], "human_receipt": receipt, "runtime_identity": identity, "runtime_adoption_digest": adoption_digest},
            }
            if deadline is not None:
                entry["review_budget"] = {"version": "v1", "deadline": deadline.isoformat(), "max_rounds": 2, "max_attempts_per_finding": 5, "rounds_used": 0, "finding_attempts": {}}
            else:
                entry["loop_control"] = loop_control
            kernel.entry(dict(intake_ref), **entry)
        for qualified_id, step_inputs in steps:
            current = kernel.head()
            result = compile_step(qualified_id, step_inputs, {key: current[key] for key in ("revision", "transaction_digest")})
            publish_step(qualified_id, step_inputs, result)
        head = kernel.head()
        compiled = compile_b7({key: head[key] for key in ("revision", "transaction_digest")})
        if compiled["candidate"]["candidate_ref"]["digest"] != candidate["digest"]:
            raise RuntimeApprovalError("B7 candidate digest changed")
        saved_path = directory / "b7-compiled.json"
        saved_value = {"inputs": inputs, "compiled": compiled}
        if saved_path.exists():
            if read_json(saved_path) != saved_value:
                raise RuntimeApprovalError("pre-approval compiler frontier changed")
        else:
            create_file(saved_path, encoded(saved_value))
        authority = {key: approval[key] for key in ("run_id", "namespace", "approval_scope", "proposal_digest", "project_root", "mode")}
        authority.update(status="approved", scopes=["approve_objective"], actor_id=receipt["actor_id"], human_receipt=receipt, protected_fields=["objective_ref"], write_scopes=[context["namespace"]])
        command = kernel.make_command("approve_objective", payload, authority_ref=authority, expected_head=head,
                                      idempotency_key="adopt-" + approval["approval_id"], protected_fields=["objective_ref"], scope=[context["namespace"]])
        kernel.apply(command)
        publish_step("group.B.B7", inputs, compiled)
        return kernel


__all__ = ["RuntimeApprovalError", "approval_context", "adopt_approved_objective"]
