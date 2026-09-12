"""Shared, versioned contracts for evidence-bounded workflow loops.

The module deliberately contains validation and identity rules only.  It does
not read the clock, dispatch work, persist Run state, or decide that a human
approval exists.  Loop components can therefore share one wire contract while
the Control Kernel remains the only durable writer.
"""
from __future__ import annotations

import copy
import hashlib
import json
import re
from collections.abc import Mapping, Sequence
from typing import Any

LOOP_CONTRACT_VERSION = "workflow-loop/v1"
REQUIRED_REVIEW_AXES = ("architecture-safety", "integration-operability")
COMPLETION_STATUSES = ("pass", "fail", "unknown")
TERMINAL_OUTCOMES = (
    "completed",
    "needs-input",
    "stalled",
    "iteration-limit",
    "execution-failed",
    "recovery-required",
)

_DIGEST = re.compile(r"^sha256:[0-9a-f]{64}$")
_IDENTIFIER = re.compile(r"^[A-Za-z0-9][A-Za-z0-9._:/-]*$")
_PHASE = re.compile(r"^(?:group\.)?([BCDEGH])(?:\.([BCDEGH]\d+)|(\d+))?$")
_POLICIES = {
    "B": (2, "purpose"),
    "C": (2, "outcome-system"),
    "D1-D4": (2, "specification-unit"),
    "D5": (1, "option-set"),
    "D6-D12": (2, "planning-system"),
    "E3-E7": (3, "logical-task"),
    "E8-E9": (2, "integration-candidate"),
    "G-H": (1, "record-or-audit-target"),
}


class LoopContractError(ValueError):
    """A loop value is malformed or violates the shared contract."""


def canonical_digest(value: Any) -> str:
    """Return the repository's canonical JSON digest for a JSON value."""
    try:
        encoded = json.dumps(
            value,
            ensure_ascii=False,
            sort_keys=True,
            separators=(",", ":"),
            allow_nan=False,
        ).encode("utf-8")
    except (TypeError, ValueError) as error:
        raise LoopContractError("value must be canonical JSON") from error
    return "sha256:" + hashlib.sha256(encoded).hexdigest()


def require_digest(value: Any, label: str) -> str:
    if not isinstance(value, str) or _DIGEST.fullmatch(value) is None:
        raise LoopContractError(label + " must be a sha256 digest")
    return value


def require_identifier(value: Any, label: str) -> str:
    if not isinstance(value, str) or _IDENTIFIER.fullmatch(value) is None:
        raise LoopContractError(label + " must be a non-empty stable identifier")
    return value


def validate_ref(value: Any, label: str, *, nullable: bool = False) -> dict[str, str] | None:
    if value is None and nullable:
        return None
    if not isinstance(value, Mapping) or not set(value).issubset({"id", "path", "digest"}):
        raise LoopContractError(label + " must be a physical or immutable reference")
    if set(value) not in ({"id", "digest"}, {"path", "digest"}, {"id", "path", "digest"}):
        raise LoopContractError(label + " must bind id or path and digest")
    if "id" in value:
        require_identifier(value["id"], label + ".id")
    if "path" in value and (not isinstance(value["path"], str) or not value["path"]):
        raise LoopContractError(label + ".path must be non-empty")
    require_digest(value.get("digest"), label + ".digest")
    return copy.deepcopy(dict(value))


def _exact_mapping(value: Any, fields: set[str], label: str) -> Mapping[str, Any]:
    if not isinstance(value, Mapping) or set(value) != fields:
        raise LoopContractError(label + " has an unsupported shape")
    return value


def _string_list(value: Any, label: str, *, unique: bool = True) -> list[str]:
    if (
        not isinstance(value, list)
        or not all(isinstance(item, str) and bool(item) for item in value)
        or (unique and len(value) != len(set(value)))
    ):
        raise LoopContractError(label + " must be a list of non-empty strings")
    return copy.deepcopy(value)


def phase_policy(phase: Any) -> dict[str, Any]:
    """Resolve a concrete phase selector to its shared counter policy."""
    if not isinstance(phase, str):
        raise LoopContractError("phase must be a supported selector")
    match = _PHASE.fullmatch(phase)
    if match is None:
        raise LoopContractError("phase must be one of B, C, D1-D12, E3-E9, G, or H")
    group = match.group(1)
    suffix = match.group(2) or (group + match.group(3) if match.group(3) else group)
    number = int(suffix[1:]) if len(suffix) > 1 else None
    if group == "B":
        key = "B"
    elif group == "C":
        key = "C"
    elif group == "D" and number is not None and 1 <= number <= 4:
        key = "D1-D4"
    elif group == "D" and number == 5:
        key = "D5"
    elif group == "D" and number is not None and 6 <= number <= 12:
        key = "D6-D12"
    elif group == "E" and number is not None and 3 <= number <= 7:
        key = "E3-E7"
    elif group == "E" and number is not None and 8 <= number <= 9:
        key = "E8-E9"
    elif group in {"G", "H"}:
        key = "G-H"
    else:
        raise LoopContractError("phase has no automatic improvement-loop policy")
    limit, counter_scope = _POLICIES[key]
    return {
        "schema": "loop-policy/v1",
        "policy_id": key,
        "phase": suffix,
        "additional_iteration_limit": limit,
        "technical_retry_limit": 1,
        "counter_scope": counter_scope,
    }


def validate_work_identity(value: Any) -> dict[str, Any]:
    item = _exact_mapping(
        value,
        {
            "schema", "work_lineage_id", "logical_task_id", "phase",
            "scope_revision", "requirements_digest", "predecessor_ref",
        },
        "work identity",
    )
    if item.get("schema") != "loop-work-identity/v1":
        raise LoopContractError("work identity schema is unsupported")
    require_identifier(item.get("work_lineage_id"), "work_lineage_id")
    require_identifier(item.get("logical_task_id"), "logical_task_id")
    phase_policy(item.get("phase"))
    require_identifier(item.get("scope_revision"), "scope_revision")
    require_digest(item.get("requirements_digest"), "requirements_digest")
    validate_ref(item.get("predecessor_ref"), "predecessor_ref", nullable=True)
    return copy.deepcopy(dict(item))


def counter_identity(identity: Any) -> dict[str, str]:
    """Return the counter key; scope/candidate/session revisions never reset it."""
    item = validate_work_identity(identity)
    policy = phase_policy(item["phase"])
    return {
        "work_lineage_id": item["work_lineage_id"],
        "logical_task_id": item["logical_task_id"],
        "policy_id": policy["policy_id"],
    }


def validate_iteration_event(value: Any) -> dict[str, Any]:
    item = _exact_mapping(
        value,
        {
            "schema", "event_id", "command_id", "identity", "kind", "status",
            "attempt", "predecessor_ref", "result_ref",
        },
        "iteration event",
    )
    if item.get("schema") != "loop-iteration-event/v1":
        raise LoopContractError("iteration event schema is unsupported")
    require_identifier(item.get("event_id"), "event_id")
    require_identifier(item.get("command_id"), "command_id")
    validate_work_identity(item.get("identity"))
    if item.get("kind") not in {"initial", "improvement", "integration-return", "technical-retry"}:
        raise LoopContractError("iteration event kind is unsupported")
    if item.get("status") not in {"reserved", "running", "evaluated", "execution-unknown"}:
        raise LoopContractError("iteration event status is unsupported")
    attempt = item.get("attempt")
    if not isinstance(attempt, int) or isinstance(attempt, bool) or attempt < 0:
        raise LoopContractError("iteration event attempt must be non-negative")
    if (item["kind"] == "initial") != (attempt == 0):
        raise LoopContractError("only the initial event may use attempt zero")
    validate_ref(item.get("predecessor_ref"), "predecessor_ref", nullable=item["kind"] == "initial")
    validate_ref(item.get("result_ref"), "result_ref", nullable=item["status"] != "evaluated")
    return copy.deepcopy(dict(item))


def validate_requirement_assessment(value: Any) -> dict[str, Any]:
    item = _exact_mapping(
        value,
        {"schema", "requirement_id", "status", "scope", "evidence_refs"},
        "requirement assessment",
    )
    if item.get("schema") != "loop-requirement-assessment/v1":
        raise LoopContractError("requirement assessment schema is unsupported")
    require_identifier(item.get("requirement_id"), "requirement_id")
    if item.get("status") not in COMPLETION_STATUSES:
        raise LoopContractError("requirement status is unsupported")
    _string_list(item.get("scope"), "requirement scope")
    refs = item.get("evidence_refs")
    if not isinstance(refs, list):
        raise LoopContractError("evidence_refs must be a list")
    for index, ref in enumerate(refs):
        validate_ref(ref, f"evidence_refs[{index}]")
    if item["status"] == "pass" and not refs:
        raise LoopContractError("a passing requirement must cite evidence")
    return copy.deepcopy(dict(item))


def validate_review_assessment(value: Any) -> dict[str, Any]:
    item = _exact_mapping(
        value,
        {
            "schema", "review_id", "axis", "actor_id", "context_epoch",
            "candidate_digest", "package_digest", "coverage", "completed",
            "unevaluated", "finding_refs",
        },
        "review assessment",
    )
    if item.get("schema") != "loop-review-assessment/v1":
        raise LoopContractError("review assessment schema is unsupported")
    require_identifier(item.get("review_id"), "review_id")
    if item.get("axis") not in REQUIRED_REVIEW_AXES:
        raise LoopContractError("review axis is unsupported")
    require_identifier(item.get("actor_id"), "actor_id")
    require_identifier(item.get("context_epoch"), "context_epoch")
    require_digest(item.get("candidate_digest"), "candidate_digest")
    require_digest(item.get("package_digest"), "package_digest")
    _string_list(item.get("coverage"), "review coverage")
    if not isinstance(item.get("completed"), bool):
        raise LoopContractError("review completed must be boolean")
    _string_list(item.get("unevaluated"), "review unevaluated")
    refs = item.get("finding_refs")
    if not isinstance(refs, list):
        raise LoopContractError("finding_refs must be a list")
    for index, ref in enumerate(refs):
        validate_ref(ref, f"finding_refs[{index}]")
    if item["completed"] and item["unevaluated"]:
        raise LoopContractError("a completed review cannot retain unevaluated scope")
    return copy.deepcopy(dict(item))


def validate_evidence_record(value: Any) -> dict[str, Any]:
    item = _exact_mapping(
        value,
        {
            "schema", "evidence_id", "evidence_digest", "candidate_digest",
            "spec_digest", "source_digest", "dependency_digest",
            "environment_digest", "check_definition_digest", "coverage", "status",
        },
        "evidence record",
    )
    if item.get("schema") != "loop-evidence-record/v1":
        raise LoopContractError("evidence record schema is unsupported")
    require_identifier(item.get("evidence_id"), "evidence_id")
    for field in (
        "evidence_digest", "candidate_digest", "spec_digest", "source_digest",
        "dependency_digest", "environment_digest", "check_definition_digest",
    ):
        require_digest(item.get(field), field)
    _string_list(item.get("coverage"), "evidence coverage")
    if item.get("status") not in COMPLETION_STATUSES:
        raise LoopContractError("evidence status is unsupported")
    unsigned = {key: copy.deepcopy(content) for key, content in item.items() if key != "evidence_digest"}
    if canonical_digest(unsigned) != item["evidence_digest"]:
        raise LoopContractError("evidence_digest does not bind the record")
    return copy.deepcopy(dict(item))


def validate_terminal_record(value: Any) -> dict[str, Any]:
    """Validate one durable, non-authorizing loop stop/completion record."""

    item = _exact_mapping(
        value,
        {
            "schema", "terminal_id", "identity", "outcome", "reason",
            "candidate_ref", "requirements", "reviews", "evidence",
            "open_items", "resume_ref", "non_authorizing",
        },
        "terminal record",
    )
    if item.get("schema") != "loop-terminal-record/v1":
        raise LoopContractError("terminal record schema is unsupported")
    require_identifier(item.get("terminal_id"), "terminal_id")
    validate_work_identity(item.get("identity"))
    if item.get("outcome") not in TERMINAL_OUTCOMES:
        raise LoopContractError("terminal outcome is unsupported")
    if not isinstance(item.get("reason"), str) or not item["reason"]:
        raise LoopContractError("terminal reason must be non-empty")
    validate_ref(item.get("candidate_ref"), "candidate_ref", nullable=True)
    requirements = item.get("requirements")
    reviews = item.get("reviews")
    evidence = item.get("evidence")
    if not isinstance(requirements, list) or not isinstance(reviews, list) or not isinstance(evidence, list):
        raise LoopContractError("terminal assessments must be lists")
    checked_requirements = [validate_requirement_assessment(entry) for entry in requirements]
    checked_reviews = [validate_review_assessment(entry) for entry in reviews]
    checked_evidence = [validate_evidence_record(entry) for entry in evidence]
    validate_unique(checked_requirements, "requirement_id", "terminal requirements")
    validate_unique(checked_reviews, "review_id", "terminal reviews")
    validate_unique(checked_evidence, "evidence_id", "terminal evidence")
    open_items = _string_list(item.get("open_items"), "terminal open_items")
    validate_ref(item.get("resume_ref"), "resume_ref", nullable=True)
    if item.get("non_authorizing") is not True:
        raise LoopContractError("terminal record must be non-authorizing")
    if item["outcome"] == "completed":
        if item.get("candidate_ref") is None:
            raise LoopContractError("completed terminal record requires candidate_ref")
        if not checked_requirements or not checked_reviews or not checked_evidence:
            raise LoopContractError("completed terminal record requires assessments and evidence")
        if open_items or item.get("resume_ref") is not None:
            raise LoopContractError("completed terminal record cannot carry open items or a resume point")
    elif not open_items:
        raise LoopContractError("an incomplete terminal record must preserve open items")
    return copy.deepcopy(dict(item))


def validate_resume_record(value: Any) -> dict[str, Any]:
    """Validate an explicit evidence-bound restart of a stopped loop unit."""

    item = _exact_mapping(
        value,
        {
            "schema", "resume_id", "identity", "terminal_ref", "reason",
            "evidence_ref", "non_authorizing",
        },
        "resume record",
    )
    if item.get("schema") != "loop-resume-record/v1":
        raise LoopContractError("resume record schema is unsupported")
    require_identifier(item.get("resume_id"), "resume_id")
    validate_work_identity(item.get("identity"))
    validate_ref(item.get("terminal_ref"), "terminal_ref")
    if not isinstance(item.get("reason"), str) or not item["reason"]:
        raise LoopContractError("resume reason must be non-empty")
    validate_ref(item.get("evidence_ref"), "evidence_ref")
    if item.get("non_authorizing") is not True:
        raise LoopContractError("resume record must be non-authorizing")
    return copy.deepcopy(dict(item))


def validate_unique(items: Sequence[Mapping[str, Any]], field: str, label: str) -> None:
    values = [item.get(field) for item in items]
    if any(not isinstance(value, str) or not value for value in values) or len(values) != len(set(values)):
        raise LoopContractError(label + " must have unique " + field)
