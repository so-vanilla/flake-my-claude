"""Pure completion classification for the versioned workflow loop contract.

The loop contracts describe the wire values used by this module.  This module
only joins those values; it does not read a clock, run a check, call a model,
write Run state, or grant an authority.  In particular, a machine completion
decision is not a review verdict and is never a human approval, a D5 choice,
or an H objective outcome.
"""
from __future__ import annotations

import copy
from collections.abc import Mapping, Sequence
from typing import Any

from .loop_contracts import (
    REQUIRED_REVIEW_AXES,
    TERMINAL_OUTCOMES,
    LoopContractError,
    canonical_digest,
    require_digest,
    require_identifier,
    validate_evidence_record,
    validate_requirement_assessment,
    validate_review_assessment,
    validate_work_identity,
)

CLASSIFICATION_SCHEMA = "loop-completion-classification/v1"
MACHINE_DECISION_RECEIPT_SCHEMA = "loop-machine-decision-receipt/v1"
MACHINE_DECIDER = {
    "kind": "deterministic-machine",
    "name": "completion-classifier",
    "version": "v1",
}
_NON_AUTHORISING_SCOPE = "completion-classification-only"
_NOT_EVALUATED = False


class CompletionError(LoopContractError):
    """A completion request or machine decision receipt is malformed."""


class CompletionClassificationError(CompletionError):
    """Compatibility name for callers that distinguish classification errors."""


class MachineDecisionReceiptError(CompletionError):
    """A machine decision receipt is malformed or not self-consistent."""


_REQUEST_ALIASES = {
    "current_candidate_digest": "candidate_digest",
    "current_package_digest": "package_digest",
    "requirement_assessments": "requirements",
    "required_assessments": "requirements",
    "review_assessments": "reviews",
    "evidence_records": "evidence",
    "open_findings": "open_required_findings",
    "mandatory_unknown": "mandatory_unknowns",
    "conflicts": "contradictions",
}
_REQUEST_KEYS = {
    "identity",
    "candidate_digest",
    "package_digest",
    "requirements",
    "required_requirement_ids",
    "reviews",
    "evidence",
    "findings",
    "open_required_findings",
    "mandatory_unknowns",
    "unknowns",
    "contradictions",
    "blockers",
    "current_context_epoch",
    "worker_actor_id",
    "worker_context_epoch",
    # These fields are intentionally accepted and ignored.  A caller may
    # carry a larger status projection, but none of them can authorize a
    # completion result.
    "human_approval",
    "d5_selection",
    "d5_selected",
    "objective_achievement",
    "objective_achieved",
}
_BLOCKER_OUTCOMES = set(TERMINAL_OUTCOMES) - {"completed"}
_BLOCKER_FIELDS = {"outcome", "kind", "type", "status", "reason", "id", "refs"}
_FINDING_FIELDS = {
    "id",
    "finding_id",
    "path",
    "required",
    "open",
    "closed",
    "status",
    "state",
    "classification",
    "disposition",
}
_CLOSED_FINDING_STATES = {
    "closed",
    "resolved",
    "accepted",
    "rejected",
    "superseded",
    "fixed",
    "non-blocking",
    "nonblocking",
}
_NON_REQUIRED_DISPOSITIONS = {
    "defer",
    "deferred",
    "reject",
    "duplicate",
    "invalid",
    "deliberate-design",
    "downstream-only",
    "too-minor",
    "test-evidence-debt",
}
_INPUT_REQUIRED_DISPOSITIONS = {"needs-user", "needs-input", "unknown"}


def _fail(message: str, *, receipt: bool = False) -> None:
    error = MachineDecisionReceiptError if receipt else CompletionClassificationError
    raise error(message)


def _string_list(value: Any, label: str, *, allow_empty: bool = True) -> list[str]:
    if (
        not isinstance(value, list)
        or any(not isinstance(item, str) or not item for item in value)
        or len(value) != len(set(value))
        or (not allow_empty and not value)
    ):
        _fail(label + " must be a list of unique non-empty strings")
    return copy.deepcopy(value)


def _digest(value: Any, label: str) -> str:
    try:
        return require_digest(value, label)
    except LoopContractError as error:
        _fail(str(error))
    raise AssertionError("unreachable")


def _identifier(value: Any, label: str) -> str:
    try:
        return require_identifier(value, label)
    except LoopContractError as error:
        _fail(str(error))
    raise AssertionError("unreachable")


def _validated_identity(value: Any) -> dict[str, Any]:
    try:
        return validate_work_identity(value)
    except LoopContractError as error:
        _fail(str(error))
    raise AssertionError("unreachable")


def _validated_requirement(value: Any, index: int) -> dict[str, Any]:
    try:
        return validate_requirement_assessment(value)
    except LoopContractError as error:
        _fail(f"requirement[{index}]: {error}")
    raise AssertionError("unreachable")


def _validated_review(value: Any, index: int) -> dict[str, Any]:
    try:
        return validate_review_assessment(value)
    except LoopContractError as error:
        _fail(f"review[{index}]: {error}")
    raise AssertionError("unreachable")


def _validated_evidence(value: Any, index: int) -> dict[str, Any]:
    try:
        return validate_evidence_record(value)
    except LoopContractError as error:
        _fail(f"evidence[{index}]: {error}")
    raise AssertionError("unreachable")


def _normalise_request(request: Any, args: tuple[Any, ...], kwargs: Mapping[str, Any]) -> dict[str, Any]:
    """Accept the mapping seam and a small positional compatibility seam.

    The canonical seam is ``classify_completion(request)`` where ``request``
    is a mapping.  The positional form exists for the straightforward
    ``identity, candidate, package, requirements, reviews, evidence`` call and
    is normalized to exactly the same input before validation.
    """
    if isinstance(request, Mapping) and not args:
        merged = copy.deepcopy(dict(request))
        if kwargs:
            for key, value in kwargs.items():
                if key in merged:
                    _fail("completion request repeats field: " + key)
                merged[key] = copy.deepcopy(value)
    elif isinstance(request, Mapping) and len(args) == 5 and not kwargs:
        merged = {
            "identity": copy.deepcopy(dict(request)),
            "candidate_digest": copy.deepcopy(args[0]),
            "package_digest": copy.deepcopy(args[1]),
            "requirements": copy.deepcopy(args[2]),
            "reviews": copy.deepcopy(args[3]),
            "evidence": copy.deepcopy(args[4]),
        }
    elif isinstance(request, Mapping) and len(args) == 3 and {
        "candidate_digest", "package_digest",
    }.issubset(kwargs):
        merged = {
            "identity": copy.deepcopy(dict(request)),
            "requirements": copy.deepcopy(args[0]),
            "reviews": copy.deepcopy(args[1]),
            "evidence": copy.deepcopy(args[2]),
            **copy.deepcopy(dict(kwargs)),
        }
    elif request is None and not args:
        merged = copy.deepcopy(dict(kwargs))
    else:
        _fail("completion request must be one mapping or identity plus five positional values")

    for alias, canonical in _REQUEST_ALIASES.items():
        if alias in merged:
            if canonical in merged:
                _fail("completion request repeats field: " + canonical)
            merged[canonical] = merged.pop(alias)
    unknown = set(merged) - _REQUEST_KEYS
    if unknown:
        _fail("completion request has unsupported fields: " + ", ".join(sorted(unknown)))
    return merged


def _normalise_blockers(value: Any) -> list[dict[str, Any]]:
    """Validate explicit terminal blockers without inventing one."""
    if value is None:
        return []
    if isinstance(value, Mapping):
        # A keyed projection is convenient for status/resume consumers.  It is
        # still explicit: only non-empty entries become blockers.
        if set(value) and set(value).issubset(_BLOCKER_OUTCOMES):
            values: list[dict[str, Any]] = []
            for outcome in TERMINAL_OUTCOMES:
                if outcome not in value:
                    continue
                reasons = value[outcome]
                if isinstance(reasons, str):
                    reasons = [reasons]
                if not isinstance(reasons, list) or any(not isinstance(reason, str) or not reason for reason in reasons):
                    _fail("blocker reasons must be non-empty strings")
                for reason in reasons:
                    values.append({"outcome": outcome, "reason": reason})
            return values
        raw = [value]
    elif isinstance(value, list):
        raw = value
    else:
        _fail("blockers must be a list or keyed mapping")

    result: list[dict[str, Any]] = []
    for index, item in enumerate(raw):
        if not isinstance(item, Mapping):
            _fail(f"blocker[{index}] must be a mapping")
        if set(item) - _BLOCKER_FIELDS:
            _fail(f"blocker[{index}] has unsupported fields")
        outcome = None
        for key in ("outcome", "kind", "type", "status"):
            if key in item:
                if outcome is not None:
                    _fail(f"blocker[{index}] repeats outcome")
                outcome = item[key]
        if isinstance(outcome, str):
            outcome = outcome.strip().lower().replace("_", "-")
        if outcome not in _BLOCKER_OUTCOMES:
            _fail(f"blocker[{index}] has unsupported outcome")
        reason = item.get("reason")
        if not isinstance(reason, str) or not reason.strip():
            _fail(f"blocker[{index}] requires a reason")
        normalized = {"outcome": outcome, "reason": reason}
        if "id" in item:
            normalized["id"] = _identifier(item["id"], f"blocker[{index}].id")
        if "refs" in item:
            refs = item["refs"]
            if not isinstance(refs, list):
                _fail(f"blocker[{index}].refs must be a list")
            normalized["refs"] = copy.deepcopy(refs)
        result.append(normalized)

    outcomes = {item["outcome"] for item in result}
    if len(outcomes) > 1:
        _fail("explicit blockers disagree about terminal outcome")
    return result


def _normalise_markers(value: Any, label: str) -> list[str]:
    if value is None:
        return []
    if not isinstance(value, list):
        _fail(label + " must be a list")
    result: list[str] = []
    for index, item in enumerate(value):
        if isinstance(item, str) and item:
            result.append(item)
        elif isinstance(item, Mapping):
            identifier = item.get("id", item.get("finding_id", item.get("reason")))
            if not isinstance(identifier, str) or not identifier:
                _fail(f"{label}[{index}] requires id or reason")
            result.append(identifier)
        else:
            _fail(f"{label}[{index}] must be a string or mapping")
    if len(result) != len(set(result)):
        _fail(label + " must have unique markers")
    return result


def _ref_identity(value: Mapping[str, Any]) -> set[str]:
    result = set()
    if isinstance(value.get("id"), str):
        result.add("id:" + value["id"])
    if isinstance(value.get("path"), str):
        result.add("path:" + value["path"])
    return result


def _finding_status(value: Any, index: int) -> tuple[set[str], bool, bool]:
    if not isinstance(value, Mapping):
        _fail(f"finding[{index}] must be a mapping")
    if set(value) - _FINDING_FIELDS:
        _fail(f"finding[{index}] has unsupported fields")
    identities = set()
    for key in ("id", "finding_id"):
        if key in value:
            identities.add("id:" + _identifier(value[key], f"finding[{index}].{key}"))
    if "path" in value:
        if not isinstance(value["path"], str) or not value["path"]:
            _fail(f"finding[{index}].path must be non-empty")
        identities.add("path:" + value["path"])
    if not identities:
        _fail(f"finding[{index}] requires id or path")

    required = value.get("required")
    if required is not None and not isinstance(required, bool):
        _fail(f"finding[{index}].required must be boolean")
    disposition = value.get("disposition", value.get("classification"))
    if disposition is not None and (not isinstance(disposition, str) or not disposition):
        _fail(f"finding[{index}] disposition must be a non-empty string")
    state = value.get("state", value.get("status"))
    if state is not None and (not isinstance(state, str) or not state):
        _fail(f"finding[{index}] state must be a non-empty string")
    opened = value.get("open")
    closed = value.get("closed")
    if opened is not None and not isinstance(opened, bool):
        _fail(f"finding[{index}].open must be boolean")
    if closed is not None and not isinstance(closed, bool):
        _fail(f"finding[{index}].closed must be boolean")
    if opened is True and closed is True:
        _fail(f"finding[{index}] cannot be open and closed")

    lower_disposition = disposition.lower() if isinstance(disposition, str) else None
    lower_state = state.lower() if isinstance(state, str) else None
    is_required = required is True or lower_disposition == "required" or lower_disposition == "open-required"
    if required is False or lower_disposition in _NON_REQUIRED_DISPOSITIONS:
        is_required = False
    if lower_disposition in _INPUT_REQUIRED_DISPOSITIONS or lower_state in _INPUT_REQUIRED_DISPOSITIONS:
        is_required = True
    is_closed = closed is True or lower_state in _CLOSED_FINDING_STATES or lower_disposition in _NON_REQUIRED_DISPOSITIONS
    if opened is True:
        is_closed = False
    if opened is None and closed is None and state is None and disposition is None:
        # An unclassified finding is conservatively treated as an open required
        # finding.  The classifier must not silently turn a review ref into a
        # completion claim.
        is_required, is_closed = True, False
    return identities, is_required, is_closed


def _validate_findings(findings: Any, review_refs: Sequence[Mapping[str, Any]]) -> list[str]:
    if findings is None:
        findings = []
    if not isinstance(findings, list):
        _fail("findings must be a list")
    statuses: dict[str, tuple[bool, bool]] = {}
    for index, finding in enumerate(findings):
        identities, required, closed = _finding_status(finding, index)
        for identity in identities:
            if identity in statuses:
                _fail("findings must have unique identities")
            statuses[identity] = (required, closed)

    open_required: set[str] = set()
    for ref_index, ref in enumerate(review_refs):
        identities = _ref_identity(ref)
        matching = [statuses[key] for key in identities if key in statuses]
        if not matching:
            label = next(iter(identities), f"finding:{ref_index}")
            open_required.add(label)
            continue
        # A ref is safe only when its matching declaration is explicitly
        # non-required or closed.  Ambiguous declarations fail closed.
        if len(set(matching)) != 1:
            _fail("finding ref has conflicting declarations")
        required, closed = matching[0]
        if required and not closed:
            open_required.add(next(iter(identities)))

    # Required findings that are not attached to either review are still open;
    # keeping them out of finding_refs cannot hide a blocker.
    for identity, (required, closed) in statuses.items():
        if required and not closed:
            open_required.add(identity)
    return sorted(open_required)


def _evidence_ref_matches(ref: Mapping[str, Any], evidence: Mapping[str, Any]) -> bool:
    if ref.get("digest") != evidence.get("evidence_digest"):
        return False
    if ref.get("id") is not None and ref.get("id") != evidence.get("evidence_id"):
        return False
    # Evidence records deliberately do not carry a physical path.  A path-only
    # ref is therefore not enough to prove that this record is the cited item.
    return ref.get("id") is not None


def _classify_request(values: Mapping[str, Any]) -> dict[str, Any]:
    identity = _validated_identity(values.get("identity"))
    candidate_digest = _digest(values.get("candidate_digest"), "candidate_digest")
    package_digest = _digest(values.get("package_digest"), "package_digest")

    requirements_value = values.get("requirements")
    reviews_value = values.get("reviews")
    evidence_value = values.get("evidence")
    if not isinstance(requirements_value, list):
        _fail("requirements must be a list")
    if not isinstance(reviews_value, list):
        _fail("reviews must be a list")
    if not isinstance(evidence_value, list):
        _fail("evidence must be a list")
    requirements = [_validated_requirement(item, index) for index, item in enumerate(requirements_value)]
    reviews = [_validated_review(item, index) for index, item in enumerate(reviews_value)]
    evidence = [_validated_evidence(item, index) for index, item in enumerate(evidence_value)]
    if len({item["requirement_id"] for item in requirements}) != len(requirements):
        _fail("requirements must have unique requirement_id")
    if len({item["review_id"] for item in reviews}) != len(reviews):
        _fail("reviews must have unique review_id")
    if len({item["evidence_id"] for item in evidence}) != len(evidence):
        _fail("evidence must have unique evidence_id")

    required_ids_value = values.get("required_requirement_ids")
    if required_ids_value is None:
        required_ids = [item["requirement_id"] for item in requirements]
    else:
        required_ids = _string_list(required_ids_value, "required_requirement_ids", allow_empty=False)
    requirement_by_id = {item["requirement_id"]: item for item in requirements}
    if set(required_ids) != set(requirement_by_id):
        _fail("required_requirement_ids must cover every supplied requirement assessment")
    missing_requirements = sorted(set(required_ids) - set(requirement_by_id))
    if missing_requirements:
        _fail("required requirement assessment is missing: " + ", ".join(missing_requirements))

    evidence_by_id = {item["evidence_id"]: item for item in evidence}
    evidence_current = True
    evidence_passed = True
    stale_evidence: list[str] = []
    failed_evidence: list[str] = []
    unknown_evidence: list[str] = []
    for item in evidence:
        if item["candidate_digest"] != candidate_digest:
            evidence_current = False
            stale_evidence.append(item["evidence_id"])
        if item["status"] != "pass":
            evidence_passed = False
            (unknown_evidence if item["status"] == "unknown" else failed_evidence).append(item["evidence_id"])

    missing_evidence: list[str] = []
    bad_requirement_evidence: list[str] = []
    requirements_passed = True
    requirement_unknowns: list[str] = []
    for requirement_id in required_ids:
        item = requirement_by_id[requirement_id]
        if item["status"] != "pass":
            requirements_passed = False
            if item["status"] == "unknown":
                requirement_unknowns.append(requirement_id)
        refs = item["evidence_refs"]
        if item["status"] == "pass":
            for ref in refs:
                matches = [
                    record
                    for record in evidence_by_id.values()
                    if _evidence_ref_matches(ref, record)
                ]
                if (
                    len(matches) != 1
                    or matches[0]["candidate_digest"] != candidate_digest
                    or matches[0]["status"] != "pass"
                    or requirement_id not in matches[0]["coverage"]
                ):
                    bad_requirement_evidence.append(requirement_id)
                    if len(matches) != 1:
                        missing_evidence.append(requirement_id)
        elif not refs:
            # The shared validator already rejects this shape for pass, but a
            # fail/unknown assessment may have no evidence and remains open.
            missing_evidence.append(requirement_id)
    requirements_have_evidence = not missing_evidence and not bad_requirement_evidence

    axis_counts = {axis: 0 for axis in REQUIRED_REVIEW_AXES}
    review_candidate_match = True
    review_package_match = True
    reviews_complete = True
    review_unevaluated: list[str] = []
    review_coverage = True
    actors: list[str] = []
    contexts: list[str] = []
    invalid_axes: list[str] = []
    current_context_epoch = values.get("current_context_epoch")
    if current_context_epoch is not None:
        _identifier(current_context_epoch, "current_context_epoch")
    worker_actor = values.get("worker_actor_id")
    if worker_actor is not None:
        _identifier(worker_actor, "worker_actor_id")
    worker_context = values.get("worker_context_epoch")
    if worker_context is not None:
        _identifier(worker_context, "worker_context_epoch")
    for review in reviews:
        axis = review["axis"]
        axis_counts[axis] += 1
        if review["candidate_digest"] != candidate_digest:
            review_candidate_match = False
        if review["package_digest"] != package_digest:
            review_package_match = False
        if not review["completed"]:
            reviews_complete = False
        if review["unevaluated"]:
            reviews_complete = False
            review_unevaluated.extend(review["unevaluated"])
        if not set(required_ids).issubset(review["coverage"]):
            review_coverage = False
        actors.append(review["actor_id"])
        contexts.append(review["context_epoch"])
        if current_context_epoch is not None and review["context_epoch"] == current_context_epoch:
            invalid_axes.append(axis + ":current-context")
        if worker_actor is not None and review["actor_id"] == worker_actor:
            invalid_axes.append(axis + ":worker-actor")
        if worker_context is not None and review["context_epoch"] == worker_context:
            invalid_axes.append(axis + ":worker-context")
    missing_axes = sorted(axis for axis, count in axis_counts.items() if count == 0)
    duplicate_axes = sorted(axis for axis, count in axis_counts.items() if count > 1)
    reviews_fresh = (
        len(reviews) == len(REQUIRED_REVIEW_AXES)
        and len(set(actors)) == len(actors)
        and len(set(contexts)) == len(contexts)
        and not invalid_axes
    )

    review_refs = [ref for review in reviews for ref in review["finding_refs"]]
    open_required_findings = _validate_findings(values.get("findings"), review_refs)
    explicit_open = _normalise_markers(values.get("open_required_findings"), "open_required_findings")
    open_required_findings = sorted(set(open_required_findings) | {"marker:" + item for item in explicit_open})

    mandatory_unknowns = _normalise_markers(values.get("mandatory_unknowns"), "mandatory_unknowns")
    mandatory_unknowns.extend(_normalise_markers(values.get("unknowns"), "unknowns"))
    mandatory_unknowns.extend(requirement_unknowns)
    mandatory_unknowns.extend(unknown_evidence)
    mandatory_unknowns = sorted(set(mandatory_unknowns))
    contradictions = _normalise_markers(values.get("contradictions"), "contradictions")

    explicit_blockers = _normalise_blockers(values.get("blockers"))
    reason_codes: list[str] = []
    if not requirements:
        reason_codes.append("no-required-assessments")
    if not requirements_passed:
        reason_codes.extend("required-assessment-not-pass:" + item for item in required_ids if requirement_by_id[item]["status"] != "pass")
    if not requirements_have_evidence:
        reason_codes.extend("required-assessment-evidence-invalid:" + item for item in sorted(set(missing_evidence + bad_requirement_evidence)))
    if not evidence:
        reason_codes.append("no-evidence-records")
    if stale_evidence:
        reason_codes.extend("evidence-not-current:" + item for item in sorted(stale_evidence))
    if not evidence_passed:
        reason_codes.extend("evidence-not-pass:" + item for item in sorted(failed_evidence))
        reason_codes.extend("evidence-unknown:" + item for item in sorted(unknown_evidence))
    if not evidence_current:
        reason_codes.append("evidence-candidate-mismatch")
    if missing_axes:
        reason_codes.extend("missing-review-axis:" + item for item in missing_axes)
    if duplicate_axes:
        reason_codes.extend("duplicate-review-axis:" + item for item in duplicate_axes)
    if not review_candidate_match:
        reason_codes.append("review-candidate-mismatch")
    if not review_package_match:
        reason_codes.append("review-package-mismatch")
    if not reviews_complete:
        reason_codes.append("review-incomplete")
    if review_unevaluated:
        reason_codes.extend("review-unevaluated:" + item for item in sorted(set(review_unevaluated)))
    if not review_coverage:
        reason_codes.append("review-no-coverage")
    if not reviews_fresh:
        reason_codes.append("reviews-not-fresh")
    if open_required_findings:
        reason_codes.extend("open-required-finding:" + item for item in open_required_findings)
    if mandatory_unknowns:
        reason_codes.extend("mandatory-unknown:" + item for item in mandatory_unknowns)
    if contradictions:
        reason_codes.extend("contradiction:" + item for item in contradictions)
    reason_codes = sorted(set(reason_codes))

    all_required_checks = (
        bool(requirements)
        and requirements_passed
        and requirements_have_evidence
        and bool(evidence)
        and evidence_current
        and evidence_passed
        and not missing_axes
        and not duplicate_axes
        and review_candidate_match
        and review_package_match
        and reviews_complete
        and review_coverage
        and reviews_fresh
        and not open_required_findings
        and not mandatory_unknowns
        and not contradictions
    )
    if explicit_blockers:
        outcome = explicit_blockers[0]["outcome"]
    elif all_required_checks:
        outcome = "completed"
    else:
        # Incompleteness has no terminal semantic of its own.  It remains an
        # input gate until the caller supplies an explicit terminal blocker.
        outcome = "needs-input"

    return {
        "schema": CLASSIFICATION_SCHEMA,
        "outcome": outcome,
        "completed": outcome == "completed",
        "identity": copy.deepcopy(identity),
        "candidate_digest": candidate_digest,
        "package_digest": package_digest,
        "required_requirement_ids": copy.deepcopy(required_ids),
        "checks": {
            "requirements_passed": requirements_passed,
            "requirements_have_evidence": requirements_have_evidence,
            "evidence_current": evidence_current,
            "evidence_passed": evidence_passed,
            "reviews_complete": reviews_complete,
            "reviews_covered": review_coverage,
            "reviews_fresh": reviews_fresh,
            "reviews_same_candidate": review_candidate_match,
            "reviews_same_package": review_package_match,
            "no_unevaluated_scope": not bool(review_unevaluated),
            "no_open_required_findings": not bool(open_required_findings),
            "no_mandatory_unknown": not bool(mandatory_unknowns),
            "no_contradiction": not bool(contradictions),
        },
        "reason_codes": reason_codes,
        "blockers": copy.deepcopy(explicit_blockers),
        "human_approval": _NOT_EVALUATED,
        "d5_selection": _NOT_EVALUATED,
        "objective_achievement": _NOT_EVALUATED,
        "non_authorizing": True,
        "machine_decider": copy.deepcopy(MACHINE_DECIDER),
        "source_request": copy.deepcopy(dict(values)),
    }


def _validate_classification(value: Any) -> dict[str, Any]:
    if not isinstance(value, Mapping):
        _fail("classification must be a mapping", receipt=True)
    document = copy.deepcopy(dict(value))
    required = {
        "schema", "outcome", "completed", "identity", "candidate_digest", "package_digest",
        "required_requirement_ids", "checks", "reason_codes", "blockers", "human_approval",
        "d5_selection", "objective_achievement", "non_authorizing", "machine_decider",
        "source_request",
    }
    if set(document) != required:
        _fail("classification has unsupported or missing fields", receipt=True)
    if document["schema"] != CLASSIFICATION_SCHEMA or document["outcome"] not in TERMINAL_OUTCOMES:
        _fail("classification schema or outcome is unsupported", receipt=True)
    if document["completed"] is not (document["outcome"] == "completed"):
        _fail("classification completed flag is inconsistent", receipt=True)
    # Revalidate identity and digest-bound, immutable machine markers.
    try:
        validate_work_identity(document["identity"])
    except LoopContractError as error:
        _fail(str(error), receipt=True)
    _digest(document["candidate_digest"], "classification.candidate_digest")
    _digest(document["package_digest"], "classification.package_digest")
    _string_list(document["required_requirement_ids"], "classification.required_requirement_ids")
    _string_list(document["reason_codes"], "classification.reason_codes")
    if not isinstance(document["checks"], Mapping) or set(document["checks"]) != {
        "requirements_passed", "requirements_have_evidence", "evidence_current", "evidence_passed",
        "reviews_complete", "reviews_covered", "reviews_fresh", "reviews_same_candidate", "reviews_same_package",
        "no_unevaluated_scope", "no_open_required_findings", "no_mandatory_unknown", "no_contradiction",
    } or any(not isinstance(item, bool) for item in document["checks"].values()):
        _fail("classification checks are malformed", receipt=True)
    if not isinstance(document["blockers"], list):
        _fail("classification blockers are malformed", receipt=True)
    # The same blocker parser catches forged outcome names and verifies that a
    # completed result cannot carry an explicit terminal blocker.
    if _normalise_blockers(document["blockers"]) and document["outcome"] == "completed":
        _fail("completed classification cannot carry blockers", receipt=True)
    if document["outcome"] == "completed" and (
        not all(document["checks"].values()) or document["reason_codes"]
    ):
        _fail("completed classification does not satisfy its checks", receipt=True)
    if any(document[key] is not False for key in ("human_approval", "d5_selection", "objective_achievement")):
        _fail("classification may not assert human, D5, or H outcomes", receipt=True)
    if document["non_authorizing"] is not True or document["machine_decider"] != MACHINE_DECIDER:
        _fail("classification machine authority markers are invalid", receipt=True)
    try:
        recomputed = _classify_request(document["source_request"])
    except CompletionError as error:
        _fail("classification source request is invalid: " + str(error), receipt=True)
    if recomputed != document:
        _fail("classification does not match its source assessments", receipt=True)
    return document


def classify_completion(request: Any = None, *args: Any, **kwargs: Any) -> dict[str, Any]:
    """Classify supplied loop evidence without changing any external state."""
    values = _normalise_request(request, args, kwargs)
    return _classify_request(values)


def create_machine_decision_receipt(classification: Mapping[str, Any], receipt_id: str | None = None) -> dict[str, Any]:
    """Create a digest-bound, non-authorizing machine decision receipt."""
    document = _validate_classification(classification)
    classification_digest = canonical_digest(document)
    if receipt_id is None:
        receipt_id = "machine-decision-" + classification_digest.removeprefix("sha256:")[:24]
    receipt_id = _identifier(receipt_id, "receipt_id")
    unsigned = {
        "schema": MACHINE_DECISION_RECEIPT_SCHEMA,
        "receipt_id": receipt_id,
        "classification_digest": classification_digest,
        "classification": document,
        "candidate_digest": document["candidate_digest"],
        "package_digest": document["package_digest"],
        "outcome": document["outcome"],
        "producer": copy.deepcopy(MACHINE_DECIDER),
        "scope": _NON_AUTHORISING_SCOPE,
        "human_approval": _NOT_EVALUATED,
        "d5_selection": _NOT_EVALUATED,
        "objective_achievement": _NOT_EVALUATED,
        "non_authorizing": True,
    }
    return {**unsigned, "receipt_digest": canonical_digest(unsigned)}


def validate_machine_decision_receipt(
    value: Any,
    *,
    expected_candidate_digest: str | None = None,
    expected_package_digest: str | None = None,
) -> dict[str, Any]:
    """Validate a machine receipt and its embedded classification."""
    if not isinstance(value, Mapping):
        _fail("machine decision receipt must be a mapping", receipt=True)
    document = copy.deepcopy(dict(value))
    required = {
        "schema", "receipt_id", "classification_digest", "classification", "candidate_digest",
        "package_digest", "outcome", "producer", "scope", "human_approval", "d5_selection",
        "objective_achievement", "non_authorizing", "receipt_digest",
    }
    if set(document) != required:
        _fail("machine decision receipt has unsupported or missing fields", receipt=True)
    if document["schema"] != MACHINE_DECISION_RECEIPT_SCHEMA:
        _fail("machine decision receipt schema is unsupported", receipt=True)
    _identifier(document["receipt_id"], "receipt_id")
    classification = _validate_classification(document["classification"])
    expected_classification_digest = canonical_digest(classification)
    if document["classification_digest"] != expected_classification_digest:
        _fail("classification_digest does not bind classification", receipt=True)
    _digest(document["classification_digest"], "classification_digest")
    if document["candidate_digest"] != classification["candidate_digest"] or document["package_digest"] != classification["package_digest"]:
        _fail("machine receipt candidate/package binding is inconsistent", receipt=True)
    _digest(document["candidate_digest"], "candidate_digest")
    _digest(document["package_digest"], "package_digest")
    if document["outcome"] != classification["outcome"] or document["producer"] != MACHINE_DECIDER:
        _fail("machine receipt outcome or producer is inconsistent", receipt=True)
    if document["scope"] != _NON_AUTHORISING_SCOPE or document["non_authorizing"] is not True:
        _fail("machine receipt scope is authorizing", receipt=True)
    if any(document[key] is not False for key in ("human_approval", "d5_selection", "objective_achievement")):
        _fail("machine receipt may not assert human, D5, or H outcomes", receipt=True)
    if expected_candidate_digest is not None and document["candidate_digest"] != _digest(expected_candidate_digest, "expected_candidate_digest"):
        _fail("machine receipt candidate digest does not match expectation", receipt=True)
    if expected_package_digest is not None and document["package_digest"] != _digest(expected_package_digest, "expected_package_digest"):
        _fail("machine receipt package digest does not match expectation", receipt=True)
    unsigned = {key: copy.deepcopy(item) for key, item in document.items() if key != "receipt_digest"}
    if document["receipt_digest"] != canonical_digest(unsigned):
        _fail("receipt_digest does not bind the machine decision", receipt=True)
    return document


class CompletionClassifier:
    """Stateless object seam for callers that prefer an explicit compiler."""

    def classify(self, request: Any = None, *args: Any, **kwargs: Any) -> dict[str, Any]:
        return classify_completion(request, *args, **kwargs)

    def create_receipt(self, classification: Mapping[str, Any], receipt_id: str | None = None) -> dict[str, Any]:
        return create_machine_decision_receipt(classification, receipt_id)

    def validate_receipt(self, value: Any, **kwargs: Any) -> dict[str, Any]:
        return validate_machine_decision_receipt(value, **kwargs)


class CompletionV1(CompletionClassifier):
    """Versioned compiler seam used by workflow selectors."""

    def compile(self, request: Any = None, *args: Any, **kwargs: Any) -> dict[str, Any]:
        return self.classify(request, *args, **kwargs)

    def machine_decision_receipt(
        self, classification: Mapping[str, Any], receipt_id: str | None = None
    ) -> dict[str, Any]:
        return self.create_receipt(classification, receipt_id)


# Public aliases keep the seam readable for status/resume consumers without
# introducing a second implementation owner.
classify = classify_completion
build_machine_decision_receipt = create_machine_decision_receipt
make_machine_decision_receipt = create_machine_decision_receipt
validate_receipt = validate_machine_decision_receipt
verify_machine_decision_receipt = validate_machine_decision_receipt


__all__ = [
    "CLASSIFICATION_SCHEMA",
    "MACHINE_DECIDER",
    "MACHINE_DECISION_RECEIPT_SCHEMA",
    "CompletionClassificationError",
    "CompletionClassifier",
    "CompletionError",
    "CompletionV1",
    "MachineDecisionReceiptError",
    "build_machine_decision_receipt",
    "classify",
    "classify_completion",
    "create_machine_decision_receipt",
    "make_machine_decision_receipt",
    "validate_machine_decision_receipt",
    "validate_receipt",
    "verify_machine_decision_receipt",
]
