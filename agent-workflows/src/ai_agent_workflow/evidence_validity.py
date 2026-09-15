"""Pure evidence invalidation and candidate-rebinding decisions."""
from __future__ import annotations

import copy
from collections.abc import Mapping
from typing import Any

from .loop_contracts import (
    LoopContractError,
    canonical_digest,
    require_digest,
    validate_evidence_record,
    validate_ref,
)

_INPUT_FIELDS = {
    "candidate_digest",
    "spec_digest",
    "source_digest",
    "dependency_digest",
    "environment_digest",
    "check_definition_digest",
    "required_coverage",
}
_DIMENSIONS = {
    "spec": "spec_digest",
    "source": "source_digest",
    "dependency": "dependency_digest",
    "environment": "environment_digest",
    "check-definition": "check_definition_digest",
}


class EvidenceValidityError(LoopContractError):
    """Evidence validity cannot be determined from the supplied contract."""


def _inputs(value: Any) -> dict[str, Any]:
    if not isinstance(value, Mapping) or set(value) != _INPUT_FIELDS:
        raise EvidenceValidityError("current_inputs has an unsupported shape")
    result = copy.deepcopy(dict(value))
    for field in _INPUT_FIELDS - {"required_coverage"}:
        require_digest(result[field], "current_inputs." + field)
    coverage = result["required_coverage"]
    if (
        not isinstance(coverage, list)
        or not coverage
        or not all(isinstance(item, str) and item for item in coverage)
        or len(coverage) != len(set(coverage))
    ):
        raise EvidenceValidityError("required_coverage must be a non-empty unique list")
    result["required_coverage"] = sorted(coverage)
    return result


def _impact(value: Any, required_coverage: list[str]) -> dict[str, Any]:
    if not isinstance(value, Mapping) or set(value) != {"known", "invalidated_dimensions", "impacted_coverage"}:
        raise EvidenceValidityError("change_impact has an unsupported shape")
    known = value.get("known")
    dimensions = value.get("invalidated_dimensions")
    coverage = value.get("impacted_coverage")
    if not isinstance(known, bool):
        raise EvidenceValidityError("change_impact.known must be boolean")
    if (
        not isinstance(dimensions, list)
        or len(dimensions) != len(set(dimensions))
        or not set(dimensions).issubset(_DIMENSIONS)
    ):
        raise EvidenceValidityError("invalidated_dimensions is malformed")
    if (
        not isinstance(coverage, list)
        or len(coverage) != len(set(coverage))
        or not all(isinstance(item, str) and item in required_coverage for item in coverage)
    ):
        raise EvidenceValidityError("impacted_coverage must be within required coverage")
    if not known and (dimensions or coverage):
        raise EvidenceValidityError("unknown impact cannot claim exact invalidation")
    return {"known": known, "invalidated_dimensions": sorted(dimensions), "impacted_coverage": sorted(coverage)}


def assess_evidence(receipt: Any, current_inputs: Any, change_impact: Any) -> dict[str, Any]:
    """Classify a prior receipt as valid, invalid, or unknown for a candidate.

    A changed candidate is not itself proof that all checks must be repeated.
    Reuse is allowed only when every validity input either still matches or a
    known source-only impact proves which covered checks were unaffected.
    """
    try:
        evidence = validate_evidence_record(receipt)
        current = _inputs(current_inputs)
        impact = _impact(change_impact, current["required_coverage"])
    except LoopContractError as error:
        raise EvidenceValidityError(str(error)) from error

    mismatches = sorted(
        dimension
        for dimension, field in _DIMENSIONS.items()
        if evidence[field] != current[field]
    )
    declared = set(impact["invalidated_dimensions"])
    if impact["known"] and declared != set(mismatches):
        raise EvidenceValidityError("change impact does not exactly explain digest mismatches")

    required = set(current["required_coverage"])
    previously_covered = required.intersection(evidence["coverage"])
    missing = required - previously_covered
    impacted = set(impact["impacted_coverage"])
    broad_change = bool(set(mismatches) - {"source"})

    reasons: list[str] = []
    if evidence["status"] != "pass":
        reasons.append("source-evidence-" + evidence["status"])
    if missing:
        reasons.append("missing-coverage")
    if mismatches:
        reasons.append("changed-" + ",".join(mismatches))
    if not impact["known"] and (
        mismatches or evidence["candidate_digest"] != current["candidate_digest"]
    ):
        reasons.append("unknown-change-impact")

    if broad_change:
        rerun = set(required)
    elif "source" in mismatches:
        rerun = set(impacted)
    else:
        rerun = set()
    rerun.update(missing)

    if evidence["status"] == "fail" or broad_change or ("source" in mismatches and rerun):
        status = "invalid"
    elif evidence["status"] == "unknown" or "unknown-change-impact" in reasons:
        status = "unknown"
        rerun = set(required)
    elif missing:
        status = "invalid"
    else:
        status = "valid"

    reusable = sorted(previously_covered - rerun) if status != "unknown" else []
    result = {
        "schema": "loop-evidence-assessment/v1",
        "evidence_id": evidence["evidence_id"],
        "source_evidence_digest": evidence["evidence_digest"],
        "source_candidate_digest": evidence["candidate_digest"],
        "current_candidate_digest": current["candidate_digest"],
        "status": status,
        "mismatched_dimensions": mismatches,
        "reusable_coverage": reusable,
        "rerun_coverage": sorted(rerun),
        "rebind_required": status == "valid" and evidence["candidate_digest"] != current["candidate_digest"],
        "reasons": reasons or ["all-validity-inputs-match"],
    }
    result["assessment_digest"] = canonical_digest(result)
    return result


def bind_reused_evidence(
    assessment: Any,
    source_ref: Any,
    current_candidate_digest: Any,
) -> dict[str, Any]:
    """Create a new immutable binding; never mutate or relabel the old receipt."""
    if not isinstance(assessment, Mapping) or assessment.get("schema") != "loop-evidence-assessment/v1":
        raise EvidenceValidityError("assessment is not loop-evidence-assessment/v1")
    supplied = assessment.get("assessment_digest")
    unsigned = {key: copy.deepcopy(value) for key, value in assessment.items() if key != "assessment_digest"}
    if require_digest(supplied, "assessment_digest") != canonical_digest(unsigned):
        raise EvidenceValidityError("assessment digest does not bind its content")
    if assessment.get("status") != "valid" or not assessment.get("reusable_coverage"):
        raise EvidenceValidityError("only valid covered evidence can be rebound")
    require_digest(current_candidate_digest, "current_candidate_digest")
    if assessment.get("current_candidate_digest") != current_candidate_digest:
        raise EvidenceValidityError("assessment belongs to another current candidate")
    source = validate_ref(source_ref, "source_ref")
    if source["digest"] != assessment.get("source_evidence_digest"):
        raise EvidenceValidityError("source_ref does not bind the assessed evidence")
    result = {
        "schema": "loop-evidence-reuse/v1",
        "source_ref": source,
        "assessment_ref": {"id": "assessment", "digest": supplied},
        "candidate_digest": current_candidate_digest,
        "coverage": copy.deepcopy(assessment["reusable_coverage"]),
    }
    result["reuse_digest"] = canonical_digest(result)
    return result

__all__ = ["EvidenceValidityError", "assess_evidence", "bind_reused_evidence"]
