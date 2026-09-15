"""Build narrow physical packages for initial and change-scoped reviews."""
from __future__ import annotations

import copy
from collections.abc import Mapping, Sequence
from typing import Any

from .loop_contracts import (
    REQUIRED_REVIEW_AXES,
    LoopContractError,
    canonical_digest,
    require_digest,
    require_identifier,
    validate_ref,
    validate_review_assessment,
)


class ReviewPackageError(LoopContractError):
    """A review package or result is incomplete, stale, or over-broad."""


def _strings(value: Any, label: str, *, non_empty: bool = False) -> list[str]:
    if (
        not isinstance(value, list)
        or (non_empty and not value)
        or not all(isinstance(item, str) and item for item in value)
        or len(value) != len(set(value))
    ):
        raise ReviewPackageError(label + " must be a unique string list")
    return sorted(value)


def _candidate(value: Any) -> dict[str, Any]:
    fields = {"candidate_ref", "spec_ref", "dependency_refs", "environment_ref", "source_paths"}
    if not isinstance(value, Mapping) or set(value) != fields:
        raise ReviewPackageError("candidate has an unsupported shape")
    item = copy.deepcopy(dict(value))
    item["candidate_ref"] = validate_ref(item["candidate_ref"], "candidate_ref")
    item["spec_ref"] = validate_ref(item["spec_ref"], "spec_ref")
    item["environment_ref"] = validate_ref(item["environment_ref"], "environment_ref")
    if not isinstance(item["dependency_refs"], list):
        raise ReviewPackageError("dependency_refs must be a list")
    item["dependency_refs"] = [validate_ref(ref, "dependency_ref") for ref in item["dependency_refs"]]
    item["source_paths"] = _strings(item["source_paths"], "source_paths", non_empty=True)
    return item


def _requirements(value: Any) -> list[dict[str, Any]]:
    if not isinstance(value, Sequence) or isinstance(value, (str, bytes)) or not value:
        raise ReviewPackageError("requirements must be a non-empty sequence")
    result = []
    for raw in value:
        if not isinstance(raw, Mapping) or set(raw) != {"requirement_id", "requirement_ref", "scope"}:
            raise ReviewPackageError("requirement has an unsupported shape")
        item = copy.deepcopy(dict(raw))
        require_identifier(item.get("requirement_id"), "requirement_id")
        item["requirement_ref"] = validate_ref(item["requirement_ref"], "requirement_ref")
        item["scope"] = _strings(item["scope"], "requirement.scope", non_empty=True)
        result.append(item)
    ids = [item["requirement_id"] for item in result]
    if len(ids) != len(set(ids)):
        raise ReviewPackageError("requirement_id must be unique")
    return sorted(result, key=lambda item: item["requirement_id"])


def _findings(value: Any) -> list[dict[str, Any]]:
    if not isinstance(value, Sequence) or isinstance(value, (str, bytes)):
        raise ReviewPackageError("prior_findings must be a sequence")
    result = []
    for raw in value:
        if not isinstance(raw, Mapping) or set(raw) != {"finding_id", "finding_ref", "status", "scope", "resolution_ref"}:
            raise ReviewPackageError("prior finding has an unsupported shape")
        item = copy.deepcopy(dict(raw))
        require_identifier(item.get("finding_id"), "finding_id")
        item["finding_ref"] = validate_ref(item["finding_ref"], "finding_ref")
        if item.get("status") not in {"required", "resolved", "defer", "duplicate", "rejected"}:
            raise ReviewPackageError("prior finding status is unsupported")
        item["scope"] = _strings(item["scope"], "finding.scope", non_empty=True)
        item["resolution_ref"] = validate_ref(item["resolution_ref"], "resolution_ref", nullable=item["status"] != "resolved")
        if item["status"] == "resolved" and item["resolution_ref"] is None:
            raise ReviewPackageError("resolved prior finding must bind its resolution")
        result.append(item)
    ids = [item["finding_id"] for item in result]
    if len(ids) != len(set(ids)):
        raise ReviewPackageError("finding_id must be unique")
    return sorted(result, key=lambda item: item["finding_id"])


def _impact(value: Any, requirement_ids: set[str], source_paths: set[str]) -> dict[str, Any]:
    fields = {"known", "changed_paths", "affected_requirements", "affected_interfaces", "affected_tests"}
    if not isinstance(value, Mapping) or set(value) != fields or not isinstance(value.get("known"), bool):
        raise ReviewPackageError("impact has an unsupported shape")
    item = copy.deepcopy(dict(value))
    item["changed_paths"] = _strings(item["changed_paths"], "changed_paths")
    item["affected_requirements"] = _strings(item["affected_requirements"], "affected_requirements")
    item["affected_interfaces"] = _strings(item["affected_interfaces"], "affected_interfaces")
    item["affected_tests"] = _strings(item["affected_tests"], "affected_tests")
    if not set(item["affected_requirements"]).issubset(requirement_ids):
        raise ReviewPackageError("impact names an unknown requirement")
    if not set(item["changed_paths"]).issubset(source_paths):
        raise ReviewPackageError("changed_paths must be within candidate source_paths")
    if not item["known"] and any(item[field] for field in fields - {"known"}):
        raise ReviewPackageError("unknown impact cannot claim a narrow affected set")
    return item


def build_review_package(
    candidate: Any,
    requirements: Any,
    prior_findings: Any,
    impact: Any,
    *,
    requested_mode: str,
    axis: str,
    assignment: Any,
) -> dict[str, Any]:
    """Compile a full initial package or a safely scoped fresh re-review."""
    current = _candidate(candidate)
    requirement_set = _requirements(requirements)
    findings = _findings(prior_findings)
    requirement_ids = {item["requirement_id"] for item in requirement_set}
    impact_value = _impact(impact, requirement_ids, set(current["source_paths"]))
    if requested_mode not in {"initial", "delta"}:
        raise ReviewPackageError("requested_mode is unsupported")
    if axis not in REQUIRED_REVIEW_AXES:
        raise ReviewPackageError("axis is unsupported")
    if requested_mode == "delta":
        source_paths = set(current["source_paths"])
        if any(not set(item["scope"]).issubset(source_paths) for item in findings):
            raise ReviewPackageError("delta finding scope must stay within candidate source_paths")
    if not isinstance(assignment, Mapping) or set(assignment) != {"assignment_id", "actor_id", "context_epoch"}:
        raise ReviewPackageError("assignment has an unsupported shape")
    assignment_value = copy.deepcopy(dict(assignment))
    for field in assignment_value:
        require_identifier(assignment_value[field], "assignment." + field)
    if requested_mode == "initial" and findings:
        raise ReviewPackageError("an initial package cannot contain prior findings")
    if requested_mode == "delta" and not findings and not impact_value["changed_paths"]:
        raise ReviewPackageError("a delta package needs a prior finding or changed path")

    mode = requested_mode if requested_mode == "initial" or impact_value["known"] else "full-impact-unknown"
    if mode == "initial" or mode == "full-impact-unknown":
        coverage = sorted(requirement_ids)
        read_scope = copy.deepcopy(current["source_paths"])
    else:
        finding_scope = {path for item in findings if item["status"] in {"required", "resolved"} for path in item["scope"]}
        coverage = sorted(set(impact_value["affected_requirements"]) | {
            requirement["requirement_id"]
            for requirement in requirement_set
            if set(requirement["scope"]).intersection(finding_scope)
        })
        read_scope = sorted(set(impact_value["changed_paths"]) | finding_scope)
        if not coverage or not read_scope:
            raise ReviewPackageError("delta scope cannot be proven from findings and impact")

    package = {
        "schema": "loop-review-package/v1",
        "package_id": "review-package-" + axis + "-" + assignment_value["assignment_id"],
        "mode": mode,
        "axis": axis,
        "assignment": assignment_value,
        "candidate": current,
        "requirements": requirement_set,
        "prior_findings": findings,
        "impact": impact_value,
        "required_coverage": coverage,
        "read_scope": read_scope,
        "fresh_review_required": True,
    }
    package["package_digest"] = canonical_digest(package)
    return package


def accept_review_result(package: Any, result: Any) -> dict[str, Any]:
    """Bind one strict assessment to exactly the package that was issued."""
    if not isinstance(package, Mapping) or package.get("schema") != "loop-review-package/v1":
        raise ReviewPackageError("package is not loop-review-package/v1")
    supplied = package.get("package_digest")
    unsigned = {key: copy.deepcopy(value) for key, value in package.items() if key != "package_digest"}
    if require_digest(supplied, "package_digest") != canonical_digest(unsigned):
        raise ReviewPackageError("package digest does not bind its content")
    try:
        review = validate_review_assessment(result)
    except LoopContractError as error:
        raise ReviewPackageError(str(error)) from error
    assignment = package["assignment"]
    if (
        review["axis"] != package["axis"]
        or review["actor_id"] != assignment["actor_id"]
        or review["context_epoch"] != assignment["context_epoch"]
        or review["candidate_digest"] != package["candidate"]["candidate_ref"]["digest"]
        or review["package_digest"] != supplied
    ):
        raise ReviewPackageError("review result is stale or belongs to another assignment")
    required = set(package["required_coverage"])
    if set(review["coverage"]) != required or review["unevaluated"]:
        raise ReviewPackageError("review does not cover the complete issued scope")
    return review


__all__ = ["ReviewPackageError", "accept_review_result", "build_review_package"]
