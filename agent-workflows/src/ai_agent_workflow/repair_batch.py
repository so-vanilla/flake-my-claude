"""Deterministic planning and resolution accounting for required Finding batches."""
from __future__ import annotations

import copy
from collections.abc import Mapping, Sequence
from typing import Any

from .loop_contracts import (
    LoopContractError,
    canonical_digest,
    require_digest,
    require_identifier,
    validate_ref,
)


class RepairBatchError(LoopContractError):
    """Required findings cannot be safely grouped or closed."""


_FINDING_FIELDS = {
    "finding_id", "fingerprint", "classification", "candidate_digest", "batch_key",
    "root_cause", "write_scope", "verification", "depends_on", "conflicts_with",
    "resolution_conditions",
}


def _strings(value: Any, label: str, *, non_empty: bool = False) -> list[str]:
    if (
        not isinstance(value, list)
        or (non_empty and not value)
        or not all(isinstance(item, str) and item for item in value)
        or len(value) != len(set(value))
    ):
        raise RepairBatchError(label + " must be a unique string list")
    return sorted(value)


def _finding(value: Any) -> dict[str, Any]:
    if not isinstance(value, Mapping) or set(value) != _FINDING_FIELDS:
        raise RepairBatchError("finding has an unsupported shape")
    item = copy.deepcopy(dict(value))
    require_identifier(item.get("finding_id"), "finding_id")
    require_identifier(item.get("fingerprint"), "fingerprint")
    if item.get("classification") not in {"required", "defer", "duplicate", "rejected"}:
        raise RepairBatchError("finding classification is unsupported")
    require_digest(item.get("candidate_digest"), "candidate_digest")
    if item.get("batch_key") is not None:
        require_identifier(item["batch_key"], "batch_key")
    if not isinstance(item.get("root_cause"), str) or not item["root_cause"]:
        raise RepairBatchError("root_cause must be non-empty")
    for field in ("write_scope", "verification", "depends_on", "conflicts_with", "resolution_conditions"):
        item[field] = _strings(item.get(field), field, non_empty=field in {"write_scope", "verification", "resolution_conditions"})
    if item["finding_id"] in item["depends_on"] or item["finding_id"] in item["conflicts_with"]:
        raise RepairBatchError("a finding cannot depend on or conflict with itself")
    return item


def plan_fix_batches(findings: Sequence[Mapping[str, Any]]) -> dict[str, Any]:
    """Group explicitly compatible required findings without losing identity.

    ``batch_key`` is supplied by the validated disposition.  The planner does
    not infer semantic compatibility from similar prose.  Findings without a
    key remain singleton batches.
    """
    if not isinstance(findings, Sequence) or isinstance(findings, (str, bytes)):
        raise RepairBatchError("findings must be a sequence")
    normalized = [_finding(item) for item in findings]
    ids = [item["finding_id"] for item in normalized]
    if len(ids) != len(set(ids)):
        raise RepairBatchError("finding_id must be unique")
    fingerprints = [item["fingerprint"] for item in normalized]
    if len(fingerprints) != len(set(fingerprints)):
        raise RepairBatchError("canonical finding fingerprints must be unique")
    known = set(ids)
    for item in normalized:
        if not set(item["depends_on"] + item["conflicts_with"]).issubset(known):
            raise RepairBatchError("finding relation names an unknown finding")
    required = [item for item in normalized if item["classification"] == "required"]
    required_ids = {item["finding_id"] for item in required}
    if any(
        dependency not in required_ids
        for item in required
        for dependency in item["depends_on"]
    ):
        raise RepairBatchError("required finding cannot depend on a non-required finding")
    candidates = {item["candidate_digest"] for item in required}
    if len(candidates) > 1:
        raise RepairBatchError("one batch plan cannot mix candidates")

    groups: dict[str, list[dict[str, Any]]] = {}
    for item in required:
        key = item["batch_key"] or "singleton:" + item["finding_id"]
        groups.setdefault(key, []).append(item)

    batches = []
    for key, members in sorted(groups.items()):
        member_ids = {item["finding_id"] for item in members}
        roots = {item["root_cause"] for item in members}
        if len(members) > 1 and len(roots) != 1:
            raise RepairBatchError("a multi-finding batch must share one root cause")
        if any(set(item["depends_on"] + item["conflicts_with"]).intersection(member_ids) for item in members):
            raise RepairBatchError("dependent or conflicting findings must be separate batches")
        batch = {
            "schema": "loop-repair-batch/v1",
            "batch_id": "batch-" + canonical_digest(sorted(member_ids))[7:19],
            "candidate_digest": next(iter(candidates)) if candidates else None,
            "finding_ids": sorted(member_ids),
            "finding_fingerprints": sorted(item["fingerprint"] for item in members),
            "root_cause": next(iter(roots)) if roots else None,
            "write_scope": sorted({path for item in members for path in item["write_scope"]}),
            "verification": sorted({check for item in members for check in item["verification"]}),
            "resolution_conditions": {
                item["finding_id"]: copy.deepcopy(item["resolution_conditions"])
                for item in sorted(members, key=lambda entry: entry["finding_id"])
            },
            "depends_on_batches": [],
        }
        batch["batch_digest"] = canonical_digest(batch)
        batches.append(batch)

    batch_for = {
        finding_id: batch["batch_id"]
        for batch in batches
        for finding_id in batch["finding_ids"]
    }
    for batch in batches:
        dependencies = {
            batch_for[dependency]
            for finding_id in batch["finding_ids"]
            for dependency in next(item for item in required if item["finding_id"] == finding_id)["depends_on"]
            if dependency in batch_for
        }
        batch["depends_on_batches"] = sorted(dependencies)
        batch["batch_digest"] = canonical_digest({key: value for key, value in batch.items() if key != "batch_digest"})
    batch_by_id = {batch["batch_id"]: batch for batch in batches}
    visiting: set[str] = set()
    visited: set[str] = set()

    def visit(batch_id: str) -> None:
        if batch_id in visiting:
            raise RepairBatchError("repair batch dependencies contain a cycle")
        if batch_id in visited:
            return
        visiting.add(batch_id)
        for dependency in batch_by_id[batch_id]["depends_on_batches"]:
            visit(dependency)
        visiting.remove(batch_id)
        visited.add(batch_id)

    for batch_id in batch_by_id:
        visit(batch_id)
    result = {
        "schema": "loop-repair-batch-plan/v1",
        "candidate_digest": next(iter(candidates)) if candidates else None,
        "batches": batches,
        "non_required_finding_ids": sorted(item["finding_id"] for item in normalized if item["classification"] != "required"),
    }
    result["plan_digest"] = canonical_digest(result)
    return result


def assess_batch_resolution(batch: Any, resolutions: Sequence[Mapping[str, Any]]) -> dict[str, Any]:
    """Account for every Finding independently after one shared repair round."""
    if not isinstance(batch, Mapping) or batch.get("schema") != "loop-repair-batch/v1":
        raise RepairBatchError("batch is not loop-repair-batch/v1")
    supplied = batch.get("batch_digest")
    unsigned = {key: copy.deepcopy(value) for key, value in batch.items() if key != "batch_digest"}
    if require_digest(supplied, "batch_digest") != canonical_digest(unsigned):
        raise RepairBatchError("batch digest does not bind the batch")
    try:
        batch_id = require_identifier(batch.get("batch_id"), "batch_id")
        candidate_digest = require_digest(batch.get("candidate_digest"), "batch.candidate_digest")
    except LoopContractError as error:
        raise RepairBatchError(str(error)) from error
    if not isinstance(resolutions, Sequence) or isinstance(resolutions, (str, bytes)):
        raise RepairBatchError("resolutions must be a sequence")
    raw_finding_ids = batch.get("finding_ids")
    if (
        not isinstance(raw_finding_ids, list)
        or not raw_finding_ids
        or any(not isinstance(item, str) or not item for item in raw_finding_ids)
        or len(raw_finding_ids) != len(set(raw_finding_ids))
    ):
        raise RepairBatchError("batch finding_ids are malformed")
    expected = set(raw_finding_ids)
    conditions = batch.get("resolution_conditions")
    if not isinstance(conditions, Mapping) or set(conditions) != expected:
        raise RepairBatchError("batch resolution_conditions are not bound to its findings")
    expected_conditions = {
        finding_id: _strings(
            conditions[finding_id], "resolution_conditions", non_empty=True
        )
        for finding_id in sorted(expected)
    }
    normalized = []
    for value in resolutions:
        if not isinstance(value, Mapping) or set(value) != {"finding_id", "status", "evidence_refs", "unresolved_conditions"}:
            raise RepairBatchError("resolution has an unsupported shape")
        item = copy.deepcopy(dict(value))
        require_identifier(item.get("finding_id"), "resolution.finding_id")
        if item.get("status") not in {"resolved", "open", "unknown"}:
            raise RepairBatchError("resolution status is unsupported")
        refs = item.get("evidence_refs")
        if not isinstance(refs, list):
            raise RepairBatchError("resolution evidence_refs are malformed")
        checked_refs = []
        try:
            for index, ref in enumerate(refs):
                checked_refs.append(validate_ref(ref, f"resolution evidence_refs[{index}]"))
        except LoopContractError as error:
            raise RepairBatchError(str(error)) from error
        item["unresolved_conditions"] = _strings(item.get("unresolved_conditions"), "unresolved_conditions")
        finding_id = item["finding_id"]
        if finding_id not in expected:
            raise RepairBatchError("resolution names an unknown batch Finding")
        required_conditions = expected_conditions[finding_id]
        if item["status"] == "resolved" and (not checked_refs or item["unresolved_conditions"]):
            raise RepairBatchError("resolved Finding needs evidence and no unresolved condition")
        item["evidence_refs"] = checked_refs
        item["evidence_binding"] = {
            "candidate_digest": candidate_digest,
            "batch_ref": {"id": batch_id, "digest": supplied},
            "finding_id": finding_id,
            "resolution_conditions": required_conditions,
        }
        normalized.append(item)
    actual = [item["finding_id"] for item in normalized]
    if len(actual) != len(set(actual)) or set(actual) != expected:
        raise RepairBatchError("resolutions must cover every batch Finding exactly once")
    result = {
        "schema": "loop-repair-resolution/v1",
        "batch_ref": {"id": batch_id, "digest": supplied, "candidate_digest": candidate_digest},
        "finding_results": sorted(normalized, key=lambda item: item["finding_id"]),
        "complete": all(item["status"] == "resolved" for item in normalized),
        "open_finding_ids": sorted(item["finding_id"] for item in normalized if item["status"] != "resolved"),
    }
    result["resolution_digest"] = canonical_digest(result)
    return result


__all__ = ["RepairBatchError", "assess_batch_resolution", "plan_fix_batches"]
