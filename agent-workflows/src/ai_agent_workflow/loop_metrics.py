"""Deterministic loop telemetry aggregation and like-for-like comparison."""
from __future__ import annotations

import copy
from collections.abc import Mapping, Sequence
from typing import Any

from .loop_contracts import (
    LoopContractError,
    canonical_digest,
    require_identifier,
    validate_ref,
)


class LoopMetricsError(LoopContractError):
    """Metrics are malformed or not comparable."""


_FIELDS = {
    "schema", "event_id", "work_id", "scenario", "stage", "model", "reasoning_effort",
    "duration_ms", "llm_calls", "read_bytes", "test_invocations", "status", "source_ref",
}
_MEASURES = ("duration_ms", "llm_calls", "read_bytes", "test_invocations")


def _event(value: Any) -> dict[str, Any]:
    if not isinstance(value, Mapping) or set(value) != _FIELDS or value.get("schema") != "loop-metric-event/v1":
        raise LoopMetricsError("metric event has an unsupported shape")
    item = copy.deepcopy(dict(value))
    for field in ("event_id", "work_id", "scenario", "stage", "model", "reasoning_effort"):
        require_identifier(item.get(field), field)
    for field in _MEASURES:
        measure = item.get(field)
        if measure is not None and (not isinstance(measure, int) or isinstance(measure, bool) or measure < 0):
            raise LoopMetricsError(field + " must be a non-negative integer or null")
    if item.get("status") not in {"completed", "stopped", "failed", "unavailable"}:
        raise LoopMetricsError("metric event status is unsupported")
    item["source_ref"] = validate_ref(item.get("source_ref"), "source_ref")
    return item


def derive_metrics(events: Sequence[Mapping[str, Any]]) -> dict[str, Any]:
    if not isinstance(events, Sequence) or isinstance(events, (str, bytes)) or not events:
        raise LoopMetricsError("events must be a non-empty sequence")
    values = [_event(item) for item in events]
    ids = [item["event_id"] for item in values]
    if len(ids) != len(set(ids)):
        raise LoopMetricsError("metric event IDs must be unique")
    identity = {(item["work_id"], item["scenario"], item["model"], item["reasoning_effort"]) for item in values}
    if len(identity) != 1:
        raise LoopMetricsError("one aggregate cannot mix work, scenario, or model conditions")
    totals: dict[str, int | None] = {}
    unavailable = []
    for field in _MEASURES:
        measurements = [item[field] for item in values]
        if any(item is None for item in measurements):
            totals[field] = None
            unavailable.append(field)
        else:
            totals[field] = sum(measurements)  # type: ignore[arg-type]
    work_id, scenario, model, effort = next(iter(identity))
    statuses = {item["status"] for item in values}
    result = {
        "schema": "loop-metrics/v1",
        "work_id": work_id,
        "scenario": scenario,
        "model": model,
        "reasoning_effort": effort,
        "event_refs": [copy.deepcopy(item["source_ref"]) for item in sorted(values, key=lambda entry: entry["event_id"])],
        "event_count": len(values),
        "totals": totals,
        "unavailable": unavailable,
        "run_status": "completed" if statuses == {"completed"} else "incomplete",
        "observed_statuses": sorted(statuses),
    }
    result["metrics_digest"] = canonical_digest(result)
    return result


def compare_metrics(baseline: Any, candidate: Any, *, quality_equivalent: bool | None) -> dict[str, Any]:
    values = []
    for label, raw in (("baseline", baseline), ("candidate", candidate)):
        if not isinstance(raw, Mapping) or raw.get("schema") != "loop-metrics/v1":
            raise LoopMetricsError(label + " is not loop-metrics/v1")
        supplied = raw.get("metrics_digest")
        unsigned = {key: copy.deepcopy(value) for key, value in raw.items() if key != "metrics_digest"}
        if supplied != canonical_digest(unsigned):
            raise LoopMetricsError(label + " metrics digest does not match")
        values.append(copy.deepcopy(dict(raw)))
    left, right = values
    if any(left[field] != right[field] for field in ("work_id", "scenario", "model", "reasoning_effort")):
        raise LoopMetricsError("metrics are not like-for-like")
    if quality_equivalent not in {True, False, None}:
        raise LoopMetricsError("quality_equivalent must be boolean or null")
    changes = {}
    for field in _MEASURES:
        before, after = left["totals"][field], right["totals"][field]
        changes[field] = None if before is None or after is None else after - before
    claimable = (
        left["run_status"] == "completed"
        and right["run_status"] == "completed"
        and quality_equivalent is True
        and changes["duration_ms"] is not None
    )
    result = {
        "schema": "loop-metrics-comparison/v1",
        "baseline_ref": {"id": "baseline", "digest": left["metrics_digest"]},
        "candidate_ref": {"id": "candidate", "digest": right["metrics_digest"]},
        "changes": changes,
        "quality_equivalent": quality_equivalent,
        "performance_claim_allowed": claimable,
        "reason": "comparable-completed-runs" if claimable else "incomplete-or-unavailable-comparison",
    }
    result["comparison_digest"] = canonical_digest(result)
    return result

__all__ = ["LoopMetricsError", "compare_metrics", "derive_metrics"]
