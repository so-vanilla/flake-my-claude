import copy
import json
import unittest
from pathlib import Path

from ai_agent_workflow.loop_metrics import (
    LoopMetricsError,
    compare_metrics,
    derive_metrics,
)
from ai_agent_workflow.schema_validation import validate_document

DIGEST = "sha256:" + "a" * 64
ROOT = Path(__file__).resolve().parents[1]


def event(identifier, **changes):
    value = {
        "schema": "loop-metric-event/v1", "event_id": identifier, "work_id": "work-1",
        "scenario": "zero-finding", "stage": "review", "model": "gpt-5.6-luna",
        "reasoning_effort": "max", "duration_ms": 100, "llm_calls": 1,
        "read_bytes": 1000, "test_invocations": 1, "status": "completed",
        "source_ref": {"id": identifier, "digest": DIGEST},
    }
    value.update(changes)
    return value


class LoopMetricsTests(unittest.TestCase):
    def test_aggregate_records_calls_time_reads_and_tests(self):
        events = [event("e1"), event("e2", duration_ms=50, llm_calls=0)]
        result = derive_metrics(events)
        self.assertEqual({"duration_ms": 150, "llm_calls": 1, "read_bytes": 2000, "test_invocations": 2}, result["totals"])
        self.assertEqual("completed", result["run_status"])
        registry = {
            path.name: json.loads(path.read_text())
            for path in (ROOT / "schemas").glob("*.schema.json")
        }
        for item in events:
            validate_document(item, registry["loop-metric-event-v1.schema.json"], registry)
        validate_document(result, registry["loop-metrics-v1.schema.json"], registry)

    def test_unavailable_is_null_not_zero(self):
        result = derive_metrics([event("e1", read_bytes=None)])
        self.assertIsNone(result["totals"]["read_bytes"])
        self.assertIn("read_bytes", result["unavailable"])

    def test_like_for_like_completed_quality_can_support_a_claim(self):
        baseline = derive_metrics([event("e1", duration_ms=200)])
        candidate = derive_metrics([event("e2", duration_ms=100)])
        result = compare_metrics(baseline, candidate, quality_equivalent=True)
        self.assertTrue(result["performance_claim_allowed"])
        self.assertEqual(-100, result["changes"]["duration_ms"])
        registry = {
            path.name: json.loads(path.read_text())
            for path in (ROOT / "schemas").glob("*.schema.json")
        }
        validate_document(result, registry["loop-metrics-comparison-v1.schema.json"], registry)

    def test_stopped_or_missing_or_quality_unknown_cannot_claim_speedup(self):
        baseline = derive_metrics([event("e1")])
        for candidate, quality in (
            (derive_metrics([event("e2", status="stopped")]), True),
            (derive_metrics([event("e3", duration_ms=None)]), True),
            (derive_metrics([event("e4")]), None),
        ):
            with self.subTest(candidate=candidate, quality=quality):
                self.assertFalse(compare_metrics(baseline, candidate, quality_equivalent=quality)["performance_claim_allowed"])

    def test_changed_model_or_tampered_metrics_are_rejected(self):
        baseline = derive_metrics([event("e1")])
        changed = derive_metrics([event("e2", model="gpt-5.6-sol")])
        with self.assertRaises(LoopMetricsError):
            compare_metrics(baseline, changed, quality_equivalent=True)
        tampered = copy.deepcopy(baseline)
        tampered["totals"]["duration_ms"] = 1
        with self.assertRaises(LoopMetricsError):
            compare_metrics(tampered, baseline, quality_equivalent=True)


if __name__ == "__main__":
    unittest.main()
