---
name: scan-decision-evidence
description: Inventory the physical sources of decision evidence within a bounded G1 scope.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_decision_system.py::G1_ScanDecisionEvidenceTests
---

# Scan decision evidence

## Inputs

Require a physical Run, period, and Group scope; event, checkpoint, report,
artifact, and existing ADR references; their digests and freshness; and
read-only authority for the stated scope.

## Method

1. Fix the search boundaries before scanning.
2. Enumerate sources that record or imply a decision.
3. Reference evidence by physical path or event ID instead of copying it.
4. Record both searched coverage and every inaccessible or unreadable range.

## Output and completion

Return a source inventory bound to the input scope and references. Complete
only when the searched and unreadable ranges are explicit and every inventory
entry resolves to physical evidence; this is evidence discovery, not approval.

## Stop and handoff

Stop on missing scope authority, stale identity, or unresolvable evidence and
preserve the partial inventory with the reason. Hand the inventory or typed
stop to `classify-decision-status`; do not change source records or durable
decisions.
