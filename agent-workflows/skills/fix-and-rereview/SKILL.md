---
name: fix-and-rereview
description: Run one bounded repair and fresh rereview for a validated required Finding.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_execution_group.py::E7_FixAndRereviewTests
---

# Fix and rereview

Use `group.E.E7` only after the DAG Orchestrator validates an E6 `required`
disposition and issues a fix Task package for the compatible required Finding
batch and permitted scope. Bind the original Task package/candidate, evidence,
expected HEAD, authority, lease, immutable loop history, and distinct technical
retry counter; an unchanged duplicate never consumes an additional iteration.

The Worker changes only the authorized branch and submits a resolution claim
with affected checks and declared regression evidence. Separate fresh delta
Reviews, or an exactly pre-approved deterministic predicate, check every
Finding ID and the aggregate regression scope. Route every newly observed
candidate back to E6.

Complete with a physical fix result, resolution claim, and fresh Verdict attempt
supporting `resolved`, `superseded`, accepted-risk/human routing, or continued
open status. The Worker never self-closes; review evidence does not itself move
HEAD. When the phase iteration limit is exhausted, progress stalls, or recovery
evidence is required, issue no new Worker and hand the preserved successes,
open IDs, immutable history, and typed stop to the Orchestrator. No external
action is authorized by this loop.
