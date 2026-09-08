---
name: fix-and-rereview
description: Run one bounded repair and fresh rereview for a validated required Finding.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_execution_group.py::E7_FixAndRereviewTests
---

# Fix and rereview

Use `group.E.E7` only after the DAG Orchestrator validates an E6 `required`
disposition and issues a fix Task package for that Finding ID and permitted
scope. Bind the original Task package/candidate, evidence, expected HEAD,
authority, lease, remaining time/round/attempt budget, and distinct retry
counters; an unchanged duplicate never consumes a product-fix attempt.

The Worker changes only the authorized branch and submits a resolution claim
with focused and declared regression evidence. A separate fresh Review attempt,
or an exactly pre-approved deterministic predicate, checks that same Finding ID
and regression scope. Route every newly observed candidate back to E6.

Complete with a physical fix result, resolution claim, and fresh Verdict attempt
supporting `resolved`, `superseded`, accepted-risk/human routing, or continued
open status. The Worker never self-closes; review evidence does not itself move
HEAD. When the remaining budget cannot contain repair plus rereview, or a limit
is exhausted, issue no new Worker and hand the preserved successes, open IDs,
and stop reason to the Orchestrator for `stopped_budget`. No external action is
authorized by this loop.
