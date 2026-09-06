---
name: implementation-readiness-review
description: Review planning artifacts for cross-artifact readiness and route defects back to their owning Skill.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_planning_system.py::D12_ReadinessReviewTests
---

# Implementation readiness review

Use `PlanningSystemV1.compile` through the `group.D.D12` selector with physical
spec, design, contract, task, execution-DAG, verification, and recovery
references.

Check cross-artifact contradictions, placeholders, unresolved critical
decisions, objective traceability, and authority. Record evidence and one of
`ready`, `ready_with_accepted_risks`, or `not_ready` in
`planning/readiness-review.md`; route corrections to the owning upstream Skill
instead of editing its artifact here.

Complete only with the evidence-bound disposition and any required written
human approval. Stop before execution on a `not_ready` result, unaccepted risk,
stale input, or missing approval. Hand the D-03 bundle to
`execution-preflight` only after clear, with every next input fixed by path,
version, and digest.
