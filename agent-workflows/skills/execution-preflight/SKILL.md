---
name: execution-preflight
description: Validate a ready Group E attempt against current state, authority, loop policy, and evidence before dispatch.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_execution_group.py::E1_ExecutionPreflightTests
---

# Execution preflight

Use `group.E.E1` with the physical readiness approval, Artifact/Task DAG,
current HEAD, workspace receipt, and Git-state receipt. Validate their paths,
schemas, digests, creators, revisions, and freshness. Bind the Run, objective
version, sub-objective contribution, workflow/graph versions, actor assignment,
write lease, authority, non-goals, expected HEAD, idempotency key, rollback,
context status, loop identity, immutable history, phase iteration count, and
technical retry count.

Check dependency artifacts, open blocking Findings, execution-package closure,
resource conflicts, and any durable non-dispatch terminal. A changed planning
input requires a versioned replan; a stale HEAD, conflicting lease, unresolved
blocker, exhausted phase limit, stalled progress, or execution-unknown without
an explicit recovery proof returns a typed stop.

Complete with one physical preflight receipt that records every check, purpose
audit, loop counters, evidence freshness, and versioned next advice. Hand off to `dispatch-task`
only on an exact pass. The DAG Orchestrator alone may validate a later command
and advance HEAD; this Skill dispatches nothing and grants no external action.
