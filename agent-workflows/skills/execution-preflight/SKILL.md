---
name: execution-preflight
description: Validate a ready Group E attempt against current state, authority, and finite budgets before dispatch.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_execution_group.py::E1_ExecutionPreflightTests
---

# Execution preflight

Use `group.E.E1` with the physical readiness approval, Artifact/Task DAG,
current HEAD, workspace receipt, and Git-state receipt. Validate their paths,
schemas, digests, creators, revisions, and freshness. Bind the Run, objective
version, sub-objective contribution, workflow/graph versions, actor assignment,
write lease, authority, non-goals, expected HEAD, idempotency key, rollback,
context budget status, and finite review/time/attempt budget.

Check dependency artifacts, open blocking Findings, execution-package closure,
resource conflicts, and any durable non-dispatch terminal. A changed planning
input requires a versioned replan; an unset or insufficient budget, stale HEAD,
conflicting lease, unresolved blocker, or terminal without an explicitly
approved replacement budget and reopen command returns a typed stop.

Complete with one physical preflight receipt that records every check, purpose
audit, remaining budget, and versioned next advice. Hand off to `dispatch-task`
only on an exact pass. The DAG Orchestrator alone may validate a later command
and advance HEAD; this Skill dispatches nothing and grants no external action.
