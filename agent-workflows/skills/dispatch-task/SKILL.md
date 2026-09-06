---
name: dispatch-task
description: Compile one immutable, budget-bounded Task package for a ready Group E attempt.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_execution_group.py::E2_DispatchTaskTests
---

# Dispatch task

Use `group.E.E2` only with a passing E1 receipt, a ready attempt, current HEAD,
and digest-bound dependency artifacts. The DAG Orchestrator validates role,
assignment, lease, authority, expected HEAD, idempotency key, graph version,
and remaining finite budget before issuing the package and dispatch receipt.

Bind one independent Epoch, `artifact | section | workflow` loop level, exact
execution-package closure, input refs, objective contribution, write scope,
non-goals, acceptance, stop conditions, freshness, output namespace, timeout,
grace, supervision, and retry counters. Use `gpt-5.6-luna` at maximum effort by
default. Keep tightly coupled work in one package for one Worker.

Complete when the immutable package enables exactly one assigned Worker to
return `DONE`, `DONE_WITH_CONCERNS`, `NEEDS_CONTEXT`, or `BLOCKED` at its
physical output path. Refuse dispatch from a stale HEAD, incompatible frontier,
or durable non-dispatch terminal lacking a validated reopen command. The
package conveys only its recorded authority; it implies no external action.
