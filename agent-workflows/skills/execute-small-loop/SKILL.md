---
name: execute-small-loop
description: Execute one Worker-owned implementation and inspection loop inside an immutable Task package.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_execution_group.py::E3_ExecuteSmallLoopTests
---

# Execute small loop

Use `group.E.E3` as the assigned Worker with one fresh Task package. Revalidate
its package/closure digest, Epoch, objective contribution, inputs, freshness,
write scope, non-goals, acceptance, stop conditions, supervision, and remaining
finite budget before touching the scoped output.

Implement one behavior, hypothesis, or artifact unit; inspect it; and preserve
the result and evidence before the next bounded unit. A retry must record the
changed hypothesis, context, tool, task size, or authorized model attempt and
its retry class. Preserve already successful independent outputs.

Complete with the scoped change or artifact, result object, test receipt,
changed paths, purpose audit, unresolved items, timing, and versioned handoff at
the package output path. Stop on spec or objective change, write-scope escape,
expired budget, missing authority, or required external critical action. The
Worker submits work and resolution claims only; it never changes HEAD, issues a
Verdict, closes a Finding, or acquires external authority.
