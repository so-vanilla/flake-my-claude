---
name: entry
description: Record a new request as a B1 intake while keeping the request text separate from interpretation and uncertainty.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_objective_system.py::B1_EntryTests
---

# Entry

## Purpose

Use `ObjectiveSystemV1.capture_intake` semantics through the `group.B.B1`
selector to compile a candidate intake, without creating a Run or approving an
objective.

## Physical inputs and authority

Require physical references for the raw request, relevant references or
deadline, and the Run-index snapshot. Authority permits candidate compilation
only; it does not create, resume, or select a Run.

## Steps

1. Preserve the raw request verbatim.
2. Record interpretation, assumptions, and unknowns separately from the raw
   request.
3. Supply the physical inputs and current authority to the B1 selector.

## Completion and stop

Completion is a B1 intake candidate whose raw request, interpretation,
assumptions, and unknowns remain distinct. Stop with
`needs_user_duplicate_run` when an active duplicate makes resume versus new
material; do not infer that choice.

## Handoff

Return the compiled intake candidate or typed refusal with its physical
references for `discover-context`.
