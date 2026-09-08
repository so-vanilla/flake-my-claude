---
name: bootstrap-initialize-run
description: Compile an A4 initial Run plan that preserves objective approval state and never creates runtime state.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_bootstrap_contracts.py::A4_RunInitializationTests
---

# Bootstrap A4: initialize Run

## Purpose

Produce an initial candidate Run plan through
`BootstrapContractsV1.initialize_run_plan` without creating a Run, event,
checkpoint, or runtime pointer.

## Physical inputs and authority

Require the approved master-plan ref, objective ref/status, and workspace-plan
ref. Source authority permits planning only and cannot promote a candidate or
unapproved objective.

## Steps

1. Verify the objective and workspace-plan references are physical and fresh.
2. Preserve the objective approval status exactly as supplied.
3. Compile and schema-validate the public A4 plan with current, next, and
   unapproved state explicit for a fresh reader.

## Completion and stop

Completion requires an already approved objective and valid references. Stop
with `objective-not-approved` for candidate or unapproved objectives; do not
create runtime state.

## Handoff

Return the candidate Run plan, objective/workspace references, outcome, and
the A5 detach-planning inputs.
