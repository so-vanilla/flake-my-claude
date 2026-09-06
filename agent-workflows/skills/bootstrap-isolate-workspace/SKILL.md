---
name: bootstrap-isolate-workspace
description: Compile a conservative A2 isolated-workspace plan from inventory and dirty-state references without creating a worktree.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_bootstrap_contracts.py::A2_WorkspaceIsolationTests
---

# Bootstrap A2: isolate workspace

## Purpose

Produce a candidate-only workspace plan through
`BootstrapContractsV1.plan_workspace`; no branch, worktree, import, or Git
operation is performed.

## Physical inputs and authority

Require the A1 inventory, base ref, dirty-path observation, local-input refs,
write scope, and rollback ref as physical inputs. Source authority permits a
plan only; it does not approve branch/ref selection or local-file import.

## Steps

1. Verify the supplied ref, dirty-state, local-input, rollback, and inventory
   references are current and digest-bound.
2. Compare dirty paths with the proposed write scope.
3. Compile and schema-validate the public A2 result with its ref, local input,
   and rollback binding.

## Completion and stop

Completion requires non-overlapping foreign diffs, a local input, and a
rollback ref. Stop with `foreign-diff-overlap`, `missing-local-input`, or a
typed refusal when a human decision is required.

## Handoff

Return the candidate workspace plan, exact input references, refusal or
completion outcome, and the A3 rollback inputs.
