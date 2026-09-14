---
name: bootstrap-design-rollback
description: Compile an A3 owner-limited rollback plan from physical inventory and workspace references without backing up or restoring data.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_bootstrap_contracts.py::A3_RollbackDesignTests
---

# Bootstrap A3: design rollback

## Purpose

Produce recovery guidance through `BootstrapContractsV1.design_rollback` for
the candidate workspace; this Skill never performs backup, restore, or
activation.

## Physical inputs and authority

Require digest-bound inventory and workspace-plan inputs plus owned restore
sources. Source authority permits a plan only and never authorizes secret
backup.

## Steps

1. Verify every proposed restore source is physical and owned by the supplied
   inventory authority.
2. Exclude secret-bearing paths from every backup proposal.
3. Compile and schema-validate the public A3 rollback result, including
   pre- and post-activation recovery guidance.

## Completion and stop

Completion requires owned restore sources and no secret backup. Stop with
`missing-restore-source` or `secret-backup`; preserve the current source and
runtime when stopped.

## Handoff

Return the rollback plan, exact restore-source references, outcome, and A4
initialization inputs.
