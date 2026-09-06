---
name: bootstrap-detach-legacy-owner
description: Compile an A5 legacy-detach plan only after a physical non-use authority receipt, without detaching legacy runtime ownership.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_bootstrap_contracts.py::A5_LegacyDetachTests
---

# Bootstrap A5: detach legacy owner

## Purpose

Produce a candidate legacy-detach plan through
`BootstrapContractsV1.plan_legacy_detach`; it separates proposed runtime
ownership from retained research without performing a detach.

## Physical inputs and authority

Require digest-bound inventory and rollback references, a physical non-use
authority receipt, and A6 input refs. Source authority permits a plan only;
it does not authorize deletion, activation, approval, or Git operations.

## Steps

1. Verify every input ref and the non-use receipt are physical, fresh, and
   bound to the candidate scope.
2. Retain research/provenance while identifying the proposed legacy runtime
   boundary and A6 input bundle.
3. Compile and schema-validate the public A5 result.

## Completion and stop

Completion requires a physical non-use authority receipt and complete A6
refs. Stop with `missing-non-use-authority` or a typed refusal; do not detach
or mutate any legacy owner.

## Handoff

Return the detach plan, non-use receipt, inventory/rollback/A6 references,
outcome, and any unapproved operation for human authorization.
