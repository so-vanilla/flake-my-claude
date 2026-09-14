---
name: bootstrap-classify-environment
description: Classify supplied Bootstrap environment paths into a conservative A1 inventory without changing the workspace.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_bootstrap_contracts.py::A1_EnvironmentInventoryTests
---

# Bootstrap A1: classify environment

## Purpose

Produce a `bootstrap-artifact/v1` inventory through
`BootstrapContractsV1.classify_environment`. This Skill is explicit-only and
does not operate on a workspace.

## Physical inputs and authority

Read the repository-root, Git-state, AI/Nix/runtime path observations supplied
as path/digest-bound input. Source creation authority permits only
candidate-generic compilation; it does not permit writes to inspected paths.

## Steps

1. Verify each input reference is physical, fresh, and within the supplied
   candidate scope.
2. Classify every supplied path as source, generated, managed, unmanaged,
   app-owned, secret-bearing, or ephemeral; retain its owner, source,
   retention, and inspection method.
3. Compile the inventory through the public A1 seam and validate its result
   against `bootstrap-artifact/v1`.

## Completion and stop

Completion requires every path to retain its supplied ownership and a valid
artifact. Stop with the typed `unknown-owner` refusal when ownership is not
known; do not infer an owner or write the path.

## Handoff

Return the inventory artifact, its input references, outcome, and the next
A2 workspace-planning input. Do not return conversation text as an input.
