---
name: converge-parallel-batch
description: Converge accepted sibling results into one reviewed, digest-bound batch result.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_execution_group.py::E8_ConvergeParallelBatchTests
---

# Converge parallel batch

Use `group.E.E8` with physical sibling result, review, validation, and receipt
objects plus their shared contract, batch frontier, expected HEAD, actor
assignments, freshness, and remaining finite budget. Retain each independently
successful result and reject incomplete, conflicting, or unbound joins.

For a purely mechanical join, the DAG Orchestrator may validate compatibility
and compile the next frontier. When integration needs domain judgment, issue
one immutable package to a convergence Worker to reconcile contradictions,
duplicates, gaps, alignment, and integration risk, then send its frozen Section
candidate to a fresh Reviewer. The Orchestrator and Thin Controller do not make
that domain judgment or ingest full tool transcripts.

Complete with a reviewed canonical convergence result and a HEAD-bound batch
Artifact Bundle containing paths, versions, digests, Findings, evidence,
unresolved items, budget consumption, purpose audit, and next frontier. Close
the batch Epoch. Stop when fragmented knowledge cannot be safely converged or
budget expires, preserving partial success for a tightly coupled replan. This
Skill does not itself advance HEAD or grant external action; only a validated
Orchestrator command may advance canonical state.
