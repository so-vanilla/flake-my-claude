---
name: sequence-and-parallelize
description: Build a safe task dependency DAG with disjoint parallel writes and explicit convergence.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_planning_system.py::D9_ExecutionDAGTests
---

# Sequence and parallelize

Use `PlanningSystemV1.compile` through the `group.D.D9` selector with physical
task-plan, contract, and outcome-DAG references.

Define task dependencies, non-overlapping write sets, parallel batches, batch
convergence, and tightly coupled tasks in `planning/execution-dag.yaml`. Only
sibling tasks may run in parallel; each domain convergence point needs one
convergence Worker followed by a fresh Reviewer.

Complete with an acyclic, owner-bound execution DAG. Stop by serializing any
causal decision chain or write scope that cannot be separated safely. Hand the
DAG and its references to `prepare-worker-briefs`; do not issue workers.
