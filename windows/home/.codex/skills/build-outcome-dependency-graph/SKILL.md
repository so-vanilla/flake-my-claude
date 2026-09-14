---
name: build-outcome-dependency-graph
description: Build an owner-bound prerequisite DAG from a compiled outcome map.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_outcome_system.py::C2_OutcomeDAGTests
---

# Build outcome dependency graph

## Invocation boundary

Follow the [Inception single-Skill protocol](../entry/references/inception-single-skill.md).
Execute only this Skill, save its output and physical handoff, then stop for
human clear and explicit next invocation. A handoff below names the next
Skill; it never authorizes automatically invoking it in this conversation.

Use `OutcomeSystemV1.build_dependency_graph` through the `group.C.C2` selector
with the compiled outcome map and physical input references.

Produce an acyclic prerequisite DAG with reachable outcome nodes, known edge
endpoints, and owner-bound convergence joins. Include the batches and joins
needed to establish the C-01 handoff.

Complete by returning the compiled dependency graph and C-01 handoff or typed
refusal. Stop on a dependency cycle, orphan outcome, unknown endpoint, or
ownerless join; do not repair those conditions locally. Do not retain mutable
state or provide C3 measurement guidance without the C-01 binding.
