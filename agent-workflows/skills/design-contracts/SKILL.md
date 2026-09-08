---
name: design-contracts
description: Fix versioned interfaces and compatibility boundaries before tasks are split for parallel work.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_planning_system.py::D7_ContractDesignTests
---

# Design contracts

## Invocation boundary

Follow the [Inception single-Skill protocol](../entry/references/inception-single-skill.md).
Execute only this Skill, save its output and physical handoff, then stop for
human clear and explicit next invocation. A handoff below names the next
Skill; it never authorizes automatically invoking it in this conversation.

Use `PlanningSystemV1.compile` through the `group.D.D7` selector with physical
design and existing external-contract references.

Specify consumes and produces, schema, version, failures, idempotency, and
backward compatibility as contract, schema, and test-fixture artifacts. Fix
parallel-task boundaries so sibling tasks do not share a mutable file.

Complete when each boundary is versioned, testable, and owned. Stop on an
unresolved incompatibility, ownerless contract, or inseparable write overlap.
Hand the artifacts to the D-02 bundle and clear boundary; an authority or
approval change closes the Epoch before `decompose-tasks`.
