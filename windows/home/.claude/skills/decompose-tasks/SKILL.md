---
name: decompose-tasks
description: Decompose an approved design into independently reviewable tasks with exact boundaries and checks.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_planning_system.py::D8_TaskDecompositionTests
---

# Decompose tasks

## Invocation boundary

Follow the [Inception single-Skill protocol](../entry/references/inception-single-skill.md).
Execute only this Skill, save its output and physical handoff, then stop for
human clear and explicit next invocation. A handoff below names the next
Skill; it never authorizes automatically invoking it in this conversation.

Use `PlanningSystemV1.compile` through the `group.D.D8` selector with the fresh
D-02 bundle and physical spec, design, contracts, and outcome-target references.

Split work into the smallest units for which a reviewer can accept one and
reject a sibling. Give every task exact files, interface, inputs, outputs,
task-specific acceptance, and contribution to its parent outcome in
`planning/tasks.yaml`.

Complete when every task is independently verifiable and none is merely a
component-shaped placeholder. Stop and return an inseparable or underspecified
unit to its owning design or contract Skill. Hand the task plan and references
to `sequence-and-parallelize`; do not dispatch workers.
