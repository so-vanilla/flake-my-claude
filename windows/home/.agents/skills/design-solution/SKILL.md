---
name: design-solution
description: Turn an approved option and canonical specification into an implementable solution design.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_planning_system.py::D6_SolutionDesignTests
---

# Design solution

## Invocation boundary

Follow the [Inception single-Skill protocol](../entry/references/inception-single-skill.md).
Execute only this Skill, save its output and physical handoff, then stop for
human clear and explicit next invocation. A handoff below names the next
Skill; it never authorizes automatically invoking it in this conversation.

Use `PlanningSystemV1.compile` through the `group.D.D6` selector with physical
approved-option and canonical-spec references.

Define architecture, component responsibilities, interfaces, data and control
flow, error handling, compatibility, migration, observability, security, and
test seams in `design.md`. Separate accepted design decisions from unresolved
items.

Complete when downstream work can implement the design without reinterpreting
the specification. Stop on absent option approval, a stale spec, or an
unowned critical decision; do not choose or implement around it. Hand the
design and its references to `design-contracts`.
