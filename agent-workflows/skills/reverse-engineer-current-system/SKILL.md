---
name: reverse-engineer-current-system
description: Map the current system and adjacent verification surfaces before specifying a change.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_planning_system.py::D2_CurrentSystemTests
---

# Reverse engineer current system

## Invocation boundary

Follow the [Inception single-Skill protocol](../entry/references/inception-single-skill.md).
Execute only this Skill, save its output and physical handoff, then stop for
human clear and explicit next invocation. A handoff below names the next
Skill; it never authorizes automatically invoking it in this conversation.

Use `PlanningSystemV1.compile` through the `group.D.D2` selector with physical
objective, outcome, and repository or business-process references under
read-only authority.

Trace entrypoints, domain terms, data and control flow, external contracts,
tests, existing constraints, and prior decisions. Independent surfaces may be
researched in parallel, but one owner must converge their sourced findings into
`planning/current-system.md`.

Complete when the change surface, adjacent contracts, and verification surfaces
are explicit. Stop on missing read authority or material unsourced gaps rather
than guessing. Hand the current-system artifact and its references to
`discover-practices`; do not modify the inspected system.
