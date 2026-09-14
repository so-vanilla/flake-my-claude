---
name: discover-practices
description: Derive one sourced constraints list from actual repository, CI, review, and release practices.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_planning_system.py::D3_PracticeDiscoveryTests
---

# Discover practices

## Invocation boundary

Follow the [Inception single-Skill protocol](../entry/references/inception-single-skill.md).
Execute only this Skill, save its output and physical handoff, then stop for
human clear and explicit next invocation. A handoff below names the next
Skill; it never authorizes automatically invoking it in this conversation.

Use `PlanningSystemV1.compile` through the `group.D.D3` selector with the
current-system artifact plus physical repository-guidance, CI, and team-policy
references.

Compare multiple sources for coding, testing, review, release, and narrow owner
routing practices. Record discrepancies between written rules and observed
behavior, then compile one global constraints list in `planning/practices.md`.

Complete when downstream Skills have a single sourced constraints list. Stop
on unresolved contradictory authority or an unsupported convention instead of
choosing one silently. Hand the artifact and its references to
`specify-what-and-why`; do not change policy or repository behavior.
