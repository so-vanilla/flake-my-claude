---
name: assess-feasibility-and-constraints
description: Assess proposed purposes against sourced constraints and return a typed feasibility stop when needed.
disable-model-invocation: true
acceptance-test: test_objective_system.py::B6_FeasibilityTests
---

# Assess feasibility and constraints

## Invocation boundary

Follow the [Inception single-Skill protocol](../entry/references/inception-single-skill.md).
Execute only this Skill, save its output and physical handoff, then stop for
human clear and explicit next invocation. A handoff below names the next
Skill; it never authorizes automatically invoking it in this conversation.

Use `ObjectiveSystemV1.assess_feasibility` with option and constraint-snapshot
refs. One accountable owner converges delegated evidence into four explicit
classes: hard, soft, assumption, and open.

The completion is the selector `B6.constraints-separated`, with every supplied
constraint represented in one of those classes and its source preserved.

A hard constraint or missing authority ends in the typed stop
`blocked_missing_authority`. Hand off the classified constraints and refs; do
not resolve an open constraint by assumption.
