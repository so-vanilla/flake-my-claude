---
name: decompose-outcomes
description: Decompose an approved objective into owner-bound, verifiable outcomes.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_outcome_system.py::C1_DecomposeOutcomesTests
---

# Decompose outcomes

## Invocation boundary

Follow the [Inception single-Skill protocol](../entry/references/inception-single-skill.md).
Execute only this Skill, save its output and physical handoff, then stop for
human clear and explicit next invocation. A handoff below names the next
Skill; it never authorizes automatically invoking it in this conversation.

Use `OutcomeSystemV1.decompose_outcomes` through the `group.C.C1` selector
with an approved objective reference and its physical input references.

For every outcome, provide an achieved state, why it is required, objective
contribution, exclusion conditions, owner, and acceptance-predicate references.
Cover every required objective contribution. Frame nodes as verifiable results,
not tasks or solution choices.

Complete by returning the compiled outcome map or typed refusal. A task or
solution-shaped node returns `outcome_is_implementation_task`; incomplete
contribution coverage returns `coverage_hole`. Hand the outcome map and its
physical references to `build-outcome-dependency-graph`; do not retain mutable
state, create C-01, or prescribe C3 measurement work.
