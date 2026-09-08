---
name: propose-true-purpose
description: Present a human with distinct purpose options and a recommendation for an explicit choice.
disable-model-invocation: true
acceptance-test: test_objective_system.py::B5_ProposePurposeTests
---

# Propose true purpose

## Invocation boundary

Follow the [Inception single-Skill protocol](../entry/references/inception-single-skill.md).
Execute only this Skill, save its output and physical handoff, then stop for
human clear and explicit next invocation. A handoff below names the next
Skill; it never authorizes automatically invoking it in this conversation.

Use `ObjectiveSystemV1.propose_options` with inquiry and context refs. Present
two or three distinguishable options, each with a material trade-off in scope,
cost, or authority, plus one recommendation.

The completion is the selector `B5.unselected-options`: the option set remains
unselected for the human to decide. Hand off the options, recommendation, and
their refs for an explicit human choice.

If a selection is supplied or inferred without that choice, stop with
`blocked_implicit_approval`; retain the unselected proposal.
