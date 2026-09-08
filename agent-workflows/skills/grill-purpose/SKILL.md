---
name: grill-purpose
description: Ask one material purpose question when a human needs to resolve an undiscoverable unknown.
disable-model-invocation: true
acceptance-test: test_objective_system.py::B4_GrillPurposeTests
---

# Grill purpose

## Invocation boundary

Follow the [Inception single-Skill protocol](../entry/references/inception-single-skill.md).
Execute only this Skill, save its output and physical handoff, then stop for
human clear and explicit next invocation. A handoff below names the next
Skill; it never authorizes automatically invoking it in this conversation.

Use `ObjectiveSystemV1.compile` with `group.B.B4`, the current context, scope, and
prior human answers. Supply the physical input refs and append the resulting
inquiry to its existing record.

Ask at most one question at a time, and make it the material unknown that cannot be
discovered from the supplied sources. The completion is the selector
`B4.one-material-question` with that single next question.

When no material unknown remains, supply `material_unknowns: []`,
`inquiry_complete: true`, a nonempty `resolution_reason`, and physical
`resolution_refs`. The compiler returns `B4.inquiry-resolved`; do not invent a
question merely to continue. If an answer is still needed, use
`needs_user_purpose`. Do not choose or revise the purpose.

In an explicitly authorized rehearsal, record answers with `source: mock`;
never label generated answers human or use them as real approval.
