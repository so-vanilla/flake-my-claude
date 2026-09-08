---
name: approve-objective
description: Compile an explicit objective approval and hand its physical receipt to the fixture or project-local runtime adapter.
disable-model-invocation: true
acceptance-test: test_objective_system.py::B7_ApproveObjectiveTests
---

# Approve objective

## Invocation boundary

Follow the [Inception single-Skill protocol](../entry/references/inception-single-skill.md).
Execute only this Skill, save its output and physical handoff, then stop for
human clear and explicit next invocation. A handoff below names the next
Skill; it never authorizes automatically invoking it in this conversation.

Use `ObjectiveSystemV1.compile` with `group.B.B7` only with the selected option,
feasibility refs, an explicit human approval receipt, the prior-objective
pointer, and the expected HEAD. The compiler constructs a candidate command;
it does not mutate the objective pointer itself.

For retained S2-U fixtures, use `fixture-only` and the fixture namespace.
For an actual project, use `project-local`, its exact project/Run identity,
and the `runtime_approval` adapter described in the linked protocol. The
adapter validates physical receipt bytes, then records the atomic approval,
version history and event in the project-local Kernel. Report completion only
from that transaction; a draft command is not an approved objective.

Only an explicitly authorized isolated rehearsal may use a `mock` receipt and
`rehearsal` mode. Preserve both labels throughout; never turn them into human
approval. The adapter does not infer the human's response or invoke C1.

An absent or implicit receipt, a source/mode mismatch, fixture-to-live mismatch, or
stale HEAD stops without mutation (`blocked_implicit_approval`,
`blocked_fixture_live`, or `blocked_stale_head`). Chat, task start, and AI
inference are never approval evidence; do not offer a fixture-to-live
conversion.
