---
name: approve-objective
description: Construct a fixture-scoped objective approval command from an explicit human receipt.
disable-model-invocation: true
acceptance-test: test_objective_system.py::B7_ApproveObjectiveTests
---

# Approve objective

Use `ObjectiveSystemV1.prepare_approval` only with the selected option,
feasibility refs, an explicit human approval receipt, the prior-objective
pointer, and the expected HEAD. Construct only the fixture-scoped approval
command for the S2-U adapter; it carries the atomic version, event, pointer,
actor, time, impact, and reopen conditions.

Completion is an `objective-approval-command/v1` for the fixture namespace and
`fixture-only` scope. Hand off that command and its refs to the adapter.

An absent or implicit receipt, a non-human source, fixture-to-live mismatch, or
stale HEAD stops without mutation (`blocked_implicit_approval`,
`blocked_fixture_live`, or `blocked_stale_head`). Chat, task start, and AI
inference are never approval evidence; do not offer a fixture-to-live
conversion.
