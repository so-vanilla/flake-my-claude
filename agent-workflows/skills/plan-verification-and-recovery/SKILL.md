---
name: plan-verification-and-recovery
description: Define finite verification, review, recovery, and delivery gates for an execution plan.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_planning_system.py::D11_VerificationRecoveryTests
---

# Plan verification and recovery

## Invocation boundary

Follow the [Inception single-Skill protocol](../entry/references/inception-single-skill.md).
Execute only this Skill, save its output and physical handoff, then stop for
human clear and explicit next invocation. A handoff below names the next
Skill; it never authorizes automatically invoking it in this conversation.

Use `PlanningSystemV1.compile` through the `group.D.D11` selector with physical
task-plan, risk, and delivery-surface references.

Define test levels, independent review, Finding validation, E2E, dry-run,
rollback, post-check, and activation, commit, and push gates. Assign every task
a finite deadline or wall-clock timebox, maximum review rounds, and maximum fix
attempts per Finding. Produce `planning/verification-plan.md` and
`recovery-plan.md`.

Complete when rollback depth, retained successes, required approver, and each
budget-exhaustion stop are explicit. Stop on an unbounded budget or unspecified
delivery authority. Hand both plans and their references to
`implementation-readiness-review`; planning a gate neither grants nor consumes
approval.
