---
name: verify-whole-change
description: Verify the integrated whole change in a fresh context against its declared plan and objective contribution.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_execution_group.py::E9_VerifyWholeChangeTests
---

# Verify whole change

Use `group.E.E9` with every reviewed batch Bundle, the frozen whole-change
candidate, aggregate receipts, canonical spec, verification plan, objective
contribution, non-goals, execution closure, fresh actor/Epoch assignment, loop
identity/history, and current evidence bindings. The verifier must be independent of implementation;
an exact deterministic check may cover only its declared predicate.

Run the declared integration, E2E, regression, and objective-contribution
checks over the complete candidate. Bind each command, arguments, expected and
actual result, environment, isolation, capture state, timing, and receipt to the
package closure. Send every new candidate Finding to E6 rather than repairing it.

Complete with a physical whole-change verification object and closed E9 Epoch
that exposes all unverified risk and typed stops. An iteration limit, stalled
progress, execution failure, or recovery requirement is a typed non-pass. This task-scoped verification is not Workflow
completion or acceptance: the verifier does not implement, close Findings,
update HEAD, approve risk, or authorize any external action.
