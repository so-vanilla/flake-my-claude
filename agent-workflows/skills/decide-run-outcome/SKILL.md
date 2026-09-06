---
name: decide-run-outcome
description: Record an H2 human-authorized Run outcome grounded in the final objective audit.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_workflow_completion.py::H2_DecideRunOutcomeTests
---

# Decide Run outcome

## Inputs

Require the digest-bound H1 objective audit, current risks, remaining tasks,
Run and objective versions, and an explicit user or authorized-human decision.

## Method

Choose exactly one of `achieved`, `partially achieved`, `not achieved`,
`superseded`, or `abandoned`. Record the rationale, objective-audit reference,
unmet reasons, remaining tasks, approver, and receipt without collapsing
delivery status into objective outcome.

## Output and completion

Return a Run outcome event bound to the audit and human decision. Complete only
when the outcome, audit reference, unmet reasons, remaining tasks, and approver
are recorded; this event does not itself archive, delete, or start a Run.

## Stop and handoff

Stop on implicit or AI-authored judgment, stale audit or Run identity, or
authority mismatch. Preserve the audit and request the missing human decision.
Hand a valid outcome event to `archive-or-continue` without live or external
mutation.
