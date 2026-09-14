---
name: classify-decision-status
description: Classify G2 decision evidence without turning candidates into approved decisions.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_decision_system.py::G2_ClassifyDecisionStatusTests
---

# Classify decision status

## Inputs

Require a digest-bound G1 source inventory, the referenced evidence, actor and
authority records, and the current Run and objective versions.

## Method

Classify each candidate as `observation`, `assumption`, `proposal`,
`temporary ruling`, `approved decision`, or `superseding candidate`. Record
the grounding event, relevant authority holder, and confidence separately.
Use `approved decision` only when explicit approval evidence and authority
both resolve.

## Output and completion

Return a classification table linked to every inventory entry. Complete only
when every candidate has a status, grounding event, authority holder or
explicit unknown, and confidence; classification does not promote a record.

## Stop and handoff

An unknown approver, missing authority, or ambiguous receipt cannot produce an
approved classification. Preserve it as a non-approved candidate or typed
stop, then hand the table to `deduplicate-and-link` without mutation.
