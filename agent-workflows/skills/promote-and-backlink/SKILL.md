---
name: promote-and-backlink
description: Promote an explicitly approved G6 candidate and create traceable repository-local backlinks.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_decision_system.py::G6_PromoteAndBacklinkTests
---

# Promote and backlink

## Inputs

Require an adopted candidate, its exact G5 human approval event, destination
and diff, expected HEAD, repository conventions, and repository-local write
authority limited to the approved promotion.

## Method

Revalidate the candidate digest, approval binding, destination, and HEAD.
Create the durable record at the approved ADR, `docs/decisions/`, or design
source location. Preserve source events and old ADRs; supersede an old record
with a new record. Record source and candidate references plus `promoted_to`
in a new promotion transaction and regenerate the backlink projection.

## Output and completion

Return the durable record, promotion transaction, backlink, and validation
result. Complete only when repository syntax and conventions pass and both
directions of traceability resolve.

## Stop and handoff

Stop without writes on stale or mismatched approval, HEAD, target, or
authority. Promotion grants no commit, push, deployment, live-Run, or external
mutation authority. Hand the validated repository-local result to the Group G
closure owner; only the DAG Orchestrator may commit workflow state or HEAD.
