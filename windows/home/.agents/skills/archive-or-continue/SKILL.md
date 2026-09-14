---
name: archive-or-continue
description: Plan H3 retention, rollback preservation, and an archive or continuation handoff.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_workflow_completion.py::H3_ArchiveOrContinueTests
---

# Archive or continue

## Inputs

Require the H2 outcome event, decision candidates and promotion status, the
retention policy, local artifact inventory, rollback references, and any
separately authorized next-Run identity.

## Method

Route unresolved important decisions through Group G. Classify local artifacts
for retention, protected rollback, or cleanup candidacy. Produce either an
archive manifest or a handoff linked to an already authorized next Run. Treat
cleanup as a proposal and retain provenance for every disposition.

## Output and completion

Return an archive manifest or new-Run link with retention, cleanup candidates,
decision candidates, rollback artifacts, and continuation location. Complete
only when every item and link is physically verified and the rollback path is
preserved.

## Stop and handoff

Stop while an important decision is unpromoted or a rollback-required artifact
would be lost. Do not delete artifacts, create a live Run, commit, push, or
mutate external state under this Skill's authority. Hand the verified manifest
or link to the Workflow closure owner for separately authorized action.
