---
name: draft-durable-records
description: Draft G4 preview candidates for durable decision records from a decision graph.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_decision_system.py::G4_DraftDurableRecordsTests
---

# Draft durable records

## Inputs

Require a digest-bound G3 decision graph, the target repository's decision
record conventions, destination candidates, and draft-only write authority.

## Method

Create one preview candidate per independent decision. Include context,
decision, drivers, options, rationale and evidence, rejected alternatives,
consequences, owner, revisit trigger, and physical source references. Separate
the current rule from its history and label every output as a candidate.

## Output and completion

Return preview decision candidates and proposed destinations. Complete only
when no record combines independent decisions and each field and source link is
accounted for. A complete draft remains unapproved and non-canonical.

## Stop and handoff

Stop on missing repository convention, unresolved graph conflict, or write
scope outside the preview area. Preserve draftable candidates and hand the
previews plus proposed diffs to `approve-promotion`; do not promote, commit,
push, deploy, or mutate external state.
