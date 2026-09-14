---
name: deduplicate-and-link
description: Build a G3 decision graph that exposes duplicates, conflicts, and supersession.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_decision_system.py::G3_DeduplicateAndLinkTests
---

# Deduplicate and link

## Inputs

Require the G2 classification table, physical existing ADR and design-record
references, their chronology and digests, and read-only comparison authority.

## Method

Compare decision identity, scope, time, and authority. Link equivalent records,
expose contradictions, and represent `supersedes` and `superseded-by` in both
directions. Keep approved decisions distinct from proposals and superseding
candidates even when their text overlaps.

## Output and completion

Return a decision graph whose every candidate is classified as new, duplicate,
conflicting, or superseding and links back to its evidence. Complete only when
chronology and unresolved conflicts are explicit; the graph is not a durable
decision record.

## Stop and handoff

Stop rather than merge identities when scope, chronology, or authority cannot
be resolved. Preserve both nodes and the ambiguity, then hand the graph or
typed stop to `draft-durable-records`; do not rewrite existing records.
