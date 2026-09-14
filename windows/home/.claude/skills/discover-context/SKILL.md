---
name: discover-context
description: Build a B2 sourced-context candidate from an intake and read-only physical source snapshots.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_objective_system.py::B2_DiscoverContextTests
---

# Discover context

## Invocation boundary

Follow the [Inception single-Skill protocol](../entry/references/inception-single-skill.md).
Execute only this Skill, save its output and physical handoff, then stop for
human clear and explicit next invocation. A handoff below names the next
Skill; it never authorizes automatically invoking it in this conversation.

## Purpose

Use `ObjectiveSystemV1.discover_context` semantics through the `group.B.B2`
selector to compile sourced context, without changing sources or objective
state.

## Physical inputs and authority

Require the intake and read-only physical source snapshots. Every fact needs
its own physical source reference; authority permits inspection and candidate
compilation only.

## Steps

1. Read the supplied snapshots without mutation.
2. Attach a physical source reference to every fact.
3. Keep inference, assumptions, and unknowns distinct from sourced facts.
4. Supply the context and authority to the B2 selector.

## Completion and stop

Completion is a B2 sourced-context candidate with fact provenance explicit.
Stop with `blocked_missing_authority` when a material source or its authority
is absent; do not promote inference to fact.

## Handoff

Return the sourced-context candidate or typed refusal with its input references
for `classify-scope`.
