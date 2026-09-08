---
name: classify-scope
description: Classify a B3 request scope across depth, operation, ownership, and reversibility before further planning.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_objective_system.py::B3_ClassifyScopeTests
---

# Classify scope

## Invocation boundary

Follow the [Inception single-Skill protocol](../entry/references/inception-single-skill.md).
Execute only this Skill, save its output and physical handoff, then stop for
human clear and explicit next invocation. A handoff below names the next
Skill; it never authorizes automatically invoking it in this conversation.

## Purpose

Use `ObjectiveSystemV1.classify_scope` semantics through the `group.B.B3`
selector to compile a scope classification, without authorizing work.

## Physical inputs and authority

Require the intake and sourced context as physical inputs. Authority permits
classification only and must identify an owner; it cannot convert an unknown
owner into an assumption.

## Steps

1. Classify depth and operation.
2. Classify ownership as personal or company, and record any omission reason.
3. Classify reversibility.
4. Supply the classification and authority to the B3 selector.

## Completion and stop

Completion is a B3 scope-classified candidate with all four dimensions and any
omission reason explicit. Stop with `blocked_missing_authority` for an unknown
owner or risky quick work; do not continue under a quick-work assumption.

## Handoff

Return the scope candidate or typed refusal with its physical inputs for the
next objective workflow step.
