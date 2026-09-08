---
name: capture-baseline
description: Record a comparable baseline from a supplied observation receipt for a defined outcome target.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_outcome_system.py::C5_BaselineTests
---

# Capture baseline

## Invocation boundary

Follow the [Inception single-Skill protocol](../entry/references/inception-single-skill.md).
Execute only this Skill, save its output and physical handoff, then stop for
human clear and explicit next invocation. A handoff below names the next
Skill; it never authorizes automatically invoking it in this conversation.

Use `OutcomeSystemV1.record_baseline` through the `group.C.C5` selector with
the target set and a physical observation receipt only.

Preserve an available value of `0` as available. Preserve an unavailable value
as `null` with its reason; retain stale, access, side-effect, or incomparable
observations as typed stops rather than converting them to values.

Complete by returning the compiled baseline or typed refusal. Hand the
baseline, target set, and receipt references to `validate-outcome-system`.
This Skill performs no observation collection and does not synthesize a pass.
