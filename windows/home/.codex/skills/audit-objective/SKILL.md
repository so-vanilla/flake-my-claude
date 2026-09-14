---
name: audit-objective
description: Audit H1 delivery, trajectory, and objective realization as separate evidence-bound claims.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_workflow_completion.py::H1_AuditObjectiveTests
---

# Audit objective

## Inputs

Require the approved objective and version, every outcome and target, accepted
deliveries, unresolved findings, unverified items, and their physical evidence
references and digests.

## Method

Evaluate delivery completion, trajectory improvement, and realization of the
objective state independently. Bind evidence and uncertainty to each claim and
trace every outcome and target; never lift Skill, Group, or delivery completion
into objective achievement.

## Output and completion

Return a final objective audit with separate scoped findings and evidence.
Complete only when every claim resolves to evidence and every missing,
conflicting, or unverified item remains visible.

## Stop and handoff

Stop on stale objective identity, incomplete outcome coverage, or unavailable
material evidence and return an uncertain audit rather than a success claim.
Hand the audit and unresolved risks to `decide-run-outcome`; do not decide the
Run outcome or mutate live or external state.
