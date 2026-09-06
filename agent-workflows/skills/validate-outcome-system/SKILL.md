---
name: validate-outcome-system
description: Validate the trace from an objective through its outcome graph, measurements, targets, and baselines.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_outcome_system.py::C6_OutcomeValidationTests
---

# Validate outcome system

Use `OutcomeSystemV1.validate_system` through the `group.C.C6` selector with
physical objective, DAG, plan, target, and baseline references.

Validate the complete trace. An orphan, duplicate, mismatch, gaming concern,
or ownerless correction returns a typed upstream owner; it is not a local
repair or a synthesized pass.

Complete by returning the trace-validation result or typed upstream return with
its physical references. Hand the result to the outcome-system convergence
owner; do not use live data or claim outcome success.
