---
name: define-targets
description: Define source-bound outcome targets from an approved measurement plan and its deadline window.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_outcome_system.py::C4_TargetDefinitionTests
---

# Define targets

Use `OutcomeSystemV1.define_targets` through the `group.C.C4` selector with
the measurement plan, deadline/window, and physical source references.

Specify one shared target set with its unit, formula, source, frequency,
window, and guard. Complete by returning the compiled target set or typed
refusal. Reject a contradictory target or a divergent worker-local success
definition instead of creating a local target.

Hand the target set and references to `capture-baseline`; do not collect data
or declare a pass.
