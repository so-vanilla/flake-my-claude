---
name: design-measurement
description: Design an outcome measurement plan from the current C-01 bundle and available source capabilities.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_outcome_system.py::C3_MeasurementDesignTests
---

# Design measurement

Use `OutcomeSystemV1.design_measurement` through the `group.C.C3` selector
with the current C-01 bundle and physical source-capability references.

Choose `direct_metric`, `proxy`, `qualitative_rubric`, or `not_measured` in
the measurement plan. Bind the choice to its rationale, gaming risk, and guard.
`not_measured` requires its explicit, objective/profile-bound decision; it
does not synthesize a comparison or success value.

Complete by returning the compiled measurement plan or typed refusal. When no
meaningful measure is available, return `needs_user` rather than inventing one.
Hand the plan and physical references to `define-targets`; do not collect live
data or pass an outcome.
