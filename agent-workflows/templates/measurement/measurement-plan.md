# Measurement-plan projection

Canonical source: `OutcomeSystemV1.compile` artifact for `group.C.C3`.
This is a deterministic display projection, not collection, state, a pass
decision, or a credential request.

## Measurement

- Outcome: `{{output.payload.outcome_id}}`
- Strategy: `{{output.payload.strategy}}` (`direct_metric`, `proxy`,
  `qualitative_rubric`, or `not_measured`)
- Rationale: `{{output.payload.rationale}}`
- Gaming risk: `{{inputs.gaming_risk}}`
- Gaming guard ref: `{{inputs.gaming_guard_ref.path}}` /
  `{{inputs.gaming_guard_ref.digest}}`
- Rubric anchors: `{{inputs.rubric_anchor_refs[]}}`
- Not-measured decision ref: `{{inputs.decision_ref.path}}` /
  `{{inputs.decision_ref.digest}}`

Unknown values remain unknown and null remains null. This projection never
substitutes a measurement, imports it back into canonical data, or synthesizes
a pass result.
