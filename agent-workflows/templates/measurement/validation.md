# Outcome-validation projection

Canonical source: `OutcomeSystemV1.compile` artifact for `group.C.C6`.
This display is trace-only. It does not infer validity, collect data, mutate
an epoch, or send an upstream return.

## Trace bindings

- Objective: `{{inputs.trace.objective.path}}` / `{{inputs.trace.objective.digest}}`
- Outcome map: `{{inputs.trace.outcome_map.schema}}`
- Dependency graph: `{{inputs.trace.dependency_graph.schema}}`
- Measurement plan: `{{inputs.trace.measurement_plan.schema}}`
- Target set: `{{inputs.trace.target_set.schema}}`
- Baseline: `{{inputs.trace.baseline.schema}}`
- C-01 bundle ref: `{{inputs.c01_bundle_ref.path}}` /
  `{{inputs.c01_bundle_ref.digest}}`
- C-02 bundle ref: `{{inputs.c02_bundle_ref.path}}` /
  `{{inputs.c02_bundle_ref.digest}}`

## Validation result

- Status: `{{output.payload.status}}`
- Upstream owner stable ID: `{{output.payload.upstream_owner_ref.stable_id}}`
- Upstream owner role: `{{output.payload.upstream_owner_ref.role}}`

Unknown values remain unknown and null remains null. An orphan, duplicate,
mismatch, gaming concern, or ownerless trace is returned to the named upstream
owner as a typed result; this projection does not convert it into a pass.
