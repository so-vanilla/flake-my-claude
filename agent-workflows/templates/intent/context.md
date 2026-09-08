# Context projection

Canonical source: `ObjectiveSystemV1.compile` artifact for `group.B.B2`.
This is a deterministic display projection, not state or an approval receipt.

## Context records

| Kind | Value source | Evidence ref |
| --- | --- | --- |
| Sourced fact | `{{inputs[0].selector}}` | `{{inputs[0].path}}` / `{{inputs[0].digest}}` |
| Inference | `{{candidate.candidate_ref.selector}}` | `{{candidate.candidate_ref.path}}` / `{{candidate.candidate_ref.digest}}` |
| Assumption | `{{authority.source_refs[0].selector}}` | `{{authority.source_refs[0].path}}` / `{{authority.source_refs[0].digest}}` |
| Unknown | `{{expected_head.revision}}` | `{{expected_head.transaction_digest}}` |

## Authority

- Owner kind: `{{authority.owner_ref.owner_kind}}`
- Owner ID: `{{authority.owner_ref.owner_id}}`
- Authority evidence: `{{authority.owner_ref.authority_ref.path}}`
