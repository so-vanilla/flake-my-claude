# Feasibility projection

Canonical source: `ObjectiveSystemV1.compile` artifact for `group.B.B6`.
This is a deterministic display projection, not state or an approval receipt.

## Constraints

| Class | Source |
| --- | --- |
| Hard | `{{inputs[0].path}}` / `{{inputs[0].digest}}` |
| Soft | `{{candidate.candidate_ref.path}}` / `{{candidate.candidate_ref.digest}}` |
| Assumption | `{{authority.source_refs[0].path}}` / `{{authority.source_refs[0].digest}}` |
| Open | `{{expected_head.revision}}` / `{{expected_head.transaction_digest}}` |

## Evidence owner

- Owner: `{{authority.owner_ref.owner_kind}}` / `{{authority.owner_ref.owner_id}}`
- Authority ref: `{{authority.owner_ref.authority_ref.path}}`
