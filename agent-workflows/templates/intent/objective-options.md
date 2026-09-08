# Objective options projection

Canonical source: `ObjectiveSystemV1.compile` artifact for `group.B.B5`.
This is a deterministic display projection, not state or an approval receipt.

## Candidate options

| Option | Distinguishing trade-off | Scope | Cost | Authority |
| --- | --- | --- | --- | --- |
| Option 1 | `{{inputs[0].selector}}` | `{{inputs[0].path}}` | `{{inputs[0].digest}}` | `{{authority.owner_ref.owner_id}}` |
| Option 2 | `{{candidate.candidate_ref.selector}}` | `{{candidate.candidate_ref.path}}` | `{{candidate.candidate_ref.digest}}` | `{{authority.owner_ref.owner_kind}}` |
| Option 3 | `{{authority.source_refs[0].selector}}` | `{{authority.source_refs[0].path}}` | `{{authority.source_refs[0].digest}}` | `{{authority.owner_ref.authority_ref.path}}` |

## Non-selection note

- Recommendation evidence: `{{expected_head.transaction_digest}}`
- Selection is absent from this projection.
