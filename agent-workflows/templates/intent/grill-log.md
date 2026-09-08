# Inquiry projection

Canonical source: `ObjectiveSystemV1.compile` artifact for `group.B.B4`.
This is a deterministic display projection, not state or an approval receipt.

## Append-only inquiry entry

- Prior inquiry evidence: `{{inputs[0].path}}` (`{{inputs[0].digest}}`)
- Material unknown: `{{candidate.candidate_ref.selector}}`
- Next question (exactly one): `{{candidate.candidate_ref.path}}`
- Question authority: `{{authority.owner_ref.authority_ref.selector}}`
- Expected HEAD: `{{expected_head.revision}}` / `{{expected_head.transaction_digest}}`
