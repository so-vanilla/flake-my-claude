# Candidate objective projection

Canonical source: `ObjectiveSystemV1.compile` artifact.
This is a deterministic display projection, not state or an approval receipt.

## Candidate

- Status: `{{candidate.status}}`
- Candidate ref: `{{candidate.candidate_ref.path}}`
- Candidate digest: `{{candidate.candidate_ref.digest}}`
- Candidate selector: `{{candidate.candidate_ref.selector}}`
- Version: `{{candidate.version}}`

## Compile context

- Qualified ID: `{{qualified_id}}`
- Input ref: `{{inputs[0].path}}` / `{{inputs[0].digest}}` / `{{inputs[0].selector}}`
- Authority owner: `{{authority.owner_ref.owner_id}}`
- Expected HEAD: `{{expected_head.revision}}` / `{{expected_head.transaction_digest}}`
