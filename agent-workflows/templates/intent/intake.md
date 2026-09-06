# Intake projection

Canonical source: `ObjectiveSystemV1.compile` artifact for `group.B.B1`.
This is a deterministic display projection, not state or an approval receipt.

## Request record

- Raw request: `{{inputs[0].path}}`
- Raw request digest: `{{inputs[0].digest}}`
- Raw request selector: `{{inputs[0].selector}}`
- Interpretation: `{{candidate.candidate_ref.path}}`
- Interpretation source: `{{candidate.candidate_ref.selector}}`
- Assumption: `{{authority.source_refs[0].path}}`
- Unknown: `{{expected_head.transaction_digest}}`

## Candidate identity

- Candidate status: `{{candidate.status}}`
- Candidate digest: `{{candidate.candidate_ref.digest}}`
- Candidate version: `{{candidate.version}}`
