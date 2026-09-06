# Finding validation

## Joined review inputs

- E4 report digest: `sha256:<...>`
- E5 report digest: `sha256:<...>`
- Review budget: `<remaining time, round, product fix attempts>`

## Classification

Deduplicate each fingerprint and classify it exactly once. Route `required` to
E7, no-required results to E8, and `needs-user` or unresolved results to a
terminal stop. Do not start a fix for a non-required class.
