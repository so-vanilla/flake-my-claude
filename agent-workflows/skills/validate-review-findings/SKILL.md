---
name: validate-review-findings
description: Classify joined review candidates once through an independent, advisory Finding Validator.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_execution_group.py::E6_ValidateReviewFindingsTests
---

# Validate review findings

Use `group.E.E6` with the exact closure join of the E4/E5 terminal reports,
their shared frozen candidate and aggregate receipt, canonical spec/design/
non-goals, approved decisions and risks, prior Finding index, actor provenance,
and current loop identity/history. The fresh Validator must differ from the Worker
and both Reviewers and operate in its own package and Epoch.

Classify every candidate exactly once as `required`, `duplicate`, `invalid`,
`deliberate-design`, `downstream-only`, `too-minor`, `test-evidence-debt`, or
`needs-user`. Merge an unchanged fingerprint into its canonical Finding ID.
Only a mandatory correctness, security, integrity, regression, or acceptance
gap is `required`; record reason, materiality, refs, and proposed fix scope.
A deterministic zero-Finding substitute is allowed only when both required
review axes are complete for the same candidate/package, reviewers are
independent, every mandatory requirement has current passing evidence, and no
unknown, contradiction, unevaluated scope, or open required Finding remains.
It produces a distinct immutable machine-decision receipt without interpretation.

Complete with one physical, advisory-only validation set covering all inputs.
Non-required dispositions do not start a fix; `needs-user` stops its branch.
The Validator implements nothing and owns no fix, counter, transition, closure,
approval, or external authority. Only the DAG Orchestrator may validate this
advice against HEAD, authority, evidence validity, and phase policy before
issuing or refusing the next command.
