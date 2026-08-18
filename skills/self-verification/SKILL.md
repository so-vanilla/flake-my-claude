---
name: self-verification
description: Verify completed or proposed work against explicit acceptance criteria and independent evidence. Use before declaring substantive implementation, documentation, configuration, migration, diagnosis, or external-operation work complete, and when reporting what passed, what remains unverified, or what risks still block completion.
---

# Self Verification

1. Restate measurable acceptance criteria from the request, plan, ledger, and repository instructions.
2. Map each criterion to the strongest practical evidence: focused tests, static checks, builds/evaluation, rendered output, reread external state, targeted diff inspection, or reproducible observations.
3. Run proportionate checks. Verify the changed surface first, then broader regressions when risk justifies the cost.
4. Inspect command exit status and relevant output; do not equate command execution, worker confidence, or hook validity with success.
5. Review the actual diff and working-tree state. Preserve unrelated user changes and detect unauthorized files, secrets, generated noise, or missing removals.
6. Reconcile independent reviewer findings. Fix only when implementation authority exists; otherwise report findings without mutation.
7. Update the root ledger with evidence and checkpoint it.

Report four parts: `verified`, `failed`, `unverified`, and `residual risks`. Declare completion only when every required criterion is verified or the user explicitly accepts a named limitation. Include the exact next action for any failure or uncertainty.
