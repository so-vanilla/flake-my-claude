---
name: review-task-quality
description: Review a frozen task candidate for standards, integration, operability, and evidence quality.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_execution_group.py::E5_ReviewTaskQualityTests
---

# Review task quality

Use `group.E.E5` as a fresh Reviewer distinct from the Worker and E4 Reviewer.
Require a separate Review package bound to the same frozen candidate and
aggregate receipt, plus the approved practices, design, tests, non-goals,
execution closure, actor/Epoch identity, freshness, and finite review budget.

Inspect only the standards, integration, operability, maintainability,
correctness, security, scope, and test-quality axis. Emit stable candidate IDs
and fingerprints with Background, As-Is, canonical To-Be, Gap, requirement and
evidence refs, and proposed severity/blocking/owner. Keep preferences and
optional improvements visibly distinct from mandatory requirements.

Complete with one physical candidate Finding set and quality review Verdict
attempt, separate from E4. Stop at the package time/round limit or on stale or
incomplete inputs. The Reviewer performs no implementation, disposition,
fix-authority, Finding-close, HEAD, approval, or external action; hand both
terminal review reports to `validate-review-findings` by exact closure join.
