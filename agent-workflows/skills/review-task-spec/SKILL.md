---
name: review-task-spec
description: Review a frozen task candidate against its specification, architecture, and safety obligations.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_execution_group.py::E4_ReviewTaskSpecTests
---

# Review task spec

Use `group.E.E4` as a fresh Reviewer distinct from the implementation Worker.
Require a Review package that binds the frozen candidate, aggregate test
receipt, specification, objective/sub-objective, acceptance, non-goals,
execution closure, actor assignment, Epoch, freshness, loop identity, and
current evidence bindings.

Inspect only the spec, architecture, and safety axis. For each candidate Finding,
record a stable ID/fingerprint, Background, As-Is, canonical To-Be, Gap,
requirement and evidence refs, proposed severity/blocking status, and proposed
owner. Keep candidate Findings separate from the terminal review report.

Complete with one physical candidate Finding set and spec review Verdict
attempt whose inputs and provenance are reproducible. Stop at the phase
iteration limit or on stale or incomplete inputs. The Reviewer neither edits
the implementation nor assigns final disposition, fix authority, Finding
closure, HEAD transition, approval, or external authority; send all candidates
to `validate-review-findings` after the independent quality review joins.
