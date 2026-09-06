---
name: prepare-worker-briefs
description: Compile each planned task into a narrow fresh-worker brief without exposing the whole plan.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_planning_system.py::D10_WorkerBriefTests
---

# Prepare worker briefs

Use `PlanningSystemV1.compile` through the `group.D.D10` selector with physical
task, spec and design, global-constraints, and execution-DAG references.

For each task, extract only its purpose reference, task, interface, write scope,
checks, stop conditions, and report path into `briefs/<task-id>.md`. Preserve
path, version, digest, authority, and freshness bindings instead of copying the
whole plan.

Complete when a fresh worker can begin without additional exploration, or its
allowed exploration scope is explicit and bounded. Stop on a stale reference,
ambiguous write scope, or missing acceptance check. Hand the briefs and their
references to `plan-verification-and-recovery`; this Skill does not dispatch
them.
