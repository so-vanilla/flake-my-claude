---
name: explore-options
description: Compare viable solution options and status quo before an authorized option is selected.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_planning_system.py::D5_OptionExplorationTests
---

# Explore options

Use `PlanningSystemV1.compile` through the `group.D.D5` selector with the fresh
D-01 bundle and physical spec, current-system, and constraints references.

Compare two or three viable options plus status quo across complexity,
reversibility, risk, operating cost, and migration. Record the recommendation
and the evidence or condition that would falsify it in `planning/options.md`.

Complete with an unapproved option comparison or typed refusal. Stop when the
canonical spec is stale or the comparison lacks material evidence. Hand the
options to the authorized decision owner; `design-solution` may start only from
an explicitly approved option, because exploration is not implementation
approval.
