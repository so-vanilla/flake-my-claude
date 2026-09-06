---
name: specify-what-and-why
description: Write a canonical behavior specification without embedding a technical implementation choice.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_planning_system.py::D4_SpecificationTests
---

# Specify what and why

Use `PlanningSystemV1.compile` through the `group.D.D4` selector with physical
objective, outcome, current-system, and applicable constraints references.

Define behavior, user scenarios, capabilities, constraints, non-goals, edge
cases, error behavior, and acceptance in canonical `spec.md`. Keep technical
implementation options outside the specification.

Complete when the document is human-reviewable and contains no ambiguous term,
placeholder, or unowned question. Stop and return any unresolved item to its
owner. Hand the spec to the D-01 bundle and clear boundary; only a
fresh, digest-bound bundle may enter `explore-options`, and a changed spec
version closes the current Epoch rather than authorizing implementation.
