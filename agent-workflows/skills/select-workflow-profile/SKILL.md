---
name: select-workflow-profile
description: Select one planning profile and its required or optional Groups from an approved scope and outcome system.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_planning_system.py::D1_ProfileSelectionTests
---

# Select workflow profile

Use `PlanningSystemV1.compile` through the `group.D.D1` selector with physical
scope-classification, outcome-system, and target-domain references.

Choose the applicable feature, bug-fix, improvement, research, decision, or
business-improvement profile and its required and optional Groups. Produce one
versioned Workflow manifest with exactly one lifecycle owner; legacy AI-DLC or
Superpowers workflows are context only, never candidate owners.

Complete by returning the manifest or a typed refusal. Stop when inputs are
stale, the domain is unresolved, or lifecycle ownership conflicts; profile
selection grants no implementation authority. Hand the manifest and its
physical references to `reverse-engineer-current-system`.
