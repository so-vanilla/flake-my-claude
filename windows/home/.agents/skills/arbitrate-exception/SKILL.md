---
name: arbitrate-exception
description: Produce one bounded recommendation for a validated execution conflict or exhausted normal loop.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_execution_group.py::E10_ArbitrateExceptionTests
---

# Arbitrate exception

Use `group.E.E10` only after the normal loop stops on a valid review/validation
conflict, repeated Finding, exhausted finite budget, specification defect,
failed decomposition, or root-cause/whole-design conflict. Require physical
artifact refs, actor provenance, expected HEAD, stop terminal, authority,
non-goals, and one immutable Arbiter package with a bounded attempt.

Use `gpt-5.6-luna` at maximum effort by default. Advise `gpt-5.6-sol` at high
effort only for an explicitly authorized Arbiter attempt after the normal model
is concretely unable to resolve the problem. Classify the cause and return
options, trade-offs, a recommendation, evidence, and any required human choice.

Complete when the physical recommendation selects a next mechanical command,
upstream return, human gate, or finite stop. A budget terminal cannot dispatch
a new attempt: reopening requires a versioned authorization bound to its digest,
expected HEAD, a new lease, and replacement budget. The Arbiter issues no
protected approval, implementation, Finding closure, transition, HEAD update,
or external authority; objective, scope, authority, or material-risk change
stops for the authorized human.
