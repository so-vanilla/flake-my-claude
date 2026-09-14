---
name: approve-promotion
description: Record a human G5 disposition for each proposed durable-record promotion.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_decision_system.py::G5_ApprovePromotionTests
---

# Approve promotion

## Inputs

Require digest-bound G4 candidates, exact promotion destinations and diffs,
the expected HEAD, and an explicit receipt from the user or authorized human.

## Method

Bind each candidate to one disposition: adopt, revise, hold, or reject. Record
the human authority, receipt, candidate digest, destination, and reason. Route
revision back to drafting; retain hold and reject without promotion.

## Output and completion

Return an approval event covering every candidate. Complete only when every
disposition and its human authority are explicit. This event records promotion
authority but does not itself write a durable record or change live state.

## Stop and handoff

Implicit, AI-authored, stale, destination-mismatched, or authority-ambiguous
approval stops without promotion. Hand only an adopted, exactly bound
candidate and approval event to `promote-and-backlink`; preserve all other
dispositions for their recorded route.
