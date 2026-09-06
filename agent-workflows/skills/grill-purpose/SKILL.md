---
name: grill-purpose
description: Ask one material purpose question when a human needs to resolve an undiscoverable unknown.
disable-model-invocation: true
acceptance-test: test_objective_system.py::B4_GrillPurposeTests
---

# Grill purpose

Use `ObjectiveSystemV1.continue_inquiry` with the current context, scope, and
prior human answers. Supply the physical input refs and append the resulting
inquiry to its existing record.

Ask exactly one question, and make it the one material unknown that cannot be
discovered from the supplied sources. The completion is the selector
`B4.one-material-question` with that single next question.

If no such question exists, or resolving it would change the purpose without a
human answer, stop with `needs_user_purpose`. Hand the inquiry and its refs to
the human; do not choose or revise the purpose.
