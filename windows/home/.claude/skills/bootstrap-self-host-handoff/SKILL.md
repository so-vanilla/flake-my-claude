---
name: bootstrap-self-host-handoff
description: Adopt a digest-bound latest manual Run snapshot into the migrated control kernel after operational gates pass.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_a7_self_host_handoff.py::A7_SelfHostHandoffTests
---

# Bootstrap A7: self-host handoff

The canonical contract is A7 in
`docs/plans/ai-agent-workflow-step-catalog.md`. Read the current physical Run,
latest Checkpoint, A6R operational evidence, and migration approval. Conversation
history is not an input.

## Steps

1. Verify the copied manual Run and Checkpoint have the same run ID, workflow
   version, `manual_state_revision`, Group, objective version/digest, aliases,
   and Checkpoint digest. Preserve the closed Epoch revision separately.
2. Require exactly G1-G8 with operational `pass` evidence, explicit A7
   migration approval, and an explicit accepted-source identity. Require a
   machine-readable migration/cutover/rollback rehearsal receipt, raw-digest
   bound to its Run, workflow, accepted source, physical old/new targets,
   migrated HEAD, and expected-pointer CAS. An unverified or failed gate is a
   stop result.
3. Build an immutable `a7-handoff-package/v1` in storage separate from its
   source files. Bind every copied file by raw SHA-256 and bind the package to
   the expected kernel HEAD.
4. Adopt the package through `SelfHostHandoff`. Reuse the existing A6 migration
   source, open one fresh A7 Epoch, and publish one handoff artifact. A retry
   after only the Epoch-open transaction completes must finish the same handoff.
5. Read `a7-self-host-status/v1` from a fresh kernel instance. Verify the same
   run ID, objective version, manual revision, Checkpoint, manifest digest,
   current kernel HEAD, next-Group readiness, and rollback point. Derive
   next-Group readiness from durable lifecycle state: it is false before A7
   closure and true only after the clear A7 Context epoch and its Group are
   both closed with a nonempty `next_group`.
6. Rehearse pointer-only cutover and rollback against physical old/new targets
   with expected-pointer CAS before any operational pointer mutation.

## Completion

A7 is complete only when a fresh reader returns one current state, the manual
revision remains explicit rather than being rewritten as a kernel transaction
number, the handoff package and artifact validate, the next Group can begin,
and rollback evidence names physical recoverable targets.

Do not activate a Nix or Home Manager generation. Stop on an identity, digest,
HEAD, G1-G8, source-copy, objective, Checkpoint, cutover, or rollback mismatch.
Preserve the manual Run and the A6 migration source on every stop.
