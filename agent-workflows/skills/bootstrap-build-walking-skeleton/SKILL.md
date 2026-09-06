---
name: bootstrap-build-walking-skeleton
description: Build or verify the isolated A6 durable Workflow walking skeleton from the approved Artifact Bundle.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_walking_skeleton.py::A6_WalkingSkeletonTests
---

# Bootstrap A6: build walking skeleton

Use this skill only when the Bootstrap Run has closed A5 with
`clear_before_start: true` and the caller explicitly names this Skill.

## Inputs

- The A5 Epoch Artifact Bundle and Checkpoint, referenced by path, version,
  digest, and state revision.
- The approved master plan and step catalog source digests.
- The isolated `agent-workflows/` write scope. Live Codex configuration,
  Home Manager activation, Git staging, and Git history are outside this Skill.

## Steps

1. Read the Run state and verify workflow version, objective version, state
   revision, Artifact Bundle digest, and authority. On any mismatch, stop and
   report stale input; do not reconstruct from conversation text.
2. Implement only the path `entry → artifact → close-epoch → open-epoch →
   close-group → checkpoint → resume/status` with schemas and deterministic
   tests. Keep Workflow, Group, and Skill as the semantic layers; Epoch is
   runtime metadata within a Group.
3. Persist canonical artifacts as path/version/digest references. Keep token
   status `exact`, `estimated`, or `unavailable`; when telemetry is absent,
   retain `unavailable` and do not invent a count.
4. Verify stale revision, workflow version, and bundle digest rejection;
   objective protection; alias/Issue lookup; same-Group Epoch continuation;
   Group clear; bundle completeness; atomic state and append-only events.
5. Write an acceptance receipt containing commands, evidence, unresolved
   items, and the next Skill advice. Close this A6 Epoch with an Artifact
   Bundle and Checkpoint before suggesting A7.

## Completion

The Skill is `completed` only when the fixture tests pass from a fresh Python
process, a new reader can recover the current state from files alone, all
required bundle fields and digests validate, and no live runtime or Git state
was changed. Otherwise record `blocked` or `needs_decision` with the exact
missing evidence.

## Handoff

Return the receipt path, artifact paths/versions/digests, verification result,
unresolved items, input and produced state revisions, and the next Skill
advice. Do not return full tool output as the handoff record.
