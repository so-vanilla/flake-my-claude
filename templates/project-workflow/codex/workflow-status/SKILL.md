---
name: workflow-status
description: Report the selected AI-DLC or Superpowers workflow position, evidence, exact next entry, and uncertainty without changing the project. Use only when explicitly invoked as $workflow-status.
---

# Workflow Status

Read `./phases.json` first. The initializer installs that sibling from the
single shared template map. Then read `.local/agent/workflow-selection.json`.

Before using either workflow section, validate the selection as
`project-workflow-selection/v1`: it must be a JSON object; `agent` must be
exactly `codex`; `workflow` must be `aidlc` or `superpowers` and present in
`phases.json`; and `upstream.repository`, `upstream.ref`, and
`upstream.commit` must all be non-empty strings. If the file is absent,
malformed, unknown, incomplete, or names another harness, do not run status
commands or infer progress. Report all seven fields with `Workflow`, `Current
position`, and `Next Skill/entry` set to `Unverified`, and put the exact
validation failure in `Evidence` and `Unverified`.

Never edit source, state, Git, or external systems. Never start workers or
advance a workflow. Do not invoke an AI-DLC entry without `--status`.

Report exactly these fields, in this order:

```text
Workflow: <AI-DLC|Superpowers|Unverified>
Harness: Codex
Full flow: <ordered phases from phases.json>
Current position: <confirmed phase/stage, or Inferred/Unverified>
Evidence: <official status, state, artifact, task, branch, or command>
Next Skill/entry: <exact Codex invocation, or Unverified>
Unverified: <missing, conflicting, inferred, or unavailable evidence>
```

## AI-DLC

Use `$aidlc --status` as the authoritative read-only evidence. Its valid
state/status output wins over artifact inference. Use the `aidlc.state_cases`
rules in `phases.json`: report a valid lifecycle phase/current stage; report
the documented no-active-workflow result as unavailable; and report missing,
unknown, or inconsistent required fields as invalid and unverified. Never
repair state or guess from artifacts. Use the mapped `$aidlc --resume` or
`$aidlc` only as the reported next entry; do not invoke it.

## Superpowers

Superpowers has no equivalent deterministic persistent state machine. Inspect
only the mapped design, plan, implementation, review, and completion evidence.
State `Current position` as `Inferred — ...`, and say that artifact presence
does not prove phase completion. Choose the mapped Codex `$...` entry only
when the evidence is unambiguous; otherwise set `Next Skill/entry: Unverified`.
