---
name: work-ledger
description: Create, maintain, validate, and resume a durable repository-local work ledger for medium or large agent tasks. Use when work spans phases, tickets, subagents, decisions, compaction, interruption, or handoff, and whenever root-only state, worker reports, checkpoints, or exact recovery actions must survive lost conversational context.
---

# Work Ledger

Keep semantic state under the repository root in `.local/agent/`. Verify `/.local/` is ignored before private writes. Treat Git ignore as publication protection, not encryption.

## Initialize and own state

- Let only the root orchestrator edit `.local/agent/workplans/<work-id>/ledger.md` and the semantic content it contains.
- Resolve the deployed helper as `~/.claude/hooks/work-ledger-hook` in Claude Code or `~/.codex/hooks/work-ledger-hook` in Codex. Use the bare `work-ledger-hook` name only when it is actually on `PATH`.
- Initialize with that helper:

  ```text
  <helper> init --work-id <id> --next-action <text> --status active
  ```

- Let the helper create the initial template and atomic active pointer. After initialization, let the helper hash and validate semantic state but never ask it or a hook to summarize work.
- Record objective, constraints, approved and excluded scope, route, tickets, dependencies, owners/models, status, decisions, changed files, validation evidence, risks, and one exact next action.

## Dispatch workers

- Checkpoint immediately before dispatch.
- Put an exact `WORK_TICKET_ID=<ticket-id>` line in every subagent prompt so the worker copies the root-assigned identity. Current Claude `SubagentStart` input does not expose the prompt or ticket ID, so automatic stop validation can prove work/agent identity and structure but not always the assigned ticket.
- Give each worker a unique `.local/agent/reports/<work-id>/<agent-id>.md` path. Require frontmatter with `schema: agent-worker-report/v1`, `work_id`, `agent_id`, `ticket_id`, and `status: done|partial|blocked`.
- Require these exact non-empty H1 headings: `Summary`, `Findings or changes`, `Decisions for parent`, `Validation`, `Remaining or blocked`, and `Files changed`.
- Let workers edit only their assigned files and report. Never let a worker edit `ledger.md` or the active pointer.
- Root-only ledger ownership is a workflow invariant, not a same-user filesystem security boundary. The hook detects a changed ledger hash at worker stop, and root diff review remains required; neither mechanism prevents a malicious worker from writing first.
- Validate a returned report before assimilation:

  ```text
  <helper> validate --report <path> --agent-id <id> --ticket-id <id> --json
  ```

- The root must take the expected ticket ID from its ledger and run the explicit validation above even when `SubagentStop` reported success. Read and reconcile the report as root, update the ledger, then checkpoint before releasing dependent tickets.

## Checkpoint semantic boundaries

Update the ledger and run:

```text
<helper> checkpoint --reason <reason> --next-action <text> --status <active|paused|complete|blocked>
```

Checkpoint after requirements or plan approval; before worker dispatch; after every worker assimilation; after each ticket or wave; after a material implementation decision; after validation; before manual compaction; on blocking failure; before root stop; and immediately after resume reconciliation.

Do not checkpoint every tool call. Do not mark `complete` until acceptance criteria and relevant verification pass. Use `blocked` only for a real impasse and state the authority, information, or external change needed.

## Inspect, validate, and resume

- Read current state with `<helper> status --json`.
- Validate the active pointer and ledger with `<helper> validate --json`.
- On resume, trust the last valid file checkpoint over conversation memory. Identify reports newer than the last assimilation, inspect changed files and Git state, reconcile discrepancies, and write the next checkpoint before new work.
- Treat hooks invoked as `work-ledger-hook hook --harness <claude|codex>` or the compatible `work-ledger-hook --harness <claude|codex>` form as mechanical enforcement only. A hook receipt, fresh hash, or valid schema does not prove semantic correctness.
- If there is no Git root, `.local/` is not ignored, state is ambiguous, or validation fails, stop private writes and report the exact repair action. Never guess which ledger is active.
