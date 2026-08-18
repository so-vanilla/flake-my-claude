---
name: record-decision
description: Capture an important human or AI implementation decision at the correct durability level. Use when a choice changes scope, interfaces, architecture, data handling, authority, dependencies, operational behavior, or recovery, especially when context loss would make the rationale or rejected alternatives hard to reconstruct.
---

# Record Decision

Record the decision immediately in the active root-owned work ledger with:

- status: `working`, `candidate-adr`, or `requires-user`;
- context and decision;
- rationale and evidence;
- rejected alternatives and tradeoffs;
- consequences, owner, and revisit trigger.

Use `requires-user` and stop the affected phase when the choice exceeds approved authority. Never treat silence as approval.

Use `candidate-adr` only when the decision is difficult to reverse, surprising without context, or has durable cross-cutting consequences. Keep it private until tracked documentation is authorized; then promote it into the repository's established ADR format without deleting its ledger history.

Skip routine implementation details that are obvious from the diff, cheap to reverse, and irrelevant to recovery.
