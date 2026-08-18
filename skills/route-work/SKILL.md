---
name: route-work
description: Classify and orchestrate coding-agent work through one durable entry point. Use for normal requests and for explicit check, status, resume, or handoff operations; route questions, research, planning, documentation, diagnosis, implementation, and external operations while preserving authorization boundaries, ticket dependencies, subagent delegation, verification, and recovery state.
---

# Route Work

Use this Skill as the public workflow entry. Keep specialized Skills and harness syntax internal unless a preserved manual-only Skill requires direct user invocation.

## Select the mode

- Treat an ordinary request as `normal`: classify and perform the authorized work.
- Use `/route-work <mode> ...` in Claude Code and `$route-work <mode> ...` in Codex. The canonical dry-run forms are `/route-work check <request>` and `$route-work check <request>`; the text after the Skill name is its argument, not a separate command.
- Treat `check <request>` as a dry route inspection. Report the class, size, ordered phases, candidate Skills, ledger need, tickets, proposed workers, and authorization gates. Do not write files, initialize a ledger, launch workers, switch models, or mutate external state.
- Treat `status` as read-only recovery inspection. Run the deployed ledger helper's `status --json` command when available, read the selected ledger, and report completed, active, blocked, remaining, and exact next action. Do not infer missing state from conversation alone.
- Treat `resume` as continuation from the helper-selected ledger. Validate the pointer and ledger, reconcile unassimilated worker reports, then checkpoint before dispatching new work.
- Treat `handoff` as an explicit redacted emergency export under `.local/agent/handoffs/`. Keep the continuous ledger authoritative; never replace it with the handoff.

## Build the route

1. Read [references/workflows.yaml](references/workflows.yaml).
2. Classify every requested outcome as one or more of: `question-explanation`, `research`, `design-planning`, `documentation`, `diagnosis-review`, `implementation-fix`, or `publish-external-operation`.
3. For mixed requests, order classes by dependency and retain the narrowest authority for each phase. Do not let an implementation clause authorize publication or let a diagnosis clause authorize a fix.
4. Estimate durability. Apply the small-work exception first: keep work ephemeral when it is one bounded answer or edit, needs no worker, carries no durable decision, and can be verified in one continuous session. Otherwise use `$work-ledger` before substantive work when the task has two or more independently recoverable phases, needs delegation, has dependent tickets, risks compaction or interruption, changes several concerns, or contains a durable/contested decision. Default inspect/edit/verify steps inside one bounded action do not by themselves count as several recoverable phases.
5. Select candidate Skills using `route_mode`, relevant gates, and the available repository/tool context. Treat selection as a workflow hint, never as authorization.
6. For an upstream manual-only Skill, either name it as a suggested/direct next Skill or execute the catalog's reviewed equivalent phase contract. State which happened; never claim that a manual-only Skill was invoked when it was not.
7. Ask only for material missing choices that change scope or authority. Continue with safe, reversible, in-scope assumptions otherwise.

## Plan implementation and delegation

For non-trivial implementation, create tickets before edits:

1. Define each ticket's work/ticket IDs, objective, dependencies, owned files or read-only action scope, inputs, acceptance criteria, checks, stop conditions, child-delegation policy, missing-authority behavior, and report path.
2. Separate read-only discovery, implementation with non-overlapping write sets, independent review, and final verification.
3. Build dependency waves. Dispatch every useful ticket on the ready frontier, subject only to real harness concurrency and write-set safety; impose no artificial total-worker cap.
4. In Claude Code, assign bounded exploration, implementation, review, and verification to Sonnet workers. Keep an already-selected Opus orchestrator as root only; never spawn Opus as an ordinary worker.
5. In Codex, keep Sol as root and assign bounded tickets to named Terra workers. If Terra is unavailable, record the fallback and perform the ticket sequentially at root; do not fan out unpinned expensive workers.
6. Give every worker the approved scope, source requirements, completion criteria, read-only main-ledger rule, an exact `WORK_TICKET_ID=<ticket-id>` prompt line, and unique `.local/agent/reports/<work-id>/<agent-id>.md` path.
7. Checkpoint before dispatch. After each completion, validate and read the worker report, assimilate it as root, resolve contradictions, checkpoint, then release the next dependency frontier.

Do not delegate merely to increase agent count. Do not permit concurrent writers to the same files, recursive fan-out without a bounded ticket, or a worker edit to the main ledger.

## Execute and verify

- Run only phases authorized by the request-class policy and the catalog gates.
- Keep branch creation, staging, commits, pushes, deploys, merge/rebase continuation, tracker mutations, deletion, secret handling, and material scope expansion behind their own explicit authority.
- Stop diagnosis-only work after the causal finding. Enter fix phases only when the request includes a fix or the user adds that authority.
- Review uncommitted implementation from the fixed point plus staged and working-tree diffs; do not create a commit merely to make review possible.
- Invoke `$record-decision` when the decision threshold is met and `$self-verification` before declaring substantive work complete.
- Update the durable ledger at every semantic checkpoint required by `$work-ledger`.

Finish with the outcome, validation evidence, unverified items, remaining risks, and exact next action. Never present a selected route, worker message, command success, or hook receipt alone as proof of completion.

Natural-language routing is a standing model instruction and therefore best-effort Skill discovery. The explicit `/route-work` or `$route-work` form is the deterministic user fallback when inspection of the route itself matters.
