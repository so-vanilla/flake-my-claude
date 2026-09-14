---
name: execute-small-loop
description: Execute one Worker-owned implementation and inspection loop inside an immutable Task package.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_execution_group.py::E3_ExecuteSmallLoopTests
---

# Execute small loop

Use `group.E.E3` as the assigned Worker with one fresh Task package. Revalidate
its package/closure digest, Epoch, objective contribution, inputs, freshness,
write scope, non-goals, acceptance, stop conditions, supervision, and remaining
finite budget before touching the scoped output.

Implement one behavior, hypothesis, or artifact unit; inspect it; and preserve
the result and evidence before the next bounded unit. A retry must record the
changed hypothesis, context, tool, task size, or authorized model attempt and
its retry class. Preserve already successful independent outputs.

When E2 selected the macOS broker-only path, execute through the parent-owned
`MacOSTaskProcessBroker` and submit only its receipt reference. E3 must load and
authenticate that receipt through the same broker, and must bind the E2 package,
execution closure, exact command/process identity, sandbox profile, declared
roots, trusted allow/deny probes, and terminal receipt. Never promote a Worker-
authored observation map to OS-isolation evidence. An unsandboxed observation
remains rehearsal-only and cannot satisfy confidential isolation acceptance.

For new broker-only packages, accept only the parent-authenticated v2 receipt
for profile `macos-parent-writer-practical/v1`, authorized by
`r5-practical-profile-approval/v1` with approval digest
`sha256:2f0ae19fac7b90778d1b5713c51071f1ab7e96ce179237a1e5892cf320dfaefb`.
Verify that the trusted Codex host, parent Orchestrator, and same-UID parent
runtime process are the writer, while the sandboxed task child, malformed or
stale caller input, and path substitution available to that child are treated
as adversarial. Verify task-write/broker-state non-overlap and evidence that the
task child cannot read or mutate broker state. Require these residuals exactly:

- `a malicious external process running as the same login UID may rename a project ancestor`
- `SIGKILL of the trusted parent after a publication syscall and before bookkeeping may leave bytes in a detached tree`

Require `arbitrary_same_uid_atomicity=false` and
`publication_sigkill_atomicity=false`. Descriptor anchoring, no-follow checks,
attachment revalidation, and rollback remain defense in depth. E3 must not
reinterpret them as arbitrary same-UID or publication-window SIGKILL atomicity,
nor claim that a separate privileged state writer exists.

Complete with the scoped change or artifact, result object, test receipt,
changed paths, purpose audit, unresolved items, timing, and versioned handoff at
the package output path. Stop on spec or objective change, write-scope escape,
expired budget, missing authority, or required external critical action. The
Worker submits work and resolution claims only; it never changes HEAD, issues a
Verdict, closes a Finding, or acquires external authority.
