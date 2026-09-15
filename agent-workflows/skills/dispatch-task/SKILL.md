---
name: dispatch-task
description: Compile one immutable, phase-bounded Task package for a ready Group E attempt.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_execution_group.py::E2_DispatchTaskTests
---

# Dispatch task

Use `group.E.E2` only with a passing E1 receipt, a ready attempt, current HEAD,
and digest-bound dependency artifacts. The DAG Orchestrator validates role,
assignment, lease, authority, expected HEAD, idempotency key, graph version,
loop identity, and reserved iteration event before issuing the package and
dispatch receipt.

Bind one independent Epoch, `artifact | section | workflow` loop level, exact
execution-package closure, input refs, objective contribution, write scope,
non-goals, acceptance, stop conditions, freshness, output namespace, timeout,
grace, supervision, phase iteration counter, and technical retry counter. Use `gpt-5.6-luna` at maximum effort by
default. Keep tightly coupled work in one package for one Worker.

For confidential or case-isolated work on macOS, issue a broker-only task
process release. Bind the selected project, parent-owned broker state, and the
explicit system/runtime prerequisite roots into the sandbox profile and E2
package digest. The real command may be released only by the project-local
`MacOSTaskProcessBroker`; a known-sibling deny-list is not a substitute for its
positive allow-list. Use `unsandboxed-rehearsal` only for non-confidential
contract-shape checks and retain its `os_isolation_enforced=false` limitation.

For a new broker-only dispatch, require the v2 parent-authenticated receipt for
profile `macos-parent-writer-practical/v1`, authorized by
`r5-practical-profile-approval/v1` with approval digest
`sha256:2f0ae19fac7b90778d1b5713c51071f1ab7e96ce179237a1e5892cf320dfaefb`.
The trusted Codex host, parent Orchestrator, and same-UID parent runtime process
remain the writer; the sandboxed task child, malformed or stale caller input,
and path substitution available to that child are adversarial. Reject release
unless every task write root is disjoint from the broker state root: neither
side may equal, contain, or be contained by the other. The sandbox must deny
the task child read and mutation access to broker state.

Carry these accepted residuals verbatim in the v2 profile and receipt:

- `a malicious external process running as the same login UID may rename a project ancestor`
- `SIGKILL of the trusted parent after a publication syscall and before bookkeeping may leave bytes in a detached tree`

The receipt must record `arbitrary_same_uid_atomicity=false` and
`publication_sigkill_atomicity=false`. Descriptor anchoring, no-follow checks,
attachment revalidation, and rollback are required defense in depth; they do
not strengthen either value or establish a separate privileged state writer.

Complete when the immutable package enables exactly one assigned Worker to
return `DONE`, `DONE_WITH_CONCERNS`, `NEEDS_CONTEXT`, or `BLOCKED` at its
physical output path. Refuse dispatch from a stale HEAD, incompatible frontier,
or durable non-dispatch terminal lacking a validated reopen command. The
package conveys only its recorded authority; it implies no external action.
