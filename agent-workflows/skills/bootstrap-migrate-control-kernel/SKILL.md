---
name: bootstrap-migrate-control-kernel
description: Rehearse copied A6 sources into the v3 file-backed Artifact DAG Control Kernel under the revision-25 source-only contract.
disable-model-invocation: true
acceptance-test: agent-workflows/tests/test_a6r_evidence.py::A6R_EvidenceTests
---

# Bootstrap A6R: migrate control kernel

## Identity, ownership, and authority

This source-only remediation execution is ticket `a6r-fix-006` with role
`A6R-source-only-remediation-worker`. It is valid only for Run
`2026-09-01-ai-agent-workflow-rebuild`, workflow `manual-bootstrap/v1`, and
state revision `25`. The required checkpoint is
`checkpoints/020-a6r-fix-005-re-review-rejected-fix-006-authorized.md`
(`sha256:d009bfb220103d9dfa159e55f8413be90a5caee4f7380fbbd4947ec757abddd7`),
the bound findings record is
`bootstrap/a6r-fix-005-re-review-findings.md`
(`sha256:676268d50ca66468a6887d6b29779b1e0678a03f231f0aa7add8797da030a29f`),
and the authority is
`artifacts/a6r-fix-006-authorization.yaml`
(`sha256:ad83b7bbcec1a7781bc14ae9e6220ce02e15e0b396de692aa0259f130155f6cc`).
A mismatch is a hard stop before any source write.

The transition owner is the authorized orchestrator `codex-root`; only that
role may advance canonical `HEAD`. The current Group is `bootstrap`. The
objective link is `objectives/v001.md`, version `v001`, digest
`sha256:38b66e58c1a24419159d1e10f4e659cdcc46dbbdfc5cbbae1e321e3325094752`.
The sub-objective contribution is the A6R source-only control-kernel slice
described by `docs/plans/ai-agent-workflow-step-catalog.md` A6R, using the
integrated Artifact DAG design and adoption-verification gates. purpose audit:
the contribution is aligned to the A6R source-only acceptance contract.
Purpose is
`aligned`: preserve the frozen A6 boundary while proving a separately
reviewable v3 source/schema/test implementation. This Skill never claims
operational adoption, A6R acceptance/closure, A7 readiness, real-Run
migration/cutover, activation, or Git publication.

## Input paths, schemas, and provenance

The converter may consume only a fresh, copied, physically attested A6 source
set. The small JSON files under `agent-workflows/tests/fixtures/a6r/` are
legacy compatibility samples and negative-test inputs; they are intentionally
not a complete revision-25 migration source and must never be described as
complete. Hermetic tests create the complete copied actual-A6 set in a
`TemporaryDirectory`, then pass the three concrete paths to the converter:

| required copied source | required identity and provenance |
| --- | --- |
| Run | Run ID, workflow/version, Group ID/status, Epoch ID/status/clear boundary and closure revision, aliases, objective, source revision, raw bytes/digest |
| Bundle | Bundle ID/version, same Run/Group/Epoch, closed status and closure revision, aliases, state refs, source revision, raw bytes/digest |
| Worker report | report/work ID, agent/ticket creator, same Run/Group/Epoch, status/revision, aliases, provenance, raw bytes/digest |

Every copied source is read afresh at validation and publication. A complete
input set must carry all of the above fields, the expected `source_revision`
and the exact `source_digests` mapping; an incomplete fixture, mapping-only
caller, missing field, or changed raw source is rejected before any durable
write. The static compatibility samples below are retained only to exercise
old-reader and rejection behavior:

| input | schema / provenance | raw SHA-256 |
| --- | --- | --- |
| `legacy-run.json` | legacy Run shape; source revision 11; creator/owner `codex-root` source-freeze | `sha256:aac00f165c4a9a3edf0c7701903d006e124b005b2805598f347dc0a76a2aefa9` |
| `legacy-bundle.json` | `artifact-bundle/v1`; source revision 11; creator/owner `codex-root` source-freeze | `sha256:f9c27c43e2b611110c15a81fa528bb792b06370683b69592779d1f07e8f75f2d` |
| `legacy-worker-report.json` | `agent-worker-report/v1`; source revision 11; creator/owner `codex-root` source-freeze | `sha256:cb5d7471fa203139769f46ec078a05f58d40c2023d800bc15abb0e11f8beab5a` |

The frozen A6 reference is
`.local/agent/runs/2026-09-01-ai-agent-workflow-rebuild/artifacts/epoch-0002-a6-bundle.yaml`,
file digest `sha256:3abe4d04ab81e196900f5f953074d0a404a296dc11c898309ea08825c551600b`
and canonical digest `sha256:b436a52cf640aef19dc1508d0fac5f65646d16b9a996f71986f0d02e5e1dab84`.
Its checkpoint is
`checkpoints/006-bootstrap-a6-epoch-closed.md`, file digest
`sha256:65dd016c3aada2b703f2e83b6bc9e9eb7d64dac2c535449490d874e901ca3f2e`.
These real Run paths are read-only references, never converter inputs.

Every complete input record carries path, schema, raw digest, source revision
`11`, Run ID `legacy-a6`, Group `bootstrap`, Epoch `epoch-0002`, aliases,
creator, revision, and freshness boundary. A handoff also records the Run,
objective, and sub-objective contribution, every input path/schema/digest/
creator/revision/freshness, the Epoch, Bundle, and Task package refs, a
purpose/audit reason, versioned next/model advice, and failure/recovery
instructions. The expected raw source digest is authoritative;
uncertain or diverged identity, status, alias, revision, or clear-boundary
evidence is a hard stop. The converter re-reads all three copied files before
candidate creation and immediately before publication; it records an
immutable snapshot and a physical `source_attestation` containing path,
raw-byte digest, and filtered-value digest. A mapping-only caller, missing
source, identity/status/alias/revision mismatch, or changed source is
rejected. The destination is a fresh isolated kernel and the real Run is
never migrated.

The v3 implementation consumes these versioned schemas and bindings:
`dag-command/v1`, `dag-state/v1`, `dag-edge-v1`, `dag-transaction/v1`,
`dag-head/v1`, `task-package-v1`, `review-package-v1`, `finding-v1`,
`artifact-bundle/v1`, `legacy-migration-v1`, `cutover-pointer-v1`, and
`projection-manifest-v1`. Input references are immutable digest references;
relative paths reject absolute, drive-letter, backslash, empty, dot, dot-dot,
and wildcard traversal forms. Unknown fields are rejected at every nested
schema/runtime boundary.

## Runtime and persistence contract

- Objects are immutable and content addressed; transactions are immutable and
  parent linked. `HEAD` is the only canonical mutable pointer. Projections are
  deterministic views rebuilt from the lock-held canonical HEAD/object state.
- Validate the complete `dag-command/v1` envelope before staging. Required
  fields are never synthesized. Reject unknown nested properties, malformed
  graph versions, expected-head transaction digests, role/assignment,
  authority, scope, duplicate, parent, and digest mismatches before inbox,
  object, transaction, HEAD, or projection persistence.
- Enforce exact source/target/direction for `requires`, `produces`,
  `authorizes`, `verdict-for`, and `converges` in schema, compiler, reducer,
  readiness, and recovery. Task-to-Task edges are forbidden; dependencies use
  produced Artifact/work-product references. Review/fix loops add fresh
  attempts without cycles.
- Before readiness or claim, validate the live authority object, assignment,
  expiry, operation and filesystem scope, proposal digest, expected HEAD,
  freshness, and lease conflict. `leased`, `result_submitted`,
  `needs_decision`, `replan`, and `invalidate` are explicit states; unsupported
  transitions stop with a typed error instead of silently routing.
- A Finding uses only `open`, `resolved`, `unresolved`, `invalid`, or
  `superseded`. Closure requires a registered independent Review, the same
  candidate Task package, Finding, reviewer, fresh Epoch, resolution object,
  exact evidence references, and a fresh passing closure package. A worker
  resolution claim cannot close a Finding. Closed Run/Group/Epoch boundaries
  cannot be mutated, and a Group close cannot be reopened by Epoch operations.
- Apply the normalized conservative durable filter before every persistence
  path, including inbox/submission, opaque text, nested mappings, lists, and
  tuples. Reject authorization/auth/access/private-key/cookie/session/refresh/
  bearer/password/credential/token/private-reasoning/context/model-thought/
  transcript/raw/tool-output material, including compound and suffix variants.
  The only token-named values allowed are the schema-owned `context_budget`
  telemetry fields `token_status` and `token_count`, after exact structural
  validation; arbitrary opaque or nested telemetry is rejected. Token status
  remains only `exact`, `estimated`, or `unavailable`.

## Migration, cutover, and rollback

Run/Bundle/report IDs, Group/Epoch identity and status, aliases, revision,
existence, and raw digests are all bound. The converter preserves the copied
source object and creates immutable Bundle/Checkpoint evidence at a closed
Epoch boundary. Initial pointer cutover validates the new physical target and
any fully described old physical target (reader, status, HEAD, projection,
Run/version/revision, Group/Epoch, aliases, source binding) before pointer
mutation. A legacy revision/digest-only old pointer can be retained as opaque
history for compatibility but is not eligible for rollback until a physical
target exists.

Pointer-only cutover requires the complete authority tuple, proposal/approval,
expected-old CAS, target HEAD/schema/role, and source binding. NewReader reads
the active pointer and target under a coherent pointer/Run lock boundary and
rechecks the pointer after resume. Rollback validates the physical old target
using its own pre-cutover source digests before changing the pointer; missing,
corrupt, stale, or mismatched old state leaves the active pointer unchanged.
The old state/source digest and new candidate remain recoverable.

## Verification and fault evidence

Run focused adversarial tests for every reproduced bypass: wrong edge endpoint
or direction, Task-to-Task edge, live authority expiry/scope/proposal/HEAD,
future freshness, wildcard overlap in both directions, bare auth variants,
absolute output/reference paths, unbound migration mapping, source mutation
at publication, deterministic projection forgery, reverse-bound Finding
verdicts, and physical old-target corruption. Inject faults before transaction
publish, after transaction publish/before HEAD, and after HEAD/before
projection; verify quarantine/orphan behavior, rebuild, duplicate refusal,
corruption blocking, and no silent advance. Run fresh-process resume and
schema/fixture parsing.

Record an explicit G1-G8 matrix with exact command, exit status, and evidence
path for every gate:

| Gate | Required local evidence | adoption status |
| --- | --- | --- |
| G1 | revision-25 authority tuple and source-only scope | local pass; adoption unverified |
| G2 | only orchestrator advances HEAD; worker/reviewer use inbox | local pass; adoption-grade unverified |
| G3 | fresh-process resume/status from canonical HEAD | local pass |
| G4 | independent reviewer and fresh passing closure package | local pass; adoption-grade unverified |
| G5 | durable filter at all persistence paths | local pass; adoption-grade unverified |
| G6 | one-off/managed routing plus cutover/rollback CAS | local pass |
| G7 | exact/estimated/unavailable at 200K/300K/500K | local pass |
| G8 | injected filesystem fault/recovery checks | local pass; physical power-loss unverified |

Local source/fixture tests are not operational adoption evidence. Preserve the
G2, G4, and G5 adoption-grade `unverified` status and G8 physical power-loss
`unverified` status unless separately authorized operational evidence exists.
The historical v2 prototype remains unreproducible; only versioned v3
evidence may be used.

## Versioned next, model, failure, and recovery contract

The versioned next Skill advice is
`bootstrap-migrate-control-kernel/v1` until a fresh independent decision
accepts A6R. Normal model advice is `gpt-5.6-luna` with maximum effort.
`gpt-5.6-sol` with high effort is an alternative only for an explicitly
authorized Arbiter attempt after the normal path fails; it is not a default
switch. No next Skill may infer A7, activation, real-Run migration approval,
or Git authority from this source-only result.

On failure, preserve staging/quarantine and evidence, do not retry an
unchanged condition, and do not weaken a gate. Repair the envelope, source
attestation/snapshot, projection, lifecycle state, or CAS expectation before
retrying. A corrupt canonical chain enters `INTEGRITY_BLOCKED` and never
auto-repairs history. A failed physical check is `unverified` and stops at
A6R. A blocked execution writes the permitted worker report with its concrete
stop reason, last completed step, modified artifacts, and exact recovery
action.

Historical revision 19, revision 20, and revision `21` records remain
provenance only; revision `22` and revision `24` are review baselines. They do
not override this revision-25 contract.
