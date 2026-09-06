# AI agent Workflow Bootstrap control kernel

`src/ai_agent_workflow` contains the standard-library-only A7 control kernel,
its cutover reader, and the retained A6 implementation used as a legacy
migration source. It does not install, activate, or modify live Codex state.

This source covers the `bootstrap-control-kernel` release scope, not the whole
AI agent Workflow.  Do not copy a current count or completion label from this
README. The digest-bound classifier is the only current source for those
values:

```sh
agent-workflow-implementation-status
```

It derives release and whole-Workflow status from the canonical plan/catalog,
typed coverage evidence, mandatory surfaces, and the fixed completion-gate
registry. It fails closed if the manifest tries to redefine those requirements.

Each implemented contract has a dedicated acceptance receipt binding its Skill
or Workflow profile to a concrete test selector and both content digests. A
different contract's test or a bare identifier cannot inflate coverage.

The retained A6 helper persists a Run under `.local/agent/` and exposes:

`entry → produce_artifact → close_epoch → open_epoch → close_group → resume/status`

Every mutation uses a state revision compare-and-swap, writes an append-only
event, and records path/version/digest references. Epoch and Group exits write
an Artifact Bundle and a Checkpoint. Objective changes are rejected unless a
separate approved version/migration is implemented by a later workflow.

Run the fixture contract tests from this directory's repository root with:

```sh
python3 -m unittest discover -s agent-workflows/tests -v
```

The distributed default CLI is read-only and requires the active A7 cutover
pointer explicitly. The Nix wrapper binds the implementation manifest and its
source root. Both commands validate that the pointer selects `new`, that its
recorded HEAD still matches the physical control kernel, and that the
implementation coverage still matches the digest-bound canonical plans:

```sh
agent-workflow status --pointer /absolute/path/to/active-pointer.json
agent-workflow resume --pointer /absolute/path/to/active-pointer.json
```

Direct source invocation (without the Nix wrapper) must also identify those
two completion inputs explicitly:

```sh
PYTHONPATH=agent-workflows/src python3 -m ai_agent_workflow status \
  --pointer /absolute/path/to/active-pointer.json \
  --implementation-manifest agent-workflows/manifests/implementation-status.json \
  --source-root /absolute/path/to/this-repository
```

The output preserves the kernel status fields and adds an `implementation`
object. A missing/stale manifest, plan digest mismatch, distribution drift, or
unsupported full-workflow claim fails closed instead of returning an unscoped
status. Activation remains a separate explicit user approval even after source
readiness is eventually achieved.

The former A6 mutation CLI remains available only as the explicitly addressed
`python3 -m ai_agent_workflow.cli` legacy/migration source. It is not the
distributed default entry point.
