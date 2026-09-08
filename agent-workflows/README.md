# AI agent Workflow Bootstrap control kernel

## 利用説明書

[AI Agent Workflow 利用説明書（HTML）](../docs/ai-agent-workflow-manual.html)

Skillの用途と呼び出し順、clearを挟む位置、親・Worker・Reviewer・Validatorの
関係、コーディング以外の用途、承認・時間制限・再開方法をまとめています。
実装済みの経路と動作確認済みの範囲も区別しています。
HTMLはローカルのブラウザで開いてください。

## Current Inception operation

Group B/C/D use one explicitly invoked Skill per conversation. Save its
output and handoff, stop, then let the human clear context and invoke the
next Skill. The [current operating decision](../docs/plans/inception-single-skill-operation.md)
supersedes the original group-only clear convention for these groups. The
[distributed protocol](skills/entry/references/inception-single-skill.md)
defines cold resume, blocked steps, group closure, and the handoff to Group E.

The installed absolute
`/absolute/project/.agent-workflow/bin/agent-workflow-inception init/save/resume`
entrypoint provides pre-Run draft persistence
without requiring an invented Kernel HEAD. `init` requires ignored project
storage; `save` appends one immutable handoff; `resume` verifies input digests
and returns the remaining wall-clock budget. It never invokes the next Skill.
For a repository-local Codex setup, install Skills and the matching runtime as
one create-only snapshot, then use its wrapper rather than guessing `PATH` or
a relative `PYTHONPATH`:

```sh
python3 /absolute/source/agent-workflows/scripts/install-project-local-inception.py \
  --project /absolute/project
/absolute/project/.agent-workflow/bin/agent-workflow-inception --help
```

The installer consumes the checked-in
`manifests/project-local-inception-release.json`; it refuses source drift,
unlisted managed inputs, destination symlinks, and conflicting existing files.
The snapshot includes the traced Skill, Python, schema, Workflow, Group,
catalog, and profile-binding inputs. The wrapper verifies the snapshot manifest
and exact managed inventory before every command. A handoff is a usable frontier only
after helper `resume` returns `frontier_status: helper-verified`; a handwritten
JSON file or an agent's success message is not state.
Use `--mode rehearsal` only for an authorized isolated test with mock dialogue.
Draft recording is not compiler/Group acceptance: the normal draft route stops
at B7 pending Kernel approval/closure, and the rehearsal route stops at D12
without granting execution authority. See the protocol above for commands.

The installed absolute
`/absolute/project/.agent-workflow/bin/agent-workflow-inception runtime`
entrypoint is the separate project-local execution
adapter. `adopt --inputs adoption-args.json` validates a supplied physical
human receipt (or an explicitly isolated mock receipt), compiles B7, and
commits an actual Kernel objective approval. Its JSON binds intake, candidate,
proposal, actor and receipt refs, mode and a finite budget; optional
`preapproval_steps` recompiles saved B1–B6 inputs without invoking AI Skills.
Draft storage by itself remains candidate-only.

In each fresh conversation,
`/absolute/project/.agent-workflow/bin/agent-workflow-inception runtime status
--project /absolute/project --run-id work-id` reads the persisted Run. The same
absolute wrapper's `runtime step --qualified-id … --inputs … --actor …`
invokes the current C/D compiler and records its actual result.
At a Group boundary, `close --inputs group-audit.json` requires the actual
alignment audit, runs F1–F7, and commits Epoch/Group closure. After stopping
and clearing, explicitly use `advance` to open the next Group. Paths and IDs
here are user-supplied examples; see the distributed protocol for binding
requirements and full command syntax.

The helper does not launch models or clear contexts. E execution uses existing
Kernel APIs. After inspecting receipts, an identical retry resumes F7 from an
accepted F6 or reuses an accepted F7 report. F1–F5 partial failures still fail
closed. Adapter success alone is not a production benchmark; see
`docs/reports/inception-runtime-trial-2026-09-06.md` for the isolated case and
its limitations. The helper does not activate, rebuild, or publish Git changes.

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
