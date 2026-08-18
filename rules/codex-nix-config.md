# Claude Code / Codex Nix coexistence

Claude Code and Codex share semantic workflow Skills and private ledger state, but keep native configuration surfaces separate.

## Ownership

| Purpose | Claude Code | Codex |
| --- | --- | --- |
| Global instructions | `~/.claude/CLAUDE.md` | `~/.codex/AGENTS.md` |
| Shared user Skills | `~/.claude/skills/<name>/` | `~/.agents/skills/<name>/` |
| Custom agents | `~/.claude/agents/*.md` | `~/.codex/agents/*.toml` |
| Hooks | `~/.claude/settings.json` | `~/.codex/hooks.json` |
| Mutable user config | Claude settings above | `~/.codex/config.toml`, kept outside Home Manager |
| Repository state | `.local/agent/` | the same `.local/agent/` |

- Use direct `home.file` entries. For each Skill directory use `recursive = true`; keep parent Skill roots as ordinary directories.
- Do not copy Claude agent Markdown, hook JSON, permission patterns, status-line settings, or model names mechanically into Codex.
- Do not claim `~/.codex/skills/.system`, plugin caches, marketplaces, sessions, databases, or the mutable desktop-generated `config.toml`.
- Never invoke an installer or updater during Home Manager activation. Nix inputs and the reviewed manifest are authoritative.

## Shared workflow

- `route-work` is the common entry. `work-ledger`, `record-decision`, `self-verification`, and `use-repo-local-workspace` are internal shared helpers.
- The AI Hero source is pinned once and its exact 25 plugin-manifest Skill directories are mirrored to both harnesses. Do not recursively install every upstream `SKILL.md`.
- Preserve upstream bytes and the root MIT license. Put cross-harness invocation translation and authorization gates in `route-work`, not silent edits to one mirror.
- `setup-matt-pocock-skills` remains explicit-only and is never run during setup or activation.
- Skill selection is not side-effect authorization. Commit, branch, push, issue/tracker write, deletion, merge/rebase continuation, deploy, and secret operations retain separate gates.

## Models and delegation

- Claude: normal Sonnet root at medium; Sonnet workers at medium, reviewer at high. Opus high is evidence-gated and uses either an explicit root profile/model switch or a clearly labeled read-oriented architect fallback.
- Codex: Sol root remains a mutable user/app choice; named worker TOMLs pin Terra. Do not take over `config.toml` just to set the model.
- Codex explorer/reviewer profiles use `workspace-write` because the sandbox has no report-path-only exception. Their instructions permit only the assigned `.local/agent/reports/` file and forbid source/ledger edits; independent review must verify that boundary.
- This is an instruction-enforced boundary, not filesystem isolation. Root diff review must detect any source, Git-state, or ledger write outside the assigned report.
- Do not make Max, Ultra, xhigh, a 1M context, or prompt/agent hooks workflow defaults.
- Dispatch independent ready tickets in waves without overlapping writer sets. The root alone writes the semantic ledger; workers write task reports.

## Cutover and rollback

- Do not use `force = true` for unmanaged conflicts. Review and remove the exact old regular files immediately before activation.
- Before activation, read `~/.codex/config.toml` and require `model = "gpt-5.6-sol"` plus `model_reasoning_effort = "medium"`. The current live `xhigh` setting is an open cutover blocker until the user changes it through the app or a reviewed direct edit; Home Manager must not change it.
- Removing obsolete `home.file` mappings removes Home Manager-owned symlink leaves. Empty old directories may remain inactive and can be removed later only after exact enumeration.
- After activation, inventory `~/.claude/commands`, `~/.claude/skills/plan-task`, and `~/.claude/skills/self-verification-loop`. Remove only these exact paths, only when empty, and only as the separately reviewed cleanup step; then prove no legacy `SKILL.md` or command entrypoint remains.
- Roll back with the prior Home Manager generation and reviewed backups of migrated regular files. Do not run installer-specific removal commands on Nix-owned destinations.

## Verification

Pair `nix flake check --no-build --no-write-lock-file .` with targeted `nix eval` of the Home Manager target set. Assert:

- exactly 30 shared Skill names in each harness;
- exact 25-name AI Hero registry and complete source directories;
- no old commands, old agents, `plan-task`, `self-verification-loop`, or local `grill-me` mapping;
- native agent models/efforts and command-only hooks;
- product-managed Codex state remains outside Home Manager.
