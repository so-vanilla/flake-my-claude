# Codex Nix configuration coexistence rules

この文書は、Claude Code 設定と Codex 設定を Nix / Home Manager で共存管理するためのルールである。
Claude Code から Codex への移行手順ではない。

## 基本方針

- Claude Code 用の `~/.claude/*` と Codex 用の `~/.codex/*` / `~/.agents/*` は別物として管理する。
- Home Manager で配る対象は、原則としてユーザー共通の global 層に限定する。
- repo / project 層の設定は、各リポジトリに commit する。
- Claude Code の `settings.json`、slash command、agent、hook を Codex へ機械コピーしない。
- Codex 設定を追加しても、既存の Claude Code 設定を削除・上書きしない。
- Superpowers のような広い workflow plugin は標準導入しない。
- 必要な workflow は、小さく監査しやすい Codex skill として手書きする。

## 配置先

| 用途 | Codex 配置先 | Nix 管理方針 |
|---|---|---|
| ユーザー共通の指示 | `~/.codex/AGENTS.md` | Home Manager の `home.file` で配る |
| repo 固有の指示 | `<repo>/AGENTS.md` | 対象 repo に commit する |
| repo 固有の一時上書き指示 | `<repo>/AGENTS.override.md` | 原則 commit しない。個人作業用に使う |
| ユーザー共通 config | `~/.codex/config.toml` | Home Manager の `home.file` で配る |
| repo 固有 config | `<repo>/.codex/config.toml` | 対象 repo に commit する |
| ユーザー共通 skill | `~/.agents/skills/<name>/SKILL.md` | Home Manager の `home.file` で配る |
| repo 固有 skill | `<repo>/.agents/skills/<name>/SKILL.md` | 対象 repo に commit する |
| ユーザー共通 custom agent | `~/.codex/agents/*.toml` | Home Manager の `home.file` で配る |
| repo 固有 custom agent | `<repo>/.codex/agents/*.toml` | 対象 repo に commit する |
| command exec rules | `~/.codex/rules/*.rules` / `<repo>/.codex/rules/*.rules` | Bash 系のコマンド制御だけに使う |
| hooks | `config.toml` の hooks 設定と実行 script | Claude hook JSON をコピーせず再設計する |

`CODEX_HOME` を変更する場合は、`~/.codex` をその値に読み替える。

## Claude Code との非互換

### `settings.json`

Claude Code の `settings.json` は Codex の `config.toml` に一括変換しない。

Codex 側へ移せる可能性があるもの:

- model の既定値
- sandbox / approval 方針
- MCP server
- 環境変数
- hooks

Codex 側で別導線または非対応として扱うもの:

- Claude Code の `statusLine`
- Claude Code の `theme`
- Claude Code の `editorMode`
- Claude Code plugin の `enabledPlugins`
- Claude 固有の permission pattern

### permissions / rules

Codex の `.rules` は Claude Code の `permissions.deny` 全体の移行先ではない。
`.rules` は sandbox 外で実行される command exec の制御に使う。

- `Bash(git push --force *)` のような command prefix は `.rules` 候補にできる。
- `Read(**/.env)`、`Edit(~/.ssh/**)`、`Write(~/Documents/**)` のような file read / write 制限は `.rules` へ機械変換しない。
- file read / write 制限は sandbox、permission profile、hooks、managed requirements などで個別設計する。

### commands / skills

Claude Code の `commands/*.md` は、Codex では原則 skill として再構成する。
slash 名や `$ARGUMENTS` の互換性を完全に保つ必要がある個人用の短い prompt だけ、Codex custom prompts を検討する。
repo 共有する手順は skill を優先する。

### workflow plugins

Superpowers のような広い workflow plugin は、Codex で利用可能でも標準導入しない。
常時の判断、参照、手順が増えることで、短い作業では token overhead が増える可能性がある。

- workflow plugin は参照元として扱う。
- 採用したい手順だけを、小さい `SKILL.md` に分解して管理する。
- 導入前後で context / token overhead を測るまで、広い workflow plugin を常用しない。
- `grill-me` のように短い手順は、外部 plugin 依存ではなく手書き skill として配る。

### agents

Claude Code の `agents/*.md` は、Codex の `.codex/agents/*.toml` に変換する。
Markdown frontmatter をそのまま置かない。

Codex custom agent では少なくとも以下を明示する。

- `name`
- `description`
- `developer_instructions`
- 必要なら sandbox / model / tool 方針

Claude の `tools` や model 名は Codex の指定へそのまま対応しない場合がある。

### hooks

Claude Code の hook JSON は Codex へコピーしない。
Codex の hooks 設定として TOML で再記述する。

- project-local hook は trusted project として扱える場合だけ使う。
- hook script は配置先と実行権限を Home Manager 側で明示する。
- repo-local script を呼ぶ場合は、git root 基準でパスを設計する。
- `statusLine` は hook ではないため、Codex で同等表示が必要なら別途設計する。

## Home Manager での配置例

Codex 設定本体を配る場合は、Claude Code の設定とは別の `home.file` entry として追加する。

```nix
home.file = {
  ".codex/AGENTS.md".source = "${self}/codex/AGENTS.md";
  ".codex/config.toml".source = "${self}/codex/config.toml";
  ".agents/skills/grill-me/SKILL.md".source = "${self}/codex/skills/grill-me/SKILL.md";
  ".agents/skills/plan-task/SKILL.md".source = "${self}/codex/skills/plan-task/SKILL.md";
  ".codex/agents/quick-explorer.toml".source = "${self}/codex/agents/quick-explorer.toml";
};
```

このリポジトリで Codex 設定本体を配るか、別リポジトリ `flake-my-codex` に分離するかは別途判断する。
共存を続ける場合も、Claude Code 用ファイルと Codex 用ファイルは配置先と責任範囲を分ける。

## 検証

ルール文書や Home Manager 配布を変更したら、最低限以下を確認する。

```bash
git diff --check
nix flake check --no-build --no-write-lock-file .
nix eval --json --no-write-lock-file .#homeManagerModules.default --apply 'm: builtins.hasAttr ".claude/rules/codex-nix-config.md" ((m {}).home.file)'
nix eval --json --no-write-lock-file .#homeManagerModules.default --apply 'm: builtins.hasAttr ".agents/skills/grill-me/SKILL.md" ((m {}).home.file)'
nix eval --raw --no-write-lock-file .#homeManagerModules.default --apply 'm: toString ((m {}).home.file.".claude/rules/codex-nix-config.md".source)'
nix eval --raw --no-write-lock-file .#homeManagerModules.default --apply 'm: toString ((m {}).home.file.".agents/skills/grill-me/SKILL.md".source)'
rg -n "codex-nix-config|grill-me|Superpowers|\\.codex|\\.agents|home.file" CLAUDE.md flake.nix rules/codex-nix-config.md codex/skills/grill-me/SKILL.md
```

下流の Home Manager 構成に組み込む変更では、実際の下流評価も追加する。
