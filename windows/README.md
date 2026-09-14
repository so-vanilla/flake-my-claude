# Windows 用コピー

`home/` は Windows の user home へそのままコピーする静的 payload である。
installer や Nix / Home Manager は使わない。

```text
windows/home/
├── .agents/skills/
├── .claude/skills/
├── .codex/skills/
└── .local/share/agent-workflows/
```

格納内容は repository root の `agent-workflows/manifests/distribution.json` が列挙する
Workflow Skills と portable manifests のコピーである。`.codex/config.toml`、認証情報、
sessions、plugins、state は含めない。

`home/` の中身を `%USERPROFILE%` へコピーする。既に同名の Skill directory がある場合は
内容を確認してから置換すること。provider root にある、それ以外の user-owned directory は
削除しない。

この snapshot は Skills と manifests の配布用であり、POSIX/macOS 固有の処理を含む
file-backed runtime や Nix package の CLI は含めない。
