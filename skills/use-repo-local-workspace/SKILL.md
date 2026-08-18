---
name: use-repo-local-workspace
description: リポジトリ内にGit公開しない一時ファイル、work ledger、ローカルな決定記録、handoff、worker reportを作るとき、repo rootの.local/を安全に使う。雑な作業ファイルや再開用状態を保存するとき、または公開物とローカル作業物の配置を判断するときに使う。
---

# Repository-local workspace

## 配置を決める

- Git管理して共有する成果物、正式なドキュメント、採用済みADRは、リポジトリの通常の配置へ置く。
- 公開しないscratch、work ledger、ローカルな決定記録、handoff、worker reportは、repo rootの`.local/`へ置く。
- repo rootは`git rev-parse --show-toplevel`で解決する。ホームディレクトリの`~/.local/`と混同しない。

推奨配置:

```text
.local/
  scratch/
  agent/
    workplans/
    decisions/
    handoffs/
    reports/
```

## 書き込み前に確認する

1. `git ls-files '.local/**'`で、repo rootの`.local/`が既に追跡対象でないことを確認する。
2. 実際に書く候補パスを`git check-ignore -v --no-index <path>`で確認する。
3. 無視されていない場合は、非公開だと仮定して書き込まない。既存規約を調査するかユーザーへ報告する。
4. credentials、token、秘密鍵などのsecretは、無視されていても`.local/`へ保存しない。

## 運用する

- 中〜大規模作業では`work-ledger`を使い、`.local/agent/workplans/<work-id>/ledger.md`と`.local/agent/workplans/active.json`を継続管理する。別形式のplanを並立させない。
- subagentは`.local/agent/reports/<work-id>/<agent-id>.md`へ担当単位の報告を書く。共有work ledgerとactive pointerはroot agentだけが更新する。
- 長期共有すべき決定は、承認後に追跡対象のADRやproject documentationへ昇格する。
- `.local/`はGit除外と整理の規約であり、暗号化やbackupではない。
- `git reset`では無視ファイルを削除できない。cleanup時は対象を正確に確認してから明示的に削除またはtrashへ移す。
