# Plan file guidelines

Plan file は `/clear` 後でも再開できる内容にする。

## 必須項目

- Background: 経緯、理由、関連する過去判断。
- User request: ユーザー要求の要約。
- SMART goal: 測定可能な目標、受け入れ条件、停止条件。
- Scope: 対象と対象外。
- Constraints: 技術、安全、スタイル、ユーザー指定。
- Current findings: 調査済み情報と根拠。
- Decisions: 方針と却下した代替案。
- Task breakdown: 実行手順、担当、検証。
- Progress: todo / in_progress / done / blocked。
- Files touched: 変更範囲と理由。
- Verification evidence: 実行済み検証と残リスク。
- Resume prompt: 新しいセッションで再開するための指示。

## 保存先

- 通常モードで維持する永続的な plan file は、リポジトリルートの
  `claude-plans/` に保存する。
- plan file は `claude-plans/<task-slug>.md` とする。
- plan file を `docs/` やその他の Git 追跡対象ディレクトリに作成しない。
- `claude-plans/` が存在しない場合は作成してよい。

## 更新タイミング

- 実装前に SMART goal と acceptance criteria を書く。
- 方針判断を変えたら Decisions を更新する。
- 各実装ステップ後に Progress と Files touched を更新する。
- 検証コマンドを実行したら Verification evidence を更新する。
- `/clear` 前には Resume prompt が実行可能か確認する。
