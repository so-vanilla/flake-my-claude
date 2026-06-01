# /org-bump - デイリータスクのDEADLINE修正

`~/org/todo.org` のデイリーリピートタスク（`++1d`）で DEADLINE が過去日のものを、今日または明日に修正する。

**確認なしで即実行し、結果を事後報告する。**

## 引数

なし

## 実行方法

Task ツールで以下の通り subagent を起動し、全処理を委譲せよ:
- subagent_type: `"general-purpose"`
- model: `"haiku"`
- prompt: 以下のプロンプト全文。`{TODAY}` は実際の今日の日付（YYYY-MM-DD 曜日）に、`{NOW}` は現在時刻（HH:MM）に置換すること

subagent の出力をそのままユーザーに表示すること。

---

### subagent prompt

```
あなたは org-mode ファイルのデイリータスクを修正するエージェントです。
今日の日付は {TODAY} です。現在時刻は {NOW} です。

以下の手順を順に実行してください。

## 0. フォーマットルールの確認

Read ツールで ~/.claude/commands/org-format-rules.md を読み込み、フォーマットルールを把握する。

## 1. todo.org の読み込み

Read ツールで ~/org/todo.org を読み込む。

## 2. 対象タスクの抽出

以下の全条件を満たすエントリを抽出する:

- ステータスが TODO
- DEADLINE行に ++1d を含む
- DEADLINE の日付が今日より前（過去日）

## 3. 新しい日付の決定

各タスクについて、DEADLINE行の時刻部分と現在時刻を比較する:

- deadline時刻 > 現在時刻 → 今日の日付に設定
- deadline時刻 <= 現在時刻 → 明日の日付に設定
- 時刻指定なし → 今日の日付に設定

曜日は必ず `date -d "YYYY-MM-DD" "+%a"` コマンドで取得すること（推測禁止）。
月・日・時・分はすべて2桁ゼロパディングすること（例: 4月 → `04`、9時 → `09`）。

## 4. DEADLINE行の書き換え

sed コマンドで各タスクの DEADLINE行の日付と曜日を書き換える。
Edit ツールは使わないこと（日付文字列が欠落するため）。
++1d やその他の修飾子はそのまま維持する。

各タスクごとに以下の形式で sed -i を実行する:
```
sed -i 's/DEADLINE: <OLD_DATE OLD_DOW/DEADLINE: <NEW_DATE NEW_DOW/' ~/org/todo.org
```

例: `sed -i 's/DEADLINE: <2025-01-01 Wed/DEADLINE: <2026-04-13 Mon/' ~/org/todo.org`

書き換え後、Emacsのバッファを最新のディスク内容に同期する:
```
emacsclient -e '(when-let ((buf (find-buffer-visiting (expand-file-name "~/org/todo.org")))) (with-current-buffer buf (revert-buffer t t t)))'
```

## 5. 結果報告

以下の情報を報告する:

- 対象タスクの総数
- 各タスクのタイトルと変更内容（旧日付 → 新日付）
- 対象タスクがなかった場合は「更新対象のデイリータスクはありません」と報告

日本語で出力すること。
```
