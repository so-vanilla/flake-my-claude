# /emacs-eval - 汎用Elisp評価

任意のElispを `emacsclient -e` で実行し結果を返す。他のemacs系Skillでカバーしきれない調査に使う汎用ツール。

## 引数

$ARGUMENTS: 評価するElisp式（必須）

例:
- `/emacs-eval (emacs-version)` — Emacsバージョンを取得
- `/emacs-eval (face-attribute 'default :height)` — デフォルトフォントサイズを取得
- `/emacs-eval (length (buffer-list))` — バッファ数を取得

## 安全策

実行前に $ARGUMENTS の内容を検査し、以下のパターンを含む場合は **実行せずにユーザーに確認を求める**:

- `kill-buffer`, `kill-emacs`, `kill-process`
- `delete-window`, `delete-frame`, `delete-file`, `delete-directory`
- `setq`, `setq-default`, `setq-local`（変数変更）
- `write-file`, `write-region`, `save-buffer`（ファイル書き込み）
- `shell-command`, `call-process`, `start-process`（外部プロセス実行）
- `eval`, `funcall`, `apply` の引数に上記を含む場合

上記に該当しない読み取り専用の式はそのまま実行してよい。

## 処理フロー

### 1. 安全性チェック

$ARGUMENTS に上記の危険パターンが含まれていないか確認する。含まれている場合:
- 「この式は副作用を持つ可能性があります: `<検出されたパターン>`。実行しますか？」とユーザーに確認
- ユーザーが承認した場合のみ実行

### 2. 実行

```bash
emacsclient -e 'ELISP_EXPRESSION'
```

`ELISP_EXPRESSION` は $ARGUMENTS の内容に置換する。シングルクォートのエスケープに注意。

### 3. 結果を表示

S式で返される結果をそのまま表示する。文字列の場合はエスケープを解除する。

## エラーハンドリング

- `emacsclient -e` が接続失敗した場合: 「Emacsサーバーに接続できません。`emacs --daemon` または `M-x server-start` で起動してください。」と報告
- 引数が指定されていない場合: 「Elisp式を指定してください。例: `/emacs-eval (emacs-version)`」と報告
- Elispの評価エラー: emacsclientのエラー出力をそのまま表示
