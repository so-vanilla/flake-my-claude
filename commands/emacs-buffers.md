# /emacs-buffers - バッファ一覧取得

全バッファ（またはフィルタ付き）の一覧を取得する。major-mode、変更状態、ファイルパスを含む。

**重要: このコマンドは読み取り専用。状態変更は一切行わない。**

## 引数

$ARGUMENTS: フィルタ条件（省略可）

- 省略時: 内部バッファ（スペースで始まる名前）を除く全バッファを一覧
- mode名指定: そのmajor-modeのバッファのみ表示（例: `org-mode`, `emacs-lisp-mode`）
- パターン指定: バッファ名にマッチするものを表示（部分一致）

例:
- `/emacs-buffers` — 全バッファ一覧
- `/emacs-buffers org-mode` — org-modeバッファのみ
- `/emacs-buffers .el` — 名前に `.el` を含むバッファ

## 処理フロー

### 1. バッファ情報を取得

```bash
emacsclient -e '(mapcar (lambda (b) (list (buffer-name b) (buffer-file-name b) (with-current-buffer b (symbol-name major-mode)) (with-current-buffer b (buffer-modified-p)))) (buffer-list))'
```

### 2. フィルタを適用

取得結果に対して:

- 引数なし: バッファ名がスペースで始まるもの（内部バッファ）を除外
- mode名指定（`-mode` で終わる引数）: major-modeが一致するもののみ残す
- それ以外: バッファ名に引数文字列を含むもののみ残す

### 3. 結果を整形して出力

```
## Buffers (N total)

| Buffer | File | Mode | Modified |
|--------|------|------|----------|
| ...    | ...  | ...  | ...      |
```

ファイルパスが長い場合は `~/` への短縮やディレクトリ省略で見やすくする。

## エラーハンドリング

- `emacsclient -e` が接続失敗した場合: 「Emacsサーバーに接続できません。`emacs --daemon` または `M-x server-start` で起動してください。」と報告
