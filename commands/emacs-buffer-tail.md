# /emacs-buffer-tail - バッファ末尾取得

指定バッファの末尾N行を取得する。長いシェルセッション等でコンテキスト消費を抑えつつ最新の出力を確認するのに有用。

**重要: このコマンドは読み取り専用。状態変更は一切行わない。**

## 引数

$ARGUMENTS: `バッファ名 [行数]`

- バッファ名: 必須。取得対象のバッファ名（例: `*scratch*`, `*eshell*`）
- 行数: 省略時は50。取得する末尾行数

例:
- `/emacs-buffer-tail *scratch*` — scratchバッファ末尾50行
- `/emacs-buffer-tail *eshell* 100` — eshellバッファ末尾100行
- `/emacs-buffer-tail main.el 30` — main.elバッファ末尾30行

## 処理フロー

### 1. 引数をパース

$ARGUMENTS からバッファ名と行数を分離する。行数が省略されていれば50をデフォルトとする。

### 2. バッファ末尾を取得

```bash
emacsclient -e '(with-current-buffer "BUFFER" (save-excursion (goto-char (point-max)) (forward-line (- N)) (buffer-substring-no-properties (point) (point-max))))'
```

`BUFFER` はバッファ名、`N` は行数に置換する。

### 3. 結果を整形して出力

S式で返される文字列（`"..."` 形式）のエスケープを解除し、プレーンテキストとして表示する。

出力形式:
```
## BUFFER (last N lines)

<content>
```

## エラーハンドリング

- `emacsclient -e` が接続失敗した場合: 「Emacsサーバーに接続できません。`emacs --daemon` または `M-x server-start` で起動してください。」と報告
- バッファが存在しない場合: emacsclientがエラーを返すので、「バッファ `BUFFER` が見つかりません。」と報告
- 引数が指定されていない場合: 「バッファ名を指定してください。例: `/emacs-buffer-tail *scratch* 50`」と報告
