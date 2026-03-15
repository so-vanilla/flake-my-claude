# /emacs-minor-modes - マイナーモード一覧

指定バッファ（またはカレントバッファ）で有効なマイナーモードを一覧表示する。設定デバッグ時に「このモードが有効になっているか」を確認するのに有用。

**重要: このコマンドは読み取り専用。状態変更は一切行わない。**

## 引数

$ARGUMENTS: バッファ名（省略可）

- 指定時: そのバッファで有効なマイナーモードを一覧
- 省略時: カレントバッファ（selected-windowのバッファ）で有効なマイナーモードを一覧

例:
- `/emacs-minor-modes` — カレントバッファのマイナーモード
- `/emacs-minor-modes main.el` — main.elバッファのマイナーモード
- `/emacs-minor-modes *scratch*` — scratchバッファのマイナーモード

## 処理フロー

### バッファ名指定時

```bash
emacsclient -e '(with-current-buffer "BUFFER" (let ((modes nil)) (dolist (m minor-mode-list) (when (and (boundp m) (symbol-value m)) (push (symbol-name m) modes))) (sort modes (quote string<))))'
```

`BUFFER` はバッファ名に置換する。

### バッファ名省略時

```bash
emacsclient -e '(let ((modes nil) (buf-name (buffer-name))) (dolist (m minor-mode-list) (when (and (boundp m) (symbol-value m)) (push (symbol-name m) modes))) (list buf-name (sort modes (quote string<))))'
```

### 結果を整形して出力

```
## Minor modes in BUFFER (N active)

- mode-name-1
- mode-name-2
- ...
```

## エラーハンドリング

- `emacsclient -e` が接続失敗した場合: 「Emacsサーバーに接続できません。`emacs --daemon` または `M-x server-start` で起動してください。」と報告
- バッファが存在しない場合: 「バッファ `BUFFER` が見つかりません。」と報告
