# /emacs-window-content - ウィンドウ表示内容取得

指定ウィンドウ（またはバッファ名）の表示範囲のテキストを取得する。

**重要: このコマンドは読み取り専用。状態変更は一切行わない。**

## 引数

$ARGUMENTS: バッファ名（省略可）

- 指定時: そのバッファを表示しているウィンドウの表示範囲を取得
- 省略時: 最大面積のnon-dedicatedウィンドウの表示範囲を取得

例:
- `/emacs-window-content` — メインウィンドウの表示内容
- `/emacs-window-content main.el` — main.elバッファの表示内容
- `/emacs-window-content *eshell*` — eshellの表示内容

## 処理フロー

### バッファ名指定時

```bash
emacsclient -e '(let ((w (get-buffer-window "BUFFER"))) (when w (with-current-buffer "BUFFER" (buffer-substring-no-properties (window-start w) (window-end w t)))))'
```

`BUFFER` はバッファ名に置換する。

### バッファ名省略時

#### 1. ウィンドウ一覧を取得して最大面積のnon-dedicatedウィンドウを特定

```bash
emacsclient -e '(let ((best nil) (best-area 0)) (dolist (w (window-list)) (unless (or (window-dedicated-p w) (string-prefix-p " *Minibuf-" (buffer-name (window-buffer w)))) (let ((area (* (window-total-width w) (window-total-height w)))) (when (> area best-area) (setq best w best-area area))))) (when best (list (buffer-name (window-buffer best)) (with-current-buffer (window-buffer best) (buffer-substring-no-properties (window-start best) (window-end best t))))))'
```

### 結果を整形して出力

S式で返される文字列のエスケープを解除し、プレーンテキストとして表示する。

出力形式:
```
## BUFFER (visible region)

<content>
```

## エラーハンドリング

- `emacsclient -e` が接続失敗した場合: 「Emacsサーバーに接続できません。`emacs --daemon` または `M-x server-start` で起動してください。」と報告
- バッファがどのウィンドウにも表示されていない場合: 「バッファ `BUFFER` は現在どのウィンドウにも表示されていません。」と報告
- 結果がnilの場合: non-dedicatedウィンドウが見つからなかった旨を報告
