# /emacs-persp-windows - Perspective一覧 + ウィンドウ構成取得

各perspectiveに切り替えてウィンドウ構成を取得し、一覧で返す。

**重要: このコマンドは読み取り専用。perspective切替は行うが元に戻すため状態変更はない。**

## 処理フロー

以下の手順で `emacsclient -e` を使って情報を取得する。subagentへの委譲は不要。

### 1. 現在のperspective名を保存

```bash
emacsclient -e '(persp-current-name)'
```

返り値を `ORIGINAL_PERSP` として記録する。

### 2. 全perspective名を取得

```bash
emacsclient -e '(persp-names)'
```

### 3. 各perspectiveのウィンドウ構成を取得

各perspective名 `NAME` について以下を実行:

```bash
emacsclient -e '(progn (persp-switch "NAME") (mapcar (lambda (w) (list (buffer-name (window-buffer w)) (window-edges w) (window-dedicated-p w) (window-total-width w) (window-total-height w))) (window-list)))'
```

`NAME` は各perspective名に置換する。

### 4. 元のperspectiveに戻す

```bash
emacsclient -e '(persp-switch "ORIGINAL_PERSP")'
```

`ORIGINAL_PERSP` は手順1で保存した名前に置換する。

### 5. 結果を整形して出力

各perspectiveについて以下の形式で報告:

```
## Perspective: NAME
| Buffer | Edges (L T R B) | Dedicated | Width | Height |
|--------|-----------------|-----------|-------|--------|
| ...    | ...             | ...       | ...   | ...    |
```

現在のperspectiveには `(current)` マークを付ける。

## エラーハンドリング

- `emacsclient -e` が接続失敗した場合: 「Emacsサーバーに接続できません。`emacs --daemon` または `M-x server-start` で起動してください。」と報告
- `persp-names` が未定義の場合: 「perspective.elが有効ではありません。」と報告
