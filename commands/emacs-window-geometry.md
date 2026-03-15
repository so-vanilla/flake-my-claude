# /emacs-window-geometry - ウィンドウジオメトリ取得

現在のフレームの全ウィンドウについて、ピクセル・行列数でのサイズを取得する。

**重要: このコマンドは読み取り専用。状態変更は一切行わない。**

## 処理フロー

以下の `emacsclient -e` コマンドを順に実行する。subagentへの委譲は不要。

### 1. フレーム情報を取得

```bash
emacsclient -e '(list (frame-pixel-width) (frame-pixel-height) (frame-width) (frame-height))'
```

### 2. 全ウィンドウの詳細情報を取得

```bash
emacsclient -e '(mapcar (lambda (w) (list (buffer-name (window-buffer w)) (window-edges w) (window-pixel-width w) (window-pixel-height w) (window-total-width w) (window-total-height w) (window-body-width w) (window-body-height w) (window-dedicated-p w))) (window-list))'
```

### 3. 結果を整形して出力

以下の形式で報告:

```
## Frame
- Pixel: WxH
- Columns x Rows: CxR

## Windows
| Buffer | Edges (L T R B) | Pixel (WxH) | Total (CxR) | Body (CxR) | Dedicated |
|--------|-----------------|-------------|-------------|------------|-----------|
| ...    | ...             | ...         | ...         | ...        | ...       |
```

## エラーハンドリング

- `emacsclient -e` が接続失敗した場合: 「Emacsサーバーに接続できません。`emacs --daemon` または `M-x server-start` で起動してください。」と報告
