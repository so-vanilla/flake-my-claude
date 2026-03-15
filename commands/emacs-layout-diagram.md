# /emacs-layout-diagram - ウィンドウレイアウト図示

現在のウィンドウ構成をASCIIアートで図示する。

**重要: このコマンドは読み取り専用。状態変更は一切行わない（`all`モード時のperspective切替は元に戻す）。**

## 引数

$ARGUMENTS:
- 省略: 現在のperspectiveのレイアウトを図示
- `all`: 全perspectiveのレイアウトを順に図示
- `plan DESCRIPTION`: 指定の説明に基づいて計画レイアウトを図示（emacsclientは使わず、テキストベースで作図）

## 処理フロー

### 通常モード（引数省略）

#### 1. フレームサイズとウィンドウ情報を取得

```bash
emacsclient -e '(list (frame-width) (frame-height))'
```

```bash
emacsclient -e '(mapcar (lambda (w) (list (buffer-name (window-buffer w)) (window-edges w) (window-dedicated-p w) (window-total-width w) (window-total-height w))) (window-list))'
```

#### 2. ASCIIアートを描画

`window-edges` の `(left top right bottom)` 座標から空間配置を再構成し、ASCIIアートで描画する。

描画ルール:
- **英語のみ使用**（日本語はフォーマット崩れるため）
- 各ウィンドウにバッファ名、dedicated状態、サイズ（cols x rows）を表示
- バッファ名が長い場合は末尾を `...` で省略
- `+`, `-`, `|` で枠を描画

出力例:
```
Frame: 220 cols x 60 rows

+----------+----------------------------+----------------------------+
| Sidebar  | dired (~/projects)         | *claude-code[...]*         |
| (dedic.) |                            | (dedicated)                |
| 28 cols  | 96 cols                    | 96 cols                    |
| 60 rows  | 60 rows                    | 60 rows                    |
+----------+----------------------------+----------------------------+
```

### `all` モード

1. 現在のperspective名を保存: `emacsclient -e '(persp-current-name)'`
2. 全perspective名を取得: `emacsclient -e '(persp-names)'`
3. 各perspectiveに切替 → ウィンドウ情報取得 → ASCIIアート描画
4. 元のperspectiveに戻す
5. 各perspectiveの図を `## Perspective: NAME` のヘッダ付きで出力

### `plan` モード

emacsclientは使用しない。`DESCRIPTION` の内容をもとに計画レイアウトをASCIIアートで作図する。

## エラーハンドリング

- `emacsclient -e` が接続失敗した場合: 「Emacsサーバーに接続できません。`emacs --daemon` または `M-x server-start` で起動してください。」と報告
