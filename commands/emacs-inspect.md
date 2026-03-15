# /emacs-inspect - Emacs状態検査オーケストレータ

全emacs系Skillのロジックを組み合わせて包括的なEmacs状態検査を行うメタSkill。Emacs設定の作業開始時に呼び出し、現在の状態を把握する。

**重要: このコマンドは読み取り専用。perspective切替は行うが元に戻すため状態変更はない。**

**注意: Skill呼び出しの入れ子はできないため、各Skillと同等の `emacsclient -e` 呼び出しを直接行う。**

## 引数

$ARGUMENTS: 検査の焦点（省略可）

- 省略: 全体検査
- `perspective NAME`: 特定perspectiveに絞って検査
- `buffer NAME`: 特定バッファの詳細情報（minor-modes、表示内容含む）

例:
- `/emacs-inspect` — 全体検査
- `/emacs-inspect perspective code` — "code" perspectiveの詳細
- `/emacs-inspect buffer *scratch*` — scratchバッファの詳細

## 処理フロー

### 全体検査モード（引数省略）

以下の情報を順に取得し、統合レポートとして出力する。

#### 1. Perspective一覧 + ウィンドウ構成（/emacs-persp-windows相当）

```bash
emacsclient -e '(persp-current-name)'
emacsclient -e '(persp-names)'
```

各perspectiveに切替してウィンドウ構成を取得:

```bash
emacsclient -e '(progn (persp-switch "NAME") (mapcar (lambda (w) (list (buffer-name (window-buffer w)) (window-edges w) (window-dedicated-p w) (window-total-width w) (window-total-height w))) (window-list)))'
```

最後に元のperspectiveに戻す。

#### 2. フレームジオメトリ（/emacs-window-geometry相当）

```bash
emacsclient -e '(list (frame-pixel-width) (frame-pixel-height) (frame-width) (frame-height))'
```

#### 3. レイアウト図（/emacs-layout-diagram相当）

手順1,2の結果から現在のperspectiveのASCIIアートレイアウト図を生成する。描画は英語のみ。

#### 4. 統合レポート出力

以下の構成で出力:

```
# Emacs State Report

## Frame
- Size: WxH pixels (CxR chars)

## Perspectives (N total, current: NAME)
### NAME1
[ASCII layout diagram]
| Buffer | Edges | Dedicated | Size |
| ...    | ...   | ...       | ...  |

### NAME2
...
```

### `perspective NAME` モード

指定perspectiveのみ検査:
1. 指定perspectiveに切替
2. ウィンドウ構成 + ジオメトリ取得
3. ASCIIレイアウト図生成
4. 元のperspectiveに戻す

### `buffer NAME` モード

指定バッファの詳細情報:

#### 1. バッファ基本情報

```bash
emacsclient -e '(with-current-buffer "NAME" (list (buffer-name) (buffer-file-name) (symbol-name major-mode) (buffer-modified-p) (buffer-size)))'
```

#### 2. マイナーモード一覧（/emacs-minor-modes相当）

```bash
emacsclient -e '(with-current-buffer "NAME" (let ((modes nil)) (dolist (m minor-mode-list) (when (and (boundp m) (symbol-value m)) (push (symbol-name m) modes))) (sort modes (quote string<))))'
```

#### 3. 表示内容（ウィンドウに表示中の場合、/emacs-window-content相当）

```bash
emacsclient -e '(let ((w (get-buffer-window "NAME"))) (when w (with-current-buffer "NAME" (buffer-substring-no-properties (window-start w) (window-end w t)))))'
```

## エラーハンドリング

- `emacsclient -e` が接続失敗した場合: 「Emacsサーバーに接続できません。`emacs --daemon` または `M-x server-start` で起動してください。」と報告
- perspectiveが見つからない場合: 「Perspective `NAME` が見つかりません。利用可能: ...」と報告
- バッファが見つからない場合: 「バッファ `NAME` が見つかりません。」と報告
