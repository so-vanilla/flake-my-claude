# ウィンドウ管理アドバイザリーガイド

Claudeオーケストレータがワーカー配置のためにEmacsウィンドウを管理する際の指針。
状態を見て柔軟に判断するため、意図的にスクリプト化していない。

## 確認

1. 現在のウィンドウ構成を取得:
```bash
emacsclient -e '(mapcar (lambda (w) (buffer-name (window-buffer w))) (seq-remove (function window-minibuffer-p) (window-list)))'
```

2. perspective.elが有効な場合:
```bash
emacsclient -e '(and (featurep (quote perspective)) (persp-current-name))'
```

3. 想定構成は **sidebar | workspace | claude-code の3ウィンドウ**。
   判定基準:
   - ウィンドウ数が3であること
   - "Side Bar" バッファ、通常バッファ、"claude-code" バッファが各1つ
   - 前回のワーカー分割が残っていないこと

4. 想定と異なる場合はユーザーに現状を報告し、リセットの確認を取る。

## リセット

ユーザーの承認を得てから実行する。拒否された場合は現状のまま続行。

```bash
emacsclient -e '(let ((claude-win (seq-find (lambda (w) (let ((case-fold-search t)) (string-match-p "claude-code" (buffer-name (window-buffer w))))) (window-list))) (sidebar-win (seq-find (lambda (w) (string-match-p "Side Bar" (buffer-name (window-buffer w)))) (window-list)))) (dolist (w (window-list)) (unless (or (eq w claude-win) (eq w sidebar-win) (window-minibuffer-p w)) (delete-window w))) (when claude-win (let ((ws-win (split-window claude-win nil (quote left)))) (set-window-buffer ws-win (car (buffer-list)))) (select-window claude-win)))'
```

## 分割

分割前にワークスペースウィンドウを特定する:
```bash
emacsclient -e '(mapcar (lambda (w) (list (buffer-name (window-buffer w)) (window-dedicated-p w) (window-parameter w (quote window-side)))) (seq-remove (function window-minibuffer-p) (window-list)))'
```

条件: バッファ名が "Side Bar"/"claude-code" を含まず、`window-dedicated-p` が nil、`window-side` が nil。

### WORKER_COUNT=4: 2x2グリッド

```bash
emacsclient -e '(let* ((target-win (seq-find (lambda (w) (and (not (string-match-p "Side Bar" (buffer-name (window-buffer w)))) (not (string-match-p "claude-code" (buffer-name (window-buffer w)))) (not (window-dedicated-p w)) (not (window-parameter w (quote window-side))))) (seq-remove (function window-minibuffer-p) (window-list)))) (claude-win (seq-find (lambda (w) (string-match-p "claude-code" (buffer-name (window-buffer w)))) (window-list)))) (when target-win (select-window target-win) (let* ((top-left target-win) (top-right (split-window top-left nil (quote right))) (bottom-left (progn (select-window top-left) (split-window top-left nil (quote below)))) (bottom-right (progn (select-window top-right) (split-window top-right nil (quote below))))) (set-window-buffer top-left (get-buffer "*eat-claude-worker-1*")) (set-window-buffer top-right (get-buffer "*eat-claude-worker-2*")) (set-window-buffer bottom-left (get-buffer "*eat-claude-worker-3*")) (set-window-buffer bottom-right (get-buffer "*eat-claude-worker-4*")) (when claude-win (select-window claude-win)))))'
```

### WORKER_COUNT=2: 左右分割

```bash
emacsclient -e '(let* ((target-win (seq-find (lambda (w) (and (not (string-match-p "Side Bar" (buffer-name (window-buffer w)))) (not (string-match-p "claude-code" (buffer-name (window-buffer w)))) (not (window-dedicated-p w)) (not (window-parameter w (quote window-side))))) (seq-remove (function window-minibuffer-p) (window-list)))) (claude-win (seq-find (lambda (w) (string-match-p "claude-code" (buffer-name (window-buffer w)))) (window-list)))) (when target-win (select-window target-win) (let* ((left target-win) (right (split-window left nil (quote right)))) (set-window-buffer left (get-buffer "*eat-claude-worker-1*")) (set-window-buffer right (get-buffer "*eat-claude-worker-2*")) (when claude-win (select-window claude-win)))))'
```

### WORKER_COUNT=1: 分割なし

```bash
emacsclient -e '(let* ((target-win (seq-find (lambda (w) (and (not (string-match-p "Side Bar" (buffer-name (window-buffer w)))) (not (string-match-p "claude-code" (buffer-name (window-buffer w)))) (not (window-dedicated-p w)) (not (window-parameter w (quote window-side))))) (seq-remove (function window-minibuffer-p) (window-list)))) (claude-win (seq-find (lambda (w) (string-match-p "claude-code" (buffer-name (window-buffer w)))) (window-list)))) (when target-win (set-window-buffer target-win (get-buffer "*eat-claude-worker-1*")) (when claude-win (select-window claude-win))))'
```

### 分割後の確認

```bash
emacsclient -e '(mapcar (lambda (w) (buffer-name (window-buffer w))) (seq-remove (function window-minibuffer-p) (window-list)))'
```

失敗時（side windowエラー等）: エラーをユーザーに報告する。バッファは作成済みなのでワーカー自体は動作する。ウィンドウ表示は必須ではない。

## 復元

### WORKER_COUNT=1

```bash
emacsclient -e '(let* ((target-win (seq-find (lambda (w) (string-match-p "eat-claude-worker" (buffer-name (window-buffer w)))) (window-list)))) (when target-win (set-window-buffer target-win (car (buffer-list)))))'
```

### WORKER_COUNT=2/4

```bash
emacsclient -e '(let ((claude-win (seq-find (lambda (w) (string-match-p "claude-code" (buffer-name (window-buffer w)))) (window-list))) (sidebar-win (seq-find (lambda (w) (or (string-match-p "Side Bar" (buffer-name (window-buffer w))) (window-parameter w (quote window-side)))) (window-list)))) (let* ((normal-wins (seq-filter (lambda (w) (not (or (eq w claude-win) (eq w sidebar-win) (window-minibuffer-p w)))) (window-list))) (keep-win (car normal-wins)) (delete-wins (cdr normal-wins))) (dolist (w delete-wins) (ignore-errors (delete-window w))) (when keep-win (set-window-buffer keep-win (car (buffer-list))))) (when claude-win (select-window claude-win)))'
```

### 復元後の確認

```bash
emacsclient -e '(mapcar (lambda (w) (buffer-name (window-buffer w))) (seq-remove (function window-minibuffer-p) (window-list)))'
```
