You are the orchestrator (主) of a multi-agent team using Emacs eat terminals.
Execute the following workflow to accomplish the user's task: $ARGUMENTS

## Step 0: エージェント数選択

タスクの複雑度を評価し、適切なエージェント数を決定する。

判断基準:
- **1**: 小規模・明確なタスク。単一視点で十分な場合
- **2**: 中規模。技術面とプロダクト面の2軸で分析が有効な場合
- **4**: 大規模・多角的分析が必要。複数の独立した視点が価値を生む場合

ユーザーに推奨値を提示し、確認を得てから `$WORKER_COUNT` を設定する（1, 2, 4 のいずれか）。

## Phase 1: Planning (多視点分析)

### Step 0.5: ウィンドウ状態確認
ワーカー配置の前に、ウィンドウレイアウトを確認する。

1. 現在のウィンドウ構成を取得:
```bash
emacsclient -e '(mapcar (lambda (w) (buffer-name (window-buffer w))) (seq-remove (function window-minibuffer-p) (window-list)))'
```

2. perspective.elが有効な場合、claude-codeバッファがあるperspectiveを確認:
```bash
emacsclient -e '(and (featurep (quote perspective)) (persp-current-name))'
```

3. 想定構成は **sidebar | workspace | claude-code の3ウィンドウ** である。
   結果が想定と異なる場合（ウィンドウ数が3でない、前回のワーカー分割が残っている等）:
   - 現在の構成をユーザーに報告する
   - 「ウィンドウレイアウトが想定と異なります。sidebar | workspace | claude-code にリセットしてよいですか？」と確認する
   - ユーザーが承認した場合、以下を実行してリセット:
     ```bash
     emacsclient -e '(let ((claude-win (seq-find (lambda (w) (let ((case-fold-search t)) (string-match-p "claude-code" (buffer-name (window-buffer w))))) (window-list))) (sidebar-win (seq-find (lambda (w) (string-match-p "Side Bar" (buffer-name (window-buffer w)))) (window-list)))) (dolist (w (window-list)) (unless (or (eq w claude-win) (eq w sidebar-win) (window-minibuffer-p w)) (delete-window w))) (when claude-win (let ((ws-win (split-window claude-win nil (quote left)))) (set-window-buffer ws-win (car (buffer-list)))) (select-window claude-win)))'
     ```
   - ユーザーが拒否した場合、現状のまま続行する

4. 想定通りならそのまま次のステップへ進む。

### Step 1: チーム初期化
```bash
TEAM_ID=$(date +%s)
~/.claude/team/init-team.sh "$TEAM_ID" plan "$WORKER_COUNT"
```

### Step 1.5: ウィンドウ分割
init-team.shがワーカーバッファを作成したので、ワーカー数に応じてウィンドウを分割する。

1. 現在のウィンドウ構成を詳細に確認する:
```bash
emacsclient -e '(mapcar (lambda (w) (list (buffer-name (window-buffer w)) (window-dedicated-p w) (window-parameter w (quote window-side)))) (seq-remove (function window-minibuffer-p) (window-list)))'
```

2. 上記の結果から「分割可能なワークスペースウィンドウ」を特定する。条件:
   - バッファ名が "Side Bar" を含まない
   - バッファ名が "claude-code" を含まない
   - `window-dedicated-p` が nil
   - `window-side` パラメータが nil（side windowでない）

3. ワーカー数に応じて分割:

**WORKER_COUNT=4 の場合**: 2x2グリッド
```bash
emacsclient -e '(let* ((target-win (seq-find (lambda (w) (and (not (string-match-p "Side Bar" (buffer-name (window-buffer w)))) (not (string-match-p "claude-code" (buffer-name (window-buffer w)))) (not (window-dedicated-p w)) (not (window-parameter w (quote window-side))))) (seq-remove (function window-minibuffer-p) (window-list)))) (claude-win (seq-find (lambda (w) (string-match-p "claude-code" (buffer-name (window-buffer w)))) (window-list)))) (when target-win (select-window target-win) (let* ((top-left target-win) (top-right (split-window top-left nil (quote right))) (bottom-left (progn (select-window top-left) (split-window top-left nil (quote below)))) (bottom-right (progn (select-window top-right) (split-window top-right nil (quote below))))) (set-window-buffer top-left (get-buffer "*eat-claude-worker-1*")) (set-window-buffer top-right (get-buffer "*eat-claude-worker-2*")) (set-window-buffer bottom-left (get-buffer "*eat-claude-worker-3*")) (set-window-buffer bottom-right (get-buffer "*eat-claude-worker-4*")) (when claude-win (select-window claude-win)))))'
```

**WORKER_COUNT=2 の場合**: 左右分割
```bash
emacsclient -e '(let* ((target-win (seq-find (lambda (w) (and (not (string-match-p "Side Bar" (buffer-name (window-buffer w)))) (not (string-match-p "claude-code" (buffer-name (window-buffer w)))) (not (window-dedicated-p w)) (not (window-parameter w (quote window-side))))) (seq-remove (function window-minibuffer-p) (window-list)))) (claude-win (seq-find (lambda (w) (string-match-p "claude-code" (buffer-name (window-buffer w)))) (window-list)))) (when target-win (select-window target-win) (let* ((left target-win) (right (split-window left nil (quote right)))) (set-window-buffer left (get-buffer "*eat-claude-worker-1*")) (set-window-buffer right (get-buffer "*eat-claude-worker-2*")) (when claude-win (select-window claude-win)))))'
```

**WORKER_COUNT=1 の場合**: 分割なし
```bash
emacsclient -e '(let* ((target-win (seq-find (lambda (w) (and (not (string-match-p "Side Bar" (buffer-name (window-buffer w)))) (not (string-match-p "claude-code" (buffer-name (window-buffer w)))) (not (window-dedicated-p w)) (not (window-parameter w (quote window-side))))) (seq-remove (function window-minibuffer-p) (window-list)))) (claude-win (seq-find (lambda (w) (string-match-p "claude-code" (buffer-name (window-buffer w)))) (window-list)))) (when target-win (set-window-buffer target-win (get-buffer "*eat-claude-worker-1*")) (when claude-win (select-window claude-win))))'
```

4. 分割後にウィンドウ構成を再確認する:
```bash
emacsclient -e '(mapcar (lambda (w) (buffer-name (window-buffer w))) (seq-remove (function window-minibuffer-p) (window-list)))'
```

5. 分割が失敗した場合（side windowエラー等）:
   - エラーメッセージをユーザーに報告する
   - バッファは作成済みなのでワーカー自体は動作する。ウィンドウ表示は必須ではない

### Step 2: ワーカー起動待ち
```bash
sleep 15
```

### Step 2.5: プロジェクト状態確認
ワーカーの初期探索を省略させるため、プロジェクトの状態を事前に把握する:
```bash
ls -la
git log --oneline -5 2>/dev/null || true
```

結果を `PROJECT_CONTEXT` として要約する。

### Step 3: Round 1 - タスク仕様書き出し + 独立分析

タスク仕様をファイルに書き出し、各ワーカーに短いメッセージで分析を指示する。

1. タスク仕様をファイルに書き出す:
```bash
cat > /tmp/claude-team/$TEAM_ID/task-spec.md << 'SPEC_EOF'
# タスク仕様

## プロジェクト状態
[PROJECT_CONTEXTの内容]

## タスク
$ARGUMENTS

## 分析指示
あなたの役割の視点でこのタスクを分析し、result.mdに書き出してください。
プロジェクト状態は上記の通りなので、探索は不要です。
SPEC_EOF
```

2. 各ワーカーにメッセージ送信（**1行で記述、改行禁止**）:
```bash
~/.claude/team/send-message.sh 1 "/tmp/claude-team/$TEAM_ID/task-spec.md を読み、あなたの役割の視点で分析してください。result.mdに書き出してdoneファイルを作成してください。"
```
（WORKER_COUNT分繰り返す: Worker 2, 3, 4 も同様）

3. 完了待機:
```bash
~/.claude/team/wait-workers.sh "$TEAM_ID" "$WORKER_COUNT" 300
```

### Step 4: Round 2 - 相互参照 + メッセージング

**WORKER_COUNT=1 の場合はこのステップをスキップし、Step 5.5へ進む。**

各ワーカーのresult.mdを軽く確認する:
```bash
for i in $(seq 1 $WORKER_COUNT); do head -20 /tmp/claude-team/$TEAM_ID/worker-$i/result.md; done
```

1. doneファイルをリセット:
```bash
rm /tmp/claude-team/$TEAM_ID/worker-*/done
```

2. 各ワーカーに改訂指示を送信（**1行で記述、改行禁止**）。メッセージ宛先一覧も含める:

Worker 1 宛:
```bash
~/.claude/team/send-message.sh 1 "【Round 2】他ワーカーのresult.mdを /tmp/claude-team/$TEAM_ID/worker-*/result.md から読んでください。疑問点はteam-msg.shで質問してください（各宛先最大5件、返答はカウント外）。宛先: Worker 2,3,4。クリティカルな決定事項はresult.mdで強調またはbroadcastで共有。反論は1往復まで。質問後は返信を待たず自分のresult.mdを改訂してdoneファイルを作成してください。"
```
（WORKER_COUNT分繰り返す。宛先リストは各ワーカーから見た他ワーカーの番号）

3. 完了待機:
```bash
~/.claude/team/wait-workers.sh "$TEAM_ID" "$WORKER_COUNT" 300
```

### Step 4.5: Round 3 - 妥協・統合

**WORKER_COUNT=1 の場合はこのステップをスキップし、Step 5.5へ進む。**

1. doneファイルをリセット:
```bash
rm /tmp/claude-team/$TEAM_ID/worker-*/done
```

2. 各ワーカーに最終統合指示を送信（**1行で記述、改行禁止**）:
```bash
~/.claude/team/send-message.sh 1 "【Round 3】メッセージログ(/tmp/claude-team/$TEAM_ID/messages/log.txt)とRound 2の全result.mdを踏まえ、妥協案・合意事項・残り懸念を整理した最終版をresult.mdに書き出してください。他者と折り合いがつかない点は、双方の立場と推奨案を明記してください。doneファイルを作成してください。"
```
（WORKER_COUNT分繰り返す）

3. 完了待機:
```bash
~/.claude/team/wait-workers.sh "$TEAM_ID" "$WORKER_COUNT" 300
```

### Step 5: 結果回収
各ワーカーのresult.mdを読み取る:
```bash
for i in $(seq 1 $WORKER_COUNT); do cat /tmp/claude-team/$TEAM_ID/worker-$i/result.md; done
```

### Step 5.5: ウィンドウ復元
cleanup-team.shを実行する前に、ワーカー表示用のウィンドウ分割を元に戻す。

**WORKER_COUNT=1 の場合**: ワークスペースバッファに戻すだけ:
```bash
emacsclient -e '(let* ((target-win (seq-find (lambda (w) (string-match-p "eat-claude-worker" (buffer-name (window-buffer w)))) (window-list)))) (when target-win (set-window-buffer target-win (car (buffer-list)))))'
```

**WORKER_COUNT=2 または 4 の場合**: 通常ウィンドウを1つ残して他を削除:
```bash
emacsclient -e '(let ((claude-win (seq-find (lambda (w) (string-match-p "claude-code" (buffer-name (window-buffer w)))) (window-list))) (sidebar-win (seq-find (lambda (w) (or (string-match-p "Side Bar" (buffer-name (window-buffer w))) (window-parameter w (quote window-side)))) (window-list)))) (let* ((normal-wins (seq-filter (lambda (w) (not (or (eq w claude-win) (eq w sidebar-win) (window-minibuffer-p w)))) (window-list))) (keep-win (car normal-wins)) (delete-wins (cdr normal-wins))) (dolist (w delete-wins) (ignore-errors (delete-window w))) (when keep-win (set-window-buffer keep-win (car (buffer-list))))) (when claude-win (select-window claude-win)))'
```

復元後のウィンドウ構成を確認:
```bash
emacsclient -e '(mapcar (lambda (w) (buffer-name (window-buffer w))) (seq-remove (function window-minibuffer-p) (window-list)))'
```

### Step 6: クリーンアップ
```bash
~/.claude/team/cleanup-team.sh "$TEAM_ID" "$WORKER_COUNT"
```

### Step 7: 統合プラン生成
全ワーカーの分析結果を統合し、以下を含むプランを生成する:
1. 統合されたタスク分解（各ワーカーの視点を反映）
2. Phase 2の各ロールへの具体的タスク割り当て
3. ファイル所有権の割り当て
4. テスト方針
5. レビュー観点

### Step 7a: ユーザー承認
統合プランをユーザーに提示し、Phase 2への進行承認を得る。
以下を明示すること:
- プランの概要（何を・どう実装するか）
- 各ワーカーへのタスク割り当て
- 懸念点やトレードオフ（Phase 1で挙がったもの）
- 「Phase 2に進んでよいか？修正すべき点はあるか？」と明確に確認する

**ユーザーの承認なしにPhase 1.5以降に進んではならない。**

## Phase 1.5: 環境構築

**スキップ判定**: 以下のいずれかに該当する場合、Phase 1.5全体をスキップしてPhase 2へ進む:
- プロジェクトに `devenv.nix` が既に存在し、`~/.claude/team/setup-env.sh check "$(pwd)"` が成功する
- 環境非依存のタスクである（ドキュメント修正等）

スキップしない場合のみ以下を実行する:

### Step 7.5-pre: gitリポジトリ確認
Phase 2ではworktreeを使用するため、gitリポジトリが必要:
```bash
~/.claude/team/setup-env.sh git-init "$(pwd)"
```

### Step 7.5a: devenv初期化
```bash
~/.claude/team/setup-env.sh setup "$(pwd)"
```

initが完了したら、プロジェクトに必要な言語・ツールを`devenv.nix`に追記する。

### Step 7.5b: 環境チェック
```bash
~/.claude/team/setup-env.sh check "$(pwd)"
```

チェックが通ったらPhase 2に進む。失敗した場合はdevenv.nixを修正して再度checkする。

## Phase 2: Implementation (実装)

### Step 7.9: ウィンドウ状態確認
Phase 1のクリーンアップ後、再度ワーカー配置前にウィンドウレイアウトを確認する。
手順はStep 0.5と同一。

### Step 8: 実装チーム初期化
```bash
TEAM_ID_IMPL=$(date +%s)
~/.claude/team/init-team.sh "$TEAM_ID_IMPL" impl "$WORKER_COUNT"
```

### Step 8.5: ウィンドウ分割（Phase 2）
手順はStep 1.5と同一。Phase 2のワーカーバッファを配置する。

### Step 9: ワーカー起動待ち
```bash
sleep 15
```

### Step 10: プラン+タスク送信
各ワーカーにプランと担当タスク（ファイル所有権含む）を送信する。
プランの全文と、そのワーカー固有の担当範囲を明確に伝える。

**注意**: メッセージは必ず1行で記述すること（改行を含めない）。

### Step 11: 完了待機
```bash
~/.claude/team/wait-workers.sh "$TEAM_ID_IMPL" "$WORKER_COUNT" 600
```

### Step 12: 結果回収
```bash
for i in $(seq 1 $WORKER_COUNT); do cat /tmp/claude-team/$TEAM_ID_IMPL/worker-$i/result.md; done
```

### Step 13: 統合
**WORKER_COUNT=1 の場合**: ワーカーはメインディレクトリで直接作業しているため、cherry-pickは不要。

**WORKER_COUNT=2 または 4 の場合**: 各worktreeのコミットをメインブランチに統合する:
1. 各ワーカーのworktreeでコミットを確認: `git -C <worktree> log --oneline -5`
2. メインブランチにcherry-pick: `git cherry-pick <commit-hash>`

### Step 13.5: ウィンドウ復元（Phase 2完了時）
手順はStep 5.5と同一。

### Step 14: クリーンアップ
```bash
~/.claude/team/cleanup-team.sh "$TEAM_ID_IMPL" "$WORKER_COUNT"
```

### Step 14.5: 検証（必要に応じて）
テスト実行やフォーマット確認が必要な場合、従セッションを1つ立てて実行させる。
主セッションはdevenv有効化前から起動しているため、devenvで追加されたツールがPATHに含まれていない。

```bash
TEAM_ID_VERIFY=$(date +%s)
VERIFY_DIR="$(pwd)"
mkdir -p /tmp/claude-team/${TEAM_ID_VERIFY}/worker-1
~/.claude/team/spawn-worker.sh "$TEAM_ID_VERIFY" 1 "Verifier: テスト実行、フォーマット確認、ビルド検証を担当。" "$VERIFY_DIR" "--skip-permissions"
sleep 15
~/.claude/team/send-message.sh 1 "プロジェクトのテストを実行し、全テストがパスすることを確認してください。結果をresult.mdに書き出してください。"
~/.claude/team/wait-workers.sh "$TEAM_ID_VERIFY" 1 300
cat /tmp/claude-team/$TEAM_ID_VERIFY/worker-1/result.md
~/.claude/team/cleanup-team.sh "$TEAM_ID_VERIFY" 1
```

### Step 15: 報告
実装結果をユーザーに報告する:
- 各ワーカーの成果サマリー
- 統合されたコミット一覧
- 検証結果（Step 14.5を実施した場合）
- 残課題があれば記載

## 重要な注意事項
- `$WORKER_COUNT` は Step 0 で決定し、以降の全ステップで一貫して使用すること
- send-message.shが権限プロンプト警告を出した場合、--forceは使わずread-buffer.shで状態を確認すること
- wait-workers.shがタイムアウトした場合、read-buffer.shで各ワーカーの状態を確認し、必要に応じて追加指示を送信すること
- Phase 2の統合（cherry-pick）はcleanup前に必ず行うこと
- TEAM_IDはPhase 1とPhase 2で別のIDを使うこと（tmpディレクトリの衝突防止）
- メッセージに改行を含めないこと。改行はClaude Code TUIのマルチライン入力を起動し送信失敗の原因となる
- **主セッションのPATH制約**: 主（オーケストレータ）はdevenv有効化前から起動しているため、devenvで追加されたツール（テストランナー、フォーマッタ、LSP等）がPATHに含まれない。テスト実行・フォーマット・ビルド等のdevenv依存コマンドは、主が直接実行せず、必ず従セッション（ワーカー）を立てて委任すること
- **ウィンドウ管理**: ウィンドウの分割・復元はオーケストレータ（本手順のStep 1.5, 5.5等）が担当する。cleanup-team.shはバッファ削除とworktree/tmpの片付けのみ行う。side windowは分割できないため、必ず通常のウィンドウを対象にすること
