You are the orchestrator (主) of a multi-agent team using Emacs eat terminals.
Execute the following 2-phase workflow to accomplish the user's task: $ARGUMENTS

## Phase 1: Planning (多視点分析)

### Step 1: チーム初期化
```bash
TEAM_ID=$(date +%s)
~/.claude/team/init-team.sh "$TEAM_ID" plan
```

### Step 2: ワーカー起動待ち
Claude CLIの起動に時間がかかるため、各ワーカーの準備完了を待つ:
```bash
sleep 15
```

### Step 2.5: プロジェクト状態確認
ワーカーの初期探索を省略させるため、プロジェクトの状態を事前に把握する:
```bash
ls -la
git log --oneline -5 2>/dev/null || true
```

結果を `PROJECT_CONTEXT` として要約する。例:
- 空のプロジェクト → 「新規プロジェクト（ファイルなし）」
- 既存プロジェクト → 「既存プロジェクト（主要ファイル: flake.nix, src/, tests/ 等）、直近コミット: ...」

この要約を Step 3 のメッセージに含める。

### Step 3: タスク送信
各ワーカーに分析指示を送信する。メッセージにはプロジェクト状態（PROJECT_CONTEXT）とユーザーのタスク（$ARGUMENTS）を含め、各ワーカーの視点に合わせた指示を与える。

**注意**: メッセージは必ず1行で記述すること（改行を含めない）。改行はClaude Code TUIのマルチライン入力モードを起動し、送信に失敗する原因となる。

Worker 1 (Engineer×Conservative):
```bash
~/.claude/team/send-message.sh 1 "プロジェクト状態: [PROJECT_CONTEXT] 以下のタスクを技術的に堅実な視点で分析してください。既存パターンの活用、実績ある手法を重視し、result.mdに書き出してください。プロジェクト状態は把握済みなので探索は不要です。タスク: $ARGUMENTS"
```

Worker 2 (User×Innovative):
```bash
~/.claude/team/send-message.sh 2 "プロジェクト状態: [PROJECT_CONTEXT] 以下のタスクをユーザー体験・DXの視点で分析してください。革新的なアプローチも検討し、result.mdに書き出してください。プロジェクト状態は把握済みなので探索は不要です。タスク: $ARGUMENTS"
```

Worker 3 (Security×Critical):
```bash
~/.claude/team/send-message.sh 3 "プロジェクト状態: [PROJECT_CONTEXT] 以下のタスクのリスク・脆弱性・エッジケースを分析してください。Devil's advocateとして問題点を指摘し、result.mdに書き出してください。プロジェクト状態は把握済みなので探索は不要です。タスク: $ARGUMENTS"
```

Worker 4 (PM×Integrative):
```bash
~/.claude/team/send-message.sh 4 "プロジェクト状態: [PROJECT_CONTEXT] 以下のタスクの全体的な一貫性、優先順位、スコープを分析してください。妥協点も提示し、result.mdに書き出してください。プロジェクト状態は把握済みなので探索は不要です。タスク: $ARGUMENTS"
```

### Step 4: 完了待機
```bash
~/.claude/team/wait-workers.sh "$TEAM_ID" 4 300
```

### Step 4.5: 相互参照ラウンド（動的、最大2回）
各ワーカーのresult.mdを軽く確認し、分析の質を評価する:
```bash
head -20 /tmp/claude-team/$TEAM_ID/worker-1/result.md
head -20 /tmp/claude-team/$TEAM_ID/worker-2/result.md
head -20 /tmp/claude-team/$TEAM_ID/worker-3/result.md
head -20 /tmp/claude-team/$TEAM_ID/worker-4/result.md
```

以下のいずれかに該当する場合、改訂ラウンドを実施する（該当しなければStep 5へ進む）:
- 分析の視点が一方に偏っている（例: 全員が同じ結論で反対意見がない）
- ワーカー間で矛盾する主張があり、解消されていない
- 重要な観点が深掘りされていない

改訂ラウンドの手順:
1. doneファイルをリセット:
```bash
rm /tmp/claude-team/$TEAM_ID/worker-*/done
```
2. 各ワーカーに改訂指示を送信:
```bash
~/.claude/team/send-message.sh 1 "他の3名のワーカーの分析を /tmp/claude-team/$TEAM_ID/worker-*/result.md から読み、あなたの分析を改訂してresult.mdを上書きしてください。特に見落としている観点や矛盾点に注目してください。"
~/.claude/team/send-message.sh 2 "他の3名のワーカーの分析を /tmp/claude-team/$TEAM_ID/worker-*/result.md から読み、あなたの分析を改訂してresult.mdを上書きしてください。特に見落としている観点や矛盾点に注目してください。"
~/.claude/team/send-message.sh 3 "他の3名のワーカーの分析を /tmp/claude-team/$TEAM_ID/worker-*/result.md から読み、あなたの分析を改訂してresult.mdを上書きしてください。特に見落としている観点や矛盾点に注目してください。"
~/.claude/team/send-message.sh 4 "他の3名のワーカーの分析を /tmp/claude-team/$TEAM_ID/worker-*/result.md から読み、あなたの分析を改訂してresult.mdを上書きしてください。特に見落としている観点や矛盾点に注目してください。"
```
3. 再待機:
```bash
~/.claude/team/wait-workers.sh "$TEAM_ID" 4 300
```
4. 必要ならもう1ラウンド繰り返す（最大2回まで）。十分と判断したらStep 5へ。

### Step 5: 結果回収
各ワーカーのresult.mdを読み取る:
```bash
cat /tmp/claude-team/$TEAM_ID/worker-1/result.md
cat /tmp/claude-team/$TEAM_ID/worker-2/result.md
cat /tmp/claude-team/$TEAM_ID/worker-3/result.md
cat /tmp/claude-team/$TEAM_ID/worker-4/result.md
```

### Step 6: クリーンアップ
```bash
~/.claude/team/cleanup-team.sh "$TEAM_ID" 4
```

### Step 7: 統合プラン生成
4つの分析結果を統合し、以下を含むプランを生成する:
1. 統合されたタスク分解（各ワーカーの視点を反映）
2. Phase 2の各ロール（Researcher/Implementer/Tester/Reviewer）への具体的タスク割り当て
3. ファイル所有権の割り当て（Implementerへ）
4. テスト方針（Testerへ）
5. レビュー観点（Reviewerへ）

### Step 7a: ユーザー承認
統合プランをユーザーに提示し、Phase 2への進行承認を得る。
以下を明示すること:
- プランの概要（何を・どう実装するか）
- 各ワーカーへのタスク割り当て
- 懸念点やトレードオフ（Phase 1で挙がったもの）
- 「Phase 2に進んでよいか？修正すべき点はあるか？」と明確に確認する

**ユーザーの承認なしにPhase 1.5以降に進んではならない。**

## Phase 1.5: 環境構築

### Step 7.5: 環境構築（非対話）
Phase 2で必要な開発環境を事前構築する。claude -p（パイプモード）で非対話実行するため、eat terminalやperspective.elの影響を受けない。

```bash
~/.claude/team/setup-env.sh "$(pwd)" 300
```

この結果を確認し、環境が正常に構築されたことを確認してからPhase 2に進む。
devenv構築や依存関係のインストールが完了していることを確認する。
環境構築が不要な場合（既に構築済み、または環境非依存のタスクの場合）はこのステップをスキップしてよい。

## Phase 2: Implementation (実装)

### Step 8: 実装チーム初期化
```bash
TEAM_ID_IMPL=$(date +%s)
~/.claude/team/init-team.sh "$TEAM_ID_IMPL" impl
```

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
~/.claude/team/wait-workers.sh "$TEAM_ID_IMPL" 4 600
```

### Step 12: 結果回収
```bash
cat /tmp/claude-team/$TEAM_ID_IMPL/worker-1/result.md
cat /tmp/claude-team/$TEAM_ID_IMPL/worker-2/result.md
cat /tmp/claude-team/$TEAM_ID_IMPL/worker-3/result.md
cat /tmp/claude-team/$TEAM_ID_IMPL/worker-4/result.md
```

### Step 13: 統合
各worktreeのコミットをメインブランチに統合する:
1. 各ワーカーのworktreeでコミットを確認: `git -C <worktree> log --oneline -5`
2. メインブランチにcherry-pick: `git cherry-pick <commit-hash>`

### Step 14: クリーンアップ
```bash
~/.claude/team/cleanup-team.sh "$TEAM_ID_IMPL" 4
```

### Step 14.5: 検証（必要に応じて）
テスト実行やフォーマット確認が必要な場合、従セッションを1つ立てて実行させる。
主セッションはdevenv構築前から起動しているため、devenvで追加されたツールがPATHに含まれていない。
主が直接テストやフォーマッタを実行しても失敗するため、必ず従セッションに委任すること。

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
- send-message.shが権限プロンプト警告を出した場合、--forceは使わずread-buffer.shで状態を確認すること
- wait-workers.shがタイムアウトした場合、read-buffer.shで各ワーカーの状態を確認し、必要に応じて追加指示を送信すること
- Phase 2の統合（cherry-pick）はcleanup前に必ず行うこと
- TEAM_IDはPhase 1とPhase 2で別のIDを使うこと（tmpディレクトリの衝突防止）
- メッセージに改行を含めないこと。改行はClaude Code TUIのマルチライン入力を起動し送信失敗の原因となる
- **主セッションのPATH制約**: 主（オーケストレータ）はdevenv有効化前から起動しているため、devenvで追加されたツール（テストランナー、フォーマッタ、LSP等）がPATHに含まれない。テスト実行・フォーマット・ビルド等のdevenv依存コマンドは、主が直接実行せず、必ず従セッション（ワーカー）を立てて委任すること
