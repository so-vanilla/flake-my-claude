You are the orchestrator (主) of a multi-agent team using Emacs eat terminals.
Execute the following workflow to accomplish the user's task.

## Step 0: 指示書の確認

$ARGUMENTS をスペックファイルのパスとして解釈する。

1. パスが指定されていない（$ARGUMENTS が空）場合:
   ユーザーに以下を伝えて中断する:
   「指示書ファイルのパスを指定してください。使い方: /team ./path/to/spec.md」

2. 指定されたパスのファイルが存在しない場合:
   Readツールでファイルを読み取り、存在しなければユーザーに以下を伝えて中断する:
   「指示書ファイルが見つかりません: $ARGUMENTS
    ファイルを作成してから再度実行してください。」

3. ファイルが存在する場合:
   SPEC_FILE="$ARGUMENTS" として以降のステップで使用する。
   Readツールで内容を読み取り、SPEC_CONTENT として保持する。

## Step 1: エージェント数選択

タスクの複雑度を評価し、適切なエージェント数を決定する。

判断基準:
- **1**: 小規模・明確なタスク。単一視点で十分な場合
- **2**: 中規模。2軸で分析が有効な場合
- **4**: 大規模・多角的分析が必要。複数の独立した視点が価値を生む場合

ユーザーに推奨値を提示し、確認を得てから `$WORKER_COUNT` を設定する（1, 2, 4 のいずれか）。

## Step 2: ロール適合判定

タスク内容とデフォルトロールテンプレートを照合する。各Phaseのデフォルトロール:

**Phase 1（要件）**:
- 4: End User Advocate / Business/Domain Expert / Technical Feasibility / Quality Gatekeeper
- 2: Product(User+Business) / Technical(Engineering+Quality)
- 1: Requirements Analyst

**Phase 2（設計）**:
- 4: Architect / Interface Designer / Implementation Planner / Integration Engineer
- 2: Architecture(Architect+Interface) / Implementation(Planner+Integration)
- 1: Full-Stack Designer

**Phase 4（実装）**:
- 4: Researcher / Implementer / Tester / Reviewer
- 2: Builder(Researcher+Implementer) / Verifier(Tester+Reviewer)
- 1: Full-Stack

タスクにデフォルトロールが明らかにマッチしない場合、カスタムロールを提案する。
カスタムロール承認時:
```bash
echo "ロール名: 説明..." > /tmp/claude-team/${MASTER_TEAM_ID}/custom-roles-{phase}.txt
```
（1行1ロール。WORKER_COUNT分の行数）

## Step 3: 状態ディレクトリ作成
```bash
MASTER_TEAM_ID=$(date +%s)
mkdir -p /tmp/claude-team/${MASTER_TEAM_ID}
```
team-state.txt に MASTER_TEAM_ID, WORKER_COUNT, スペックファイルパスを記録:
```bash
cat > /tmp/claude-team/${MASTER_TEAM_ID}/team-state.txt << EOF
MASTER_TEAM_ID=${MASTER_TEAM_ID}
WORKER_COUNT=${WORKER_COUNT}
SPEC_FILE=${SPEC_FILE}
EOF
```
スペック内容を共有ファイルにコピー:
```bash
cp "${SPEC_FILE}" /tmp/claude-team/${MASTER_TEAM_ID}/spec.md
```

## Step 4: Phase 1 実行
Skillツールで `/team-plan` を呼び出す（args: "$MASTER_TEAM_ID $WORKER_COUNT"）。

## Step 5: 要件プラン承認
`/tmp/claude-team/${MASTER_TEAM_ID}/requirements-plan.md` をReadツールで読み取り、
ユーザーに提示してPhase 2進行の承認を得る。

## Step 6: Phase 2 実行
Skillツールで `/team-design` を呼び出す（args: "$MASTER_TEAM_ID $WORKER_COUNT"）。

## Step 7: 設計プラン承認
`/tmp/claude-team/${MASTER_TEAM_ID}/design-plan.md` をReadツールで読み取り、
ユーザーに提示してPhase 3/4進行の承認を得る。
以下を明示すること:
- プランの概要（何を・どう実装するか）
- 各ワーカーへのタスク割り当て
- 懸念点やトレードオフ

**ユーザーの承認なしにPhase 3以降に進んではならない。**

## Step 8: Phase 3 実行（条件付き）
スキップ条件: devenv.nix が既存 かつ `~/.claude/team/setup-env.sh check "$(pwd)"` が成功。
スキップしない場合: Skillツールで `/team-env` を呼び出す。

## Step 9: Phase 4 実行
Skillツールで `/team-impl` を呼び出す（args: "$MASTER_TEAM_ID $WORKER_COUNT"）。

## Step 10: 最終報告
全Phaseの結果をユーザーに報告する。

## 重要な注意事項
- `$WORKER_COUNT` は Step 1 で決定し、以降の全ステップで一貫して使用すること
- send-message.shが権限プロンプト警告を出した場合、--forceは使わずread-buffer.shで状態を確認
- wait-workers.shがタイムアウトした場合、read-buffer.shで各ワーカーの状態を確認
- メッセージに改行を含めないこと。改行はClaude Code TUIのマルチライン入力を起動し送信失敗の原因となる
- **主セッションのPATH制約**: devenvで追加されたツールがPATHに含まれない。テスト・フォーマット・ビルド等は必ずワーカーに委任
- **TEAM_ID分離**: 各Phase + 検証用で別のTEAM_IDを使うこと（tmpディレクトリの衝突防止）。MASTER_TEAM_IDは全Phase共通
- **ウィンドウ管理**: ウィンドウの分割・復元は各Phaseコマンド内で実施。cleanup-team.shはバッファ削除とworktree/tmpの片付けのみ
- 出力トークンの制限に注意。長いプラン生成やレポートは分割して書き出す
- ワーカーの result.md が途中で切れている場合は、send-message.sh で続きの書き出しを指示する
- 環境変数 CLAUDE_CODE_MAX_OUTPUT_TOKENS が設定されていれば参照する
