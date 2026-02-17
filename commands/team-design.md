Phase 2: 設計プランニング（3ラウンド制）。
引数: $ARGUMENTS → "MASTER_TEAM_ID WORKER_COUNT" をスペース区切りでパース。
引数なしの場合は独立実行: 自分でTEAM_ID生成 + WORKER_COUNTをユーザーに確認。

### Step 1: 引数パース
```
MASTER_TEAM_ID = $ARGUMENTSの第1トークン（なければ空）
WORKER_COUNT = $ARGUMENTSの第2トークン（なければユーザーに確認）
```
独立実行（MASTER_TEAM_ID空）の場合:
- MASTER_TEAM_ID=$(date +%s) で生成
- mkdir -p /tmp/claude-team/${MASTER_TEAM_ID}

### Step 2: ウィンドウ準備
Readツールで `~/.claude/team/window-guide.md` を読み、「確認」と「リセット」の手順に従ってウィンドウを準備する。

### Step 3: チーム初期化
```bash
TEAM_ID=$(date +%s)
~/.claude/team/init-team.sh "$TEAM_ID" design "$WORKER_COUNT" "$(pwd)" "$MASTER_TEAM_ID"
```

### Step 4: ウィンドウ分割
window-guide.md の「分割」手順に従い、WORKER_COUNT に応じてウィンドウを分割する。

### Step 5: ワーカー起動待ち
```bash
sleep 15
```

### Step 6: プロジェクト状態確認 + 要件プラン読み込み
```bash
ls -la
git log --oneline -5 2>/dev/null || true
```
結果を `PROJECT_CONTEXT` として要約する。

Phase 1の要件プラン (`/tmp/claude-team/${MASTER_TEAM_ID}/requirements-plan.md`) をReadツールで読み取る。

### Step 7: Round 1 — 独立分析

1. タスク仕様をファイルに書き出す（要件プランを含む）:
```bash
cat > /tmp/claude-team/$TEAM_ID/task-spec.md << 'SPEC_EOF'
# 設計タスク仕様

## プロジェクト状態
[PROJECT_CONTEXTの内容]

## 指示書
[/tmp/claude-team/${MASTER_TEAM_ID}/spec.md の全文。
 独立実行（spec.mdが存在しない）の場合は省略]

## 要件プラン
[requirements-plan.mdの全文]

## 設計指示
上記の要件プランに基づき、あなたの役割の視点で設計を提案してください。
以下を含むこと:
- モジュール分割とコンポーネント構成
- インターフェース定義（モジュール間の型・契約）
- ファイル所有権の割り当て案
- データフロー
- クリアすべきAPIテスト項目（各APIの動作確認基準）
result.mdに書き出してdoneファイルを作成してください。
SPEC_EOF
```

2. 各ワーカーにメッセージ送信（**1行で記述、改行禁止**）。制限前文を必ず含める:
```bash
~/.claude/team/send-message.sh 1 "【重要な行動制限】あなたは強い権限(--dangerously-skip-permissions)で動作しています。以下を厳守してください: (1) 指示書とオーケストレータの指示の範囲外の作業は一切禁止 (2) rm/削除は実行前にファイル一覧を確認し対象のみ削除(rm -rfの広範囲適用禁止) (3) git push/force-push禁止 (4) /tmp/claude-team/ 以外のシステムファイル変更禁止 (5) 不明点はresult.mdに書き出して判断をオーケストレータに委ねること。指示の範囲を超えた「改善」は行わないこと。--- /tmp/claude-team/$TEAM_ID/task-spec.md を読み、あなたの役割の視点で設計を提案してください。result.mdに書き出してdoneファイルを作成してください。"
```
（WORKER_COUNT分繰り返す。制限前文は毎回含めること）

3. 完了待機:
```bash
~/.claude/team/wait-workers.sh "$TEAM_ID" "$WORKER_COUNT" 300
```

### Step 8: Round 2 — 相互参照

**WORKER_COUNT=1 の場合はスキップ → Step 10へ。**

1. doneファイルをリセット:
```bash
rm /tmp/claude-team/$TEAM_ID/worker-*/done
```

2. 各ワーカーに改訂指示を送信（**1行で記述、改行禁止**）。制限前文を含める:
```bash
~/.claude/team/send-message.sh 1 "【重要な行動制限】(1) 指示範囲外の作業禁止 (2) rm -rfの広範囲適用禁止 (3) git push禁止 (4) システムファイル変更禁止 (5) 不明点はresult.mdへ。--- 【Round 2】他ワーカーのresult.mdを /tmp/claude-team/$TEAM_ID/worker-*/result.md から読んでください。インターフェースの整合性、モジュール境界の妥当性を検証し、疑問点はteam-msg.shで質問してください。改訂版をresult.mdに書き出してdoneファイルを作成してください。"
```
（WORKER_COUNT分繰り返す）

3. 完了待機:
```bash
~/.claude/team/wait-workers.sh "$TEAM_ID" "$WORKER_COUNT" 300
```

### Step 9: Round 3 — 妥協・統合

**WORKER_COUNT=1 の場合はスキップ → Step 10へ。**

1. doneファイルをリセット:
```bash
rm /tmp/claude-team/$TEAM_ID/worker-*/done
```

2. 各ワーカーに最終統合指示を送信（**1行で記述、改行禁止**）。制限前文を含める:
```bash
~/.claude/team/send-message.sh 1 "【重要な行動制限】(1) 指示範囲外の作業禁止 (2) rm -rfの広範囲適用禁止 (3) git push禁止 (4) システムファイル変更禁止 (5) 不明点はresult.mdへ。--- 【Round 3】メッセージログ(/tmp/claude-team/$TEAM_ID/messages/log.txt)とRound 2の全result.mdを踏まえ、設計の妥協案・合意事項・残り懸念を整理した最終版をresult.mdに書き出してください。doneファイルを作成してください。"
```
（WORKER_COUNT分繰り返す）

3. 完了待機:
```bash
~/.claude/team/wait-workers.sh "$TEAM_ID" "$WORKER_COUNT" 300
```

### Step 10: 結果回収
各ワーカーのresult.mdをReadツールで読み取る。

### Step 11: ウィンドウ復元
window-guide.md の「復元」手順に従う。

### Step 12: クリーンアップ
```bash
~/.claude/team/cleanup-team.sh "$TEAM_ID" "$WORKER_COUNT"
```

### Step 13: 設計プラン生成
全ワーカーの分析結果を統合し、以下を含む設計プランを生成する:
1. モジュール分割とコンポーネント構成
2. インターフェース定義（モジュール間の型・契約）
3. ファイル所有権の割り当て（Phase 4の各ワーカーに対応）
4. データフロー
5. クリアすべきAPIテスト項目（各APIの動作確認基準）

書き出し先: `/tmp/claude-team/${MASTER_TEAM_ID}/design-plan.md`

### Step 14: テスト項目の整合性検証（オーケストレータ）
design-plan.md 内のAPI仕様とテスト項目を照合する:
- 全APIに対応するテスト項目が存在するか
- テスト項目がAPI仕様と矛盾していないか
- テストのカバレッジに漏れがないか
不整合がある場合はオーケストレータ自身が修正してdesign-plan.mdを更新する。

## 設計プランの品質要件
設計プランは、Phase 4（実装）で複数ワーカーが並列作業しても不整合が起きない粒度で記述すること。
ファイル単位の分割書き込み指示、モジュール間インターフェースの型定義、
共有定数・型の一元管理方針を含めること。巨大ファイルの分割書き込みになる場合も、
インターフェースが明確であれば問題ない。

設計プランには「クリアすべきAPIテスト項目」セクションを必ず含めること。
各APIエンドポイント/インターフェースに対して、正常系・異常系の動作確認基準を定義する。
Phase 4の検証ステップ（Step 11）でこのテスト項目をクリアすることが完了条件となる。

## 注意事項
- send-message.shが権限プロンプト警告を出した場合、--forceは使わずread-buffer.shで状態を確認
- wait-workers.shがタイムアウトした場合、read-buffer.shで各ワーカーの状態を確認
- メッセージに改行を含めないこと
- 出力トークンの制限に注意。長いプラン生成は分割して書き出す
- ワーカーの result.md が途中で切れている場合は、send-message.sh で続きの書き出しを指示する
