Phase 4: 実装。
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

### Step 3: 実装チーム初期化
```bash
TEAM_ID=$(date +%s)
~/.claude/team/init-team.sh "$TEAM_ID" impl "$WORKER_COUNT" "$(pwd)" "$MASTER_TEAM_ID"
```
WORKER_COUNT>=2の場合、init-team.shが自動的にworktreeを作成する
（{repo}_{team-TEAM_ID-worker-N} の形式で親ディレクトリに配置）。

### Step 4: ウィンドウ分割
window-guide.md の「分割」手順に従い、WORKER_COUNT に応じてウィンドウを分割する。

### Step 5: ワーカー起動待ち
```bash
sleep 15
```

### Step 6: プラン+タスク送信
design-plan.md (`/tmp/claude-team/${MASTER_TEAM_ID}/design-plan.md`) からプランをReadツールで読み取り、各ワーカーに担当タスク（ファイル所有権含む）を送信する。

各ワーカーへの送信時、以下の制限前文を必ずメッセージの先頭に含める:
```
【重要な行動制限】あなたは強い権限(--dangerously-skip-permissions)で動作しています。以下を厳守してください: (1) 設計プランとオーケストレータの指示の範囲外の作業は禁止 (2) 担当ファイル以外の編集禁止 (3) rm/削除は対象を明示的に指定し最小範囲で実行(rm -rf でディレクトリごと削除する前に内容を確認) (4) git push/force-push禁止 (5) 不明点はresult.mdに書き出して判断をオーケストレータに委ねること。---
```

続けて、送信時に以下を明記すること（**1行で記述、改行禁止**）:
- 各ワーカーのworktreeパス（WORKER_COUNT>=2の場合）
- ファイル所有権（どのワーカーがどのファイルを担当するか）
- ワーカー間の依存関係がある場合は team-msg.sh での連携を推奨:
  「実装が他ワーカーの成果に依存する場合や、インターフェースの変更が必要な場合は、
   team-msg.sh で相手ワーカーと合意を取ってから進めること。
   用法: ~/.claude/team/team-msg.sh $TEAM_ID <自分の番号> <相手の番号|broadcast> <メッセージ>」

### Step 7: 完了待機
```bash
~/.claude/team/wait-workers.sh "$TEAM_ID" "$WORKER_COUNT" 600
```

### Step 8: 結果回収
各ワーカーのresult.mdをReadツールで読み取る。

### Step 9: 統合
**WORKER_COUNT=1**: ワーカーはメインディレクトリで直接作業。cherry-pick不要。

**WORKER_COUNT=2/4**: 各worktreeのコミットをメインブランチにcherry-pick。
1. 各ワーカーのworktreeでコミットを確認:
```bash
git -C <worktree> log --oneline -5
```
2. メインブランチにcherry-pick:
```bash
git cherry-pick <commit-hash>
```
衝突が発生した場合は手動で解決し、解決内容をメモする。

### Step 10: ウィンドウ復元
window-guide.md の「復元」手順に従う。

### Step 11: クリーンアップ
```bash
~/.claude/team/cleanup-team.sh "$TEAM_ID" "$WORKER_COUNT"
```

### Step 12: 検証・修正（1ワーカー）
マージ後の統合確認として、ワーカーを1つ起動して以下を実行させる:
- design-plan.md の「クリアすべきAPIテスト項目」に基づく検証
- テスト実行、フォーマット確認、ビルド検証
- 不整合や失敗があれば修正
- 修正完了後に結果をresult.mdに報告

背景: 主セッションはdevenvのPATHを持たないため、devenv依存コマンドは
必ずワーカーに委任する。ワーカーは --dangerously-skip-permissions で
起動するため権限確認が不要。

```bash
TEAM_ID_VERIFY=$(date +%s)
mkdir -p /tmp/claude-team/${TEAM_ID_VERIFY}/worker-1
~/.claude/team/spawn-worker.sh "$TEAM_ID_VERIFY" 1 "Verifier: テスト実行、フォーマット確認、ビルド検証、不整合修正を担当。" "$(pwd)" "--skip-permissions"
sleep 15
~/.claude/team/send-message.sh 1 "【重要な行動制限】(1) 設計プランとオーケストレータの指示の範囲外の作業禁止 (2) rm/削除は対象を明示的に指定し最小範囲で実行 (3) git push禁止 (4) 不明点はresult.mdへ。--- マージ後の統合確認をしてください。テスト実行、フォーマット確認、ビルド検証を行い、不整合があれば修正してください。結果をresult.mdに書き出してdoneファイルを作成してください。"
~/.claude/team/wait-workers.sh "$TEAM_ID_VERIFY" 1 300
```

結果をReadツールで確認。

```bash
~/.claude/team/cleanup-team.sh "$TEAM_ID_VERIFY" 1
```

### Step 13: 報告
実装結果をユーザーに報告する:
- 各ワーカーの成果サマリー
- 統合されたコミット一覧
- 検証結果（Step 12の結果）
- 残課題があれば記載

## 注意事項
- send-message.shが権限プロンプト警告を出した場合、--forceは使わずread-buffer.shで状態を確認
- wait-workers.shがタイムアウトした場合、read-buffer.shで各ワーカーの状態を確認
- メッセージに改行を含めないこと
- Phase 4の統合（cherry-pick）はcleanup前に必ず行うこと
- 出力トークンの制限に注意。長いレポートは分割して書き出す
- ワーカーの result.md が途中で切れている場合は、send-message.sh で続きの書き出しを指示する
- **主セッションのPATH制約**: devenv依存コマンドは主が直接実行せず、必ずワーカーに委任
