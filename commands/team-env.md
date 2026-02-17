Phase 3: 環境構築 + 検証。
devenv環境を構築し、ワーカーで動作検証する。

### Step 1: gitリポジトリ確認
```bash
~/.claude/team/setup-env.sh git-init "$(pwd)"
```

### Step 2: devenv初期化
```bash
~/.claude/team/setup-env.sh setup "$(pwd)"
```

### Step 3: devenv.nixへの必要ツール追記
LSP、フォーマッタ、リンタ等はグローバル環境に存在しないため、
devenv.nixで明示的にインストールすること:
- 言語ランタイム（language servers）
- フォーマッタ（formatters）
- リンタ（linters）
- テストランナー（test runners）

プロジェクトの要件に応じて適切なパッケージを追加する。

### Step 4: 環境チェック
```bash
~/.claude/team/setup-env.sh check "$(pwd)"
```
失敗時はdevenv.nixを修正して再度checkする。

### Step 5: ワーカー検証
devenv環境が正常に動作するか、ワーカーを1つ起動して検証する。
背景: claudeプロセスの立ち上げ後にdevenv環境ができるとPATHがプロセスに含まれない。
ワーカーは新プロセスとして起動するためdevenvのPATHを持つ。

```bash
TEAM_ID_VERIFY=$(date +%s)
mkdir -p /tmp/claude-team/${TEAM_ID_VERIFY}/worker-1
~/.claude/team/spawn-worker.sh "$TEAM_ID_VERIFY" 1 "Environment Verifier: 開発環境の動作確認を担当。" "$(pwd)" "--skip-permissions"
sleep 15
~/.claude/team/send-message.sh 1 "開発環境の動作確認をしてください。言語ランタイム、LSP、フォーマッタ、リンタ、テストランナーが正常に動作するか確認し、結果をresult.mdに書き出してdoneファイルを作成してください。"
~/.claude/team/wait-workers.sh "$TEAM_ID_VERIFY" 1 300
```

結果をReadツールで確認する。問題があればdevenv.nixを修正してStep 4から再実行。

```bash
~/.claude/team/cleanup-team.sh "$TEAM_ID_VERIFY" 1
```

## 注意事項
- ウィンドウ分割は不要（検証用ワーカー1つのみ）
- 主セッションはdevenvのPATHを持たないため、検証は必ずワーカーに委任する
- メッセージに改行を含めないこと
