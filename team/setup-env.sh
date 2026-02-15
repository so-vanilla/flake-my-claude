#!/usr/bin/env bash
# Usage: setup-env.sh <working-dir> [timeout-seconds]
# claude -p（パイプモード）で環境構築を非対話実行
set -euo pipefail

WORK_DIR="$1"
TIMEOUT="${2:-300}"
RESULT_FILE="/tmp/claude-team/setup-$(date +%s)/result.md"
mkdir -p "$(dirname "$RESULT_FILE")"

cd "$WORK_DIR"

export CLAUDE_TEAM_WORKER=1
timeout "$TIMEOUT" claude -p \
  --dangerously-skip-permissions \
  "このプロジェクトの開発環境を構築してください。devenv.nixがあればdevenvを使い、必要な依存関係をインストールしてください。構築内容をサマリーとして出力してください。" \
  > "$RESULT_FILE" 2>&1 || {
    echo "環境構築が失敗またはタイムアウトしました (exit: $?)" >&2
    if [[ -f "$RESULT_FILE" ]]; then
      echo "--- 出力 ---"
      cat "$RESULT_FILE"
    fi
    exit 1
  }

echo "環境構築完了: $RESULT_FILE"
cat "$RESULT_FILE"
