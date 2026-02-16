#!/usr/bin/env bash
# Usage: init-team.sh <team-id> <phase> [working-dir]
# phase: "plan" or "impl"
set -euo pipefail

TEAM_ID="$1"
PHASE="$2"
WORK_DIR="${3:-$(pwd)}"

# 事前確認: Emacs + eat + claude-code-ideバッファの存在チェック
CHECK=$(emacsclient -e '(list
  (featurep (quote eat))
  (and (seq-find (lambda (w)
    (string-match-p "claude-code" (buffer-name (window-buffer w))))
    (window-list)) t))' 2>/dev/null || echo "(nil nil)")

if echo "$CHECK" | grep -q "nil"; then
  echo "Error: Emacs上のclaude-code-ide環境でのみ実行可能です。" >&2
  echo "必要条件: Emacsサーバー起動、eatパッケージ、claude-code-ideバッファ" >&2
  exit 1
fi

# フェーズに応じたロール定義と権限設定
case "$PHASE" in
  plan)
    PERM_FLAG="--skip-permissions"
    ROLES=(
      "Engineer×Conservative: 技術的に堅実な設計を提案。既存パターン活用、実績ある手法を重視。プロジェクトファイルの編集は行わないこと。"
      "User×Innovative: ユーザー体験の革新を提案。DX向上、新しいアプローチを追求。プロジェクトファイルの編集は行わないこと。"
      "Security×Critical: リスク・脆弱性を指摘。エッジケース発見、Devil's advocateとして機能。プロジェクトファイルの編集は行わないこと。"
      "PM×Integrative: 全体の一貫性を確保。優先順位、スコープ調整、妥協点を提示。プロジェクトファイルの編集は行わないこと。"
    )
    ;;
  impl)
    PERM_FLAG="--skip-permissions"
    ROLES=(
      "Researcher: コードベース調査、情報収集、既存パターン分析を担当。"
      "Implementer: コード実装、ファイル編集を担当。"
      "Tester: テスト作成・実行、エッジケース検証を担当。"
      "Reviewer: コードレビュー、品質チェック、改善提案を担当。"
    )
    ;;
  *)
    echo "Error: phase must be 'plan' or 'impl'" >&2
    exit 1
    ;;
esac

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
WORKER_COUNT=4

# 共有ディレクトリとチーム情報を作成
mkdir -p "/tmp/claude-team/${TEAM_ID}/messages"
{
  echo "TEAM_ID=${TEAM_ID}"
  echo "PHASE=${PHASE}"
  echo "WORKER_COUNT=${WORKER_COUNT}"
  for i in $(seq 1 $WORKER_COUNT); do
    echo "WORKER_${i}_ROLE=${ROLES[$((i-1))]}"
  done
} > "/tmp/claude-team/${TEAM_ID}/team-info.txt"

# Phase 2ではワーカーごとにworktreeを作成
if [[ "$PHASE" == "impl" ]]; then
  REPO_DIR=$(git -C "$WORK_DIR" rev-parse --show-toplevel)
  REPO_NAME=$(basename "$REPO_DIR")
  PARENT_DIR=$(dirname "$REPO_DIR")

  for i in 1 2 3 4; do
    BRANCH="team-${TEAM_ID}-worker-${i}"
    WORKTREE_DIR="${PARENT_DIR}/${REPO_NAME}_${BRANCH}"
    git -C "$REPO_DIR" worktree add "$WORKTREE_DIR" -b "$BRANCH" 2>/dev/null || true
    "$SCRIPT_DIR/spawn-worker.sh" "$TEAM_ID" "$i" "${ROLES[$((i-1))]}" "$WORKTREE_DIR" "$PERM_FLAG"
  done
else
  # Phase 1: 同一ディレクトリで読み取り専用
  for i in 1 2 3 4; do
    "$SCRIPT_DIR/spawn-worker.sh" "$TEAM_ID" "$i" "${ROLES[$((i-1))]}" "$WORK_DIR" "$PERM_FLAG"
  done
fi

echo "Team ${TEAM_ID} initialized (phase: ${PHASE}, workers: 4)"
