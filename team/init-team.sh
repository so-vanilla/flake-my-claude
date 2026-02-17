#!/usr/bin/env bash
# Usage: init-team.sh <team-id> <phase> [worker-count] [working-dir] [master-team-id]
# phase: "req-plan", "design", "plan" (後方互換=req-plan), or "impl"
# worker-count: 1, 2, or 4 (default: 4)
set -euo pipefail

TEAM_ID="$1"
PHASE="$2"
WORKER_COUNT="${3:-4}"
WORK_DIR="${4:-$(pwd)}"
MASTER_TEAM_ID="${5:-}"

# ワーカー数のバリデーション
case "$WORKER_COUNT" in
  1|2|4) ;;
  *)
    echo "Error: worker-count must be 1, 2, or 4 (got: ${WORKER_COUNT})" >&2
    exit 1
    ;;
esac

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

# フェーズとワーカー数に応じたロール定義と権限設定
PERM_FLAG="--skip-permissions"

# カスタムロールファイルチェック（デフォルトロールより優先）
ROLES=()
if [[ -n "$MASTER_TEAM_ID" ]]; then
  CUSTOM_FILE="/tmp/claude-team/${MASTER_TEAM_ID}/custom-roles-${PHASE}.txt"
  if [[ -f "$CUSTOM_FILE" ]]; then
    mapfile -t ROLES < "$CUSTOM_FILE"
  fi
fi

# ROLES未設定ならデフォルトのcase文へ
if [[ ${#ROLES[@]} -eq 0 ]]; then
  case "$PHASE" in
    req-plan|plan)
      case "$WORKER_COUNT" in
        4)
          ROLES=(
            "End User Advocate: ユーザー体験・ユースケース・使いやすさを分析。プロジェクトファイルの編集は行わないこと。"
            "Business/Domain Expert: ビジネス要件・優先度・ROIを分析。プロジェクトファイルの編集は行わないこと。"
            "Technical Feasibility: 技術的実現可能性・制約・リスクを評価。プロジェクトファイルの編集は行わないこと。"
            "Quality Gatekeeper: 品質要件・セキュリティ・運用性・保守性を評価。プロジェクトファイルの編集は行わないこと。"
          )
          ;;
        2)
          ROLES=(
            "Product(User+Business): プロダクト視点で要件を定義。ユーザー体験とビジネス優先度を統合。プロジェクトファイルの編集は行わないこと。"
            "Technical(Engineering+Quality): 技術視点で実現性と品質を評価。制約・リスク・保守性を分析。プロジェクトファイルの編集は行わないこと。"
          )
          ;;
        1)
          ROLES=(
            "Requirements Analyst: 全視点を統合した包括的要件分析。ユーザー・ビジネス・技術・品質の全観点。プロジェクトファイルの編集は行わないこと。"
          )
          ;;
      esac
      ;;
    design)
      case "$WORKER_COUNT" in
        4)
          ROLES=(
            "Architect: 全体構造・モジュール分割・依存関係を設計。プロジェクトファイルの編集は行わないこと。"
            "Interface Designer: API設計・型定義・モジュール間契約を策定。プロジェクトファイルの編集は行わないこと。"
            "Implementation Planner: 実装手順・アルゴリズム・ファイル分割を計画。プロジェクトファイルの編集は行わないこと。"
            "Integration Engineer: 統合計画・テスト戦略・エッジケースを検証。プロジェクトファイルの編集は行わないこと。"
          )
          ;;
        2)
          ROLES=(
            "Architecture(Architect+Interface): 構造設計と契約定義。モジュール分割・API設計を統合。プロジェクトファイルの編集は行わないこと。"
            "Implementation(Planner+Integration): 実装計画と統合検証。ファイル分割・テスト戦略を統合。プロジェクトファイルの編集は行わないこと。"
          )
          ;;
        1)
          ROLES=(
            "Full-Stack Designer: 全設計を一貫して担当。構造・API・実装計画・テスト戦略。プロジェクトファイルの編集は行わないこと。"
          )
          ;;
      esac
      ;;
    impl)
      case "$WORKER_COUNT" in
        4)
          ROLES=(
            "Researcher: コードベース調査、情報収集、既存パターン分析を担当。"
            "Implementer: コード実装、ファイル編集を担当。"
            "Tester: テスト作成・実行、エッジケース検証を担当。"
            "Reviewer: コードレビュー、品質チェック、改善提案を担当。"
          )
          ;;
        2)
          ROLES=(
            "Builder(Researcher+Implementer): コードベース調査・情報収集と、コード実装・ファイル編集を担当。"
            "Verifier(Tester+Reviewer): テスト作成・実行、エッジケース検証、コードレビュー、品質チェックを担当。"
          )
          ;;
        1)
          ROLES=(
            "Full-Stack: コードベース調査、コード実装、テスト作成・実行、コードレビューの全工程を担当。"
          )
          ;;
      esac
      ;;
    *)
      echo "Error: phase must be 'req-plan', 'design', 'plan', or 'impl'" >&2
      exit 1
      ;;
  esac
fi

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# 共有ディレクトリとチーム情報を作成
mkdir -p "/tmp/claude-team/${TEAM_ID}/messages"
{
  echo "TEAM_ID=${TEAM_ID}"
  echo "PHASE=${PHASE}"
  echo "WORKER_COUNT=${WORKER_COUNT}"
  [[ -n "$MASTER_TEAM_ID" ]] && echo "MASTER_TEAM_ID=${MASTER_TEAM_ID}"
  for i in $(seq 1 "$WORKER_COUNT"); do
    echo "WORKER_${i}_ROLE=${ROLES[$((i-1))]}"
  done
} > "/tmp/claude-team/${TEAM_ID}/team-info.txt"

# implフェーズではワーカーごとにworktreeを作成
if [[ "$PHASE" == "impl" ]]; then
  REPO_DIR=$(git -C "$WORK_DIR" rev-parse --show-toplevel)
  REPO_NAME=$(basename "$REPO_DIR")
  PARENT_DIR=$(dirname "$REPO_DIR")

  # HEADにコミットが存在するか検証（空リポジトリ対策）
  if ! git -C "$REPO_DIR" rev-parse HEAD &>/dev/null; then
    echo "Warning: リポジトリにコミットが存在しません。initial commitを作成します。" >&2
    git -C "$REPO_DIR" commit --allow-empty -m "initial commit for team worktrees"
  fi

  for i in $(seq 1 "$WORKER_COUNT"); do
    BRANCH="team-${TEAM_ID}-worker-${i}"
    WORKTREE_DIR="${PARENT_DIR}/${REPO_NAME}_${BRANCH}"

    # 既存の同名ブランチがあれば削除を試みる
    if git -C "$REPO_DIR" show-ref --verify --quiet "refs/heads/${BRANCH}"; then
      echo "Warning: ブランチ ${BRANCH} が既に存在します。既存worktreeを確認します。" >&2
      git -C "$REPO_DIR" worktree remove "$WORKTREE_DIR" 2>/dev/null || true
      git -C "$REPO_DIR" branch -D "$BRANCH" 2>/dev/null || true
    fi

    # worktree作成（エラーを表示）
    if ! git -C "$REPO_DIR" worktree add "$WORKTREE_DIR" -b "$BRANCH"; then
      echo "Error: worktree作成に失敗しました (branch: ${BRANCH}, dir: ${WORKTREE_DIR})" >&2
      exit 1
    fi

    # 作成後の検証: ブランチがHEADから分岐していることを確認
    WORKTREE_HEAD=$(git -C "$WORKTREE_DIR" rev-parse HEAD)
    MAIN_HEAD=$(git -C "$REPO_DIR" rev-parse HEAD)
    if [[ "$WORKTREE_HEAD" != "$MAIN_HEAD" ]]; then
      echo "Warning: worktreeのHEAD(${WORKTREE_HEAD:0:8})がメインのHEAD(${MAIN_HEAD:0:8})と一致しません。" >&2
    fi

    "$SCRIPT_DIR/spawn-worker.sh" "$TEAM_ID" "$i" "${ROLES[$((i-1))]}" "$WORKTREE_DIR" "$PERM_FLAG"
  done
else
  # req-plan/design/plan: 同一ディレクトリで読み取り専用
  for i in $(seq 1 "$WORKER_COUNT"); do
    "$SCRIPT_DIR/spawn-worker.sh" "$TEAM_ID" "$i" "${ROLES[$((i-1))]}" "$WORK_DIR" "$PERM_FLAG"
  done
fi

echo "Team ${TEAM_ID} initialized (phase: ${PHASE}, workers: ${WORKER_COUNT})"
