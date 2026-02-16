#!/usr/bin/env bash
# Usage: cleanup-team.sh <team-id> <worker-count> [--keep-worktrees] [--force]
set -euo pipefail

TEAM_ID="$1"
WORKER_COUNT="$2"
shift 2

KEEP_WORKTREES=""
FORCE=""
for arg in "$@"; do
  case "$arg" in
    --keep-worktrees) KEEP_WORKTREES="yes" ;;
    --force) FORCE="yes" ;;
  esac
done

# --force時はエラーを無視して全リソースを確実に削除
if [[ "$FORCE" == "yes" ]]; then
  set +e
fi

# 1. eatバッファを安全に削除（perspective.el対応、nilガード付き）
emacsclient -e "(let* ((use-persp (featurep 'perspective))
         (orig-persp (and use-persp (persp-current-name)))
         (_persp-switched
           (when (and use-persp orig-persp)
             (seq-find
               (lambda (name)
                 (when (and name (stringp name))
                   (persp-switch name t)
                   (seq-some
                     (lambda (b)
                       (string-match-p \"claude-code\" (buffer-name b)))
                     (persp-current-buffers))))
               (seq-remove #'null (persp-names))))))
  ;; eatバッファを安全に削除
  (dolist (i '($(seq -s ' ' 1 "$WORKER_COUNT")))
    (let ((buf (get-buffer (format \"*eat-claude-worker-%d*\" i))))
      (when buf
        (with-current-buffer buf
          (when-let ((proc (get-buffer-process buf)))
            (set-process-query-on-exit-flag proc nil)))
        (let ((kill-buffer-hook nil)
              (kill-buffer-query-functions nil))
          (kill-buffer buf)))))
  ;; 元のperspectiveに復帰
  (when (and use-persp orig-persp
             (not (string= orig-persp (persp-current-name))))
    (persp-switch orig-persp t)))" 2>/dev/null || true

# 2. worktreeの片付け（--keep-worktreesで維持可能）
if [[ "$KEEP_WORKTREES" != "yes" ]]; then
  REPO_DIR=$(git rev-parse --show-toplevel 2>/dev/null || echo "")
  if [[ -n "$REPO_DIR" ]]; then
    REPO_NAME=$(basename "$REPO_DIR")
    PARENT_DIR=$(dirname "$REPO_DIR")
    for i in $(seq 1 "$WORKER_COUNT"); do
      BRANCH="team-${TEAM_ID}-worker-${i}"
      WORKTREE_DIR="${PARENT_DIR}/${REPO_NAME}_${BRANCH}"
      if [[ -d "$WORKTREE_DIR" ]]; then
        git -C "$REPO_DIR" worktree remove "$WORKTREE_DIR" --force 2>/dev/null || true
        git -C "$REPO_DIR" branch -D "$BRANCH" 2>/dev/null || true
      fi
    done
  fi
fi

# 3. tmpファイルの片付け
rm -rf "/tmp/claude-team/${TEAM_ID}"
echo "チーム ${TEAM_ID} をクリーンアップしました。"
