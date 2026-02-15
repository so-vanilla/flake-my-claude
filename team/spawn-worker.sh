#!/usr/bin/env bash
# Usage: spawn-worker.sh <team-id> <worker-num> <role> [working-dir] [--skip-permissions|--no-skip-permissions]
set -euo pipefail

TEAM_ID="$1"
WORKER_NUM="$2"
ROLE="$3"
WORK_DIR="${4:-$(pwd)}"
SKIP_PERMS="${5:---no-skip-permissions}"
BUFFER_NAME="*eat-claude-worker-${WORKER_NUM}*"
RESULT_DIR="/tmp/claude-team/${TEAM_ID}/worker-${WORKER_NUM}"

mkdir -p "$RESULT_DIR"

# ワーカー用システムプロンプトをファイルに書き出し
cat > "${RESULT_DIR}/system-prompt.txt" << PROMPT_EOF
あなたはチーム(ID: ${TEAM_ID})のワーカー${WORKER_NUM}（従）です。
役割: ${ROLE}

重要なルール:
- 作業結果は ${RESULT_DIR}/result.md に書き出すこと（Bashのechoやcatで書く）
- 全作業完了後、最後に必ず ${RESULT_DIR}/done ファイルを作成すること（touch コマンドで作成。内容は空でよい）
- 他ワーカーの成果物は /tmp/claude-team/${TEAM_ID}/worker-*/result.md で参照できる
- あなたはオーケストレータ（主）の指示に従って作業する従セッションである
- タスクは1回で完結させること。他ワーカーへの追加要求は行わない
- 作業が完了したらresult.mdに書き出してdoneファイルを作成し、それ以上の作業はしない
PROMPT_EOF

PROMPT_FILE="${RESULT_DIR}/system-prompt.txt"

# claude起動引数の構築
if [[ "$SKIP_PERMS" == "--skip-permissions" ]]; then
  CLAUDE_LIST_ARGS="\"--dangerously-skip-permissions\" \"--append-system-prompt\" (with-temp-buffer (insert-file-contents \"${PROMPT_FILE}\") (buffer-string))"
else
  CLAUDE_LIST_ARGS="\"--append-system-prompt\" (with-temp-buffer (insert-file-contents \"${PROMPT_FILE}\") (buffer-string))"
fi

# eatバッファ作成 + Claude起動（perspective.el対応）
emacsclient -e "(let* ((use-persp (featurep 'perspective))
         (orig-persp (and use-persp (persp-current-name)))
         (_persp-switched
           (when use-persp
             (seq-find
               (lambda (name)
                 (persp-switch name t)
                 (seq-some
                   (lambda (b)
                     (string-match-p \"claude-code\" (buffer-name b)))
                   (persp-current-buffers)))
               (persp-names)))))
  (require 'eat)
  (let* ((buf (get-buffer-create \"${BUFFER_NAME}\"))
         (default-directory \"${WORK_DIR}/\")
         (process-environment (cons \"CLAUDE_TEAM_WORKER=1\" process-environment)))
    (with-current-buffer buf
      (unless (eq major-mode 'eat-mode) (eat-mode))
      (eat-exec buf \"claude-worker-${WORKER_NUM}\"
                \"claude\" nil
                (list ${CLAUDE_LIST_ARGS})))
    ;; perspective.elにバッファを追加
    (when use-persp (persp-add-buffer buf))
    ;; 元のperspectiveに復帰
    (when (and use-persp orig-persp
               (not (string= orig-persp (persp-current-name))))
      (persp-switch orig-persp t))
    \"${BUFFER_NAME}\"))"

# --dangerously-skip-permissions時の確認プロンプトを自動応答
if [[ "$SKIP_PERMS" == "--skip-permissions" ]]; then
  for _i in $(seq 1 30); do
    TAIL=$(emacsclient -e "(with-current-buffer \"${BUFFER_NAME}\"
      (buffer-substring-no-properties
        (max (point-min) (- (point-max) 500)) (point-max)))" 2>/dev/null || echo "")
    if echo "$TAIL" | grep -q "Yes, I accept"; then
      break
    fi
    sleep 0.5
  done
  # 下矢印で"Yes, I accept"を選択し、遅延後にEnterで確定
  emacsclient -e "(let ((buf (get-buffer \"${BUFFER_NAME}\")))
    (with-current-buffer buf
      (eat-term-send-string eat-terminal \"\e[B\"))
    (run-at-time 0.2 nil
      (lambda ()
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (eat-term-send-string eat-terminal \"\r\"))))))"
fi

echo "Worker ${WORKER_NUM} spawned in ${BUFFER_NAME} (dir: ${WORK_DIR})"
