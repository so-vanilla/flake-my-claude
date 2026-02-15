#!/usr/bin/env bash
# Usage: send-message.sh <worker-num> <message> [--force]
set -euo pipefail

WORKER_NUM="$1"
MESSAGE="$2"
FORCE="${3:-}"
BUFFER_NAME="*eat-claude-worker-${WORKER_NUM}*"

# 権限プロンプト中の送信防止チェック（--forceで無視可能）
if [[ "$FORCE" != "--force" ]]; then
  TAIL=$(emacsclient -e "(with-current-buffer \"${BUFFER_NAME}\"
    (buffer-substring-no-properties
      (max (point-min) (- (point-max) 500)) (point-max)))" 2>/dev/null || echo "")

  if echo "$TAIL" | grep -viE 'bypass permissions' | grep -qiE '(allow|deny|permission|approve|\[Y/n\]|\[y/N\])'; then
    echo "WARNING: Worker ${WORKER_NUM} は権限プロンプト中の可能性があります。" >&2
    echo "メッセージ送信を中止しました。--force で強制送信できます。" >&2
    exit 1
  fi
fi

# メッセージ内のダブルクォートとバックスラッシュをエスケープ
ESCAPED=$(printf '%s' "$MESSAGE" | tr '\n' ' ' | sed 's/\\/\\\\/g; s/"/\\"/g')

# テキストとCR(Enter)を分離送信
# eat-term-send-stringで一括送信するとCRがテキストの一部として扱われ確定されない
# run-at-timeで遅延させることでClaude CodeのTUIがEnterとして認識する
emacsclient -e "(let ((buf (get-buffer \"${BUFFER_NAME}\")))
  (with-current-buffer buf
    (eat-term-send-string eat-terminal \"${ESCAPED}\"))
  (run-at-time 0.2 nil
    (lambda ()
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (eat-term-send-string eat-terminal \"\r\"))))))"
