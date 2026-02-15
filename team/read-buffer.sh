#!/usr/bin/env bash
# Usage: read-buffer.sh <worker-num> [char-count]
set -euo pipefail

WORKER_NUM="$1"
CHAR_COUNT="${2:-3000}"
BUFFER_NAME="*eat-claude-worker-${WORKER_NUM}*"

emacsclient -e "(with-current-buffer \"${BUFFER_NAME}\"
  (buffer-substring-no-properties
    (max (point-min) (- (point-max) ${CHAR_COUNT}))
    (point-max)))"
