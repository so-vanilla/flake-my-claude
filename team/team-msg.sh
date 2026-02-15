#!/usr/bin/env bash
# Usage: team-msg.sh <team-id> <from-worker> <to-worker|broadcast> "<message>"
# ワーカー間メッセージング（ユニキャスト + ブロードキャスト）
set -euo pipefail

TEAM_ID="$1"
FROM="$2"
TO="$3"
MESSAGE="$4"

TEAM_DIR="/tmp/claude-team/${TEAM_ID}"
INFO_FILE="${TEAM_DIR}/team-info.txt"
MSG_DIR="${TEAM_DIR}/messages"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# バリデーション: チームディレクトリ存在
if [[ ! -d "$TEAM_DIR" ]]; then
  echo "Error: Team '${TEAM_ID}' が存在しません" >&2
  exit 1
fi

# バリデーション: team-info.txt存在
if [[ ! -f "$INFO_FILE" ]]; then
  echo "Error: ${INFO_FILE} が見つかりません" >&2
  exit 1
fi

# ワーカー数を動的取得
WORKER_COUNT=$(grep '^WORKER_COUNT=' "$INFO_FILE" | cut -d= -f2)
if [[ -z "$WORKER_COUNT" ]]; then
  echo "Error: WORKER_COUNT が team-info.txt に定義されていません" >&2
  exit 1
fi

# バリデーション: from番号
if [[ ! "$FROM" =~ ^[0-9]+$ ]] || (( FROM < 1 || FROM > WORKER_COUNT )); then
  echo "Error: from-worker は 1〜${WORKER_COUNT} の範囲で指定してください" >&2
  exit 1
fi

# バリデーション: to番号（broadcastまたは数値）
if [[ "$TO" != "broadcast" ]]; then
  if [[ ! "$TO" =~ ^[0-9]+$ ]] || (( TO < 1 || TO > WORKER_COUNT )); then
    echo "Error: to-worker は 1〜${WORKER_COUNT} または 'broadcast' で指定してください" >&2
    exit 1
  fi
  # 自己送信防止
  if [[ "$FROM" == "$TO" ]]; then
    echo "Error: 自分自身にメッセージを送信できません" >&2
    exit 1
  fi
fi

# messagesディレクトリ確保
mkdir -p "$MSG_DIR"

# タイムスタンプ
TIMESTAMP=$(date '+%Y-%m-%d %H:%M:%S')

# メッセージ送信関数
send_to_worker() {
  local target="$1"
  local formatted="$2"
  if "${SCRIPT_DIR}/send-message.sh" "$target" "$formatted"; then
    echo "Sent to Worker ${target}"
  else
    echo "WARNING: Worker ${target} への送信に失敗しました" >&2
  fi
}

if [[ "$TO" == "broadcast" ]]; then
  FORMATTED="[Worker ${FROM} → All] ${MESSAGE}"
  echo "[${TIMESTAMP}] ${FORMATTED}" >> "${MSG_DIR}/log.txt"

  for (( i=1; i<=WORKER_COUNT; i++ )); do
    [[ "$i" -eq "$FROM" ]] && continue
    send_to_worker "$i" "$FORMATTED"
  done
else
  FORMATTED="[Worker ${FROM} → Worker ${TO}] ${MESSAGE}"
  echo "[${TIMESTAMP}] ${FORMATTED}" >> "${MSG_DIR}/log.txt"
  send_to_worker "$TO" "$FORMATTED"
fi
