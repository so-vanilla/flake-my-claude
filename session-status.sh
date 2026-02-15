#!/usr/bin/env bash
set -euo pipefail

# ワーカーセッションではステータス更新をスキップ
[[ "${CLAUDE_TEAM_WORKER:-}" == "1" ]] && exit 0

run_jq() {
  if command -v jq &>/dev/null; then
    jq "$@"
  else
    , jq "$@"
  fi
}

DATA_DIR="/tmp/claude-code-status"
mkdir -p "$DATA_DIR"

input=$(cat)

hook_event=$(echo "$input" | run_jq -r '.hook_event_name // empty')
# cwdを末尾スラッシュなしに正規化（Emacs側のdirectory-file-nameと一致させるため）
project_dir=$(echo "$input" | run_jq -r '.cwd // empty' | sed 's:/$::')
session_id=$(echo "$input" | run_jq -r '.session_id // empty')

if [[ -z "$project_dir" ]]; then
  exit 0
fi

# MD5ハッシュ生成
if command -v md5sum &>/dev/null; then
  hash=$(echo -n "$project_dir" | md5sum | cut -d' ' -f1)
else
  hash=$(echo -n "$project_dir" | md5 -q)
fi

case "$hook_event" in
  SessionStart|Stop)
    state="idle"
    ;;
  UserPromptSubmit)
    state="working"
    ;;
  PostToolUse|PostToolUseFailure)
    state="working"
    ;;
  Notification)
    notification_type=$(echo "$input" | run_jq -r '.notification_type // empty')
    if [[ -z "$notification_type" ]]; then
      # notification_type欠落バグ(#11964)のフォールバック
      message=$(echo "$input" | run_jq -r '.message // empty')
      case "$message" in
        *[Pp]ermission*) notification_type="permission_prompt" ;;
        *idle*|*waiting*) notification_type="idle_prompt" ;;
        *) notification_type="unknown" ;;
      esac
    fi
    case "$notification_type" in
      permission_prompt|idle_prompt|elicitation_dialog)
        state="waiting"
        ;;
      *)
        exit 0
        ;;
    esac
    ;;
  SessionEnd)
    rm -f "${DATA_DIR}/${hash}.json"
    exit 0
    ;;
  *)
    exit 0
    ;;
esac

# PostToolUse/PostToolUseFailureの最適化:
# 既にworkingなら書き込み不要（高頻度イベントのI/O削減）
if [[ "$hook_event" == PostToolUse || "$hook_event" == PostToolUseFailure ]]; then
  if [[ -f "${DATA_DIR}/${hash}.json" ]] && \
     grep -q '"state": "working"' "${DATA_DIR}/${hash}.json" 2>/dev/null; then
    exit 0
  fi
fi

# アトミック書き込み
output=$(run_jq -n \
  --arg state "$state" \
  --arg event "$hook_event" \
  --arg session_id "$session_id" \
  --arg project_dir "$project_dir" \
  '{state: $state, event: $event, session_id: $session_id, project_dir: $project_dir, timestamp: now}')

tmp_file="${DATA_DIR}/${hash}.tmp"
dest_file="${DATA_DIR}/${hash}.json"
echo "$output" > "$tmp_file"
mv "$tmp_file" "$dest_file"
