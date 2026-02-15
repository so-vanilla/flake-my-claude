#!/usr/bin/env bash
# log-permission-request.sh - PermissionRequestフックからのログ記録
# stdin JSONを受け取り、タイムスタンプ付きでJSONLにappend
# O_APPENDによりPIPE_BUF以下の書き込みは原子的（並列セッション安全）
set -euo pipefail

# ワーカーセッションではログ記録をスキップ
[[ "${CLAUDE_TEAM_WORKER:-}" == "1" ]] && exit 0

run_jq() {
  if command -v jq &>/dev/null; then
    jq "$@"
  else
    , jq "$@"
  fi
}

LOG_DIR="$HOME/.claude/permission-logs"
LOG_FILE="$LOG_DIR/requests.jsonl"
mkdir -p "$LOG_DIR"

input=$(cat)

# 必須フィールドの抽出
tool_name=$(echo "$input" | run_jq -r '.tool_name // empty')
if [[ -z "$tool_name" ]]; then
  exit 0
fi

# タイムスタンプ付きログエントリを生成してappend
echo "$input" | run_jq -c '{
  timestamp: now | todate,
  session_id: (.session_id // "unknown"),
  cwd: (.cwd // "unknown"),
  tool_name: .tool_name,
  tool_input: (.tool_input // {}),
  permission_mode: (.permission_mode // "unknown")
}' >> "$LOG_FILE"
