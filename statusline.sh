#!/usr/bin/env bash
# statusline.sh - Claude Code statusline handler
# stdinでセッションJSONを受け取り、中間ファイルに書き出す。
# stdoutは空にしてstatusline UIを非表示にする。

set -euo pipefail

# jqの存在確認。なければcommaで実行を試みる
run_jq() {
  if command -v jq &>/dev/null; then
    jq "$@"
  else
    , jq "$@"
  fi
}

DATA_DIR="/tmp/claude-code"
mkdir -p "$DATA_DIR"

input=$(cat)

# project_dirからMD5ハッシュを生成してファイル名にする
project_dir=$(echo "$input" | run_jq -r '.workspace.project_dir // empty')
if [[ -z "$project_dir" ]]; then
  exit 0
fi

if command -v md5sum &>/dev/null; then
  hash=$(echo -n "$project_dir" | md5sum | cut -d' ' -f1)
else
  hash=$(echo -n "$project_dir" | md5 -q)
fi

# 必要なフィールドを抽出して書き出すJSONを構築
output=$(echo "$input" | run_jq '{
  model: .model.display_name,
  model_id: .model.id,
  context_used: (.context_window.used_percentage // 0),
  context_remaining: (.context_window.remaining_percentage // 100),
  cost_usd: (.cost.total_cost_usd // 0),
  duration_ms: (.cost.total_duration_ms // 0),
  session_id: .session_id,
  project_dir: .workspace.project_dir,
  current_dir: .workspace.current_dir,
  version: .version,
  timestamp: now
}')

# アトミック書き込み
tmp_file="${DATA_DIR}/${hash}.tmp"
dest_file="${DATA_DIR}/${hash}.json"
echo "$output" > "$tmp_file"
mv "$tmp_file" "$dest_file"
