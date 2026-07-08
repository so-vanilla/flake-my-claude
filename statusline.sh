#!/usr/bin/env bash
set -euo pipefail

input=$(cat)

if ! command -v jq >/dev/null 2>&1; then
  echo " Claude"
  exit 0
fi

json() {
  jq -r "$1" <<<"$input" 2>/dev/null || true
}

model=$(json '.model.display_name // .model.id // "Claude"')
dir=$(json '.workspace.current_dir // .cwd // ""')
project_dir=$(json '.workspace.project_dir // .workspace.current_dir // .cwd // ""')
session_id=$(json '.session_id // "unknown"')
pct=$(json '(.context_window.used_percentage // 0) | floor')
cost=$(json '.cost.total_cost_usd // 0')
duration_ms=$(json '.cost.total_duration_ms // 0')
five_hour=$(json '.rate_limits.five_hour.used_percentage // empty')
agent=$(json '.agent.name // empty')
pr_number=$(json '.pr.number // empty')
pr_state=$(json '.pr.review_state // empty')

[[ -n "$dir" ]] || dir="$PWD"
base="${dir##*/}"
[[ -n "$base" ]] || base="/"

# Context bar
bar_width=10
if [[ "$pct" =~ ^[0-9]+$ ]]; then
  (( pct < 0 )) && pct=0
  (( pct > 100 )) && pct=100
else
  pct=0
fi
filled=$((pct * bar_width / 100))
empty=$((bar_width - filled))
bar=""
if (( filled > 0 )); then
  printf -v fill_chars "%${filled}s"
  bar="${fill_chars// /█}"
fi
if (( empty > 0 )); then
  printf -v empty_chars "%${empty}s"
  bar="${bar}${empty_chars// /░}"
fi

# Duration
if [[ "$duration_ms" =~ ^[0-9]+$ ]]; then
  total_sec=$((duration_ms / 1000))
  mins=$((total_sec / 60))
  secs=$((total_sec % 60))
else
  mins=0
  secs=0
fi
cost_fmt=$(printf '$%.2f' "$cost" 2>/dev/null || printf '$0.00')

# Git status, cached because statusline runs often.
git_segment=""
if [[ -n "$dir" ]] && command -v git >/dev/null 2>&1 && git -C "$dir" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  cache_file="/tmp/claude-statusline-git-${session_id}"
  cache_max_age=5
  cache_stale=true
  if [[ -f "$cache_file" ]]; then
    mtime=$(stat -f %m "$cache_file" 2>/dev/null || stat -c %Y "$cache_file" 2>/dev/null || echo 0)
    now=$(date +%s)
    if (( now - mtime <= cache_max_age )); then
      cache_stale=false
    fi
  fi

  if [[ "$cache_stale" == true ]]; then
    branch=$(git -C "$dir" branch --show-current 2>/dev/null || true)
    [[ -n "$branch" ]] || branch=$(git -C "$dir" rev-parse --short HEAD 2>/dev/null || true)
    staged=$(git -C "$dir" diff --cached --numstat 2>/dev/null | wc -l | tr -d ' ')
    modified=$(git -C "$dir" diff --numstat 2>/dev/null | wc -l | tr -d ' ')
    printf '%s|%s|%s\n' "$branch" "${staged:-0}" "${modified:-0}" > "$cache_file"
  fi

  IFS='|' read -r branch staged modified < "$cache_file" || true
  if [[ -n "${branch:-}" ]]; then
    git_segment="  ${branch}"
    [[ "${staged:-0}" != "0" ]] && git_segment+=" ${staged}"
    [[ "${modified:-0}" != "0" ]] && git_segment+=" ${modified}"
  fi
fi

extra=""
[[ -n "$agent" ]] && extra+=" 󰒋 ${agent}"
if [[ -n "$pr_number" ]]; then
  extra+="  #${pr_number}"
  [[ -n "$pr_state" ]] && extra+=" ${pr_state}"
fi
if [[ -n "$five_hour" ]]; then
  limit=$(printf '%.0f' "$five_hour" 2>/dev/null || true)
  [[ -n "$limit" ]] && extra+=" 󰓅5h:${limit}%"
fi

echo "󰚩 ${model}   ${base}${git_segment}  󰅟 ${bar} ${pct}%   ${cost_fmt}   ${mins}m${secs}s${extra}"
