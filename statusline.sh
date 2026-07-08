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

percent_floor() {
  local value="$1"
  if [[ "$value" =~ ^[0-9]+([.][0-9]+)?$ ]]; then
    printf '%.0f' "$value" 2>/dev/null || echo ""
  else
    echo ""
  fi
}

clamp_percent() {
  local value="$1"
  if [[ -z "$value" ]]; then
    echo ""
    return 0
  fi
  if (( value < 0 )); then
    echo 0
  elif (( value > 100 )); then
    echo 100
  else
    echo "$value"
  fi
}

progress_bar() {
  local raw="$1"
  local width="${2:-10}"
  local pct
  pct=$(percent_floor "$raw")
  pct=$(clamp_percent "$pct")

  if [[ -z "$pct" ]]; then
    printf '%*s --%%' "$width" '' | tr ' ' '░'
    return 0
  fi

  local filled=$((pct * width / 100))
  local empty=$((width - filled))
  local bar=""

  if (( filled > 0 )); then
    local fill_chars
    printf -v fill_chars "%${filled}s"
    bar="${fill_chars// /█}"
  fi
  if (( empty > 0 )); then
    local empty_chars
    printf -v empty_chars "%${empty}s"
    bar="${bar}${empty_chars// /░}"
  fi

  printf '%s %s%%' "$bar" "$pct"
}

model=$(json '.model.display_name // .model.id // "Claude"')
dir=$(json '.workspace.current_dir // .cwd // ""')
session_id=$(json '.session_id // "unknown"')
context_pct=$(json '.context_window.used_percentage // empty')
five_hour_pct=$(json '.rate_limits.five_hour.used_percentage // empty')
seven_day_pct=$(json '.rate_limits.seven_day.used_percentage // empty')
agent=$(json '.agent.name // empty')
pr_number=$(json '.pr.number // empty')
pr_state=$(json '.pr.review_state // empty')

[[ -n "$dir" ]] || dir="$PWD"
base="${dir##*/}"
[[ -n "$base" ]] || base="/"

context_bar=$(progress_bar "$context_pct" 10)
five_hour_bar=$(progress_bar "$five_hour_pct" 10)
seven_day_bar=$(progress_bar "$seven_day_pct" 10)

# Git status is cached because the statusline runs often.
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

echo "󰚩 ${model}   ${base}${git_segment}  󰅟 ctx:${context_bar}  󰥔 5h:${five_hour_bar}  󰃭 7d:${seven_day_bar}${extra}"
