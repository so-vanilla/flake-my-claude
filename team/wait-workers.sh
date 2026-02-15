#!/usr/bin/env bash
# Usage: wait-workers.sh <team-id> <worker-count> [timeout-seconds]
set -euo pipefail

TEAM_ID="$1"
WORKER_COUNT="$2"
TIMEOUT="${3:-300}"
BASE_DIR="/tmp/claude-team/${TEAM_ID}"

# eatバッファが入力待ち状態かどうかを判定
is_buffer_idle() {
  local worker_num="$1"
  local buffer_name="*eat-claude-worker-${worker_num}*"
  local tail_text
  tail_text=$(emacsclient -e "(with-current-buffer \"${buffer_name}\"
    (buffer-substring-no-properties
      (max (point-min) (- (point-max) 200)) (point-max)))" 2>/dev/null || echo "")
  echo "$tail_text" | grep -qE '(> $|╰─|❯ |>\s*$)' && return 0
  return 1
}

# ワーカーの完了判定（2段階）
is_worker_done() {
  local worker_num="$1"
  local result_dir="${BASE_DIR}/worker-${worker_num}"

  if [[ -f "${result_dir}/done" ]]; then
    return 0
  fi

  if [[ -f "${result_dir}/result.md" ]] && is_buffer_idle "$worker_num"; then
    echo "Worker ${worker_num}: doneファイルなし、result.md+バッファアイドルで完了と判定" >&2
    return 0
  fi

  return 1
}

# 完了済みワーカー数を返す
check_done_count() {
  local count=0
  for i in $(seq 1 "$WORKER_COUNT"); do
    is_worker_done "$i" && count=$((count + 1))
  done
  echo "$count"
}

# 完了時の結果表示
print_results() {
  echo "全 ${WORKER_COUNT} ワーカーが完了しました。"
  for i in $(seq 1 "$WORKER_COUNT"); do
    local result="${BASE_DIR}/worker-${i}/result.md"
    if [ -f "$result" ]; then
      echo "--- Worker ${i} ---"
      head -5 "$result"
      echo "..."
    fi
  done
}

# タイムアウト時の情報表示
print_timeout() {
  echo "タイムアウト (${TIMEOUT}秒): 未完了のワーカーがあります"
  for i in $(seq 1 "$WORKER_COUNT"); do
    if ! is_worker_done "$i"; then
      echo "  未完了: Worker ${i}"
      local buffer_name="*eat-claude-worker-${i}*"
      local tail_text
      tail_text=$(emacsclient -e "(with-current-buffer \"${buffer_name}\"
        (buffer-substring-no-properties
          (max (point-min) (- (point-max) 500)) (point-max)))" 2>/dev/null || echo "(バッファ読み取り失敗)")
      echo "  バッファ末尾: ${tail_text}"
    fi
  done
}

# --- inotifywait 検出・実行 ---

resolve_inotifywait() {
  if command -v inotifywait &>/dev/null; then
    echo "direct"
  elif command -v , &>/dev/null; then
    echo "comma"
  elif command -v nix-shell &>/dev/null; then
    echo "nix-shell"
  else
    echo "none"
  fi
}

run_inotifywait() {
  local method="$1"
  shift
  case "$method" in
    direct)   inotifywait "$@" ;;
    comma)    , inotifywait "$@" ;;
    nix-shell) nix-shell -p inotify-tools --run "inotifywait $*" ;;
  esac
}

# --- inotifywait によるイベント駆動待機 ---

wait_with_inotifywait() {
  local method="$1"

  # 監視対象ディレクトリを準備
  local watch_dirs=()
  for i in $(seq 1 "$WORKER_COUNT"); do
    local dir="${BASE_DIR}/worker-${i}"
    mkdir -p "$dir"
    watch_dirs+=("$dir")
  done

  local fifo="${BASE_DIR}/.inotify-fifo"
  rm -f "$fifo"
  mkfifo "$fifo"

  # read-writeオープンでブロックを回避（readerがいないとwriterがブロックする問題の防止）
  exec 3<>"$fifo"

  # inotifywait起動（fifoに書き込み、fdが開いているので即座にモニタリング開始）
  run_inotifywait "$method" -m -e create --format '%w%f' "${watch_dirs[@]}" > "$fifo" 2>/dev/null &
  local inotify_pid=$!

  # タイムアウト用バックグラウンドプロセス
  (sleep "$TIMEOUT" && kill -TERM $$ 2>/dev/null) &
  local timeout_pid=$!

  cleanup() {
    kill "$inotify_pid" 2>/dev/null || true
    kill "$timeout_pid" 2>/dev/null || true
    exec 3<&- 2>/dev/null || true
    rm -f "$fifo"
  }
  trap cleanup EXIT

  # inotifywaitが起動済みなので、この間に作成されたdoneファイルもキューに入る
  local done_count
  done_count=$(check_done_count)
  if [[ "$done_count" -ge "$WORKER_COUNT" ]]; then
    print_results
    exit 0
  fi
  echo "完了済み: ${done_count}/${WORKER_COUNT} (inotifywait: ${method})" >&2

  # イベントループ: fd 3からinotifywaitの出力を読む
  while IFS= read -r path <&3; do
    if [[ "$path" == */done ]]; then
      # パスからワーカー番号を抽出: .../worker-N/done
      local dir_name
      dir_name=$(basename "$(dirname "$path")")
      local worker_num="${dir_name#worker-}"
      echo "Worker ${worker_num} 完了を検知" >&2

      done_count=$(check_done_count)
      if [[ "$done_count" -ge "$WORKER_COUNT" ]]; then
        print_results
        exit 0
      fi
      echo "完了: ${done_count}/${WORKER_COUNT}" >&2
    fi
  done

  # パイプ切断（inotifywait異常終了）→ 最終チェック
  done_count=$(check_done_count)
  if [[ "$done_count" -ge "$WORKER_COUNT" ]]; then
    print_results
    exit 0
  fi

  print_timeout
  exit 1
}

# --- ポーリング方式での待機（フォールバック） ---

wait_with_polling() {
  echo "inotifywait不可: 5秒ポーリングにフォールバック" >&2
  local elapsed=0
  while [ "$elapsed" -lt "$TIMEOUT" ]; do
    local done_count
    done_count=$(check_done_count)
    if [[ "$done_count" -ge "$WORKER_COUNT" ]]; then
      print_results
      exit 0
    fi
    sleep 5
    elapsed=$((elapsed + 5))
  done

  print_timeout
  exit 1
}

# --- メインロジック ---

method=$(resolve_inotifywait)
if [[ "$method" != "none" ]]; then
  wait_with_inotifywait "$method"
else
  wait_with_polling
fi
