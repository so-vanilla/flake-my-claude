#!/usr/bin/env bash
# setup.sh - 会社macOS環境向けClaude Code設定セットアップ
# settings.jsonはスマートマージ: language/statusLine/permissionsはリポジトリ、
# enabledPluginsは既存を維持、その他の既存フィールドも保持

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
CLAUDE_DIR="$HOME/.claude"
BACKUP_SUFFIX=".backup.$(date +%Y%m%d%H%M%S)"

# jqの存在確認。なければnix-shellで実行を試みる
run_jq() {
  if command -v jq &>/dev/null; then
    jq "$@"
  elif command -v nix-shell &>/dev/null; then
    nix-shell -p jq --run "jq $(printf '%q ' "$@")"
  else
    echo "Error: jq が見つかりません。jq をインストールするか、nix をインストールしてください。" >&2
    exit 1
  fi
}

# コピー先が書き込み不可でも上書きできるようにする
safe_cp() {
  local src="$1" dest="$2"
  if [[ -f "$dest" && ! -w "$dest" ]]; then
    chmod u+w "$dest"
  fi
  cp "$src" "$dest"
}

# ~/.claude/ の存在確認
if [[ ! -d "$CLAUDE_DIR" ]]; then
  echo "~/.claude/ が存在しません。Claude Codeを一度起動してから再実行してください。"
  exit 1
fi

echo "=== Claude Code 設定セットアップ ==="
echo ""

# --- settings.json ---
echo "[1/7] settings.json"
DEST_SETTINGS="$CLAUDE_DIR/settings.json"
SRC_SETTINGS="$SCRIPT_DIR/settings.json"

if [[ -f "$DEST_SETTINGS" ]]; then
  # バックアップ
  cp "$DEST_SETTINGS" "${DEST_SETTINGS}${BACKUP_SUFFIX}"
  echo "  バックアップ: ${DEST_SETTINGS}${BACKUP_SUFFIX}"

  # マージ:
  # - language, statusLine, permissions, hooks, sandbox, env, enabledPlugins → リポジトリのもので完全上書き
  # - その他の既存フィールド → 全て残す
  run_jq -s '
    .[0] as $existing |
    .[1] as $repo |
    ($existing * $repo) |
    .permissions = $repo.permissions
  ' "$DEST_SETTINGS" "$SRC_SETTINGS" > "${DEST_SETTINGS}.tmp"
  mv "${DEST_SETTINGS}.tmp" "$DEST_SETTINGS"
  echo "  マージ完了: $DEST_SETTINGS"
else
  cp "$SRC_SETTINGS" "$DEST_SETTINGS"
  echo "  配置完了: $DEST_SETTINGS"
fi

# --- commands/ ---
echo "[2/7] commands/"
DEST_COMMANDS="$CLAUDE_DIR/commands"

if [[ -d "$DEST_COMMANDS" ]]; then
  # バックアップ
  cp -r "$DEST_COMMANDS" "${DEST_COMMANDS}${BACKUP_SUFFIX}"
  echo "  バックアップ: ${DEST_COMMANDS}${BACKUP_SUFFIX}"
  rm -rf "$DEST_COMMANDS"
fi

cp -r "$SCRIPT_DIR/commands" "$DEST_COMMANDS"
echo "  配置完了: $DEST_COMMANDS"

# --- CLAUDE.md ---
echo "[3/7] CLAUDE.md"
DEST_CLAUDE="$CLAUDE_DIR/CLAUDE.md"
SRC_CLAUDE="$SCRIPT_DIR/CLAUDE.md"

if [[ -L "$DEST_CLAUDE" ]]; then
  echo "  symlinkが検出されました。スキップします（Home Manager管理の可能性）"
elif [[ -f "$DEST_CLAUDE" ]]; then
  cp "$DEST_CLAUDE" "${DEST_CLAUDE}${BACKUP_SUFFIX}"
  echo "  バックアップ: ${DEST_CLAUDE}${BACKUP_SUFFIX}"
  safe_cp "$SRC_CLAUDE" "$DEST_CLAUDE"
  echo "  配置完了: $DEST_CLAUDE"
else
  cp "$SRC_CLAUDE" "$DEST_CLAUDE"
  echo "  配置完了: $DEST_CLAUDE"
fi

# --- statusline.sh ---
echo "[4/7] statusline.sh"
DEST_STATUSLINE="$CLAUDE_DIR/statusline.sh"
SRC_STATUSLINE="$SCRIPT_DIR/statusline.sh"

if [[ -L "$DEST_STATUSLINE" ]]; then
  echo "  symlinkが検出されました。スキップします（Home Manager管理の可能性）"
elif [[ -f "$DEST_STATUSLINE" ]]; then
  cp "$DEST_STATUSLINE" "${DEST_STATUSLINE}${BACKUP_SUFFIX}"
  echo "  バックアップ: ${DEST_STATUSLINE}${BACKUP_SUFFIX}"
  safe_cp "$SRC_STATUSLINE" "$DEST_STATUSLINE"
  chmod +x "$DEST_STATUSLINE"
  echo "  配置完了: $DEST_STATUSLINE"
else
  cp "$SRC_STATUSLINE" "$DEST_STATUSLINE"
  chmod +x "$DEST_STATUSLINE"
  echo "  配置完了: $DEST_STATUSLINE"
fi

# --- session-status.sh ---
echo "[5/7] session-status.sh"
DEST_SESSION_STATUS="$CLAUDE_DIR/session-status.sh"
SRC_SESSION_STATUS="$SCRIPT_DIR/session-status.sh"

if [[ -L "$DEST_SESSION_STATUS" ]]; then
  echo "  symlinkが検出されました。スキップします（Home Manager管理の可能性）"
elif [[ -f "$DEST_SESSION_STATUS" ]]; then
  cp "$DEST_SESSION_STATUS" "${DEST_SESSION_STATUS}${BACKUP_SUFFIX}"
  echo "  バックアップ: ${DEST_SESSION_STATUS}${BACKUP_SUFFIX}"
  safe_cp "$SRC_SESSION_STATUS" "$DEST_SESSION_STATUS"
  chmod +x "$DEST_SESSION_STATUS"
  echo "  配置完了: $DEST_SESSION_STATUS"
else
  cp "$SRC_SESSION_STATUS" "$DEST_SESSION_STATUS"
  chmod +x "$DEST_SESSION_STATUS"
  echo "  配置完了: $DEST_SESSION_STATUS"
fi

# --- log-permission-request.sh ---
echo "[6/7] log-permission-request.sh"
DEST_LOG_PERM="$CLAUDE_DIR/log-permission-request.sh"
SRC_LOG_PERM="$SCRIPT_DIR/log-permission-request.sh"

if [[ -L "$DEST_LOG_PERM" ]]; then
  echo "  symlinkが検出されました。スキップします（Home Manager管理の可能性）"
elif [[ -f "$DEST_LOG_PERM" ]]; then
  cp "$DEST_LOG_PERM" "${DEST_LOG_PERM}${BACKUP_SUFFIX}"
  echo "  バックアップ: ${DEST_LOG_PERM}${BACKUP_SUFFIX}"
  safe_cp "$SRC_LOG_PERM" "$DEST_LOG_PERM"
  chmod +x "$DEST_LOG_PERM"
  echo "  配置完了: $DEST_LOG_PERM"
else
  cp "$SRC_LOG_PERM" "$DEST_LOG_PERM"
  chmod +x "$DEST_LOG_PERM"
  echo "  配置完了: $DEST_LOG_PERM"
fi

# --- bin/ ---
echo "[7/7] bin/"
DEST_BIN="$CLAUDE_DIR/bin"
mkdir -p "$DEST_BIN"

for src_bin in "$SCRIPT_DIR/bin"/*; do
  [[ -f "$src_bin" ]] || continue
  name="$(basename "$src_bin")"
  safe_cp "$src_bin" "$DEST_BIN/$name"
  chmod +x "$DEST_BIN/$name"
done
echo "  配置完了: $DEST_BIN"

echo ""
echo "=== セットアップ完了 ==="
echo ""
echo "Bedrock利用の場合は settings.json にenvフィールドを手動で追加してください:"
echo '  "env": {'
echo '    "CLAUDE_USE_BEDROCK": "1",'
echo '    "AWS_PROFILE": "<your-profile>"'
echo '  }'
