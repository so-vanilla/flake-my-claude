#!/usr/bin/env bash
# Usage: setup-env.sh <subcommand> <working-dir>
# Subcommands:
#   setup <dir> - devenv初期化（既存ファイル削除 + devenv init）
#   check <dir> - 環境状態チェック（devenv.nix存在確認 + devenv build）
set -euo pipefail

SUBCMD="${1:-}"
WORK_DIR="${2:-}"

if [[ -z "$SUBCMD" || -z "$WORK_DIR" ]]; then
  echo "Usage: setup-env.sh <setup|check> <working-dir>" >&2
  exit 1
fi

case "$SUBCMD" in
  setup)
    cd "$WORK_DIR"
    # 既存devenv環境のチェック
    if [[ -f devenv.nix ]]; then
      echo "devenv.nix が既に存在します。既存の環境を使用します。" >&2
      echo "再初期化するには先に devenv.nix を削除してください。" >&2
      exit 0
    fi
    # 既存devenvファイルの削除（devenv.nixがない場合のゴミ掃除）
    rm -f devenv.yaml devenv.lock .devenv.flake.nix
    rm -rf .devenv
    # devenv init（非対話）
    echo "" | devenv init
    echo "devenv init完了。devenv.nixを編集してください。"
    ;;
  check)
    cd "$WORK_DIR"
    echo "=== devenv環境チェック ==="
    # devenv.nixの存在確認
    if [[ -f devenv.nix ]]; then
      echo "✓ devenv.nix exists"
    else
      echo "✗ devenv.nix missing"
      exit 1
    fi
    # devenv buildの実行
    if devenv build; then
      echo "✓ devenv build success"
    else
      echo "✗ devenv build failed"
      exit 1
    fi
    ;;
  *)
    echo "Error: unknown subcommand '$SUBCMD'. Use 'setup' or 'check'." >&2
    exit 1
    ;;
esac
