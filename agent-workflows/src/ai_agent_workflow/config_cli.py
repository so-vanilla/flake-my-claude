"""Operator-invoked plan/apply/doctor/rollback for portable Codex model keys."""

from __future__ import annotations

import argparse
import json
import os
import sys
from pathlib import Path
from typing import List, Optional

from .config_policy import CodexConfigManager, ConfigError


def _default_config() -> Path:
    codex_root = os.environ.get("CODEX_HOME")
    return Path(codex_root) / "config.toml" if codex_root else Path.home() / ".codex" / "config.toml"


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--policy", required=True, type=Path)
    parser.add_argument("--config", type=Path, default=_default_config())
    commands = parser.add_subparsers(dest="command", required=True)
    commands.add_parser("plan", help="show only managed-key differences; write nothing")
    commands.add_parser("doctor", help="compare managed keys without changing them")
    apply = commands.add_parser("apply", help="explicitly apply a previously reviewed plan")
    apply.add_argument("--expected-before-digest", required=True)
    apply.add_argument("--confirm-runtime-compatible", action="store_true")
    rollback = commands.add_parser("rollback", help="restore one timestamped backup under CAS")
    rollback_source = rollback.add_mutually_exclusive_group(required=True)
    rollback_source.add_argument("--backup", type=Path)
    rollback_source.add_argument(
        "--restore-absent",
        action="store_true",
        help="restore an absent pre-state after an initial config creation",
    )
    rollback.add_argument("--expected-current-digest", required=True)
    rollback.add_argument("--expected-current-identity", required=True)
    return parser


def main(argv: Optional[List[str]] = None) -> int:
    args = _parser().parse_args(argv)
    manager = CodexConfigManager(args.config, args.policy)
    try:
        if args.command == "plan":
            result = manager.plan()
        elif args.command == "doctor":
            result = manager.doctor()
        elif args.command == "apply":
            result = manager.apply(
                expected_before_digest=args.expected_before_digest,
                runtime_compatibility_confirmed=args.confirm_runtime_compatible,
            )
        elif args.restore_absent:
            result = manager.rollback_absent(
                expected_current_digest=args.expected_current_digest,
                expected_current_identity=args.expected_current_identity,
            )
        else:
            result = manager.rollback(
                args.backup,
                expected_current_digest=args.expected_current_digest,
                expected_current_identity=args.expected_current_identity,
            )
        print(json.dumps(result, ensure_ascii=False, sort_keys=True, indent=2))
        return 0
    except (ConfigError, OSError, UnicodeError, ValueError) as exc:
        print("error: %s" % exc, file=sys.stderr)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
