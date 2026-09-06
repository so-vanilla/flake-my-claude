#!/usr/bin/env python3
"""Explicit, narrow entry point for model-policy and Codex config fixtures.

The entry point delegates config mutation mechanics to ``CodexConfigManager``.
It has no implicit config destination: every config command requires an explicit
``--config`` path, so source verification cannot accidentally inspect a live
user config.  Plan output redacts current managed values as well as all
unmanaged content.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import sys
from pathlib import Path
from typing import Any, Mapping, Optional, Sequence


SOURCE_ROOT = Path(__file__).resolve().parent
sys.path.insert(0, str(SOURCE_ROOT / "src"))

from ai_agent_workflow.config_policy import CodexConfigManager, ConfigError  # noqa: E402


_EXPECTED_MODEL_POLICY: Mapping[str, Any] = {
    "schema": "agent-model-policy/v1",
    "default": {"model": "gpt-5.6-luna", "reasoning_effort": "max"},
    "exceptional_arbiter": {"model": "gpt-5.6-sol", "reasoning_effort": "high"},
    "automatic_fallback": False,
    "config_apply": "explicit-user-command-only",
    "runtime_compatibility": "requires-fresh-codex-validation",
    "unsupported_value_action": "stop-without-substitution",
    "switch_conditions": {
        "default": "ordinary bounded work",
        "exceptional_arbiter": (
            "recorded review-validation conflict or exhausted finite convergence only"
        ),
    },
}


def _digest(content: bytes) -> str:
    return "sha256:" + hashlib.sha256(content).hexdigest()


def effective_model_fixture_receipt(policy_path: Path) -> dict[str, Any]:
    """Validate the exact source policy and return a non-runtime fixture receipt."""

    path = Path(policy_path)
    try:
        source = path.read_bytes()
        policy = json.loads(source)
    except (OSError, ValueError) as exc:
        raise ConfigError("model policy is unreadable or invalid") from exc
    if policy != _EXPECTED_MODEL_POLICY:
        raise ConfigError(
            "model policy differs from the exact explicit source contract; "
            "stop without substitution"
        )
    default = policy["default"]
    return {
        "schema": "effective-model-fixture-receipt/v1",
        "source_digest": _digest(source),
        "model": default["model"],
        "reasoning_effort": default["reasoning_effort"],
        "automatic_fallback": False,
        "config_apply": "explicit-user-command-only",
        "unsupported_value_action": "stop-without-substitution",
        "runtime_compatibility_requirement": policy["runtime_compatibility"],
        "runtime_compatibility": "unverified",
        "fixture_only": True,
    }


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--policy", required=True, type=Path)
    parser.add_argument(
        "--config",
        type=Path,
        help="explicit config path; required for every config operation",
    )
    commands = parser.add_subparsers(dest="command", required=True)
    commands.add_parser(
        "effective-model-fixture",
        help="validate the exact source model policy; no runtime compatibility claim",
    )
    commands.add_parser("plan", help="show redacted managed-key differences")
    commands.add_parser("doctor", help="compare managed keys without changing them")
    apply = commands.add_parser("apply", help="explicitly apply a reviewed plan")
    apply.add_argument("--expected-before-digest", required=True)
    apply.add_argument("--confirm-runtime-compatible", action="store_true")
    rollback = commands.add_parser("rollback", help="restore a backup under CAS")
    source = rollback.add_mutually_exclusive_group(required=True)
    source.add_argument("--backup", type=Path)
    source.add_argument("--restore-absent", action="store_true")
    rollback.add_argument("--expected-current-digest", required=True)
    rollback.add_argument("--expected-current-identity", required=True)
    return parser


def _redacted_plan(plan: Mapping[str, Any]) -> dict[str, Any]:
    changes = {
        key: {
            "current": "<redacted>" if change.get("current") is not None else None,
            "desired": change["desired"],
        }
        for key, change in plan["changes"].items()
    }
    return {
        "schema": plan["schema"],
        "path": "<explicit-config-path>",
        "exists": plan["exists"],
        "before_digest": plan["before_digest"],
        "after_digest": plan["after_digest"],
        "changed": plan["changed"],
        "changed_keys": plan["changed_keys"],
        "changes": changes,
        "runtime_compatibility": plan["runtime_compatibility"],
        "automatic_fallback": plan["automatic_fallback"],
    }


def main(argv: Optional[Sequence[str]] = None) -> int:
    args = _parser().parse_args(list(argv) if argv is not None else None)
    try:
        if args.command == "effective-model-fixture":
            result = effective_model_fixture_receipt(args.policy)
        else:
            if args.config is None:
                raise ConfigError(
                    "an explicit --config path is required; no live default is used"
                )
            effective_model_fixture_receipt(args.policy)
            manager = CodexConfigManager(args.config, args.policy)
            if args.command == "plan":
                result = _redacted_plan(manager.plan())
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
