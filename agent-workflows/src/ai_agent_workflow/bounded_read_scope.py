"""Digest-bound read-scope contracts for one Workflow execution attempt.

This module validates declared and *observed* workspace-relative paths.  It is
deliberately not an operating-system sandbox: a passing receipt proves that the
supplied complete observation stayed inside the declared scope, but cannot
prove that an uninstrumented process performed no other reads.
"""
from __future__ import annotations

import copy
import hashlib
import json
import posixpath
from collections.abc import Mapping, Sequence
from pathlib import Path
from typing import Any


_LIMITATION = (
    "contract and complete observed-path validation only; operating-system "
    "read isolation is not enforced"
)


class ReadScopeError(ValueError):
    """A read-scope declaration or observation is ambiguous or unbound."""


def _digest(value: Mapping[str, Any]) -> str:
    raw = json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=False).encode("utf-8")
    return "sha256:" + hashlib.sha256(raw).hexdigest()


def _paths(value: Any, label: str) -> list[str]:
    if not isinstance(value, Sequence) or isinstance(value, (str, bytes)):
        raise ReadScopeError(label + " must be a path list")
    result = []
    for item in value:
        if (not isinstance(item, str) or not item or item.startswith("/")
                or item in {".", ".."} or ".." in item.split("/")
                or posixpath.normpath(item) != item or "//" in item):
            raise ReadScopeError(label + " contains a non-canonical workspace-relative path")
        result.append(item)
    if len(result) != len(set(result)):
        raise ReadScopeError(label + " contains duplicates")
    return sorted(result)


def compile_read_scope(workspace_identity: str, declared_paths: Sequence[str]) -> dict[str, Any]:
    """Freeze the exact read roots released to one worker.

    The false OS-isolation field is normative.  A launcher that adds a real OS
    sandbox needs a new versioned contract and separate evidence rather than
    silently upgrading this claim.
    """
    if (not isinstance(workspace_identity, str) or not workspace_identity.startswith("/")
            or workspace_identity == "/" or posixpath.normpath(workspace_identity) != workspace_identity):
        raise ReadScopeError("workspace_identity must be one canonical absolute non-root path")
    paths = _paths(declared_paths, "declared_paths")
    if not paths:
        raise ReadScopeError("declared_paths must not be empty")
    scope = {
        "schema": "bounded-read-scope/v1",
        "workspace_identity": workspace_identity,
        "declared_paths": paths,
        "enforcement": "declared-contract-plus-complete-observation",
        "os_isolation_enforced": False,
        "limitation": _LIMITATION,
    }
    scope["scope_digest"] = _digest(scope)
    return scope


def _validate_scope(value: Any) -> dict[str, Any]:
    keys = {
        "schema", "workspace_identity", "declared_paths", "enforcement",
        "os_isolation_enforced", "limitation", "scope_digest",
    }
    if not isinstance(value, Mapping) or set(value) != keys:
        raise ReadScopeError("read scope shape is invalid")
    rebuilt = compile_read_scope(value["workspace_identity"], value["declared_paths"])
    if rebuilt != value:
        raise ReadScopeError("read scope digest or limitation claim drifted")
    return rebuilt


def _within(path: str, roots: Sequence[str]) -> bool:
    return any(path == root or path.startswith(root + "/") for root in roots)


def observe_read_scope(
    scope: Mapping[str, Any], observed_paths: Sequence[str], *, observation_complete: bool
) -> dict[str, Any]:
    """Create a non-OS receipt for a complete instrumented read observation."""
    frozen = _validate_scope(scope)
    observed = _paths(observed_paths, "observed_paths")
    if observation_complete is not True:
        raise ReadScopeError("a partial read observation cannot close execution")
    if any(not _within(path, frozen["declared_paths"]) for path in observed):
        raise ReadScopeError("observed read escaped the declared scope")
    receipt = {
        "schema": "bounded-read-scope-receipt/v1",
        "scope_digest": frozen["scope_digest"],
        "observed_paths": observed,
        "observation_complete": True,
        "status": "passed",
        "os_isolation_enforced": False,
        "limitation": _LIMITATION,
    }
    receipt["receipt_digest"] = _digest(receipt)
    return receipt


def verify_read_scope_receipt(scope: Mapping[str, Any], receipt: Mapping[str, Any]) -> dict[str, Any]:
    """Verify that an immutable receipt closes the supplied exact scope."""
    frozen = _validate_scope(scope)
    keys = {
        "schema", "scope_digest", "observed_paths", "observation_complete",
        "status", "os_isolation_enforced", "limitation", "receipt_digest",
    }
    if not isinstance(receipt, Mapping) or set(receipt) != keys:
        raise ReadScopeError("read scope receipt shape is invalid")
    unsigned = {key: copy.deepcopy(item) for key, item in receipt.items() if key != "receipt_digest"}
    if (receipt.get("schema") != "bounded-read-scope-receipt/v1"
            or receipt.get("scope_digest") != frozen["scope_digest"]
            or receipt.get("observation_complete") is not True
            or receipt.get("status") != "passed"
            or receipt.get("os_isolation_enforced") is not False
            or receipt.get("limitation") != _LIMITATION
            or receipt.get("receipt_digest") != _digest(unsigned)):
        raise ReadScopeError("read scope receipt does not bind a complete non-OS observation")
    observed = _paths(receipt["observed_paths"], "observed_paths")
    if observed != receipt["observed_paths"] or any(not _within(path, frozen["declared_paths"]) for path in observed):
        raise ReadScopeError("read scope receipt contains an undeclared or non-canonical read")
    return copy.deepcopy(dict(receipt))


def compile_macos_forbidden_read_profile(forbidden_roots: Sequence[str | Path]) -> str:
    """Build a macOS test profile that denies data reads from known siblings.

    This is a negative fixture guard, not a portable positive allow-list.  It
    is useful when disposable projects share a parent: every sibling root can
    be denied after resolving macOS' ``/tmp`` alias to ``/private/tmp``.
    """
    if not isinstance(forbidden_roots, Sequence) or isinstance(forbidden_roots, (str, bytes)):
        raise ReadScopeError("forbidden_roots must be a path list")
    canonical = []
    for item in forbidden_roots:
        try:
            path = Path(item).resolve(strict=True)
        except (OSError, TypeError) as error:
            raise ReadScopeError("forbidden root must physically exist") from error
        if str(path) == "/" or not path.is_dir():
            raise ReadScopeError("forbidden root must be one non-root directory")
        canonical.append(str(path))
    if not canonical or len(canonical) != len(set(canonical)):
        raise ReadScopeError("forbidden roots must be non-empty and unique")
    clauses = ["(version 1)", "(allow default)"]
    clauses.extend(
        "(deny file-read-data (subpath %s))" % json.dumps(path)
        for path in sorted(canonical)
    )
    return "\n".join(clauses) + "\n"


__all__ = [
    "ReadScopeError", "compile_read_scope", "observe_read_scope",
    "verify_read_scope_receipt", "compile_macos_forbidden_read_profile",
]
