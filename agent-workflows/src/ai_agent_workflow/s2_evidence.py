"""Fail-closed candidate evaluator for the future S2 source close graph.

This module deliberately has no checked-in authority digest.  S2-E's separate
Anchor is the only owner permitted to add one; until then fixed replay is false.
"""

from __future__ import annotations

import copy
import hashlib
import json
import re
from collections.abc import Mapping
from pathlib import Path
from typing import Any


_IDS = frozenset([*(f"group.B.B{i}" for i in range(1, 8)), *(f"group.C.C{i}" for i in range(1, 7))])
_DIGEST = re.compile(r"sha256:[0-9a-f]{64}\Z")
_SELECTOR = re.compile(r"[^\s][^\r\n]*\Z")
_ACCEPTANCE_KEYS = frozenset(("schema", "contract_id", "contract_name", "implementation_kind", "implementation_ref", "test_ref", "status"))
_IMPLEMENTATION_KINDS = frozenset(("skill", "shared-protocol-operation", "workflow-profile-step", "composed-on-demand-operation"))
_ROTATION = "sha256:8eeb198dd2a3592733c4e028767ee9d07450ea9a2f3e5df3323260d4268a6fe2"
_LIVE_FALSE = frozenset((
    "current_run_objective_approved", "actual_a7_handoff_complete",
    "migration_complete", "activation_complete", "source_wide_integration_complete",
    "full_workflow_ready",
))
_ZERO = frozenset(("required", "needs_user", "incomplete", "unknown", "integrity", "stopped"))


class S2EvidenceError(ValueError):
    """The proposed S2 evidence is not physically bound and candidate-safe."""


def _no_duplicate(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
    result: dict[str, Any] = {}
    for key, value in pairs:
        if key in result:
            raise S2EvidenceError("duplicate JSON member")
        result[key] = value
    return result


def _file(root: Path, relative: object) -> Path:
    if root.is_symlink() or not isinstance(relative, str) or not relative or "\\" in relative:
        raise S2EvidenceError("caller root or physical path is invalid")
    if relative.startswith(".local/") or re.search(r"(^/|//|(^|/)\.{1,2}(/|$)|/$)", relative):
        raise S2EvidenceError("ambient Run or non-normalized path is not evidence")
    candidate = Path(relative)
    raw = root / candidate
    component = root
    for part in candidate.parts:
        component /= part
        if component.is_symlink():
            raise S2EvidenceError("symlinked evidence is not admissible")
    try:
        resolved = raw.resolve(strict=True)
        resolved.relative_to(root.resolve(strict=True))
    except (OSError, ValueError) as exc:
        raise S2EvidenceError("evidence escapes caller root") from exc
    if not resolved.is_file() or resolved.is_symlink():
        raise S2EvidenceError("evidence is not a regular file")
    return resolved


def _load(root: Path, ref: object, label: str) -> Mapping[str, Any]:
    if not isinstance(ref, Mapping) or set(ref) != {"path", "digest"} or not _DIGEST.fullmatch(str(ref.get("digest"))):
        raise S2EvidenceError(label + " ref is not exact")
    raw = _file(root, ref["path"]).read_bytes()
    if "sha256:" + hashlib.sha256(raw).hexdigest() != ref["digest"]:
        raise S2EvidenceError(label + " digest does not bind bytes")
    try:
        document = json.loads(raw.decode("utf-8"), object_pairs_hook=_no_duplicate)
    except (UnicodeDecodeError, json.JSONDecodeError, S2EvidenceError) as exc:
        raise S2EvidenceError(label + " is not strict JSON") from exc
    if not isinstance(document, Mapping):
        raise S2EvidenceError(label + " must be an object")
    return document


class S2EvidenceCompiler:
    """Compile independently supplied physical inputs into an unaccepted candidate."""

    def compile(self, root: Path, accepted_refs: Mapping[str, Any], receipts: Mapping[str, Any], expected_counts: Mapping[str, Any]) -> dict[str, Any]:
        root = Path(root)
        if root.is_symlink() or not root.is_dir() or not isinstance(accepted_refs, Mapping):
            raise S2EvidenceError("caller root and accepted ref map are required")
        if set(accepted_refs) != _IDS:
            raise S2EvidenceError("exactly the 13 B/C qualified IDs are required")
        paths: set[str] = set()
        digests: set[str] = set()
        selectors: set[str] = set()
        bound_refs: dict[str, dict[str, str]] = {}
        for qualified_id in sorted(_IDS):
            ref = accepted_refs[qualified_id]
            if not isinstance(ref, Mapping) or set(ref) != {"path", "digest", "selector"}:
                raise S2EvidenceError("qualified ref is not exact")
            selector = ref["selector"]
            if not isinstance(selector, str) or not _SELECTOR.fullmatch(selector):
                raise S2EvidenceError("acceptance selector is invalid")
            document = _load(root, {"path": ref["path"], "digest": ref["digest"]}, qualified_id)
            test_ref = document.get("test_ref")
            implementation_ref = document.get("implementation_ref")
            if (
                set(document) != _ACCEPTANCE_KEYS
                or document.get("schema") != "qualified-contract-acceptance/v1"
                or document.get("contract_id") != qualified_id
                or not isinstance(document.get("contract_name"), str)
                or not document["contract_name"]
                or document.get("implementation_kind") not in _IMPLEMENTATION_KINDS
                or not isinstance(implementation_ref, Mapping)
                or set(implementation_ref) != {"path", "digest"}
                or not isinstance(implementation_ref.get("path"), str)
                or not implementation_ref["path"]
                or not _DIGEST.fullmatch(str(implementation_ref.get("digest")))
                or not isinstance(test_ref, Mapping)
                or set(test_ref) != {"selector"}
                or test_ref.get("selector") != selector
                or document.get("status") != "passed"
            ):
                raise S2EvidenceError("physical qualified acceptance does not bind")
            if ref["path"] in paths or ref["digest"] in digests or selector in selectors:
                raise S2EvidenceError("physical path, digest, and selector must be unique")
            paths.add(ref["path"]); digests.add(ref["digest"]); selectors.add(selector)
            bound_refs[qualified_id] = dict(ref)

        if not isinstance(receipts, Mapping) or set(receipts) != {"s0_authority", "s1_authority", "rotation_receipt", "closure", "counts_receipt"}:
            raise S2EvidenceError("accepted predecessors, closure, and count receipt are exact")
        for name, path in (("s0_authority", "agent-workflows/manifests/s0-source-transition-current-authority.json"), ("s1_authority", "agent-workflows/manifests/s1-source-authority.json")):
            value = receipts[name]
            if not isinstance(value, Mapping) or set(value) != {"path", "digest"} or value.get("path") != path or not _DIGEST.fullmatch(str(value.get("digest"))):
                raise S2EvidenceError("predecessor authority is invalid")
        rotation = receipts["rotation_receipt"]
        if not isinstance(rotation, Mapping) or rotation != {"receipt_id": "S2-U-R-U-001", "digest": _ROTATION}:
            raise S2EvidenceError("R-U receipt is invalid")
        closure = receipts["closure"]
        if not isinstance(closure, Mapping) or set(closure) != _ZERO or any(closure[key] != 0 for key in _ZERO):
            raise S2EvidenceError("open closure state prevents a candidate")
        count_document = _load(root, receipts["counts_receipt"], "count receipt")
        if count_document != expected_counts:
            raise S2EvidenceError("count literals lack a matching physical receipt")
        if not isinstance(expected_counts, Mapping) or set(expected_counts) != {"named_contracts", "profile_steps", "claim_vector"}:
            raise S2EvidenceError("expected counts are not exact")
        if expected_counts.get("named_contracts") != {"accepted": 29, "total": 60} or expected_counts.get("profile_steps") != {"accepted": 0, "total": 23}:
            raise S2EvidenceError("S2 counts are inflated or incomplete")
        claims = expected_counts.get("claim_vector")
        if not isinstance(claims, Mapping) or set(claims) != _LIVE_FALSE or any(claims[name] is not False for name in _LIVE_FALSE):
            raise S2EvidenceError("live completion claims must remain false")
        if any(ref["path"] == "agent-workflows/manifests/s2-source-authority.json" or ref["path"].endswith("s2_evidence.py") for ref in bound_refs.values()):
            raise S2EvidenceError("candidate may not self-accept or bind its verifier")
        return {"candidate": True, "section_accepted": False, "fixed_anchor": False, "accepted_refs": copy.deepcopy(bound_refs), "expected_counts": copy.deepcopy(dict(expected_counts))}


def verify_s2_source_evidence(source_root: Path) -> bool:
    """Fail closed until a separate S2-E Anchor supplies an external digest."""
    del source_root
    return False


__all__ = ["S2EvidenceCompiler", "S2EvidenceError", "verify_s2_source_evidence"]
