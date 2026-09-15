"""Verify current source bytes without rewriting historical acceptance receipts."""
import hashlib
import json
from pathlib import Path

EVIDENCE_PATH = "agent-workflows/evidence/compatibility/runtime-source-extension.json"
EVIDENCE_DIGEST = "sha256:db8283193f19d5b7925a0c2fd3afb71f75faba0f4fb25a0f9d368c901b6ed496"
HISTORICAL_PATH = "agent-workflows/evidence/compatibility/S2-objective-transition.json"
HISTORICAL_DIGEST = "sha256:e00285c0f2843ba52ea4df9f482c551e22953d73bd4d2f5a4d1ea7f448789cee"


def _bytes(root, relative):
    path = Path(relative)
    if path.is_absolute() or any(part in {".", ".."} for part in path.parts):
        raise ValueError("source extension path must be relative and normalized")
    target = root / path
    if root not in target.parents or any(part.is_symlink() for part in (target, *target.parents)):
        raise ValueError("source extension path escapes its root")
    return target.read_bytes()


def _digest(raw):
    return "sha256:" + hashlib.sha256(raw).hexdigest()


def verify_runtime_source_extension(root):
    """Return the exact current map; a missing or changed binding fails closed."""
    root = Path(root).resolve(strict=True)
    raw = _bytes(root, EVIDENCE_PATH)
    if _digest(raw) != EVIDENCE_DIGEST:
        raise ValueError("current source extension digest is not trusted")
    evidence = json.loads(raw)
    if (evidence.get("schema") != "runtime-source-extension/v1"
            or evidence.get("scope") != "current-source-compatibility-only"
            or evidence.get("historical_acceptance_rewritten") is not False
            or evidence.get("runtime_sample_completed") is not False):
        raise ValueError("current source extension scope is invalid")
    historical = _bytes(root, HISTORICAL_PATH)
    if _digest(historical) != HISTORICAL_DIGEST:
        raise ValueError("historical compatibility receipt changed")
    history = json.loads(historical)
    if evidence.get("historical_products") != history["bound_product_digests"]:
        raise ValueError("historical product map was rewritten")
    current = evidence.get("current_products")
    if not isinstance(current, dict) or set(current) != set(history["bound_product_digests"]):
        raise ValueError("current source extension product set differs")
    for path, digest in current.items():
        if _digest(_bytes(root, path)) != digest:
            raise ValueError("current source extension product changed: " + path)
    for path, digest in evidence["validation_sources"].items():
        if _digest(_bytes(root, path)) != digest:
            raise ValueError("current source validation input changed: " + path)
    receipt = evidence.get("test_receipt", {})
    if (receipt.get("exit_status") != 0 or receipt.get("failures") != 0
            or receipt.get("errors") != 0 or receipt.get("tests_run", 0) < 1
            or not receipt.get("command") or not receipt.get("started_at") or not receipt.get("ended_at")):
        raise ValueError("current source extension test evidence is incomplete")
    return current


def is_extended_historical_source(root, path, historical_digest):
    current = verify_runtime_source_extension(root)
    history = json.loads(_bytes(Path(root), HISTORICAL_PATH))["bound_product_digests"]
    return path in current and history.get(path) == historical_digest
