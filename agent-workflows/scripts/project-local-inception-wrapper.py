#!/usr/bin/env python3
"""Verify and enter one exact project-local Inception release snapshot."""
from __future__ import annotations

import hashlib
import json
import os
from pathlib import Path
import re
import stat
import sys


SNAPSHOT_SCHEMA = "agent-workflow-project-snapshot/v2"
SHA256 = re.compile(r"^sha256:[0-9a-f]{64}$")


def fail(message: str) -> None:
    print("error: " + message, file=sys.stderr)
    raise SystemExit(2)


def unique_object(pairs):
    value = {}
    for key, item in pairs:
        if key in value:
            raise ValueError("duplicate JSON key: " + key)
        value[key] = item
    return value


def digest(data: bytes) -> str:
    return "sha256:" + hashlib.sha256(data).hexdigest()


def canonical_relative(value: object) -> Path:
    if not isinstance(value, str) or not value or "\\" in value or "\x00" in value:
        raise ValueError("non-canonical snapshot path")
    path = Path(value)
    if path.is_absolute() or path.as_posix() != value or any(part in {"", ".", ".."} for part in path.parts):
        raise ValueError("non-canonical snapshot path: " + value)
    return path


def regular_no_follow(project: Path, relative: str) -> Path:
    path = project / canonical_relative(relative)
    current = project
    for part in canonical_relative(relative).parts:
        current = current / part
        info = os.lstat(current)
        if stat.S_ISLNK(info.st_mode):
            raise ValueError("unsafe snapshot symlink: " + relative)
    if not stat.S_ISREG(os.lstat(path).st_mode):
        raise ValueError("missing or unsafe snapshot file: " + relative)
    return path


def listed_files(project: Path, root_relative: str, kind: str) -> set[str]:
    root_path = project / canonical_relative(root_relative)
    if not os.path.lexists(root_path):
        return set()
    if root_path.is_symlink():
        raise ValueError("unsafe managed root: " + root_relative)
    if kind == "file":
        if not root_path.is_file():
            raise ValueError("unsafe managed file root: " + root_relative)
        return {root_relative}
    if kind != "tree" or not root_path.is_dir():
        raise ValueError("unsafe managed root: " + root_relative)
    result: set[str] = set()
    for directory, names, files in os.walk(root_path, followlinks=False):
        base = Path(directory)
        for name in names:
            child = base / name
            if child.is_symlink():
                raise ValueError("unsafe managed directory symlink: " + str(child.relative_to(project)))
        for name in files:
            child = base / name
            relative = child.relative_to(project).as_posix()
            if child.is_symlink() or not child.is_file():
                raise ValueError("unsafe managed file: " + relative)
            result.add(relative)
    return result


project = Path(__file__).resolve().parents[2]
manifest_path = project / ".agent-workflow/SNAPSHOT-MANIFEST.json"
try:
    manifest = json.loads(regular_no_follow(project, ".agent-workflow/SNAPSHOT-MANIFEST.json").read_text(encoding="utf-8"),
                          object_pairs_hook=unique_object)
    if manifest.get("schema") != SNAPSHOT_SCHEMA:
        raise ValueError("unsupported project snapshot manifest")
    if set(manifest) != {"schema", "release_manifest", "release_manifest_digest", "runtime_source",
                         "wrapper", "managed_roots", "files", "snapshot_digest"}:
        raise ValueError("unsupported project snapshot manifest fields")
    if manifest.get("release_manifest") != "agent-workflows/manifests/project-local-inception-release.json":
        raise ValueError("unexpected reviewed release manifest path")
    entries = manifest.get("files")
    roots = manifest.get("managed_roots")
    if not isinstance(entries, dict) or not entries or not isinstance(roots, list) or not roots:
        raise ValueError("empty project snapshot manifest")
    if not SHA256.fullmatch(str(manifest.get("release_manifest_digest"))):
        raise ValueError("invalid reviewed release manifest digest")
    unsigned = dict(manifest)
    supplied_digest = unsigned.pop("snapshot_digest", None)
    actual_snapshot = digest(json.dumps(unsigned, ensure_ascii=True, sort_keys=True,
                                        separators=(",", ":")).encode("utf-8"))
    if supplied_digest != actual_snapshot:
        raise ValueError("project snapshot manifest digest mismatch")
    expected = set(entries)
    for relative, metadata in entries.items():
        canonical_relative(relative)
        if (not isinstance(metadata, dict) or set(metadata) != {"source", "digest", "mode"}
                or not SHA256.fullmatch(str(metadata.get("digest")))
                or not re.fullmatch(r"0[0-7]{3}", str(metadata.get("mode")))):
            raise ValueError("invalid snapshot entry: " + relative)
        path = regular_no_follow(project, relative)
        if digest(path.read_bytes()) != metadata["digest"]:
            raise ValueError("stale snapshot file: " + relative)
        actual_mode = stat.S_IMODE(os.lstat(path).st_mode)
        if actual_mode != int(metadata["mode"], 8):
            raise ValueError("stale snapshot mode: " + relative)
    observed: set[str] = set()
    for root in roots:
        if not isinstance(root, dict) or set(root) != {"source", "destination", "kind", "reason"}:
            raise ValueError("invalid managed root")
        observed.update(listed_files(project, root["destination"], root["kind"]))
    managed_expected = {
        relative for relative in expected
        if any(relative == root["destination"] or relative.startswith(root["destination"] + "/")
               for root in roots)
    }
    if observed != managed_expected:
        extra = sorted(observed - managed_expected)
        missing = sorted(managed_expected - observed)
        raise ValueError("managed snapshot inventory mismatch: extra=%s missing=%s" % (extra, missing))
    wrapper = manifest.get("wrapper")
    runtime_source = manifest.get("runtime_source")
    if wrapper != ".agent-workflow/bin/agent-workflow-inception":
        raise ValueError("unexpected project wrapper destination")
    runtime = project / canonical_relative(runtime_source)
    if not runtime.is_dir() or runtime.is_symlink():
        raise ValueError("missing project runtime source")
except (OSError, ValueError, KeyError, TypeError, json.JSONDecodeError) as error:
    fail(str(error))

os.environ["AGENT_WORKFLOW_RUNTIME_MANIFEST"] = str(manifest_path)
os.environ["AGENT_WORKFLOW_SOURCE_ROOT"] = str(project / ".agent-workflow/runtime")
sys.path.insert(0, str(runtime))
from ai_agent_workflow.inception_cli import main
raise SystemExit(main())
