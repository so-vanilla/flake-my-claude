#!/usr/bin/env python3
"""Generate/check the reviewed input inventory for project-local Inception."""
from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path
import stat
import sys


SCHEMA = "agent-workflow-project-release-input/v1"
OUTPUT = Path("agent-workflows/manifests/project-local-inception-release.json")
ROOTS = (
    ("tree", "agent-workflows/skills", ".agents/skills", "Codex project-local Skill discovery"),
    ("tree", "agent-workflows/src", ".agent-workflow/runtime/agent-workflows/src", "B/C/D/E Python import closure"),
    ("tree", "agent-workflows/schemas", ".agent-workflow/runtime/agent-workflows/schemas", "runtime schema validation"),
    ("tree", "agent-workflows/workflows", ".agent-workflow/runtime/agent-workflows/workflows", "D1 workflow selection and validation"),
    ("tree", "agent-workflows/groups", ".agent-workflow/runtime/agent-workflows/groups", "Group composition and required-only E policy"),
    ("file", "agent-workflows/catalog.yaml", ".agent-workflow/runtime/agent-workflows/catalog.yaml", "workflow/profile selector catalog"),
    ("file", "docs/plans/ai-agent-workflow-step-catalog.md", ".agent-workflow/runtime/docs/plans/ai-agent-workflow-step-catalog.md", "profile upstream binding"),
    ("file", "agent-workflows/scripts/project-local-inception-wrapper.py", ".agent-workflow/bin/agent-workflow-inception", "manifest-verifying installed entrypoint"),
)


def digest(data: bytes) -> str:
    return "sha256:" + hashlib.sha256(data).hexdigest()


def inventory(root: Path) -> dict:
    files = []
    managed_roots = []
    for kind, source_name, destination_name, reason in ROOTS:
        source = root / source_name
        if source.is_symlink() or (kind == "tree" and not source.is_dir()) or (kind == "file" and not source.is_file()):
            raise ValueError("missing or unsafe release source: " + source_name)
        managed_roots.append({"kind": kind, "source": source_name,
                              "destination": destination_name, "reason": reason})
        paths = [source] if kind == "file" else sorted(
            item for item in source.rglob("*")
            if item.is_file() and "__pycache__" not in item.parts and not item.name.endswith(".pyc")
        )
        for path in paths:
            if path.is_symlink():
                raise ValueError("release source contains a symlink: " + str(path))
            relative = Path() if kind == "file" else path.relative_to(source)
            destination = Path(destination_name) / relative
            mode = "0755" if destination.as_posix() == ".agent-workflow/bin/agent-workflow-inception" else "0644"
            files.append({"source": path.relative_to(root).as_posix(),
                          "destination": destination.as_posix(),
                          "digest": digest(path.read_bytes()), "mode": mode})
    files.sort(key=lambda item: (item["destination"], item["source"]))
    return {
        "schema": SCHEMA,
        "runtime_source": ".agent-workflow/runtime/agent-workflows/src",
        "wrapper": ".agent-workflow/bin/agent-workflow-inception",
        "managed_roots": managed_roots,
        "files": files,
    }


def encoded(value: dict) -> bytes:
    return (json.dumps(value, ensure_ascii=False, sort_keys=True, indent=2) + "\n").encode("utf-8")


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source-root", default=str(Path(__file__).resolve().parents[2]))
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args(argv)
    root = Path(args.source_root).resolve(strict=True)
    data = encoded(inventory(root))
    target = root / OUTPUT
    if args.check:
        if not target.is_file() or target.read_bytes() != data:
            print("project-local release manifest is stale", file=sys.stderr)
            return 1
        return 0
    target.write_bytes(data)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
