#!/usr/bin/env python3
"""Create one reviewed, digest-bound project-local Inception snapshot.

The installer consumes a checked-in release-input manifest instead of walking
mutable source trees to decide what to publish. It is create-only, anchors all
destination operations to the canonical project directory descriptor, and
publishes the usable snapshot manifest last.
"""
from __future__ import annotations

import argparse
import errno
import hashlib
import json
import os
from pathlib import Path
import re
import secrets
import stat


class InstallError(ValueError):
    pass


RELEASE_SCHEMA = "agent-workflow-project-release-input/v1"
SNAPSHOT_SCHEMA = "agent-workflow-project-snapshot/v2"
RELEASE_MANIFEST = Path("agent-workflows/manifests/project-local-inception-release.json")
SNAPSHOT_MANIFEST = Path(".agent-workflow/SNAPSHOT-MANIFEST.json")
EXPECTED_RUNTIME_SOURCE = ".agent-workflow/runtime/agent-workflows/src"
EXPECTED_WRAPPER = ".agent-workflow/bin/agent-workflow-inception"
SHA256 = re.compile(r"^sha256:[0-9a-f]{64}$")
MODE = re.compile(r"^0[0-7]{3}$")
NOFOLLOW = getattr(os, "O_NOFOLLOW", 0)
DIRECTORY = getattr(os, "O_DIRECTORY", 0)
EXPECTED_ROOTS = (
    {"kind": "tree", "source": "agent-workflows/skills", "destination": ".agents/skills", "reason": "Codex project-local Skill discovery"},
    {"kind": "tree", "source": "agent-workflows/src", "destination": ".agent-workflow/runtime/agent-workflows/src", "reason": "B/C/D/E Python import closure"},
    {"kind": "tree", "source": "agent-workflows/schemas", "destination": ".agent-workflow/runtime/agent-workflows/schemas", "reason": "runtime schema validation"},
    {"kind": "tree", "source": "agent-workflows/workflows", "destination": ".agent-workflow/runtime/agent-workflows/workflows", "reason": "D1 workflow selection and validation"},
    {"kind": "tree", "source": "agent-workflows/groups", "destination": ".agent-workflow/runtime/agent-workflows/groups", "reason": "Group composition and required-only E policy"},
    {"kind": "file", "source": "agent-workflows/catalog.yaml", "destination": ".agent-workflow/runtime/agent-workflows/catalog.yaml", "reason": "workflow/profile selector catalog"},
    {"kind": "file", "source": "docs/plans/ai-agent-workflow-step-catalog.md", "destination": ".agent-workflow/runtime/docs/plans/ai-agent-workflow-step-catalog.md", "reason": "profile upstream binding"},
    {"kind": "file", "source": "agent-workflows/scripts/project-local-inception-wrapper.py", "destination": EXPECTED_WRAPPER, "reason": "manifest-verifying installed entrypoint"},
)


def digest(data: bytes) -> str:
    return "sha256:" + hashlib.sha256(data).hexdigest()


def unique_object(pairs):
    value = {}
    for key, item in pairs:
        if key in value:
            raise InstallError("duplicate JSON key: " + key)
        value[key] = item
    return value


def canonical_relative(value: object, label: str) -> Path:
    if not isinstance(value, str) or not value or "\\" in value or "\x00" in value:
        raise InstallError(label + " is not a canonical relative path")
    path = Path(value)
    if path.is_absolute() or path.as_posix() != value or any(part in {"", ".", ".."} for part in path.parts):
        raise InstallError(label + " is not a canonical relative path: " + value)
    return path


def safe_root(path: object, label: str) -> Path:
    try:
        root = Path(path).resolve(strict=True)
        info = os.lstat(root)
    except OSError as error:
        raise InstallError(label + " is unavailable") from error
    if not stat.S_ISDIR(info.st_mode) or stat.S_ISLNK(info.st_mode):
        raise InstallError(label + " must be a real directory")
    return root


def nofollow_source(root: Path, relative: object, label: str) -> Path:
    rel = canonical_relative(relative, label)
    current = root
    try:
        for part in rel.parts:
            current = current / part
            info = os.lstat(current)
            if stat.S_ISLNK(info.st_mode):
                raise InstallError(label + " contains a symlink: " + rel.as_posix())
    except OSError as error:
        raise InstallError(label + " is missing: " + rel.as_posix()) from error
    if not stat.S_ISREG(os.lstat(current).st_mode):
        raise InstallError(label + " is not a regular file: " + rel.as_posix())
    return current


def source_tree_files(root: Path, relative: Path) -> set[str]:
    directory = root / relative
    if directory.is_symlink() or not directory.is_dir():
        raise InstallError("missing or unsafe managed source root: " + relative.as_posix())
    found: set[str] = set()
    for current, names, files in os.walk(directory, followlinks=False):
        base = Path(current)
        for name in names:
            child = base / name
            if child.is_symlink():
                raise InstallError("managed source root contains a symlink: " + str(child))
        for name in files:
            child = base / name
            if child.is_symlink() or not child.is_file():
                raise InstallError("managed source root contains a non-file: " + str(child))
            if "__pycache__" in child.parts or child.name.endswith(".pyc"):
                continue
            found.add(child.relative_to(root).as_posix())
    return found


def load_release(source_root: Path):
    manifest_path = source_root / RELEASE_MANIFEST
    try:
        manifest_path = manifest_path.resolve(strict=True)
        manifest_path.relative_to(source_root)
    except (OSError, ValueError) as error:
        raise InstallError("release manifest must be a source-root regular file") from error
    relative_manifest = manifest_path.relative_to(source_root).as_posix()
    raw = nofollow_source(source_root, relative_manifest, "release manifest").read_bytes()
    try:
        release = json.loads(raw.decode("utf-8"), object_pairs_hook=unique_object)
    except (UnicodeDecodeError, json.JSONDecodeError) as error:
        raise InstallError("release manifest is not valid JSON") from error
    if set(release) != {"schema", "runtime_source", "wrapper", "managed_roots", "files"}:
        raise InstallError("release manifest has unsupported fields")
    if release["schema"] != RELEASE_SCHEMA:
        raise InstallError("unsupported release manifest schema")
    if release["runtime_source"] != EXPECTED_RUNTIME_SOURCE or release["wrapper"] != EXPECTED_WRAPPER:
        raise InstallError("release runtime_source or wrapper destination drifted")
    roots = release["managed_roots"]
    entries = release["files"]
    if not isinstance(roots, list) or not roots or not isinstance(entries, list) or not entries:
        raise InstallError("release manifest inventory is empty")
    if roots != list(EXPECTED_ROOTS):
        raise InstallError("release managed-root set drifted")
    if any(not isinstance(item, dict) for item in entries):
        raise InstallError("invalid release file entry")
    if entries != sorted(entries, key=lambda item: (item.get("destination", ""), item.get("source", ""))):
        raise InstallError("release entries are not in canonical order")
    source_roots: set[str] = set()
    destination_roots: set[str] = set()
    normalized_roots = []
    for item in roots:
        if not isinstance(item, dict) or set(item) != {"kind", "source", "destination", "reason"}:
            raise InstallError("invalid managed root declaration")
        if item["kind"] not in {"tree", "file"} or not isinstance(item["reason"], str) or not item["reason"]:
            raise InstallError("invalid managed root kind/reason")
        source = canonical_relative(item["source"], "managed source root").as_posix()
        destination = canonical_relative(item["destination"], "managed destination root").as_posix()
        if source in source_roots or destination in destination_roots:
            raise InstallError("duplicate managed root")
        source_roots.add(source)
        destination_roots.add(destination)
        normalized_roots.append({**item, "source": source, "destination": destination})
    mappings: dict[Path, tuple[bytes, int, str]] = {}
    observed_sources: set[str] = set()
    observed_destinations: set[str] = set()
    by_root: dict[str, set[str]] = {item["source"]: set() for item in normalized_roots}
    for entry in entries:
        if not isinstance(entry, dict) or set(entry) != {"source", "destination", "digest", "mode"}:
            raise InstallError("invalid release file entry")
        source = canonical_relative(entry["source"], "release source").as_posix()
        destination = canonical_relative(entry["destination"], "release destination").as_posix()
        if source in observed_sources or destination in observed_destinations:
            raise InstallError("duplicate release source or destination")
        if not SHA256.fullmatch(str(entry["digest"])) or not MODE.fullmatch(str(entry["mode"])):
            raise InstallError("invalid release digest or mode")
        owners = [item for item in normalized_roots
                  if (item["kind"] == "file" and source == item["source"] and destination == item["destination"])
                  or (item["kind"] == "tree" and source.startswith(item["source"] + "/")
                      and destination.startswith(item["destination"] + "/")
                      and source[len(item["source"]):] == destination[len(item["destination"]):])]
        if len(owners) != 1:
            raise InstallError("release entry is outside or ambiguous between managed roots: " + source)
        data = nofollow_source(source_root, source, "release source").read_bytes()
        if digest(data) != entry["digest"]:
            raise InstallError("changed release source: " + source)
        observed_sources.add(source)
        observed_destinations.add(destination)
        by_root[owners[0]["source"]].add(source)
        mappings[Path(destination)] = (data, int(entry["mode"], 8), source)
    for root in normalized_roots:
        source = canonical_relative(root["source"], "managed source root")
        actual = ({source.as_posix()} if root["kind"] == "file"
                  else source_tree_files(source_root, source))
        if actual != by_root[root["source"]]:
            raise InstallError("added or removed managed release input under %s: added=%s removed=%s" % (
                root["source"], sorted(actual - by_root[root["source"]]),
                sorted(by_root[root["source"]] - actual)))
    if EXPECTED_WRAPPER not in observed_destinations:
        raise InstallError("release manifest omits the exact installed wrapper")
    return release, raw, mappings


def snapshot_from_release(release: dict, release_raw: bytes, mappings) -> dict:
    unsigned = {
        "schema": SNAPSHOT_SCHEMA,
        "release_manifest": RELEASE_MANIFEST.as_posix(),
        "release_manifest_digest": digest(release_raw),
        "runtime_source": release["runtime_source"],
        "wrapper": release["wrapper"],
        "managed_roots": release["managed_roots"],
        "files": {
            relative.as_posix(): {"source": source, "digest": digest(data), "mode": "0%03o" % mode}
            for relative, (data, mode, source) in sorted(mappings.items(), key=lambda item: item[0].as_posix())
        },
    }
    unsigned["snapshot_digest"] = digest(json.dumps(unsigned, ensure_ascii=True, sort_keys=True,
                                                      separators=(",", ":")).encode("utf-8"))
    return unsigned


def _open_root(project: Path) -> int:
    descriptor = os.open(project, os.O_RDONLY | DIRECTORY | NOFOLLOW)
    if not stat.S_ISDIR(os.fstat(descriptor).st_mode):
        os.close(descriptor)
        raise InstallError("project root changed type")
    return descriptor


def _open_directory(root_fd: int, parts: tuple[str, ...], create: bool) -> int | None:
    current = os.dup(root_fd)
    try:
        for part in parts:
            try:
                info = os.stat(part, dir_fd=current, follow_symlinks=False)
            except FileNotFoundError:
                if not create:
                    os.close(current)
                    return None
                try:
                    os.mkdir(part, 0o755, dir_fd=current)
                except FileExistsError:
                    pass
                info = os.stat(part, dir_fd=current, follow_symlinks=False)
            if stat.S_ISLNK(info.st_mode) or not stat.S_ISDIR(info.st_mode):
                raise InstallError("destination ancestor is a symlink or non-directory: " + part)
            try:
                child = os.open(part, os.O_RDONLY | DIRECTORY | NOFOLLOW, dir_fd=current)
            except OSError as error:
                raise InstallError("destination ancestor changed during install: " + part) from error
            os.close(current)
            current = child
        return current
    except Exception:
        try:
            os.close(current)
        except OSError:
            pass
        raise


def _existing(root_fd: int, relative: Path):
    parent = _open_directory(root_fd, relative.parts[:-1], False)
    if parent is None:
        return None
    try:
        try:
            info = os.stat(relative.name, dir_fd=parent, follow_symlinks=False)
        except FileNotFoundError:
            return None
        if stat.S_ISLNK(info.st_mode) or not stat.S_ISREG(info.st_mode):
            raise InstallError("destination is a symlink or non-file: " + relative.as_posix())
        descriptor = os.open(relative.name, os.O_RDONLY | NOFOLLOW, dir_fd=parent)
        try:
            opened = os.fstat(descriptor)
            if not stat.S_ISREG(opened.st_mode):
                raise InstallError("destination changed type: " + relative.as_posix())
            chunks = []
            while True:
                chunk = os.read(descriptor, 1024 * 1024)
                if not chunk:
                    break
                chunks.append(chunk)
            return b"".join(chunks), stat.S_IMODE(opened.st_mode)
        finally:
            os.close(descriptor)
    finally:
        os.close(parent)


def _atomic_create(root_fd: int, relative: Path, data: bytes, mode: int) -> None:
    parent = _open_directory(root_fd, relative.parts[:-1], True)
    assert parent is not None
    temporary = ".pending-%d-%s" % (os.getpid(), secrets.token_hex(8))
    descriptor = None
    try:
        try:
            info = os.stat(relative.name, dir_fd=parent, follow_symlinks=False)
        except FileNotFoundError:
            info = None
        if info is not None:
            raise InstallError("destination appeared during install: " + relative.as_posix())
        descriptor = os.open(temporary, os.O_WRONLY | os.O_CREAT | os.O_EXCL | NOFOLLOW,
                             mode, dir_fd=parent)
        offset = 0
        while offset < len(data):
            offset += os.write(descriptor, data[offset:])
        os.fchmod(descriptor, mode)
        os.fsync(descriptor)
        os.close(descriptor)
        descriptor = None
        os.link(temporary, relative.name, src_dir_fd=parent, dst_dir_fd=parent,
                follow_symlinks=False)
        os.fsync(parent)
    except OSError as error:
        if error.errno in {errno.ELOOP, errno.ENOTDIR, errno.EXDEV}:
            raise InstallError("unsafe destination changed during install: " + relative.as_posix()) from error
        raise
    finally:
        if descriptor is not None:
            os.close(descriptor)
        try:
            os.unlink(temporary, dir_fd=parent)
        except FileNotFoundError:
            pass
        os.close(parent)


def _walk_destination(root_fd: int, prefix: Path) -> set[str]:
    result = set()
    for name in os.listdir(root_fd):
        info = os.stat(name, dir_fd=root_fd, follow_symlinks=False)
        relative = prefix / name
        if stat.S_ISLNK(info.st_mode):
            raise InstallError("managed destination contains a symlink: " + relative.as_posix())
        if stat.S_ISDIR(info.st_mode):
            child = os.open(name, os.O_RDONLY | DIRECTORY | NOFOLLOW, dir_fd=root_fd)
            try:
                result.update(_walk_destination(child, relative))
            finally:
                os.close(child)
        elif stat.S_ISREG(info.st_mode):
            result.add(relative.as_posix())
        else:
            raise InstallError("managed destination contains a non-file: " + relative.as_posix())
    return result


def _destination_inventory(root_fd: int, root: dict) -> set[str]:
    relative = canonical_relative(root["destination"], "managed destination root")
    if root["kind"] == "file":
        return {relative.as_posix()} if _existing(root_fd, relative) is not None else set()
    directory = _open_directory(root_fd, relative.parts, False)
    if directory is None:
        return set()
    try:
        return _walk_destination(directory, relative)
    finally:
        os.close(directory)


def install(project, source_root, *, before_publish=None):
    project = safe_root(project, "project root")
    source_root = safe_root(source_root, "source root")
    release, release_raw, mappings = load_release(source_root)
    snapshot = snapshot_from_release(release, release_raw, mappings)
    snapshot_data = (json.dumps(snapshot, ensure_ascii=False, sort_keys=True, indent=2) + "\n").encode()
    all_mappings = dict(mappings)
    all_mappings[SNAPSHOT_MANIFEST] = (snapshot_data, 0o644, RELEASE_MANIFEST.as_posix())

    root_fd = _open_root(project)
    try:
        expected_destinations = {path.as_posix() for path in mappings}
        observed = set()
        for root in release["managed_roots"]:
            observed.update(_destination_inventory(root_fd, root))
        if observed - expected_destinations:
            raise InstallError("unlisted managed destination files: " + ", ".join(sorted(observed - expected_destinations)))
        conflicts = []
        for relative, (data, mode, _) in all_mappings.items():
            existing = _existing(root_fd, relative)
            if existing is not None and existing != (data, mode):
                conflicts.append(relative.as_posix())
        if conflicts:
            raise InstallError("existing project snapshot conflicts: " + ", ".join(sorted(conflicts)))
        if before_publish is not None:
            before_publish(project)
        for relative, (data, mode, _) in sorted(mappings.items(), key=lambda item: item[0].as_posix()):
            if _existing(root_fd, relative) is None:
                _atomic_create(root_fd, relative, data, mode)
        # Manifest-last: without this exact file the wrapper always fails closed.
        if _existing(root_fd, SNAPSHOT_MANIFEST) is None:
            _atomic_create(root_fd, SNAPSHOT_MANIFEST, snapshot_data, 0o644)
    finally:
        os.close(root_fd)
    return snapshot


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--project", required=True)
    parser.add_argument("--source-root", default=str(Path(__file__).resolve().parents[2]))
    args = parser.parse_args(argv)
    try:
        result = install(args.project, args.source_root)
        print(json.dumps(result, ensure_ascii=False, sort_keys=True, indent=2))
        return 0
    except (InstallError, OSError, ValueError, KeyError, TypeError) as error:
        print("error: " + str(error), file=os.sys.stderr)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
