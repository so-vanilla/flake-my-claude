"""Parent-owned macOS task launcher with a positive file-read boundary.

The broker is the process boundary used by a future persisted Group E runtime.
It deliberately does not grant workflow authority or mutate Kernel state.  A
Worker receives only the sandboxed task process; the parent retains the HMAC
capability used to authenticate the resulting isolation receipt.
"""

from __future__ import annotations

import copy
import errno
import hashlib
import hmac
import json
import os
import posixpath
import secrets
import stat
import subprocess
import sys
from collections.abc import Mapping, Sequence
from pathlib import Path
from typing import Any

from .persistent_receipts import (
    DuplicateReceiptConflict,
    PersistentReceiptError,
    PersistentReceiptRunner,
    PosixProcessAdapter,
)
from .schema_validation import SchemaValidationError, validate_document


class MacOSTaskProcessError(PersistentReceiptError):
    """The trusted macOS task-process boundary is unavailable or unbound."""


def _canonical(value: Any) -> bytes:
    return json.dumps(
        value, sort_keys=True, separators=(",", ":"), ensure_ascii=False
    ).encode("utf-8")


def _digest(value: Any) -> str:
    return "sha256:" + hashlib.sha256(_canonical(value)).hexdigest()


def _quote(value: str) -> str:
    return json.dumps(value, ensure_ascii=False)


_DIRECTORY = getattr(os, "O_DIRECTORY", 0)
_NOFOLLOW = getattr(os, "O_NOFOLLOW", 0)

_APPROVAL_DIGEST = (
    "sha256:2f0ae19fac7b90778d1b5713c51071f1ab7e96ce179237a1e5892cf320dfaefb"
)
_ACCEPTED_RESIDUALS = [
    "a malicious external process running as the same login UID may rename a project ancestor",
    "SIGKILL of the trusted parent after a publication syscall and before bookkeeping may leave bytes in a detached tree",
]
_PRACTICAL_THREAT_PROFILE = {
    "schema": "macos-practical-threat-profile/v1",
    "profile_id": "macos-parent-writer-practical/v1",
    "approval_digest": _APPROVAL_DIGEST,
    "trusted": [
        "Codex host",
        "parent Orchestrator",
        "same-UID parent runtime process",
    ],
    "adversarial": [
        "sandboxed task child",
        "malformed or stale caller input",
        "path substitution available to the sandboxed task child",
    ],
    "accepted_residuals": _ACCEPTED_RESIDUALS,
    "guarantees": [
        "sandboxed task child cannot read broker state",
        "sandboxed task child cannot mutate broker state",
        "task write scope is disjoint from broker state",
        "trusted parent authenticates the receipt and rejects caller-authored safety labels",
    ],
    "not_guaranteed": _ACCEPTED_RESIDUALS,
}
_PRACTICAL_THREAT_PROFILE_DIGEST = _digest(_PRACTICAL_THREAT_PROFILE)


def _practical_threat_profile() -> dict[str, Any]:
    return copy.deepcopy(_PRACTICAL_THREAT_PROFILE)


def _validate_macos_document(document: Any, schema_name: str) -> None:
    schema_root = Path(__file__).resolve().parents[2] / "schemas"
    names = {
        schema_name,
        "macos-practical-threat-profile-v1.schema.json",
    }
    try:
        schemas = {
            name: json.loads((schema_root / name).read_text(encoding="utf-8"))
            for name in names
        }
        registry: dict[str, Mapping[str, Any]] = {
            name: schema for name, schema in schemas.items()
        }
        for schema in schemas.values():
            registry.update(schema.get("$defs", {}))
        validate_document(document, schemas[schema_name], registry)
    except (OSError, json.JSONDecodeError, SchemaValidationError) as error:
        raise MacOSTaskProcessError(
            "macOS task-process practical contract is unavailable or invalid"
        ) from error


def _path_parts(value: str, label: str) -> tuple[str, ...]:
    if (
        not isinstance(value, str)
        or not value
        or value.startswith("/")
        or posixpath.normpath(value) != value
        or value in {".", ".."}
        or ".." in value.split("/")
    ):
        raise MacOSTaskProcessError(label + " must be a canonical relative path")
    return tuple(value.split("/"))


def _validate_existing_absolute_prefix(path: str) -> None:
    """Reject an already-unsafe absolute prefix without creating any state."""
    parts = tuple(part for part in path.split("/") if part)
    current = os.open("/", os.O_RDONLY | _DIRECTORY | _NOFOLLOW)
    try:
        for part in parts:
            try:
                expected = os.stat(part, dir_fd=current, follow_symlinks=False)
            except FileNotFoundError:
                return
            if stat.S_ISLNK(expected.st_mode) or not stat.S_ISDIR(expected.st_mode):
                raise MacOSTaskProcessError(
                    "broker state prefix is a symlink or non-directory: " + part
                )
            try:
                child = os.open(
                    part,
                    os.O_RDONLY | _DIRECTORY | _NOFOLLOW,
                    dir_fd=current,
                )
            except OSError as error:
                raise MacOSTaskProcessError(
                    "broker state prefix changed during validation: " + part
                ) from error
            opened = os.fstat(child)
            if (expected.st_dev, expected.st_ino) != (opened.st_dev, opened.st_ino):
                os.close(child)
                raise MacOSTaskProcessError(
                    "broker state prefix identity changed: " + part
                )
            os.close(current)
            current = child
    finally:
        os.close(current)


class _AnchoredBrokerState:
    """Private broker state rooted at held, no-follow directory descriptors."""

    def __init__(
        self, workspace: str, state_root: str, *, create: bool = True
    ) -> None:
        relative = posixpath.relpath(state_root, workspace)
        parts = _path_parts(relative, "broker state root")
        try:
            expected = os.stat(workspace, follow_symlinks=False)
            project_fd = os.open(workspace, os.O_RDONLY | _DIRECTORY | _NOFOLLOW)
        except OSError as error:
            raise MacOSTaskProcessError(
                "selected project cannot be anchored without following links"
            ) from error
        try:
            opened = os.fstat(project_fd)
            if (
                not stat.S_ISDIR(expected.st_mode)
                or not stat.S_ISDIR(opened.st_mode)
                or (expected.st_dev, expected.st_ino) != (opened.st_dev, opened.st_ino)
            ):
                raise MacOSTaskProcessError("selected project identity changed")
            self._project_identity = (opened.st_dev, opened.st_ino)
            self._state_fd = self._open_directory(project_fd, parts, create=create)
            self._project_fd = project_fd
            project_fd = -1
        finally:
            if project_fd >= 0:
                os.close(project_fd)
        state = os.fstat(self._state_fd)
        if stat.S_IMODE(state.st_mode) & 0o077:
            self.close()
            raise MacOSTaskProcessError(
                "broker state root must not grant group or other access"
            )
        self._state_identity = (state.st_dev, state.st_ino)
        self._state_parts = parts
        self.workspace = workspace
        self.state_root = state_root

    @staticmethod
    def _open_directory(
        root_fd: int, parts: Sequence[str], *, create: bool
    ) -> int:
        current = os.dup(root_fd)
        try:
            for part in parts:
                try:
                    expected = os.stat(part, dir_fd=current, follow_symlinks=False)
                except FileNotFoundError:
                    if not create:
                        raise
                    try:
                        os.mkdir(part, 0o700, dir_fd=current)
                    except FileExistsError:
                        pass
                    expected = os.stat(part, dir_fd=current, follow_symlinks=False)
                if stat.S_ISLNK(expected.st_mode) or not stat.S_ISDIR(expected.st_mode):
                    raise MacOSTaskProcessError(
                        "broker state ancestor is a symlink or non-directory: " + part
                    )
                try:
                    child = os.open(
                        part,
                        os.O_RDONLY | _DIRECTORY | _NOFOLLOW,
                        dir_fd=current,
                    )
                except OSError as error:
                    raise MacOSTaskProcessError(
                        "broker state ancestor changed during traversal: " + part
                    ) from error
                opened = os.fstat(child)
                if (
                    not stat.S_ISDIR(opened.st_mode)
                    or (expected.st_dev, expected.st_ino)
                    != (opened.st_dev, opened.st_ino)
                ):
                    os.close(child)
                    raise MacOSTaskProcessError(
                        "broker state ancestor identity changed: " + part
                    )
                os.close(current)
                current = child
            return current
        except Exception:
            try:
                os.close(current)
            except OSError:
                pass
            raise

    def close(self) -> None:
        for field in ("_state_fd", "_project_fd"):
            descriptor = getattr(self, field, None)
            if descriptor is not None:
                os.close(descriptor)
                setattr(self, field, None)

    def _project_attached(self) -> None:
        if self._state_fd is None or self._project_fd is None:
            raise MacOSTaskProcessError("broker state anchor is closed")
        try:
            project = os.stat(self.workspace, follow_symlinks=False)
        except OSError as error:
            raise MacOSTaskProcessError(
                "selected project path is no longer available"
            ) from error
        if (
            not stat.S_ISDIR(project.st_mode)
            or (project.st_dev, project.st_ino) != self._project_identity
        ):
            raise MacOSTaskProcessError("selected project identity changed")

    def _parent(
        self, relative: str, *, create: bool
    ) -> tuple[int, str, tuple[str, ...]]:
        self._project_attached()
        parts = _path_parts(relative, "broker state path")
        # Re-open the declared state path from the held project descriptor for
        # every operation.  A renamed/replaced ancestor therefore refuses;
        # writes never switch to a path reached through a link.
        state = self._open_directory(
            self._project_fd, self._state_parts, create=False
        )
        try:
            observed = os.fstat(state)
            if (observed.st_dev, observed.st_ino) != self._state_identity:
                raise MacOSTaskProcessError("broker state root identity changed")
            parent = self._open_directory(state, parts[:-1], create=create)
            return parent, parts[-1], parts[:-1]
        finally:
            os.close(state)

    def _assert_attached(
        self, parent_fd: int, parent_parts: Sequence[str]
    ) -> None:
        """Re-establish the exact project-to-parent identity at an I/O boundary."""
        self._project_attached()
        if self._project_fd is None:
            raise MacOSTaskProcessError("broker project anchor is closed")
        state: int | None = None
        try:
            state = self._open_directory(
                self._project_fd, self._state_parts, create=False
            )
            observed_state = os.fstat(state)
            if (observed_state.st_dev, observed_state.st_ino) != self._state_identity:
                raise MacOSTaskProcessError("broker state root identity changed")
            current = self._open_directory(state, parent_parts, create=False)
            try:
                expected = os.fstat(current)
                opened = os.fstat(parent_fd)
                if (
                    not stat.S_ISDIR(opened.st_mode)
                    or (expected.st_dev, expected.st_ino)
                    != (opened.st_dev, opened.st_ino)
                ):
                    raise MacOSTaskProcessError(
                        "broker publication parent detached from selected project"
                    )
            finally:
                os.close(current)
        except (FileNotFoundError, NotADirectoryError) as error:
            raise MacOSTaskProcessError(
                "broker publication parent detached from selected project"
            ) from error
        finally:
            if state is not None:
                os.close(state)

    def read(self, relative: str) -> tuple[bytes, int]:
        try:
            parent, name, parent_parts = self._parent(relative, create=False)
        except FileNotFoundError:
            raise
        try:
            self._assert_attached(parent, parent_parts)
            descriptor = os.open(name, os.O_RDONLY | _NOFOLLOW, dir_fd=parent)
            try:
                info = os.fstat(descriptor)
                if not stat.S_ISREG(info.st_mode):
                    raise MacOSTaskProcessError(
                        "broker state entry is not a regular file: " + relative
                    )
                chunks: list[bytes] = []
                while True:
                    chunk = os.read(descriptor, 1024 * 1024)
                    if not chunk:
                        break
                    chunks.append(chunk)
                result = b"".join(chunks), stat.S_IMODE(info.st_mode)
            finally:
                os.close(descriptor)
            self._assert_attached(parent, parent_parts)
            return result
        except OSError as error:
            if error.errno in {errno.ELOOP, errno.ENOTDIR}:
                raise MacOSTaskProcessError(
                    "broker state entry changed type: " + relative
                ) from error
            raise
        finally:
            os.close(parent)

    def create(self, relative: str, data: bytes) -> None:
        parent, name, parent_parts = self._parent(relative, create=True)
        temporary = ".pending-%d-%s" % (os.getpid(), secrets.token_hex(8))
        descriptor: int | None = None
        staged = False
        published = False
        try:
            self._assert_attached(parent, parent_parts)
            descriptor = os.open(
                temporary,
                os.O_WRONLY | os.O_CREAT | os.O_EXCL | _NOFOLLOW,
                0o600,
                dir_fd=parent,
            )
            staged = True
            # The destination is not made visible until the staged bytes are
            # durable and the exact project-to-parent chain is re-established.
            self._assert_attached(parent, parent_parts)
            self._write_all(descriptor, data)
            os.fsync(descriptor)
            os.close(descriptor)
            descriptor = None
            self._assert_attached(parent, parent_parts)
            os.link(
                temporary,
                name,
                src_dir_fd=parent,
                dst_dir_fd=parent,
                follow_symlinks=False,
            )
            published = True
            self._assert_attached(parent, parent_parts)
            os.fsync(parent)
            self._assert_attached(parent, parent_parts)
            os.unlink(temporary, dir_fd=parent)
            staged = False
            os.fsync(parent)
            self._assert_attached(parent, parent_parts)
        except Exception as error:
            if descriptor is not None:
                os.close(descriptor)
                descriptor = None
            if published:
                try:
                    os.unlink(name, dir_fd=parent)
                except FileNotFoundError:
                    pass
            if staged:
                try:
                    os.unlink(temporary, dir_fd=parent)
                except FileNotFoundError:
                    pass
            if published or staged:
                try:
                    os.fsync(parent)
                except OSError:
                    pass
            if isinstance(error, OSError) and error.errno in {
                errno.ELOOP,
                errno.ENOTDIR,
            }:
                raise MacOSTaskProcessError(
                    "broker state destination changed type: " + relative
                ) from error
            raise
        finally:
            if descriptor is not None:
                os.close(descriptor)
            if staged:
                try:
                    os.unlink(temporary, dir_fd=parent)
                except FileNotFoundError:
                    pass
            os.close(parent)

    def replace(self, relative: str, data: bytes) -> None:
        parent, name, parent_parts = self._parent(relative, create=True)
        temporary = ".pending-%d-%s" % (os.getpid(), secrets.token_hex(8))
        backup = ".backup-%d-%s" % (os.getpid(), secrets.token_hex(8))
        descriptor: int | None = None
        backup_created = False
        replaced = False
        try:
            self._assert_attached(parent, parent_parts)
            try:
                existing = os.stat(name, dir_fd=parent, follow_symlinks=False)
            except FileNotFoundError:
                existing = None
            if existing is not None and (
                stat.S_ISLNK(existing.st_mode) or not stat.S_ISREG(existing.st_mode)
            ):
                raise MacOSTaskProcessError(
                    "broker state destination is a symlink or non-file: " + relative
                )
            if existing is not None:
                os.link(
                    name,
                    backup,
                    src_dir_fd=parent,
                    dst_dir_fd=parent,
                    follow_symlinks=False,
                )
                backup_created = True
                self._assert_attached(parent, parent_parts)
            descriptor = os.open(
                temporary,
                os.O_WRONLY | os.O_CREAT | os.O_EXCL | _NOFOLLOW,
                0o600,
                dir_fd=parent,
            )
            self._assert_attached(parent, parent_parts)
            self._write_all(descriptor, data)
            os.fsync(descriptor)
            os.close(descriptor)
            descriptor = None
            self._assert_attached(parent, parent_parts)
            os.replace(
                temporary,
                name,
                src_dir_fd=parent,
                dst_dir_fd=parent,
            )
            replaced = True
            self._assert_attached(parent, parent_parts)
            os.fsync(parent)
            self._assert_attached(parent, parent_parts)
            if backup_created:
                # The target has already passed its post-publication identity
                # and durability checks.  Remove the rollback link only after
                # that point; it is not itself a publication of new bytes.
                os.unlink(backup, dir_fd=parent)
                backup_created = False
                os.fsync(parent)
        except Exception as error:
            if descriptor is not None:
                os.close(descriptor)
                descriptor = None
            if replaced:
                try:
                    if backup_created:
                        os.replace(
                            backup,
                            name,
                            src_dir_fd=parent,
                            dst_dir_fd=parent,
                        )
                        backup_created = False
                    else:
                        os.unlink(name, dir_fd=parent)
                    os.fsync(parent)
                except FileNotFoundError:
                    pass
            if isinstance(error, OSError) and error.errno in {
                errno.ELOOP,
                errno.ENOTDIR,
                errno.EXDEV,
            }:
                raise MacOSTaskProcessError(
                    "broker state destination changed during replace: " + relative
                ) from error
            raise
        finally:
            if descriptor is not None:
                os.close(descriptor)
            try:
                os.unlink(temporary, dir_fd=parent)
            except FileNotFoundError:
                pass
            if backup_created:
                try:
                    os.unlink(backup, dir_fd=parent)
                except FileNotFoundError:
                    pass
            os.close(parent)

    @staticmethod
    def _write_all(descriptor: int, data: bytes) -> None:
        offset = 0
        while offset < len(data):
            offset += os.write(descriptor, data[offset:])


def _canonical_directory(value: Any, label: str) -> str:
    if (
        not isinstance(value, str)
        or not value.startswith("/")
        or value == "/"
        or posixpath.normpath(value) != value
    ):
        raise MacOSTaskProcessError(
            label + " must be one canonical absolute non-root directory"
        )
    path = Path(value)
    try:
        resolved = path.resolve(strict=True)
    except OSError as error:
        raise MacOSTaskProcessError(label + " must physically exist") from error
    if not resolved.is_dir() or str(resolved) != value:
        raise MacOSTaskProcessError(label + " must be a resolved physical directory")
    return value


def _directories(value: Any, label: str) -> list[str]:
    if not isinstance(value, Sequence) or isinstance(value, (str, bytes)) or not value:
        raise MacOSTaskProcessError(label + " must be a non-empty directory list")
    result: list[str] = []
    for item in value:
        if (
            not isinstance(item, str)
            or not item.startswith("/")
            or item == "/"
            or posixpath.normpath(item) != item
            or not Path(item).is_dir()
        ):
            raise MacOSTaskProcessError(
                label + " must contain existing canonical absolute non-root directories"
            )
        # Keep the declared spelling.  macOS firmlinks make /bin and /usr/bin
        # physically related but Seatbelt process-exec filters match /bin/cat.
        result.append(item)
    result.sort()
    if len(result) != len(set(result)):
        raise MacOSTaskProcessError(label + " contains duplicates")
    return result


def _inside(path: str, root: str) -> bool:
    return path == root or path.startswith(root + "/")


def _ancestors(path: str) -> list[str]:
    current = Path(path)
    values: list[str] = []
    while current != current.parent:
        current = current.parent
        values.append(str(current))
    return sorted(set(values))


def _relative_roots(workspace: str, values: Any, label: str) -> list[str]:
    if not isinstance(values, Sequence) or isinstance(values, (str, bytes)):
        raise MacOSTaskProcessError(label + " must be a path list")
    result: list[str] = []
    for value in values:
        if (
            not isinstance(value, str)
            or not value
            or value.startswith("/")
            or value in {".", ".."}
            or posixpath.normpath(value) != value
            or ".." in value.split("/")
        ):
            raise MacOSTaskProcessError(
                label + " contains a non-canonical project-relative path"
            )
        result.append(workspace + "/" + value)
    if len(result) != len(set(result)):
        raise MacOSTaskProcessError(label + " contains duplicates")
    return sorted(result)


def _compile_macos_task_process_release_v1(
    closure: Mapping[str, Any],
    *,
    broker_state_root: str,
    system_read_roots: Sequence[str],
    runtime_read_roots: Sequence[str],
) -> dict[str, Any]:
    """Rebuild a frozen historical v1 release without upgrading its evidence."""
    if sys.platform != "darwin" or not Path("/usr/bin/sandbox-exec").is_file():
        raise MacOSTaskProcessError("macOS sandbox-exec is unavailable")
    if (
        not isinstance(closure, Mapping)
        or closure.get("schema") != "execution-package-closure/v2"
    ):
        raise MacOSTaskProcessError(
            "task-process release requires an execution closure"
        )
    workspace = _canonical_directory(
        closure.get("workspace_identity"), "workspace_identity"
    )
    state_root = broker_state_root
    if (
        not isinstance(state_root, str)
        or posixpath.normpath(state_root) != state_root
        or not _inside(state_root, workspace)
        or state_root == workspace
    ):
        raise MacOSTaskProcessError(
            "broker_state_root must be a canonical directory below the selected project"
        )
    systems = _directories(system_read_roots, "system_read_roots")
    runtimes = _directories(runtime_read_roots, "runtime_read_roots")
    for root in systems + runtimes:
        if _inside(workspace, root):
            raise MacOSTaskProcessError(
                "a system/runtime prerequisite may not contain the selected project"
            )

    command = closure.get("command")
    claims = closure.get("resource_claims")
    if not isinstance(command, Mapping) or not isinstance(claims, Mapping):
        raise MacOSTaskProcessError("closure command or resource claims are missing")
    argv = command.get("argv")
    if (
        not isinstance(argv, list)
        or not argv
        or not isinstance(argv[0], str)
        or not argv[0].startswith("/")
    ):
        raise MacOSTaskProcessError(
            "sandboxed task command must use an absolute executable"
        )
    try:
        executable = str(Path(argv[0]).resolve(strict=True))
    except OSError as error:
        raise MacOSTaskProcessError(
            "sandboxed task executable is unavailable"
        ) from error
    if not any(
        _inside(executable, root) or _inside(argv[0], root)
        for root in systems + runtimes
    ):
        raise MacOSTaskProcessError(
            "task executable is outside explicit system/runtime prerequisites"
        )
    cwd = command.get("cwd")
    if not isinstance(cwd, str) or not _inside(cwd, workspace):
        raise MacOSTaskProcessError("task cwd is outside the selected project")
    write_roots = _relative_roots(
        workspace, claims.get("write_paths"), "resource_claims.write_paths"
    )
    if any(
        _inside(state_root, root) or _inside(root, state_root) for root in write_roots
    ):
        raise MacOSTaskProcessError(
            "task write scope overlaps parent-owned broker state"
        )

    # The selected project is the sole user-data read root.  The other roots are
    # explicit runtime/system prerequisites and are retained in the receipt.
    read_roots = sorted(set([workspace] + systems + runtimes))
    metadata_roots = sorted({item for root in read_roots for item in _ancestors(root)})
    process_roots = sorted(set(systems + runtimes))
    read_rules = " ".join(f"(subpath {_quote(root)})" for root in read_roots)
    metadata_rules = " ".join(f"(literal {_quote(root)})" for root in metadata_roots)
    process_rules = " ".join(f"(subpath {_quote(root)})" for root in process_roots)
    lines = [
        "(version 1)",
        "(deny default)",
        '(import "dyld-support.sb")',
        f"(allow file-read* {read_rules})",
        f"(deny file-read* (subpath {_quote(state_root)}))",
        f"(allow file-read-metadata {metadata_rules})",
        '(allow file-write-data (literal "/dev/null") (literal "/dev/zero"))',
        f"(allow file-map-executable {process_rules})",
        f"(allow process-exec {process_rules})",
        "(allow process-fork)",
        "(allow signal process-info-dirtycontrol process-info-pidinfo (target self))",
        "(allow sysctl-read)",
        '(allow mach-lookup (global-name "com.apple.logd") (global-name "com.apple.system.logger"))',
    ]
    if write_roots:
        write_rules = " ".join(f"(subpath {_quote(root)})" for root in write_roots)
        lines.append(f"(allow file-write* {write_rules})")
    profile = "\n".join(lines) + "\n"
    release = {
        "schema": "macos-task-process-release/v1",
        "engine": "/usr/bin/sandbox-exec",
        "profile_imports": ["dyld-support.sb"],
        "workspace_identity": workspace,
        "broker_state_root": state_root,
        "system_read_roots": systems,
        "runtime_read_roots": runtimes,
        "project_read_roots": [workspace],
        "write_roots": write_roots,
        "command": copy.deepcopy(dict(command)),
        "execution_closure_ref": {
            "id": closure.get("package_id"),
            "digest": closure.get("closure_digest"),
        },
        "profile": profile,
        "profile_digest": "sha256:"
        + hashlib.sha256(profile.encode("utf-8")).hexdigest(),
        "os_isolation_enforced": True,
        "release_rule": "broker-only",
    }
    release["release_digest"] = _digest(release)
    return release


def _state_writer_boundary(
    broker_state_root: str, task_write_roots: Sequence[str]
) -> dict[str, Any]:
    return {
        "writer": "trusted-same-uid-parent",
        "task_child_is_writer": False,
        "broker_state_root": broker_state_root,
        "task_write_roots": copy.deepcopy(list(task_write_roots)),
        "task_write_scope_disjoint": True,
        "descriptor_anchored": True,
        "no_follow": True,
        "device_inode_revalidated": True,
        "pre_post_attachment_checks": True,
        "rollback": "defense-in-depth",
        "arbitrary_same_uid_atomicity": False,
        "publication_sigkill_atomicity": False,
    }


def compile_macos_task_process_release(
    closure: Mapping[str, Any],
    *,
    broker_state_root: str,
    system_read_roots: Sequence[str],
    runtime_read_roots: Sequence[str],
) -> dict[str, Any]:
    """Compile the exact practical-profile v2 release for new dispatch."""
    release = _compile_macos_task_process_release_v1(
        closure,
        broker_state_root=broker_state_root,
        system_read_roots=system_read_roots,
        runtime_read_roots=runtime_read_roots,
    )
    release.pop("release_digest")
    release["schema"] = "macos-task-process-release/v2"
    release["threat_profile"] = _practical_threat_profile()
    release["threat_profile_digest"] = _PRACTICAL_THREAT_PROFILE_DIGEST
    release["state_writer_boundary"] = _state_writer_boundary(
        release["broker_state_root"], release["write_roots"]
    )
    release["release_digest"] = _digest(release)
    _validate_macos_document(
        release, "macos-task-process-release-v2.schema.json"
    )
    return release


class _SandboxedProcessAdapter(PosixProcessAdapter):
    """PersistentReceiptRunner adapter that gates the real command in sandbox-exec."""

    def __init__(self, release: Mapping[str, Any]) -> None:
        super().__init__()
        self.release_spec = copy.deepcopy(dict(release))
        self.spawn_count = 0
        self.last_identity: dict[str, Any] | None = None

    def prepare(
        self, command: list[str], cwd: str, environment: Mapping[str, str]
    ) -> Mapping[str, Any]:
        if (
            command != self.release_spec["command"]["argv"]
            or cwd != self.release_spec["command"]["cwd"]
        ):
            raise MacOSTaskProcessError(
                "runner attempted to release a command outside the E2 package"
            )
        read_fd, write_fd = os.pipe()
        os.set_inheritable(read_fd, True)
        python = str(Path(sys.executable).resolve(strict=True))
        wrapped = [
            python,
            "-I",
            "-S",
            "-c",
            self.WRAPPER,
            str(read_fd),
            json.dumps(command),
        ]
        argv = [
            self.release_spec["engine"],
            "-p",
            self.release_spec["profile"],
            "--",
        ] + wrapped
        env = dict(environment)
        try:
            process = subprocess.Popen(
                argv,
                cwd=cwd,
                env=env,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                start_new_session=True,
                pass_fds=(read_fd,),
            )
        finally:
            os.close(read_fd)
        self._processes[process.pid] = process
        self._gates[process.pid] = write_fd
        identity = {
            "pid": process.pid,
            "process_group_id": os.getpgid(process.pid),
            "birth_token": self._birth(process.pid),
        }
        self.spawn_count += 1
        self.last_identity = copy.deepcopy(identity)
        return identity


class _AnchoredReceiptRunner(PersistentReceiptRunner):
    """Use the broker's held state descriptor for every durable runner byte."""

    def __init__(
        self,
        store: _AnchoredBrokerState,
        *,
        adapter: PosixProcessAdapter,
        boot_id: str | None,
    ) -> None:
        super().__init__(store.state_root, adapter=adapter, boot_id=boot_id)
        self._store = store

    def _existing(
        self, package: Mapping[str, Any], policy: Mapping[str, Any]
    ) -> dict[str, Any] | None:
        relative = "command-runner/receipts/%s.json" % self._key(package)
        try:
            raw, _ = self._store.read(relative)
        except FileNotFoundError:
            return None
        try:
            value = json.loads(raw)
        except (UnicodeDecodeError, json.JSONDecodeError) as error:
            raise PersistentReceiptError("receipt is corrupt") from error
        if value.get("fingerprint") != self._fingerprint(package, policy):
            raise DuplicateReceiptConflict("idempotency key has changed payload")
        if value.get("terminal_receipt") is not None:
            timing = value.get("timing", {})
            phases = timing.get("phase_seconds")
            if (
                not isinstance(phases, Mapping)
                or set(phases)
                != {"reserve", "launch", "supervision", "publication"}
                or sum(phases.values()) != timing.get("elapsed_seconds")
            ):
                raise PersistentReceiptError(
                    "terminal phase attribution is inconsistent"
                )
            captures = value.get("captures")
            if not isinstance(captures, Mapping) or set(captures) != {
                "stdout",
                "stderr",
            }:
                raise PersistentReceiptError("capture inventory is corrupt")
            for stream, capture in captures.items():
                capture_relative = capture.get("report_safe_ref")
                expected = "captures/%s/%s.bin" % (
                    stream,
                    str(capture.get("digest", "")).removeprefix("sha256:"),
                )
                if capture_relative != expected or capture.get("stream") != stream:
                    raise PersistentReceiptError("capture identity is corrupt")
                try:
                    retained, mode = self._store.read(
                        "command-runner/" + capture_relative
                    )
                except FileNotFoundError as error:
                    raise PersistentReceiptError("capture is unavailable") from error
                if (
                    len(retained) != capture.get("retained_byte_count")
                    or "sha256:" + hashlib.sha256(retained).hexdigest()
                    != capture.get("digest")
                    or mode != 0o600
                ):
                    raise PersistentReceiptError("capture integrity is corrupt")
            payload = {
                key: copy.deepcopy(item)
                for key, item in value.items()
                if key not in {"payload_digest", "terminal_receipt"}
            }
            receipt = value["terminal_receipt"]
            unsigned_receipt = {
                key: copy.deepcopy(item)
                for key, item in receipt.items()
                if key != "receipt_digest"
            }
            if (
                value.get("payload_digest") != _digest(payload)
                or receipt.get("payload_digest") != value.get("payload_digest")
                or receipt.get("receipt_digest") != _digest(unsigned_receipt)
            ):
                raise PersistentReceiptError("terminal receipt identity is corrupt")
            schema_path = (
                Path(__file__).resolve().parents[2]
                / "schemas"
                / "command-receipt-v1.schema.json"
            )
            try:
                validate_document(receipt, json.loads(schema_path.read_text()))
            except (OSError, json.JSONDecodeError, SchemaValidationError) as error:
                raise PersistentReceiptError(
                    "terminal receipt contract is corrupt"
                ) from error
        return value

    def _persist_new(self, value: Mapping[str, Any]) -> None:
        self._store.create(
            "command-runner/receipts/%s.json" % value["idempotency_key_digest"],
            _canonical(value),
        )

    def _persist(self, value: Mapping[str, Any]) -> None:
        self._store.replace(
            "command-runner/receipts/%s.json" % value["idempotency_key_digest"],
            _canonical(value),
        )

    def _capture(
        self, stream: str, data: bytes, policy: Mapping[str, Any]
    ) -> dict[str, Any]:
        retained = data[: int(policy["output_limit_bytes"])]
        digest = hashlib.sha256(retained).hexdigest()
        relative = "captures/%s/%s.bin" % (stream, digest)
        self._store.replace("command-runner/" + relative, retained)
        return {
            "stream": stream,
            "original_byte_count": len(data),
            "retained_byte_count": len(retained),
            "integrity": "truncated" if len(data) > len(retained) else "complete",
            "digest": "sha256:" + digest,
            "sensitivity": policy["sensitivity"],
            "report_safe_ref": relative,
        }


class MacOSTaskProcessBroker:
    """Launch and authenticate one E2 task without trusting Worker observations."""

    def __init__(
        self,
        state_root: str | Path,
        *,
        capability: bytes | None = None,
        boot_id: str | None = None,
    ) -> None:
        self.state_root = Path(state_root)
        if not self.state_root.is_absolute():
            raise MacOSTaskProcessError("broker state root must be absolute")
        if posixpath.normpath(str(self.state_root)) != str(self.state_root):
            raise MacOSTaskProcessError("broker state root must be canonical")
        _validate_existing_absolute_prefix(str(self.state_root))
        # Construction is write-free.  execute() binds the destination to the
        # E2 package's selected physical project before the first state byte.
        self._state: _AnchoredBrokerState | None = None
        self._workspace_identity: str | None = None
        self._capability = capability or secrets.token_bytes(32)
        if not isinstance(self._capability, bytes) or len(self._capability) < 32:
            raise MacOSTaskProcessError(
                "broker capability must contain at least 256 bits"
            )
        self.boot_id = boot_id
        self.broker_id = "sha256:" + hashlib.sha256(self._capability).hexdigest()

    def close(self) -> None:
        state = getattr(self, "_state", None)
        if state is not None:
            state.close()
            self._state = None

    def __del__(self) -> None:
        try:
            self.close()
        except OSError:
            pass

    def execute(
        self,
        e2_package: Mapping[str, Any],
        runner_package: Mapping[str, Any],
        policy: Mapping[str, Any],
        *,
        probes: Sequence[Mapping[str, str]],
    ) -> dict[str, Any]:
        release = self._release(e2_package)
        state = self._state_for_release(release)
        self._validate_runner_binding(e2_package, runner_package)
        operations = [self._probe(release, item) for item in probes]
        if (
            not operations
            or not any(item["expected"] == "allowed" for item in operations)
            or not any(item["expected"] == "denied" for item in operations)
        ):
            raise MacOSTaskProcessError(
                "trusted boundary requires both allowed and denied OS probes"
            )
        if any(item["result"] != item["expected"] for item in operations):
            raise MacOSTaskProcessError(
                f"macOS positive read boundary failed its trusted probes: {operations}"
            )

        adapter = _SandboxedProcessAdapter(release)
        runner = _AnchoredReceiptRunner(
            state, adapter=adapter, boot_id=self.boot_id
        )
        terminal = runner.run(runner_package, policy)
        reused = runner.run(runner_package, policy)
        if (
            terminal != reused
            or adapter.spawn_count != 1
            or adapter.last_identity is None
        ):
            raise MacOSTaskProcessError(
                "sandboxed terminal receipt did not reuse one exact process"
            )
        recovery = {
            "schema": "e3-recovery-evidence/v1",
            "terminal_reused": True,
            "spawn_count": adapter.spawn_count,
            "idempotency_key": runner_package["idempotency_key"],
            "closure_digest": runner_package["execution_closure"]["closure_digest"],
            "terminal_receipt": copy.deepcopy(terminal),
        }
        recovery["evidence_digest"] = _digest(recovery)
        identity = copy.deepcopy(adapter.last_identity)
        identity["boot_identity"] = runner.boot_id
        receipt = self._seal(e2_package, release, identity, operations, terminal)
        return {
            "terminal_receipt": terminal,
            "recovery_receipt": recovery,
            "isolation_receipt_ref": {
                "schema": "macos-task-process-receipt-ref/v2",
                "receipt_id": receipt["receipt_id"],
                "receipt_digest": receipt["receipt_digest"],
                "broker_id": self.broker_id,
                "threat_profile_digest": _PRACTICAL_THREAT_PROFILE_DIGEST,
            },
        }

    def verify_for_e3(
        self, e2_package: Mapping[str, Any], receipt_ref: Mapping[str, Any]
    ) -> dict[str, Any]:
        release = self._release(e2_package)
        state = self._state_for_release(release, create=False)
        expected_keys = {
            "schema",
            "receipt_id",
            "receipt_digest",
            "broker_id",
            "threat_profile_digest",
        }
        if (
            not isinstance(receipt_ref, Mapping)
            or set(receipt_ref) != expected_keys
            or receipt_ref.get("schema") != "macos-task-process-receipt-ref/v2"
            or receipt_ref.get("broker_id") != self.broker_id
            or receipt_ref.get("threat_profile_digest")
            != _PRACTICAL_THREAT_PROFILE_DIGEST
        ):
            raise MacOSTaskProcessError(
                "E3 isolation receipt ref is not parent-broker-owned"
            )
        _validate_macos_document(
            receipt_ref, "macos-task-process-receipt-ref-v2.schema.json"
        )
        receipt_id = receipt_ref.get("receipt_id")
        if (
            not isinstance(receipt_id, str)
            or not receipt_id.startswith("sha256:")
            or len(receipt_id) != 71
        ):
            raise MacOSTaskProcessError("E3 isolation receipt identity is malformed")
        receipt = self._read_authenticated_receipt(state, receipt_id)
        _validate_macos_document(
            receipt, "macos-task-process-receipt-v2.schema.json"
        )
        receipt = self._authenticate_receipt(receipt)
        unsigned = {
            key: copy.deepcopy(item)
            for key, item in receipt.items()
            if key != "receipt_digest"
        }
        if (
            receipt.get("receipt_digest") != _digest(unsigned)
            or receipt_ref.get("receipt_digest") != receipt.get("receipt_digest")
            or receipt.get("receipt_id") != receipt_id
            or receipt.get("broker_id") != self.broker_id
            or receipt.get("e2_package_digest") != _digest(e2_package)
            or receipt.get("execution_closure_ref")
            != e2_package.get("execution_closure_ref")
            or receipt.get("os_isolation_enforced") is not True
            or receipt.get("caller_observations_accepted") is not False
            or receipt.get("threat_profile") != release.get("threat_profile")
            or receipt.get("threat_profile_digest")
            != release.get("threat_profile_digest")
            or receipt.get("state_writer_boundary")
            != release.get("state_writer_boundary")
            or receipt.get("limitations_acknowledged") is not True
        ):
            raise MacOSTaskProcessError(
                "isolation receipt does not bind the exact E2 package and closure"
            )
        return copy.deepcopy(receipt)

    def verify_historical_v1(
        self, e2_package: Mapping[str, Any], receipt_ref: Mapping[str, Any]
    ) -> dict[str, Any]:
        """Read one exact v1 pair without upgrading it to practical acceptance."""
        release = self._legacy_release(e2_package)
        state = self._state_for_release(release, create=False)
        expected_keys = {"schema", "receipt_id", "receipt_digest", "broker_id"}
        if (
            not isinstance(receipt_ref, Mapping)
            or set(receipt_ref) != expected_keys
            or receipt_ref.get("schema") != "macos-task-process-receipt-ref/v1"
            or receipt_ref.get("broker_id") != self.broker_id
        ):
            raise MacOSTaskProcessError(
                "historical v1 receipt ref is not one exact parent-broker pair"
            )
        receipt_id = receipt_ref.get("receipt_id")
        if (
            not isinstance(receipt_id, str)
            or not receipt_id.startswith("sha256:")
            or len(receipt_id) != 71
        ):
            raise MacOSTaskProcessError(
                "historical v1 isolation receipt identity is malformed"
            )
        receipt = self._authenticate_receipt(
            self._read_authenticated_receipt(state, receipt_id)
        )
        unsigned = {
            key: copy.deepcopy(item)
            for key, item in receipt.items()
            if key != "receipt_digest"
        }
        if (
            receipt.get("schema") != "macos-task-process-receipt/v1"
            or receipt.get("receipt_digest") != _digest(unsigned)
            or receipt_ref.get("receipt_digest") != receipt.get("receipt_digest")
            or receipt.get("receipt_id") != receipt_id
            or receipt.get("broker_id") != self.broker_id
            or receipt.get("e2_package_digest") != _digest(e2_package)
            or receipt.get("execution_closure_ref")
            != e2_package.get("execution_closure_ref")
            or receipt.get("sandbox_identity", {}).get("profile_digest")
            != release.get("profile_digest")
            or receipt.get("sandbox_identity", {}).get("release_digest")
            != release.get("release_digest")
            or receipt.get("os_isolation_enforced") is not True
            or receipt.get("caller_observations_accepted") is not False
            or any(
                key in receipt
                for key in (
                    "threat_profile",
                    "threat_profile_digest",
                    "state_writer_boundary",
                    "limitations_acknowledged",
                )
            )
        ):
            raise MacOSTaskProcessError(
                "historical v1 receipt does not bind its exact v1 package"
            )
        return {
            "schema": "macos-task-process-historical-v1-reader/v1",
            "status": "legacy-unprofiled",
            "practical_profile_accepted": False,
            "e2_package_digest": _digest(e2_package),
            "receipt_ref": copy.deepcopy(dict(receipt_ref)),
            "receipt": copy.deepcopy(receipt),
        }

    def _read_authenticated_receipt(
        self, state: _AnchoredBrokerState, receipt_id: str
    ) -> dict[str, Any]:
        path = (
            "isolation-receipts/"
            + receipt_id.removeprefix("sha256:")
            + ".json"
        )
        try:
            raw, mode = state.read(path)
            if mode != 0o600:
                raise MacOSTaskProcessError(
                    "parent-owned isolation receipt mode is unsafe"
                )
            receipt = json.loads(raw)
        except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
            raise MacOSTaskProcessError(
                "parent-owned isolation receipt is unavailable"
            ) from error
        if not isinstance(receipt, Mapping):
            raise MacOSTaskProcessError(
                "parent-owned isolation receipt is unavailable"
            )
        return copy.deepcopy(dict(receipt))

    def _authenticate_receipt(
        self, receipt: Mapping[str, Any]
    ) -> dict[str, Any]:
        authenticated = copy.deepcopy(dict(receipt))
        mac = authenticated.pop("broker_mac", None)
        expected_mac = hmac.new(
            self._capability, _canonical(authenticated), hashlib.sha256
        ).hexdigest()
        if not isinstance(mac, str) or not hmac.compare_digest(mac, expected_mac):
            raise MacOSTaskProcessError(
                "parent-owned isolation receipt authentication failed"
            )
        return authenticated

    def _state_for_release(
        self, release: Mapping[str, Any], *, create: bool = True
    ) -> _AnchoredBrokerState:
        workspace = release.get("workspace_identity")
        if not isinstance(workspace, str):
            raise MacOSTaskProcessError("E2 release omits selected project identity")
        if self._state is None:
            self._state = _AnchoredBrokerState(
                workspace, str(self.state_root), create=create
            )
            self._workspace_identity = workspace
        elif self._workspace_identity != workspace:
            raise MacOSTaskProcessError(
                "broker instance cannot cross selected project identities"
            )
        return self._state

    def _release(self, package: Mapping[str, Any]) -> dict[str, Any]:
        if not isinstance(package, Mapping):
            raise MacOSTaskProcessError("E2 package is missing")
        release = package.get("task_process_release")
        if (
            not isinstance(release, Mapping)
            or release.get("schema") != "macos-task-process-release/v2"
            or release.get("release_rule") != "broker-only"
        ):
            raise MacOSTaskProcessError(
                "E2 package did not select broker-only macOS execution"
            )
        unsigned = {
            key: copy.deepcopy(item)
            for key, item in release.items()
            if key != "release_digest"
        }
        if release.get("release_digest") != _digest(unsigned) or release.get(
            "broker_state_root"
        ) != str(self.state_root):
            raise MacOSTaskProcessError("E2 task-process release identity drifted")
        rebuilt = compile_macos_task_process_release(
            package.get("execution_closure"),
            broker_state_root=release.get("broker_state_root"),
            system_read_roots=release.get("system_read_roots"),
            runtime_read_roots=release.get("runtime_read_roots"),
        )
        if rebuilt != release or release.get("execution_closure_ref") != package.get(
            "execution_closure_ref"
        ):
            raise MacOSTaskProcessError(
                "E2 task-process profile is not the canonical positive allow-list"
            )
        return copy.deepcopy(dict(release))

    def _legacy_release(self, package: Mapping[str, Any]) -> dict[str, Any]:
        if not isinstance(package, Mapping):
            raise MacOSTaskProcessError("historical v1 E2 package is missing")
        release = package.get("task_process_release")
        if (
            not isinstance(release, Mapping)
            or release.get("schema") != "macos-task-process-release/v1"
            or release.get("release_rule") != "broker-only"
        ):
            raise MacOSTaskProcessError(
                "historical reader requires one exact v1 broker release"
            )
        unsigned = {
            key: copy.deepcopy(item)
            for key, item in release.items()
            if key != "release_digest"
        }
        if release.get("release_digest") != _digest(unsigned) or release.get(
            "broker_state_root"
        ) != str(self.state_root):
            raise MacOSTaskProcessError("historical v1 release identity drifted")
        rebuilt = _compile_macos_task_process_release_v1(
            package.get("execution_closure"),
            broker_state_root=release.get("broker_state_root"),
            system_read_roots=release.get("system_read_roots"),
            runtime_read_roots=release.get("runtime_read_roots"),
        )
        if rebuilt != release or release.get("execution_closure_ref") != package.get(
            "execution_closure_ref"
        ):
            raise MacOSTaskProcessError(
                "historical v1 release is not its canonical exact-pair input"
            )
        return copy.deepcopy(dict(release))

    @staticmethod
    def _validate_runner_binding(
        e2_package: Mapping[str, Any], runner: Mapping[str, Any]
    ) -> None:
        if (
            not isinstance(runner, Mapping)
            or runner.get("execution_closure") != e2_package.get("execution_closure")
            or runner.get("execution_closure_ref")
            != e2_package.get("execution_closure_ref")
            or runner.get("idempotency_key") != e2_package.get("idempotency_key")
        ):
            raise MacOSTaskProcessError(
                "runner input is not the exact E2 package closure"
            )

    @staticmethod
    def _probe(release: Mapping[str, Any], value: Mapping[str, str]) -> dict[str, Any]:
        keys = {"operation", "path", "expected"}
        if (
            not isinstance(value, Mapping)
            or set(value) != keys
            or value.get("operation") not in {"content-read", "stat", "enumerate"}
            or value.get("expected") not in {"allowed", "denied"}
        ):
            raise MacOSTaskProcessError("trusted probe is malformed")
        path_value = value.get("path")
        if (
            not isinstance(path_value, str)
            or not path_value.startswith("/")
            or posixpath.normpath(path_value) != path_value
            or not Path(path_value).exists()
        ):
            raise MacOSTaskProcessError(
                "trusted probe path must be one existing canonical absolute path"
            )
        allowed = (
            release["project_read_roots"]
            + release["system_read_roots"]
            + release["runtime_read_roots"]
        )
        inside = any(_inside(path_value, root) for root in allowed) and not _inside(
            path_value, release["broker_state_root"]
        )
        if (value["expected"] == "allowed") != inside:
            raise MacOSTaskProcessError(
                "trusted probe expectation contradicts the positive allow-list"
            )
        tool = {
            "content-read": "/bin/cat",
            "stat": "/usr/bin/stat",
            "enumerate": "/bin/ls",
        }[value["operation"]]
        result = subprocess.run(
            [release["engine"], "-p", release["profile"], "--", tool, path_value],
            capture_output=True,
            timeout=10,
            check=False,
        )
        observed = "allowed" if result.returncode == 0 else "denied"
        return {
            "operation": value["operation"],
            "path": path_value,
            "expected": value["expected"],
            "result": observed,
            "exit_code": result.returncode,
        }

    def _seal(
        self,
        package: Mapping[str, Any],
        release: Mapping[str, Any],
        identity: Mapping[str, Any],
        operations: Sequence[Mapping[str, Any]],
        terminal: Mapping[str, Any],
    ) -> dict[str, Any]:
        receipt_id = _digest(
            {
                "e2_package_digest": _digest(package),
                "terminal_receipt_digest": terminal.get("receipt_digest"),
                "profile_digest": release["profile_digest"],
                "threat_profile_digest": release["threat_profile_digest"],
                "release_digest": release["release_digest"],
            }
        )
        receipt = {
            "schema": "macos-task-process-receipt/v2",
            "receipt_id": receipt_id,
            "broker_id": self.broker_id,
            "e2_package_digest": _digest(package),
            "execution_closure_ref": copy.deepcopy(package["execution_closure_ref"]),
            "command_identity": {
                "argv": copy.deepcopy(release["command"]["argv"]),
                "argv_digest": _digest(release["command"]["argv"]),
                "cwd": release["command"]["cwd"],
                "environment_digest": _digest(
                    package["execution_closure"]["environment"]
                ),
            },
            "process_identity": copy.deepcopy(dict(identity)),
            "sandbox_identity": {
                "engine": release["engine"],
                "profile_imports": copy.deepcopy(release["profile_imports"]),
                "profile_digest": release["profile_digest"],
                "release_digest": release["release_digest"],
            },
            "declared_roots": {
                "project": copy.deepcopy(release["project_read_roots"]),
                "system": copy.deepcopy(release["system_read_roots"]),
                "runtime": copy.deepcopy(release["runtime_read_roots"]),
                "write": copy.deepcopy(release["write_roots"]),
            },
            "operations": copy.deepcopy(list(operations)),
            "terminal_receipt_digest": terminal.get("receipt_digest"),
            "os_isolation_enforced": True,
            "enforcement": "macos-positive-read-allow-list",
            "caller_observations_accepted": False,
            "threat_profile": copy.deepcopy(release["threat_profile"]),
            "threat_profile_digest": release["threat_profile_digest"],
            "state_writer_boundary": copy.deepcopy(
                release["state_writer_boundary"]
            ),
            "limitations_acknowledged": True,
        }
        receipt["receipt_digest"] = _digest(receipt)
        receipt["broker_mac"] = hmac.new(
            self._capability, _canonical(receipt), hashlib.sha256
        ).hexdigest()
        _validate_macos_document(
            receipt, "macos-task-process-receipt-v2.schema.json"
        )
        if self._state is None:
            raise MacOSTaskProcessError("broker state is not anchored")
        self._state.create(
            "isolation-receipts/"
            + receipt_id.removeprefix("sha256:")
            + ".json",
            _canonical(receipt),
        )
        return receipt


__all__ = [
    "MacOSTaskProcessBroker",
    "MacOSTaskProcessError",
    "compile_macos_task_process_release",
]
