"""Explicit, narrow, rollbackable Codex user-config policy updates.

The Home Manager module deliberately does not own ``~/.codex/config.toml``.
This module is the separate operator-invoked seam for the two portable model
keys.  It preserves every other byte except the two assignment values.
"""

from __future__ import annotations

import hashlib
import json
import os
import re
import stat
import tempfile
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, Mapping, Optional, Tuple

try:  # Python 3.11+; the Nix-distributed runtime always provides this.
    import tomllib  # type: ignore
except ImportError:  # pragma: no cover - exercised by the macOS Python 3.9 test host.
    tomllib = None  # type: ignore


class ConfigError(RuntimeError):
    """The requested config operation is unsafe or conflicts with live state."""


_MANAGED_KEYS = ("model", "model_reasoning_effort")
_KEY_PATTERN = re.compile(
    r"^(?P<indent>\s*)(?P<key>model|model_reasoning_effort|\"model\"|\"model_reasoning_effort\"|'model'|'model_reasoning_effort')(?P<between>\s*=\s*)(?P<value>.*?)(?P<newline>\r?\n)?$"
)


@dataclass(frozen=True)
class _FileSnapshot:
    content: bytes
    identity: str


def _identity(metadata: os.stat_result) -> str:
    return "device=%d,inode=%d" % (metadata.st_dev, metadata.st_ino)


def _state(metadata: os.stat_result) -> Tuple[int, int, int, int, int]:
    return (
        metadata.st_dev,
        metadata.st_ino,
        metadata.st_size,
        metadata.st_mtime_ns,
        metadata.st_ctime_ns,
    )


def _digest(data: bytes) -> str:
    return "sha256:" + hashlib.sha256(data).hexdigest()


def _unquote_key(value: str) -> str:
    return value[1:-1] if value[:1] in {"'", '"'} else value


def _comment_suffix(value: str) -> str:
    quote: Optional[str] = None
    escaped = False
    for index, char in enumerate(value):
        if escaped:
            escaped = False
            continue
        if char == "\\" and quote == '"':
            escaped = True
            continue
        if quote:
            if char == quote:
                quote = None
            continue
        if char in {"'", '"'}:
            quote = char
        elif char == "#":
            start = index
            while start and value[start - 1] in {" ", "\t"}:
                start -= 1
            return value[start:]
    return ""


def _fallback_managed_values(text: str) -> Dict[str, str]:
    """Parse only top-level managed string keys on pre-3.11 test hosts.

    Production execution is packaged with modern Nix Python and therefore
    uses ``tomllib`` for whole-document syntax validation.  This fallback is
    intentionally narrow and never attempts to understand unmanaged values.
    """

    values: Dict[str, str] = {}
    in_table = False
    for line in text.splitlines():
        stripped = line.strip()
        if stripped.startswith("["):
            in_table = True
        if in_table:
            continue
        match = _KEY_PATTERN.match(line)
        if not match:
            continue
        key = _unquote_key(match.group("key"))
        if key in values:
            raise ConfigError("duplicate managed key: %s" % key)
        raw = match.group("value")
        comment = _comment_suffix(raw)
        if comment:
            raw = raw[: -len(comment)]
        try:
            parsed = json.loads(raw.strip())
        except (ValueError, json.JSONDecodeError) as exc:
            raise ConfigError("managed key %s is not a basic TOML string" % key) from exc
        if not isinstance(parsed, str):
            raise ConfigError("managed key %s must be a string" % key)
        values[key] = parsed
    return values


def _parse_managed_values(text: str) -> Dict[str, Optional[str]]:
    if tomllib is not None:
        try:
            parsed = tomllib.loads(text)
        except Exception as exc:
            raise ConfigError("config.toml syntax validation failed: %s" % exc) from exc
        values = {key: parsed.get(key) for key in _MANAGED_KEYS}
        for key, value in values.items():
            if value is not None and not isinstance(value, str):
                raise ConfigError("managed key %s must be a string" % key)
        return values
    fallback = _fallback_managed_values(text)
    return {key: fallback.get(key) for key in _MANAGED_KEYS}


def _render(text: str, desired: Mapping[str, str]) -> str:
    # The renderer intentionally handles only the line-oriented TOML subset that
    # it can preserve byte-for-byte.  A line beginning with ``[`` is ambiguous
    # inside a multiline string, so refuse every multiline-string document
    # instead of silently treating string content as a table boundary.
    if '"""' in text or "'''" in text:
        raise ConfigError(
            "multiline TOML strings are outside the safe config update subset"
        )
    newline = "\r\n" if "\r\n" in text else "\n"
    lines = text.splitlines(keepends=True)
    seen = set()
    in_table = False
    rendered = []
    insertion_index: Optional[int] = None
    for line in lines:
        stripped = line.lstrip()
        if stripped.startswith("[") and insertion_index is None:
            insertion_index = len(rendered)
            in_table = True
        match = None if in_table else _KEY_PATTERN.match(line)
        if match:
            key = _unquote_key(match.group("key"))
            if key in seen:
                raise ConfigError("duplicate managed key: %s" % key)
            seen.add(key)
            suffix = _comment_suffix(match.group("value"))
            line_ending = match.group("newline") or ""
            line = "%s%s%s%s%s%s" % (
                match.group("indent"),
                match.group("key"),
                match.group("between"),
                json.dumps(desired[key]),
                suffix,
                line_ending,
            )
        rendered.append(line)
    missing = [key for key in _MANAGED_KEYS if key not in seen]
    additions = ["%s = %s%s" % (key, json.dumps(desired[key]), newline) for key in missing]
    if additions:
        index = insertion_index if insertion_index is not None else len(rendered)
        if index and rendered[index - 1] and not rendered[index - 1].endswith(("\n", "\r")):
            rendered[index - 1] += newline
        rendered[index:index] = additions
    result = "".join(rendered)
    if not text and result:
        result = result.rstrip("\r\n") + newline
    parsed = _parse_managed_values(result)
    mismatched = [key for key in _MANAGED_KEYS if parsed.get(key) != desired[key]]
    if mismatched:
        raise ConfigError(
            "managed-key postcondition failed for: %s" % ", ".join(mismatched)
        )
    return result


class CodexConfigManager:
    """Plan, explicitly apply, diagnose, and roll back portable model keys."""

    def __init__(self, config_path: Path, policy_path: Path):
        self.config_path = Path(config_path)
        self.policy_path = Path(policy_path)

    def _policy(self) -> Tuple[Dict[str, str], Dict[str, Any]]:
        try:
            policy = json.loads(self.policy_path.read_text())
        except (OSError, ValueError) as exc:
            raise ConfigError("model policy is unreadable or invalid") from exc
        if not isinstance(policy, dict) or policy.get("schema") != "agent-model-policy/v1":
            raise ConfigError("model policy schema is unsupported")
        default = policy.get("default")
        if not isinstance(default, dict) or set(default) != {"model", "reasoning_effort"}:
            raise ConfigError("model policy default is malformed")
        if not all(isinstance(value, str) and value for value in default.values()):
            raise ConfigError("model policy default values must be nonempty strings")
        if policy.get("automatic_fallback") is not False:
            raise ConfigError("automatic model fallback must remain disabled")
        if policy.get("config_apply") != "explicit-user-command-only":
            raise ConfigError("config apply policy is not explicit-only")
        desired = {
            "model": default["model"],
            "model_reasoning_effort": default["reasoning_effort"],
        }
        return desired, policy

    @staticmethod
    def _snapshot_existing(path: Path) -> _FileSnapshot:
        resolved = path.resolve(strict=False)
        if str(resolved) == "/nix/store" or str(resolved).startswith("/nix/store/"):
            raise ConfigError("refusing to manage a path in /nix/store")
        flags = os.O_RDONLY | getattr(os, "O_NOFOLLOW", 0)
        try:
            descriptor = os.open(str(path), flags)
        except OSError as exc:
            if path.is_symlink():
                raise ConfigError("refusing to read or replace a config symlink") from exc
            raise
        try:
            before = os.fstat(descriptor)
            if not stat.S_ISREG(before.st_mode):
                raise ConfigError("config path is not a regular file")
            if before.st_uid != os.getuid():
                raise ConfigError("config path is not owned by the current user")
            if not before.st_mode & stat.S_IWUSR:
                raise ConfigError("config path is not user-writable")
            with os.fdopen(descriptor, "rb") as stream:
                descriptor = -1
                content = stream.read()
                after = os.fstat(stream.fileno())
        finally:
            if descriptor >= 0:
                os.close(descriptor)
        if _state(before) != _state(after):
            raise ConfigError("config changed while it was being read")
        try:
            path_metadata = os.stat(str(path), follow_symlinks=False)
        except FileNotFoundError as exc:
            raise ConfigError("config path changed while it was being read") from exc
        if stat.S_ISLNK(path_metadata.st_mode):
            raise ConfigError("refusing to read or replace a config symlink")
        if _identity(path_metadata) != _identity(after):
            raise ConfigError("config identity changed while it was being read")
        return _FileSnapshot(content=content, identity=_identity(after))

    @classmethod
    def _safe_existing(cls, path: Path) -> bytes:
        return cls._snapshot_existing(path).content

    def _current(self) -> Tuple[bool, Optional[_FileSnapshot]]:
        resolved = self.config_path.resolve(strict=False)
        if str(resolved) == "/nix/store" or str(resolved).startswith("/nix/store/"):
            raise ConfigError("refusing to manage a path in /nix/store")
        try:
            return True, self._snapshot_existing(self.config_path)
        except FileNotFoundError:
            if self.config_path.is_symlink():
                raise ConfigError("refusing to read or replace a config symlink")
            return False, None

    def _before_commit(self, operation: str, path: Path) -> None:
        """Deterministic interleaving seam; production intentionally does nothing."""

    @classmethod
    def _require_unchanged(
        cls, path: Path, expected: _FileSnapshot, *, operation: str
    ) -> _FileSnapshot:
        try:
            current = cls._snapshot_existing(path)
        except FileNotFoundError as exc:
            raise ConfigError("config disappeared during %s" % operation) from exc
        if current.identity != expected.identity:
            raise ConfigError("config identity changed during %s" % operation)
        if current.content != expected.content:
            raise ConfigError("config changed during %s" % operation)
        return current

    @staticmethod
    def _require_absent(path: Path, *, operation: str) -> None:
        try:
            os.stat(str(path), follow_symlinks=False)
        except FileNotFoundError:
            return
        raise ConfigError("%s appeared during %s" % (path.name, operation))

    @staticmethod
    def _fsync_directory(path: Path) -> None:
        directory = os.open(str(path), os.O_RDONLY)
        try:
            os.fsync(directory)
        finally:
            os.close(directory)

    @staticmethod
    def _prepare_write(path: Path, content: bytes) -> Path:
        path.parent.mkdir(parents=True, exist_ok=True)
        descriptor, temporary = tempfile.mkstemp(
            prefix=".%s." % path.name, dir=str(path.parent)
        )
        temporary_path = Path(temporary)
        try:
            os.fchmod(descriptor, 0o600)
            with os.fdopen(descriptor, "wb") as stream:
                descriptor = -1
                stream.write(content)
                stream.flush()
                os.fsync(stream.fileno())
            return temporary_path
        except Exception:
            if descriptor >= 0:
                os.close(descriptor)
            if temporary_path.exists():
                temporary_path.unlink()
            raise

    @classmethod
    def _publish_exclusive(
        cls, temporary_path: Path, path: Path, *, collision_message: str
    ) -> None:
        try:
            # Hard-link publication is an atomic create-if-absent operation on
            # the same filesystem; unlike rename it cannot replace a winner.
            os.link(str(temporary_path), str(path))
        except FileExistsError as exc:
            raise ConfigError(collision_message) from exc
        os.unlink(temporary_path)
        cls._fsync_directory(path.parent)

    @classmethod
    def _publish_replace(cls, temporary_path: Path, path: Path) -> None:
        os.replace(str(temporary_path), str(path))
        cls._fsync_directory(path.parent)

    @classmethod
    def _remove_exact(cls, path: Path, expected: _FileSnapshot, *, operation: str) -> None:
        # As with replace, POSIX provides no path unlink-if-inode-and-bytes-match
        # primitive for a non-cooperating writer.  Revalidate immediately before
        # unlink and fail closed on every observable drift.
        cls._require_unchanged(path, expected, operation=operation)
        os.unlink(path)
        cls._fsync_directory(path.parent)

    def plan(self) -> Dict[str, Any]:
        desired, policy = self._policy()
        exists, before_snapshot = self._current()
        before = before_snapshot.content if before_snapshot is not None else b""
        try:
            text = before.decode("utf-8")
        except UnicodeDecodeError as exc:
            raise ConfigError("config.toml must be UTF-8") from exc
        current = _parse_managed_values(text)
        rendered = _render(text, desired)
        changes = {
            key: {"current": current.get(key), "desired": desired[key]}
            for key in _MANAGED_KEYS
            if current.get(key) != desired[key]
        }
        return {
            "schema": "codex-config-plan/v1",
            "path": str(self.config_path),
            "exists": exists,
            "before_digest": _digest(before),
            "after_digest": _digest(rendered.encode("utf-8")),
            "changed": bool(changes),
            "changed_keys": list(changes),
            "changes": changes,
            "runtime_compatibility": policy.get("runtime_compatibility", "unverified"),
            "automatic_fallback": False,
        }

    def apply(
        self,
        *,
        expected_before_digest: str,
        runtime_compatibility_confirmed: bool,
        timestamp: Optional[str] = None,
    ) -> Dict[str, Any]:
        if runtime_compatibility_confirmed is not True:
            raise ConfigError("runtime compatibility must be confirmed before apply")
        desired, _ = self._policy()
        exists, before_snapshot = self._current()
        before = before_snapshot.content if before_snapshot is not None else b""
        if _digest(before) != expected_before_digest:
            raise ConfigError("config changed after plan; rerun plan")
        try:
            text = before.decode("utf-8")
        except UnicodeDecodeError as exc:
            raise ConfigError("config.toml must be UTF-8") from exc
        rendered = _render(text, desired).encode("utf-8")
        if rendered == before:
            return {
                "schema": "codex-config-apply-receipt/v1",
                "changed": False,
                "before_digest": _digest(before),
                "after_digest": _digest(before),
                "after_identity": (
                    before_snapshot.identity if before_snapshot is not None else None
                ),
                "backup_path": None,
                "rollback_action": None,
                "rollback": None,
            }
        self.config_path.parent.mkdir(parents=True, exist_ok=True)
        backup_path: Optional[Path] = None
        backup_snapshot: Optional[_FileSnapshot] = None
        prepared: Optional[Path] = None
        config_publish_attempted = False
        try:
            if exists:
                if before_snapshot is None:  # pragma: no cover - type narrowing.
                    raise ConfigError("existing config snapshot is unavailable")
                stamp = timestamp or datetime.now(timezone.utc).strftime(
                    "%Y%m%dT%H%M%SZ"
                )
                backup_path = self.config_path.with_name(
                    self.config_path.name + ".backup." + stamp
                )
                backup_temp = self._prepare_write(backup_path, before)
                try:
                    self._before_commit("backup-create", backup_path)
                    self._publish_exclusive(
                        backup_temp,
                        backup_path,
                        collision_message="backup path already exists",
                    )
                finally:
                    if backup_temp.exists():
                        backup_temp.unlink()
                backup_snapshot = self._snapshot_existing(backup_path)

            prepared = self._prepare_write(self.config_path, rendered)
            if before_snapshot is None:
                self._before_commit("config-create", self.config_path)
                self._require_absent(self.config_path, operation="apply")
                self._publish_exclusive(
                    prepared,
                    self.config_path,
                    collision_message="config appeared during apply",
                )
            else:
                self._before_commit("config-apply", self.config_path)
                self._require_unchanged(
                    self.config_path, before_snapshot, operation="apply"
                )
                # POSIX has no non-cooperating compare-and-swap-by-inode for a
                # path.  This is therefore a minimal fail-closed protocol: the
                # exact bytes and identity are checked immediately before the
                # atomic publish.  It does not claim to eliminate the remaining
                # validation-to-rename instruction window.
                config_publish_attempted = True
                self._publish_replace(prepared, self.config_path)
            prepared = None
        except Exception as exc:
            if (
                not config_publish_attempted
                and backup_path is not None
                and backup_snapshot is not None
            ):
                try:
                    self._remove_exact(
                        backup_path,
                        backup_snapshot,
                        operation="failed-apply backup cleanup",
                    )
                except ConfigError as cleanup_exc:
                    raise ConfigError(
                        "%s; backup cleanup also stopped: %s" % (exc, cleanup_exc)
                    ) from exc
            raise
        finally:
            if prepared is not None and prepared.exists():
                prepared.unlink()

        after_snapshot = self._snapshot_existing(self.config_path)
        after = after_snapshot.content
        if after != rendered:
            raise ConfigError("post-write config readback failed")
        if backup_path is not None:
            rollback_action: Dict[str, Any] = {
                "kind": "restore_backup",
                "backup_path": str(backup_path),
                "expected_current_digest": _digest(after),
                "expected_current_identity": after_snapshot.identity,
            }
            rollback = (
                "agent-workflow-config rollback --backup %s "
                "--expected-current-digest %s --expected-current-identity %s"
                % (backup_path, _digest(after), after_snapshot.identity)
            )
        else:
            rollback_action = {
                "kind": "restore_absent",
                "expected_current_digest": _digest(after),
                "expected_current_identity": after_snapshot.identity,
            }
            rollback = (
                "agent-workflow-config rollback --restore-absent "
                "--expected-current-digest %s --expected-current-identity %s"
                % (_digest(after), after_snapshot.identity)
            )
        return {
            "schema": "codex-config-apply-receipt/v1",
            "changed": True,
            "before_digest": _digest(before),
            "after_digest": _digest(after),
            "after_identity": after_snapshot.identity,
            "backup_path": str(backup_path) if backup_path else None,
            "rollback_action": rollback_action,
            "rollback": rollback,
        }

    def doctor(self) -> Dict[str, Any]:
        desired, _ = self._policy()
        exists, current_snapshot = self._current()
        if not exists:
            return {
                "schema": "codex-config-doctor/v1",
                "exists": False,
                "matches_policy": False,
                "mismatched_keys": list(_MANAGED_KEYS),
            }
        if current_snapshot is None:  # pragma: no cover - type narrowing.
            raise ConfigError("existing config snapshot is unavailable")
        current_bytes = current_snapshot.content
        try:
            current = _parse_managed_values(current_bytes.decode("utf-8"))
        except UnicodeDecodeError as exc:
            raise ConfigError("config.toml must be UTF-8") from exc
        mismatched = [key for key in _MANAGED_KEYS if current.get(key) != desired[key]]
        return {
            "schema": "codex-config-doctor/v1",
            "exists": True,
            "matches_policy": not mismatched,
            "mismatched_keys": mismatched,
            "config_digest": _digest(current_bytes),
        }

    def rollback(
        self,
        backup_path: Path,
        *,
        expected_current_digest: str,
        expected_current_identity: str,
    ) -> Dict[str, Any]:
        backup_path = Path(backup_path)
        if backup_path.parent != self.config_path.parent or not backup_path.name.startswith(
            self.config_path.name + ".backup."
        ):
            raise ConfigError("backup path is outside the config backup namespace")
        backup_snapshot = self._snapshot_existing(backup_path)
        backup = backup_snapshot.content
        exists, current_snapshot = self._current()
        if (
            not exists
            or current_snapshot is None
            or _digest(current_snapshot.content) != expected_current_digest
            or current_snapshot.identity != expected_current_identity
        ):
            raise ConfigError("config changed after apply; refusing rollback")
        _parse_managed_values(backup.decode("utf-8"))
        prepared = self._prepare_write(self.config_path, backup)
        try:
            self._before_commit("config-rollback", self.config_path)
            self._require_unchanged(
                backup_path, backup_snapshot, operation="rollback source"
            )
            self._require_unchanged(
                self.config_path, current_snapshot, operation="rollback"
            )
            self._publish_replace(prepared, self.config_path)
            prepared = None
        finally:
            if prepared is not None and prepared.exists():
                prepared.unlink()
        restored_snapshot = self._snapshot_existing(self.config_path)
        restored = restored_snapshot.content
        if restored != backup:
            raise ConfigError("post-rollback config readback failed")
        return {
            "schema": "codex-config-rollback-receipt/v1",
            "restored_digest": _digest(restored),
            "restored_identity": restored_snapshot.identity,
            "restored_state": "present",
            "source_backup": str(backup_path),
        }

    def rollback_absent(
        self, *, expected_current_digest: str, expected_current_identity: str
    ) -> Dict[str, Any]:
        exists, current_snapshot = self._current()
        if (
            not exists
            or current_snapshot is None
            or _digest(current_snapshot.content) != expected_current_digest
            or current_snapshot.identity != expected_current_identity
        ):
            raise ConfigError("config changed after apply; refusing absent rollback")
        self._before_commit("config-remove", self.config_path)
        self._remove_exact(
            self.config_path,
            current_snapshot,
            operation="absent rollback",
        )
        try:
            os.stat(str(self.config_path), follow_symlinks=False)
        except FileNotFoundError:
            pass
        else:
            raise ConfigError("config path was recreated during absent rollback")
        return {
            "schema": "codex-config-rollback-receipt/v1",
            "restored_state": "absent",
            "removed_digest": expected_current_digest,
            "removed_identity": expected_current_identity,
            "source_backup": None,
        }


__all__ = ["CodexConfigManager", "ConfigError"]
