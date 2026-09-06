"""A6-to-A6R fixture migration helpers.

The converter accepts copies (JSON files or already-loaded mappings), records
their source digests, and writes only to a caller-supplied isolated kernel.
It deliberately does not discover or mutate the real Bootstrap Run.
"""

from __future__ import annotations

import copy
import fcntl
import hashlib
import json
import os
import re
import shutil
import tempfile
from contextlib import contextmanager
from pathlib import Path
from typing import Any, Dict, Iterator, Mapping, Optional, Union

from .control_kernel import ControlKernel, KernelError, canonical_digest


_DIGEST = re.compile(r"^sha256:[0-9a-f]{64}$")
_ID = re.compile(r"^[A-Za-z0-9][A-Za-z0-9_.:@/-]*$")


def _safe_legacy_value(
    value: Any,
    *,
    _allow_reference_paths: bool = False,
    _allow_context_telemetry: bool = False,
) -> Any:
    """Keep only durable legacy metadata while retaining its raw digest.

    Frozen A6 Run files contain authority/provenance labels that are useful
    for source authentication but are not allowed in the A6R durable context.
    Drop those fields from the copied projection; the original bytes remain
    authenticated by ``source_digest`` and are never rewritten.
    """

    if isinstance(value, Mapping):
        result: Dict[str, Any] = {}
        for key, child in value.items():
            normalized = re.sub(r"[^a-z0-9]", "", str(key).lower())
            telemetry_key = _allow_context_telemetry and normalized in {
                "tokenstatus", "tokencount"
            }
            if ControlKernel._is_forbidden_durable_key(normalized) and not telemetry_key:
                continue
            child_allows_reference_paths = normalized in {
                "path", "filepath", "filename", "sourcepath", "objectpath",
                "kernelpath", "targetpath", "outputpath", "artifactpath",
                "writescope", "outputpaths", "writepaths", "changedpaths",
            }
            child_allows_context_telemetry = normalized == "contextbudget"
            result[str(key)] = _safe_legacy_value(
                child,
                _allow_reference_paths=child_allows_reference_paths,
                _allow_context_telemetry=child_allows_context_telemetry,
            )
        return result
    if isinstance(value, list):
        return [
            _safe_legacy_value(
                child,
                _allow_reference_paths=_allow_reference_paths,
                _allow_context_telemetry=_allow_context_telemetry,
            )
            for child in value
        ]
    if isinstance(value, tuple):
        return [
            _safe_legacy_value(
                child,
                _allow_reference_paths=_allow_reference_paths,
                _allow_context_telemetry=_allow_context_telemetry,
            )
            for child in value
        ]
    if isinstance(value, str):
        try:
            ControlKernel._ensure_durable_payload(
                value,
                "legacy scalar",
                _allow_reference_paths=_allow_reference_paths,
            )
        except KernelError:
            return "[legacy opaque text omitted]"
        return value
    return copy.deepcopy(value)


class MigrationError(KernelError):
    """The copied fixture is invalid or the cutover target is unsafe."""


def _bytes_digest(data: bytes) -> str:
    return "sha256:" + hashlib.sha256(data).hexdigest()


def _fsync_directory(path: Path) -> None:
    """Flush a directory after an atomic conversion-tree rename."""

    try:
        descriptor = os.open(str(path), os.O_RDONLY)
    except OSError:
        return
    try:
        os.fsync(descriptor)
    finally:
        os.close(descriptor)


def _pointer_digest(value: Mapping[str, Any]) -> str:
    return canonical_digest({key: copy.deepcopy(item) for key, item in value.items() if key != "pointer_digest"})


def _validate_absolute_pointer_path(value: Any, label: str) -> str:
    """Validate the lexical path carried by a cutover pointer.

    Pointer paths are absolute because they identify an already-created local
    kernel, but they are still untrusted durable input.  Reject non-canonical
    separators and traversal before ``Path.resolve`` can erase the evidence
    of a redirected target.  Physical existence/canonicality is checked by
    ``_target_kernel`` while the target lock is held.
    """

    if not isinstance(value, str) or not value or "\x00" in value:
        raise MigrationError("%s is malformed" % label)
    if not value.startswith("/") or "\\" in value or "//" in value:
        raise MigrationError("%s must be a canonical absolute path" % label)
    if value != "/" and value.endswith("/"):
        raise MigrationError("%s must not have a trailing separator" % label)
    parts = value.split("/")
    if any(part in ("", ".", "..") for part in parts[1:]):
        raise MigrationError("%s contains traversal or an empty path segment" % label)
    if value == "/":
        raise MigrationError("%s must identify a kernel directory" % label)
    return value


def _yaml_scalar(value: str) -> Any:
    value = value.strip()
    if value in ("", "~", "null", "Null", "NULL"):
        return None
    if value in ("true", "True", "TRUE"):
        return True
    if value in ("false", "False", "FALSE"):
        return False
    if value.startswith(("'", '"')) and value[-1:] == value[0]:
        try:
            return json.loads(value) if value[0] == '"' else value[1:-1].replace("''", "'")
        except json.JSONDecodeError:
            return value[1:-1]
    if value.startswith("[") or value.startswith("{"):
        try:
            return json.loads(value)
        except json.JSONDecodeError:
            pass
    if re.fullmatch(r"-?[0-9]+", value):
        return int(value)
    if re.fullmatch(r"-?[0-9]+\.[0-9]+", value):
        return float(value)
    return value


def _yaml_key_value(content: str) -> tuple[str, str]:
    # A6 fixtures use ordinary ``key: value`` mappings.  URLs and quoted
    # values are preserved by splitting only at the first mapping colon.
    match = re.match(r"^([^:]+):(.*)$", content)
    if not match:
        raise MigrationError("unsupported YAML mapping: %s" % content)
    return match.group(1).strip(), match.group(2).strip()


def _minimal_yaml(text: str) -> Dict[str, Any]:
    """Parse the small YAML subset used by copied A6 state fixtures.

    This keeps the converter dependency-free.  It supports indentation,
    mappings, sequences, JSON-style inline values, and ``|``/``>`` blocks;
    it is intentionally not a general YAML implementation.
    """

    rows = []
    for raw in text.splitlines():
        if not raw.strip() or raw.lstrip().startswith("#") or raw.strip() in ("---", "..."):
            continue
        indent = len(raw) - len(raw.lstrip(" "))
        rows.append((indent, raw.strip()))

    def block(index: int, indent: int) -> tuple[Any, int]:
        if index >= len(rows) or rows[index][0] < indent:
            return {}, index
        is_list = rows[index][0] == indent and rows[index][1].startswith("-")
        value: Any = [] if is_list else {}
        while index < len(rows) and rows[index][0] == indent:
            content = rows[index][1]
            if is_list:
                if not content.startswith("-"):
                    break
                item = content[1:].strip()
                index += 1
                if not item:
                    if index < len(rows) and rows[index][0] > indent:
                        child, index = block(index, rows[index][0])
                    else:
                        child = None
                    value.append(child)
                    continue
                if ":" in item and not item.startswith(("'", '"')):
                    key, rest = _yaml_key_value(item)
                    child_map: Dict[str, Any] = {}
                    if rest in ("|", "|-", "|+", ">", ">-", ">+"):
                        parts = []
                        while index < len(rows) and rows[index][0] > indent:
                            parts.append(rows[index][1])
                            index += 1
                        child_map[key] = ("\n" if rest.startswith("|") else " ").join(parts)
                    elif rest:
                        child_map[key] = _yaml_scalar(rest)
                    elif index < len(rows) and rows[index][0] > indent:
                        child_map[key], index = block(index, rows[index][0])
                    else:
                        child_map[key] = None
                    if index < len(rows) and rows[index][0] > indent:
                        extra, index = block(index, rows[index][0])
                        if isinstance(extra, dict):
                            child_map.update(extra)
                    value.append(child_map)
                else:
                    value.append(_yaml_scalar(item))
                continue
            key, rest = _yaml_key_value(content)
            index += 1
            if rest in ("|", "|-", "|+", ">", ">-", ">+"):
                parts = []
                while index < len(rows) and rows[index][0] > indent:
                    parts.append(rows[index][1])
                    index += 1
                value[key] = ("\n" if rest.startswith("|") else " ").join(parts)
            elif rest:
                value[key] = _yaml_scalar(rest)
            elif index < len(rows) and rows[index][0] > indent:
                value[key], index = block(index, rows[index][0])
            else:
                value[key] = None
        return value, index

    parsed, _ = block(0, rows[0][0] if rows else 0)
    if not isinstance(parsed, dict):
        raise MigrationError("legacy YAML fixture must contain a mapping")
    return parsed


def _load_copy(source: Union[str, os.PathLike, Mapping[str, Any]]) -> tuple[Dict[str, Any], str, Optional[Path]]:
    if isinstance(source, Mapping):
        value = copy.deepcopy(dict(source))
        return value, canonical_digest(value), None
    path = Path(source).resolve()
    try:
        raw = path.read_bytes()
        text = raw.decode("utf-8")
        try:
            value = json.loads(text)
        except json.JSONDecodeError:
            if path.suffix.lower() in (".yaml", ".yml"):
                value = _minimal_yaml(text)
            elif path.suffix.lower() in (".md", ".markdown"):
                # Worker reports may be Markdown.  Parse only the leading
                # YAML frontmatter for identity/binding.  The raw report is
                # authenticated by its source digest but is intentionally not
                # copied into a durable object: opaque report text is outside
                # the canonical context boundary.
                frontmatter = {}
                lines = text.splitlines()
                if lines and lines[0].strip() == "---":
                    try:
                        end = next(index for index, line in enumerate(lines[1:], 1) if line.strip() == "---")
                    except StopIteration:
                        end = 0
                    if end:
                        frontmatter = _minimal_yaml("\n".join(lines[1:end]))
                value = {**frontmatter, "format": "markdown"}
            else:
                raise
    except (OSError, UnicodeDecodeError, json.JSONDecodeError, MigrationError) as exc:
        raise MigrationError("legacy fixture must be readable JSON/YAML/Markdown: %s" % path) from exc
    if not isinstance(value, dict):
        raise MigrationError("legacy fixture must contain an object: %s" % path)
    return copy.deepcopy(value), _bytes_digest(raw), path


class LegacyReader:
    """Read an A6 fixture without changing it."""

    def read(self, source: Union[str, os.PathLike, Mapping[str, Any]]) -> Dict[str, Any]:
        return _load_copy(source)[0]

    def read_with_digest(self, source: Union[str, os.PathLike, Mapping[str, Any]]) -> Dict[str, Any]:
        value, digest, path = _load_copy(source)
        return {"value": value, "source_digest": digest, "source_path": str(path) if path else None}


class NewReader:
    """Read only the new kernel's HEAD/projection truth."""

    @staticmethod
    def _validate_head_pointer(value: Any, label: str = "new_pointer") -> Dict[str, Any]:
        """Validate the HEAD snapshot carried by an active cutover pointer.

        ``kernel_path`` is pointer metadata and is intentionally excluded from
        the content digest: the canonical HEAD itself has no filesystem path.
        All other fields must be the exact v1 HEAD shape so a reader cannot be
        redirected to a different Run, role, graph, or revision.
        """

        if not isinstance(value, Mapping):
            raise MigrationError("%s must be an object" % label)
        required = {
            "schema", "run_id", "workflow_version", "graph_version", "role",
            "revision", "state_revision", "transaction_digest", "digest", "kernel_path",
        }
        if set(value) != required:
            raise MigrationError("%s is incomplete" % label)
        if value.get("schema") != "dag-head/v1":
            raise MigrationError("%s schema is invalid" % label)
        if not isinstance(value.get("run_id"), str) or not _ID.fullmatch(value["run_id"]):
            raise MigrationError("%s Run ID is malformed" % label)
        for key in ("workflow_version", "graph_version", "kernel_path"):
            if not isinstance(value.get(key), str) or not value[key] or "\x00" in value[key]:
                raise MigrationError("%s.%s is malformed" % (label, key))
        _validate_absolute_pointer_path(value["kernel_path"], "%s.kernel_path" % label)
        if value.get("graph_version") != "artifact-task-dag/v1":
            raise MigrationError("%s graph version is invalid" % label)
        if value.get("role") != "orchestrator":
            raise MigrationError("%s transition owner must be orchestrator" % label)
        for key in ("revision", "state_revision"):
            if not isinstance(value.get(key), int) or isinstance(value[key], bool) or value[key] < 1:
                raise MigrationError("%s.%s is malformed" % (label, key))
        if value["revision"] != value["state_revision"]:
            raise MigrationError("%s revision fields disagree" % label)
        if not isinstance(value.get("transaction_digest"), str) or not _DIGEST.fullmatch(value["transaction_digest"]):
            raise MigrationError("%s transaction digest is malformed" % label)
        if not isinstance(value.get("digest"), str) or not _DIGEST.fullmatch(value["digest"]):
            raise MigrationError("%s digest is malformed" % label)
        canonical_head = {key: copy.deepcopy(item) for key, item in value.items() if key not in {"digest", "kernel_path"}}
        if canonical_digest(canonical_head) != value["digest"]:
            raise MigrationError("%s digest mismatch" % label)
        return copy.deepcopy(dict(value))

    def read(
        self,
        kernel: Optional[ControlKernel] = None,
        *,
        pointer: Optional["PointerCutover"] = None,
    ) -> Dict[str, Any]:
        """Read an active new target, validating the pointer and HEAD CAS.

        A pointer is optional for callers that already hold a kernel.  When it
        is supplied, the pointer is authoritative and the active target must
        be ``new``; the old target is never silently read as new state.
        """

        if pointer is None:
            if kernel is None:
                raise MigrationError("NewReader requires a kernel or active cutover pointer")
            return kernel.resume()

        # Pointer selection and target resume share one pointer lock.  The
        # post-read CAS check prevents a concurrent rollback from returning a
        # state that was no longer the active target when it was observed.
        with pointer._lock():
            pointer_value = pointer._read_unlocked()
            if pointer_value.get("active") != "new":
                raise MigrationError("active cutover target is not the new kernel")
            target = self._validate_head_pointer(pointer_value.get("new_pointer"))
            if kernel is None:
                target_path = Path(target["kernel_path"]).resolve()
                if not target_path.is_dir():
                    raise MigrationError("new kernel target does not exist")
                kernel = ControlKernel(target_path)
                # A direct-path kernel normally learns its Run ID from the
                # caller; the pointer binding is trusted when opening one.
                kernel.run_id = target["run_id"]
            if target["run_id"] != kernel.run_id:
                raise MigrationError("new pointer Run ID does not match kernel")
            if target["kernel_path"] != str(kernel.path):
                raise MigrationError("new pointer kernel path does not match target")
            result = kernel.resume(
                expected_revision=target["revision"],
                expected_state_revision=target["state_revision"],
                expected_head_digest=target["transaction_digest"],
                expected_workflow_version=target["workflow_version"],
            )
            current = pointer._read_unlocked()
            if current.get("active") != "new" or current.get("new_pointer") != target:
                raise MigrationError("active cutover pointer changed during resume")
            actual_head = result.get("head")
            if not isinstance(actual_head, Mapping):
                raise MigrationError("new kernel did not return a HEAD snapshot")
            for key in ("schema", "run_id", "workflow_version", "graph_version", "role", "revision", "state_revision", "transaction_digest"):
                if actual_head.get(key) != target.get(key):
                    raise MigrationError("new pointer HEAD binding does not match kernel: %s" % key)
            return result

    def status(
        self,
        kernel: Optional[ControlKernel] = None,
        *,
        pointer: Optional["PointerCutover"] = None,
    ) -> Dict[str, Any]:
        return self.read(kernel, pointer=pointer)


class LegacyConverter:
    """Map copied A6 fields to one immutable migration transaction."""

    FIELD_MAPPING = {
        "run_id": "state.run_id",
        "workflow_version": "state.workflow_version",
        "state_revision": "state.revision",
        "objective_ref": "state.objective_ref",
        "current_group": "state.group",
        "current_epoch": "state.epoch",
        "artifacts": "state.artifacts -> legacy-artifact objects",
        "last_checkpoint": "migration.source_refs/checkpoint provenance",
        "bundle": "migration.source_refs/legacy-bundle",
        "worker_report": "migration.source_refs/legacy-worker-report",
    }

    def mapping(self) -> Dict[str, str]:
        return dict(self.FIELD_MAPPING)

    @staticmethod
    def _require_id(value: Any, label: str) -> str:
        if not isinstance(value, str) or not _ID.fullmatch(value):
            raise MigrationError("%s is missing or malformed" % label)
        return value

    @staticmethod
    def _require_digest(value: Any, label: str) -> str:
        if not isinstance(value, str) or not _DIGEST.fullmatch(value):
            raise MigrationError("%s is missing or malformed" % label)
        return value

    @staticmethod
    def _source_binding(
        run_value: Mapping[str, Any], bundle_value: Mapping[str, Any], worker_value: Mapping[str, Any]
    ) -> Dict[str, Any]:
        """Extract and cross-check immutable source identity before writing."""

        def mapping(value: Mapping[str, Any], *keys: str) -> Optional[Mapping[str, Any]]:
            for key in keys:
                candidate = value.get(key)
                if isinstance(candidate, Mapping):
                    return candidate
            return None

        def scalar(value: Mapping[str, Any], *keys: str) -> Any:
            for key in keys:
                candidate = value.get(key)
                if candidate is not None:
                    return candidate
            return None

        source_run_id = LegacyConverter._require_id(scalar(run_value, "run_id"), "legacy Run run_id")
        bundle_run_id = LegacyConverter._require_id(bundle_value.get("run_id"), "legacy Bundle run_id")
        if bundle_run_id != source_run_id:
            raise MigrationError("legacy Bundle Run ID does not match Run")
        run_group = mapping(run_value, "current_group", "group")
        bundle_group_value = scalar(bundle_value, "group_id", "current_group", "group")
        bundle_group = bundle_group_value if isinstance(bundle_group_value, Mapping) else None
        if run_group is None or bundle_group_value is None:
            raise MigrationError("legacy Run and Bundle Group identities are required")
        group_id = LegacyConverter._require_id(run_group.get("id") or run_group.get("group_id"), "legacy Group id")
        bundle_group_id = LegacyConverter._require_id(
            bundle_group_value.get("id") if isinstance(bundle_group_value, Mapping) else bundle_group_value,
            "legacy Bundle Group id",
        )
        if bundle_group_id != group_id:
            raise MigrationError("legacy Bundle Group does not match Run")
        run_group_status = run_group.get("status") if isinstance(run_group, Mapping) else None
        bundle_group_status = bundle_group.get("status") if isinstance(bundle_group, Mapping) else bundle_value.get("group_status")
        source_status = run_group_status
        if source_status is None:
            source_status = bundle_group_status
        if source_status is None:
            source_status = scalar(run_value, "status")
        if not isinstance(source_status, str) or not source_status:
            raise MigrationError("legacy source Group/Run status is required")
        if run_group_status not in ("open", "closed"):
            run_group_status = bundle_group_status
        if run_group_status not in ("open", "closed"):
            # The frozen A6 Bundle closes its Epoch while intentionally leaving
            # the bootstrap Group open for the next handoff.  A missing Group
            # status in an old reader therefore has this conservative default.
            run_group_status = "open"

        run_epoch = mapping(run_value, "current_epoch", "context_epoch", "epoch")
        bundle_epoch = mapping(bundle_value, "context_epoch", "current_epoch", "epoch")
        if run_epoch is None or bundle_epoch is None:
            raise MigrationError("legacy Run and Bundle Epoch identities are required")
        epoch_id = LegacyConverter._require_id(run_epoch.get("id") or run_epoch.get("epoch_id"), "legacy Epoch id")
        bundle_epoch_id = LegacyConverter._require_id(
            bundle_epoch.get("id") or bundle_epoch.get("epoch_id"),
            "legacy Bundle Epoch id",
        )
        if bundle_epoch_id != epoch_id:
            raise MigrationError("legacy Bundle Epoch does not match Run")
        epoch_status = None
        for candidate in (run_epoch, bundle_epoch):
            if isinstance(candidate, Mapping) and candidate.get("status") in ("open", "closed"):
                epoch_status = candidate.get("status")
                break
        clear_boundary = scalar(run_epoch or {}, "clear_before_next", "clear_before_start") or scalar(bundle_epoch or {}, "clear_before_next", "clear_before_start")
        if clear_boundary is not True:
            raise MigrationError("legacy Epoch clear boundary is missing")
        # A clear boundary is the durable closure marker in the frozen A6
        # Bundle, even when an older reader omitted the redundant status.
        if epoch_status is None:
            epoch_status = "closed"
        if epoch_status != "closed":
            raise MigrationError("legacy source Epoch is not closed")

        report_declared_run_id = worker_value.get("run_id")
        report_work_id = worker_value.get("work_id")
        if report_declared_run_id is not None and report_work_id is not None and report_declared_run_id != report_work_id:
            raise MigrationError("worker report run_id and work_id disagree")
        report_identity = report_declared_run_id or report_work_id
        if report_identity is None:
            # Older JSON worker reports do not carry a Run ID, but their
            # context_epoch and worker_id still provide a bounded provenance
            # tuple.  Do not accept an entirely anonymous report or silently
            # bind a report with the wrong Epoch to the source Run.
            if worker_value.get("context_epoch") != epoch_id or not isinstance(worker_value.get("worker_id"), str) or not worker_value.get("worker_id"):
                raise MigrationError("worker report Run identity is missing")
            report_identity = source_run_id
        report_run_id = LegacyConverter._require_id(report_identity, "worker report run_id/work_id")
        report_group_value = worker_value.get("group_id") or worker_value.get("group") or group_id
        if isinstance(report_group_value, Mapping):
            report_group_value = report_group_value.get("id") or report_group_value.get("group_id")
        report_group_id = LegacyConverter._require_id(report_group_value, "worker report Group id")
        report_epoch = worker_value.get("context_epoch") or worker_value.get("epoch_id") or worker_value.get("epoch") or epoch_id
        if isinstance(report_epoch, Mapping):
            report_epoch = report_epoch.get("id") or report_epoch.get("epoch_id")
        report_epoch_id = LegacyConverter._require_id(report_epoch, "worker report Epoch id")
        if report_run_id != source_run_id or report_group_id != group_id or report_epoch_id != epoch_id:
            raise MigrationError("worker report Run/Group/Epoch binding does not match source")
        report_status = worker_value.get("status")
        if report_status not in ("done", "partial", "blocked"):
            raise MigrationError("worker report status is not a completed source report")

        workflows = []
        for source_name, source in (("Run", run_value), ("Bundle", bundle_value), ("worker report", worker_value)):
            workflow = source.get("workflow_version")
            if workflow is not None:
                if not isinstance(workflow, str) or not workflow:
                    raise MigrationError("legacy %s workflow_version is malformed" % source_name)
                workflows.append(workflow)
        if workflows and len(set(workflows)) != 1:
            raise MigrationError("legacy source workflow versions disagree")

        def source_aliases(source: Mapping[str, Any]) -> Any:
            direct = source.get("aliases")
            if direct is not None:
                return direct
            metadata = source.get("metadata")
            return metadata.get("aliases") if isinstance(metadata, Mapping) else None

        aliases = source_aliases(run_value)
        if aliases is None:
            aliases = source_aliases(bundle_value)
        if aliases is None:
            aliases = source_aliases(worker_value)
        if aliases is None:
            raise MigrationError("legacy source aliases are required")
        if not isinstance(aliases, list) or any(not isinstance(alias, str) or not alias for alias in aliases):
            raise MigrationError("legacy source aliases are malformed")
        for source_name, source in (("Run", run_value), ("Bundle", bundle_value), ("worker report", worker_value)):
            declared_aliases = source_aliases(source)
            if declared_aliases is not None and declared_aliases != aliases:
                raise MigrationError("legacy %s aliases do not match Run" % source_name)

        revisions = []
        source_revision_evidence = {"run": False, "bundle": False}
        for value, label in ((run_value, "Run"), (bundle_value, "Bundle")):
            historical = []
            direct = value.get("source_state_revision")
            if direct is not None:
                historical.append(direct)
            migration_source = value.get("trajectory_correction")
            if isinstance(migration_source, Mapping):
                migration_source = migration_source.get("migration_source")
            if isinstance(migration_source, Mapping) and migration_source.get("source_state_revision") is not None:
                historical.append(migration_source.get("source_state_revision"))
            for nested_key in ("context_epoch", "current_epoch", "epoch"):
                nested = value.get(nested_key)
                if isinstance(nested, Mapping):
                    context_revision = nested.get("closed_at_revision") or nested.get("state_revision")
                    if context_revision is not None:
                        historical.append(context_revision)
            candidates = historical or ([value.get("state_revision")] if value.get("state_revision") is not None else [])
            source_revision_evidence["run" if label == "Run" else "bundle"] = bool(candidates)
            for candidate in candidates:
                if not isinstance(candidate, int) or isinstance(candidate, bool) or candidate < 1:
                    raise MigrationError("legacy %s source revision is malformed" % label)
            revisions.extend(candidates)
        for candidate in (worker_value.get("source_state_revision"), worker_value.get("state_revision")):
            if candidate is not None:
                if not isinstance(candidate, int) or isinstance(candidate, bool) or candidate < 1:
                    raise MigrationError("legacy worker report source revision is malformed")
                revisions.append(candidate)
        if not revisions or not all(source_revision_evidence.values()):
            raise MigrationError("legacy Run and Bundle source revisions are required")
        if len(set(revisions)) != 1:
            raise MigrationError("legacy source revision claims disagree")
        source_revision = revisions[0]
        return {
            "source_run_id": source_run_id,
            "group_id": group_id,
            "epoch_id": epoch_id,
            "aliases": copy.deepcopy(aliases),
            "source_revision": source_revision,
            # A6's frozen Bundle closes the context Epoch while the bootstrap
            # Group remains open for a subsequent handoff.  Preserve that
            # distinction instead of coercing every completed source into a
            # closed Group.
            "run_status": run_group_status,
            "source_status": source_status,
            "epoch_status": epoch_status,
        }

    def convert(
        self,
        run: Union[str, os.PathLike, Mapping[str, Any]],
        bundle: Optional[Union[str, os.PathLike, Mapping[str, Any]]] = None,
        worker_report: Optional[Union[str, os.PathLike, Mapping[str, Any]]] = None,
        *,
        destination: Union[str, os.PathLike],
        run_id: Optional[str] = None,
        authority_ref: Optional[Mapping[str, Any]] = None,
        expected_source_revision: Optional[int] = None,
        expected_source_digests: Optional[Mapping[str, str]] = None,
    ) -> Dict[str, Any]:
        if bundle is None or worker_report is None:
            raise MigrationError("run, Bundle, and worker report are all required")
        if not all(isinstance(source, (str, os.PathLike)) for source in (run, bundle, worker_report)):
            raise MigrationError("migration sources must be readable copied files")
        if expected_source_revision is None or not isinstance(expected_source_revision, int) or isinstance(expected_source_revision, bool) or expected_source_revision < 1:
            raise MigrationError("expected_source_revision is required")
        if expected_source_digests is None or set(expected_source_digests) != {"run", "bundle", "worker_report"}:
            raise MigrationError("expected_source_digests must bind run, bundle, and worker_report")
        if authority_ref is None:
            raise MigrationError("migration authority_ref is required")
        run_value, run_digest, run_path = _load_copy(run)
        bundle_value, bundle_digest, bundle_path = _load_copy(bundle)
        worker_value, worker_digest, worker_path = _load_copy(worker_report)

        source_paths = [item for item in (run_path, bundle_path, worker_path) if item]
        destination_path = Path(destination).resolve()
        repository_root = Path(__file__).resolve().parents[3]
        protected_source_roots = (
            repository_root / ".local" / "agent" / "runs",
            repository_root / ".local" / "agent" / "reports",
        )
        if any(path == protected_root or protected_root in path.parents for path in source_paths for protected_root in protected_source_roots):
            raise MigrationError("conversion requires copied fixtures; real Run/report state is read-only")
        if any(path == destination_path or destination_path in path.parents for path in source_paths):
            raise MigrationError("conversion destination must be separate from legacy source")
        if source_paths:
            # Treat the common source directory as read-only.  This prevents
            # an accidental ``real-run/converted`` target while still
            # allowing the normal isolated sibling fixture used by tests.
            source_root = Path(os.path.commonpath([str(path.parent) for path in source_paths]))
            try:
                destination_path.relative_to(source_root)
            except ValueError:
                pass
            else:
                raise MigrationError("conversion destination must be outside the legacy source tree")
        actual_source_digests = {
            "run": run_digest,
            "bundle": bundle_digest,
            "worker_report": worker_digest,
        }
        for key, actual in actual_source_digests.items():
            expected = expected_source_digests.get(key)
            if not isinstance(actual, str) or expected != actual or not _DIGEST.fullmatch(expected):
                raise MigrationError("source digest mismatch for %s" % key)
        binding = self._source_binding(run_value, bundle_value, worker_value)
        source_revision = binding["source_revision"]
        if source_revision != expected_source_revision:
            raise MigrationError("source revision mismatch")
        # The candidate is built from this deep-copied, content-addressed
        # snapshot.  Source files are still re-read before candidate apply as
        # an early race guard, but publication never reconstructs a candidate
        # from a mutable path after the snapshot has been captured.
        source_snapshot = {
            "mode": "immutable",
            "source_digests": copy.deepcopy(actual_source_digests),
            "source_bindings": copy.deepcopy(binding),
        }
        target_run_id = self._require_id(run_id or binding["source_run_id"], "destination run_id")
        objective = None
        if bundle_value and isinstance(bundle_value.get("objective_ref"), Mapping):
            objective = bundle_value["objective_ref"]
        if objective is None and isinstance(run_value.get("objective_ref"), Mapping):
            objective = run_value["objective_ref"]
        legacy_objective = run_value.get("objective")
        if objective is None and isinstance(legacy_objective, Mapping):
            proposed_path = legacy_objective.get("proposed_path")
            proposed_version = legacy_objective.get("proposed_version")
            proposed_digest = legacy_objective.get("proposed_digest")
            if isinstance(proposed_path, str) and isinstance(proposed_version, str) and isinstance(proposed_digest, str) and proposed_digest:
                objective = {
                    "path": proposed_path,
                    "version": proposed_version,
                    "digest": proposed_digest,
                    "approval_status": legacy_objective.get("approval_status"),
                }
        if not isinstance(objective, Mapping):
            raise MigrationError("legacy objective_ref is missing")
        objective = copy.deepcopy(dict(objective))
        if any(not isinstance(objective.get(key), str) or not objective[key] for key in ("path", "version", "digest")):
            raise MigrationError("legacy objective_ref must contain path, version, and digest")
        workflow_version = (
            bundle_value.get("workflow_version") if isinstance(bundle_value, Mapping) else None
        ) or run_value.get("workflow_version", "manual-bootstrap/v1")
        if not isinstance(workflow_version, str) or not workflow_version:
            raise MigrationError("legacy workflow_version is missing")
        authority = authority_ref

        def fresh_source_check() -> tuple[Dict[str, Any], Dict[str, Any], Dict[str, Any], Dict[str, Any]]:
            reread: Dict[str, Dict[str, Any]] = {}
            for key, source_path in (("run", run_path), ("bundle", bundle_path), ("worker_report", worker_path)):
                if source_path is None or not source_path.is_file():
                    raise MigrationError("bound source is missing: %s" % key)
                try:
                    value, digest, reread_path = _load_copy(source_path)
                except OSError as exc:
                    raise MigrationError("bound source cannot be reread: %s" % key) from exc
                if reread_path is None or reread_path.resolve() != source_path.resolve() or digest != actual_source_digests[key]:
                    raise MigrationError("bound source changed during conversion: %s" % key)
                reread[key] = value
            reread_binding = self._source_binding(reread["run"], reread["bundle"], reread["worker_report"])
            identity_keys = ("source_run_id", "group_id", "epoch_id", "aliases", "source_revision", "run_status", "source_status", "epoch_status")
            if any(reread_binding.get(key) != binding.get(key) for key in identity_keys):
                raise MigrationError("bound source identity changed during conversion")
            return reread["run"], reread["bundle"], reread["worker_report"], reread_binding

        # Re-read all sources after validation and immediately before the
        # first destination mutation.  The destination is always a copied
        # fixture; the source files remain untouched.
        fresh_source_check()
        destination_path.parent.mkdir(parents=True, exist_ok=True)

        # Build the complete candidate in a sibling temporary tree.  The
        # caller-visible destination is changed only once, after a final
        # digest/identity reread succeeds; this prevents a source race from
        # leaving a partially converted Run behind.
        if destination_path.exists():
            raise MigrationError("conversion destination already exists")
        temporary_root = Path(tempfile.mkdtemp(prefix=".%s.migration-" % destination_path.name, dir=str(destination_path.parent))).resolve()
        temporary_kernel: Optional[ControlKernel] = None
        try:
            # Entry creates the new genesis.  migrate_legacy then records all
            # copied values and mapped artifact objects in one child transaction.
            temporary_kernel = ControlKernel(temporary_root, target_run_id)
            temporary_kernel.entry(
                objective,
                workflow_version=workflow_version,
                group_id=binding["group_id"],
                epoch_id=binding["epoch_id"],
                aliases=binding["aliases"],
                authority_ref=authority,
            )
            migration = {
                "source_revision": source_revision,
                "source_digests": actual_source_digests,
                "source_bindings": binding,
                "source_snapshot": source_snapshot,
                "run": _safe_legacy_value(run_value),
                "bundle": _safe_legacy_value(bundle_value),
                "worker_report": _safe_legacy_value(worker_value),
                "field_mapping": self.mapping(),
                "artifacts": _safe_legacy_value(run_value.get("artifacts") or {}),
            }
            # The public ``apply(migrate_legacy)`` API accepts no mapping-only
            # shortcut.  Carry a physical raw-byte attestation alongside the
            # filtered source values so the kernel can independently verify
            # the copied files immediately before publication.
            migration["source_attestation"] = {
                key: {
                    "path": str(path.resolve()),
                    "raw_digest": actual_source_digests[key],
                    "value_digest": canonical_digest(migration[key]),
                }
                for key, path in (
                    ("run", run_path),
                    ("bundle", bundle_path),
                    ("worker_report", worker_path),
                )
            }
            if bundle_value and isinstance(bundle_value.get("canonical_artifacts"), list):
                # A real A6 Bundle is the preferred artifact catalog.  Preserve
                # its path/version/digest references under stable path-derived
                # IDs without copying source content into the new canonical state.
                mapped = {}
                for index, artifact in enumerate(bundle_value["canonical_artifacts"]):
                    if not isinstance(artifact, Mapping):
                        continue
                    path = artifact.get("path")
                    if not isinstance(path, str) or not path:
                        continue
                    artifact_id = path.rsplit("/", 1)[-1] or "legacy-%s" % index
                    mapped[artifact_id] = {
                        "path": path,
                        "version": artifact.get("version", "legacy"),
                        "digest": artifact.get("digest"),
                    }
                if mapped:
                    migration["artifacts"] = {**mapped, **migration["artifacts"]}
            fresh_source_check()
            command = temporary_kernel._command_for("migrate_legacy", {"migration": migration}, authority_ref=authority, idempotency_key="migration:" + target_run_id)
            state = temporary_kernel.apply(command)
            # A final reread happens after all candidate writes but before
            # publication.  Any source race removes only the disposable tree.
            fresh_source_check()

            lock_path = destination_path.parent / (".%s.migration.lock" % destination_path.name)
            with lock_path.open("a+", encoding="utf-8") as lock_handle:
                fcntl.flock(lock_handle.fileno(), fcntl.LOCK_EX)
                try:
                    # Re-read after waiting for the destination CAS lock as
                    # well as before building the candidate.  The final
                    # source check and publication are therefore one
                    # serialized decision for this conversion attempt; a
                    # changed copied fixture cannot be published merely
                    # because another conversion was holding the lock.
                    fresh_source_check()
                    if destination_path.exists():
                        raise MigrationError("conversion destination appeared during conversion")
                    os.replace(str(temporary_root), str(destination_path))
                    _fsync_directory(destination_path.parent)
                finally:
                    fcntl.flock(lock_handle.fileno(), fcntl.LOCK_UN)
            temporary_root = destination_path
            kernel = ControlKernel(destination_path, target_run_id)
            kernel.last_receipt = copy.deepcopy(temporary_kernel.last_receipt)
            # Verify the renamed tree under its final path before exposing it
            # to a caller or constructing a cutover pointer.
            state = kernel.read_state()
            head = kernel.head()
            if not isinstance(head, Mapping):
                raise MigrationError("new kernel did not publish a HEAD")
            pointer_head = {**copy.deepcopy(dict(head)), "kernel_path": str(kernel.path)}
            return {
                "kernel": kernel,
                "run_id": target_run_id,
                "source_revision": migration["source_revision"],
                "source_digests": migration["source_digests"],
                "source_snapshot": copy.deepcopy(source_snapshot),
                "field_mapping": self.mapping(),
                "new_head": pointer_head,
                "migration_transaction": kernel.last_receipt,
                "history_preserved": bool(state.get("migration", {}).get("history_preserved")),
                "source_paths": [str(path) for path in source_paths],
            }
        except Exception:
            if temporary_root.exists() and temporary_root != destination_path:
                shutil.rmtree(temporary_root, ignore_errors=True)
            raise


class PointerCutover:
    """Pointer-only cutover and rollback for an isolated fixture."""

    _REQUIRED_SOURCE_DIGESTS = frozenset({"run", "bundle", "worker_report"})

    def __init__(self, pointer_path: Union[str, os.PathLike]) -> None:
        self.path = Path(pointer_path).resolve()

    @contextmanager
    def _lock(self) -> Iterator[None]:
        """Serialize pointer read/compare/write sequences for this fixture."""

        self.path.parent.mkdir(parents=True, exist_ok=True)
        with Path(str(self.path) + ".lock").open("a+", encoding="utf-8") as handle:
            fcntl.flock(handle.fileno(), fcntl.LOCK_EX)
            try:
                yield
            finally:
                fcntl.flock(handle.fileno(), fcntl.LOCK_UN)

    def _write(self, value: Mapping[str, Any]) -> None:
        try:
            ControlKernel._ensure_durable_payload(value, "cutover pointer")
        except KernelError as exc:
            raise MigrationError("cutover pointer contains non-durable data") from exc
        self.path.parent.mkdir(parents=True, exist_ok=True)
        descriptor, temporary = tempfile.mkstemp(prefix=".%s." % self.path.name, dir=str(self.path.parent))
        try:
            with os.fdopen(descriptor, "w", encoding="utf-8") as handle:
                json.dump(value, handle, ensure_ascii=False, sort_keys=True, indent=2)
                handle.write("\n")
                handle.flush()
                os.fsync(handle.fileno())
            os.replace(temporary, str(self.path))
            try:
                fd = os.open(str(self.path.parent), os.O_RDONLY)
                os.fsync(fd)
                os.close(fd)
            except OSError:
                pass
        except Exception:
            try:
                os.unlink(temporary)
            except FileNotFoundError:
                pass
            raise

    def _read_unlocked(self) -> Dict[str, Any]:
        if not self.path.exists():
            raise MigrationError("cutover pointer does not exist")
        try:
            value = json.loads(self.path.read_text(encoding="utf-8"))
        except (OSError, UnicodeDecodeError, json.JSONDecodeError) as exc:
            raise MigrationError("invalid cutover pointer encoding") from exc
        if not isinstance(value, dict) or value.get("schema") != "cutover-pointer/v1":
            raise MigrationError("invalid cutover pointer")
        try:
            ControlKernel._ensure_durable_payload(value, "cutover pointer")
        except KernelError as exc:
            raise MigrationError("cutover pointer contains non-durable data") from exc
        if value.get("active") not in ("old", "new"):
            raise MigrationError("invalid cutover active pointer")
        required = (
            "schema",
            "active",
            "old_pointer",
            "new_pointer",
            "source_digests",
            "history_preserved",
            "proposal_digest",
            "approval_ref",
            "authority",
            "reader_pointers",
        )
        if any(key not in value for key in required):
            raise MigrationError("cutover pointer is incomplete")
        allowed = set(required) | {"pointer_digest", "rollback_preserved_candidate", "rollback_approval_ref"}
        if set(value) - allowed:
            raise MigrationError("cutover pointer has unsupported fields")
        old_pointer = value.get("old_pointer")
        self._validate_old_pointer(old_pointer)
        if not isinstance(value.get("new_pointer"), Mapping) or not value["new_pointer"]:
            raise MigrationError("invalid cutover new pointer")
        NewReader._validate_head_pointer(value["new_pointer"])
        source_digests = value.get("source_digests")
        if not isinstance(source_digests, Mapping) or set(source_digests) != self._REQUIRED_SOURCE_DIGESTS:
            raise MigrationError("cutover source digests are missing")
        if any(not isinstance(digest, str) or not re.fullmatch(r"sha256:[0-9a-f]{64}", digest) for digest in source_digests.values()):
            raise MigrationError("cutover source digest is malformed")
        if value.get("history_preserved") is not True:
            raise MigrationError("cutover history preservation marker is missing")
        if not isinstance(value.get("proposal_digest"), str) or not re.fullmatch(r"sha256:[0-9a-f]{64}", value["proposal_digest"]):
            raise MigrationError("cutover proposal digest is malformed")
        if not isinstance(value.get("approval_ref"), str) or not value["approval_ref"]:
            raise MigrationError("cutover approval reference is missing")
        authority = value.get("authority")
        self._validate_authority_tuple(authority, "migration_cutover", expected_run_id=value["new_pointer"]["run_id"], proposal_digest=value["proposal_digest"])
        if authority.get("approval_ref") != value["approval_ref"]:
            raise MigrationError("cutover authority approval reference does not match")
        if not isinstance(value.get("reader_pointers"), Mapping) or any(key not in value["reader_pointers"] for key in ("old", "new")):
            raise MigrationError("cutover reader pointers are incomplete")
        # The reader projection is part of the pointer's durable CAS shape,
        # not an advisory mirror.  Validate both branches with the same
        # complete old/new contracts used by cutover and rollback so an
        # unknown or missing nested field cannot be smuggled past schema
        # validation and later selected by a fresh reader.
        self._validate_old_pointer(value["reader_pointers"].get("old"))
        NewReader._validate_head_pointer(value["reader_pointers"].get("new"), "reader_pointers.new")
        if value["reader_pointers"].get("old") != value.get("old_pointer") or value["reader_pointers"].get("new") != value.get("new_pointer"):
            raise MigrationError("cutover reader pointers do not match active targets")
        if "rollback_preserved_candidate" in value:
            candidate = value.get("rollback_preserved_candidate")
            if candidate is not None:
                NewReader._validate_head_pointer(candidate, "rollback_preserved_candidate")
            if candidate != value.get("new_pointer"):
                raise MigrationError("rollback candidate is not the preserved new pointer")
        if "rollback_approval_ref" in value:
            rollback_ref = value.get("rollback_approval_ref")
            if not isinstance(rollback_ref, str) or not rollback_ref:
                raise MigrationError("rollback approval reference is malformed")
        if value.get("proposal_digest") != self.proposal_digest(value["new_pointer"], source_digests, value.get("old_pointer")):
            raise MigrationError("cutover proposal digest does not match pointer contents")
        if value.get("pointer_digest") != _pointer_digest(value):
            raise MigrationError("cutover pointer digest mismatch")
        return copy.deepcopy(value)

    @staticmethod
    def _validate_old_pointer(value: Any, *, require_target: bool = False) -> None:
        """Validate a legacy CAS pointer and, for rollback, its target tuple.

        A first cutover may preserve a pointer owned by the old system whose
        only durable identity is ``revision``/``digest``.  Rollback is a
        stronger operation: before changing the active pointer it must name a
        physical, reader-verifiable old kernel and its source binding.  Keep
        those two contracts distinct so cutover can preserve opaque history
        without letting rollback switch to an unverified target.
        """

        if value is None:
            if require_target:
                raise MigrationError("rollback requires a physical old target")
            return
        if not isinstance(value, Mapping):
            raise MigrationError("invalid cutover old pointer")
        base = {"revision", "digest"}
        target_fields = {
            "schema", "run_id", "workflow_version", "graph_version", "role",
            "state_revision", "transaction_digest", "kernel_path", "status",
            "source_digests", "source_revision", "group_id", "epoch_id", "aliases",
        }
        unknown = set(value) - (base | target_fields)
        if unknown:
            raise MigrationError("invalid cutover old pointer fields: %s" % ", ".join(sorted(unknown)))
        if not isinstance(value.get("revision"), int) or isinstance(value.get("revision"), bool) or value["revision"] < 0:
            raise MigrationError("invalid cutover old pointer revision")
        if not isinstance(value.get("digest"), str) or not _DIGEST.fullmatch(value["digest"]):
            raise MigrationError("invalid cutover old pointer digest")
        supplied_target = set(value) - base
        if not supplied_target:
            if require_target:
                raise MigrationError("rollback requires a complete physical old target")
            return
        required_target = target_fields
        if supplied_target != required_target:
            raise MigrationError("old pointer physical target metadata is incomplete")
        if value.get("schema") != "dag-head/v1" or value.get("graph_version") != "artifact-task-dag/v1" or value.get("role") != "orchestrator":
            raise MigrationError("old pointer target schema/transition owner is invalid")
        if not isinstance(value.get("run_id"), str) or not _ID.fullmatch(value["run_id"]):
            raise MigrationError("old pointer target Run ID is malformed")
        if not isinstance(value.get("workflow_version"), str) or not value["workflow_version"]:
            raise MigrationError("old pointer target workflow version is malformed")
        _validate_absolute_pointer_path(value.get("kernel_path"), "old pointer target kernel path")
        if not isinstance(value.get("state_revision"), int) or isinstance(value.get("state_revision"), bool) or value["state_revision"] < 1 or value["state_revision"] != value["revision"]:
            raise MigrationError("old pointer target state revision is malformed")
        if not isinstance(value.get("transaction_digest"), str) or not _DIGEST.fullmatch(value["transaction_digest"]):
            raise MigrationError("old pointer target transaction digest is malformed")
        if value["digest"] != canonical_digest({key: copy.deepcopy(value[key]) for key in ("schema", "run_id", "workflow_version", "graph_version", "role", "revision", "state_revision", "transaction_digest")}):
            raise MigrationError("old pointer target HEAD digest mismatch")
        if value.get("status") not in {"active", "paused_after_epoch", "paused_after_group"}:
            raise MigrationError("old pointer target status is malformed")
        if not isinstance(value.get("source_revision"), int) or isinstance(value.get("source_revision"), bool) or value["source_revision"] < 1:
            raise MigrationError("old pointer target source revision is malformed")
        if not isinstance(value.get("source_digests"), Mapping) or set(value["source_digests"]) != {"run", "bundle", "worker_report"} or any(not isinstance(item, str) or not _DIGEST.fullmatch(item) for item in value["source_digests"].values()):
            raise MigrationError("old pointer target source digests are malformed")
        for key in ("group_id", "epoch_id"):
            if not isinstance(value.get(key), str) or not _ID.fullmatch(value[key]):
                raise MigrationError("old pointer target %s is malformed" % key)
        if not isinstance(value.get("aliases"), list) or any(not isinstance(alias, str) or not alias for alias in value["aliases"]):
            raise MigrationError("old pointer target aliases are malformed")

    @staticmethod
    def _old_target_head(value: Mapping[str, Any]) -> Dict[str, Any]:
        return {
            key: copy.deepcopy(value[key])
            for key in (
                "schema", "run_id", "workflow_version", "graph_version", "role",
                "revision", "state_revision", "transaction_digest", "digest", "kernel_path",
            )
        }

    @staticmethod
    def _target_kernel(target: Mapping[str, Any]) -> ControlKernel:
        _validate_absolute_pointer_path(target.get("kernel_path"), "cutover target kernel path")
        target_path = Path(target["kernel_path"]).resolve()
        if str(target_path) != target["kernel_path"]:
            raise MigrationError("cutover target path is not canonical")
        if not target_path.is_dir():
            raise MigrationError("cutover target kernel does not exist")
        kernel = ControlKernel(target_path)
        # A pointer names the direct control-kernel directory, rather than a
        # repository root from which the Run layout should be derived.
        kernel.run_id = target["run_id"]
        return kernel

    @classmethod
    def _validate_target_locked(
        cls,
        target: Mapping[str, Any],
        source_digests: Mapping[str, Any],
    ) -> ControlKernel:
        """Verify the physical target while its Run lock is held."""

        kernel = cls._target_kernel(target)
        state, head = kernel._load_current()
        for key in (
            "schema", "run_id", "workflow_version", "graph_version", "role",
            "revision", "state_revision", "transaction_digest",
        ):
            if head.get(key) != target.get(key):
                raise MigrationError("cutover target HEAD binding does not match: %s" % key)
        if target.get("digest") != canonical_digest({key: copy.deepcopy(value) for key, value in head.items() if key != "digest"}):
            raise MigrationError("cutover target HEAD digest does not match")
        if not kernel._projection_is_current(head):
            raise MigrationError("cutover target projection is stale or incomplete")
        migration = state.get("migration")
        if not isinstance(migration, Mapping):
            raise MigrationError("cutover target has no source migration binding")
        if migration.get("source_digests") != dict(source_digests):
            raise MigrationError("cutover target source digest binding does not match")
        bindings = migration.get("source_bindings")
        if not isinstance(bindings, Mapping) or bindings.get("source_revision") != migration.get("source_revision"):
            raise MigrationError("cutover target source identity binding is malformed")
        # Source bindings describe the immutable A6 snapshot.  A valid target
        # is expected to advance into later Epoch/Group lifecycle states before
        # cutover, so those historical fields must bind to the immutable
        # snapshot rather than equal the live A7 state.  Aliases remain stable
        # target identity and are still checked against current metadata.
        snapshot = migration.get("source_snapshot")
        if (
            not isinstance(snapshot, Mapping)
            or snapshot.get("mode") != "immutable"
            or snapshot.get("source_digests") != migration.get("source_digests")
            or snapshot.get("source_bindings") != bindings
        ):
            raise MigrationError("cutover target immutable source identity binding does not match")
        if bindings.get("aliases") != state.get("metadata", {}).get("aliases"):
            raise MigrationError("cutover target alias/source identity binding does not match")
        if migration.get("history_preserved") is not True or migration.get("cutover_status") not in {"candidate", "cutover", "rolled_back"}:
            raise MigrationError("cutover target migration lifecycle is invalid")
        if state.get("run_id") != target.get("run_id") or state.get("workflow_version") != target.get("workflow_version"):
            raise MigrationError("cutover target Run/version binding does not match")
        return kernel

    @classmethod
    def _validate_old_target_locked(
        cls,
        old_pointer: Mapping[str, Any],
        fallback_source_digests: Mapping[str, Any],
    ) -> ControlKernel:
        """Validate a physical old target before an initial pointer mutation.

        A legacy pointer may contain only its historical CAS pair.  When the
        pointer carries the complete v1 target tuple, however, accepting it
        without opening the named directory would turn the old side of a
        cutover into an unverifiable rollback claim.  Validate the old
        target with its own source binding (falling back to the new binding
        only for the historical tuple) while its Run lock is held.
        """

        cls._validate_old_pointer(old_pointer, require_target=True)
        target = cls._old_target_head(old_pointer)
        kernel = cls._target_kernel(target)
        source_digests = old_pointer.get("source_digests") or fallback_source_digests
        with kernel._lock():
            cls._validate_target_locked(target, source_digests)
            state, head = kernel._load_current()
            if old_pointer.get("status") != state.get("status"):
                raise MigrationError("old pointer target status does not match kernel")
            if old_pointer.get("source_revision") != state.get("migration", {}).get("source_revision"):
                raise MigrationError("old pointer target source revision does not match kernel")
            if old_pointer.get("group_id") != state.get("group", {}).get("id") or old_pointer.get("epoch_id") != state.get("epoch", {}).get("id"):
                raise MigrationError("old pointer target Group/Epoch does not match kernel")
            if old_pointer.get("aliases") != state.get("metadata", {}).get("aliases"):
                raise MigrationError("old pointer target aliases do not match kernel")
            if old_pointer.get("source_digests") != state.get("migration", {}).get("source_digests"):
                raise MigrationError("old pointer target source binding does not match kernel")
            if head.get("transaction_digest") != old_pointer.get("transaction_digest"):
                raise MigrationError("old pointer target HEAD changed during validation")
        return kernel

    def read(self) -> Dict[str, Any]:
        with self._lock():
            return self._read_unlocked()

    @staticmethod
    def proposal_digest(new_head: Mapping[str, Any], source_digests: Mapping[str, Any], old_pointer: Any) -> str:
        """Return the approval-bound digest for a pointer-only cutover."""

        return canonical_digest({
            "new_pointer": copy.deepcopy(dict(new_head)),
            "source_digests": copy.deepcopy(dict(source_digests)),
            "old_pointer": copy.deepcopy(old_pointer),
        })

    @staticmethod
    def _validate_authority(authority_ref: Optional[Mapping[str, Any]], operation: str) -> Mapping[str, Any]:
        return PointerCutover._validate_authority_tuple(authority_ref, operation)

    @staticmethod
    def _validate_authority_tuple(
        authority_ref: Optional[Mapping[str, Any]],
        operation: str,
        *,
        expected_run_id: Optional[str] = None,
        proposal_digest: Optional[str] = None,
    ) -> Mapping[str, Any]:
        if not isinstance(authority_ref, Mapping):
            raise MigrationError("%s requires authority_ref" % operation)
        required = {"status", "scopes", "migration_approval", "approval_ref", "proposal_digest", "run_id", "role", "assignment_id"}
        if set(authority_ref) != required:
            raise MigrationError("%s authority tuple is incomplete" % operation)
        status = authority_ref.get("status")
        if status not in ("approved", "authorized", "granted"):
            raise MigrationError("%s requires approved authority" % operation)
        scopes = authority_ref.get("scopes") or []
        if not isinstance(scopes, list) or any(not isinstance(scope, str) or not scope for scope in scopes) or len(set(scopes)) != len(scopes) or ("*" not in scopes and operation not in scopes):
            raise MigrationError("authority does not cover %s" % operation)
        if authority_ref.get("migration_approval") is not True:
            raise MigrationError("migration approval is required")
        if not isinstance(authority_ref.get("approval_ref"), str) or not authority_ref["approval_ref"]:
            raise MigrationError("approval_ref is required")
        if authority_ref.get("role") != "orchestrator":
            raise MigrationError("%s requires orchestrator authority" % operation)
        if not isinstance(authority_ref.get("assignment_id"), str) or not _ID.fullmatch(authority_ref["assignment_id"]):
            raise MigrationError("%s requires an authority assignment_id" % operation)
        if not isinstance(authority_ref.get("run_id"), str) or not _ID.fullmatch(authority_ref["run_id"]):
            raise MigrationError("%s authority Run ID is malformed" % operation)
        if expected_run_id is not None and authority_ref.get("run_id") != expected_run_id:
            raise MigrationError("%s authority Run ID does not match target" % operation)
        if proposal_digest is not None and authority_ref.get("proposal_digest") != proposal_digest:
            raise MigrationError("%s authority proposal digest does not match" % operation)
        return copy.deepcopy(dict(authority_ref))

    @staticmethod
    def _pointer_digest(value: Mapping[str, Any]) -> str:
        return _pointer_digest(value)

    def read_active(self) -> Any:
        value = self.read()
        return copy.deepcopy(value["new_pointer"] if value["active"] == "new" else value["old_pointer"])

    def cutover(
        self,
        new_head: Mapping[str, Any],
        *,
        source_digests: Mapping[str, Any],
        old_pointer: Optional[Any] = None,
        expected_old_pointer: Optional[Any] = None,
        authority_ref: Optional[Mapping[str, Any]] = None,
    ) -> Dict[str, Any]:
        if not isinstance(new_head, Mapping):
            raise MigrationError("new_head is required")
        target = NewReader._validate_head_pointer(new_head)
        if not isinstance(source_digests, Mapping) or set(source_digests) != self._REQUIRED_SOURCE_DIGESTS:
            raise MigrationError("source_digests must bind run, bundle, and worker_report")
        if any(not isinstance(digest, str) or not _DIGEST.fullmatch(digest) for digest in source_digests.values()):
            raise MigrationError("source digest is malformed")
        authority = self._validate_authority_tuple(
            authority_ref,
            "migration_cutover",
            expected_run_id=target["run_id"],
        )
        if expected_old_pointer is None:
            raise MigrationError("expected_old_pointer is required for cutover CAS")
        self._validate_old_pointer(old_pointer)
        self._validate_old_pointer(expected_old_pointer)
        with self._lock():
            old = old_pointer
            current = None
            if self.path.exists():
                current = self._read_unlocked()
                current = copy.deepcopy(current["new_pointer"] if current["active"] == "new" else current["old_pointer"])
                if old is None:
                    old = current
            if current != expected_old_pointer and (current is not None or old != expected_old_pointer):
                raise MigrationError("expected old pointer is stale")
            if old != expected_old_pointer:
                raise MigrationError("old pointer does not match expected CAS")
            # Validate a fully described old pointer before touching the
            # pointer file.  Opaque revision/digest-only pointers remain
            # readable for legacy first-cutover compatibility, but they can
            # never pass rollback's ``require_target`` path.
            if isinstance(old, Mapping) and set(old) - {"revision", "digest"}:
                self._validate_old_target_locked(old, source_digests)
            target_kernel = self._target_kernel(target)
            with target_kernel._lock():
                # Keep the target lock through pointer publication.  A target
                # HEAD advance cannot race the pointer's physical validation.
                self._validate_target_locked(target, source_digests)
                proposal = self.proposal_digest(target, source_digests, old)
                if authority.get("proposal_digest") != proposal:
                    raise MigrationError("authority proposal digest does not match cutover")
                value = {
                    "schema": "cutover-pointer/v1",
                    "active": "new",
                    "old_pointer": copy.deepcopy(old),
                    "new_pointer": copy.deepcopy(target),
                    "source_digests": copy.deepcopy(dict(source_digests)),
                    "history_preserved": True,
                    "proposal_digest": proposal,
                    "approval_ref": authority["approval_ref"],
                    "authority": copy.deepcopy(dict(authority)),
                    "reader_pointers": {"old": copy.deepcopy(old), "new": copy.deepcopy(target)},
                }
                value["pointer_digest"] = _pointer_digest(value)
                self._write(value)
                return self._read_unlocked()

    def rollback(
        self,
        *,
        authority_ref: Optional[Mapping[str, Any]] = None,
        expected_active: Optional[Any] = None,
    ) -> Dict[str, Any]:
        authority = self._validate_authority_tuple(authority_ref, "migration_rollback")
        if expected_active is None:
            raise MigrationError("expected_active is required for rollback CAS")
        with self._lock():
            value = self._read_unlocked()
            self._validate_authority_tuple(
                authority,
                "migration_rollback",
                expected_run_id=value["new_pointer"]["run_id"],
                proposal_digest=value["proposal_digest"],
            )
            if value.get("old_pointer") is None:
                raise MigrationError("no old pointer is available for rollback")
            # Validate the old target while the pointer is still unchanged.
            # The target's own Run lock covers HEAD/state/projection reads, so
            # a concurrent target advance cannot be mistaken for the pointer
            # snapshot that the rollback authority approved.
            self._validate_old_pointer(value["old_pointer"], require_target=True)
            old_target = self._old_target_head(value["old_pointer"])
            old_kernel = self._target_kernel(old_target)
            with old_kernel._lock():
                # The old target is allowed to carry the source binding that
                # was current before the cutover.  Do not accidentally
                # validate it against the new candidate's digests: doing so
                # would make a legitimate rollback between two source
                # revisions impossible, while still requiring the pointer's
                # own binding below.
                old_source_digests = value["old_pointer"].get("source_digests") or value["source_digests"]
                self._validate_target_locked(old_target, old_source_digests)
                old_state, old_head = old_kernel._load_current()
                old_pointer = value["old_pointer"]
                if (
                    old_pointer.get("status") != old_state.get("status")
                    or old_pointer.get("source_revision") != old_state.get("migration", {}).get("source_revision")
                    or old_pointer.get("group_id") != old_state.get("group", {}).get("id")
                    or old_pointer.get("epoch_id") != old_state.get("epoch", {}).get("id")
                    or old_pointer.get("aliases") != old_state.get("metadata", {}).get("aliases")
                    or old_pointer.get("source_digests") != old_state.get("migration", {}).get("source_digests")
                ):
                    raise MigrationError("old pointer target lifecycle/source binding does not match")
                if old_head.get("transaction_digest") != old_pointer.get("transaction_digest"):
                    raise MigrationError("old pointer target HEAD changed during rollback validation")
            active = value.get("active")
            if expected_active is not None and active != expected_active:
                raise MigrationError("expected active pointer is stale")
            value["active"] = "old"
            value["rollback_preserved_candidate"] = value.get("new_pointer")
            value["rollback_approval_ref"] = authority["approval_ref"]
            value["pointer_digest"] = _pointer_digest(value)
            self._write(value)
            return self._read_unlocked()


A6RConverter = LegacyConverter
CutoverManager = PointerCutover


__all__ = [
    "A6RConverter",
    "CutoverManager",
    "LegacyConverter",
    "LegacyReader",
    "MigrationError",
    "NewReader",
    "PointerCutover",
]
