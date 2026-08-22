#!/usr/bin/env python3
"""Install one explicitly selected development workflow into a Git project.

The command intentionally has no workflow defaults.  It preflights a temporary
Git checkout and every target write before it touches the selected project.
"""

from __future__ import annotations

import argparse
import errno
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path, PurePosixPath
from typing import Callable, Iterable, Mapping, Sequence


SCRIPT_ROOT = Path(__file__).resolve().parents[1]
DEFAULT_MANIFEST = SCRIPT_ROOT / "manifests" / "project-workflows.json"
STATE_SCHEMA = "project-workflow-selection/v1"


class InitializerError(RuntimeError):
    """A safe-stop condition that leaves the initializer's target untouched."""


class InputRequired(InitializerError):
    pass


class CollisionError(InitializerError):
    pass


class SelectionConflict(InitializerError):
    pass


class UpgradeRequired(InitializerError):
    pass


class CommandFailure(InitializerError):
    pass


@dataclass(frozen=True)
class CommandResult:
    returncode: int
    stdout: str = ""
    stderr: str = ""


class CommandRunner:
    """Small injectable seam for harness and marketplace commands in tests."""

    def run(self, argv: Sequence[str], *, cwd: Path | None = None) -> CommandResult:
        try:
            completed = subprocess.run(
                list(argv),
                cwd=str(cwd) if cwd else None,
                text=True,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                check=False,
            )
        except FileNotFoundError as exc:
            return CommandResult(127, stderr=str(exc))
        return CommandResult(completed.returncode, completed.stdout, completed.stderr)


@dataclass(frozen=True)
class Selection:
    agent: str
    workflow: str


@dataclass(frozen=True)
class PlannedWrite:
    destination: PurePosixPath
    source: Path | None
    content: bytes
    description: str

    def bytes_to_write(self) -> bytes:
        return self.content


@dataclass(frozen=True)
class RunOptions:
    selection: Selection
    target: Path
    dry_run: bool = False
    yes: bool = False
    interactive: bool = False


@dataclass(frozen=True)
class RunResult:
    action: str
    messages: tuple[str, ...]
    exit_code: int = 0


@dataclass(frozen=True)
class SettingsSnapshot:
    """Pre-operation state of the one project file owned by the Claude CLI."""

    target: Path
    path: Path
    existed: bool
    content: bytes | None
    directories_missing_before: tuple[Path, ...]


@dataclass(frozen=True)
class AnchoredEntry:
    """A directory entry anchored to an open parent, with its installed inode."""

    parent_fd: int
    name: str
    device: int
    inode: int


def read_json(path: Path) -> Mapping[str, object]:
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as exc:
        raise InitializerError(f"cannot read JSON {path}: {exc}") from exc
    if not isinstance(value, dict):
        raise InitializerError(f"JSON object required in {path}")
    return value


def safe_relative(value: str) -> PurePosixPath:
    result = PurePosixPath(value)
    if result.is_absolute() or ".." in result.parts or result == PurePosixPath("."):
        raise InitializerError(f"manifest contains unsafe relative path: {value!r}")
    return result


def join_target(root: Path, relative: PurePosixPath) -> Path:
    candidate = root.joinpath(*relative.parts)
    try:
        candidate.resolve(strict=False).relative_to(root.resolve())
    except ValueError as exc:
        raise InitializerError(f"target path escapes repository: {relative}") from exc
    return candidate


def version_at_least(actual: str, minimum: str) -> bool:
    def parts(value: str) -> tuple[int, ...]:
        found = re.search(r"(\d+(?:\.\d+)+)", value)
        if not found:
            raise ValueError(value)
        return tuple(int(item) for item in found.group(1).split("."))

    actual_parts, minimum_parts = parts(actual), parts(minimum)
    width = max(len(actual_parts), len(minimum_parts))
    return actual_parts + (0,) * (width - len(actual_parts)) >= minimum_parts + (0,) * (
        width - len(minimum_parts)
    )


def choose_selection(
    agent: str | None,
    workflow: str | None,
    *,
    interactive: bool,
    input_func: Callable[[str], str] = input,
) -> Selection:
    """Resolve explicit choices, asking only in an interactive terminal."""

    allowed_agents = {"codex", "claude"}
    allowed_workflows = {"aidlc", "superpowers"}
    if agent is None and interactive:
        agent = input_func("Agent harness (codex or claude): ").strip().lower()
    if workflow is None and interactive:
        workflow = input_func("Workflow (aidlc or superpowers): ").strip().lower()
    if agent not in allowed_agents or workflow not in allowed_workflows:
        raise InputRequired(
            "non-interactive use requires both --agent codex|claude and "
            "--workflow aidlc|superpowers; interactive answers have no defaults"
        )
    return Selection(agent=agent, workflow=workflow)


class WorkflowInitializer:
    def __init__(
        self,
        *,
        manifest_path: Path = DEFAULT_MANIFEST,
        source_root: Path = SCRIPT_ROOT,
        runner: CommandRunner | None = None,
    ) -> None:
        self.manifest_path = manifest_path
        self.source_root = source_root
        self.runner = runner or CommandRunner()
        self.manifest = read_json(manifest_path)
        if self.manifest.get("schema") != "project-workflows/v1":
            raise InitializerError("unsupported project-workflows manifest schema")

    def _command(self, argv: Sequence[str], *, cwd: Path | None = None) -> CommandResult:
        result = self.runner.run(argv, cwd=cwd)
        if result.returncode != 0:
            rendered = " ".join(argv)
            detail = result.stderr.strip() or result.stdout.strip() or f"exit {result.returncode}"
            raise CommandFailure(f"command failed: {rendered}: {detail}")
        return result

    def _resolve_target(self, requested: Path) -> Path:
        candidate = requested.expanduser().resolve()
        if not candidate.is_dir():
            raise InitializerError(f"target is not a directory: {candidate}")
        result = self._command(["git", "-C", str(candidate), "rev-parse", "--show-toplevel"])
        return Path(result.stdout.strip()).resolve()

    def _selection_config(self, selection: Selection) -> tuple[Mapping[str, object], Mapping[str, object]]:
        workflows = self.manifest.get("workflows")
        if not isinstance(workflows, dict):
            raise InitializerError("manifest workflows must be an object")
        workflow = workflows.get(selection.workflow)
        if not isinstance(workflow, dict):
            raise InitializerError(f"workflow missing from manifest: {selection.workflow}")
        selections = workflow.get("selections")
        if not isinstance(selections, dict):
            raise InitializerError(f"workflow selections missing: {selection.workflow}")
        config = selections.get(selection.agent)
        if not isinstance(config, dict):
            raise InitializerError(f"unsupported selection: {selection.agent}/{selection.workflow}")
        return workflow, config

    def _check_prerequisites(self, config: Mapping[str, object]) -> None:
        self._command(["git", "--version"])
        requirements = config.get("requirements", [])
        if not isinstance(requirements, list):
            raise InitializerError("manifest requirements must be a list")
        for requirement in requirements:
            if not isinstance(requirement, dict) or not isinstance(requirement.get("command"), str):
                raise InitializerError("invalid command prerequisite in manifest")
            command = str(requirement["command"])
            result = self._command([command, "--version"])
            minimum = requirement.get("minimum_version")
            if minimum is not None:
                if not isinstance(minimum, str):
                    raise InitializerError(f"invalid minimum version for {command}")
                try:
                    compatible = version_at_least(result.stdout + " " + result.stderr, minimum)
                except ValueError as exc:
                    raise InitializerError(f"cannot determine {command} version") from exc
                if not compatible:
                    raise InitializerError(
                        f"{command} {minimum} or newer is required; found {result.stdout.strip()!r}"
                    )

    def _checkout_upstream(self, workflow: Mapping[str, object]) -> tuple[tempfile.TemporaryDirectory[str], Path, str]:
        upstream = workflow.get("upstream")
        if not isinstance(upstream, dict):
            raise InitializerError("manifest workflow has no upstream definition")
        repository, ref = upstream.get("repository"), upstream.get("ref")
        if not isinstance(repository, str) or not isinstance(ref, str):
            raise InitializerError("manifest upstream requires repository and ref")
        if not ref.startswith("refs/heads/") or ref == "refs/heads/":
            raise InitializerError(
                "manifest upstream ref must be a fully qualified branch under refs/heads/"
            )
        remote_ref = "refs/remotes/origin/" + ref.removeprefix("refs/heads/")
        temporary = tempfile.TemporaryDirectory(prefix="agent-workflow-upstream-")
        checkout = Path(temporary.name) / "upstream"
        try:
            # A normal Git checkout deliberately replaces unsafe curl|shell installation paths.
            self._command(["git", "clone", "--quiet", "--no-checkout", repository, str(checkout)])
            commit = self._command(
                ["git", "-C", str(checkout), "rev-parse", "--verify", f"{remote_ref}^{{commit}}"]
            ).stdout.strip()
            if not re.fullmatch(r"[0-9a-f]{40}", commit):
                raise InitializerError(f"upstream ref did not resolve to a Git commit: {commit!r}")
            self._command(
                ["git", "-C", str(checkout), "switch", "--quiet", "--detach", commit]
            )
            checked_out = self._command(
                ["git", "-C", str(checkout), "rev-parse", "HEAD"]
            ).stdout.strip()
            if checked_out != commit:
                raise InitializerError("detached upstream checkout does not match resolved commit")
        except Exception:
            temporary.cleanup()
            raise
        return temporary, checkout, commit

    @staticmethod
    def _files_under(source: Path) -> Iterable[tuple[Path, PurePosixPath]]:
        if source.is_symlink():
            raise InitializerError(f"upstream source may not be a symlink: {source}")
        if source.is_file():
            yield source, PurePosixPath(source.name)
            return
        if not source.is_dir():
            raise InitializerError(f"upstream payload is missing: {source}")
        for path in sorted(source.rglob("*")):
            if path.is_symlink():
                raise InitializerError(f"upstream payload may not contain symlinks: {path}")
            if path.is_file():
                yield path, PurePosixPath(path.relative_to(source).as_posix())

    def _plan_writes(
        self,
        *,
        target: Path,
        selection: Selection,
        config: Mapping[str, object],
        checkout: Path,
        commit: str,
        workflow: Mapping[str, object],
    ) -> list[PlannedWrite]:
        writes: list[PlannedWrite] = []
        targets = config.get("payload_targets", {})
        if not isinstance(targets, dict):
            raise InitializerError("manifest payload_targets must be an object")
        payload = config.get("payload", [])
        if not isinstance(payload, list) or not all(isinstance(item, str) for item in payload):
            raise InitializerError("manifest payload must be a list of paths")
        for payload_path in payload:
            source_relative = safe_relative(payload_path)
            source = checkout.joinpath(*source_relative.parts)
            target_root_text = targets.get(payload_path, payload_path)
            if not isinstance(target_root_text, str):
                raise InitializerError(f"invalid payload target for {payload_path}")
            target_root = safe_relative(target_root_text)
            if source.is_file():
                destination = target_root
                writes.append(
                    PlannedWrite(destination, source, source.read_bytes(), f"upstream {payload_path}")
                )
            else:
                for source_file, suffix in self._files_under(source):
                    writes.append(
                        PlannedWrite(
                            target_root / suffix,
                            source_file,
                            source_file.read_bytes(),
                            f"upstream {payload_path}",
                        )
                    )

        template_text = config.get("workflow_status_template")
        status_target_text = config.get("workflow_status_target")
        if not isinstance(template_text, str) or not isinstance(status_target_text, str):
            raise InitializerError("manifest requires workflow-status template and target")
        template = self.source_root.joinpath(*safe_relative(template_text).parts)
        status_target = safe_relative(status_target_text)
        for source_file, suffix in self._files_under(template):
            writes.append(
                PlannedWrite(
                    status_target / suffix,
                    source_file,
                    source_file.read_bytes(),
                    f"workflow-status template {template_text}",
                )
            )
        shared_text = config.get("workflow_status_shared")
        if shared_text is not None:
            if not isinstance(shared_text, str):
                raise InitializerError("manifest workflow_status_shared must be a path")
            shared = self.source_root.joinpath(*safe_relative(shared_text).parts)
            if not shared.is_file() or shared.is_symlink():
                raise InitializerError(f"workflow-status shared source is missing or unsafe: {shared}")
            writes.append(
                PlannedWrite(
                    status_target / shared.name,
                    shared,
                    shared.read_bytes(),
                    f"workflow-status shared source {shared_text}",
                )
            )

        state_relative = safe_relative(str(self.manifest.get("selection_state", "")))
        upstream = workflow.get("upstream")
        if not isinstance(upstream, dict):
            raise InitializerError("manifest workflow has no upstream")
        state = {
            "schema": STATE_SCHEMA,
            "agent": selection.agent,
            "workflow": selection.workflow,
            "upstream": {
                "repository": upstream["repository"],
                "ref": upstream["ref"],
                "commit": commit,
            },
        }
        adapter = config.get("adapter")
        if isinstance(adapter, str):
            state["adapter"] = adapter
        writes.append(
            PlannedWrite(
                state_relative,
                None,
                (json.dumps(state, indent=2, sort_keys=True) + "\n").encode("utf-8"),
                "workflow selection state",
            )
        )
        self._assert_unique_destinations(writes)
        return writes

    @staticmethod
    def _assert_unique_destinations(writes: Sequence[PlannedWrite]) -> None:
        destinations = [item.destination.as_posix() for item in writes]
        duplicates = sorted({item for item in destinations if destinations.count(item) > 1})
        if duplicates:
            raise InitializerError(f"manifest plans duplicate target writes: {', '.join(duplicates)}")

    def _state_path(self, target: Path) -> Path:
        return join_target(target, safe_relative(str(self.manifest["selection_state"])))

    def _existing_selection(self, target: Path, selection: Selection, commit: str) -> bool:
        state_path = self._state_path(target)
        if not (state_path.exists() or state_path.is_symlink()):
            return False
        if state_path.is_symlink():
            raise CollisionError(f"workflow selection state is a symlink: {state_path}")
        state = read_json(state_path)
        if state.get("schema") != STATE_SCHEMA:
            raise CollisionError(f"unknown workflow selection state schema: {state_path}")
        previous = (state.get("agent"), state.get("workflow"))
        if previous != (selection.agent, selection.workflow):
            raise SelectionConflict(
                "target already selects "
                f"{previous[0]}/{previous[1]}; migration or uninstall is an explicit separate operation"
            )
        upstream = state.get("upstream")
        old_commit = upstream.get("commit") if isinstance(upstream, dict) else None
        if old_commit != commit:
            raise UpgradeRequired(
                "selected workflow resolves to a different upstream revision; upgrades are not supported by rerun"
            )
        return True

    @staticmethod
    def _safe_parent(root: Path, destination: Path, *, create: bool) -> tuple[Path, ...]:
        parent = destination.parent
        relative_parts = parent.relative_to(root).parts
        cursor = root
        created: list[Path] = []
        for part in relative_parts:
            cursor = cursor / part
            if cursor.exists() or cursor.is_symlink():
                if cursor.is_symlink() or not cursor.is_dir():
                    raise CollisionError(f"target parent is not a safe directory: {cursor}")
            elif create:
                cursor.mkdir()
                created.append(cursor)
        return tuple(created)

    @staticmethod
    def _open_parent_anchored(
        root_fd: int, relative: PurePosixPath, *, create: bool
    ) -> tuple[int, list[AnchoredEntry]]:
        """Open a destination parent without following target-controlled symlinks."""

        flags = os.O_RDONLY | os.O_DIRECTORY | getattr(os, "O_CLOEXEC", 0)
        nofollow_flags = flags | getattr(os, "O_NOFOLLOW", 0)
        current_fd = os.dup(root_fd)
        created: list[AnchoredEntry] = []
        try:
            for part in relative.parts[:-1]:
                try:
                    next_fd = os.open(part, nofollow_flags, dir_fd=current_fd)
                except FileNotFoundError:
                    if not create:
                        raise CollisionError(
                            f"target parent disappeared during install: {relative}"
                        )
                    try:
                        os.mkdir(part, dir_fd=current_fd)
                    except FileExistsError:
                        # A concurrent creator won. Re-open it with no-follow below.
                        pass
                    else:
                        metadata = os.stat(part, dir_fd=current_fd, follow_symlinks=False)
                        created.append(
                            AnchoredEntry(
                                os.dup(current_fd), part, metadata.st_dev, metadata.st_ino
                            )
                        )
                    try:
                        next_fd = os.open(part, nofollow_flags, dir_fd=current_fd)
                    except OSError as exc:
                        raise CollisionError(
                            f"target parent changed to an unsafe entry during install: {relative}"
                        ) from exc
                except OSError as exc:
                    if exc.errno in {errno.ELOOP, errno.ENOTDIR}:
                        raise CollisionError(
                            f"target parent changed to an unsafe entry during install: {relative}"
                        ) from exc
                    raise
                os.close(current_fd)
                current_fd = next_fd
            return current_fd, created
        except Exception:
            os.close(current_fd)
            for entry in created:
                os.close(entry.parent_fd)
            raise

    @staticmethod
    def _same_anchored_entry(entry: AnchoredEntry) -> bool:
        try:
            metadata = os.stat(entry.name, dir_fd=entry.parent_fd, follow_symlinks=False)
        except FileNotFoundError:
            return False
        return metadata.st_dev == entry.device and metadata.st_ino == entry.inode

    def _preflight_collisions(
        self, target: Path, writes: Sequence[PlannedWrite], *, idempotent: bool
    ) -> None:
        missing: list[str] = []
        drifted: list[str] = []
        collisions: list[str] = []
        for write in writes:
            destination = join_target(target, write.destination)
            self._safe_parent(target, destination, create=False)
            exists = destination.exists() or destination.is_symlink()
            if idempotent:
                if not exists or destination.is_symlink() or not destination.is_file():
                    missing.append(write.destination.as_posix())
                elif destination.read_bytes() != write.bytes_to_write():
                    drifted.append(write.destination.as_posix())
            elif exists:
                collisions.append(write.destination.as_posix())
        if missing:
            raise InitializerError(
                "selection state claims this revision is installed but expected files are missing: "
                + ", ".join(missing)
            )
        if drifted:
            raise InitializerError(
                "installation drift detected; planned files differ from the selected upstream/template bytes: "
                + ", ".join(drifted)
            )
        if collisions:
            raise CollisionError(
                "collision-free additive installation is not possible; no target writes were made: "
                + ", ".join(collisions)
            )

    def _preflight_cli_owned_settings(self, config: Mapping[str, object], target: Path) -> None:
        """Check the one allowed semantic-write boundary without taking ownership of it."""

        settings_text = config.get("project_settings")
        if not isinstance(settings_text, str):
            raise InitializerError("manifest project_settings must be a path")
        settings = join_target(target, safe_relative(settings_text))
        self._safe_parent(target, settings, create=False)
        if settings.is_symlink() or (settings.exists() and not settings.is_file()):
            raise CollisionError(f"Claude CLI-owned project settings path is unsafe: {settings}")
        if settings.exists():
            # The official CLI may add its own entry, but a malformed existing JSON is
            # neither safely mergeable nor safe to hand to a CLI operation.
            read_json(settings)

    def _ignored_status(self, target: Path, relative: PurePosixPath) -> str:
        result = self.runner.run(
            ["git", "check-ignore", "-v", "--no-index", "--", relative.as_posix()], cwd=target
        )
        if result.returncode == 0:
            rule = result.stdout.strip().replace("\n", " ")
            return f"IGNORED ({rule})"
        if result.returncode == 1:
            return "TRACKED/trackable (not ignored; this command never git-adds)"
        detail = result.stderr.strip() or result.stdout.strip()
        raise CommandFailure(f"git check-ignore failed for {relative}: {detail}")

    def _model_settings_preview(self, config: Mapping[str, object], checkout: Path) -> list[str]:
        paths = config.get("model_settings", [])
        if not isinstance(paths, list):
            raise InitializerError("manifest model_settings must be a list")
        preview: list[str] = []
        for item in paths:
            if not isinstance(item, str):
                raise InitializerError("manifest model_settings entry must be a path")
            source = checkout.joinpath(*safe_relative(item).parts)
            if not source.is_file():
                raise InitializerError(f"official model/provider settings payload is missing: {source}")
            preview.append(f"MODEL/PROVIDER PAYLOAD {item}:")
            preview.extend(f"  {line}" for line in source.read_text(encoding="utf-8").splitlines())
        return preview

    def preview(
        self,
        *,
        target: Path,
        selection: Selection,
        config: Mapping[str, object],
        checkout: Path,
        commit: str,
        writes: Sequence[PlannedWrite],
        idempotent: bool,
    ) -> tuple[str, ...]:
        lines = [
            f"Target Git root: {target}",
            f"Selection: {selection.agent}/{selection.workflow}",
            f"Resolved upstream commit: {commit}",
            f"Mode: {'idempotent validation (no writes)' if idempotent else 'additive installation'}",
        ]
        label = config.get("label")
        if isinstance(label, str):
            lines.append(f"Adapter/install contract: {label}")
        lines.extend(self._model_settings_preview(config, checkout))
        for write in writes:
            lines.append(
                f"WRITE {write.destination.as_posix()} [{self._ignored_status(target, write.destination)}] "
                f"<- {write.description}"
            )
        cli = config.get("project_cli")
        if isinstance(cli, list):
            settings = config.get("project_settings", ".claude/settings.json")
            if not isinstance(settings, str):
                raise InitializerError("manifest project_settings must be a path")
            settings_relative = safe_relative(settings)
            lines.append(
                "CLI-OWNED PROJECT OPERATION "
                + " ".join(str(item) for item in cli)
                + f"; only Claude Code may update {settings} "
                + f"[{self._ignored_status(target, settings_relative)}]."
            )
            inventory = config.get("plugin_inventory")
            if isinstance(inventory, list):
                lines.append("VERIFY AFTER CLI: " + " ".join(str(item) for item in inventory))
        doctor = config.get("doctor")
        if isinstance(doctor, list):
            lines.append("VERIFY AFTER COPY: " + " ".join(str(item) for item in doctor))
        for gate in config.get("manual_gates", []):
            lines.append(f"MANUAL/UNVERIFIED: {gate}")
        for gate in config.get("completion_gates", []):
            lines.append(f"COMPLETION GATE/INCOMPLETE: {gate}")
        return tuple(lines)

    def _apply_transaction(self, target: Path, writes: Sequence[PlannedWrite]) -> None:
        """Install without replacement through anchored parents; rollback owned inodes only."""

        staging = Path(tempfile.mkdtemp(prefix=".agent-workflow-init-", dir=target))
        applied: list[AnchoredEntry] = []
        created_directories: list[AnchoredEntry] = []
        root_flags = os.O_RDONLY | os.O_DIRECTORY | getattr(os, "O_CLOEXEC", 0)
        root_fd = os.open(target, root_flags | getattr(os, "O_NOFOLLOW", 0))
        try:
            for write in writes:
                staged = join_target(staging, write.destination)
                staged.parent.mkdir(parents=True, exist_ok=True)
                staged.write_bytes(write.bytes_to_write())
                if write.source is not None:
                    shutil.copystat(write.source, staged, follow_symlinks=False)
            for write in writes:
                parent_fd, newly_created = self._open_parent_anchored(
                    root_fd, write.destination, create=True
                )
                created_directories.extend(newly_created)
                name = write.destination.name
                try:
                    # Hard-link placement is atomic, stays on the target filesystem, and
                    # fails instead of replacing a leaf created after preflight.
                    os.link(
                        join_target(staging, write.destination),
                        name,
                        dst_dir_fd=parent_fd,
                        follow_symlinks=False,
                    )
                except FileExistsError as exc:
                    raise CollisionError(
                        f"target changed during install: {write.destination}"
                    ) from exc
                metadata = os.stat(name, dir_fd=parent_fd, follow_symlinks=False)
                applied.append(
                    AnchoredEntry(parent_fd, name, metadata.st_dev, metadata.st_ino)
                )
            self._verify_applied_entries(root_fd, writes, applied)
        except Exception:
            for entry in reversed(applied):
                try:
                    if self._same_anchored_entry(entry):
                        os.unlink(entry.name, dir_fd=entry.parent_fd)
                except OSError:
                    pass
            for entry in reversed(created_directories):
                try:
                    if self._same_anchored_entry(entry):
                        os.rmdir(entry.name, dir_fd=entry.parent_fd)
                except OSError:
                    pass
            raise
        finally:
            for entry in applied:
                os.close(entry.parent_fd)
            for entry in created_directories:
                os.close(entry.parent_fd)
            os.close(root_fd)
            shutil.rmtree(staging, ignore_errors=True)

    def _verify_applied_entries(
        self,
        root_fd: int,
        writes: Sequence[PlannedWrite],
        applied: Sequence[AnchoredEntry],
    ) -> None:
        """Verify bytes and that every anchored inode is still reachable at its target path."""

        if len(writes) != len(applied):
            raise InitializerError("post-transaction verification received an incomplete write set")
        drifted: list[str] = []
        file_flags = os.O_RDONLY | getattr(os, "O_CLOEXEC", 0) | getattr(os, "O_NOFOLLOW", 0)
        for write, entry in zip(writes, applied):
            current_parent_fd: int | None = None
            descriptor: int | None = None
            try:
                if not self._same_anchored_entry(entry):
                    raise FileNotFoundError(entry.name)
                descriptor = os.open(entry.name, file_flags, dir_fd=entry.parent_fd)
                with os.fdopen(descriptor, "rb") as installed:
                    descriptor = None
                    if installed.read() != write.bytes_to_write():
                        raise ValueError("byte drift")
                current_parent_fd, created = self._open_parent_anchored(
                    root_fd, write.destination, create=False
                )
                if created:
                    raise AssertionError("read-only parent traversal created directories")
                current = os.stat(
                    write.destination.name,
                    dir_fd=current_parent_fd,
                    follow_symlinks=False,
                )
                if current.st_dev != entry.device or current.st_ino != entry.inode:
                    raise ValueError("target path no longer names installed inode")
            except (OSError, ValueError):
                drifted.append(write.destination.as_posix())
            finally:
                if descriptor is not None:
                    os.close(descriptor)
                if current_parent_fd is not None:
                    os.close(current_parent_fd)
        if drifted:
            raise InitializerError(
                "post-transaction byte verification failed for: " + ", ".join(drifted)
            )

    def _snapshot_cli_owned_settings(
        self, config: Mapping[str, object], target: Path
    ) -> SettingsSnapshot:
        settings_text = config.get("project_settings")
        if not isinstance(settings_text, str):
            raise InitializerError("manifest project_settings must be a path")
        settings = join_target(target, safe_relative(settings_text))
        self._safe_parent(target, settings, create=False)
        if settings.is_symlink() or (settings.exists() and not settings.is_file()):
            raise CollisionError(f"Claude CLI-owned project settings path is unsafe: {settings}")
        missing: list[Path] = []
        cursor = target
        for part in settings.relative_to(target).parts[:-1]:
            cursor = cursor / part
            if not cursor.exists():
                missing.append(cursor)
        return SettingsSnapshot(
            target=target,
            path=settings,
            existed=settings.exists(),
            content=settings.read_bytes() if settings.exists() else None,
            directories_missing_before=tuple(missing),
        )

    def _restore_cli_owned_settings(self, snapshot: SettingsSnapshot) -> None:
        """Rollback only the CLI-owned setting after a failed first installation."""

        settings = snapshot.path
        if snapshot.existed:
            if snapshot.content is None:
                raise AssertionError("existing settings snapshot has no bytes")
            if settings.is_file() and not settings.is_symlink() and settings.read_bytes() == snapshot.content:
                return
            self._safe_parent(snapshot.target, settings, create=True)
            descriptor, temporary_name = tempfile.mkstemp(
                prefix=".agent-workflow-settings-", dir=str(settings.parent)
            )
            temporary = Path(temporary_name)
            try:
                with os.fdopen(descriptor, "wb") as output:
                    output.write(snapshot.content)
                os.replace(temporary, settings)
            finally:
                temporary.unlink(missing_ok=True)
            return

        if settings.is_symlink() or settings.is_file():
            settings.unlink()
        elif settings.exists():
            try:
                settings.rmdir()
            except OSError as exc:
                raise InitializerError(
                    f"cannot safely roll back unexpected non-empty settings path: {settings}"
                ) from exc
        for directory in reversed(snapshot.directories_missing_before):
            if directory.exists():
                try:
                    directory.rmdir()
                except OSError as exc:
                    raise InitializerError(
                        f"cannot safely remove CLI-created non-empty directory during rollback: {directory}"
                    ) from exc

    def _verify_claude_plugin_installation(
        self, config: Mapping[str, object], target: Path
    ) -> str:
        inventory = config.get("plugin_inventory")
        settings_text = config.get("project_settings")
        if not isinstance(inventory, list) or not all(isinstance(item, str) for item in inventory):
            raise InitializerError("manifest plugin_inventory must be a command list")
        if not isinstance(settings_text, str):
            raise InitializerError("manifest project_settings must be a path")
        settings = join_target(target, safe_relative(settings_text))
        if not settings.is_file():
            raise InitializerError("Claude project settings file is missing or unreadable")
        # This deliberately reads but never edits the JSON that the official CLI owns.
        settings_value = read_json(settings)
        enabled = settings_value.get("enabledPlugins")
        plugin_name = "superpowers@claude-plugins-official"
        project_enabled = (
            isinstance(enabled, list)
            and plugin_name in enabled
            or isinstance(enabled, dict)
            and enabled.get(plugin_name) is True
        )
        if not project_enabled:
            raise InitializerError(
                "Claude project settings do not enable superpowers@claude-plugins-official"
            )
        inventory_result = self._command(inventory, cwd=target)
        try:
            inventory_value = json.loads(inventory_result.stdout)
        except json.JSONDecodeError as exc:
            raise InitializerError("Claude plugin inventory did not return valid JSON") from exc
        if plugin_name not in json.dumps(inventory_value, sort_keys=True):
            raise InitializerError("Claude plugin inventory does not confirm superpowers@claude-plugins-official")
        return (
            "VERIFIED: project .claude/settings.json enables superpowers@claude-plugins-official; "
            "JSON plugin inventory independently confirms the installed payload."
        )

    def _run_claude_plugin_operation(self, config: Mapping[str, object], target: Path) -> str:
        operation = config.get("project_cli")
        if not isinstance(operation, list) or not all(isinstance(item, str) for item in operation):
            raise InitializerError("manifest project_cli must be a command list")
        self._command(operation, cwd=target)
        return self._verify_claude_plugin_installation(config, target)

    def _post_install_messages(
        self, config: Mapping[str, object], target: Path
    ) -> tuple[list[str], int]:
        messages: list[str] = []
        exit_code = 0
        doctor = config.get("doctor")
        if isinstance(doctor, list):
            result = self.runner.run([str(item) for item in doctor], cwd=target)
            if result.returncode == 0:
                messages.append("VERIFIED: upstream AI-DLC doctor completed successfully.")
            else:
                messages.append(
                    "INCOMPLETE: upstream AI-DLC doctor did not pass; installed files and selection state "
                    "remain for diagnosis. "
                    + (result.stderr.strip() or result.stdout.strip() or f"exit {result.returncode}")
                )
                exit_code = 1
        for gate in config.get("manual_gates", []):
            messages.append(f"UNVERIFIED: {gate}")
        for gate in config.get("completion_gates", []):
            messages.append(f"INCOMPLETE: {gate}")
            exit_code = 1
        return messages, exit_code

    def run(self, options: RunOptions, *, confirm: Callable[[str], bool] | None = None) -> RunResult:
        workflow, config = self._selection_config(options.selection)
        target = self._resolve_target(options.target)
        self._check_prerequisites(config)
        temporary, checkout, commit = self._checkout_upstream(workflow)
        try:
            writes = self._plan_writes(
                target=target,
                selection=options.selection,
                config=config,
                checkout=checkout,
                commit=commit,
                workflow=workflow,
            )
            idempotent = self._existing_selection(target, options.selection, commit)
            self._preflight_collisions(target, writes, idempotent=idempotent)
            if not idempotent and "project_cli" in config:
                self._preflight_cli_owned_settings(config, target)
            messages = list(
                self.preview(
                    target=target,
                    selection=options.selection,
                    config=config,
                    checkout=checkout,
                    commit=commit,
                    writes=writes,
                    idempotent=idempotent,
                )
            )
            if idempotent:
                if "project_cli" in config:
                    messages.append(self._verify_claude_plugin_installation(config, target))
                verification, exit_code = self._post_install_messages(config, target)
                messages.extend(verification)
                messages.append("IDEMPOTENT: same selection and resolved revision validated; no changes made.")
                action = "incomplete" if exit_code else "idempotent"
                return RunResult(action, tuple(messages), exit_code)
            if options.dry_run:
                messages.append("DRY-RUN: no target files or project CLI operations were changed.")
                return RunResult("dry-run", tuple(messages))
            if not options.yes:
                if not options.interactive or confirm is None:
                    raise InputRequired("non-interactive apply requires --yes; use --dry-run for preview only")
                if not confirm("Apply this complete collision-free preview?"):
                    raise InitializerError("installation cancelled; no target writes were made")
            cli_verification: str | None = None
            cli_snapshot: SettingsSnapshot | None = None
            try:
                if "project_cli" in config:
                    cli_snapshot = self._snapshot_cli_owned_settings(config, target)
                    cli_verification = self._run_claude_plugin_operation(config, target)
                self._apply_transaction(target, writes)
            except Exception as operation_error:
                if cli_snapshot is not None:
                    try:
                        self._restore_cli_owned_settings(cli_snapshot)
                    except Exception as rollback_error:
                        raise InitializerError(
                            f"installation failed and CLI-owned settings rollback also failed: {rollback_error}"
                        ) from operation_error
                raise
            verification, exit_code = self._post_install_messages(config, target)
            if cli_verification is not None:
                messages.append(cli_verification)
            messages.extend(verification)
            if exit_code:
                messages.append(
                    "INCOMPLETE: files and selection state are installed, but completion gates remain."
                )
                return RunResult("incomplete", tuple(messages), exit_code)
            messages.append("INSTALLED: selection state was written atomically under .local/agent/.")
            return RunResult("installed", tuple(messages), exit_code)
        finally:
            temporary.cleanup()


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--agent", choices=("codex", "claude"))
    parser.add_argument("--workflow", choices=("aidlc", "superpowers"))
    parser.add_argument("--target", type=Path, default=Path.cwd())
    parser.add_argument("--dry-run", action="store_true")
    parser.add_argument("--yes", action="store_true")
    return parser


def main(argv: Sequence[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    interactive = sys.stdin.isatty()
    try:
        selection = choose_selection(args.agent, args.workflow, interactive=interactive)
        initializer = WorkflowInitializer()
        result = initializer.run(
            RunOptions(
                selection=selection,
                target=args.target,
                dry_run=args.dry_run,
                yes=args.yes,
                interactive=interactive,
            ),
            confirm=lambda prompt: input(f"{prompt} [y/N] ").strip().lower() in {"y", "yes"},
        )
    except InitializerError as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        return 2
    for line in result.messages:
        print(line)
    return result.exit_code


if __name__ == "__main__":
    raise SystemExit(main())
