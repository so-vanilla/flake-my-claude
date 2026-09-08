"""Deterministic, file-backed walking skeleton for Workflow state.

The module deliberately uses only the Python standard library.  It keeps
Workflow/Group/Skill as the semantic layers and records Context Epoch as
runtime metadata inside a Group.  All mutations require an expected state
revision and are serialized with a per-run file lock.
"""

from __future__ import annotations

import copy
import datetime as _datetime
import fcntl
import hashlib
import json
import os
import re
import tempfile
import uuid
from contextlib import contextmanager
from pathlib import Path
from typing import Any, Dict, Iterable, Iterator, List, Optional, Tuple, Union


BUDGET_POLICY = {
    "target": 200000,
    "normal_limit": 300000,
    "absolute_limit": 500000,
}
TOKEN_STATUSES = {"exact", "estimated", "unavailable"}
_COMPONENT = re.compile(r"^[A-Za-z0-9][A-Za-z0-9._-]*$")


class WorkflowError(RuntimeError):
    """Base error for a rejected or invalid Workflow operation."""


class StaleStateError(WorkflowError):
    """The caller supplied a state/version/digest that is no longer current."""


class InvariantError(WorkflowError):
    """The operation would violate a state or ownership invariant."""


class NotFoundError(WorkflowError):
    """The requested Run or artifact reference does not exist."""


def canonical_digest(value: Any) -> str:
    """Return a stable SHA-256 digest for JSON-compatible data."""

    encoded = json.dumps(
        value,
        ensure_ascii=False,
        sort_keys=True,
        separators=(",", ":"),
        allow_nan=False,
    ).encode("utf-8")
    return hashlib.sha256(encoded).hexdigest()


def _component(value: str, label: str) -> str:
    if not isinstance(value, str) or not _COMPONENT.fullmatch(value):
        raise InvariantError("invalid %s: %r" % (label, value))
    return value


def _now() -> str:
    return _datetime.datetime.now(_datetime.timezone.utc).isoformat()


def _json_load(path: Path) -> Dict[str, Any]:
    try:
        with path.open(encoding="utf-8") as handle:
            value = json.load(handle)
    except FileNotFoundError as exc:
        raise NotFoundError("missing file: %s" % path) from exc
    if not isinstance(value, dict):
        raise InvariantError("expected JSON object: %s" % path)
    return value


def _atomic_json(path: Path, value: Dict[str, Any], mode: int = 0o600) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary = tempfile.mkstemp(
        prefix=".%s." % path.name,
        dir=str(path.parent),
    )
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8") as handle:
            json.dump(value, handle, ensure_ascii=False, sort_keys=True, indent=2)
            handle.write("\n")
        os.chmod(temporary, mode)
        os.replace(temporary, str(path))
    except Exception:
        try:
            os.unlink(temporary)
        except FileNotFoundError:
            pass
        raise


def _append_jsonl(path: Path, value: Dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a", encoding="utf-8") as handle:
        handle.write(json.dumps(value, ensure_ascii=False, sort_keys=True))
        handle.write("\n")
        handle.flush()
        os.fsync(handle.fileno())


def _without_digest(value: Dict[str, Any]) -> Dict[str, Any]:
    copy_value = copy.deepcopy(value)
    copy_value.pop("digest", None)
    return copy_value


def _require_list(value: Any, label: str) -> List[Any]:
    if not isinstance(value, list):
        raise InvariantError("%s must be a list" % label)
    return value


def _require_ref(value: Any, label: str) -> None:
    """Validate the path/version/digest shape used for artifact handoff."""

    if not isinstance(value, dict):
        raise InvariantError("%s must be an object" % label)
    for key in ("path", "version", "digest"):
        if not isinstance(value.get(key), str) or not value[key]:
            raise InvariantError("%s.%s is required" % (label, key))
    if not re.fullmatch(r"[0-9a-f]{64}", value["digest"]):
        raise InvariantError("%s.digest must be a SHA-256 hex digest" % label)


def _safe_relative_path(root: Path, relative: str) -> Path:
    """Resolve a state-stored path without allowing it to escape the Run."""

    candidate = Path(relative)
    if candidate.is_absolute() or ".." in candidate.parts:
        raise InvariantError("path must stay within the Run: %s" % relative)
    resolved = (root / candidate).resolve()
    try:
        resolved.relative_to(root.resolve())
    except ValueError as exc:
        raise InvariantError("path must stay within the Run: %s" % relative) from exc
    return resolved


def _validate_budget(token_status: str, token_count: Optional[int]) -> Dict[str, Any]:
    if token_status not in TOKEN_STATUSES:
        raise InvariantError("token_status must be exact, estimated, or unavailable")
    if token_status == "unavailable":
        if token_count is not None:
            raise InvariantError("unavailable token status cannot carry a token count")
    elif not isinstance(token_count, int) or token_count < 0:
        raise InvariantError("exact/estimated token status requires a non-negative count")
    result = dict(BUDGET_POLICY)
    result["token_status"] = token_status
    result["token_count"] = token_count
    return result


def _validate_bundle(bundle: Dict[str, Any]) -> None:
    required = {
        "schema",
        "bundle_id",
        "version",
        "run_id",
        "workflow_version",
        "group_id",
        "context_epoch",
        "canonical_artifacts",
        "acceptance_evidence",
        "approved_decisions",
        "unresolved_items",
        "invalidated_artifacts",
        "next_inputs",
        "context_budget",
        "digest",
    }
    missing = sorted(required.difference(bundle))
    if missing:
        raise InvariantError("Artifact Bundle missing: %s" % ", ".join(missing))
    if bundle["schema"] != "artifact-bundle/v1":
        raise InvariantError("unsupported Artifact Bundle schema")
    if not isinstance(bundle["context_epoch"], dict):
        raise InvariantError("context_epoch must be an object")
    for key in ("run_id", "workflow_version", "group_id"):
        if not isinstance(bundle.get(key), str) or not bundle[key]:
            raise InvariantError("bundle.%s is required" % key)
    for key in (
        "canonical_artifacts",
        "acceptance_evidence",
        "approved_decisions",
        "unresolved_items",
        "invalidated_artifacts",
        "next_inputs",
    ):
        _require_list(bundle[key], key)
    for index, artifact_ref in enumerate(bundle["canonical_artifacts"]):
        _require_ref(artifact_ref, "canonical_artifacts[%s]" % index)
    budget = bundle["context_budget"]
    if not isinstance(budget, dict):
        raise InvariantError("context_budget must be an object")
    _validate_budget(budget.get("token_status"), budget.get("token_count"))
    if bundle["digest"] != canonical_digest(_without_digest(bundle)):
        raise InvariantError("Artifact Bundle digest mismatch")


class WorkflowStore:
    """Operate on one repository-local `.local/agent` state tree."""

    def __init__(self, root: Union[os.PathLike, str]):
        self.root = Path(root).resolve()
        self.agent_dir = self.root / ".local" / "agent"
        self.runs_dir = self.agent_dir / "runs"
        self.index_path = self.agent_dir / "run-index.jsonl"

    def run_dir(self, run_id: str) -> Path:
        return self.runs_dir / _component(run_id, "run_id")

    def state_path(self, run_id: str) -> Path:
        return self.run_dir(run_id) / "state.json"

    def events_path(self, run_id: str) -> Path:
        return self.run_dir(run_id) / "events.jsonl"

    @contextmanager
    def _lock(self, run_id: str, create: bool = False) -> Iterator[None]:
        run_dir = self.run_dir(run_id)
        if create:
            run_dir.mkdir(parents=True, exist_ok=True)
        elif not run_dir.is_dir():
            raise NotFoundError("unknown Run: %s" % run_id)
        lock_path = run_dir / ".state.lock"
        with lock_path.open("a+", encoding="utf-8") as lock:
            fcntl.flock(lock.fileno(), fcntl.LOCK_EX)
            try:
                yield
            finally:
                fcntl.flock(lock.fileno(), fcntl.LOCK_UN)

    def _load_locked(self, run_id: str) -> Dict[str, Any]:
        state = _json_load(self.state_path(run_id))
        if state.get("schema") != "ai-agent-run-state/v1":
            raise InvariantError("unsupported Run state schema")
        if state.get("run_id") != run_id:
            raise InvariantError("Run ID/path mismatch")
        if not isinstance(state.get("state_revision"), int):
            raise InvariantError("state_revision must be an integer")
        return state

    def load_state(self, run_id: str) -> Dict[str, Any]:
        with self._lock(run_id):
            return self._load_locked(run_id)

    def _append_event_locked(self, run_id: str, event: Dict[str, Any]) -> None:
        _append_jsonl(self.events_path(run_id), event)

    def _append_index(self, state: Dict[str, Any]) -> None:
        self.agent_dir.mkdir(parents=True, exist_ok=True)
        index_lock_path = self.agent_dir / ".run-index.lock"
        with index_lock_path.open("a+", encoding="utf-8") as lock:
            fcntl.flock(lock.fileno(), fcntl.LOCK_EX)
            try:
                row = {
                    "schema": "ai-agent-run-index/v1",
                    "event_type": "run_reindexed",
                    "event_id": "idx-%s" % uuid.uuid4().hex,
                    "run_id": state["run_id"],
                    "aliases": state["aliases"],
                    "external_refs": state["external_refs"],
                    "objective_version": state["objective_ref"]["version"],
                    "workflow_version": state["workflow_version"],
                    "state_revision": state["state_revision"],
                    "status": state["status"],
                    "current_group": state["current_group"]["id"],
                    "current_epoch": state["current_epoch"]["id"],
                    "last_checkpoint": state.get("last_checkpoint"),
                    "next_recommended_skill": state["current_group"].get("next_skill"),
                    "updated_at": state["updated_at"],
                }
                _append_jsonl(self.index_path, row)
            finally:
                fcntl.flock(lock.fileno(), fcntl.LOCK_UN)

    def _commit_locked(
        self,
        run_id: str,
        previous: Dict[str, Any],
        new_state: Dict[str, Any],
        expected_revision: int,
        event_type: str,
        payload: Dict[str, Any],
    ) -> Dict[str, Any]:
        if previous["state_revision"] != expected_revision:
            raise StaleStateError(
                "state revision mismatch: expected %s, current %s"
                % (expected_revision, previous["state_revision"])
            )
        new_state["state_revision"] = expected_revision + 1
        new_state["updated_at"] = _now()
        event = {
            "schema": "ai-agent-event/v1",
            "event_id": "evt-%s" % uuid.uuid4().hex,
            "event_type": event_type,
            "run_id": run_id,
            "actor": "workflow-root",
            "parent_revision": expected_revision,
            "produced_revision": new_state["state_revision"],
            "occurred_at": new_state["updated_at"],
        }
        event.update(payload)
        _atomic_json(self.state_path(run_id), new_state)
        self._append_event_locked(run_id, event)
        self._append_index(new_state)
        return copy.deepcopy(new_state)

    def entry(
        self,
        run_id: str,
        objective_ref: Dict[str, Any],
        workflow_version: str,
        group_id: str = "bootstrap",
        epoch_id: str = "epoch-0001",
        aliases: Optional[Iterable[str]] = None,
        external_refs: Optional[Iterable[str]] = None,
        authority: Optional[Dict[str, List[str]]] = None,
    ) -> Dict[str, Any]:
        run_id = _component(run_id, "run_id")
        _component(group_id, "group_id")
        _component(epoch_id, "epoch_id")
        if not isinstance(objective_ref, dict):
            raise InvariantError("objective_ref must be an object")
        for key in ("path", "version", "digest", "approval_status"):
            if not objective_ref.get(key):
                raise InvariantError("objective_ref.%s is required" % key)
        run_dir = self.run_dir(run_id)
        if run_dir.exists():
            raise InvariantError("Run already exists: %s" % run_id)
        state = {
            "schema": "ai-agent-run-state/v1",
            "run_id": run_id,
            "workflow_version": workflow_version,
            "state_revision": 0,
            "status": "active",
            "objective_ref": copy.deepcopy(objective_ref),
            "aliases": list(aliases or []),
            "external_refs": list(external_refs or []),
            "authority": authority or {"approved": [], "not_approved": []},
            "current_group": {
                "id": group_id,
                "status": "open",
                "completed_skills": [],
                "next_skill": "artifact-producing-skill",
            },
            "current_epoch": {
                "id": epoch_id,
                "group_id": group_id,
                "status": "open",
                "started_at_revision": 0,
                "boundary_reason": "entry",
                "input_bundle": None,
                "clear_before_start": False,
            },
            "context_budget": dict(BUDGET_POLICY),
            "artifacts": {},
            "last_checkpoint": None,
            "unresolved_items": [],
            "invalidated_artifacts": [],
            "next_inputs": [],
        }
        with self._lock(run_id, create=True):
            if self.state_path(run_id).exists():
                raise InvariantError("Run already exists: %s" % run_id)
            committed = self._commit_locked(
                run_id,
                state,
                state,
                0,
                "run_entered",
                {
                    "group_id": group_id,
                    "context_epoch": epoch_id,
                    "objective_ref": copy.deepcopy(objective_ref),
                },
            )
            return committed

    def _check_revision(self, state: Dict[str, Any], expected_revision: int) -> None:
        current = state["state_revision"]
        if current != expected_revision:
            raise StaleStateError(
                "state revision mismatch: expected %s, current %s" % (expected_revision, current)
            )

    def produce_artifact(
        self,
        run_id: str,
        expected_revision: int,
        artifact_id: str,
        version: str,
        payload: Dict[str, Any],
        acceptance_evidence: Optional[List[str]] = None,
    ) -> Dict[str, Any]:
        _component(artifact_id, "artifact_id")
        _component(version, "artifact_version")
        if not isinstance(payload, dict):
            raise InvariantError("artifact payload must be an object")
        with self._lock(run_id):
            previous = self._load_locked(run_id)
            self._check_revision(previous, expected_revision)
            epoch = previous["current_epoch"]
            if epoch["status"] != "open":
                raise InvariantError("cannot produce an artifact in a closed Epoch")
            envelope = {
                "schema": "artifact/v1",
                "artifact_id": artifact_id,
                "version": version,
                "run_id": run_id,
                "group_id": previous["current_group"]["id"],
                "epoch_id": epoch["id"],
                "payload": copy.deepcopy(payload),
            }
            digest = canonical_digest(envelope)
            artifact_path = "artifacts/%s-%s.json" % (artifact_id, version)
            existing = previous["artifacts"].get(artifact_id)
            if existing:
                if existing.get("version") == version and existing.get("digest") == digest:
                    raise InvariantError("artifact already exists: %s@%s" % (artifact_id, version))
                raise InvariantError("artifact_id is immutable within a Run: %s" % artifact_id)
            _atomic_json(
                _safe_relative_path(self.run_dir(run_id), artifact_path),
                dict(envelope, digest=digest),
                0o600,
            )
            new_state = copy.deepcopy(previous)
            new_state["artifacts"][artifact_id] = {
                "path": artifact_path,
                "version": version,
                "digest": digest,
                "group_id": previous["current_group"]["id"],
                "epoch_id": epoch["id"],
                "acceptance_evidence": list(acceptance_evidence or []),
                "produced_at_revision": expected_revision + 1,
            }
            new_state["current_group"]["next_skill"] = "close-epoch"
            return self._commit_locked(
                run_id,
                previous,
                new_state,
                expected_revision,
                "artifact_produced",
                {
                    "artifact_ref": {
                        "path": artifact_path,
                        "version": version,
                        "digest": digest,
                    }
                },
            )

    def close_epoch(
        self,
        run_id: str,
        expected_revision: int,
        boundary_reason: str,
        acceptance_evidence: List[str],
        approved_decisions: List[str],
        unresolved_items: List[Dict[str, Any]],
        invalidated_artifacts: List[Dict[str, Any]],
        next_inputs: List[Dict[str, Any]],
        next_epoch_id: str,
        clear_before_next: bool = True,
        token_status: str = "unavailable",
        token_count: Optional[int] = None,
    ) -> Tuple[Dict[str, Any], Dict[str, Any]]:
        _component(next_epoch_id, "next_epoch_id")
        budget = _validate_budget(token_status, token_count)
        with self._lock(run_id):
            previous = self._load_locked(run_id)
            self._check_revision(previous, expected_revision)
            epoch = previous["current_epoch"]
            if epoch["status"] != "open":
                raise InvariantError("Epoch is already closed")
            new_revision = expected_revision + 1
            canonical_artifacts = [
                copy.deepcopy(value)
                for _, value in sorted(previous["artifacts"].items())
                if value.get("epoch_id") == epoch["id"]
            ]
            bundle = {
                "schema": "artifact-bundle/v1",
                "bundle_id": "%s-%s-bundle" % (previous["current_group"]["id"], epoch["id"]),
                "version": "v1",
                "run_id": run_id,
                "workflow_version": previous["workflow_version"],
                "group_id": previous["current_group"]["id"],
                "context_epoch": {
                    "id": epoch["id"],
                    "group_id": epoch["group_id"],
                    "started_at_revision": epoch["started_at_revision"],
                    "closed_at_revision": new_revision,
                    "boundary_reason": boundary_reason,
                    "next_epoch": next_epoch_id,
                    "clear_before_next": clear_before_next,
                },
                "canonical_artifacts": canonical_artifacts,
                "acceptance_evidence": list(acceptance_evidence),
                "approved_decisions": list(approved_decisions),
                "unresolved_items": copy.deepcopy(unresolved_items),
                "invalidated_artifacts": copy.deepcopy(invalidated_artifacts),
                "next_inputs": copy.deepcopy(next_inputs),
                "context_budget": budget,
            }
            bundle["digest"] = canonical_digest(bundle)
            _validate_bundle(bundle)
            bundle_path = "artifacts/bundles/%s-%s-bundle.json" % (
                previous["current_group"]["id"],
                epoch["id"],
            )
            _atomic_json(
                _safe_relative_path(self.run_dir(run_id), bundle_path), bundle, 0o600
            )
            checkpoint = {
                "schema": "checkpoint/v1",
                "checkpoint_id": "cp-%s" % epoch["id"],
                "run_id": run_id,
                "workflow_version": previous["workflow_version"],
                "state_revision": new_revision,
                "group_id": previous["current_group"]["id"],
                "context_epoch": epoch["id"],
                "objective_ref": copy.deepcopy(previous["objective_ref"]),
                "bundle_ref": {
                    "path": bundle_path,
                    "version": bundle["version"],
                    "digest": bundle["digest"],
                },
                "acceptance_evidence": list(acceptance_evidence),
                "approved_decisions": list(approved_decisions),
                "unresolved_items": copy.deepcopy(unresolved_items),
                "invalidated_artifacts": copy.deepcopy(invalidated_artifacts),
                "next_inputs": copy.deepcopy(next_inputs),
                "clear_before_start": clear_before_next,
            }
            checkpoint_path = "checkpoints/%s.json" % epoch["id"]
            _atomic_json(
                _safe_relative_path(self.run_dir(run_id), checkpoint_path), checkpoint, 0o600
            )
            new_state = copy.deepcopy(previous)
            new_state["status"] = "paused_after_epoch"
            new_state["context_budget"] = budget
            new_state["current_epoch"].update(
                {
                    "status": "closed",
                    "closed_at_revision": new_revision,
                    "boundary_reason": boundary_reason,
                    "bundle_ref": {
                        "path": bundle_path,
                        "version": bundle["version"],
                        "digest": bundle["digest"],
                    },
                    "next_epoch": next_epoch_id,
                    "clear_before_next": clear_before_next,
                }
            )
            new_state["current_group"]["next_skill"] = "open-epoch"
            new_state["unresolved_items"] = copy.deepcopy(unresolved_items)
            new_state["invalidated_artifacts"] = copy.deepcopy(invalidated_artifacts)
            new_state["next_inputs"] = copy.deepcopy(next_inputs)
            new_state["last_checkpoint"] = {
                "id": checkpoint["checkpoint_id"],
                "path": checkpoint_path,
                "state_revision": new_revision,
                "bundle_digest": bundle["digest"],
            }
            committed = self._commit_locked(
                run_id,
                previous,
                new_state,
                expected_revision,
                "epoch_closed",
                {
                    "context_epoch": copy.deepcopy(new_state["current_epoch"]),
                    "artifact_bundle_ref": new_state["current_epoch"]["bundle_ref"],
                    "checkpoint_ref": new_state["last_checkpoint"],
                },
            )
            return committed, bundle

    def open_epoch(
        self,
        run_id: str,
        expected_revision: int,
        epoch_id: str,
        input_bundle: Dict[str, str],
        boundary_reason: str,
        clear_before_start: bool = True,
    ) -> Dict[str, Any]:
        _component(epoch_id, "epoch_id")
        if not clear_before_start:
            raise InvariantError("the walking skeleton requires clear_before_start=true")
        for key in ("path", "version", "digest"):
            if not input_bundle.get(key):
                raise InvariantError("input_bundle.%s is required" % key)
        with self._lock(run_id):
            previous = self._load_locked(run_id)
            self._check_revision(previous, expected_revision)
            old_epoch = previous["current_epoch"]
            if old_epoch["status"] != "closed":
                raise InvariantError("new Epoch requires a closed previous Epoch")
            if not old_epoch.get("clear_before_next"):
                raise InvariantError("previous Epoch did not request a clear boundary")
            bundle_path = _safe_relative_path(self.run_dir(run_id), input_bundle["path"])
            bundle = _json_load(bundle_path)
            _validate_bundle(bundle)
            if (
                bundle.get("version") != input_bundle["version"]
                or bundle.get("digest") != input_bundle["digest"]
            ):
                raise StaleStateError("input Artifact Bundle version/digest is stale")
            if bundle.get("workflow_version") != previous["workflow_version"]:
                raise StaleStateError("input Artifact Bundle workflow version is stale")
            new_state = copy.deepcopy(previous)
            new_state["status"] = "active"
            new_state["current_epoch"] = {
                "id": epoch_id,
                "group_id": previous["current_group"]["id"],
                "status": "open",
                "started_at_revision": expected_revision + 1,
                "boundary_reason": boundary_reason,
                "input_bundle": copy.deepcopy(input_bundle),
                "clear_before_start": clear_before_start,
            }
            new_state["current_group"]["status"] = "open"
            new_state["current_group"]["next_skill"] = "artifact-producing-skill"
            return self._commit_locked(
                run_id,
                previous,
                new_state,
                expected_revision,
                "epoch_opened",
                {
                    "context_epoch": copy.deepcopy(new_state["current_epoch"]),
                    "input_bundle_ref": copy.deepcopy(input_bundle),
                    "clear_before_start": clear_before_start,
                },
            )

    def close_group(
        self,
        run_id: str,
        expected_revision: int,
        acceptance_evidence: List[str],
        approved_decisions: List[str],
        unresolved_items: List[Dict[str, Any]],
        invalidated_artifacts: List[Dict[str, Any]],
        next_inputs: List[Dict[str, Any]],
        next_group: Optional[str] = None,
    ) -> Tuple[Dict[str, Any], Dict[str, Any]]:
        with self._lock(run_id):
            previous = self._load_locked(run_id)
            self._check_revision(previous, expected_revision)
            epoch = previous["current_epoch"]
            group = previous["current_group"]
            if epoch["status"] != "closed":
                raise InvariantError("Group cannot close while its current Epoch is open")
            if not epoch.get("clear_before_next"):
                raise InvariantError("Group close requires a clear boundary")
            if next_group is not None:
                _component(next_group, "next_group")
            new_revision = expected_revision + 1
            epoch_refs = []
            if epoch.get("bundle_ref"):
                epoch_refs.append(copy.deepcopy(epoch["bundle_ref"]))
            bundle = {
                "schema": "artifact-bundle/v1",
                "bundle_id": "%s-group-bundle" % group["id"],
                "version": "v1",
                "run_id": run_id,
                "workflow_version": previous["workflow_version"],
                "group_id": group["id"],
                "context_epoch": {
                    "id": "group-close",
                    "group_id": group["id"],
                    "started_at_revision": epoch.get("started_at_revision"),
                    "closed_at_revision": new_revision,
                    "boundary_reason": "Level 2 Group exit and clear boundary",
                    "next_epoch": None,
                    "clear_before_next": True,
                },
                "canonical_artifacts": epoch_refs + [
                    copy.deepcopy(value) for _, value in sorted(previous["artifacts"].items())
                ],
                "acceptance_evidence": list(acceptance_evidence),
                "approved_decisions": list(approved_decisions),
                "unresolved_items": copy.deepcopy(unresolved_items),
                "invalidated_artifacts": copy.deepcopy(invalidated_artifacts),
                "next_inputs": copy.deepcopy(next_inputs),
                "context_budget": dict(previous["context_budget"]),
            }
            bundle["digest"] = canonical_digest(bundle)
            _validate_bundle(bundle)
            bundle_path = "artifacts/bundles/%s-group-bundle.json" % group["id"]
            _atomic_json(
                _safe_relative_path(self.run_dir(run_id), bundle_path), bundle, 0o600
            )
            checkpoint = {
                "schema": "checkpoint/v1",
                "checkpoint_id": "cp-group-%s" % group["id"],
                "run_id": run_id,
                "workflow_version": previous["workflow_version"],
                "state_revision": new_revision,
                "group_id": group["id"],
                "context_epoch": None,
                "objective_ref": copy.deepcopy(previous["objective_ref"]),
                "bundle_ref": {
                    "path": bundle_path,
                    "version": bundle["version"],
                    "digest": bundle["digest"],
                },
                "acceptance_evidence": list(acceptance_evidence),
                "approved_decisions": list(approved_decisions),
                "unresolved_items": copy.deepcopy(unresolved_items),
                "invalidated_artifacts": copy.deepcopy(invalidated_artifacts),
                "next_inputs": copy.deepcopy(next_inputs),
                "clear_before_start": True,
            }
            checkpoint_path = "checkpoints/group-%s.json" % group["id"]
            _atomic_json(
                _safe_relative_path(self.run_dir(run_id), checkpoint_path), checkpoint, 0o600
            )
            new_state = copy.deepcopy(previous)
            new_state["status"] = "paused_after_group"
            new_state["current_group"].update(
                {
                    "status": "closed",
                    "next_group": next_group,
                    "next_skill": None,
                    "clear_required": True,
                }
            )
            new_state["last_checkpoint"] = {
                "id": checkpoint["checkpoint_id"],
                "path": checkpoint_path,
                "state_revision": new_revision,
                "bundle_digest": bundle["digest"],
            }
            new_state["unresolved_items"] = copy.deepcopy(unresolved_items)
            new_state["invalidated_artifacts"] = copy.deepcopy(invalidated_artifacts)
            new_state["next_inputs"] = copy.deepcopy(next_inputs)
            committed = self._commit_locked(
                run_id,
                previous,
                new_state,
                expected_revision,
                "group_closed",
                {
                    "group_id": group["id"],
                    "artifact_bundle_ref": new_state["last_checkpoint"],
                    "checkpoint_ref": new_state["last_checkpoint"],
                    "clear_required": True,
                },
            )
            return committed, bundle

    def _find_run_id(self, query: str) -> str:
        try:
            if self.state_path(query).exists():
                return query
        except InvariantError:
            # Issue URLs and human aliases are deliberately allowed as lookup
            # queries even though they are not safe path components.
            pass
        if not self.index_path.exists():
            raise NotFoundError("no Run index")
        matches: List[str] = []
        with self.index_path.open(encoding="utf-8") as handle:
            for line in handle:
                row = json.loads(line)
                if row.get("run_id") == query:
                    matches.append(row["run_id"])
                elif query in row.get("aliases", []):
                    matches.append(row["run_id"])
                elif query in row.get("external_refs", []):
                    matches.append(row["run_id"])
        if not matches:
            raise NotFoundError("Run not found for query: %s" % query)
        return matches[-1]

    def resume(
        self,
        query: str,
        expected_revision: Optional[int] = None,
        expected_workflow_version: Optional[str] = None,
        expected_bundle_digest: Optional[str] = None,
    ) -> Dict[str, Any]:
        run_id = self._find_run_id(query)
        state = self.load_state(run_id)
        if expected_revision is not None and state["state_revision"] != expected_revision:
            raise StaleStateError(
                "state revision mismatch: expected %s, current %s"
                % (expected_revision, state["state_revision"])
            )
        if (
            expected_workflow_version is not None
            and state["workflow_version"] != expected_workflow_version
        ):
            raise StaleStateError(
                "workflow version mismatch: expected %s, current %s"
                % (expected_workflow_version, state["workflow_version"])
            )
        current_bundle = state["current_epoch"].get("bundle_ref")
        checkpoint_bundle = (state.get("last_checkpoint") or {}).get("bundle_digest")
        known_digests = set()
        if current_bundle and current_bundle.get("digest"):
            known_digests.add(current_bundle["digest"])
        if checkpoint_bundle:
            known_digests.add(checkpoint_bundle)
        if expected_bundle_digest is not None and expected_bundle_digest not in known_digests:
            raise StaleStateError("Artifact Bundle digest is stale")
        return {
            "run_id": run_id,
            "workflow_version": state["workflow_version"],
            "state_revision": state["state_revision"],
            "status": state["status"],
            "objective_ref": copy.deepcopy(state["objective_ref"]),
            "current_group": copy.deepcopy(state["current_group"]),
            "current_epoch": copy.deepcopy(state["current_epoch"]),
            "last_checkpoint": copy.deepcopy(state.get("last_checkpoint")),
            "unresolved_items": copy.deepcopy(state.get("unresolved_items", [])),
            "invalidated_artifacts": copy.deepcopy(state.get("invalidated_artifacts", [])),
            "next_inputs": copy.deepcopy(state.get("next_inputs", [])),
            "authority": copy.deepcopy(state["authority"]),
        }

    def status(self, query: str) -> Dict[str, Any]:
        return self.resume(query)

    def update_objective(self, run_id: str, expected_revision: int, objective_ref: Dict[str, Any]) -> None:
        """Reject implicit objective mutation; approval is outside this skeleton."""

        del run_id, expected_revision, objective_ref
        raise InvariantError(
            "objective changes require a separate approved version and migration event"
        )


__all__ = [
    "BUDGET_POLICY",
    "InvariantError",
    "NotFoundError",
    "StaleStateError",
    "WorkflowError",
    "WorkflowStore",
    "canonical_digest",
]
