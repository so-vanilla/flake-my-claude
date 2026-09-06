"""A small, file-backed Artifact/Task DAG control kernel.

This module is the isolated A6R implementation.  It intentionally has no
dependencies outside the Python standard library.  Objects and transactions
are content addressed, ``HEAD`` is the only mutable canonical pointer, and
all human-facing files are projections that can be rebuilt from that pointer.

The API is deliberately boring: workers and reviewers submit envelopes to an
inbox, while a deterministic orchestrator validates and commits them.  This
keeps the authority boundary visible in tests and in the on-disk format.
"""

from __future__ import annotations

import copy
import datetime as _datetime
import fcntl
import hashlib
import json
import os
import re
import shutil
import tempfile
import uuid
from contextlib import contextmanager
from pathlib import Path
from typing import Any, Dict, Iterable, Iterator, List, Mapping, Optional, Sequence, Tuple, Union


class KernelError(RuntimeError):
    """Base error for a rejected kernel operation."""


class StaleHeadError(KernelError):
    """The command was based on a HEAD that is no longer current."""


class AuthorizationError(KernelError):
    """The actor, assignment, or authority does not permit the operation."""


class CommandValidationError(KernelError):
    """A command envelope is not a complete, versioned v1 command."""


class ObjectValidationError(KernelError):
    """A durable DAG object does not satisfy its runtime contract."""


class ResultAcceptanceError(KernelError):
    """A task result cannot be accepted under its lease/result contract."""


class LifecycleClosedError(KernelError):
    """The Run, Group, or current Epoch no longer accepts mutation."""


class ReviewProvenanceError(AuthorizationError):
    """A review is not registered, fresh, independent, or object-bound."""


class BudgetError(KernelError):
    """Context budget input is invalid or exceeds a lifecycle boundary."""


class ProtectedFieldError(AuthorizationError):
    """A protected field was changed without a matching approval receipt."""


class DuplicateCommandError(KernelError):
    """An idempotency key was reused with a different command."""


class IntegrityBlockedError(KernelError):
    """Canonical history or an object failed an integrity check."""


class DAGCycleError(IntegrityBlockedError):
    """The execution edge projection contains a cycle."""


class NotFoundError(KernelError):
    """A requested run, object, transaction, task, or finding is absent."""


class InjectedCrash(KernelError):
    """A deliberate fault injection stopped a commit at a named boundary."""


SCHEMA_OBJECT = "dag-object/v1"
SCHEMA_TRANSACTION = "dag-transaction/v1"
SCHEMA_COMMAND = "dag-command/v1"
SCHEMA_HEAD = "dag-head/v1"
SCHEMA_STATE = "dag-state/v1"
GRAPH_VERSION = "artifact-task-dag/v1"
KERNEL_VERSION = "control-kernel/v3"
TOKEN_STATUSES = {"exact", "estimated", "unavailable"}
BUDGET_POLICY = {"target": 200000, "normal_limit": 300000, "absolute_limit": 500000}
EDGE_TYPES = {"requires", "produces", "authorizes", "verdict-for", "converges"}
# The graph design intentionally has no Task -> Task execution edge.  A task
# dependency is represented by the producing Task's Artifact/work-product and
# a consuming Task's ``requires`` edge.  Keep this table as the single source
# of truth for the validator, reducer/compiler and recovery reader.  The
# concrete ``work-product``/``review`` records are the kernel's Artifact-like
# execution records; they are not permission to add a new direction.
EDGE_ENDPOINTS = {
    "requires": {
        ("artifact", "task"),
        ("work-product", "task"),
    },
    "produces": {
        ("task", "artifact"),
        ("task", "work-product"),
        ("review", "finding"),
    },
    # Authority/Approval Artifacts authorize a Task only.  Authority is not a
    # general-purpose edge to immutable output, review, or Finding objects;
    # those objects retain the command's authority reference as provenance.
    "authorizes": {
        ("authority", "task"),
        ("approval", "task"),
    },
    "verdict-for": {
        ("review", "finding"),
    },
    "converges": {
        ("artifact", "task"),
        ("work-product", "task"),
    },
}
FINDING_STATES = {"open", "resolved", "unresolved", "invalid", "superseded"}
COMMAND_TYPES = {
    "entry",
    "approve_objective",
    "publish_artifact",
    "publish_task_package",
    "migrate_legacy",
    "accept_task_result",
    "open_review_epoch",
    "open_review",
    "accept_review",
    "accept_resolution_claim",
    "accept_finding_closure",
    "validate_findings",
    "terminal_review",
    "reopen_review",
    "claim_task",
    "release_task",
    "invalidate_task",
    "replan_task",
    "open_epoch",
    "close_epoch",
    "close_group",
    "open_section",
    "open_group",
}
_OBJECT_TYPES = {
    "objective",
    "objective-candidate",
    "approved-objective",
    "objective-approval",
    "objective-approval-event",
    "artifact",
    "task-package",
    "work-product",
    "review-package",
    "finding",
    "finding-resolution",
    "finding-verdict",
    "evaluation-package",
    "finding-validation",
    "budget-terminal",
    "artifact-bundle",
    "checkpoint",
    "legacy-run",
    "legacy-bundle",
    "legacy-worker-report",
    "legacy-artifact",
}
_DURABLE_FORBIDDEN_KEYS = {
    "secret",
    "secrets",
    "credential",
    "credentials",
    "authorization",
    "auth",
    "apikey",
    "accesskey",
    "accesstoken",
    "authtoken",
    "privatekey",
    "cookie",
    "cookies",
    "session",
    "sessionid",
    "bearertoken",
    "bearer",
    "refresh",
    "refreshtoken",
    "sessiontoken",
    "password",
    "passwd",
    "passphrase",
    "token",
    "privatecontext",
    "privatereasoning",
    "modelthought",
    "modelthoughts",
    "chainofthought",
    "transcript",
    "fulltranscript",
    "rawoutput",
    "rawtooloutput",
    "tooloutput",
    "toolresult",
}
_DURABLE_FORBIDDEN_TEXT_MARKERS = (
    "authorization",
    "authentication",
    "auth token",
    "auth_token",
    "auth-token",
    "auth header",
    "auth_header",
    "auth credential",
    "auth_credential",
    "apikey",
    "api_key",
    "api-key",
    "accesskey",
    "access_key",
    "access-key",
    "access_token",
    "access-token",
    "access token",
    "access key",
    "access_key",
    "private key",
    "private_key",
    "private-key",
    "cookie",
    "session",
    "session id",
    "session_id",
    "session-id",
    "refresh",
    "refresh token",
    "refresh-token",
    "bearer",
    "bearer token",
    "bearer-token",
    "password",
    "credential",
    "private reasoning",
    "private_reasoning",
    "private-reasoning",
    "private context",
    "private_context",
    "private-context",
    "model thoughts",
    "model_thoughts",
    "model-thoughts",
    "chain of thought",
    "chain-of-thought",
    "transcript",
    "raw output",
    "raw tool output",
    "raw_tool_output",
    "raw-tool-output",
    "tool result",
    "tool output",
    "tool_output",
    "tool-result",
    "tool-output",
    "token value",
    "token_value",
    "token header",
    "token_header",
    "token",
)
# Opaque strings are a separate persistence surface from structured keys.  A
# marker list alone is easy to bypass with a bare, case-variant word such as
# ``AUTH`` or ``temporary auth``.  Keep this expression conservative and
# token-boundary based: reference paths are explicitly exempted below, while
# prose or opaque values carrying an authentication/secret boundary are not
# durable context.
_DURABLE_FORBIDDEN_TEXT_RE = re.compile(
    r"(?i)(?<![a-z0-9])(?:"
    r"authorization|authentication|auth|access|"
    r"private(?:[\s_-]+(?:key|reasoning|context))|"
    r"cookie(?:s)?|session(?:[\s_-]*id)?|"
    r"refresh(?:[\s_-]*token)?|bearer(?:[\s_-]*token)?|"
    r"password|passwd|passphrase|credential(?:s)?|"
    r"token(?:[\s_-]*(?:value|header))?|"
    r"model[\s_-]*thoughts?|chain[\s_-]*of[\s_-]*thought|transcript|"
    r"raw[\s_-]*(?:output|tool[\s_-]*output)|"
    r"tool[\s_-]*(?:result|output)"
    r")(?![a-z0-9])"
)
_ID = re.compile(r"^[A-Za-z0-9][A-Za-z0-9_.:@/-]*$")
_DIGEST = re.compile(r"^sha256:[0-9a-f]{64}$")


def _json_bytes(value: Any) -> bytes:
    return json.dumps(
        value,
        ensure_ascii=False,
        sort_keys=True,
        separators=(",", ":"),
        allow_nan=False,
    ).encode("utf-8")


def canonical_digest(value: Any) -> str:
    """Return the prefixed SHA-256 of a JSON-compatible value."""

    return "sha256:" + hashlib.sha256(_json_bytes(value)).hexdigest()


def _without_digest(value: Mapping[str, Any]) -> Dict[str, Any]:
    result = copy.deepcopy(dict(value))
    result.pop("digest", None)
    return result


def _now() -> str:
    return _datetime.datetime.now(_datetime.timezone.utc).isoformat()


def _id(value: str, label: str = "identifier") -> str:
    if not isinstance(value, str) or not _ID.fullmatch(value):
        raise KernelError("invalid %s: %r" % (label, value))
    return value


def _digest(value: Any) -> str:
    result = canonical_digest(value)
    if not _DIGEST.fullmatch(result):  # pragma: no cover - defensive
        raise AssertionError(result)
    return result


def _digest_bytes(value: bytes) -> str:
    return "sha256:" + hashlib.sha256(value).hexdigest()


def _fsync_directory(path: Path) -> None:
    """Durably flush a directory when the platform exposes directory fsync."""

    try:
        fd = os.open(str(path), os.O_RDONLY)
    except OSError:
        return
    try:
        os.fsync(fd)
    finally:
        os.close(fd)


def _read_json(path: Path) -> Any:
    try:
        with path.open(encoding="utf-8") as handle:
            return json.load(handle)
    except FileNotFoundError as exc:
        raise NotFoundError("missing file: %s" % path) from exc
    except (OSError, json.JSONDecodeError) as exc:
        raise IntegrityBlockedError("unreadable JSON: %s" % path) from exc


def _atomic_json(path: Path, value: Any, mode: int = 0o600) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary = tempfile.mkstemp(prefix=".%s." % path.name, dir=str(path.parent))
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8") as handle:
            json.dump(value, handle, ensure_ascii=False, sort_keys=True, indent=2)
            handle.write("\n")
            handle.flush()
            os.fsync(handle.fileno())
        os.chmod(temporary, mode)
        os.replace(temporary, str(path))
        _fsync_directory(path.parent)
    except Exception:
        try:
            os.unlink(temporary)
        except FileNotFoundError:
            pass
        raise


def _pretty_json_bytes(value: Any) -> bytes:
    """Encode a projection exactly as ``_atomic_json`` would write it."""

    return (
        json.dumps(value, ensure_ascii=False, sort_keys=True, indent=2)
        + "\n"
    ).encode("utf-8")


def _atomic_bytes(path: Path, value: bytes, mode: int = 0o600) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary = tempfile.mkstemp(prefix=".%s." % path.name, dir=str(path.parent))
    try:
        with os.fdopen(descriptor, "wb") as handle:
            handle.write(value)
            handle.flush()
            os.fsync(handle.fileno())
        os.chmod(temporary, mode)
        os.replace(temporary, str(path))
        _fsync_directory(path.parent)
    except Exception:
        try:
            os.unlink(temporary)
        except FileNotFoundError:
            pass
        raise


def _atomic_text(path: Path, value: str, mode: int = 0o600) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary = tempfile.mkstemp(prefix=".%s." % path.name, dir=str(path.parent))
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8") as handle:
            handle.write(value)
            if not value.endswith("\n"):
                handle.write("\n")
            handle.flush()
            os.fsync(handle.fileno())
        os.chmod(temporary, mode)
        os.replace(temporary, str(path))
        _fsync_directory(path.parent)
    except Exception:
        try:
            os.unlink(temporary)
        except FileNotFoundError:
            pass
        raise


def _safe_ref(value: Mapping[str, Any], label: str = "reference") -> Dict[str, Any]:
    if not isinstance(value, Mapping):
        raise KernelError("%s must be an object" % label)
    result = dict(value)
    digest = result.get("digest") or result.get("object_digest")
    if result.get("digest") is not None and result.get("object_digest") is not None and result.get("digest") != result.get("object_digest"):
        raise IntegrityBlockedError("%s has conflicting digest fields" % label)
    if not isinstance(digest, str) or not _DIGEST.fullmatch(digest):
        raise KernelError("%s.digest must be a sha256: digest" % label)
    result["digest"] = digest
    return result


def _copy(value: Any) -> Any:
    return copy.deepcopy(value)


class ControlKernel:
    """Operate one isolated file-backed managed Run.

    ``root`` may be a repository root (the usual form, with ``run_id``), or a
    direct Run directory when ``run_id`` is omitted.  No directory is created
    until ``entry`` or a submission/commit is requested.
    """

    _REQUIRED_PROJECTION_FILES = frozenset(
        {
            "run.json",
            "status.json",
            "ready.json",
            "artifact-bundle.json",
            "checkpoint.json",
            "run.yaml",
            "plan.yaml",
            "status.md",
            "events.jsonl",
        }
    )

    def __init__(
        self,
        root: Union[str, os.PathLike],
        run_id: Optional[str] = None,
        fault_phase: Optional[str] = None,
        fault_injection: Optional[str] = None,
    ) -> None:
        self.repository_root = Path(root).resolve()
        self.run_id = _id(run_id, "run_id") if run_id is not None else None
        self._direct_path = self.repository_root if self.run_id is None else None
        self._fault_phase = fault_phase or fault_injection
        self.last_receipt: Optional[Dict[str, Any]] = None

    @property
    def path(self) -> Path:
        # ``run_id is None`` denotes a direct Run directory.  The overload
        # ``ControlKernel(root).entry(run_id, objective)`` fills in the id
        # after construction, so keep using that direct directory instead of
        # unexpectedly switching to the repository-root layout mid-entry.
        if self._direct_path is not None:
            return self._direct_path  # type: ignore[return-value]
        return self.repository_root / ".local" / "agent" / "runs" / self.run_id / "control-kernel"

    @property
    def run_dir(self) -> Path:
        return self.path

    @property
    def head_path(self) -> Path:
        return self.path / "HEAD"

    @property
    def objects_dir(self) -> Path:
        return self.path / "objects"

    @property
    def transactions_dir(self) -> Path:
        return self.path / "transactions"

    @property
    def staging_dir(self) -> Path:
        return self.path / "staging"

    @property
    def quarantine_dir(self) -> Path:
        return self.path / "quarantine"

    @property
    def projection_dir(self) -> Path:
        return self.path / "projections"

    @property
    def inbox_dir(self) -> Path:
        return self.path / "inbox"

    @property
    def blocked_path(self) -> Path:
        return self.path / "INTEGRITY_BLOCKED.json"

    def _ensure_layout(self) -> None:
        for directory in (
            self.path,
            self.objects_dir,
            self.transactions_dir,
            self.staging_dir,
            self.quarantine_dir,
            self.projection_dir,
            self.inbox_dir,
        ):
            directory.mkdir(parents=True, exist_ok=True)

    @contextmanager
    def _lock(self, create: bool = False) -> Iterator[None]:
        if create:
            self._ensure_layout()
        elif not self.path.is_dir():
            raise NotFoundError("unknown kernel Run: %s" % (self.run_id or self.path))
        lock_path = self.path / ".run.lock"
        with lock_path.open("a+", encoding="utf-8") as handle:
            fcntl.flock(handle.fileno(), fcntl.LOCK_EX)
            try:
                yield
            finally:
                fcntl.flock(handle.fileno(), fcntl.LOCK_UN)

    @contextmanager
    def fault(self, phase: str) -> Iterator["ControlKernel"]:
        """Inject one crash at ``before_publish``, ``after_publish_before_head``,
        or ``after_head_before_projection``.
        """

        old = self._fault_phase
        self._fault_phase = phase
        try:
            yield self
        finally:
            self._fault_phase = old

    def set_fault(self, phase: Optional[str]) -> None:
        self._fault_phase = phase

    inject_fault = set_fault

    def _maybe_fault(self, phase: str) -> None:
        aliases = {
            "before_transaction_publish": "before_publish",
            "transaction_publish": "before_publish",
            "after_transaction_before_head": "after_publish_before_head",
            "after_publish": "after_publish_before_head",
            "after_head": "after_head_before_projection",
            "before_projection": "after_head_before_projection",
        }
        configured = aliases.get(self._fault_phase or "", self._fault_phase)
        if configured == phase:
            raise InjectedCrash("fault injected at %s" % phase)

    def _object_path(self, digest: str) -> Path:
        if not isinstance(digest, str) or not _DIGEST.fullmatch(digest):
            raise IntegrityBlockedError("invalid object digest: %s" % digest)
        return self.objects_dir / (digest.split(":", 1)[1] + ".json")

    def _transaction_path(self, digest: str) -> Path:
        if not isinstance(digest, str) or not _DIGEST.fullmatch(digest):
            raise IntegrityBlockedError("invalid transaction digest: %s" % digest)
        return self.transactions_dir / (digest.split(":", 1)[1] + ".json")

    def _load_object(self, digest: str) -> Dict[str, Any]:
        try:
            value = _read_json(self._object_path(digest))
        except NotFoundError as exc:
            raise IntegrityBlockedError("missing object: %s" % digest) from exc
        if not isinstance(value, dict) or set(value) != {"schema", "object_type", "payload", "digest"} or value.get("digest") != digest:
            raise IntegrityBlockedError("object digest mismatch: %s" % digest)
        if _digest(_without_digest(value)) != digest:
            raise IntegrityBlockedError("object content mismatch: %s" % digest)
        if value.get("schema") != SCHEMA_OBJECT:
            raise IntegrityBlockedError("unsupported object schema: %s" % digest)
        try:
            self._validate_object_payload(value.get("object_type"), value.get("payload"))
            self._ensure_durable_payload(value.get("payload"), "object.%s" % value.get("object_type"))
        except KernelError as exc:
            raise IntegrityBlockedError("object runtime shape is invalid: %s" % digest) from exc
        return value

    def _load_ref_object(self, ref: Mapping[str, Any], label: str = "reference") -> Dict[str, Any]:
        """Load an internal reference and enforce its declared object type."""

        self._validate_ref_runtime(ref, label)
        digest = ref.get("digest") or ref.get("object_digest")
        value = self._load_object(digest)
        declared = ref.get("object_type")
        if declared is not None and value.get("object_type") != declared:
            raise IntegrityBlockedError("%s object type does not match digest" % label)
        return value

    def read_object(self, ref_or_digest: Union[str, Mapping[str, Any]]) -> Dict[str, Any]:
        """Read and re-verify one immutable object by digest or reference."""

        if isinstance(ref_or_digest, str):
            digest = ref_or_digest
        elif isinstance(ref_or_digest, Mapping):
            digest = ref_or_digest.get("digest") or ref_or_digest.get("object_digest")
        else:
            digest = None
        if not isinstance(digest, str):
            raise KernelError("object digest is required")
        with self._lock():
            return _copy(self._load_object(digest))

    @staticmethod
    def _validate_graph_delta(value: Any, label: str = "graph_delta") -> None:
        if not isinstance(value, Mapping):
            raise IntegrityBlockedError("%s must be an object" % label)
        required = {
            "revision", "added_nodes", "removed_nodes", "added_edges", "removed_edges",
            "changed_artifacts", "changed_tasks", "changed_reviews", "changed_findings",
        }
        if set(value) != required:
            raise IntegrityBlockedError("%s is incomplete" % label)
        if not isinstance(value["revision"], int) or isinstance(value["revision"], bool) or value["revision"] < 1:
            raise IntegrityBlockedError("%s.revision is malformed" % label)
        for key in ("added_nodes", "removed_nodes", "changed_artifacts", "changed_tasks", "changed_reviews", "changed_findings"):
            if not isinstance(value[key], list) or any(not isinstance(item, str) or not item for item in value[key]) or len(set(value[key])) != len(value[key]):
                raise IntegrityBlockedError("%s.%s is malformed" % (label, key))
        for key in ("added_edges", "removed_edges"):
            if not isinstance(value[key], list):
                raise IntegrityBlockedError("%s.%s is malformed" % (label, key))
            for edge in value[key]:
                if not isinstance(edge, Mapping) or set(edge) != {"from", "to", "type"} or edge.get("type") not in EDGE_TYPES or not isinstance(edge.get("from"), str) or not isinstance(edge.get("to"), str):
                    raise IntegrityBlockedError("%s.%s contains a malformed edge" % (label, key))
                source = edge["from"]
                target = edge["to"]
                if ":" not in source or ":" not in target:
                    raise IntegrityBlockedError("%s.%s contains a non-canonical edge node" % (label, key))
                source_type = source.split(":", 1)[0]
                target_type = target.split(":", 1)[0]
                if (
                    source_type not in {"objective", "artifact", "task", "work-product", "review", "finding", "authority", "approval"}
                    or target_type not in {"objective", "artifact", "task", "work-product", "review", "finding", "authority", "approval"}
                    or not _ID.fullmatch(source.split(":", 1)[1])
                    or not _ID.fullmatch(target.split(":", 1)[1])
                ):
                    raise IntegrityBlockedError("%s.%s contains a malformed typed edge node" % (label, key))
                if (source_type, target_type) not in EDGE_ENDPOINTS.get(edge["type"], set()):
                    if source_type == "task" and target_type == "task":
                        raise IntegrityBlockedError("%s.%s contains a forbidden Task-to-Task edge" % (label, key))
                    raise IntegrityBlockedError("%s.%s violates documented edge semantics" % (label, key))

    def _validate_transaction_shape(self, value: Mapping[str, Any]) -> None:
        required = {
            "schema", "kernel_version", "run_id", "workflow_version", "graph_version", "revision",
            "state_revision", "parent", "command", "command_digest", "idempotency_digest", "state",
            "object_refs", "compiler_version", "graph_delta", "digest",
        }
        if set(value) != required:
            raise IntegrityBlockedError("transaction has unsupported or missing fields")
        if value.get("schema") != SCHEMA_TRANSACTION or value.get("kernel_version") != KERNEL_VERSION or value.get("compiler_version") != KERNEL_VERSION:
            raise IntegrityBlockedError("transaction schema/compiler is invalid")
        if value.get("graph_version") != GRAPH_VERSION or value.get("workflow_version") is None:
            raise IntegrityBlockedError("transaction version is invalid")
        for key in ("run_id", "workflow_version"):
            if not isinstance(value.get(key), str) or not value[key]:
                raise IntegrityBlockedError("transaction.%s is malformed" % key)
        for key in ("revision", "state_revision"):
            if not isinstance(value.get(key), int) or isinstance(value[key], bool) or value[key] < 1:
                raise IntegrityBlockedError("transaction.%s is malformed" % key)
        if value["revision"] != value["state_revision"]:
            raise IntegrityBlockedError("transaction revision fields disagree")
        for key in ("command_digest", "idempotency_digest", "digest"):
            if not isinstance(value.get(key), str) or not _DIGEST.fullmatch(value[key]):
                raise IntegrityBlockedError("transaction.%s is malformed" % key)
        if not isinstance(value.get("command"), Mapping):
            raise IntegrityBlockedError("transaction command is missing")
        try:
            self._validate_command_shape(value["command"], verify_physical_attestation=False)
        except KernelError as exc:
            raise IntegrityBlockedError("transaction command envelope is invalid") from exc
        try:
            # A transaction is immutable, but its command remains untrusted
            # input when an on-disk history is opened in a fresh process.
            # Re-apply the same durable-context exclusion used before
            # publication so a digest-consistent forged transaction cannot
            # smuggle private reasoning or credentials into history.
            self._ensure_durable_payload(value["command"], "transaction.command")
        except KernelError as exc:
            raise IntegrityBlockedError("transaction command contains non-durable data") from exc
        if _digest(value["command"]) != value["command_digest"]:
            raise IntegrityBlockedError("transaction command digest mismatch")
        if self._idempotency_digest(value["command"]) != value["idempotency_digest"]:
            raise IntegrityBlockedError("transaction idempotency digest mismatch")
        if not isinstance(value.get("state"), Mapping) or not isinstance(value.get("object_refs"), Mapping):
            raise IntegrityBlockedError("transaction state/object_refs are malformed")
        self._validate_graph_delta(value.get("graph_delta"))
        if value["graph_delta"]["revision"] != value["revision"]:
            raise IntegrityBlockedError("transaction graph delta revision mismatch")
        parent = value.get("parent")
        if parent is not None:
            if not isinstance(parent, Mapping) or set(parent) - {"revision", "transaction_digest", "digest"} or "revision" not in parent or "transaction_digest" not in parent:
                raise IntegrityBlockedError("transaction parent is malformed")
            if not isinstance(parent["revision"], int) or isinstance(parent["revision"], bool) or parent["revision"] != value["revision"] - 1:
                raise IntegrityBlockedError("transaction parent revision mismatch")
            if not isinstance(parent["transaction_digest"], str) or not _DIGEST.fullmatch(parent["transaction_digest"]):
                raise IntegrityBlockedError("transaction parent digest is malformed")
            if parent.get("digest") is not None and parent.get("digest") != parent["transaction_digest"]:
                raise IntegrityBlockedError("transaction parent digest fields disagree")
        elif value["revision"] != 1:
            raise IntegrityBlockedError("non-genesis transaction needs a parent")

    def _load_transaction(self, digest: str) -> Dict[str, Any]:
        try:
            value = _read_json(self._transaction_path(digest))
        except NotFoundError as exc:
            raise IntegrityBlockedError("missing transaction: %s" % digest) from exc
        if not isinstance(value, dict) or value.get("digest") != digest:
            raise IntegrityBlockedError("transaction digest mismatch: %s" % digest)
        if _digest(_without_digest(value)) != digest:
            raise IntegrityBlockedError("transaction content mismatch: %s" % digest)
        self._validate_transaction_shape(value)
        return value

    def _load_head(self) -> Dict[str, Any]:
        try:
            value = _read_json(self.head_path)
        except NotFoundError as exc:
            raise IntegrityBlockedError("missing HEAD") from exc
        if not isinstance(value, dict) or value.get("schema") != SCHEMA_HEAD:
            raise IntegrityBlockedError("invalid HEAD schema")
        required = {"schema", "run_id", "workflow_version", "graph_version", "role", "revision", "state_revision", "transaction_digest", "digest"}
        if set(value) != required:
            raise IntegrityBlockedError("HEAD has unsupported or missing fields")
        digest = value.get("digest")
        if not isinstance(digest, str) or not _DIGEST.fullmatch(digest) or _digest(_without_digest(value)) != digest:
            raise IntegrityBlockedError("HEAD digest mismatch")
        if value.get("run_id") != self.run_id:
            raise IntegrityBlockedError("HEAD Run ID mismatch")
        if not isinstance(value.get("workflow_version"), str) or not value["workflow_version"]:
            raise IntegrityBlockedError("HEAD workflow version is malformed")
        if value.get("role") != "orchestrator":
            raise IntegrityBlockedError("HEAD transition owner is not the Orchestrator")
        if value.get("graph_version") != GRAPH_VERSION:
            raise IntegrityBlockedError("HEAD graph version mismatch")
        if not isinstance(value.get("revision"), int) or isinstance(value["revision"], bool) or value["revision"] < 1:
            raise IntegrityBlockedError("invalid HEAD revision")
        if not isinstance(value.get("state_revision"), int) or isinstance(value["state_revision"], bool) or value.get("state_revision") != value.get("revision"):
            raise IntegrityBlockedError("HEAD state_revision mismatch")
        if not isinstance(value.get("transaction_digest"), str) or not _DIGEST.fullmatch(value["transaction_digest"]):
            raise IntegrityBlockedError("HEAD transaction pointer missing")
        return value

    @staticmethod
    def _graph_delta(previous: Optional[Mapping[str, Any]], current: Mapping[str, Any]) -> Dict[str, Any]:
        """Describe the exact graph/state collections changed by a transaction."""

        previous = previous or {}
        old_nodes = previous.get("nodes", {}) if isinstance(previous.get("nodes", {}), Mapping) else {}
        new_nodes = current.get("nodes", {}) if isinstance(current.get("nodes", {}), Mapping) else {}
        old_edges = previous.get("edges", []) if isinstance(previous.get("edges", []), list) else []
        new_edges = current.get("edges", []) if isinstance(current.get("edges", []), list) else []
        old_edges_set = {canonical_digest(edge) for edge in old_edges}
        new_edges_set = {canonical_digest(edge) for edge in new_edges}
        old_by_digest = {canonical_digest(edge): edge for edge in old_edges}
        new_by_digest = {canonical_digest(edge): edge for edge in new_edges}
        collections = ("artifacts", "tasks", "reviews", "findings")
        changed: Dict[str, List[str]] = {}
        for collection in collections:
            before = previous.get(collection, {}) if isinstance(previous.get(collection, {}), Mapping) else {}
            after = current.get(collection, {}) if isinstance(current.get(collection, {}), Mapping) else {}
            changed[collection] = sorted(key for key in set(before) | set(after) if before.get(key) != after.get(key))
        return {
            "revision": current.get("revision"),
            "added_nodes": sorted(set(new_nodes) - set(old_nodes)),
            "removed_nodes": sorted(set(old_nodes) - set(new_nodes)),
            "added_edges": [new_by_digest[key] for key in sorted(new_edges_set - old_edges_set)],
            "removed_edges": [old_by_digest[key] for key in sorted(old_edges_set - new_edges_set)],
            "changed_artifacts": changed["artifacts"],
            "changed_tasks": changed["tasks"],
            "changed_reviews": changed["reviews"],
            "changed_findings": changed["findings"],
        }

    def head(self) -> Optional[Dict[str, Any]]:
        if not self.head_path.exists():
            return None
        try:
            with self._lock():
                return _copy(self._load_head())
        except (IntegrityBlockedError, DAGCycleError) as exc:
            self._record_integrity_block(str(exc))
            raise IntegrityBlockedError(str(exc)) from exc

    def _write_object_to_stage(
        self, stage: Path, object_type: str, payload: Mapping[str, Any]
    ) -> Dict[str, Any]:
        self._validate_object_payload(object_type, payload)
        self._ensure_durable_payload(payload, object_type)
        body = {"schema": SCHEMA_OBJECT, "object_type": object_type, "payload": _copy(payload)}
        digest = _digest(body)
        destination = self._object_path(digest)
        if destination.exists():
            self._load_object(digest)
            return {"digest": digest, "object_type": object_type, "path": "objects/%s.json" % digest[7:]}
        staged = stage / "objects" / (digest[7:] + ".json")
        staged.parent.mkdir(parents=True, exist_ok=True)
        _atomic_json(staged, dict(body, digest=digest))
        return {"digest": digest, "object_type": object_type, "path": "objects/%s.json" % digest[7:]}

    @staticmethod
    def _validate_ref_runtime(value: Any, label: str) -> None:
        if not isinstance(value, Mapping):
            raise ObjectValidationError("%s must be an object" % label)
        allowed = {"digest", "object_digest", "object_type", "path"}
        unknown = sorted(set(value) - allowed)
        if unknown:
            raise ObjectValidationError("%s has unsupported fields: %s" % (label, ", ".join(unknown)))
        digest = value.get("digest") or value.get("object_digest")
        if not isinstance(digest, str) or not _DIGEST.fullmatch(digest):
            raise ObjectValidationError("%s.digest is malformed" % label)
        if value.get("digest") is not None and value.get("object_digest") is not None and value["digest"] != value["object_digest"]:
            raise ObjectValidationError("%s digest fields disagree" % label)
        if "object_type" in value and (not isinstance(value["object_type"], str) or value["object_type"] not in _OBJECT_TYPES):
            raise ObjectValidationError("%s.object_type is malformed" % label)
        if "path" in value:
            path = value["path"]
            if not isinstance(path, str) or not path or "\x00" in path:
                raise ObjectValidationError("%s.path is malformed" % label)
            # References are durable relative POSIX names, never filesystem
            # destinations.  Keep this check identical to scope/package
            # validation so a reference cannot smuggle a platform-specific
            # absolute or traversal path through a less strict reader.
            if "\\" in path or re.match(r"^[A-Za-z]:[\\/]", path) or Path(path).is_absolute() or any(part in ("", ".", "..") for part in path.split("/")) or "*" in path:
                raise ObjectValidationError("%s.path is malformed" % label)

    def _validate_input_ref_binding(
        self, state: Mapping[str, Any], value: Mapping[str, Any], label: str = "input reference"
    ) -> Dict[str, Any]:
        """Resolve a reference and bind every claimed identity to its object.

        A digest alone identifies bytes, not the domain object a caller says
        those bytes represent.  Claims such as ``artifact_id`` and
        ``object_type`` therefore have to agree with both the immutable object
        and the current state catalog before a readiness edge is emitted.
        """

        safe = _safe_ref(value, label)
        if value.get("external"):
            if any(key in value for key in ("artifact_id", "task_id", "object_type", "node_id")):
                raise IntegrityBlockedError("external reference cannot claim an internal object: %s" % label)
            return {}
        loaded = self._load_object(safe["digest"])
        declared_type = value.get("object_type")
        if declared_type is not None and loaded.get("object_type") != declared_type:
            raise IntegrityBlockedError("%s object type does not match digest" % label)
        artifact_id = value.get("artifact_id")
        if artifact_id is not None:
            artifact = state.get("artifacts", {}).get(artifact_id)
            if not isinstance(artifact, Mapping) or artifact.get("digest") != safe["digest"]:
                raise IntegrityBlockedError("%s artifact_id does not match immutable object" % label)
            artifact_object_type = "legacy-artifact" if artifact.get("legacy") else "artifact"
            if loaded.get("object_type") != artifact_object_type:
                raise IntegrityBlockedError("%s artifact object type is not bound" % label)
            artifact_payload = loaded.get("payload")
            expected_id = artifact_payload.get("legacy_id") if artifact_object_type == "legacy-artifact" else artifact_payload.get("artifact_id") if isinstance(artifact_payload, Mapping) else None
            if expected_id != artifact_id:
                raise IntegrityBlockedError("%s artifact claim disagrees with object content" % label)
        task_id = value.get("task_id")
        if task_id is not None:
            task = state.get("tasks", {}).get(task_id)
            if not isinstance(task, Mapping) or (task.get("package_ref") or {}).get("digest") != safe["digest"]:
                raise IntegrityBlockedError("%s task_id does not match immutable object" % label)
            if loaded.get("object_type") != "task-package" or not isinstance(loaded.get("payload"), Mapping) or loaded["payload"].get("task_id") != task_id:
                raise IntegrityBlockedError("%s task package claim disagrees with object content" % label)
        node_id = value.get("node_id")
        if node_id is not None:
            nodes = state.get("nodes", {})
            if node_id not in nodes or ":" not in node_id:
                raise IntegrityBlockedError("%s references an unknown node" % label)
            if artifact_id is not None and node_id != "artifact:" + artifact_id:
                raise IntegrityBlockedError("%s node/artifact claim disagrees" % label)
            if task_id is not None and node_id != "task:" + task_id:
                raise IntegrityBlockedError("%s node/task claim disagrees" % label)
            prefix, suffix = node_id.split(":", 1)
            if not suffix or nodes.get(node_id) != prefix:
                raise IntegrityBlockedError("%s node type is not canonical" % label)

            # A node claim is another identity claim, not merely a display
            # label.  Resolve its state record and require its object
            # reference and payload identity to agree with the same digest.
            if prefix == "artifact":
                record = state.get("artifacts", {}).get(suffix)
                expected_type = "legacy-artifact" if isinstance(record, Mapping) and record.get("legacy") else "artifact"
                payload = loaded.get("payload")
                if (
                    not isinstance(record, Mapping)
                    or record.get("digest") != safe["digest"]
                    or loaded.get("object_type") != expected_type
                    or not isinstance(payload, Mapping)
                    or payload.get("artifact_id", payload.get("legacy_id")) != suffix
                ):
                    raise IntegrityBlockedError("%s artifact node does not match immutable object" % label)
            elif prefix == "task":
                record = state.get("tasks", {}).get(suffix)
                payload = loaded.get("payload")
                if (
                    not isinstance(record, Mapping)
                    or (record.get("package_ref") or {}).get("digest") != safe["digest"]
                    or loaded.get("object_type") != "task-package"
                    or not isinstance(payload, Mapping)
                    or payload.get("task_id") != suffix
                ):
                    raise IntegrityBlockedError("%s task node does not match immutable object" % label)
            elif prefix == "review":
                record = state.get("reviews", {}).get(suffix)
                payload = loaded.get("payload")
                if (
                    not isinstance(record, Mapping)
                    or (record.get("package_ref") or {}).get("digest") != safe["digest"]
                    or loaded.get("object_type") != "review-package"
                    or not isinstance(payload, Mapping)
                    or payload.get("review_id") != suffix
                ):
                    raise IntegrityBlockedError("%s review node does not match immutable object" % label)
            elif prefix == "finding":
                record = state.get("findings", {}).get(suffix)
                payload = loaded.get("payload")
                if (
                    not isinstance(record, Mapping)
                    or (record.get("object_ref") or {}).get("digest") != safe["digest"]
                    or loaded.get("object_type") != "finding"
                    or not isinstance(payload, Mapping)
                    or payload.get("finding_id") != suffix
                ):
                    raise IntegrityBlockedError("%s Finding node does not match immutable object" % label)
            elif prefix == "work-product":
                record = state.get("tasks", {}).get(suffix)
                payload = loaded.get("payload")
                if (
                    not isinstance(record, Mapping)
                    or (record.get("result_ref") or {}).get("digest") != safe["digest"]
                    or loaded.get("object_type") != "work-product"
                    or not isinstance(payload, Mapping)
                    or payload.get("task_id") != suffix
                ):
                    raise IntegrityBlockedError("%s work-product node does not match immutable object" % label)
            elif prefix not in {"authority", "approval", "objective"}:
                raise IntegrityBlockedError("%s has an unsupported node prefix" % label)
        return loaded

    @staticmethod
    def _validate_review_package_shape(package: Any, label: str = "review package") -> None:
        if not isinstance(package, Mapping):
            raise ObjectValidationError("%s must be an object" % label)
        required = {
            "review_id", "candidate_task_id", "reviewer_assignment_id", "fresh_epoch_id",
            "review_kind", "target_finding_id", "findings", "candidate_ref", "evidence_refs", "provenance",
        }
        unknown = sorted(set(package) - required)
        if unknown:
            raise ObjectValidationError("%s has unsupported fields: %s" % (label, ", ".join(unknown)))
        missing = sorted(required - set(package))
        if missing:
            raise ObjectValidationError("%s is missing: %s" % (label, ", ".join(missing)))
        for key in ("review_id", "candidate_task_id", "reviewer_assignment_id", "fresh_epoch_id"):
            if not isinstance(package[key], str) or not _ID.fullmatch(package[key]):
                raise ObjectValidationError("%s.%s is malformed" % (label, key))
        if package["review_kind"] not in ("initial", "closure"):
            raise ObjectValidationError("%s.review_kind is invalid" % label)
        if package["target_finding_id"] is not None and (not isinstance(package["target_finding_id"], str) or not _ID.fullmatch(package["target_finding_id"])):
            raise ObjectValidationError("%s.target_finding_id is malformed" % label)
        if not isinstance(package["findings"], list):
            raise ObjectValidationError("%s.findings must be an array" % label)
        for index, finding in enumerate(package["findings"]):
            if not isinstance(finding, Mapping):
                raise ObjectValidationError("%s.findings[%s] must be an object" % (label, index))
            required_finding = {"finding_id", "fingerprint", "requirement_ref", "description", "evidence", "severity", "blocking", "state"}
            if sorted(set(finding) - required_finding) or sorted(required_finding - set(finding)):
                raise ObjectValidationError("%s.findings[%s] is incomplete" % (label, index))
            if not isinstance(finding["finding_id"], str) or not _ID.fullmatch(finding["finding_id"]):
                raise ObjectValidationError("%s.findings[%s].finding_id is malformed" % (label, index))
            if (
                not isinstance(finding["requirement_ref"], (str, Mapping, list, tuple, int, float, bool))
                and finding["requirement_ref"] is not None
            ):
                raise ObjectValidationError("%s.findings[%s].requirement_ref is malformed" % (label, index))
            if not isinstance(finding["description"], str) or not finding["description"]:
                raise ObjectValidationError("%s.findings[%s].description is invalid" % (label, index))
            if not isinstance(finding["severity"], str) or not finding["severity"]:
                raise ObjectValidationError("%s.findings[%s].severity is invalid" % (label, index))
            if not isinstance(finding["evidence"], list) or not isinstance(finding["blocking"], bool) or finding["state"] != "open":
                raise ObjectValidationError("%s.findings[%s] has invalid fields" % (label, index))
            if not isinstance(finding["fingerprint"], str) or not _DIGEST.fullmatch(finding["fingerprint"]):
                raise ObjectValidationError("%s.findings[%s].fingerprint is malformed" % (label, index))
            try:
                expected_fingerprint = _digest({
                    "requirement_ref": finding["requirement_ref"],
                    "description": finding["description"],
                    "severity": finding["severity"],
                })
            except (TypeError, ValueError) as exc:
                raise ObjectValidationError("%s.findings[%s].stable fields are not JSON-compatible" % (label, index)) from exc
            if finding["fingerprint"] != expected_fingerprint:
                raise ObjectValidationError("%s.findings[%s].fingerprint does not match stable fields" % (label, index))
        ControlKernel._validate_ref_runtime(package["candidate_ref"], "%s.candidate_ref" % label)
        if package["candidate_ref"].get("object_type") != "task-package":
            raise ObjectValidationError("%s.candidate_ref must identify a task package" % label)
        if not isinstance(package["evidence_refs"], list):
            raise ObjectValidationError("%s.evidence_refs must be an array" % label)
        for index, ref in enumerate(package["evidence_refs"]):
            ControlKernel._validate_ref_runtime(ref, "%s.evidence_refs[%s]" % (label, index))
        provenance = package["provenance"]
        if not isinstance(provenance, Mapping) or set(provenance) != {"candidate_package_digest", "evidence_digests", "review_epoch_id", "reviewer_assignment_id"}:
            raise ObjectValidationError("%s.provenance is incomplete" % label)
        if provenance.get("candidate_package_digest") != package["candidate_ref"].get("digest") and provenance.get("candidate_package_digest") != package["candidate_ref"].get("object_digest"):
            raise ObjectValidationError("%s.provenance candidate binding is invalid" % label)
        if provenance.get("review_epoch_id") != package["fresh_epoch_id"] or provenance.get("reviewer_assignment_id") != package["reviewer_assignment_id"]:
            raise ObjectValidationError("%s.provenance reviewer/epoch binding is invalid" % label)
        if not isinstance(provenance.get("evidence_digests"), list) or provenance["evidence_digests"] != [ref.get("digest") or ref.get("object_digest") for ref in package["evidence_refs"]]:
            raise ObjectValidationError("%s.provenance evidence binding is invalid" % label)

    def _validate_object_payload(self, object_type: str, payload: Any) -> None:
        if object_type not in _OBJECT_TYPES:
            raise ObjectValidationError("unsupported object type: %s" % object_type)
        if not isinstance(payload, Mapping):
            raise ObjectValidationError("%s payload must be an object" % object_type)
        if object_type == "objective":
            for key in ("path", "version", "digest"):
                if not isinstance(payload.get(key), str) or not payload[key]:
                    raise ObjectValidationError("objective.%s is required" % key)
        elif object_type == "objective-candidate":
            if set(payload) != {"path", "version", "digest", "namespace"}:
                raise ObjectValidationError("objective-candidate is incomplete")
        elif object_type == "objective-approval":
            try:
                self._validate_objective_approval_payload(payload)
            except CommandValidationError as exc:
                raise ObjectValidationError(str(exc)) from exc
        elif object_type == "approved-objective":
            if set(payload) != {"candidate_ref", "objective", "approval_ref"}:
                raise ObjectValidationError("approved-objective is incomplete")
            for key in ("candidate_ref", "approval_ref"):
                self._validate_ref_runtime(payload.get(key), "approved-objective.%s" % key)
        elif object_type == "objective-approval-event":
            required = {"event", "approval_id", "run_id", "namespace", "approval_scope", "actor_id", "candidate_ref", "approval_ref", "prior_objective", "approved_version"}
            if set(payload) != required or payload.get("event") != "objective-approved":
                raise ObjectValidationError("objective-approval-event is incomplete")
            for key in ("candidate_ref", "approval_ref"):
                self._validate_ref_runtime(payload.get(key), "objective-approval-event.%s" % key)
        elif object_type == "artifact":
            if not isinstance(payload.get("artifact_id"), str) or not _ID.fullmatch(payload["artifact_id"]):
                raise ObjectValidationError("artifact.artifact_id is malformed")
            if not isinstance(payload.get("version"), str) or not _ID.fullmatch(payload["version"]):
                raise ObjectValidationError("artifact.version is malformed")
            if not isinstance(payload.get("kind"), str) or not payload["kind"]:
                raise ObjectValidationError("artifact.kind is required")
        elif object_type == "task-package":
            try:
                self._validate_task_package_shape(payload, object_type)
            except CommandValidationError as exc:
                raise ObjectValidationError(str(exc)) from exc
        elif object_type == "work-product":
            if set(payload) != {"task_id", "result"} or not isinstance(payload.get("task_id"), str) or not _ID.fullmatch(payload["task_id"]) or not isinstance(payload.get("result"), Mapping) or not isinstance(payload["result"].get("status"), str):
                raise ObjectValidationError("work-product is incomplete")
        elif object_type == "review-package":
            self._validate_review_package_shape(payload, object_type)
        elif object_type == "finding":
            required = {"finding_id", "fingerprint", "requirement_ref", "description", "evidence", "severity", "blocking", "state", "candidate_task_id", "introduced_review_id", "owner", "original_reviewer_assignment", "admitted", "resolution_ref", "closed_by"}
            if sorted(set(payload) - required) or sorted(required - set(payload)):
                raise ObjectValidationError("finding is incomplete")
            if not isinstance(payload["finding_id"], str) or not _ID.fullmatch(payload["finding_id"]) or not isinstance(payload["fingerprint"], str) or not _DIGEST.fullmatch(payload["fingerprint"]):
                raise ObjectValidationError("finding identity is malformed")
            if (
                not isinstance(payload["requirement_ref"], (str, Mapping, list, tuple, int, float, bool))
                and payload["requirement_ref"] is not None
            ):
                raise ObjectValidationError("finding.requirement_ref is malformed")
            if payload["state"] not in FINDING_STATES or not isinstance(payload["description"], str) or not payload["description"] or not isinstance(payload["evidence"], list) or not isinstance(payload["severity"], str) or not payload["severity"] or not isinstance(payload["blocking"], bool):
                raise ObjectValidationError("finding state or evidence is malformed")
            try:
                expected_fingerprint = _digest({
                    "requirement_ref": payload["requirement_ref"],
                    "description": payload["description"],
                    "severity": payload["severity"],
                })
            except (TypeError, ValueError) as exc:
                raise ObjectValidationError("finding stable fields are not JSON-compatible") from exc
            if payload["fingerprint"] != expected_fingerprint:
                raise ObjectValidationError("finding fingerprint does not match stable fields")
            if not isinstance(payload["candidate_task_id"], str) or not isinstance(payload["introduced_review_id"], str) or not isinstance(payload["owner"], str) or not isinstance(payload["original_reviewer_assignment"], str):
                raise ObjectValidationError("finding provenance is malformed")
            if payload["resolution_ref"] is not None:
                self._validate_ref_runtime(payload["resolution_ref"], "finding.resolution_ref")
            if payload["closed_by"] is not None and (not isinstance(payload["closed_by"], str) or not _ID.fullmatch(payload["closed_by"])):
                raise ObjectValidationError("finding.closed_by is malformed")
            if not isinstance(payload["admitted"], bool):
                raise ObjectValidationError("finding.admitted is malformed")
        elif object_type in {"evaluation-package", "finding-validation"}:
            required = {"review_id", "review_package_ref", "validator_assignment_id", "fresh_epoch_id", "outcomes"}
            if object_type == "finding-validation":
                required.add("admissibility_verdict")
            if set(payload) != required or not isinstance(payload.get("outcomes"), list) or not payload["outcomes"]:
                raise ObjectValidationError("%s is incomplete" % object_type)
            self._validate_ref_runtime(payload.get("review_package_ref"), "%s.review_package_ref" % object_type)
            if any(not isinstance(payload.get(key), str) or not _ID.fullmatch(payload[key]) for key in ("review_id", "validator_assignment_id", "fresh_epoch_id")):
                raise ObjectValidationError("%s provenance is malformed" % object_type)
            if object_type == "finding-validation" and payload.get("admissibility_verdict") != "required-only":
                raise ObjectValidationError("finding-validation verdict is malformed")
        elif object_type == "budget-terminal":
            if set(payload) != {"reason", "unresolved_finding_ids", "success_refs", "budget", "terminal_at_revision"} or payload.get("reason") not in {"time_exhausted", "rounds_exhausted", "attempts_exhausted", "needs_user"} or not isinstance(payload.get("unresolved_finding_ids"), list) or not isinstance(payload.get("success_refs"), list) or not isinstance(payload.get("budget"), Mapping) or not isinstance(payload.get("terminal_at_revision"), int):
                raise ObjectValidationError("budget-terminal is incomplete")
        elif object_type == "finding-resolution":
            if set(payload) != {"finding_id", "evidence"} or not isinstance(payload.get("finding_id"), str) or not _ID.fullmatch(payload["finding_id"]) or not isinstance(payload.get("evidence"), list):
                raise ObjectValidationError("finding-resolution is incomplete")
        elif object_type == "finding-verdict":
            required = {
                "finding_id", "review_id", "reviewer_assignment_id", "fresh_epoch_id",
                "resolution_ref", "evidence", "evidence_refs", "closure_package_ref", "verdict",
            }
            if sorted(set(payload) - required) or sorted(required - set(payload)) or payload.get("verdict") != "pass" or not isinstance(payload.get("evidence"), list) or not isinstance(payload.get("evidence_refs"), list) or any(not isinstance(payload.get(key), str) or not _ID.fullmatch(payload[key]) for key in ("finding_id", "review_id", "reviewer_assignment_id", "fresh_epoch_id")):
                raise ObjectValidationError("finding-verdict is incomplete")
            for key in ("resolution_ref", "closure_package_ref"):
                try:
                    self._validate_ref_runtime(payload.get(key), "finding-verdict.%s" % key)
                except KernelError as exc:
                    raise ObjectValidationError("finding-verdict.%s is malformed" % key) from exc
            for index, ref in enumerate(payload["evidence_refs"]):
                try:
                    self._validate_ref_runtime(ref, "finding-verdict.evidence_refs[%s]" % index)
                except KernelError as exc:
                    raise ObjectValidationError("finding-verdict evidence reference is malformed") from exc
            if payload["evidence_refs"] != payload["evidence"]:
                raise ObjectValidationError("finding-verdict evidence fields disagree")
        elif object_type == "artifact-bundle":
            required = {
                "schema", "bundle_id", "version", "run_id", "workflow_version",
                "group_id", "epoch_id", "closure_revision", "state_ref",
                "context_epoch", "canonical_artifacts", "acceptance_evidence",
                "approved_decisions", "unresolved_items", "invalidated_artifacts",
                "next_inputs", "context_budget", "digest",
            }
            if sorted(set(payload) - required) or sorted(required - set(payload)) or payload.get("schema") != "artifact-bundle/v1" or any(not isinstance(payload.get(key), str) or not _ID.fullmatch(payload[key]) for key in ("bundle_id", "version", "run_id", "workflow_version", "group_id")) or not isinstance(payload.get("canonical_artifacts"), list) or not isinstance(payload.get("context_budget"), Mapping) or not isinstance(payload.get("digest"), str) or not re.fullmatch(r"[0-9a-f]{64}", payload["digest"]):
                raise ObjectValidationError("artifact-bundle is incomplete")
            if not isinstance(payload.get("epoch_id"), str) or not _ID.fullmatch(payload["epoch_id"]):
                raise ObjectValidationError("artifact-bundle epoch_id is malformed")
            if not isinstance(payload.get("closure_revision"), int) or isinstance(payload.get("closure_revision"), bool) or payload["closure_revision"] < 1:
                raise ObjectValidationError("artifact-bundle closure_revision is malformed")
            state_ref = payload.get("state_ref")
            if not isinstance(state_ref, Mapping) or set(state_ref) != {"run_id", "revision", "state_revision"} or state_ref.get("run_id") != payload.get("run_id") or state_ref.get("revision") != payload.get("closure_revision") or state_ref.get("state_revision") != payload.get("closure_revision"):
                raise ObjectValidationError("artifact-bundle state_ref is not bound to its closure")
            if payload["digest"] != canonical_digest({key: _copy(value) for key, value in payload.items() if key != "digest"})[7:]:
                raise ObjectValidationError("artifact-bundle digest does not match canonical content")
            try:
                self._validate_budget(payload["context_budget"])
            except KernelError as exc:
                raise ObjectValidationError("artifact-bundle context_budget is invalid") from exc
            context_epoch = payload["context_epoch"]
            if not isinstance(context_epoch, Mapping) or set(context_epoch) - {"id", "status", "group_id", "boundary_reason", "clear_before_next", "closed_at_revision", "bundle_ref", "checkpoint_ref"} or not isinstance(context_epoch.get("id"), str) or not _ID.fullmatch(context_epoch["id"]) or context_epoch.get("status") != "closed" or context_epoch.get("group_id") != payload["group_id"] or not isinstance(context_epoch.get("boundary_reason"), str) or not context_epoch.get("boundary_reason") or context_epoch.get("clear_before_next") is not True or not isinstance(context_epoch.get("closed_at_revision"), int) or isinstance(context_epoch.get("closed_at_revision"), bool) or context_epoch["closed_at_revision"] != payload["closure_revision"]:
                raise ObjectValidationError("artifact-bundle context_epoch is not a closed clear boundary")
            for item in payload["canonical_artifacts"]:
                if not isinstance(item, Mapping) or set(item) != {"path", "version", "digest"} or not isinstance(item["path"], str) or not item["path"] or not isinstance(item["version"], str) or not item["version"] or not isinstance(item["digest"], str) or not re.fullmatch(r"[0-9a-f]{64}", item["digest"]):
                    raise ObjectValidationError("artifact-bundle canonical_artifacts is malformed")
                try:
                    self._scope_paths(item["path"], "artifact-bundle.canonical_artifacts.path")
                except KernelError as exc:
                    raise ObjectValidationError("artifact-bundle canonical_artifacts path is unsafe") from exc
        elif object_type == "checkpoint":
            required = {
                "schema", "checkpoint_id", "run_id", "workflow_version", "group_id",
                "epoch_id", "state_revision", "closure_revision", "state_ref",
                "bundle_ref", "clear_before_start",
            }
            if sorted(set(payload) - required) or sorted(required - set(payload)) or payload.get("schema") != "checkpoint/v1" or not isinstance(payload.get("checkpoint_id"), str) or not _ID.fullmatch(payload["checkpoint_id"]) or not isinstance(payload.get("run_id"), str) or not _ID.fullmatch(payload["run_id"]) or not isinstance(payload.get("workflow_version"), str) or not payload["workflow_version"] or not isinstance(payload.get("group_id"), str) or not _ID.fullmatch(payload["group_id"]) or not isinstance(payload.get("epoch_id"), str) or not _ID.fullmatch(payload["epoch_id"]) or not isinstance(payload.get("state_revision"), int) or isinstance(payload.get("state_revision"), bool) or payload["state_revision"] < 1 or not isinstance(payload.get("closure_revision"), int) or isinstance(payload.get("closure_revision"), bool) or payload["closure_revision"] != payload["state_revision"] or not isinstance(payload.get("clear_before_start"), bool):
                raise ObjectValidationError("checkpoint is incomplete")
            state_ref = payload.get("state_ref")
            if not isinstance(state_ref, Mapping) or set(state_ref) != {"run_id", "revision", "state_revision"} or state_ref.get("run_id") != payload.get("run_id") or state_ref.get("revision") != payload.get("closure_revision") or state_ref.get("state_revision") != payload.get("closure_revision"):
                raise ObjectValidationError("checkpoint state_ref is not bound to its closure")
            self._validate_ref_runtime(payload["bundle_ref"], "checkpoint.bundle_ref")
            if payload["bundle_ref"].get("object_type") not in (None, "artifact-bundle"):
                raise ObjectValidationError("checkpoint.bundle_ref must identify an artifact bundle")
        elif object_type in {"legacy-run", "legacy-bundle", "legacy-worker-report"}:
            if set(payload) != {"source_digest", "value"} or not isinstance(payload.get("source_digest"), str) or not _DIGEST.fullmatch(payload["source_digest"]) or not isinstance(payload.get("value"), Mapping):
                raise ObjectValidationError("legacy object is incomplete")
        elif object_type == "legacy-artifact":
            if set(payload) != {"legacy_id", "value"} or not isinstance(payload.get("legacy_id"), str) or not _ID.fullmatch(payload["legacy_id"]) or not isinstance(payload.get("value"), Mapping):
                raise ObjectValidationError("legacy artifact object is incomplete")

    def _validate_closed_boundary_objects(
        self,
        state: Mapping[str, Any],
        boundary_name: str,
        boundary: Mapping[str, Any],
    ) -> None:
        """Reverse-bind a closed Bundle/Checkpoint to its canonical boundary.

        Bundle and Checkpoint files are projections/evidence, not a second
        state authority.  Their identity tuple must nevertheless point back
        to the exact Run, Group, Epoch, closure revision, and state snapshot
        represented by the boundary that references them.
        """

        closed_at = boundary.get("closed_at_revision")
        expected_state_ref = {
            "run_id": state.get("run_id"),
            "revision": closed_at,
            "state_revision": closed_at,
        }
        bundle_ref = boundary.get("bundle_ref")
        checkpoint_ref = boundary.get("checkpoint_ref")
        bundle_object = self._load_ref_object(bundle_ref, "%s.bundle_ref" % boundary_name)
        checkpoint_object = self._load_ref_object(checkpoint_ref, "%s.checkpoint_ref" % boundary_name)
        if bundle_object.get("object_type") != "artifact-bundle":
            raise IntegrityBlockedError("closed %s bundle reference has the wrong object type" % boundary_name)
        if checkpoint_object.get("object_type") != "checkpoint":
            raise IntegrityBlockedError("closed %s checkpoint reference has the wrong object type" % boundary_name)
        bundle = bundle_object.get("payload")
        checkpoint = checkpoint_object.get("payload")
        for label, payload in (("Bundle", bundle), ("Checkpoint", checkpoint)):
            if not isinstance(payload, Mapping):
                raise IntegrityBlockedError("closed %s payload is missing" % label)
            if (
                payload.get("run_id") != state.get("run_id")
                or payload.get("group_id") != state.get("group", {}).get("id")
                or payload.get("epoch_id") != state.get("epoch", {}).get("id")
                or payload.get("closure_revision") != closed_at
                or payload.get("state_ref") != expected_state_ref
            ):
                raise IntegrityBlockedError("closed %s is not reverse-bound to the same Run/Group/Epoch/state" % label)
        context_epoch = bundle.get("context_epoch")
        if (
            not isinstance(context_epoch, Mapping)
            or context_epoch.get("group_id") != state.get("group", {}).get("id")
            or context_epoch.get("status") != "closed"
            or context_epoch.get("closed_at_revision") != closed_at
        ):
            raise IntegrityBlockedError("closed Bundle context boundary is not reverse-bound")
        if checkpoint.get("state_revision") != closed_at or checkpoint.get("bundle_ref") != bundle_ref:
            raise IntegrityBlockedError("closed Checkpoint does not reverse-bind its Bundle and closure revision")

    def _validate_accepted_section_receipt_provenance(
        self,
        state: Mapping[str, Any],
        receipt_ref: Mapping[str, Any],
        transactions: Mapping[int, Mapping[str, Any]],
        *,
        label: str,
        section_id: str,
        group_id: str,
        bundle_ref: Mapping[str, Any],
        checkpoint_ref: Mapping[str, Any],
    ) -> Mapping[str, Any]:
        """Bind an accepted receipt to the HEAD-anchored close transaction.

        Catalog membership proves only that an immutable object exists.  The
        receipt must additionally be the object minted by the reachable
        ``close_group`` transaction for the exact accepted Run/Section/Group
        boundary represented by the canonical state.
        """

        def fail() -> None:
            raise IntegrityBlockedError("%s payload is not bound" % label)

        try:
            self._validate_ref_runtime(receipt_ref, label)
            receipt = self._load_ref_object(receipt_ref, label)
        except KernelError:
            fail()
        wrapper = receipt.get("payload")
        payload = wrapper.get("payload") if isinstance(wrapper, Mapping) else None
        if (
            receipt.get("object_type") != "artifact"
            or not isinstance(wrapper, Mapping)
            or set(wrapper) != {"artifact_id", "version", "kind", "payload"}
            or wrapper.get("artifact_id") != "section-acceptance-" + section_id
            or wrapper.get("version") != "v1"
            or wrapper.get("kind") != "section-transition-receipt"
            or not isinstance(payload, Mapping)
            or set(payload) != {
                "schema", "section_id", "run_id", "accepted_group_id",
                "accepted_at_revision", "acceptance_evidence", "bundle_ref",
                "checkpoint_ref",
            }
            or payload.get("schema") != "section-acceptance-receipt/v1"
            or payload.get("run_id") != self.run_id
            or payload.get("section_id") != section_id
            or payload.get("accepted_group_id") != group_id
            or payload.get("bundle_ref") != bundle_ref
            or payload.get("checkpoint_ref") != checkpoint_ref
        ):
            fail()
        accepted_at = payload.get("accepted_at_revision")
        evidence = payload.get("acceptance_evidence")
        if (
            not isinstance(accepted_at, int)
            or isinstance(accepted_at, bool)
            or accepted_at < 1
            or not isinstance(evidence, list)
            or not evidence
            or any(not isinstance(digest, str) or not _DIGEST.fullmatch(digest) for digest in evidence)
            or len(set(evidence)) != len(evidence)
        ):
            fail()

        acceptance_transaction = transactions.get(accepted_at)
        if not isinstance(acceptance_transaction, Mapping):
            fail()
        accepted_state = acceptance_transaction.get("state")
        accepted_section = (
            accepted_state.get("metadata", {}).get("section_control")
            if isinstance(accepted_state, Mapping)
            else None
        )
        accepted_group = accepted_state.get("group") if isinstance(accepted_state, Mapping) else None
        command = acceptance_transaction.get("command")
        command_payload = command.get("payload") if isinstance(command, Mapping) else None
        if (
            not isinstance(accepted_section, Mapping)
            or accepted_section.get("section_id") != section_id
            or accepted_section.get("transition")
            != {"intent": "source-transition-fixture-passed", "state": "accepted"}
            or accepted_section.get("accepted_section_receipt") != receipt_ref
            or not isinstance(accepted_group, Mapping)
            or accepted_group.get("id") != group_id
            or accepted_group.get("status") != "closed"
            or accepted_group.get("bundle_ref") != bundle_ref
            or accepted_group.get("checkpoint_ref") != checkpoint_ref
            or accepted_group.get("closed_at_revision") != accepted_at
            or not isinstance(command, Mapping)
            or command.get("command_type") != "close_group"
            or not isinstance(command_payload, Mapping)
            or command_payload.get("acceptance_evidence") != evidence
        ):
            fail()

        parent = acceptance_transaction.get("parent")
        parent_revision = parent.get("revision") if isinstance(parent, Mapping) else None
        parent_transaction = transactions.get(parent_revision) if isinstance(parent_revision, int) else None
        parent_state = parent_transaction.get("state") if isinstance(parent_transaction, Mapping) else None
        if (
            not isinstance(parent_state, Mapping)
            or receipt_ref.get("digest") in parent_state.get("object_refs", {})
            or bundle_ref.get("digest") in parent_state.get("object_refs", {})
            or checkpoint_ref.get("digest") in parent_state.get("object_refs", {})
        ):
            fail()

        try:
            bundle_object = self._load_ref_object(bundle_ref, "%s bundle" % label)
            checkpoint_object = self._load_ref_object(checkpoint_ref, "%s checkpoint" % label)
        except KernelError:
            fail()
        bundle = bundle_object.get("payload")
        checkpoint = checkpoint_object.get("payload")
        expected_state_ref = {
            "run_id": self.run_id,
            "revision": accepted_at,
            "state_revision": accepted_at,
        }
        if (
            bundle_object.get("object_type") != "artifact-bundle"
            or checkpoint_object.get("object_type") != "checkpoint"
            or not isinstance(bundle, Mapping)
            or bundle.get("run_id") != self.run_id
            or bundle.get("group_id") != group_id
            or bundle.get("closure_revision") != accepted_at
            or bundle.get("state_ref") != expected_state_ref
            or bundle.get("acceptance_evidence") != evidence
            or not isinstance(checkpoint, Mapping)
            or checkpoint.get("run_id") != self.run_id
            or checkpoint.get("group_id") != group_id
            or checkpoint.get("closure_revision") != accepted_at
            or checkpoint.get("state_revision") != accepted_at
            or checkpoint.get("state_ref") != expected_state_ref
            or checkpoint.get("bundle_ref") != bundle_ref
        ):
            fail()

        for evidence_digest in evidence:
            evidence_ref = parent_state.get("object_refs", {}).get(evidence_digest)
            if not isinstance(evidence_ref, Mapping) or evidence_ref.get("object_type") != "artifact":
                fail()
            try:
                evidence_object = self._load_ref_object(
                    evidence_ref, "%s acceptance evidence" % label
                )
            except KernelError:
                fail()
            evidence_wrapper = evidence_object.get("payload")
            evidence_payload = (
                evidence_wrapper.get("payload")
                if isinstance(evidence_wrapper, Mapping)
                else None
            )
            artifact_id = evidence_wrapper.get("artifact_id") if isinstance(evidence_wrapper, Mapping) else None
            artifact_record = parent_state.get("artifacts", {}).get(artifact_id)
            if (
                evidence_object.get("object_type") != "artifact"
                or not isinstance(evidence_wrapper, Mapping)
                or evidence_wrapper.get("kind") != "section-accepted-result"
                or not isinstance(evidence_payload, Mapping)
                or evidence_payload.get("schema") != "section-accepted-result/v1"
                or evidence_payload.get("run_id") != self.run_id
                or evidence_payload.get("section_id") != section_id
                or evidence_payload.get("group_id") != group_id
                or evidence_payload.get("result") != "passed"
                or not isinstance(artifact_record, Mapping)
                or artifact_record.get("digest") != evidence_digest
            ):
                fail()
        return payload

    def _validate_parent_accepted_section_receipt_provenance(
        self,
        state: Mapping[str, Any],
        transactions: Mapping[int, Mapping[str, Any]],
    ) -> None:
        """Bind the retained parent receipt to the opening transaction's parent.

        ``parent_accepted_section_receipt`` survives the later close of the
        current Group.  Its authority therefore comes from the HEAD-parented
        ``open_section``/``open_group`` transition that introduced the current
        Group, never from whichever catalogued receipt a mutable state field
        happens to nominate.
        """

        label = "parent accepted section receipt"

        def fail() -> None:
            raise IntegrityBlockedError("%s payload is not bound" % label)

        section = state.get("metadata", {}).get("section_control")
        current_group = state.get("group")
        if not isinstance(section, Mapping) or not isinstance(current_group, Mapping):
            fail()
        parent_ref = section.get("parent_accepted_section_receipt")
        if not isinstance(parent_ref, Mapping):
            fail()

        opening_transaction: Optional[Mapping[str, Any]] = None
        for revision in sorted(transactions, reverse=True):
            candidate = transactions[revision]
            candidate_command = candidate.get("command")
            candidate_state = candidate.get("state")
            candidate_section = (
                candidate_state.get("metadata", {}).get("section_control")
                if isinstance(candidate_state, Mapping)
                else None
            )
            candidate_group = candidate_state.get("group") if isinstance(candidate_state, Mapping) else None
            if (
                isinstance(candidate_command, Mapping)
                and candidate_command.get("command_type") in {"open_section", "open_group"}
                and isinstance(candidate_section, Mapping)
                and candidate_section.get("section_id") == section.get("section_id")
                and isinstance(candidate_group, Mapping)
                and candidate_group.get("id") == current_group.get("id")
            ):
                opening_transaction = candidate
                break
        if not isinstance(opening_transaction, Mapping):
            fail()

        opening_parent = opening_transaction.get("parent")
        parent_revision = opening_parent.get("revision") if isinstance(opening_parent, Mapping) else None
        predecessor_transaction = transactions.get(parent_revision) if isinstance(parent_revision, int) else None
        predecessor_state = (
            predecessor_transaction.get("state")
            if isinstance(predecessor_transaction, Mapping)
            else None
        )
        predecessor_section = (
            predecessor_state.get("metadata", {}).get("section_control")
            if isinstance(predecessor_state, Mapping)
            else None
        )
        predecessor_group = (
            predecessor_state.get("group") if isinstance(predecessor_state, Mapping) else None
        )
        opening_state = opening_transaction.get("state")
        opening_section = (
            opening_state.get("metadata", {}).get("section_control")
            if isinstance(opening_state, Mapping)
            else None
        )
        expected_ref = (
            predecessor_section.get("accepted_section_receipt")
            if isinstance(predecessor_section, Mapping)
            else None
        )
        if (
            not isinstance(predecessor_section, Mapping)
            or predecessor_section.get("transition")
            != {"intent": "source-transition-fixture-passed", "state": "accepted"}
            or not isinstance(predecessor_group, Mapping)
            or predecessor_group.get("status") != "closed"
            or not isinstance(expected_ref, Mapping)
            or parent_ref != expected_ref
            or not isinstance(opening_section, Mapping)
            or opening_section.get("parent_accepted_section_receipt") != expected_ref
        ):
            fail()

        opening_command = opening_transaction.get("command")
        if (
            isinstance(opening_command, Mapping)
            and opening_command.get("command_type") == "open_section"
            and opening_section.get("section_id") != predecessor_section.get("section_id")
        ):
            history = opening_section.get("section_history")
            current_history = section.get("section_history")
            if (
                not isinstance(history, list)
                or not history
                or current_history != history
                or history[-1]
                != {
                    "position": len(history) - 1,
                    "section_id": predecessor_section.get("section_id"),
                    "accepted_receipt_ref": expected_ref,
                    "bundle_ref": predecessor_group.get("bundle_ref"),
                    "checkpoint_ref": predecessor_group.get("checkpoint_ref"),
                }
            ):
                fail()

        self._validate_accepted_section_receipt_provenance(
            state,
            expected_ref,
            transactions,
            label=label,
            section_id=predecessor_section["section_id"],
            group_id=predecessor_group["id"],
            bundle_ref=predecessor_group["bundle_ref"],
            checkpoint_ref=predecessor_group["checkpoint_ref"],
        )

    def _publish_staged_file(self, staged: Path, destination: Path) -> None:
        destination.parent.mkdir(parents=True, exist_ok=True)
        if destination.exists():
            # Never replace an existing content-addressed object.  A mismatch
            # is evidence of corruption, not a reason to overwrite history.
            if staged.exists():
                left = _read_json(staged)
                right = _read_json(destination)
                if left != right:
                    raise IntegrityBlockedError("immutable object collision: %s" % destination)
                staged.unlink()
            return
        os.replace(str(staged), str(destination))

    def _publish_projection_file(self, staged: Path, destination: Path) -> None:
        """Replace one disposable projection file after it was fsynced."""

        destination.parent.mkdir(parents=True, exist_ok=True)
        os.replace(str(staged), str(destination))
        _fsync_directory(destination.parent)

    def _record_integrity_block(self, reason: str) -> None:
        try:
            self._ensure_layout()
            if not self.blocked_path.exists():
                safe_reason = str(reason)
                try:
                    self._ensure_durable_payload({"reason": safe_reason}, "integrity block")
                except KernelError:
                    safe_reason = "integrity validation failed; non-durable diagnostic omitted"
                _atomic_json(
                    self.blocked_path,
                    {"schema": "integrity-block/v1", "run_id": self.run_id, "reason": safe_reason, "recorded_at": _now()},
                )
        except Exception:
            # The original integrity error is the useful failure.  A read
            # path must not hide it behind an inability to write a marker.
            pass

    def _validate_edges(
        self,
        edges: Sequence[Mapping[str, Any]],
        nodes: Optional[Mapping[str, Any]] = None,
        *,
        exact: bool = True,
    ) -> None:
        """Validate the execution DAG and its endpoint direction.

        The exact endpoint table is the only accepted contract.  ``exact`` is
        retained as a keyword for source compatibility with earlier internal
        callers, but a false value cannot select a broader or legacy matrix.
        """

        endpoint_table = EDGE_ENDPOINTS
        adjacency: Dict[str, List[str]] = {}
        seen_edges: set[tuple[str, str, str]] = set()
        for edge in edges:
            if not isinstance(edge, Mapping) or set(edge) != {"from", "to", "type"}:
                raise IntegrityBlockedError("malformed DAG edge")
            kind = edge.get("type")
            source = edge.get("from")
            target = edge.get("to")
            if kind not in EDGE_TYPES or not isinstance(source, str) or not isinstance(target, str):
                raise IntegrityBlockedError("malformed DAG edge")
            for node in (source, target):
                if ":" not in node:
                    raise IntegrityBlockedError("DAG edge node must use a typed node id")
                node_type, node_id = node.split(":", 1)
                if node_type not in {"objective", "artifact", "task", "work-product", "review", "finding", "authority", "approval"} or not _ID.fullmatch(node_id):
                    raise IntegrityBlockedError("DAG edge node id is malformed")
            edge_key = (source, target, kind)
            if edge_key in seen_edges:
                raise IntegrityBlockedError("duplicate DAG edge")
            seen_edges.add(edge_key)
            if nodes is not None:
                if source not in nodes or target not in nodes:
                    raise IntegrityBlockedError("DAG edge references an unknown node")
                for node in (source, target):
                    if nodes.get(node) != node.split(":", 1)[0]:
                        raise IntegrityBlockedError("DAG edge node type is not canonical")
            source_prefix = source.split(":", 1)[0] if ":" in source else ""
            target_prefix = target.split(":", 1)[0] if ":" in target else ""
            if (source_prefix, target_prefix) not in endpoint_table.get(kind, set()):
                if source_prefix == "task" and target_prefix == "task":
                    raise DAGCycleError("Task-to-Task execution edges are forbidden; require an Artifact/work-product")
                raise IntegrityBlockedError("DAG edge violates documented %s semantics" % kind)
            adjacency.setdefault(source, []).append(target)
            adjacency.setdefault(target, [])
        visiting: set[str] = set()
        visited: set[str] = set()

        def visit(node: str) -> None:
            if node in visiting:
                raise DAGCycleError("DAG cycle at %s" % node)
            if node in visited:
                return
            visiting.add(node)
            for child in adjacency.get(node, []):
                visit(child)
            visiting.remove(node)
            visited.add(node)

        for node in adjacency:
            visit(node)

    def _validate_objective_associations(self, state: Mapping[str, Any], load_objects: bool) -> None:
        history = state.get("objective_history")
        associations = state.get("objective_approvals")
        events = state.get("objective_events")
        if not isinstance(history, list) or not isinstance(associations, Mapping) or not isinstance(events, list):
            raise IntegrityBlockedError("objective approval state collections are malformed")
        if len(history) != len(associations) or len(events) != len(associations):
            raise IntegrityBlockedError("objective approval association cardinality is invalid")
        seen_ref_digests: set[str] = set()
        seen_versions: Dict[str, str] = {}
        for prior in history:
            if not isinstance(prior, Mapping) or not isinstance(prior.get("version"), str) or not isinstance(prior.get("digest"), str):
                raise IntegrityBlockedError("objective history entry is malformed")
            known = seen_versions.setdefault(prior["version"], prior["digest"])
            if known != prior["digest"]:
                raise IntegrityBlockedError("objective version is bound to multiple digests")
        expected_types = {
            "approval_ref": "objective-approval", "candidate_ref": "objective-candidate",
            "approved_ref": "approved-objective", "event_ref": "objective-approval-event",
        }
        seen_approval_ids: set[str] = set()
        for index, event in enumerate(events):
            approval_id = event.get("approval_id") if isinstance(event, Mapping) else None
            if not isinstance(approval_id, str) or not _ID.fullmatch(approval_id):
                raise IntegrityBlockedError("objective approval association is malformed")
            if approval_id in seen_approval_ids:
                raise IntegrityBlockedError("objective approval event association is invalid")
            seen_approval_ids.add(approval_id)
            association = associations.get(approval_id)
            if not isinstance(association, Mapping) or set(association) != set(expected_types):
                raise IntegrityBlockedError("objective approval association is malformed")
            for field, object_type in expected_types.items():
                try:
                    self._validate_ref_runtime(association.get(field), "objective approval %s" % field)
                except KernelError as exc:
                    raise IntegrityBlockedError("objective approval association reference is malformed") from exc
                ref = association[field]
                if ref.get("object_type") != object_type or ref.get("digest") not in state.get("object_refs", {}):
                    raise IntegrityBlockedError("objective approval association reference is cross-wired")
                if ref["digest"] in seen_ref_digests:
                    raise IntegrityBlockedError("objective approval association reuses an immutable node")
                seen_ref_digests.add(ref["digest"])
            expected_event_fields = {
                "event", "approval_id", "run_id", "namespace", "approval_scope", "actor_id",
                "candidate_ref", "approval_ref", "prior_objective", "approved_version", "object_ref",
            }
            if (not isinstance(event, Mapping) or set(event) != expected_event_fields
                    or event.get("event") != "objective-approved" or event.get("approval_id") != approval_id
                    or event.get("candidate_ref") != association["candidate_ref"]
                    or event.get("approval_ref") != association["approval_ref"]
                    or event.get("object_ref") != association["event_ref"]
                    or event.get("prior_objective") != history[index]):
                raise IntegrityBlockedError("objective approval event association is invalid")
            if state.get("nodes", {}).get("approval:" + approval_id) != "approval":
                raise IntegrityBlockedError("objective approval node is missing")
            if load_objects:
                loaded = {field: self._load_ref_object(association[field], field) for field in expected_types}
                approval = loaded["approval_ref"]["payload"]
                candidate = loaded["candidate_ref"]["payload"]
                approved = loaded["approved_ref"]["payload"]
                durable_event = loaded["event_ref"]["payload"]
                if (approval.get("approval_id") != approval_id
                        or approval.get("candidate_digest") != candidate.get("digest")
                        or approval.get("candidate_version") != candidate.get("version")
                        or approval.get("prior_objective_digest") != history[index].get("digest")
                        or approval.get("prior_objective_version") != history[index].get("version")
                        or approved != {"candidate_ref": association["candidate_ref"], "objective": candidate, "approval_ref": association["approval_ref"]}
                        or durable_event != {key: value for key, value in event.items() if key != "object_ref"}):
                    raise IntegrityBlockedError("objective approval immutable objects are not joined")
                known = seen_versions.setdefault(candidate["version"], candidate["digest"])
                if known != candidate["digest"]:
                    raise IntegrityBlockedError("objective version is bound to multiple digests")
                if state.get("nodes", {}).get("objective:" + candidate["version"]) != "objective":
                    raise IntegrityBlockedError("objective version node is missing")
                if index == len(events) - 1:
                    current = state.get("objective_ref")
                    if (not isinstance(current, Mapping) or current.get("candidate_ref") != association["candidate_ref"]
                            or current.get("approval_ref") != association["approval_ref"]
                            or current.get("event_ref") != association["event_ref"]
                            or current.get("object_digest") != association["approved_ref"]["digest"]
                            or any(current.get(key) != candidate.get(key) for key in ("path", "version", "digest", "namespace"))):
                        raise IntegrityBlockedError("current objective pointer is not joined to approval")

    def _validate_state(self, state: Mapping[str, Any], load_objects: bool = True) -> None:
        if not isinstance(state, Mapping) or state.get("schema") != SCHEMA_STATE:
            raise IntegrityBlockedError("unsupported state schema")
        allowed_state_fields = {
            "schema", "run_id", "workflow_version", "graph_version", "kernel_version",
            "revision", "state_revision", "graph_revision", "status", "objective_ref", "entry_object_ref",
            "authority", "group", "epoch", "epoch_contexts", "context_budget", "object_refs",
            "artifacts", "tasks", "reviews", "findings", "verdicts", "finding_validations", "edges", "leases",
            "idempotency", "nodes", "metadata", "migration", "updated_at", "review_budget", "budget_terminal", "terminal_history",
            "objective_history", "objective_approvals", "objective_events",
        }
        if set(state) - allowed_state_fields:
            raise IntegrityBlockedError("state has unsupported fields")
        if state.get("run_id") != self.run_id:
            raise IntegrityBlockedError("state Run ID mismatch")
        for key in ("workflow_version", "graph_version", "status", "updated_at"):
            if not isinstance(state.get(key), str) or not state[key]:
                raise IntegrityBlockedError("state.%s is malformed" % key)
        if state.get("graph_version") != GRAPH_VERSION:
            raise IntegrityBlockedError("graph version mismatch")
        if state.get("status") not in {"active", "paused_after_epoch", "paused_after_group", "terminal", "needs_user"}:
            raise IntegrityBlockedError("state lifecycle status is invalid")
        for key in ("revision", "state_revision"):
            if not isinstance(state.get(key), int) or isinstance(state[key], bool) or state[key] < 1:
                raise IntegrityBlockedError("state.%s is malformed" % key)
        if state["revision"] != state["state_revision"]:
            raise IntegrityBlockedError("state revision fields disagree")
        if state.get("graph_revision") != state.get("revision"):
            raise IntegrityBlockedError("state graph revision is not bound to canonical revision")
        required_collections = ("object_refs", "artifacts", "tasks", "reviews", "findings", "verdicts", "finding_validations", "leases", "epoch_contexts", "nodes", "idempotency")
        if any(not isinstance(state.get(name), dict) for name in required_collections):
            raise IntegrityBlockedError("state collection is missing")
        if not isinstance(state.get("edges"), list) or not isinstance(state.get("authority"), Mapping) or not isinstance(state.get("metadata"), Mapping):
            raise IntegrityBlockedError("state authority/metadata/edges are malformed")
        for name, expected_type in (("objective_history", list), ("objective_approvals", dict), ("objective_events", list)):
            if name not in state or not isinstance(state[name], expected_type):
                raise IntegrityBlockedError("state.%s is malformed" % name)
        self._validate_objective_associations(state, load_objects)
        try:
            self._ensure_durable_payload(state, "state")
        except KernelError as exc:
            raise IntegrityBlockedError("state contains non-durable data") from exc
        if not isinstance(state.get("group"), Mapping) or not isinstance(state["group"].get("id"), str) or not _ID.fullmatch(state["group"]["id"]) or state["group"].get("status") not in ("open", "closed"):
            raise IntegrityBlockedError("state.group is malformed")
        if not isinstance(state.get("epoch"), Mapping) or not isinstance(state["epoch"].get("id"), str) or not _ID.fullmatch(state["epoch"]["id"]) or state["epoch"].get("status") not in ("open", "closed") or state["epoch"].get("group_id") != state["group"]["id"] or not isinstance(state["epoch"].get("clear_before_next"), bool):
            raise IntegrityBlockedError("state.epoch is malformed")
        if state["group"]["status"] == "closed" and state["epoch"]["status"] != "closed":
            raise IntegrityBlockedError("closed Group cannot contain an open Epoch")
        if state["status"] == "active" and (state["group"]["status"] != "open" or state["epoch"]["status"] != "open"):
            raise IntegrityBlockedError("active state must have an open Group and Epoch")
        if state["status"] == "paused_after_epoch" and state["epoch"]["status"] != "closed":
            raise IntegrityBlockedError("paused-after-Epoch state must have a closed Epoch")
        if state["status"] == "paused_after_group" and state["group"]["status"] != "closed":
            raise IntegrityBlockedError("paused-after-Group state must have a closed Group")
        for boundary_name, boundary in (("Epoch", state["epoch"]), ("Group", state["group"])):
            if boundary.get("status") != "closed":
                continue
            for field in ("bundle_ref", "checkpoint_ref"):
                try:
                    self._validate_ref_runtime(boundary.get(field), "%s.%s" % (boundary_name, field))
                except KernelError as exc:
                    raise IntegrityBlockedError("closed %s requires %s" % (boundary_name, field)) from exc
                if boundary[field].get("digest") not in state["object_refs"]:
                    raise IntegrityBlockedError("closed %s %s is not catalogued" % (boundary_name, field))
            closed_at = boundary.get("closed_at_revision")
            if not isinstance(closed_at, int) or isinstance(closed_at, bool) or closed_at < 1 or closed_at > state["revision"]:
                raise IntegrityBlockedError("closed %s closed_at_revision is malformed" % boundary_name)
        if state["epoch"].get("status") == "closed":
            current_context = state["epoch_contexts"].get(state["epoch"].get("id"))
            if not isinstance(current_context, Mapping) or current_context.get("status") != "closed":
                raise IntegrityBlockedError("closed Epoch context is missing its closure record")
            for field in ("bundle_ref", "checkpoint_ref"):
                try:
                    self._validate_ref_runtime(current_context.get(field), "epoch_context.%s" % field)
                except KernelError as exc:
                    raise IntegrityBlockedError("closed Epoch context requires %s" % field) from exc
                if current_context[field].get("digest") not in state["object_refs"]:
                    raise IntegrityBlockedError("closed Epoch context %s is not catalogued" % field)
            if current_context.get("closed_at_revision") != state["epoch"].get("closed_at_revision"):
                raise IntegrityBlockedError("closed Epoch context closure revision is inconsistent")
        try:
            self._validate_budget(state.get("context_budget"))
        except KernelError as exc:
            raise IntegrityBlockedError("state.context_budget is malformed") from exc
        review_budget = state.get("review_budget")
        if not isinstance(review_budget, Mapping) or set(review_budget) != {"version", "deadline", "max_rounds", "max_attempts_per_finding", "rounds_used", "finding_attempts"} or not isinstance(review_budget.get("version"), str) or not _ID.fullmatch(review_budget["version"]) or any(not isinstance(review_budget.get(key), int) or isinstance(review_budget[key], bool) or review_budget[key] < 1 for key in ("max_rounds", "max_attempts_per_finding")) or not isinstance(review_budget.get("rounds_used"), int) or review_budget["rounds_used"] < 0 or not isinstance(review_budget.get("finding_attempts"), Mapping):
            raise IntegrityBlockedError("state.review_budget is malformed")
        try:
            deadline = _datetime.datetime.fromisoformat(str(review_budget.get("deadline")).replace("Z", "+00:00"))
        except ValueError as exc:
            raise IntegrityBlockedError("state.review_budget deadline is malformed") from exc
        if deadline.tzinfo is None or any(not isinstance(key, str) or not _ID.fullmatch(key) or not isinstance(value, int) or value < 0 for key, value in review_budget["finding_attempts"].items()):
            raise IntegrityBlockedError("state.review_budget is malformed")
        if state.get("budget_terminal") is not None and not isinstance(state.get("budget_terminal"), Mapping):
            raise IntegrityBlockedError("state.budget_terminal is malformed")
        if not isinstance(state.get("terminal_history"), list):
            raise IntegrityBlockedError("state.terminal_history is malformed")
        migration = state.get("migration")
        if migration is not None:
            required_migration = {
                "schema", "source_revision", "source_digests", "source_bindings", "source_refs",
                "field_mapping", "history_preserved", "cutover_status", "source_snapshot",
                "source_attestation",
            }
            if not isinstance(migration, Mapping) or set(migration) != required_migration or migration.get("schema") != "legacy-migration/v1" or migration.get("history_preserved") is not True or migration.get("cutover_status") not in {"candidate", "cutover", "rolled_back"}:
                raise IntegrityBlockedError("state migration record is malformed")
            if not isinstance(migration.get("source_revision"), int) or isinstance(migration.get("source_revision"), bool) or migration["source_revision"] < 1:
                raise IntegrityBlockedError("state migration source revision is malformed")
            source_digests = migration.get("source_digests")
            if not isinstance(source_digests, Mapping) or set(source_digests) != {"run", "bundle", "worker_report"} or any(not isinstance(digest, str) or not _DIGEST.fullmatch(digest) for digest in source_digests.values()):
                raise IntegrityBlockedError("state migration source digests are malformed")
            bindings = migration.get("source_bindings")
            binding_required = {"source_run_id", "group_id", "epoch_id", "aliases", "source_revision", "run_status", "source_status", "epoch_status"}
            if not isinstance(bindings, Mapping) or set(bindings) != binding_required or bindings.get("source_revision") != migration["source_revision"] or bindings.get("run_status") not in {"open", "closed"} or not isinstance(bindings.get("source_status"), str) or not bindings.get("source_status") or bindings.get("epoch_status") != "closed":
                raise IntegrityBlockedError("state migration source binding is malformed")
            for key in ("source_run_id", "group_id", "epoch_id"):
                if not isinstance(bindings.get(key), str) or not _ID.fullmatch(bindings[key]):
                    raise IntegrityBlockedError("state migration source binding identity is malformed")
            if not isinstance(bindings.get("aliases"), list) or any(not isinstance(alias, str) or not alias for alias in bindings["aliases"]):
                raise IntegrityBlockedError("state migration source binding aliases are malformed")
            if not isinstance(migration.get("source_refs"), list) or len(migration["source_refs"]) != 3:
                raise IntegrityBlockedError("state migration source references are incomplete")
            source_types = {"legacy-run", "legacy-bundle", "legacy-worker-report"}
            seen_types = set()
            expected_source_types = {
                "legacy-run": "run",
                "legacy-bundle": "bundle",
                "legacy-worker-report": "worker_report",
            }
            legacy_source_values: Dict[str, Mapping[str, Any]] = {}
            for source_ref in migration["source_refs"]:
                self._validate_ref_runtime(source_ref, "state.migration.source_ref")
                object_type = source_ref.get("object_type")
                if object_type not in source_types or object_type in seen_types:
                    raise IntegrityBlockedError("state migration source reference type is invalid")
                seen_types.add(object_type)
                if source_ref.get("digest") not in state["object_refs"]:
                    raise IntegrityBlockedError("state migration source reference is not catalogued")
                if load_objects:
                    source_object = self._load_ref_object(source_ref, "state.migration.%s" % object_type)
                    source_payload = source_object.get("payload")
                    source_key = expected_source_types[object_type]
                    if not isinstance(source_payload, Mapping) or source_payload.get("source_digest") != source_digests[source_key] or not isinstance(source_payload.get("value"), Mapping):
                        raise IntegrityBlockedError("state migration source object digest binding is invalid")
                    legacy_source_values[source_key] = source_payload["value"]
            if seen_types != source_types:
                raise IntegrityBlockedError("state migration source reference set is incomplete")
            if load_objects:
                self._validate_legacy_source_binding(
                    legacy_source_values["run"],
                    legacy_source_values["bundle"],
                    legacy_source_values["worker_report"],
                    bindings,
                    migration["source_revision"],
                    IntegrityBlockedError,
                )
            if not isinstance(migration.get("field_mapping"), Mapping):
                raise IntegrityBlockedError("state migration field mapping is malformed")
            try:
                self._validate_migration_attestation_shape(
                    migration.get("source_attestation"), "state.migration.source_attestation"
                )
            except KernelError as exc:
                raise IntegrityBlockedError("state migration source attestation is malformed") from exc
            snapshot = migration.get("source_snapshot")
            if (
                not isinstance(snapshot, Mapping)
                or set(snapshot) != {"mode", "source_digests", "source_bindings"}
                or snapshot.get("mode") != "immutable"
                or snapshot.get("source_digests") != source_digests
                or snapshot.get("source_bindings") != bindings
            ):
                raise IntegrityBlockedError("state migration immutable snapshot binding is malformed")
        aliases = state["metadata"].get("aliases")
        if not isinstance(aliases, list) or any(not isinstance(alias, str) or not alias for alias in aliases):
            raise IntegrityBlockedError("state.metadata.aliases is malformed")
        section = state["metadata"].get("section_control")
        if section is not None:
            if not isinstance(section, Mapping):
                raise IntegrityBlockedError("section control state is malformed")
            allowed_section_fields = {
                "schema", "section_id", "workflow_id", "version", "groups", "transition",
                "accepted_section_receipt", "parent_accepted_section_receipt", "section_history",
            }
            if set(section) - allowed_section_fields or not {
                "schema", "section_id", "workflow_id", "version", "groups", "transition"
            }.issubset(section):
                raise IntegrityBlockedError("section control state fields are malformed")
            section_id = section.get("section_id")
            if (
                section.get("schema") != "section-control-state/v1"
                or not isinstance(section_id, str) or not re.fullmatch(r"S[0-9]+", section_id)
                or not isinstance(section.get("workflow_id"), str) or not section["workflow_id"]
                or not isinstance(section.get("version"), str) or not section["version"]
                or not isinstance(section.get("groups"), Mapping)
                or state.get("group", {}).get("id") not in section["groups"]
            ):
                raise IntegrityBlockedError("section control state is invalid")
            transition = section.get("transition")
            if transition not in (
                {"intent": "source-transition-fixture-passed", "state": "pending"},
                {"intent": "source-transition-fixture-passed", "state": "accepted"},
            ):
                raise IntegrityBlockedError("section control transition is invalid")
            position = int(section_id[1:])
            history = section.get("section_history", [])
            if not isinstance(history, list) or len(history) != position:
                raise IntegrityBlockedError("section history is not contiguous")
            receipts = set()
            for expected_position, item in enumerate(history):
                if not isinstance(item, Mapping) or set(item) != {
                    "position", "section_id", "accepted_receipt_ref", "bundle_ref", "checkpoint_ref"
                } or item.get("position") != expected_position or item.get("section_id") != "S%d" % expected_position:
                    raise IntegrityBlockedError("section history item is malformed")
                for field, object_type in (("accepted_receipt_ref", "artifact"), ("bundle_ref", "artifact-bundle"), ("checkpoint_ref", "checkpoint")):
                    try:
                        self._validate_ref_runtime(item.get(field), "section_history.%s" % field)
                    except KernelError as exc:
                        raise IntegrityBlockedError("section history reference is malformed") from exc
                    ref = item[field]
                    if ref.get("object_type") != object_type or ref.get("digest") not in state["object_refs"]:
                        raise IntegrityBlockedError("section history reference is not catalogued")
                receipt_digest = item["accepted_receipt_ref"]["digest"]
                if receipt_digest in receipts:
                    raise IntegrityBlockedError("section history reuses an accepted receipt")
                receipts.add(receipt_digest)
                if load_objects:
                    receipt = self._load_ref_object(
                        item["accepted_receipt_ref"], "section history accepted receipt"
                    )
                    wrapper = receipt.get("payload")
                    payload = wrapper.get("payload") if isinstance(wrapper, Mapping) else None
                    if (
                        receipt.get("object_type") != "artifact"
                        or not isinstance(wrapper, Mapping)
                        or wrapper.get("kind") != "section-transition-receipt"
                        or not isinstance(payload, Mapping)
                        or payload.get("schema") != "section-acceptance-receipt/v1"
                        or payload.get("run_id") != self.run_id
                        or payload.get("section_id") != item["section_id"]
                        or payload.get("bundle_ref") != item["bundle_ref"]
                        or payload.get("checkpoint_ref") != item["checkpoint_ref"]
                    ):
                        raise IntegrityBlockedError(
                            "section history accepted receipt payload is not bound"
                        )
            for field in ("accepted_section_receipt", "parent_accepted_section_receipt"):
                if field not in section:
                    continue
                try:
                    self._validate_ref_runtime(section[field], "section control %s" % field)
                except KernelError as exc:
                    raise IntegrityBlockedError("section control receipt is malformed") from exc
                if section[field].get("object_type") != "artifact" or section[field].get("digest") not in state["object_refs"]:
                    raise IntegrityBlockedError("section control receipt is not catalogued")
            if transition["state"] == "accepted" and "accepted_section_receipt" not in section:
                raise IntegrityBlockedError("accepted section is missing its canonical receipt")
        if load_objects:
            for digest, ref in state["object_refs"].items():
                if not isinstance(digest, str) or not _DIGEST.fullmatch(digest):
                    raise IntegrityBlockedError("object_refs contains malformed digest")
                self._validate_ref_runtime(ref, "state.object_refs.%s" % digest)
                if ref.get("digest") not in (None, digest) and ref.get("object_digest") not in (None, digest):
                    raise IntegrityBlockedError("object reference key/digest mismatch")
                loaded = self._load_object(digest)
                declared_type = ref.get("object_type")
                if declared_type is not None and loaded.get("object_type") != declared_type:
                    raise IntegrityBlockedError("object reference type does not match object: %s" % digest)
        else:
            for digest, ref in state["object_refs"].items():
                if not isinstance(digest, str) or not _DIGEST.fullmatch(digest):
                    raise IntegrityBlockedError("object_refs contains malformed digest")
                self._validate_ref_runtime(ref, "state.object_refs.%s" % digest)
        if load_objects:
            for boundary_name, boundary in (("Epoch", state["epoch"]), ("Group", state["group"])):
                if boundary.get("status") == "closed":
                    self._validate_closed_boundary_objects(state, boundary_name, boundary)
        for node_id, node_type in state["nodes"].items():
            if (
                not isinstance(node_id, str)
                or not node_id
                or ":" not in node_id
                or not isinstance(node_type, str)
                or node_type not in {"objective", "artifact", "task", "work-product", "review", "finding", "authority", "approval"}
                or node_id.split(":", 1)[0] != node_type
                or not _ID.fullmatch(node_id.split(":", 1)[1])
            ):
                raise IntegrityBlockedError("state.nodes contains malformed node")
        for artifact_id, artifact in state["artifacts"].items():
            if not isinstance(artifact_id, str) or not _ID.fullmatch(artifact_id) or not isinstance(artifact, Mapping) or artifact.get("artifact_id") != artifact_id or not isinstance(artifact.get("version"), str) or not isinstance(artifact.get("digest"), str) or not _DIGEST.fullmatch(artifact["digest"]):
                raise IntegrityBlockedError("invalid Artifact record: %s" % artifact_id)
            self._validate_ref_runtime(artifact.get("object_ref"), "artifact.object_ref")
            if artifact["object_ref"].get("digest") != artifact["digest"] or artifact["digest"] not in state["object_refs"] or "artifact:" + artifact_id not in state["nodes"]:
                raise IntegrityBlockedError("Artifact object/node binding is invalid: %s" % artifact_id)
            if load_objects:
                artifact_object = self._load_ref_object(artifact["object_ref"], "artifact.object_ref")
                artifact_payload = artifact_object.get("payload")
                expected_artifact_type = "legacy-artifact" if artifact.get("legacy") else "artifact"
                if artifact_object.get("object_type") != expected_artifact_type or not isinstance(artifact_payload, Mapping) or (expected_artifact_type == "artifact" and (artifact_payload.get("artifact_id") != artifact_id or artifact_payload.get("version") != artifact.get("version"))) or (expected_artifact_type == "legacy-artifact" and artifact_payload.get("legacy_id") != artifact_id):
                    raise IntegrityBlockedError("Artifact object payload binding is invalid: %s" % artifact_id)
        for task_id, task in state["tasks"].items():
            task_allowed_fields = {
                "task_id", "attempt_id", "package_ref", "assignment", "input_refs",
                "write_scope", "freshness", "stop_conditions", "invalidated",
                "stop_requested", "output_path", "status", "epoch_id",
                "sibling_group", "authority_ref", "authority_digest",
                "authority_expected_head", "authority_expected_head_revision",
                "proposal_digest", "lease_status", "result_ref", "result_status",
                "result_submitted_at_revision", "result_state", "completed_by",
                "replan_requested", "replan_reason",
                "graph_revision", "replacement_task_id", "dependency_closure",
                "cancel_requested", "quarantine_reason", "quarantine_refs",
            }
            if isinstance(task, Mapping) and set(task) - task_allowed_fields:
                raise IntegrityBlockedError("Task has unsupported fields: %s" % task_id)
            if not isinstance(task_id, str) or not _ID.fullmatch(task_id) or not isinstance(task, Mapping) or task.get("task_id") != task_id or not isinstance(task.get("assignment"), Mapping) or task["assignment"].get("role") != "worker" or not isinstance(task["assignment"].get("assignment_id"), str) or not _ID.fullmatch(task["assignment"]["assignment_id"]):
                raise IntegrityBlockedError("invalid Task record: %s" % task_id)
            self._validate_ref_runtime(task.get("package_ref"), "task.package_ref")
            if task["package_ref"].get("digest") not in state["object_refs"] or task.get("status") not in ("planned", "ready", "running", "leased", "result_submitted", "succeeded", "partial", "failed", "needs_decision", "blocked_review", "fix_claimed", "invalidated") or not isinstance(task.get("freshness"), Mapping) or task["freshness"].get("epoch_id") != task.get("epoch_id") or "task:" + task_id not in state["nodes"]:
                raise IntegrityBlockedError("Task freshness/status/node binding is invalid: %s" % task_id)
            task_authority = task.get("authority_ref")
            if not isinstance(task_authority, Mapping) or not task_authority:
                raise IntegrityBlockedError("Task authority object is missing: %s" % task_id)
            if not isinstance(task.get("authority_digest"), str) or _digest(task_authority) != task.get("authority_digest"):
                raise IntegrityBlockedError("Task authority object digest is invalid: %s" % task_id)
            try:
                self._authority_ok(task_authority, "claim_task")
            except KernelError as exc:
                # A currently expired authority is a readiness blocker, but a
                # structurally invalid authority would make the canonical
                # state unverifiable in a fresh process.
                if "expired" not in str(exc):
                    raise IntegrityBlockedError("Task authority object is malformed: %s" % task_id) from exc
            if task.get("lease_status") is not None and task.get("lease_status") not in ("leased", "released"):
                raise IntegrityBlockedError("Task lease status is invalid: %s" % task_id)
            task_status = task.get("status")
            lease = state.get("leases", {}).get(task_id)
            if task_status in ("running", "leased") and not isinstance(lease, Mapping):
                raise IntegrityBlockedError("running Task has no live lease: %s" % task_id)
            if isinstance(lease, Mapping):
                if task.get("lease_status") != "leased" or task_status not in ("running", "leased"):
                    raise IntegrityBlockedError("Task lease/status binding is invalid: %s" % task_id)
                if lease.get("status") != "leased":
                    raise IntegrityBlockedError("Task lease is not live: %s" % task_id)
            elif task.get("lease_status") == "leased":
                raise IntegrityBlockedError("Task marks a missing lease as live: %s" % task_id)
            if task.get("status") == "invalidated" and task.get("invalidated") is not True:
                raise IntegrityBlockedError("invalidated Task status is not marked invalidated: %s" % task_id)
            if task.get("result_ref") is None:
                if any(task.get(key) is not None for key in ("result_status", "result_submitted_at_revision", "result_state", "completed_by")):
                    raise IntegrityBlockedError("Task carries result metadata without a Work Product: %s" % task_id)
            else:
                if task.get("result_status") not in ("success", "partial", "failure", "failed", "needs_decision") or task.get("result_state") != "result_submitted":
                    raise IntegrityBlockedError("Task result state is not submitted: %s" % task_id)
                result_revision = task.get("result_submitted_at_revision")
                if not isinstance(result_revision, int) or isinstance(result_revision, bool) or result_revision < 1 or result_revision > state["revision"]:
                    raise IntegrityBlockedError("Task result revision is malformed: %s" % task_id)
                if not isinstance(task.get("completed_by"), str) or task.get("completed_by") != task["assignment"].get("assignment_id"):
                    raise IntegrityBlockedError("Task result producer binding is invalid: %s" % task_id)
                if task.get("status") == "needs_decision" and task.get("result_status") != "needs_decision":
                    raise IntegrityBlockedError("needs_decision Task result binding is invalid: %s" % task_id)
            if "replan_requested" in task and not isinstance(task.get("replan_requested"), bool):
                raise IntegrityBlockedError("Task replan flag is malformed: %s" % task_id)
            if task.get("replan_requested") and (not isinstance(task.get("replan_reason"), str) or not task.get("replan_reason")):
                raise IntegrityBlockedError("Task replan reason is missing: %s" % task_id)
            if task.get("graph_revision") != state.get("graph_revision"):
                raise IntegrityBlockedError("Task graph revision is not bound to state: %s" % task_id)
            replacement = task.get("replacement_task_id")
            if replacement is not None and (not isinstance(replacement, str) or not _ID.fullmatch(replacement) or replacement not in state["tasks"]):
                raise IntegrityBlockedError("Task replacement attempt is not registered: %s" % task_id)
            closure = task.get("dependency_closure", [])
            if not isinstance(closure, list) or any(not isinstance(item, str) or not _ID.fullmatch(item) for item in closure) or len(set(closure)) != len(closure):
                raise IntegrityBlockedError("Task reverse dependency closure is malformed: %s" % task_id)
            if not isinstance(task.get("cancel_requested", False), bool):
                raise IntegrityBlockedError("Task cancellation marker is malformed: %s" % task_id)
            if task.get("quarantine_reason") is not None and (not isinstance(task.get("quarantine_reason"), str) or not task.get("quarantine_reason")):
                raise IntegrityBlockedError("Task quarantine reason is malformed: %s" % task_id)
            if not isinstance(task.get("quarantine_refs", []), list):
                raise IntegrityBlockedError("Task quarantine references are malformed: %s" % task_id)
            proposal_digest = task_authority.get("proposal_digest")
            if proposal_digest is not None and task.get("proposal_digest") != proposal_digest:
                raise IntegrityBlockedError("Task authority proposal binding is invalid: %s" % task_id)
            expected_authority_head = task_authority.get("expected_head")
            if expected_authority_head is not None and task.get("authority_expected_head") != expected_authority_head:
                raise IntegrityBlockedError("Task authority expected HEAD binding is invalid: %s" % task_id)
            if task_authority.get("expected_head_revision") is not None and task.get("authority_expected_head_revision") != task_authority.get("expected_head_revision"):
                raise IntegrityBlockedError("Task authority expected HEAD revision binding is invalid: %s" % task_id)
            created_at_revision = task["freshness"].get("created_at_revision")
            if (
                not isinstance(created_at_revision, int)
                or isinstance(created_at_revision, bool)
                or created_at_revision < 1
                or created_at_revision > state["revision"]
            ):
                raise IntegrityBlockedError("Task freshness revision is future or malformed: %s" % task_id)
            try:
                task_paths = self._scope_paths(task.get("write_scope"), "task.write_scope")
                output_path = task.get("output_path")
                output_paths = self._scope_paths(output_path, "task.output_path") if output_path is not None else []
                if task_paths and any(
                    not any(self._scope_contains(prefix, path) for prefix in task_paths)
                    for path in output_paths
                ):
                    raise AuthorizationError("task output_path is outside task.write_scope")
            except KernelError as exc:
                raise IntegrityBlockedError("Task output scope binding is invalid: %s" % task_id) from exc
            if task.get("result_ref") is not None:
                self._validate_ref_runtime(task["result_ref"], "task.result_ref")
                if task["result_ref"].get("digest") not in state["object_refs"]:
                    raise IntegrityBlockedError("Task result reference is not catalogued: %s" % task_id)
                if "work-product:" + task_id not in state["nodes"] or state["nodes"].get("work-product:" + task_id) != "work-product":
                    raise IntegrityBlockedError("Task result node binding is invalid: %s" % task_id)
                if load_objects:
                    result_object = self._load_ref_object(task["result_ref"], "task.result_ref")
                    result_payload = result_object.get("payload")
                    if result_object.get("object_type") != "work-product" or not isinstance(result_payload, Mapping) or result_payload.get("task_id") != task_id:
                        raise IntegrityBlockedError("Task result object binding is invalid: %s" % task_id)
            if load_objects:
                package_object = self._load_ref_object(task["package_ref"], "task.package_ref")
                package_payload = package_object.get("payload")
                if (
                    package_object.get("object_type") != "task-package"
                    or not isinstance(package_payload, Mapping)
                    or package_payload.get("task_id") != task_id
                    or package_payload.get("attempt_id") != task.get("attempt_id")
                    or package_payload.get("assignment") != task.get("assignment")
                    or package_payload.get("input_refs") != task.get("input_refs")
                    or package_payload.get("write_scope") != task.get("write_scope")
                    or package_payload.get("output_path") != task.get("output_path")
                    # The immutable package owns the publication freshness
                    # boundary.  The live Task may add explicit stale or
                    # invalidated markers during replan/invalidate; those
                    # lifecycle markers must not rewrite the immutable
                    # package object or make a valid history unreadable.
                    or not isinstance(package_payload.get("freshness"), Mapping)
                    or not isinstance(task.get("freshness"), Mapping)
                    or any(
                        package_payload["freshness"].get(key) != task["freshness"].get(key)
                        for key in ("epoch_id", "created_at_revision")
                    )
                    or package_payload.get("stop_conditions") != task.get("stop_conditions")
                    # ``invalidated`` and ``stop_requested`` are live
                    # lifecycle markers.  A replan may set them on the
                    # canonical Task while the published package remains an
                    # immutable record of the original assignment.  Preserve
                    # a package's positive claim, but permit the live record
                    # to become more restrictive.
                    or (package_payload.get("invalidated") is True and task.get("invalidated") is not True)
                    or (package_payload.get("stop_requested") is True and task.get("stop_requested") is not True)
                ):
                    raise IntegrityBlockedError("Task package object binding is invalid: %s" % task_id)
                for input_index, input_ref in enumerate(task.get("input_refs", [])):
                    try:
                        self._validate_input_ref_binding(state, input_ref, "task.%s.input_refs[%s]" % (task_id, input_index))
                    except KernelError as exc:
                        raise IntegrityBlockedError("Task input object binding is invalid: %s" % task_id) from exc
        for review_id, review in state["reviews"].items():
            review_fields = {
                "review_id", "candidate_task_id", "reviewer_assignment_id", "fresh_epoch_id",
                "review_kind", "target_finding_id", "findings", "candidate_ref", "evidence_refs",
                "provenance", "package_ref", "verdict", "status",
            }
            if (
                not isinstance(review_id, str)
                or not _ID.fullmatch(review_id)
                or not isinstance(review, Mapping)
                or set(review) != review_fields
                or review.get("review_id") != review_id
                or review.get("review_kind") not in ("initial", "closure")
                or review.get("verdict") not in ("pass", "changes_requested", "pending")
                or review.get("status") not in ("open", "closed")
                or not isinstance(review.get("candidate_task_id"), str)
                or not _ID.fullmatch(review["candidate_task_id"])
                or not isinstance(review.get("reviewer_assignment_id"), str)
                or not _ID.fullmatch(review["reviewer_assignment_id"])
                or not isinstance(review.get("fresh_epoch_id"), str)
                or not _ID.fullmatch(review["fresh_epoch_id"])
                or (review.get("target_finding_id") is not None and (
                    not isinstance(review["target_finding_id"], str)
                    or not _ID.fullmatch(review["target_finding_id"])
                ))
                or not isinstance(review.get("findings"), list)
                or not isinstance(review.get("evidence_refs"), list)
                or not isinstance(review.get("candidate_ref"), Mapping)
                or not isinstance(review.get("provenance"), Mapping)
                or set(review["provenance"]) != {
                    "candidate_package_digest", "evidence_digests", "review_epoch_id",
                    "reviewer_assignment_id",
                }
                or review["provenance"].get("candidate_package_digest") != review.get("candidate_ref", {}).get("digest")
                or review["provenance"].get("review_epoch_id") != review.get("fresh_epoch_id")
                or review["provenance"].get("reviewer_assignment_id") != review.get("reviewer_assignment_id")
                or not isinstance(review["provenance"].get("evidence_digests"), list)
            ):
                raise IntegrityBlockedError("invalid Review record: %s" % review_id)
            self._validate_ref_runtime(review.get("package_ref"), "review.package_ref")
            self._validate_ref_runtime(review.get("candidate_ref"), "review.candidate_ref")
            candidate_task = state["tasks"].get(review["candidate_task_id"])
            if review["package_ref"].get("digest") not in state["object_refs"] or review["candidate_ref"].get("digest") not in state["object_refs"] or review["candidate_ref"].get("object_type") not in (None, "task-package") or not isinstance(candidate_task, Mapping) or review["candidate_ref"] != candidate_task.get("package_ref") or "review:" + review_id not in state["nodes"]:
                raise IntegrityBlockedError("Review provenance/node binding is invalid: %s" % review_id)
            if review.get("reviewer_assignment_id") == candidate_task.get("assignment", {}).get("assignment_id"):
                raise IntegrityBlockedError("Review reviewer is not independent from candidate: %s" % review_id)
            if review.get("review_kind") == "closure":
                if review.get("target_finding_id") is None or review.get("findings"):
                    raise IntegrityBlockedError("closure Review target/findings binding is invalid: %s" % review_id)
                if review.get("status") == "closed" and review.get("verdict") != "pass":
                    raise IntegrityBlockedError("closed closure Review must pass: %s" % review_id)
                if review.get("status") == "open" and review.get("verdict") != "pending":
                    raise IntegrityBlockedError("open closure Review must remain pending: %s" % review_id)
            elif review.get("target_finding_id") is not None:
                raise IntegrityBlockedError("initial Review cannot target a Finding: %s" % review_id)
            elif review.get("status") == "closed" and review.get("verdict") != "pass":
                raise IntegrityBlockedError("closed initial Review must pass: %s" % review_id)
            if len(review["evidence_refs"]) != len(review["provenance"]["evidence_digests"]):
                raise IntegrityBlockedError("Review evidence provenance length is invalid: %s" % review_id)
            for evidence_index, evidence_ref in enumerate(review["evidence_refs"]):
                self._validate_ref_runtime(evidence_ref, "review.evidence_refs[%s]" % evidence_index)
                if evidence_ref.get("digest") not in state["object_refs"] or evidence_ref.get("digest") != review["provenance"]["evidence_digests"][evidence_index]:
                    raise IntegrityBlockedError("Review evidence provenance is invalid: %s" % review_id)
            if review.get("review_kind") == "closure":
                target_finding = state["findings"].get(review.get("target_finding_id"))
                resolution_ref = target_finding.get("resolution_ref") if isinstance(target_finding, Mapping) else None
                expected_digest = resolution_ref.get("digest") if isinstance(resolution_ref, Mapping) else None
                evidence_digests = [ref.get("digest") or ref.get("object_digest") for ref in review.get("evidence_refs", [])]
                if not isinstance(resolution_ref, Mapping) or evidence_digests != [expected_digest]:
                    raise IntegrityBlockedError("closure Review evidence is not bound to the target Finding resolution")
            for finding_index, finding in enumerate(review["findings"]):
                if (
                    not isinstance(finding, Mapping)
                    or set(finding) != {
                        "finding_id", "fingerprint", "requirement_ref", "description",
                        "evidence", "severity", "state", "blocking",
                    }
                    or not isinstance(finding.get("finding_id"), str)
                    or not _ID.fullmatch(finding["finding_id"])
                    or not isinstance(finding.get("fingerprint"), str)
                    or not _DIGEST.fullmatch(finding["fingerprint"])
                    or (
                        not isinstance(finding.get("requirement_ref"), (str, Mapping, list, tuple, int, float, bool))
                        and finding.get("requirement_ref") is not None
                    )
                    or not isinstance(finding.get("description"), str)
                    or not finding.get("description")
                    or not isinstance(finding.get("evidence"), list)
                    or not isinstance(finding.get("severity"), str)
                    or not finding.get("severity")
                    or finding.get("state") != "open"
                    or not isinstance(finding.get("blocking"), bool)
                ):
                    raise IntegrityBlockedError("Review Finding shape is invalid: %s[%s]" % (review_id, finding_index))
                try:
                    expected_fingerprint = _digest({
                        "requirement_ref": finding["requirement_ref"],
                        "description": finding["description"],
                        "severity": finding["severity"],
                    })
                except (TypeError, ValueError) as exc:
                    raise IntegrityBlockedError("Review Finding stable fields are not JSON-compatible: %s[%s]" % (review_id, finding_index)) from exc
                if finding["fingerprint"] != expected_fingerprint:
                    raise IntegrityBlockedError("Review Finding fingerprint is inconsistent: %s[%s]" % (review_id, finding_index))
            if load_objects:
                review_object = self._load_ref_object(review["package_ref"], "review.package_ref")
                expected_review = {key: review.get(key) for key in ("review_id", "candidate_task_id", "reviewer_assignment_id", "fresh_epoch_id", "review_kind", "target_finding_id", "findings", "candidate_ref", "evidence_refs", "provenance")}
                if review_object.get("object_type") != "review-package" or review_object.get("payload") != expected_review:
                    raise IntegrityBlockedError("Review package object binding is invalid: %s" % review_id)
                candidate_object = self._load_ref_object(review.get("candidate_ref"), "review.candidate_ref")
                candidate_payload = candidate_object.get("payload")
                if candidate_object.get("object_type") != "task-package" or not isinstance(candidate_payload, Mapping) or candidate_payload.get("task_id") != review.get("candidate_task_id"):
                    raise IntegrityBlockedError("Review candidate object binding is invalid: %s" % review_id)
                for evidence_ref in review.get("evidence_refs", []):
                    self._load_ref_object(evidence_ref, "review.evidence_ref")
            if review.get("review_kind") == "closure":
                if review.get("target_finding_id") is None or review.get("findings"):
                    raise IntegrityBlockedError("closure Review target/findings binding is invalid: %s" % review_id)
                target = state["findings"].get(review.get("target_finding_id"))
                if not isinstance(target, Mapping) or target.get("candidate_task_id") != review.get("candidate_task_id"):
                    raise IntegrityBlockedError("closure Review Finding binding is invalid: %s" % review_id)
                epoch_context = state["epoch_contexts"].get(review.get("fresh_epoch_id"))
                if not isinstance(epoch_context, Mapping) or epoch_context.get("candidate_task_id") != review.get("candidate_task_id") or epoch_context.get("finding_id") != review.get("target_finding_id") or epoch_context.get("reviewer_assignment_id") != review.get("reviewer_assignment_id"):
                    raise IntegrityBlockedError("closure Review Epoch binding is invalid: %s" % review_id)
                if epoch_context.get("status") not in ("reviewed", "closed"):
                    raise IntegrityBlockedError("closure Review Epoch is not reviewed: %s" % review_id)
                candidate_freshness = candidate_task.get("freshness", {})
                if epoch_context.get("started_at_revision", 0) <= candidate_freshness.get("created_at_revision", 0):
                    raise IntegrityBlockedError("closure Review Epoch is not fresh: %s" % review_id)
            else:
                epoch_context = state["epoch_contexts"].get(review.get("fresh_epoch_id"))
                if not isinstance(epoch_context, Mapping) or epoch_context.get("candidate_task_id") != review.get("candidate_task_id") or epoch_context.get("reviewer_assignment_id") != review.get("reviewer_assignment_id") or epoch_context.get("status") not in ("reviewed", "closed"):
                    raise IntegrityBlockedError("initial Review Epoch binding is invalid: %s" % review_id)
                if epoch_context.get("started_at_revision", 0) <= candidate_task.get("freshness", {}).get("created_at_revision", 0):
                    raise IntegrityBlockedError("initial Review Epoch is not fresh: %s" % review_id)
        for verdict_id, verdict in state["verdicts"].items():
            if (
                not isinstance(verdict_id, str)
                or not _ID.fullmatch(verdict_id)
                or not isinstance(verdict, Mapping)
                or not isinstance(verdict.get("review_id"), str)
                or verdict.get("review_id") != verdict_id
                or verdict.get("verdict") not in ("pass", "changes_requested", "pending")
                or verdict.get("derived") is not True
            ):
                raise IntegrityBlockedError("invalid Verdict record: %s" % verdict_id)
            review = state["reviews"].get(verdict_id)
            if not isinstance(review, Mapping):
                raise IntegrityBlockedError("Verdict references an unregistered Review: %s" % verdict_id)
            if verdict.get("verdict") != review.get("verdict"):
                raise IntegrityBlockedError("Verdict/Review verdict binding is invalid: %s" % verdict_id)

            # A plain Review verdict may be derived before a Finding exists.
            # Once a closure Finding verdict is present, however, every
            # provenance link is mandatory and reverse-bound to the immutable
            # Review package, fresh Epoch, resolution object, and evidence.
            closure_fields = {
                "finding_id", "reviewer_assignment_id", "fresh_epoch_id",
                "resolution_ref", "evidence_refs", "closure_package_ref",
            }
            declared_closure_fields = closure_fields.intersection(verdict)
            if not declared_closure_fields:
                if set(verdict) - {"review_id", "verdict", "derived"}:
                    raise IntegrityBlockedError("plain Verdict carries incomplete Finding provenance: %s" % verdict_id)
                continue
            if set(verdict) - ({"review_id", "verdict", "derived"} | closure_fields) or declared_closure_fields != closure_fields:
                raise IntegrityBlockedError("Finding Verdict provenance is incomplete: %s" % verdict_id)
            if review.get("review_kind") != "closure" or review.get("verdict") != "pass":
                raise IntegrityBlockedError("Finding Verdict must bind a passing closure Review: %s" % verdict_id)
            finding_id = verdict.get("finding_id")
            if not isinstance(finding_id, str) or not _ID.fullmatch(finding_id):
                raise IntegrityBlockedError("Finding Verdict finding_id is malformed: %s" % verdict_id)
            finding = state["findings"].get(finding_id)
            if not isinstance(finding, Mapping) or finding.get("closure_ref") is None:
                raise IntegrityBlockedError("Finding Verdict references an unclosed Finding: %s" % finding_id)
            if review.get("target_finding_id") != finding_id or review.get("candidate_task_id") != finding.get("candidate_task_id"):
                raise IntegrityBlockedError("Finding Verdict/Review Finding binding is invalid: %s" % verdict_id)
            reviewer = verdict.get("reviewer_assignment_id")
            fresh_epoch_id = verdict.get("fresh_epoch_id")
            if (
                not isinstance(reviewer, str)
                or not _ID.fullmatch(reviewer)
                or reviewer != review.get("reviewer_assignment_id")
                or not isinstance(fresh_epoch_id, str)
                or not _ID.fullmatch(fresh_epoch_id)
                or fresh_epoch_id != review.get("fresh_epoch_id")
            ):
                raise IntegrityBlockedError("Finding Verdict reviewer/Fresh Epoch binding is invalid: %s" % verdict_id)
            epoch_context = state["epoch_contexts"].get(fresh_epoch_id)
            if (
                not isinstance(epoch_context, Mapping)
                or epoch_context.get("group_id") != state["group"].get("id")
                or epoch_context.get("reviewer_assignment_id") != reviewer
                or epoch_context.get("finding_id") != finding_id
                or epoch_context.get("status") not in ("reviewed", "closed")
                or epoch_context.get("input_ref", {}).get("digest") != (finding.get("resolution_ref") or {}).get("digest")
            ):
                raise IntegrityBlockedError("Finding Verdict fresh Epoch is not registered/bound: %s" % verdict_id)
            resolution_ref = verdict.get("resolution_ref")
            if resolution_ref != finding.get("resolution_ref"):
                raise IntegrityBlockedError("Finding Verdict resolution binding is invalid: %s" % verdict_id)
            evidence_refs = verdict.get("evidence_refs")
            if not isinstance(evidence_refs, list) or evidence_refs != review.get("evidence_refs") or evidence_refs != [resolution_ref]:
                raise IntegrityBlockedError("Finding Verdict evidence binding is invalid: %s" % verdict_id)
            closure_package_ref = verdict.get("closure_package_ref")
            if closure_package_ref != review.get("package_ref"):
                raise IntegrityBlockedError("Finding Verdict closure package binding is invalid: %s" % verdict_id)
            for label, ref in (
                ("resolution_ref", resolution_ref),
                ("closure_package_ref", closure_package_ref),
            ):
                self._validate_ref_runtime(ref, "verdict.%s" % label)
                if ref.get("digest") not in state["object_refs"]:
                    raise IntegrityBlockedError("Finding Verdict %s is not catalogued" % label)
            for index, ref in enumerate(evidence_refs):
                self._validate_ref_runtime(ref, "verdict.evidence_refs[%s]" % index)
                if ref.get("digest") not in state["object_refs"]:
                    raise IntegrityBlockedError("Finding Verdict evidence is not catalogued")
            if finding.get("closure_ref", {}).get("digest") not in state["object_refs"]:
                raise IntegrityBlockedError("Finding Verdict object is not catalogued: %s" % verdict_id)
            if load_objects:
                verdict_object = self._load_ref_object(finding["closure_ref"], "finding.closure_ref")
                expected_payload = {
                    "finding_id": finding_id,
                    "review_id": verdict_id,
                    "reviewer_assignment_id": reviewer,
                    "fresh_epoch_id": fresh_epoch_id,
                    "resolution_ref": resolution_ref,
                    "evidence": evidence_refs,
                    "evidence_refs": evidence_refs,
                    "closure_package_ref": closure_package_ref,
                    "verdict": "pass",
                }
                if verdict_object.get("object_type") != "finding-verdict" or verdict_object.get("payload") != expected_payload:
                    raise IntegrityBlockedError("Finding Verdict object reverse binding is invalid: %s" % verdict_id)
        for context_id, context in state["epoch_contexts"].items():
            if not isinstance(context_id, str) or not _ID.fullmatch(context_id) or not isinstance(context, Mapping) or context.get("id") != context_id or context.get("group_id") != state["group"].get("id") or context.get("status") not in ("open", "reviewed", "closed"):
                raise IntegrityBlockedError("invalid Context Epoch record: %s" % context_id)
            if not isinstance(context.get("started_at_revision"), int) or isinstance(context["started_at_revision"], bool) or context["started_at_revision"] < 1 or context["started_at_revision"] > state["revision"]:
                raise IntegrityBlockedError("Context Epoch start revision is malformed: %s" % context_id)
            if context.get("input_ref") is not None:
                self._validate_ref_runtime(context["input_ref"], "epoch_context.input_ref")
            if context.get("reviewer_assignment_id") is not None and (not isinstance(context["reviewer_assignment_id"], str) or not _ID.fullmatch(context["reviewer_assignment_id"])):
                raise IntegrityBlockedError("Context Epoch reviewer binding is malformed: %s" % context_id)
        for task_id, lease in state["leases"].items():
            if task_id not in state["tasks"] or not isinstance(lease, Mapping) or lease.get("assignment_id") != state["tasks"][task_id]["assignment"].get("assignment_id"):
                raise IntegrityBlockedError("lease references an invalid task: %s" % task_id)
            try:
                self._scope_paths(lease.get("write_scope"), "lease.write_scope")
            except KernelError as exc:
                raise IntegrityBlockedError("lease scope is malformed: %s" % task_id) from exc
        self._validate_edges(state.get("edges", []), state.get("nodes"), exact=True)
        for finding_id, finding in state["findings"].items():
            finding_allowed_fields = {
                "finding_id", "fingerprint", "requirement_ref", "description", "evidence", "severity",
                "state", "blocking", "candidate_task_id", "introduced_review_id", "owner",
                "original_reviewer_assignment", "resolution_ref", "closed_by", "object_ref",
                "resolution_claimed_by", "closure_evidence", "closure_ref", "admitted",
                "validator_assignment_id", "validation_disposition",
            }
            if not isinstance(finding, Mapping) or set(finding) - finding_allowed_fields:
                raise IntegrityBlockedError("Finding has unsupported fields: %s" % finding_id)
            if not isinstance(finding_id, str) or not _ID.fullmatch(finding_id) or finding.get("finding_id") != finding_id or finding.get("state") not in FINDING_STATES or "requirement_ref" not in finding:
                raise IntegrityBlockedError("invalid Finding state: %s" % finding_id)
            if "closed" in finding:
                raise IntegrityBlockedError("Finding has a competing closed field: %s" % finding_id)
            if (
                not isinstance(finding.get("fingerprint"), str)
                or not _DIGEST.fullmatch(finding["fingerprint"])
                or not isinstance(finding.get("description"), str)
                or not finding.get("description")
                or not isinstance(finding.get("evidence"), list)
                or not isinstance(finding.get("severity"), str)
                or not finding.get("severity")
                or not isinstance(finding.get("blocking"), bool)
                or not isinstance(finding.get("candidate_task_id"), str)
                or not _ID.fullmatch(finding["candidate_task_id"])
                or not isinstance(finding.get("introduced_review_id"), str)
                or not _ID.fullmatch(finding["introduced_review_id"])
                or not isinstance(finding.get("owner"), str)
                or not _ID.fullmatch(finding["owner"])
                or not isinstance(finding.get("original_reviewer_assignment"), str)
                or not _ID.fullmatch(finding["original_reviewer_assignment"])
                or (finding.get("closed_by") is not None and (not isinstance(finding["closed_by"], str) or not _ID.fullmatch(finding["closed_by"])))
            ):
                raise IntegrityBlockedError("Finding fields are malformed: %s" % finding_id)
            if finding.get("admitted") is not None and not isinstance(finding.get("admitted"), bool):
                raise IntegrityBlockedError("Finding admission is malformed: %s" % finding_id)
            try:
                expected_fingerprint = _digest({
                    "requirement_ref": finding["requirement_ref"],
                    "description": finding["description"],
                    "severity": finding["severity"],
                })
            except (TypeError, ValueError) as exc:
                raise IntegrityBlockedError("Finding stable fields are not JSON-compatible: %s" % finding_id) from exc
            if finding["fingerprint"] != expected_fingerprint:
                raise IntegrityBlockedError("Finding fingerprint is inconsistent: %s" % finding_id)
            finding_state = finding.get("state")
            resolution_ref = finding.get("resolution_ref")
            closure_ref = finding.get("closure_ref")
            closed_by = finding.get("closed_by")
            if finding_state == "resolved" and not isinstance(resolution_ref, Mapping):
                raise IntegrityBlockedError("resolved Finding has no resolution claim: %s" % finding_id)
            if finding_state in {"open", "unresolved"} and any(
                value is not None for value in (resolution_ref, closure_ref, closed_by)
            ):
                raise IntegrityBlockedError("open Finding carries closure provenance: %s" % finding_id)
            if (closure_ref is None) != (closed_by is None):
                raise IntegrityBlockedError("Finding closure provenance is incomplete: %s" % finding_id)
            if closure_ref is not None and finding_state != "resolved":
                raise IntegrityBlockedError("non-resolved Finding carries a closure verdict: %s" % finding_id)
            if closure_ref is not None:
                passing_closures = [
                    (review_id, review)
                    for review_id, review in state["reviews"].items()
                    if isinstance(review, Mapping)
                    and review.get("review_kind") == "closure"
                    and review.get("target_finding_id") == finding_id
                    and review.get("verdict") == "pass"
                    and review.get("status") == "closed"
                ]
                if len(passing_closures) != 1:
                    raise IntegrityBlockedError("Finding closure requires one passing closure Review: %s" % finding_id)
                closure_review_id, closure_review = passing_closures[0]
                closure_verdict = state["verdicts"].get(closure_review_id)
                if not isinstance(closure_verdict, Mapping) or closure_verdict.get("finding_id") != finding_id or closure_verdict.get("reviewer_assignment_id") != finding.get("closed_by") or closure_verdict.get("fresh_epoch_id") != closure_review.get("fresh_epoch_id") or closure_verdict.get("resolution_ref") != finding.get("resolution_ref") or closure_verdict.get("evidence_refs") != closure_review.get("evidence_refs") or closure_verdict.get("closure_package_ref") != closure_review.get("package_ref"):
                    raise IntegrityBlockedError("Finding closure Review/Verdict full Verdict binding is invalid: %s" % finding_id)
            self._validate_ref_runtime(finding.get("object_ref"), "finding.object_ref")
            if finding["object_ref"].get("digest") not in state["object_refs"] or not isinstance(finding.get("candidate_task_id"), str) or finding["candidate_task_id"] not in state["tasks"] or not isinstance(finding.get("introduced_review_id"), str) or finding["introduced_review_id"] not in state["reviews"] or "finding:" + finding_id not in state["nodes"]:
                raise IntegrityBlockedError("Finding provenance/node binding is invalid: %s" % finding_id)
            for key in ("resolution_ref", "closure_ref"):
                if finding.get(key) is not None:
                    self._validate_ref_runtime(finding[key], "finding.%s" % key)
                    if finding[key].get("digest") not in state["object_refs"]:
                        raise IntegrityBlockedError("Finding %s reference is not catalogued" % key)
                    if load_objects:
                        finding_object = self._load_ref_object(finding[key], "finding.%s" % key)
                        expected_type = "finding-resolution" if key == "resolution_ref" else "finding-verdict"
                        finding_payload = finding_object.get("payload")
                        if finding_object.get("object_type") != expected_type or not isinstance(finding_payload, Mapping) or finding_payload.get("finding_id") != finding_id:
                            raise IntegrityBlockedError("Finding %s object type is invalid" % key)
                        if key == "closure_ref":
                            closure_review = state["reviews"].get(finding.get("introduced_review_id"))
                            closure_review_id = finding_payload.get("review_id")
                            bound_review = state["reviews"].get(closure_review_id) if isinstance(closure_review_id, str) else None
                            if (
                                finding_payload.get("reviewer_assignment_id") != finding.get("closed_by")
                                or finding_payload.get("evidence") != finding.get("closure_evidence")
                                or finding_payload.get("evidence_refs") != finding.get("closure_evidence")
                                or finding_payload.get("resolution_ref") != finding.get("resolution_ref")
                                or not isinstance(bound_review, Mapping)
                                or finding_payload.get("closure_package_ref") != bound_review.get("package_ref")
                            ):
                                raise IntegrityBlockedError("Finding closure verdict binding is invalid: %s" % finding_id)
            if load_objects:
                finding_object = self._load_ref_object(finding["object_ref"], "finding.object_ref")
                finding_payload = finding_object.get("payload")
                immutable_fields = ("finding_id", "fingerprint", "requirement_ref", "description", "evidence", "severity", "blocking", "candidate_task_id", "introduced_review_id", "owner", "original_reviewer_assignment")
                if finding_object.get("object_type") != "finding" or not isinstance(finding_payload, Mapping) or any(finding_payload.get(key) != finding.get(key) for key in immutable_fields):
                    raise IntegrityBlockedError("Finding object binding is invalid: %s" % finding_id)

        # A closure Review and its Finding closure reference form one
        # assignment.  Do this reverse check after the Finding collection has
        # been inspected so a digest-consistent forged state cannot downgrade
        # a full closure verdict to the three-field plain verdict shape while
        # leaving the Review/Finding records marked as closed.
        for review_id, review in state["reviews"].items():
            if not isinstance(review, Mapping) or review.get("review_kind") != "closure" or review.get("verdict") != "pass":
                continue
            finding = state["findings"].get(review.get("target_finding_id"))
            if not isinstance(finding, Mapping) or finding.get("closure_ref") is None:
                continue
            verdict = state["verdicts"].get(review_id)
            if not isinstance(verdict, Mapping) or set(verdict) != {
                "review_id", "verdict", "derived", "finding_id", "reviewer_assignment_id",
                "fresh_epoch_id", "resolution_ref", "evidence_refs", "closure_package_ref",
            }:
                raise IntegrityBlockedError("closed Finding is missing its full Verdict binding: %s" % review_id)
            if verdict.get("finding_id") != finding.get("finding_id") or verdict.get("resolution_ref") != finding.get("resolution_ref") or verdict.get("reviewer_assignment_id") != review.get("reviewer_assignment_id") or verdict.get("fresh_epoch_id") != review.get("fresh_epoch_id") or verdict.get("evidence_refs") != review.get("evidence_refs") or verdict.get("closure_package_ref") != review.get("package_ref"):
                raise IntegrityBlockedError("closed Finding Verdict reverse binding is inconsistent: %s" % review_id)

    def _load_current(self) -> Tuple[Dict[str, Any], Dict[str, Any]]:
        if self.blocked_path.exists():
            marker = _read_json(self.blocked_path)
            raise IntegrityBlockedError(marker.get("reason", "integrity is blocked"))
        head = self._load_head()
        digest = head["transaction_digest"]
        transaction = self._load_transaction(digest)
        seen: set[str] = set()
        transactions: Dict[int, Mapping[str, Any]] = {}
        expected_revision = head["revision"]
        current = transaction
        while True:
            current_digest = current["digest"]
            if current_digest in seen:
                raise IntegrityBlockedError("transaction chain cycle")
            seen.add(current_digest)
            if current.get("run_id") != self.run_id or current.get("revision") != expected_revision or current.get("state_revision", current.get("revision")) != expected_revision:
                raise IntegrityBlockedError("transaction revision mismatch")
            transactions[expected_revision] = current
            try:
                self._validate_state(current.get("state"), load_objects=False)
            except KernelError as exc:
                raise IntegrityBlockedError("transaction state is malformed") from exc
            if current.get("object_refs") != current.get("state", {}).get("object_refs"):
                raise IntegrityBlockedError("transaction/object reference catalog mismatch")
            parent = current.get("parent")
            if expected_revision == 1:
                if parent is not None:
                    raise IntegrityBlockedError("genesis transaction has a parent")
                if current.get("graph_delta") != self._graph_delta(None, current.get("state", {})):
                    raise IntegrityBlockedError("genesis graph delta mismatch")
                break
            if not isinstance(parent, Mapping):
                raise IntegrityBlockedError("missing transaction parent")
            if parent.get("revision") != expected_revision - 1:
                raise IntegrityBlockedError("transaction parent revision mismatch")
            parent_digest = parent.get("transaction_digest") or parent.get("digest")
            if not isinstance(parent_digest, str):
                raise IntegrityBlockedError("transaction parent digest missing")
            if parent.get("transaction_digest") is not None and parent.get("digest") is not None and parent.get("transaction_digest") != parent.get("digest"):
                raise IntegrityBlockedError("transaction parent digest fields disagree")
            parent_transaction = self._load_transaction(parent_digest)
            if current.get("graph_delta") != self._graph_delta(parent_transaction.get("state", {}), current.get("state", {})):
                raise IntegrityBlockedError("transaction graph delta mismatch")
            current = parent_transaction
            expected_revision -= 1
        state = _copy(transaction["state"])
        self._validate_state(state)
        section = state.get("metadata", {}).get("section_control")
        if (
            isinstance(section, Mapping)
            and section.get("transition")
            == {"intent": "source-transition-fixture-passed", "state": "accepted"}
        ):
            self._validate_accepted_section_receipt_provenance(
                state,
                section["accepted_section_receipt"],
                transactions,
                label="current accepted section receipt",
                section_id=section["section_id"],
                group_id=state["group"]["id"],
                bundle_ref=state["group"]["bundle_ref"],
                checkpoint_ref=state["group"]["checkpoint_ref"],
            )
        if isinstance(section, Mapping) and "parent_accepted_section_receipt" in section:
            self._validate_parent_accepted_section_receipt_provenance(state, transactions)
        if state["revision"] != head["revision"]:
            raise IntegrityBlockedError("HEAD/state revision mismatch")
        if transaction.get("object_refs") != state.get("object_refs"):
            raise IntegrityBlockedError("transaction/object reference catalog mismatch")
        if transaction.get("workflow_version") != head.get("workflow_version"):
            raise IntegrityBlockedError("HEAD/transaction workflow mismatch")
        return state, head

    def verify_integrity(self) -> Dict[str, Any]:
        try:
            with self._lock():
                state, head = self._load_current()
                return {"ok": True, "revision": state["revision"], "head": _copy(head)}
        except (IntegrityBlockedError, DAGCycleError) as exc:
            self._record_integrity_block(str(exc))
            raise IntegrityBlockedError(str(exc)) from exc

    def _empty_state(
        self,
        objective_ref: Mapping[str, Any],
        workflow_version: str,
        group_id: str,
        epoch_id: str,
        authority_ref: Mapping[str, Any],
        aliases: Iterable[str],
        external_refs: Iterable[str], review_budget: Optional[Mapping[str, Any]] = None,
    ) -> Dict[str, Any]:
        initial_epoch = {
            "id": epoch_id,
            "status": "open",
            "group_id": group_id,
            "boundary_reason": "entry",
            "started_at_revision": 1,
            "input_ref": None,
        }
        state = {
            "schema": SCHEMA_STATE,
            "run_id": self.run_id,
            "workflow_version": workflow_version,
            "graph_version": GRAPH_VERSION,
            "kernel_version": KERNEL_VERSION,
            "revision": 1,
            "state_revision": 1,
            "graph_revision": 1,
            "status": "active",
            "objective_ref": _copy(objective_ref),
            "objective_history": [],
            "objective_approvals": {},
            "objective_events": [],
            "authority": _copy(authority_ref),
            "group": {"id": group_id, "status": "open", "next_group": None},
            "epoch": {
                "id": epoch_id,
                "status": "open",
                "group_id": group_id,
                "boundary_reason": "entry",
                "clear_before_next": False,
            },
            "epoch_contexts": {epoch_id: initial_epoch},
            "context_budget": dict(BUDGET_POLICY, token_status="unavailable", token_count=None),
            "review_budget": _copy(review_budget or {"version": "v1", "deadline": "9999-12-31T23:59:59+00:00", "max_rounds": 10, "max_attempts_per_finding": 2, "rounds_used": 0, "finding_attempts": {}}),
            "budget_terminal": None,
            "terminal_history": [],
            "object_refs": {},
            "artifacts": {},
            "tasks": {},
            "reviews": {},
            "findings": {},
            "verdicts": {},
            "finding_validations": {},
            "edges": [],
            "leases": {},
            "idempotency": {},
            "nodes": {},
            "metadata": {"aliases": list(aliases), "external_refs": list(external_refs)},
        }
        fixture_identity = authority_ref.get("fixture_identity")
        if fixture_identity is not None:
            expected_fixture = {
                "schema": "canonical-fixture-identity/v1", "run_id": self.run_id,
                "namespace": fixture_identity.get("namespace") if isinstance(fixture_identity, Mapping) else None,
                "approval_scope": "fixture-only",
            }
            if (not isinstance(fixture_identity, Mapping) or dict(fixture_identity) != expected_fixture
                    or not isinstance(expected_fixture["namespace"], str)
                    or re.fullmatch(r"fixture:[A-Za-z0-9][A-Za-z0-9_.:@/-]*", expected_fixture["namespace"]) is None):
                raise AuthorizationError("entry fixture identity is malformed")
            state["metadata"]["fixture_identity"] = _copy(fixture_identity)
        return state

    def _authority_ok(
        self,
        authority: Mapping[str, Any],
        operation: str,
        scope: Optional[str] = None,
        protected_fields: Optional[Sequence[str]] = None,
    ) -> None:
        if not isinstance(authority, Mapping):
            raise AuthorizationError("authority_ref is required")
        bound_run_id = authority.get("run_id") or authority.get("bound_run_id")
        if bound_run_id is not None and bound_run_id != self.run_id:
            raise AuthorizationError("authority is bound to another Run")
        if bound_run_id is not None and (not isinstance(bound_run_id, str) or not _ID.fullmatch(bound_run_id)):
            raise AuthorizationError("authority Run ID is malformed")
        for expiry_key in ("expires_at", "valid_until", "expiry"):
            expiry = authority.get(expiry_key)
            if expiry is None:
                continue
            if not isinstance(expiry, str):
                raise AuthorizationError("authority expiry is malformed")
            try:
                parsed = _datetime.datetime.fromisoformat(expiry.replace("Z", "+00:00"))
            except ValueError as exc:
                raise AuthorizationError("authority expiry is malformed") from exc
            if parsed.tzinfo is None:
                parsed = parsed.replace(tzinfo=_datetime.timezone.utc)
            if parsed <= _datetime.datetime.now(_datetime.timezone.utc):
                raise AuthorizationError("authority approval is expired")
        status = authority.get("status")
        approved = authority.get("approved")
        approved_value = approved is True or (isinstance(approved, list) and bool(approved))
        if status is not None and status not in ("approved", "authorized", "granted"):
            raise AuthorizationError("authority is not approved")
        if status is None and not approved_value:
            raise AuthorizationError("authority is not approved")
        scopes = authority.get("scopes") or authority.get("approved_scopes")
        if not isinstance(scopes, list) or not scopes or any(not isinstance(item, str) or not item for item in scopes) or len(set(scopes)) != len(scopes):
            raise AuthorizationError("authority scopes are required")
        if "*" not in scopes:
            allowed = {str(item) for item in scopes}
            if operation not in allowed and (scope is None or scope not in allowed):
                raise AuthorizationError("authority does not cover %s" % operation)
        for key in ("proposal_digest",):
            if authority.get(key) is not None and (not isinstance(authority[key], str) or not _DIGEST.fullmatch(authority[key])):
                raise AuthorizationError("authority %s is malformed" % key)
        expected_head = authority.get("expected_head")
        if expected_head is not None:
            if not isinstance(expected_head, Mapping) or set(expected_head) - {"revision", "transaction_digest", "digest"} or "revision" not in expected_head or "transaction_digest" not in expected_head:
                raise AuthorizationError("authority expected HEAD is malformed")
            if not isinstance(expected_head["revision"], int) or isinstance(expected_head["revision"], bool) or expected_head["revision"] < 0:
                raise AuthorizationError("authority expected HEAD revision is malformed")
            for key in ("transaction_digest", "digest"):
                value = expected_head.get(key)
                if value is not None and (not isinstance(value, str) or not _DIGEST.fullmatch(value)):
                    raise AuthorizationError("authority expected HEAD digest is malformed")
            if expected_head.get("transaction_digest") is not None and expected_head.get("digest") is not None and expected_head["transaction_digest"] != expected_head["digest"]:
                raise AuthorizationError("authority expected HEAD digest fields disagree")
        expected_revision = authority.get("expected_head_revision")
        if expected_revision is not None and (not isinstance(expected_revision, int) or isinstance(expected_revision, bool) or expected_revision < 0):
            raise AuthorizationError("authority expected_head_revision is malformed")
        if protected_fields:
            receipt_fields = authority.get("protected_fields") or []
            if (
                not isinstance(receipt_fields, list)
                or not set(protected_fields).issubset(set(receipt_fields))
                or not (authority.get("human_receipt") or authority.get("approval_ref"))
            ):
                raise ProtectedFieldError("protected fields require a matching approval receipt")

    def _validate_reopen_receipt(self, command: Mapping[str, Any], head: Mapping[str, Any], state: Mapping[str, Any]) -> None:
        payload = command.get("payload") or {}
        receipt = (command.get("authority_ref") or {}).get("approval_receipt")
        required = {"approval_id", "operation", "run_id", "replacement_budget", "replacement_budget_digest", "expected_head"}
        if not isinstance(receipt, Mapping) or set(receipt) != required:
            raise AuthorizationError("reopen_review requires an exact approval receipt")
        budget = payload.get("replacement_budget")
        bound_head = {"revision": head.get("revision"), "transaction_digest": head.get("transaction_digest")}
        payload_head = payload.get("expected_head") or {}
        payload_binding = {"revision": payload_head.get("revision"), "transaction_digest": payload_head.get("transaction_digest") or payload_head.get("digest")}
        if (not isinstance(payload.get("approval_id"), str) or not _ID.fullmatch(payload["approval_id"])
                or receipt.get("approval_id") != payload["approval_id"]
                or receipt.get("operation") != "reopen_review" or receipt.get("run_id") != self.run_id
                or receipt.get("replacement_budget") != budget or receipt.get("replacement_budget_digest") != _digest(budget)
                or receipt.get("expected_head") != bound_head or payload_binding != bound_head):
            raise AuthorizationError("reopen approval receipt does not bind this operation")
        if any(item.get("approval_id") == payload["approval_id"] for item in (state.get("metadata") or {}).get("reopen_history", [])):
            raise AuthorizationError("reopen approval receipt was already consumed")

    def _validate_live_task_authority(
        self,
        state: Mapping[str, Any],
        task: Mapping[str, Any],
        *,
        operation: str = "claim_task",
        current_head: Optional[Mapping[str, Any]] = None,
    ) -> None:
        """Validate the live authority object before readiness or claiming.

        A command's authority is copied into the Task record at publication;
        readiness must still check the current Run authority and the copied
        object.  This prevents an ``authorizes`` graph edge from becoming a
        capability by itself after expiry, scope narrowing, proposal change,
        or an expected-HEAD mismatch.
        """

        task_authority = task.get("authority_ref")
        if not isinstance(task_authority, Mapping) or not task_authority:
            raise AuthorizationError("Task has no live authority object")
        authority_digest = task.get("authority_digest")
        if not isinstance(authority_digest, str) or _digest(task_authority) != authority_digest:
            raise IntegrityBlockedError("Task authority object digest is inconsistent")
        task_scope = self._scope_paths(task.get("write_scope"), "task.write_scope")
        current_authority = state.get("authority")
        if not isinstance(current_authority, Mapping):
            raise AuthorizationError("Run has no live authority object")
        self._authority_ok(current_authority, operation)
        self._authority_ok(task_authority, operation)
        # A broad operation grant such as the historical ``scopes: [\"*\"]``
        # is not by itself a filesystem capability.  If the live Run
        # authority declares concrete write scopes, enforce them; otherwise
        # the immutable Task authority remains the approved scope binding.
        if current_authority.get("write_scopes") is not None or current_authority.get("allowed_paths") is not None:
            self._validate_scope(task_scope, current_authority, label="live authority task.write_scope")
        self._validate_scope(task_scope, task_authority, label="Task authority task.write_scope")
        for authority in (current_authority, task_authority):
            proposal = authority.get("proposal_digest")
            if proposal is not None and proposal != task.get("proposal_digest"):
                raise AuthorizationError("live authority proposal does not match Task")
            expected = authority.get("expected_head")
            if expected is None and authority.get("expected_head_revision") is None:
                continue
            if expected is not None:
                if (
                    not isinstance(expected, Mapping)
                    or not isinstance(expected.get("revision"), int)
                    or isinstance(expected.get("revision"), bool)
                ):
                    raise StaleHeadError("live authority expected HEAD is malformed")
                expected_digest = expected.get("transaction_digest") or expected.get("digest")
                if not isinstance(expected_digest, str) or not _DIGEST.fullmatch(expected_digest):
                    raise StaleHeadError("live authority expected HEAD digest is malformed")
            task_expected = task.get("authority_expected_head")
            if expected is not None and task_expected != expected:
                raise StaleHeadError("live authority expected HEAD does not match Task")
            if authority.get("expected_head_revision") is not None:
                expected_revision = authority.get("expected_head_revision")
                if not isinstance(expected_revision, int) or isinstance(expected_revision, bool):
                    raise StaleHeadError("live authority expected HEAD revision is malformed")
                if task.get("authority_expected_head_revision") != expected_revision:
                    raise StaleHeadError("live authority expected HEAD revision does not match Task")
            # The copied Task authority is not a current capability.  When an
            # authority carries an expected HEAD, compare both revision and
            # digest with the lock-held canonical snapshot before routing or
            # claiming.  This closes the gap where a task retained a valid
            # historical authority object after another command advanced HEAD.
            if current_head is not None:
                if expected is not None:
                    expected_digest = expected.get("transaction_digest") or expected.get("digest")
                    current_digest = current_head.get("transaction_digest") or current_head.get("digest")
                    if (
                        expected.get("revision") != current_head.get("revision")
                        or expected_digest != current_digest
                    ):
                        raise StaleHeadError("live authority expected HEAD is stale")
                if authority.get("expected_head_revision") is not None and authority.get("expected_head_revision") != current_head.get("revision"):
                    raise StaleHeadError("live authority expected HEAD revision is stale")
        freshness = task.get("freshness") or {}
        expected = task.get("authority_expected_head")
        if expected is not None:
            if not isinstance(expected, Mapping) or expected.get("revision") != freshness.get("created_at_revision", 0) - 1:
                raise StaleHeadError("Task authority expected HEAD is not bound to its publication")
            has_receipt = bool(task_authority.get("human_receipt") or task_authority.get("approval_ref"))
            if not has_receipt:
                raise ProtectedFieldError("protected fields require a matching approval receipt")

    @staticmethod
    def _authority_assignment_matches(command: Mapping[str, Any]) -> None:
        """Apply the actor/assignment binding even to the genesis command."""

        actor = command.get("actor")
        authority = command.get("authority_ref")
        if not isinstance(actor, Mapping) or not isinstance(authority, Mapping):
            raise AuthorizationError("actor and authority are required")
        bound_assignment = authority.get("assignment_id") or authority.get("bound_assignment_id")
        if bound_assignment is not None and bound_assignment != actor.get("assignment_id"):
            raise AuthorizationError("authority assignment does not match actor")

    @staticmethod
    def _scope_paths(value: Any, label: str = "scope") -> List[str]:
        """Normalize path-like write scope declarations.

        Scope is deliberately represented as a small list of relative paths.
        Mapping forms are accepted for package ergonomics, but are reduced to
        their path member before validation; no path may escape the managed
        workspace.
        """

        if value is None:
            return []
        if isinstance(value, str):
            values = [value]
        elif isinstance(value, Mapping):
            values = [value]
        elif isinstance(value, Sequence) and not isinstance(value, (bytes, bytearray, str)):
            values = list(value)
        else:
            raise AuthorizationError("%s must be a path or list of paths" % label)
        paths: List[str] = []
        for item in values:
            if isinstance(item, Mapping):
                item = item.get("path") or item.get("target")
            if not isinstance(item, str) or not item or "\x00" in item:
                raise AuthorizationError("%s contains an invalid path" % label)
            # Path() on POSIX does not consider a Windows drive absolute and
            # silently normalizes several ambiguous forms.  The durable
            # package contract is a relative, traversal-safe POSIX path;
            # reject those forms before Path() gets a chance to reinterpret
            # them.  Wildcards are allowed only as explicit scope patterns.
            if "\\" in item or re.match(r"^[A-Za-z]:[\\/]", item):
                raise AuthorizationError("%s contains a non-portable path: %s" % (label, item))
            if item != "*" and any(part in ("", ".", "..") for part in item.split("/")):
                raise AuthorizationError("%s contains a traversal or empty segment: %s" % (label, item))
            path = Path(item)
            if path.is_absolute() or path == Path(".") or ".." in path.parts:
                raise AuthorizationError("%s escapes its assigned scope: %s" % (label, item))
            if "*" in item and item != "*" and not item.endswith("/*"):
                raise AuthorizationError("%s contains an unsupported wildcard pattern: %s" % (label, item))
            paths.append(item)
        return paths

    def _validate_scope(
        self,
        paths: Any,
        authority: Mapping[str, Any],
        *,
        label: str = "write_scope",
    ) -> None:
        """Reject scope escapes and authority scopes that do not cover them."""

        requested = self._scope_paths(paths, label)
        if not requested:
            return
        if "write_scopes" in authority:
            declared = authority.get("write_scopes")
        elif "allowed_paths" in authority:
            declared = authority.get("allowed_paths")
        else:
            declared = None
        if declared is None:
            # An operation grant is not a filesystem grant.  A command that
            # carries a non-empty path scope must carry the corresponding
            # exact authority scope as well; otherwise any relative path
            # would be writable merely because it passed traversal checks.
            raise AuthorizationError("authority must declare an exact write scope")
        allowed = self._scope_paths(declared, "authority.write_scopes")
        if "*" in allowed:
            return
        for path in requested:
            if not any(self._scope_contains(prefix, path) for prefix in allowed):
                raise AuthorizationError("authority does not cover write scope: %s" % path)

    def _validate_task_output_scope(self, package: Mapping[str, Any], authority: Mapping[str, Any]) -> None:
        """Bind both the declared Task scope and its concrete output path.

        A package may declare a permitted directory and then point its output
        at a sibling path.  Treating ``write_scope`` as the only authority
        check would make that second path an unreviewed write capability.
        Validate the output against the authority and, when present, against
        the package's own narrower scope as well.
        """

        if not isinstance(package, Mapping):
            raise AuthorizationError("task package is required for scope validation")
        task_paths = self._scope_paths(package.get("write_scope"), "task.write_scope")
        self._validate_scope(task_paths, authority, label="task.write_scope")
        output_path = package.get("output_path")
        if output_path is None:
            return
        if isinstance(output_path, str) and "*" in output_path:
            raise AuthorizationError("task.output_path must name a concrete path")
        output_paths = self._scope_paths(output_path, "task.output_path")
        self._validate_scope(output_paths, authority, label="task.output_path")
        if task_paths and any(
            not any(self._scope_contains(prefix, path) for prefix in task_paths)
            for path in output_paths
        ):
            raise AuthorizationError("task output_path is outside task.write_scope")

    def _result_scope_paths(self, result: Mapping[str, Any]) -> List[str]:
        paths: List[str] = []
        for key in ("write_paths", "changed_paths", "output_paths"):
            if key in result:
                paths.extend(self._scope_paths(result.get(key), "result.%s" % key))
        outputs = result.get("outputs")
        if outputs is not None:
            paths.extend(self._scope_paths(outputs, "result.outputs"))
        return paths

    @staticmethod
    def _ensure_durable_payload(
        value: Any,
        label: str = "payload",
        _allow_reference_paths: bool = False,
        _allow_context_telemetry: bool = False,
    ) -> None:
        """Reject data that must remain outside durable Task/Review objects."""

        if isinstance(value, Mapping):
            for key, child in value.items():
                raw_key = str(key)
                normalized = re.sub(r"[^a-z0-9]", "", raw_key.lower())
                # Collection members are opaque identifiers, not field names.
                # A legitimate Task ID such as ``task:authority-bound`` must
                # not be mistaken for a credential key while the fields of
                # the record below it remain fully checked.
                identifier_map = label in {
                    "state.nodes", "state.object_refs", "state.artifacts",
                    "state.tasks", "state.reviews", "state.findings",
                    "state.verdicts", "state.leases", "state.idempotency",
                    "state.epoch_contexts",
                }
                # Token counters/status are not generally safe merely because
                # their spelling looks like telemetry.  They are allowed only
                # as the two schema-owned fields directly under the exact
                # ``context_budget`` object; every compound/suffix variant is
                # rejected.  This keeps a payload such as
                # ``foo_token_count`` from becoming an opaque durable channel.
                telemetry_key = _allow_context_telemetry and normalized in {
                    "tokenstatus", "tokencount"
                }
                if not identifier_map and ControlKernel._is_forbidden_durable_key(normalized) and not telemetry_key:
                    raise AuthorizationError("%s contains non-durable field: %s" % (label, key))
                child_allows_reference_path = normalized in {
                    "path", "filepath", "filename", "sourcepath", "objectpath",
                    "kernelpath", "targetpath", "outputpath", "artifactpath",
                    "writescope", "outputpaths", "writepaths", "changedpaths",
                }
                child_allows_context_telemetry = normalized == "contextbudget"
                if child_allows_context_telemetry:
                    # Token-bearing fields are a narrow schema-owned
                    # exception, not a general-purpose durable channel.  An
                    # exact budget object is required even when it appears in
                    # an opaque payload (for example a published Artifact),
                    # so ``{"context_budget": {"token_count": ...}}``
                    # cannot bypass the 200K/300K/500K contract.
                    try:
                        ControlKernel._validate_budget(child)
                    except BudgetError:
                        # Preserve the typed boundary error used by close
                        # operations and callers that need to distinguish a
                        # budget split from a durable-secret rejection.
                        raise
                    except KernelError as exc:
                        raise AuthorizationError(
                            "%s.context_budget is not schema-owned telemetry" % label
                        ) from exc
                ControlKernel._ensure_durable_payload(
                    child,
                    "%s.%s" % (label, key),
                    _allow_reference_paths=child_allows_reference_path,
                    _allow_context_telemetry=child_allows_context_telemetry,
                )
        elif isinstance(value, (list, tuple)):
            for index, child in enumerate(value):
                ControlKernel._ensure_durable_payload(
                    child,
                    "%s[%s]" % (label, index),
                    _allow_reference_paths=_allow_reference_paths,
                    _allow_context_telemetry=_allow_context_telemetry,
                )
        elif isinstance(value, (set, frozenset)):
            raise AuthorizationError("%s contains a non-JSON-serializable collection" % label)
        elif isinstance(value, str):
            lowered = value.lower()
            # Source/provenance paths are safe references even when a
            # historical filename contains words such as ``authorization``;
            # opaque prose carrying those markers remains prohibited.
            looks_like_reference_path = _allow_reference_paths and bool(
                re.fullmatch(r"[a-z0-9_.:@/-]+\.(?:json|ya?ml|md|markdown|toml|txt)", lowered)
            )
            if not looks_like_reference_path and (
                any(marker in lowered for marker in _DURABLE_FORBIDDEN_TEXT_MARKERS)
                or _DURABLE_FORBIDDEN_TEXT_RE.search(value) is not None
            ):
                raise AuthorizationError("%s contains non-durable opaque text" % label)

    @staticmethod
    def _is_forbidden_durable_key(normalized: str) -> bool:
        """Recognize credential/secret variants conservatively.

        This helper intentionally has no global telemetry allow-list.  A
        telemetry spelling is safe only when the caller is traversing the
        schema-owned ``context_budget`` object (see
        :meth:`_ensure_durable_payload`).
        """

        if normalized in _DURABLE_FORBIDDEN_KEYS or normalized in {"access", "authentication"}:
            return True
        prefixes = (
            "secret", "credential", "password", "passwd", "passphrase",
            "authorization", "apikey", "accesskey", "accesstoken",
            "authentication", "authheader", "authtoken", "authkey", "authsecret",
            "authcredential", "privatekey", "cookie", "cookies", "session",
            "bearer", "bearertoken", "refresh", "refreshtoken", "sessiontoken",
            "privatecontext", "privatereasoning", "modelthought", "chainofthought",
            "transcript", "fulltranscript", "rawoutput", "rawtooloutput",
            "tooloutput", "toolresult",
        )
        if normalized.startswith(prefixes) or normalized.endswith(prefixes):
            return True
        if any(fragment in normalized for fragment in (
            "secret", "credential", "password", "passwd", "passphrase",
            "authorization", "accesskey", "accesstoken", "authtoken", "privatekey",
            "cookie", "session", "bearer", "refreshtoken", "sessiontoken",
            "privatecontext", "privatereasoning", "modelthought", "chainofthought",
            "transcript", "rawoutput", "rawtooloutput", "tooloutput", "toolresult",
        )):
            return True
        # ``authority`` and its structured provenance members are legitimate
        # durable control metadata, while arbitrary ``foo_auth_value`` keys
        # are not.  Check the normalized spelling after the known safe
        # authority prefix has been excluded; this preserves the conservative
        # compound-auth deny policy without rejecting ``authority_ref``.
        if "auth" in normalized and not normalized.startswith(("author", "authority")):
            return True
        if "access" in normalized:
            return True
        # Every token-bearing key is non-durable unless it was explicitly
        # admitted by the parent schema-aware traversal above.  This includes
        # suffix and compound spellings (foo_token_count, foo_tokencount,
        # token_value, and punctuation/case variants).
        return "token" in normalized

    def _make_command(
        self,
        operation: str,
        actor_role: str = "orchestrator",
        assignment_id: str = "orchestrator",
        authority_ref: Optional[Mapping[str, Any]] = None,
        input_refs: Optional[Sequence[Mapping[str, Any]]] = None,
        idempotency_key: Optional[str] = None,
        payload: Optional[Mapping[str, Any]] = None,
        protected_fields: Optional[Sequence[str]] = None,
        expected_head: Optional[Mapping[str, Any]] = None,
        scope: Optional[Sequence[str]] = None,
    ) -> Dict[str, Any]:
        if self.run_id is None:
            raise KernelError("entry must establish a run_id first")
        current_head = expected_head
        if current_head is None and self.head_path.exists():
            with self._lock():
                current_head = self._load_head()
        expected = None
        if current_head is not None:
            expected = {
                "revision": current_head["revision"],
                "transaction_digest": current_head["transaction_digest"],
                # ``digest`` is the catalog's short spelling; retain the
                # explicit transaction_digest field for unambiguous storage.
                "digest": current_head["transaction_digest"],
            }
        else:
            expected = {"revision": 0, "transaction_digest": None}
        return {
            "schema": SCHEMA_COMMAND,
            "command_id": "cmd-" + uuid.uuid4().hex,
            "command_type": operation,
            "run_id": self.run_id,
            "expected_head": expected,
            "workflow_version": None,
            "graph_version": GRAPH_VERSION,
            "actor": {"role": actor_role, "assignment_id": assignment_id},
            "authority_ref": _copy(authority_ref),
            "input_refs": [_copy(item) for item in (input_refs or [])],
            "idempotency_key": idempotency_key or "idem-" + uuid.uuid4().hex,
            "protected_fields": list(protected_fields or []),
            "scope": list(scope or []),
            "payload": _copy(payload or {}),
        }

    def make_command(self, operation: str, payload: Optional[Mapping[str, Any]] = None, **kwargs: Any) -> Dict[str, Any]:
        """Public command-envelope builder for callers that want to inspect
        or hand an envelope to an Orchestrator explicitly."""

        return self._command_for(operation, payload or {}, **kwargs)

    build_command = make_command

    def _command_digest(self, command: Mapping[str, Any]) -> str:
        return _digest(command)

    def _idempotency_digest(self, command: Mapping[str, Any]) -> str:
        # command_id is a tracing identifier, not the operation's identity;
        # a retry commonly receives a new command_id with the same key.
        value = _copy(command)
        value.pop("command_id", None)
        # A retry may be assembled after another reader observes the new
        # HEAD.  The idempotency key binds the requested mutation, while the
        # expected HEAD remains a separate CAS guard for first publication.
        value.pop("expected_head", None)
        return _digest(value)

    def _normalize_command(self, command: Mapping[str, Any]) -> Dict[str, Any]:
        """Copy a command without inventing any required envelope field.

        Earlier A6R revisions accepted flat aliases and filled arrays and
        version fields.  That makes a malformed command indistinguishable
        from a complete one at the persistence boundary.  v1 is deliberately
        boring: callers must send the complete nested envelope and the shape
        validator is the first consumer of it.
        """

        if not isinstance(command, Mapping):
            raise CommandValidationError("command envelope must be an object")
        return _copy(command)

    @staticmethod
    def _validate_command_ref_shape(value: Any, label: str) -> None:
        if not isinstance(value, Mapping):
            raise CommandValidationError("%s must be an object" % label)
        allowed = {"kind", "digest", "object_digest", "external", "node_id", "artifact_id", "task_id", "object_type", "path"}
        unknown = sorted(set(value) - allowed)
        if unknown:
            raise CommandValidationError("%s has unsupported fields: %s" % (label, ", ".join(unknown)))
        if "digest" not in value:
            raise CommandValidationError("%s.digest is required" % label)
        digest = value.get("digest")
        if not isinstance(digest, str) or not _DIGEST.fullmatch(digest):
            raise CommandValidationError("%s.digest must be a sha256: digest" % label)
        object_digest = value.get("object_digest")
        if object_digest is not None and (not isinstance(object_digest, str) or not _DIGEST.fullmatch(object_digest)):
            raise CommandValidationError("%s.object_digest must be a sha256: digest" % label)
        if object_digest is not None and object_digest != digest:
            raise CommandValidationError("%s digest fields disagree" % label)
        if "external" in value and not isinstance(value["external"], bool):
            raise CommandValidationError("%s.external must be boolean" % label)
        if "kind" in value and (not isinstance(value["kind"], str) or not value["kind"]):
            raise CommandValidationError("%s.kind is malformed" % label)
        for key in ("node_id", "artifact_id", "task_id"):
            if key in value and (not isinstance(value[key], str) or not _ID.fullmatch(value[key])):
                raise CommandValidationError("%s.%s is malformed" % (label, key))
        if "object_type" in value and (not isinstance(value["object_type"], str) or value["object_type"] not in _OBJECT_TYPES):
            raise CommandValidationError("%s.object_type is malformed" % label)
        if "path" in value:
            path = value["path"]
            if (
                not isinstance(path, str)
                or not path
                or "\x00" in path
                or "\\" in path
                or re.match(r"^[A-Za-z]:[\\/]", path)
                or Path(path).is_absolute()
                or any(part in ("", ".", "..") for part in path.split("/"))
                or "*" in path
            ):
                raise CommandValidationError("%s.path is malformed" % label)

    @staticmethod
    def _validate_task_package_shape(package: Any, label: str = "package") -> None:
        if not isinstance(package, Mapping):
            raise CommandValidationError("%s must be an object" % label)
        required = {
            "task_id", "attempt_id", "assignment", "input_refs", "write_scope",
            "acceptance", "freshness", "stop_conditions", "invalidated",
            "stop_requested", "output_path", "status",
        }
        unknown = sorted(set(package) - required)
        if unknown:
            raise CommandValidationError("%s has unsupported fields: %s" % (label, ", ".join(unknown)))
        missing = sorted(required - set(package))
        if missing:
            raise CommandValidationError("%s is missing: %s" % (label, ", ".join(missing)))
        for key in ("task_id", "attempt_id"):
            if not isinstance(package[key], str) or not _ID.fullmatch(package[key]):
                raise CommandValidationError("%s.%s is malformed" % (label, key))
        assignment = package["assignment"]
        if not isinstance(assignment, Mapping) or set(assignment) != {"role", "assignment_id"}:
            raise CommandValidationError("%s.assignment is incomplete" % label)
        if assignment.get("role") != "worker" or not isinstance(assignment.get("assignment_id"), str) or not _ID.fullmatch(assignment["assignment_id"]):
            raise CommandValidationError("%s.assignment is malformed" % label)
        if not isinstance(package["input_refs"], list):
            raise CommandValidationError("%s.input_refs must be an array" % label)
        for index, ref in enumerate(package["input_refs"]):
            ControlKernel._validate_command_ref_shape(ref, "%s.input_refs[%s]" % (label, index))
        if not isinstance(package["write_scope"], list):
            raise CommandValidationError("%s.write_scope must be an array" % label)
        try:
            ControlKernel._scope_paths(package["write_scope"], "%s.write_scope" % label)
        except KernelError as exc:
            raise CommandValidationError(str(exc)) from exc
        for key in ("acceptance", "stop_conditions"):
            if not isinstance(package[key], list):
                raise CommandValidationError("%s.%s must be an array" % (label, key))
        freshness = package["freshness"]
        if not isinstance(freshness, Mapping):
            raise CommandValidationError("%s.freshness must be an object" % label)
        freshness_allowed = {"epoch_id", "created_at_revision", "stale", "invalidated"}
        if sorted(set(freshness) - freshness_allowed):
            raise CommandValidationError("%s.freshness has unsupported fields" % label)
        if not isinstance(freshness.get("epoch_id"), str) or not _ID.fullmatch(freshness["epoch_id"]):
            raise CommandValidationError("%s.freshness.epoch_id is malformed" % label)
        if not isinstance(freshness.get("created_at_revision"), int) or isinstance(freshness.get("created_at_revision"), bool) or freshness["created_at_revision"] < 1:
            raise CommandValidationError("%s.freshness.created_at_revision is malformed" % label)
        for key in ("stale", "invalidated"):
            if key in freshness and not isinstance(freshness[key], bool):
                raise CommandValidationError("%s.freshness.%s must be boolean" % (label, key))
        for key in ("invalidated", "stop_requested"):
            if not isinstance(package[key], bool):
                raise CommandValidationError("%s.%s must be boolean" % (label, key))
        if package["output_path"] is not None:
            if not isinstance(package["output_path"], str):
                raise CommandValidationError("%s.output_path must be a string or null" % label)
            if "*" in package["output_path"]:
                raise CommandValidationError("%s.output_path must name a concrete path" % label)
            try:
                ControlKernel._scope_paths(package["output_path"], "%s.output_path" % label)
            except KernelError as exc:
                raise CommandValidationError(str(exc)) from exc
        if package["status"] != "ready":
            raise CommandValidationError("%s.status must be ready" % label)

    @staticmethod
    def _validate_finding_input_shape(value: Any, label: str) -> None:
        if not isinstance(value, Mapping):
            raise CommandValidationError("%s must be an object" % label)
        # A review command carries the same complete Finding input that will
        # be emitted into a Review package.  The public convenience wrapper
        # supplies the deterministic defaults, but a caller handing the
        # kernel a command directly may not rely on reducer-side synthesis.
        required = {
            "finding_id", "fingerprint", "requirement_ref", "description",
            "evidence", "severity", "blocking", "state",
        }
        # The nested Finding schema is closed.  Do not accept convenience-only
        # provenance members here and then silently discard them while
        # constructing the immutable Review package; callers that need an
        # evidence object must bind it through the Review package's own
        # evidence_refs field.
        allowed = required
        unknown = sorted(set(value) - allowed)
        if unknown:
            raise CommandValidationError("%s has unsupported fields: %s" % (label, ", ".join(unknown)))
        missing = sorted(required - set(value))
        if missing:
            raise CommandValidationError("%s is missing: %s" % (label, ", ".join(missing)))
        if not isinstance(value["finding_id"], str) or not _ID.fullmatch(value["finding_id"]):
            raise CommandValidationError("%s.finding_id is malformed" % label)
        if not isinstance(value["description"], str) or not value["description"]:
            raise CommandValidationError("%s.description is required" % label)
        if not isinstance(value["severity"], str) or not value["severity"]:
            raise CommandValidationError("%s.severity is required" % label)
        if (
            not isinstance(value["requirement_ref"], (str, Mapping, list, tuple, int, float, bool))
            and value["requirement_ref"] is not None
        ):
            raise CommandValidationError("%s.requirement_ref is malformed" % label)
        if not isinstance(value["blocking"], bool):
            raise CommandValidationError("%s.blocking must be boolean" % label)
        if not isinstance(value["fingerprint"], str) or not _DIGEST.fullmatch(value["fingerprint"]):
            raise CommandValidationError("%s.fingerprint is malformed" % label)
        try:
            expected_fingerprint = _digest({
                "requirement_ref": value["requirement_ref"],
                "description": value["description"],
                "severity": value["severity"],
            })
        except (TypeError, ValueError) as exc:
            raise CommandValidationError("%s stable fields are not JSON-compatible" % label) from exc
        if value["fingerprint"] != expected_fingerprint:
            raise CommandValidationError("%s.fingerprint does not match stable fields" % label)
        if not isinstance(value["evidence"], list):
            raise CommandValidationError("%s.evidence must be an array" % label)
        if "evidence_refs" in value:
            if not isinstance(value["evidence_refs"], list):
                raise CommandValidationError("%s.evidence_refs must be an array" % label)
            for index, ref in enumerate(value["evidence_refs"]):
                ControlKernel._validate_command_ref_shape(ref, "%s.evidence_refs[%s]" % (label, index))
        if "owner" in value and (not isinstance(value["owner"], str) or not _ID.fullmatch(value["owner"])):
            raise CommandValidationError("%s.owner is malformed" % label)
        if value["state"] != "open":
            raise CommandValidationError("%s.state must be the canonical open input state" % label)

    @staticmethod
    def _validate_legacy_source_binding(
        source_run: Mapping[str, Any],
        source_bundle: Mapping[str, Any],
        source_report: Mapping[str, Any],
        bindings: Mapping[str, Any],
        source_revision: int,
        error_type: Any = KernelError,
    ) -> None:
        """Validate the copied source identities again at the reducer boundary.

        ``LegacyConverter`` performs the path/digest reread, but the generic
        command API can also receive a migration payload directly.  Keep that
        path from publishing an arbitrary set of empty ``run``/``bundle`` /
        ``worker_report`` mappings by requiring the same source identity tuple
        that the converter records.  ``error_type`` lets a pre-persistence
        command reject with a validation error while a fresh-process state
        read reports an integrity block.
        """

        def fail(message: str) -> None:
            raise error_type(message)

        def require_id(value: Any, label: str) -> str:
            if not isinstance(value, str) or not _ID.fullmatch(value):
                fail("%s is missing or malformed" % label)
            return value

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

        source_run_id = require_id(source_run.get("run_id"), "legacy Run run_id")
        if source_run_id != bindings.get("source_run_id"):
            fail("legacy Run ID binding is invalid")
        bundle_run_id = require_id(source_bundle.get("run_id"), "legacy Bundle run_id")
        if bundle_run_id != source_run_id:
            fail("legacy Bundle Run ID binding is invalid")

        run_group = mapping(source_run, "current_group", "group")
        if run_group is None:
            fail("legacy Run Group identity is missing")
        group_id = require_id(run_group.get("id") or run_group.get("group_id"), "legacy Run Group id")
        if group_id != bindings.get("group_id"):
            fail("legacy Run Group binding is invalid")
        bundle_group = scalar(source_bundle, "group_id", "current_group", "group")
        if isinstance(bundle_group, Mapping):
            bundle_group = bundle_group.get("id") or bundle_group.get("group_id")
        if require_id(bundle_group, "legacy Bundle Group id") != group_id:
            fail("legacy Bundle Group binding is invalid")

        source_status = bindings.get("source_status")
        if not isinstance(source_status, str) or not source_status:
            fail("legacy source status binding is missing")
        # A6 Run files use ``current_group.status`` for the lifecycle/status
        # claim while their top-level ``status`` can describe the surrounding
        # workflow decision.  Prefer the explicit Group status when present;
        # only use the top-level value when that Group member is absent.
        run_status_claim = run_group.get("status")
        if run_status_claim is None:
            run_status_claim = source_run.get("status")
        if run_status_claim is not None and run_status_claim != source_status:
            fail("legacy Run source status binding is invalid")
        bundle_group_status = (
            bundle_group.get("status")
            if isinstance(bundle_group, Mapping)
            else source_bundle.get("group_status")
        )
        if bundle_group_status is not None and bundle_group_status != source_status:
            fail("legacy Bundle source status binding is invalid")
        if run_group.get("status") in ("open", "closed") and run_group.get("status") != bindings.get("run_status"):
            fail("legacy Run Group lifecycle binding is invalid")
        if bundle_group_status in ("open", "closed") and bundle_group_status != bindings.get("run_status"):
            fail("legacy Bundle Group lifecycle binding is invalid")

        run_epoch = mapping(source_run, "context_epoch", "current_epoch", "epoch")
        bundle_epoch = mapping(source_bundle, "context_epoch", "current_epoch", "epoch")
        if run_epoch is None or bundle_epoch is None:
            fail("legacy Run and Bundle Epoch identities are required")
        epoch_id = require_id(run_epoch.get("id") or run_epoch.get("epoch_id"), "legacy Run Epoch id")
        if epoch_id != bindings.get("epoch_id"):
            fail("legacy Run Epoch binding is invalid")
        if require_id(bundle_epoch.get("id") or bundle_epoch.get("epoch_id"), "legacy Bundle Epoch id") != epoch_id:
            fail("legacy Bundle Epoch binding is invalid")
        if run_epoch.get("status") is not None and run_epoch.get("status") != bindings.get("epoch_status"):
            fail("legacy Run Epoch status binding is invalid")
        if bundle_epoch.get("status") is not None and bundle_epoch.get("status") != bindings.get("epoch_status"):
            fail("legacy Bundle Epoch status binding is invalid")
        run_clear = scalar(run_epoch, "clear_before_next", "clear_before_start")
        bundle_clear = scalar(bundle_epoch, "clear_before_next", "clear_before_start")
        if run_clear is False or bundle_clear is False or (run_clear is not True and bundle_clear is not True):
            fail("legacy Epoch clear boundary binding is invalid")

        report_identity = source_report.get("run_id") or source_report.get("work_id")
        if report_identity is None:
            if source_report.get("context_epoch") != epoch_id or not isinstance(source_report.get("worker_id"), str) or not source_report.get("worker_id"):
                fail("legacy worker report Run identity is missing")
            report_identity = source_run_id
        if require_id(report_identity, "legacy worker report run_id/work_id") != source_run_id:
            fail("legacy worker report Run ID binding is invalid")
        report_group = source_report.get("group_id") or source_report.get("group")
        if isinstance(report_group, Mapping):
            report_group = report_group.get("id") or report_group.get("group_id")
        if report_group is not None and require_id(report_group, "legacy worker report Group id") != group_id:
            fail("legacy worker report Group binding is invalid")
        report_epoch = source_report.get("context_epoch") or source_report.get("epoch_id") or source_report.get("epoch")
        if isinstance(report_epoch, Mapping):
            report_epoch = report_epoch.get("id") or report_epoch.get("epoch_id")
        if report_epoch is not None and require_id(report_epoch, "legacy worker report Epoch id") != epoch_id:
            fail("legacy worker report Epoch binding is invalid")
        if source_report.get("status") not in ("done", "partial", "blocked"):
            fail("legacy worker report status is not a completed source report")

        def aliases_for(source: Mapping[str, Any]) -> Any:
            aliases = source.get("aliases")
            if aliases is None and isinstance(source.get("metadata"), Mapping):
                aliases = source["metadata"].get("aliases")
            return aliases

        aliases = None
        for source in (source_run, source_bundle, source_report):
            candidate = aliases_for(source)
            if candidate is not None:
                aliases = candidate
                break
        if not isinstance(aliases, list) or any(not isinstance(alias, str) or not alias for alias in aliases) or aliases != bindings.get("aliases"):
            fail("legacy source aliases binding is invalid")
        for source in (source_run, source_bundle, source_report):
            candidate = aliases_for(source)
            if candidate is not None and candidate != aliases:
                fail("legacy source aliases disagree")

        def revision_claims(source: Mapping[str, Any]) -> List[Any]:
            historical: List[Any] = []
            if source.get("source_state_revision") is not None:
                historical.append(source["source_state_revision"])
            correction = source.get("trajectory_correction")
            if isinstance(correction, Mapping):
                nested = correction.get("migration_source")
                if isinstance(nested, Mapping) and nested.get("source_state_revision") is not None:
                    historical.append(nested["source_state_revision"])
            for key in ("context_epoch", "current_epoch", "epoch"):
                nested = source.get(key)
                if isinstance(nested, Mapping):
                    value = nested.get("closed_at_revision") or nested.get("state_revision")
                    if value is not None:
                        historical.append(value)
            if historical:
                return historical
            return [source["state_revision"]] if source.get("state_revision") is not None else []

        def effective_revision_claims(source: Mapping[str, Any]) -> List[Any]:
            historical = revision_claims(source)
            if historical:
                return historical
            current = source.get("state_revision")
            return [current] if current is not None else []

        source_claims = effective_revision_claims(source_run) + effective_revision_claims(source_bundle)
        if not source_claims or any(not isinstance(value, int) or isinstance(value, bool) or value < 1 or value != source_revision for value in source_claims):
            fail("legacy Run/Bundle source revision binding is invalid")
        for value in (source_report.get("source_state_revision"), source_report.get("state_revision")):
            if value is not None and (not isinstance(value, int) or isinstance(value, bool) or value != source_revision):
                fail("legacy worker report source revision binding is invalid")

    @staticmethod
    def _validate_migration_attestation_shape(value: Any, label: str = "source_attestation") -> None:
        """Validate the physical source-byte attestation carried by migration."""

        required = {"run", "bundle", "worker_report"}
        if not isinstance(value, Mapping) or set(value) != required:
            raise CommandValidationError("%s must attest run, bundle, and worker_report" % label)
        for name in sorted(required):
            item = value.get(name)
            if not isinstance(item, Mapping) or set(item) != {"path", "raw_digest", "value_digest"}:
                raise CommandValidationError("%s.%s is incomplete" % (label, name))
            path = item.get("path")
            if (
                not isinstance(path, str)
                or not path
                or not Path(path).is_absolute()
                or "\x00" in path
                or "\\" in path
                or "//" in path
                or (path != "/" and path.endswith("/"))
                or any(part in ("", ".", "..") for part in path.split("/")[1:])
            ):
                raise CommandValidationError(
                    "%s.%s.path must be a canonical absolute physical path" % (label, name)
                )
            for key in ("raw_digest", "value_digest"):
                if not isinstance(item.get(key), str) or not _DIGEST.fullmatch(item[key]):
                    raise CommandValidationError("%s.%s.%s must be a SHA-256 digest" % (label, name, key))

    @staticmethod
    def _validate_migration_attestation(
        migration: Mapping[str, Any], *, verify_physical: bool = True
    ) -> None:
        """Bind migration mappings to copied source bytes before publication.

        The persisted ``value_digest`` is computed over the already-filtered
        copy that will become the immutable legacy object.  ``raw_digest`` is
        computed over the physical source bytes.  The latter cannot be
        replaced by a caller's mapping-only digest, which is the bypass this
        contract closes.  On a later history read we validate shape only; the
        immutable transaction retains the attestation even if a temporary
        source file is subsequently removed.
        """

        attestation = migration.get("source_attestation")
        ControlKernel._validate_migration_attestation_shape(attestation)
        persisted_source_digests = migration.get("source_digests")
        if (
            not isinstance(persisted_source_digests, Mapping)
            or set(persisted_source_digests) != {"run", "bundle", "worker_report"}
            or any(not isinstance(digest, str) or not _DIGEST.fullmatch(digest) for digest in persisted_source_digests.values())
        ):
            raise CommandValidationError("migration.source_digests is required for physical attestation binding")
        source_names = {"run": "run", "bundle": "bundle", "worker_report": "worker_report"}
        protected_roots = (
            Path(__file__).resolve().parents[3] / ".local" / "agent" / "runs",
            Path(__file__).resolve().parents[3] / ".local" / "agent" / "reports",
        )
        for name in sorted(source_names):
            item = attestation[name]
            if item.get("raw_digest") != persisted_source_digests.get(source_names[name]):
                raise CommandValidationError(
                    "value does not match physical source: source attestation raw digest does not match migration.source_digests.%s"
                    % name
                )
            persisted = migration.get(source_names[name])
            if not isinstance(persisted, Mapping):
                raise CommandValidationError("migration.%s is missing for attestation" % name)
            if canonical_digest(persisted) != item["value_digest"]:
                raise CommandValidationError("source attestation value digest does not match migration.%s" % name)
            path = Path(item["path"]).resolve()
            if path == Path(path.anchor) or any(path == root or root in path.parents for root in protected_roots):
                raise CommandValidationError("source attestation points at protected Run/report state")
            if verify_physical:
                try:
                    if not path.is_file() or _digest_bytes(path.read_bytes()) != item["raw_digest"]:
                        raise CommandValidationError("source attestation raw digest mismatch: %s" % name)
                    # A raw-byte digest alone would let a caller attest an
                    # unrelated file while supplying a different mapping in
                    # the command.  Parse the same copied fixture and apply
                    # the converter's durable-field projection, binding the
                    # attested bytes to the immutable value that will be
                    # stored.  Import lazily to avoid the migration module's
                    # intentional dependency on this kernel module.
                    from .migration import _load_copy, _safe_legacy_value

                    physical_value, _, physical_path = _load_copy(path)
                    if physical_path is None or canonical_digest(_safe_legacy_value(physical_value)) != item["value_digest"]:
                        raise CommandValidationError("source attestation value does not match physical source: %s" % name)
                except OSError as exc:
                    raise CommandValidationError("source attestation source is unreadable: %s" % name) from exc

    @staticmethod
    def _validate_objective_approval_payload(approval: Any) -> None:
        required = {
            "schema", "approval_id", "run_id", "namespace", "approval_scope",
            "decision", "actor", "receipt", "candidate_digest", "candidate_version",
            "prior_objective_digest", "prior_objective_version", "proposal_digest",
        }
        if not isinstance(approval, Mapping) or set(approval) != required:
            raise CommandValidationError("objective approval is incomplete")
        actor = approval.get("actor")
        receipt = approval.get("receipt")
        receipt_fields = {
            "schema", "receipt_id", "approval_id", "decision", "explicit", "source", "actor_id",
            "run_id", "namespace", "approval_scope", "candidate_path", "candidate_version",
            "candidate_namespace", "candidate_digest", "prior_objective_digest",
            "prior_objective_version", "proposal_digest", "issued_at",
        }
        if (approval.get("schema") != "objective-approval/v1"
                or approval.get("decision") != "approve"
                or approval.get("approval_scope") != "fixture-only"
                or not isinstance(approval.get("namespace"), str)
                or not approval["namespace"].startswith("fixture:")
                or not isinstance(actor, Mapping) or set(actor) != {"kind", "actor_id"}
                or actor.get("kind") != "human"
                or not isinstance(actor.get("actor_id"), str) or not _ID.fullmatch(actor["actor_id"])
                or not isinstance(receipt, Mapping) or set(receipt) != receipt_fields
                or receipt.get("schema") != "human-approval-receipt/v1"
                or receipt.get("decision") != "approve" or receipt.get("explicit") is not True
                or receipt.get("source") != "human"):
            raise AuthorizationError("explicit fixture-only human approval is required")
        for key in ("approval_id", "run_id", "candidate_version", "prior_objective_version"):
            if not isinstance(approval.get(key), str) or not _ID.fullmatch(approval[key]):
                raise CommandValidationError("objective approval.%s is malformed" % key)
        for key in ("candidate_digest", "proposal_digest"):
            if not isinstance(approval.get(key), str) or not _DIGEST.fullmatch(approval[key]):
                raise CommandValidationError("objective approval.%s is malformed" % key)
        if (not isinstance(approval.get("prior_objective_digest"), str)
                or re.fullmatch(r"(?:sha256:)?[0-9a-f]{64}", approval["prior_objective_digest"]) is None):
            raise CommandValidationError("objective approval.prior_objective_digest is malformed")
        if any(receipt.get(key) != approval.get(key) for key in (
            "approval_id", "run_id", "namespace", "approval_scope", "candidate_digest",
            "candidate_version", "prior_objective_digest", "prior_objective_version", "proposal_digest"
        )) or receipt.get("actor_id") != actor.get("actor_id"):
            raise AuthorizationError("human receipt does not bind the approval")
        if receipt.get("candidate_namespace") != approval.get("namespace"):
            raise AuthorizationError("human receipt does not bind the candidate namespace")
        if (not isinstance(receipt.get("receipt_id"), str) or not _ID.fullmatch(receipt["receipt_id"])
                or not isinstance(receipt.get("issued_at"), str) or not receipt["issued_at"]):
            raise CommandValidationError("human approval receipt is malformed")
        try:
            issued_at = _datetime.datetime.fromisoformat(receipt["issued_at"].replace("Z", "+00:00"))
        except ValueError as exc:
            raise CommandValidationError("human approval receipt time is malformed") from exc
        if issued_at.tzinfo is None:
            raise CommandValidationError("human approval receipt time is malformed")

    def _validate_payload_shape(self, operation: str, payload: Any, *, verify_physical_attestation: bool = True) -> None:
        if operation not in COMMAND_TYPES:
            raise CommandValidationError("unknown command type: %s" % operation)
        if not isinstance(payload, Mapping):
            raise CommandValidationError("command payload must be an object")
        fields = {
            "entry": {"objective_ref"},
            "approve_objective": {"candidate_ref", "prior_objective", "proposal_digest", "approval"},
            "publish_artifact": {"artifact_id", "version", "value", "kind", "path"},
            "publish_task_package": {"task_id", "package", "assignment", "input_refs", "sibling_group"},
            "migrate_legacy": {"migration"},
            "accept_task_result": {"task_id", "result", "worker_assignment_id"},
            "open_review_epoch": {"epoch_id", "input_ref", "boundary_reason", "reviewer_assignment_id"},
            "open_review": {"review_id", "candidate_task_id", "findings", "reviewer_assignment_id", "fresh_epoch_id", "review_kind", "target_finding_id"},
            "accept_review": {"review_id", "candidate_task_id", "findings", "reviewer_assignment_id", "fresh_epoch_id", "review_kind", "target_finding_id"},
            "accept_resolution_claim": {"finding_id", "evidence", "worker_assignment_id"},
            "accept_finding_closure": {"finding_id", "evidence", "reviewer_assignment_id", "fresh_epoch_id", "review_id"},
            "validate_findings": {"review_id", "outcomes", "validator_assignment_id", "fresh_epoch_id"},
            "terminal_review": {"reason", "unresolved_finding_ids"},
            "reopen_review": {"approval_id", "replacement_budget", "expected_head"},
            "claim_task": {"task_id", "assignment_id"},
            "release_task": {"task_id", "assignment_id"},
            "invalidate_task": {"task_id", "reason"},
            "replan_task": {"task_id", "reason"},
            "open_epoch": {"epoch_id", "input_ref"},
            "close_epoch": {"acceptance_evidence", "approved_decisions", "unresolved_items", "invalidated_artifacts", "next_inputs", "context_budget", "clear_before_next", "boundary_reason"},
            "close_group": {"acceptance_evidence", "approved_decisions", "unresolved_items", "invalidated_artifacts", "next_inputs", "next_group", "boundary_reason"},
            "open_section": {"section", "group", "first_frontier", "preconditions", "transition"},
            "open_group": {"section", "group", "first_frontier", "preconditions", "transition"},
        }[operation]
        unknown = sorted(set(payload) - fields)
        if unknown:
            raise CommandValidationError("%s payload has unsupported fields: %s" % (operation, ", ".join(unknown)))
        required = {
            "entry": {"objective_ref"},
            "approve_objective": fields,
            "publish_artifact": fields,
            "publish_task_package": fields,
            "migrate_legacy": {"migration"},
            "accept_task_result": fields,
            "open_review_epoch": fields,
            "open_review": fields,
            "accept_review": fields,
            "accept_resolution_claim": fields,
            "accept_finding_closure": fields,
            "validate_findings": fields,
            "terminal_review": fields,
            "reopen_review": fields,
            "claim_task": fields,
            "release_task": fields,
            "invalidate_task": fields,
            "replan_task": fields,
            "open_epoch": fields,
            "close_epoch": fields,
            "close_group": fields,
            "open_section": fields,
            "open_group": fields,
        }[operation]
        missing = sorted(required - set(payload))
        if missing:
            raise CommandValidationError("%s payload is missing: %s" % (operation, ", ".join(missing)))
        if operation == "entry":
            if not isinstance(payload["objective_ref"], Mapping):
                raise CommandValidationError("entry.objective_ref must be an object")
        elif operation == "approve_objective":
            candidate = payload["candidate_ref"]
            prior = payload["prior_objective"]
            if (not isinstance(candidate, Mapping) or set(candidate) != {"path", "version", "digest", "namespace"}
                    or not isinstance(candidate.get("path"), str) or not candidate["path"]
                    or not isinstance(candidate.get("version"), str) or not _ID.fullmatch(candidate["version"])
                    or not isinstance(candidate.get("digest"), str) or not _DIGEST.fullmatch(candidate["digest"])
                    or not isinstance(candidate.get("namespace"), str) or not candidate["namespace"].startswith("fixture:")):
                raise CommandValidationError("approve_objective.candidate_ref is malformed")
            if (not isinstance(prior, Mapping) or set(prior) != {"version", "digest"}
                    or not isinstance(prior.get("version"), str) or not _ID.fullmatch(prior["version"])
                    or not isinstance(prior.get("digest"), str) or re.fullmatch(r"(?:sha256:)?[0-9a-f]{64}", prior["digest"]) is None):
                raise CommandValidationError("approve_objective.prior_objective is malformed")
            if not isinstance(payload["proposal_digest"], str) or not _DIGEST.fullmatch(payload["proposal_digest"]):
                raise CommandValidationError("approve_objective.proposal_digest is malformed")
            self._validate_objective_approval_payload(payload["approval"])
        elif operation == "migrate_legacy":
            migration = payload["migration"]
            if not isinstance(migration, Mapping):
                raise CommandValidationError("migrate_legacy.migration must be an object")
            required_migration = {
                "source_revision", "source_digests", "source_bindings", "run", "bundle",
                "worker_report", "field_mapping", "artifacts", "source_snapshot", "source_attestation",
            }
            if set(migration) != required_migration:
                missing_migration = sorted(required_migration - set(migration))
                extra_migration = sorted(set(migration) - required_migration)
                detail = []
                if missing_migration:
                    detail.append("missing " + ", ".join(missing_migration))
                if extra_migration:
                    detail.append("unsupported " + ", ".join(extra_migration))
                raise CommandValidationError(
                    "migrate_legacy.migration is incomplete: %s" % "; ".join(detail)
                )
            if not isinstance(migration["source_revision"], int) or isinstance(migration["source_revision"], bool) or migration["source_revision"] < 1:
                raise CommandValidationError("migrate_legacy.source_revision is malformed")
            source_digests = migration["source_digests"]
            if not isinstance(source_digests, Mapping) or set(source_digests) != {"run", "bundle", "worker_report"}:
                raise CommandValidationError("migrate_legacy.source_digests must bind all copied sources")
            if any(not isinstance(digest, str) or not _DIGEST.fullmatch(digest) for digest in source_digests.values()):
                raise CommandValidationError("migrate_legacy.source_digests contains a malformed digest")
            bindings = migration["source_bindings"]
            binding_required = {"source_run_id", "group_id", "epoch_id", "aliases", "source_revision", "run_status", "source_status", "epoch_status"}
            if not isinstance(bindings, Mapping) or set(bindings) != binding_required:
                raise CommandValidationError("migrate_legacy.source_bindings is incomplete")
            for key in ("source_run_id", "group_id", "epoch_id"):
                if not isinstance(bindings[key], str) or not _ID.fullmatch(bindings[key]):
                    raise CommandValidationError("migrate_legacy.source_bindings.%s is malformed" % key)
            if bindings["source_revision"] != migration["source_revision"] or bindings["run_status"] not in {"open", "closed"} or not isinstance(bindings["source_status"], str) or not bindings["source_status"] or bindings["epoch_status"] != "closed":
                raise CommandValidationError("migrate_legacy.source_bindings closure/revision binding is invalid")
            if not isinstance(bindings["aliases"], list) or any(not isinstance(alias, str) or not alias for alias in bindings["aliases"]):
                raise CommandValidationError("migrate_legacy.source_bindings.aliases is malformed")
            for key in ("run", "bundle", "worker_report"):
                if not isinstance(migration[key], Mapping):
                    raise CommandValidationError("migrate_legacy.%s must be an object" % key)
            if not isinstance(migration["field_mapping"], Mapping) or not isinstance(migration["artifacts"], Mapping):
                raise CommandValidationError("migrate_legacy field_mapping/artifacts are malformed")
            snapshot = migration["source_snapshot"]
            if (
                not isinstance(snapshot, Mapping)
                or set(snapshot) != {"mode", "source_digests", "source_bindings"}
                or snapshot.get("mode") != "immutable"
                or snapshot.get("source_digests") != migration["source_digests"]
                or snapshot.get("source_bindings") != migration["source_bindings"]
            ):
                raise CommandValidationError("migrate_legacy.source_snapshot is not an immutable source binding")
            self._validate_legacy_source_binding(
                migration["run"],
                migration["bundle"],
                migration["worker_report"],
                bindings,
                migration["source_revision"],
                CommandValidationError,
            )
            self._validate_migration_attestation(
                migration, verify_physical=verify_physical_attestation
            )
        elif operation == "publish_artifact":
            for key in ("artifact_id", "version", "kind"):
                if not isinstance(payload[key], str) or not _ID.fullmatch(payload[key]):
                    raise CommandValidationError("publish_artifact.%s is malformed" % key)
            if payload["path"] is not None and (not isinstance(payload["path"], str) or not payload["path"]):
                raise CommandValidationError("publish_artifact.path must be a string or null")
        elif operation == "publish_task_package":
            if not isinstance(payload["task_id"], str) or not _ID.fullmatch(payload["task_id"]):
                raise CommandValidationError("publish_task_package.task_id is malformed")
            self._validate_task_package_shape(payload["package"], "publish_task_package.package")
            if payload["package"].get("task_id") != payload["task_id"]:
                raise CommandValidationError("task package task_id does not match command")
            if payload["package"].get("assignment") != payload["assignment"]:
                raise CommandValidationError("task package assignment does not match command")
            if payload["package"].get("input_refs") != payload["input_refs"]:
                raise CommandValidationError("task package input_refs do not match command")
            if not isinstance(payload["input_refs"], list):
                raise CommandValidationError("publish_task_package.input_refs must be an array")
            for index, ref in enumerate(payload["input_refs"]):
                self._validate_command_ref_shape(ref, "publish_task_package.input_refs[%s]" % index)
            assignment = payload["assignment"]
            if not isinstance(assignment, Mapping) or set(assignment) != {"role", "assignment_id"} or assignment.get("role") != "worker" or not isinstance(assignment.get("assignment_id"), str) or not _ID.fullmatch(assignment["assignment_id"]):
                raise CommandValidationError("publish_task_package.assignment is malformed")
            if payload["sibling_group"] is not None and (not isinstance(payload["sibling_group"], str) or not _ID.fullmatch(payload["sibling_group"])):
                raise CommandValidationError("publish_task_package.sibling_group is malformed")
        elif operation in ("open_review", "accept_review"):
            for key in ("review_id", "candidate_task_id", "reviewer_assignment_id", "fresh_epoch_id"):
                if not isinstance(payload[key], str) or not _ID.fullmatch(payload[key]):
                    raise CommandValidationError("%s.%s is malformed" % (operation, key))
            if payload["review_kind"] not in ("initial", "closure"):
                raise CommandValidationError("%s.review_kind is invalid" % operation)
            if payload["target_finding_id"] is not None and (not isinstance(payload["target_finding_id"], str) or not _ID.fullmatch(payload["target_finding_id"])):
                raise CommandValidationError("%s.target_finding_id is malformed" % operation)
            if not isinstance(payload["findings"], list):
                raise CommandValidationError("%s.findings must be an array" % operation)
            for index, finding in enumerate(payload["findings"]):
                self._validate_finding_input_shape(finding, "%s.findings[%s]" % (operation, index))
        elif operation == "open_review_epoch":
            if not isinstance(payload["epoch_id"], str) or not _ID.fullmatch(payload["epoch_id"]):
                raise CommandValidationError("open_review_epoch.epoch_id is malformed")
            self._validate_command_ref_shape(payload["input_ref"], "open_review_epoch.input_ref")
            if not isinstance(payload["boundary_reason"], str) or not payload["boundary_reason"]:
                raise CommandValidationError("open_review_epoch.boundary_reason is required")
            if not isinstance(payload["reviewer_assignment_id"], str) or not _ID.fullmatch(payload["reviewer_assignment_id"]):
                raise CommandValidationError("open_review_epoch.reviewer_assignment_id is required")
        elif operation == "validate_findings":
            for key in ("review_id", "validator_assignment_id", "fresh_epoch_id"):
                if not isinstance(payload[key], str) or not _ID.fullmatch(payload[key]):
                    raise CommandValidationError("validate_findings.%s is malformed" % key)
            if not isinstance(payload["outcomes"], list) or not payload["outcomes"]:
                raise CommandValidationError("validate_findings.outcomes must be a non-empty array")
            for outcome in payload["outcomes"]:
                required_outcome = {"candidate_id", "disposition", "reason", "materiality", "requirement_ref", "permitted_fix_scope"}
                if not isinstance(outcome, Mapping) or set(outcome) != required_outcome:
                    raise CommandValidationError("validate_findings outcome fields are invalid")
                if outcome["disposition"] not in {"required", "defer", "reject", "needs-user", "duplicate"}:
                    raise CommandValidationError("validate_findings disposition is invalid")
                if any(not isinstance(outcome[key], str) or not outcome[key] for key in ("candidate_id", "reason", "materiality", "requirement_ref")):
                    raise CommandValidationError("validate_findings outcome text is invalid")
                if not isinstance(outcome["permitted_fix_scope"], list) or any(not isinstance(value, str) or not value for value in outcome["permitted_fix_scope"]):
                    raise CommandValidationError("validate_findings permitted_fix_scope is invalid")
        elif operation == "terminal_review":
            if payload["reason"] not in {"time_exhausted", "rounds_exhausted", "attempts_exhausted"} or not isinstance(payload["unresolved_finding_ids"], list) or any(not isinstance(value, str) or not _ID.fullmatch(value) for value in payload["unresolved_finding_ids"]):
                raise CommandValidationError("terminal_review payload is invalid")
        elif operation == "reopen_review":
            budget = payload["replacement_budget"]
            if not isinstance(payload["approval_id"], str) or not _ID.fullmatch(payload["approval_id"]) or not isinstance(budget, Mapping) or set(budget) != {"version", "deadline", "max_rounds", "max_attempts_per_finding", "rounds_used", "finding_attempts"} or not isinstance(budget.get("version"), str) or not _ID.fullmatch(budget["version"]) or any(not isinstance(budget.get(key), int) or isinstance(budget[key], bool) or budget[key] < 1 for key in ("max_rounds", "max_attempts_per_finding")) or budget.get("rounds_used") != 0 or budget.get("finding_attempts") != {}:
                raise CommandValidationError("reopen_review replacement_budget is invalid")
            if not isinstance(payload["expected_head"], Mapping):
                raise CommandValidationError("reopen_review expected_head is required")
        elif operation == "open_epoch":
            if not isinstance(payload["epoch_id"], str) or not _ID.fullmatch(payload["epoch_id"]):
                raise CommandValidationError("open_epoch.epoch_id is malformed")
            self._validate_command_ref_shape(payload["input_ref"], "open_epoch.input_ref")
        elif operation in ("accept_task_result",):
            if not isinstance(payload["task_id"], str) or not _ID.fullmatch(payload["task_id"]):
                raise CommandValidationError("accept_task_result.task_id is malformed")
            if not isinstance(payload["worker_assignment_id"], str) or not _ID.fullmatch(payload["worker_assignment_id"]):
                raise CommandValidationError("accept_task_result.worker_assignment_id is malformed")
            if not isinstance(payload["result"], Mapping) or not isinstance(payload["result"].get("status"), str):
                raise CommandValidationError("accept_task_result.result.status is required")
        elif operation in ("accept_resolution_claim", "accept_finding_closure"):
            for key in ("finding_id", "worker_assignment_id") if operation == "accept_resolution_claim" else ("finding_id", "reviewer_assignment_id", "fresh_epoch_id", "review_id"):
                if not isinstance(payload[key], str) or not _ID.fullmatch(payload[key]):
                    raise CommandValidationError("%s.%s is malformed" % (operation, key))
            if not isinstance(payload["evidence"], list):
                raise CommandValidationError("%s.evidence must be an array" % operation)
        elif operation in ("claim_task", "release_task"):
            for key in ("task_id", "assignment_id"):
                if not isinstance(payload[key], str) or not _ID.fullmatch(payload[key]):
                    raise CommandValidationError("%s.%s is malformed" % (operation, key))
        elif operation in ("invalidate_task", "replan_task"):
            if not isinstance(payload["task_id"], str) or not _ID.fullmatch(payload["task_id"]):
                raise CommandValidationError("%s.task_id is malformed" % operation)
            if not isinstance(payload["reason"], str) or not payload["reason"]:
                raise CommandValidationError("%s.reason is required" % operation)
        elif operation in ("open_section", "open_group"):
            section, group, frontier, preconditions = payload["section"], payload["group"], payload["first_frontier"], payload["preconditions"]
            if (not isinstance(section, Mapping) or set(section) != {"id", "workflow_id", "version"}
                    or not isinstance(section.get("id"), str) or not re.fullmatch(r"S[0-9]+", section["id"])
                    or not all(isinstance(section.get(key), str) and section[key] for key in ("workflow_id", "version"))):
                raise CommandValidationError("section transition section is malformed")
            if not isinstance(group, Mapping) or set(group) != {"id", "first_epoch"} or not all(isinstance(group.get(key), str) and _ID.fullmatch(group[key]) for key in ("id", "first_epoch")):
                raise CommandValidationError("section transition group is malformed")
            if not isinstance(frontier, list) or len(frontier) != 1 or any(not isinstance(item, str) or not _ID.fullmatch(item) for item in frontier):
                raise CommandValidationError("section transition first_frontier is malformed")
            successor = operation == "open_section" and section["id"] != "S0"
            required_preconditions = (
                {"plan_digest", "catalog_digest", "predecessor_section_id", "accepted_section_receipt_digest", "lifecycle_prerequisite_digest", "checkpoint_digest", "bundle_digest"}
                if successor else
                {"plan_digest", "catalog_digest", "closed_group_id", "closed_group_receipt_digest", "lifecycle_prerequisite_digest", "checkpoint_digest", "bundle_digest"}
            )
            identity_field = "predecessor_section_id" if successor else "closed_group_id"
            if (not isinstance(preconditions, Mapping) or set(preconditions) != required_preconditions
                    or not isinstance(preconditions.get(identity_field), str)
                    or any(not isinstance(value, str) or not _DIGEST.fullmatch(value)
                           for key, value in preconditions.items() if key != identity_field)):
                raise CommandValidationError("section transition preconditions are malformed")
            if payload.get("transition") != {"intent": "source-transition-fixture-passed", "state": "pending"}:
                raise CommandValidationError("section transition intent is malformed")
        elif operation in ("close_epoch", "close_group"):
            for key in ("acceptance_evidence", "approved_decisions", "unresolved_items", "invalidated_artifacts", "next_inputs"):
                if not isinstance(payload[key], list):
                    raise CommandValidationError("%s.%s must be an array" % (operation, key))
            if operation == "close_epoch" and not isinstance(payload["clear_before_next"], bool):
                raise CommandValidationError("close_epoch.clear_before_next must be boolean")
            if operation == "close_epoch" and not isinstance(payload["context_budget"], Mapping):
                raise CommandValidationError("close_epoch.context_budget must be an object")
            if not isinstance(payload["boundary_reason"], str) or not payload["boundary_reason"]:
                raise CommandValidationError("%s.boundary_reason is required" % operation)
            if operation == "close_group" and payload["next_group"] is not None and (not isinstance(payload["next_group"], str) or not _ID.fullmatch(payload["next_group"])):
                raise CommandValidationError("close_group.next_group is malformed")

    def _validate_command_shape(
        self, command: Mapping[str, Any], *, verify_physical_attestation: bool = True
    ) -> None:
        """Validate every envelope and operation payload before persistence."""

        if not isinstance(command, Mapping):
            raise CommandValidationError("command envelope must be an object")
        allowed = {
            "schema", "command_id", "command_type", "run_id", "expected_head", "workflow_version",
            "graph_version", "actor", "authority_ref", "input_refs", "idempotency_key",
            "protected_fields", "scope", "payload",
        }
        unknown = sorted(set(command) - allowed)
        if unknown:
            raise CommandValidationError("command envelope has unsupported fields: %s" % ", ".join(unknown))
        required = tuple(allowed)
        missing = sorted(key for key in required if key not in command)
        if missing:
            raise CommandValidationError("command envelope is missing: %s" % ", ".join(missing))
        if command.get("schema") != SCHEMA_COMMAND:
            raise CommandValidationError("unsupported command schema")
        for key in ("command_id", "command_type", "run_id", "idempotency_key"):
            if not isinstance(command.get(key), str) or not _ID.fullmatch(command[key]):
                raise CommandValidationError("%s is malformed" % key)
        if command["command_type"] not in COMMAND_TYPES:
            raise CommandValidationError("unknown command type: %s" % command["command_type"])
        workflow_version = command.get("workflow_version")
        if workflow_version is not None and (not isinstance(workflow_version, str) or not workflow_version):
            raise CommandValidationError("workflow_version must be a non-empty string or null")
        if command.get("graph_version") != GRAPH_VERSION:
            raise CommandValidationError("graph_version is invalid")
        expected = command.get("expected_head")
        if not isinstance(expected, Mapping):
            raise CommandValidationError("expected_head is required")
        expected_unknown = sorted(set(expected) - {"revision", "transaction_digest", "digest"})
        if expected_unknown:
            raise CommandValidationError("expected_head has unsupported fields: %s" % ", ".join(expected_unknown))
        if "revision" not in expected or "transaction_digest" not in expected:
            raise CommandValidationError("expected_head.revision and expected_head.transaction_digest are required")
        if not isinstance(expected["revision"], int) or isinstance(expected["revision"], bool) or expected["revision"] < 0:
            raise CommandValidationError("expected_head.revision is malformed")
        tx_digest = expected["transaction_digest"]
        if tx_digest is not None and (not isinstance(tx_digest, str) or not _DIGEST.fullmatch(tx_digest)):
            raise CommandValidationError("expected_head.transaction_digest is malformed")
        if "digest" in expected:
            short = expected["digest"]
            if short is not None and (not isinstance(short, str) or not _DIGEST.fullmatch(short)):
                raise CommandValidationError("expected_head.digest is malformed")
            if short is not None and tx_digest is not None and short != tx_digest:
                raise CommandValidationError("expected_head digest fields disagree")
        if expected["revision"] == 0 and (tx_digest is not None or expected.get("digest") is not None):
            raise CommandValidationError("genesis expected_head cannot point to a transaction")
        if expected["revision"] > 0 and tx_digest is None:
            raise CommandValidationError("non-genesis expected_head requires transaction_digest")
        actor = command.get("actor")
        if not isinstance(actor, Mapping) or set(actor) != {"role", "assignment_id"}:
            raise CommandValidationError("actor must contain only role and assignment_id")
        if actor.get("role") not in {"controller", "worker", "reviewer", "arbiter", "orchestrator"} or not isinstance(actor.get("assignment_id"), str) or not _ID.fullmatch(actor["assignment_id"]):
            raise CommandValidationError("actor role or assignment_id is malformed")
        if not isinstance(command.get("authority_ref"), Mapping) or not command["authority_ref"]:
            raise CommandValidationError("authority_ref is required")
        input_refs = command.get("input_refs")
        if not isinstance(input_refs, list):
            raise CommandValidationError("input_refs must be an array")
        for index, ref in enumerate(input_refs):
            self._validate_command_ref_shape(ref, "input_refs[%s]" % index)
        if not isinstance(command.get("protected_fields"), list) or any(not isinstance(item, str) or not item for item in command["protected_fields"]):
            raise CommandValidationError("protected_fields must be an array of strings")
        if len(set(command["protected_fields"])) != len(command["protected_fields"]):
            raise CommandValidationError("protected_fields must not contain duplicates")
        if not isinstance(command.get("scope"), list):
            raise CommandValidationError("scope must be an array")
        try:
            self._scope_paths(command["scope"], "command.scope")
        except KernelError as exc:
            raise CommandValidationError(str(exc)) from exc
        if not isinstance(command.get("payload"), Mapping):
            raise CommandValidationError("payload must be an object")
        self._validate_payload_shape(
            command["command_type"],
            command["payload"],
            verify_physical_attestation=verify_physical_attestation,
        )

    def _validate_command(
        self, command: Mapping[str, Any], state: Mapping[str, Any], head: Mapping[str, Any]
    ) -> None:
        self._validate_command_shape(command)
        self._ensure_durable_payload(command, "command")
        if command.get("schema") != SCHEMA_COMMAND:
            raise AuthorizationError("unsupported command schema")
        if command.get("run_id") != self.run_id:
            raise AuthorizationError("command Run ID mismatch")
        actor = command.get("actor")
        if (
            not isinstance(actor, Mapping)
            or actor.get("role") != "orchestrator"
            or not isinstance(actor.get("assignment_id"), str)
            or not actor.get("assignment_id")
        ):
            raise AuthorizationError("only the DAG Orchestrator may advance HEAD")
        if command.get("graph_version") != GRAPH_VERSION:
            raise StaleHeadError("graph version is stale")
        if command.get("workflow_version") != state.get("workflow_version"):
            raise StaleHeadError("workflow version is stale")
        expected = command.get("expected_head")
        if not isinstance(expected, Mapping):
            raise StaleHeadError("expected HEAD is required")
        expected_transaction_digest = expected.get("transaction_digest")
        expected_short_digest = expected.get("digest")
        if expected_transaction_digest is not None and expected_short_digest is not None and expected_transaction_digest != expected_short_digest:
            raise StaleHeadError("expected HEAD digest fields disagree")
        expected_digest = expected_transaction_digest or expected_short_digest
        if expected.get("revision") != head.get("revision") or expected_digest not in (head.get("transaction_digest"), head.get("digest")):
            raise StaleHeadError("expected HEAD is stale")
        operation = command.get("command_type")
        if not isinstance(operation, str) or not operation:
            raise AuthorizationError("command_type is required")
        required_payload_fields = {
            "publish_artifact": ("artifact_id", "version"),
            "publish_task_package": ("task_id",),
            "accept_task_result": ("task_id", "worker_assignment_id"),
            "open_review": ("review_id", "candidate_task_id", "reviewer_assignment_id", "fresh_epoch_id"),
            "accept_review": ("review_id", "candidate_task_id", "reviewer_assignment_id", "fresh_epoch_id"),
            "accept_resolution_claim": ("finding_id", "worker_assignment_id"),
            "accept_finding_closure": ("finding_id", "reviewer_assignment_id", "fresh_epoch_id", "review_id"),
            "validate_findings": ("review_id", "validator_assignment_id", "fresh_epoch_id"),
            "claim_task": ("task_id", "assignment_id"),
            "release_task": ("task_id", "assignment_id"),
            "open_epoch": ("epoch_id", "input_ref"),
            "open_review_epoch": ("epoch_id", "input_ref"),
        }
        required = required_payload_fields.get(operation, ())
        payload = command.get("payload")
        if not isinstance(payload, Mapping) or any(field not in payload for field in required):
            raise KernelError("command payload is incomplete for %s" % operation)
        self._authority_ok(
            command.get("authority_ref"),
            operation,
            protected_fields=command.get("protected_fields") or [],
        )
        authority = command.get("authority_ref")
        actor_assignment = actor.get("assignment_id") if isinstance(actor, Mapping) else None
        bound_assignment = authority.get("assignment_id") or authority.get("bound_assignment_id")
        if bound_assignment is not None and bound_assignment != actor_assignment:
            raise AuthorizationError("authority assignment does not match actor")
        payload = command.get("payload") or {}
        if operation == "publish_task_package" and payload.get("input_refs") != command.get("input_refs"):
            raise CommandValidationError("publish_task_package envelope input_refs do not match payload")
        if operation in {"open_epoch", "open_review_epoch"} and command.get("input_refs") != [payload.get("input_ref")]:
            raise CommandValidationError("%s envelope input_refs do not match payload" % operation)
        requested_scope = command.get("scope") or payload.get("write_scope") or payload.get("scope")
        if operation == "publish_task_package" and isinstance(payload.get("package"), Mapping):
            package_scope = payload["package"].get("write_scope") or payload["package"].get("output_path")
            requested_scope = requested_scope or package_scope
            self._validate_task_output_scope(payload["package"], command.get("authority_ref") or {})
        if operation == "publish_artifact" and payload.get("path") is not None:
            requested_scope = requested_scope or payload.get("path")
        if operation not in {"open_section", "open_group"}:
            self._validate_scope(requested_scope, command.get("authority_ref") or {}, label="command.scope")
        authority = command.get("authority_ref") or {}
        authority_workflow = authority.get("workflow_version")
        if authority_workflow is not None and authority_workflow != state.get("workflow_version"):
            raise AuthorizationError("authority workflow binding is stale")
        authority_revision = authority.get("expected_head_revision")
        if authority_revision is None and isinstance(authority.get("expected_head"), Mapping):
            authority_revision = authority["expected_head"].get("revision")
        if authority_revision is not None and authority_revision != expected.get("revision"):
            raise AuthorizationError("authority expected HEAD is stale")
        if operation == "reopen_review":
            self._validate_reopen_receipt(command, head, state)
        if operation == "approve_objective":
            approval = payload["approval"]
            candidate = payload["candidate_ref"]
            prior = payload["prior_objective"]
            authority = command["authority_ref"]
            current = state.get("objective_ref") or {}
            expected_authority_fields = {
                "status", "scopes", "run_id", "namespace", "approval_scope", "actor_id",
                "proposal_digest", "human_receipt", "protected_fields", "write_scopes",
            }
            fixture_identity = state.get("metadata", {}).get("fixture_identity")
            if (prior.get("version") != current.get("version")
                    or prior.get("digest") != current.get("digest")):
                raise AuthorizationError("prior objective does not match the current objective")
            if (approval.get("run_id") != self.run_id
                    or approval.get("candidate_digest") != candidate.get("digest")
                    or approval.get("candidate_version") != candidate.get("version")
                    or approval.get("prior_objective_digest") != prior.get("digest")
                    or approval.get("prior_objective_version") != prior.get("version")
                    or approval.get("proposal_digest") != payload.get("proposal_digest")):
                raise AuthorizationError("objective approval bindings do not match the command")
            receipt = approval.get("receipt") or {}
            if (receipt.get("candidate_path") != candidate.get("path")
                    or receipt.get("candidate_version") != candidate.get("version")
                    or receipt.get("candidate_namespace") != candidate.get("namespace")):
                raise AuthorizationError("human receipt candidate binding does not match the command")
            if (set(authority) != expected_authority_fields
                    or authority.get("status") != "approved"
                    or authority.get("scopes") != ["approve_objective"]
                    or authority.get("run_id") != self.run_id
                    or authority.get("namespace") != candidate.get("namespace")
                    or authority.get("namespace") != approval.get("namespace")
                    or authority.get("approval_scope") != "fixture-only"
                    or authority.get("actor_id") != approval.get("actor", {}).get("actor_id")
                    or authority.get("proposal_digest") != payload.get("proposal_digest")
                    or authority.get("human_receipt") != approval.get("receipt")
                    or authority.get("protected_fields") != ["objective_ref"]
                    or authority.get("write_scopes") != [candidate.get("namespace")]
                    or command.get("scope") != [candidate.get("namespace")]
                    or command.get("protected_fields") != ["objective_ref"]):
                raise AuthorizationError("objective approval authority, namespace, scope, or actor is invalid")
            expected_fixture = {
                "schema": "canonical-fixture-identity/v1", "run_id": self.run_id,
                "namespace": candidate.get("namespace"), "approval_scope": "fixture-only",
            }
            if fixture_identity != expected_fixture:
                raise AuthorizationError("objective approval requires a pre-existing canonical fixture identity")
            for objective in [current] + list(state.get("objective_history", [])):
                if (isinstance(objective, Mapping) and objective.get("version") == candidate.get("version")
                        and objective.get("digest") != candidate.get("digest")):
                    raise AuthorizationError("objective version is already bound to a different digest")
        protected_payload_keys = {"objective_ref", "objective", "material_risk", "authority_change"}
        if operation != "entry" and isinstance(payload, Mapping) and protected_payload_keys.intersection(payload):
            changed_protected = protected_payload_keys.intersection(payload)
            if not changed_protected.issubset(set(command.get("protected_fields") or [])):
                raise ProtectedFieldError("protected field change must be declared in the command envelope")
            if not authority.get("human_receipt") and not authority.get("approval_ref"):
                raise ProtectedFieldError("protected field change requires a human approval receipt")
        proposal_digest = authority.get("proposal_digest")
        if proposal_digest is not None:
            proposed = command.get("proposal_digest")
            if proposed is None and isinstance(payload, Mapping):
                proposed = payload.get("proposal_digest") or payload.get("proposal_digest_ref")
            if proposed != proposal_digest:
                raise AuthorizationError("authority proposal digest does not match command")
        if operation in {"open_section", "open_group"}:
            self._validate_section_transition(command, state)
            return
        for index, ref in enumerate(command.get("input_refs") or []):
            safe = _safe_ref(ref, "input_refs[%s]" % index)
            if not self._object_path(safe["digest"]).exists():
                if not ref.get("external"):
                    raise IntegrityBlockedError("missing input object: %s" % safe["digest"])
            if ref.get("external"):
                self._validate_input_ref_binding(state, ref, "input_refs[%s]" % index)
            else:
                self._validate_input_ref_binding(state, ref, "input_refs[%s]" % index)

    def _reachable_transaction_digests(self, head: Optional[Mapping[str, Any]] = None) -> set[str]:
        """Return transaction digests anchored by the canonical HEAD only."""

        if head is None:
            head = self._load_head()
        digest = head.get("transaction_digest")
        if not isinstance(digest, str):
            raise IntegrityBlockedError("HEAD transaction pointer missing")
        reachable: set[str] = set()
        current = self._load_transaction(digest)
        expected_revision = head.get("revision")
        while True:
            current_digest = current.get("digest")
            if not isinstance(current_digest, str) or current_digest in reachable:
                raise IntegrityBlockedError("transaction chain cycle")
            reachable.add(current_digest)
            if current.get("revision") != expected_revision:
                raise IntegrityBlockedError("transaction revision mismatch")
            parent = current.get("parent")
            if not parent:
                break
            parent_digest = parent.get("transaction_digest") or parent.get("digest")
            if not isinstance(parent_digest, str):
                raise IntegrityBlockedError("transaction parent digest missing")
            expected_revision -= 1
            current = self._load_transaction(parent_digest)
        return reachable

    def _find_transaction_for_command(self, key: str, head: Optional[Mapping[str, Any]] = None) -> Optional[Dict[str, Any]]:
        """Find a duplicate only in the HEAD-anchored transaction chain.

        A transaction published before HEAD is replaced is an orphan, not a
        completed command.  Retrying it must therefore be able to create a
        fresh child of the actual HEAD.
        """

        if not self.transactions_dir.exists():
            return None
        for digest in self._reachable_transaction_digests(head):
            try:
                value = self._load_transaction(digest)
            except KernelError:
                continue
            command = value.get("command")
            if isinstance(command, Mapping) and command.get("idempotency_key") == key:
                return value
        return None

    def _new_object(self, stage: Path, object_type: str, payload: Mapping[str, Any]) -> Dict[str, Any]:
        # Legacy source values are retained only as already-digested evidence;
        # they still pass the same durable-field filter as every other object.
        self._ensure_durable_payload(payload, object_type)
        return self._write_object_to_stage(stage, object_type, payload)

    def _add_object_ref(self, state: Dict[str, Any], ref: Mapping[str, Any]) -> None:
        safe = _safe_ref(ref)
        state["object_refs"][safe["digest"]] = {
            "digest": safe["digest"],
            "object_type": safe.get("object_type"),
            "path": safe.get("path"),
        }

    def _commit(
        self,
        command: Mapping[str, Any],
        reducer: Any,
        *,
        initial: bool = False,
        initial_state: Optional[Mapping[str, Any]] = None,
    ) -> Dict[str, Any]:
        """Validate and commit one transaction.  ``reducer`` is pure apart
        from placing immutable objects into the supplied staging directory.
        """

        # Shape validation is deliberately outside the staging/publish path;
        # malformed envelopes cannot create a partial transaction or reach a
        # later dictionary lookup.
        self._validate_command_shape(command)
        self._ensure_durable_payload(command, "command")
        if initial:
            # Validate genesis authority before ``_lock(create=True)`` can
            # create even an empty destination layout.  Invalid authority is
            # a rejected request, not a partially initialized Run.
            actor = command.get("actor")
            if not isinstance(actor, Mapping) or actor.get("role") != "orchestrator":
                raise AuthorizationError("only the DAG Orchestrator may create the genesis HEAD")
            self._authority_ok(command.get("authority_ref"), "entry")
            self._authority_assignment_matches(command)
        with self._lock(create=initial):
            if initial:
                if self.head_path.exists():
                    raise DuplicateCommandError("Run already has a HEAD")
                previous = None
                head = {"revision": 0, "transaction_digest": None}
                state = _copy(initial_state)
            else:
                try:
                    previous, head = self._load_current()
                except (IntegrityBlockedError, DAGCycleError) as exc:
                    self._record_integrity_block(str(exc))
                    raise IntegrityBlockedError(str(exc)) from exc
                existing = self._find_transaction_for_command(command["idempotency_key"], head)
                command_digest = self._idempotency_digest(command)
                if existing is not None:
                    if existing.get("idempotency_digest", existing.get("command_digest")) != command_digest:
                        raise DuplicateCommandError("idempotency key reused with a different command")
                    self.last_receipt = {
                        "duplicate": True,
                        "transaction_digest": existing["digest"],
                        "revision": existing["revision"],
                    }
                    return _copy(previous)
                self._validate_command(command, previous, head)
            if initial:
                # Entry is the only genesis operation; its authority is still
                # checked so a caller cannot accidentally create an unmanaged
                # canonical Run through the low-level API.
                self._ensure_durable_payload(command.get("authority_ref"), "authority_ref")
                self._authority_ok(command.get("authority_ref"), "entry")
                self._authority_assignment_matches(command)

            stage = self.staging_dir / ("stage-" + uuid.uuid4().hex)
            stage.mkdir(parents=True, exist_ok=False)
            published_transaction = False
            try:
                # Reducers run once with a real staging directory.  No
                # canonical state is touched until the staged transaction is
                # published below.
                if initial:
                    state = _copy(initial_state)
                    state = _copy(reducer(state, command, stage))
                else:
                    # Reducers are allowed to update the state they receive.
                    # Keep the immutable pre-transaction snapshot separate so
                    # graph_delta describes the actual transition rather than
                    # comparing the reducer's mutated object with itself.
                    state = _copy(reducer(_copy(previous), command, stage))
                state["revision"] = (0 if initial else previous["revision"]) + 1
                state["state_revision"] = state["revision"]
                state["graph_revision"] = state["revision"]
                # The task graph revision is a state-level binding.  A
                # transaction that changes any part of the canonical state
                # advances that binding for every registered Task, including
                # tasks retained as historical siblings.
                for task_record in state.get("tasks", {}).values():
                    if isinstance(task_record, Mapping):
                        task_record["graph_revision"] = state["graph_revision"]
                state["updated_at"] = _now()
                state["idempotency"][command["idempotency_key"]] = self._idempotency_digest(command)
                self._ensure_durable_payload(state, "state")
                self._validate_state(state, load_objects=False)
                graph_delta = self._graph_delta(None if initial else previous, state)
                transaction_body = {
                    "schema": SCHEMA_TRANSACTION,
                    "kernel_version": KERNEL_VERSION,
                    "run_id": self.run_id,
                    "workflow_version": state["workflow_version"],
                    "graph_version": GRAPH_VERSION,
                    "revision": state["revision"],
                    "state_revision": state["revision"],
                    "parent": None
                    if initial
                    else {
                        "revision": head["revision"],
                        "transaction_digest": head["transaction_digest"],
                        "digest": head["transaction_digest"],
                    },
                    "command": _copy(command),
                    "command_digest": self._command_digest(command),
                    "idempotency_digest": self._idempotency_digest(command),
                    "state": _copy(state),
                    "object_refs": _copy(state["object_refs"]),
                    "compiler_version": KERNEL_VERSION,
                    "graph_delta": graph_delta,
                }
                transaction_digest = _digest(transaction_body)
                transaction = dict(transaction_body, digest=transaction_digest)
                _atomic_json(stage / "transaction.json", transaction)
                _fsync_directory(stage)
                self._maybe_fault("before_publish")

                for staged in sorted((stage / "objects").glob("*.json")) if (stage / "objects").exists() else []:
                    self._publish_staged_file(staged, self.objects_dir / staged.name)
                _fsync_directory(self.objects_dir)
                tx_destination = self._transaction_path(transaction_digest)
                if tx_destination.exists():
                    existing_tx = _read_json(tx_destination)
                    if existing_tx != transaction:
                        raise IntegrityBlockedError("transaction digest collision")
                else:
                    os.replace(str(stage / "transaction.json"), str(tx_destination))
                _fsync_directory(self.transactions_dir)
                published_transaction = True
                self._maybe_fault("after_publish_before_head")

                head_body = {
                    "schema": SCHEMA_HEAD,
                    "run_id": self.run_id,
                    "workflow_version": state["workflow_version"],
                    "graph_version": GRAPH_VERSION,
                    "role": "orchestrator",
                    "revision": state["revision"],
                    "state_revision": state["revision"],
                    "transaction_digest": transaction_digest,
                }
                _atomic_json(self.head_path, dict(head_body, digest=_digest(head_body)))
                self._maybe_fault("after_head_before_projection")
                self._rebuild_projection_locked(state, dict(head_body, digest=_digest(head_body)))
                shutil.rmtree(stage, ignore_errors=True)
                self.last_receipt = {
                    "duplicate": False,
                    "transaction_digest": transaction_digest,
                    "revision": state["revision"],
                    "command_id": command["command_id"],
                }
                return _copy(state)
            except InjectedCrash:
                # The stage (and, for the second fault, the published orphan)
                # is intentionally retained for recover() to quarantine.
                raise
            except Exception:
                # Once publication has begun, retaining the stage preserves
                # recovery evidence.  Validation failures before publication
                # can be safely discarded.
                if not published_transaction:
                    shutil.rmtree(stage, ignore_errors=True)
                raise

    def _projection_contents(
        self, state: Mapping[str, Any], head: Mapping[str, Any]
    ) -> Dict[str, bytes]:
        """Render the projection set from canonical state, without trusting claims."""

        run_projection = {"schema": "run-projection/v1", "source_head": _copy(head), "state": _copy(state)}
        ready = self._ready_tasks(state, head)
        status = {
            "schema": "status-projection/v1",
            "run_id": self.run_id,
            "workflow_version": state["workflow_version"],
            "graph_version": GRAPH_VERSION,
            "revision": state["revision"],
            "state_revision": state["state_revision"],
            "status": state["status"],
            "group": _copy(state["group"]),
            "epoch": _copy(state["epoch"]),
            "ready": ready,
            "open_blocking_findings": [
                key for key, value in state["findings"].items() if self._finding_blocks(value)
            ],
            "source_head": _copy(head),
        }
        views: Dict[str, Any] = {
            "run.json": run_projection,
            "status.json": status,
            "ready.json": {"source_head": _copy(head), "tasks": ready},
            "run.yaml": run_projection,
            "plan.yaml": {
                "source_head": _copy(head),
                "tasks": _copy(state.get("tasks", {})),
                "artifacts": _copy(state.get("artifacts", {})),
                "edges": _copy(state.get("edges", [])),
            },
        }
        bundle_ref = state.get("group", {}).get("bundle_ref") or state.get("epoch", {}).get("bundle_ref")
        checkpoint_ref = state.get("group", {}).get("checkpoint_ref") or state.get("epoch", {}).get("checkpoint_ref")
        views["artifact-bundle.json"] = {"source_head": _copy(head), "bundle_ref": _copy(bundle_ref)}
        views["checkpoint.json"] = {
            "source_head": _copy(head),
            "run_id": self.run_id,
            "revision": state["revision"],
            "bundle_ref": _copy(bundle_ref),
            "checkpoint_ref": _copy(checkpoint_ref),
            "clear_before_start": bool(
                state.get("epoch", {}).get("clear_before_next") or state.get("group", {}).get("clear_required")
            ),
        }
        contents = {relative: _pretty_json_bytes(value) for relative, value in views.items()}
        contents["status.md"] = (
            "# status\n\n- run: %s\n- revision: %s\n- status: %s\n- ready: %s\n"
            % (self.run_id, state["revision"], state["status"], ", ".join(ready) or "none")
        ).encode("utf-8")
        events = self._chain_events(head["transaction_digest"])
        contents["events.jsonl"] = "".join(
            json.dumps(event, ensure_ascii=False, sort_keys=True) + "\n" for event in events
        ).encode("utf-8")
        for task_id, task in state.get("tasks", {}).items():
            package = self._load_object(task["package_ref"]["digest"])
            contents["packages/tasks/%s.json" % task_id] = _pretty_json_bytes(
                {"source_ref": _copy(task["package_ref"]), "task": _copy(task), "package": _copy(package["payload"])}
            )
        for review_id, review in state.get("reviews", {}).items():
            package = self._load_object(review["package_ref"]["digest"])
            contents["packages/reviews/%s.json" % review_id] = _pretty_json_bytes(
                {"source_ref": _copy(review["package_ref"]), "review": _copy(review), "package": _copy(package["payload"])}
            )
        return contents

    def _projection_is_current(
        self, head: Mapping[str, Any], state: Optional[Mapping[str, Any]] = None
    ) -> bool:
        """Verify projection bytes against a deterministic canonical render."""

        try:
            manifest = _read_json(self.projection_dir / "projection-manifest.json")
            if state is None:
                state = self._load_transaction(head["transaction_digest"])["state"]
            expected_contents = self._projection_contents(state, head)
        except (KernelError, OSError, KeyError, TypeError):
            return False
        if not isinstance(manifest, Mapping) or manifest.get("schema") != "projection-manifest/v1":
            return False
        if manifest.get("run_id") != self.run_id:
            return False
        manifest_digest = manifest.get("digest")
        if not isinstance(manifest_digest, str) or _digest(_without_digest(manifest)) != manifest_digest:
            return False
        source_head = manifest.get("source_head")
        if not isinstance(source_head, Mapping):
            return False
        for key in ("run_id", "workflow_version", "graph_version", "revision", "state_revision", "transaction_digest"):
            if source_head.get(key) != head.get(key):
                return False
        files = manifest.get("files")
        expected_files = {relative: _digest_bytes(content) for relative, content in expected_contents.items()}
        if not isinstance(files, Mapping) or dict(files) != expected_files:
            return False
        if not self._REQUIRED_PROJECTION_FILES.issubset(set(files)):
            return False
        actual_files = {
            str(path.relative_to(self.projection_dir))
            for path in self.projection_dir.rglob("*")
            if path.is_file() and path.name != "projection-manifest.json"
        }
        if actual_files != set(expected_contents):
            return False
        for relative, content in expected_contents.items():
            relative_path = Path(relative)
            if relative_path.is_absolute() or ".." in relative_path.parts:
                return False
            target = self.projection_dir / relative_path
            try:
                if not target.is_file() or target.read_bytes() != content:
                    return False
            except OSError:
                return False
        return True

    def _rebuild_projection_locked(self, state: Mapping[str, Any], head: Mapping[str, Any]) -> None:
        """Build canonical views in a stage, then publish one manifest marker."""

        contents = self._projection_contents(state, head)
        stage = self.staging_dir / ("stage-projection-" + uuid.uuid4().hex)
        stage.mkdir(parents=True, exist_ok=False)
        try:
            for relative, content in contents.items():
                _atomic_bytes(stage / relative, content)
            files = {relative: _digest_bytes(content) for relative, content in contents.items()}
            manifest_body = {
                "schema": "projection-manifest/v1",
                "version": "v1",
                "run_id": self.run_id,
                "source_head": _copy(head),
                "files": files,
            }
            _atomic_json(stage / "projection-manifest.json", dict(manifest_body, digest=_digest(manifest_body)))
            candidates = sorted(
                path for path in stage.rglob("*") if path.is_file() and path.name != "projection-manifest.json"
            )
            for candidate in candidates:
                relative = candidate.relative_to(stage)
                self._publish_projection_file(candidate, self.projection_dir / relative)
                self._maybe_fault("during_projection")
            # Views are disposable projections.  Remove stale task/review
            # package files only after every canonical replacement has been
            # staged and published; the manifest remains the commit marker.
            expected_paths = {self.projection_dir / relative for relative in contents}
            for existing in sorted(
                (path for path in self.projection_dir.rglob("*") if path.is_file()),
                key=lambda path: len(path.parts),
                reverse=True,
            ):
                if existing.name != "projection-manifest.json" and existing not in expected_paths:
                    existing.unlink()
            for directory in sorted(
                (path for path in self.projection_dir.rglob("*") if path.is_dir()),
                key=lambda path: len(path.parts),
                reverse=True,
            ):
                try:
                    directory.rmdir()
                except OSError:
                    pass
            manifest_candidate = stage / "projection-manifest.json"
            self._publish_projection_file(manifest_candidate, self.projection_dir / manifest_candidate.name)
            _fsync_directory(self.projection_dir)
            shutil.rmtree(stage, ignore_errors=True)
        except InjectedCrash:
            # Keep the partial stage.  Without a new manifest, readers use the
            # previous complete projection or rebuild from canonical HEAD.
            raise
        except BaseException:
            shutil.rmtree(stage, ignore_errors=True)
            raise

    def _chain_events(self, digest: str) -> List[Dict[str, Any]]:
        transactions: List[Dict[str, Any]] = []
        current = self._load_transaction(digest)
        while current:
            transactions.append(current)
            parent = current.get("parent")
            if not parent:
                break
            current = self._load_transaction(parent.get("transaction_digest") or parent.get("digest"))
        transactions.reverse()
        return [
            {
                "revision": tx["revision"],
                "transaction_digest": tx["digest"],
                "command_type": tx["command"].get("command_type"),
                "command_id": tx["command"].get("command_id"),
            }
            for tx in transactions
        ]

    @staticmethod
    def _scope_contains(container: str, path: str) -> bool:
        """Return whether a scope pattern grants the concrete path.

        A6R scopes are relative POSIX paths.  ``*`` is the explicit managed
        workspace wildcard and a trailing ``*`` is a subtree wildcard.  The
        same predicate is used for authority, Task output, result, and lease
        checks so a wildcard cannot be treated as a literal in one path and a
        grant in another.
        """

        if not isinstance(container, str) or not isinstance(path, str):
            return False
        if container == "*":
            return True
        container = container.rstrip("/") or "."
        path = path.rstrip("/") or "."
        if "*" in container:
            pattern = re.escape(container).replace(r"\*", ".*")
            return re.fullmatch(pattern, path) is not None
        return path == container or path.startswith(container + "/")

    @classmethod
    def _scopes_overlap(cls, left: Sequence[str], right: Sequence[str]) -> bool:
        if not left or not right:
            return False
        for a in left:
            for b in right:
                if not isinstance(a, str) or not isinstance(b, str):
                    continue
                # Either declaration may be the wildcard side.  This is
                # intentionally symmetric: a broad lease blocks a concrete
                # Task and a concrete lease blocks a broad Task as well.
                if a == "*" or b == "*":
                    return True
                if cls._scope_contains(a, b) or cls._scope_contains(b, a):
                    return True
                # Two wildcard patterns can intersect without either one
                # matching the other's spelling (for example ``src/*`` and
                # ``src/component/*``).  Their literal prefixes identify the
                # shared subtree conservatively.
                a_prefix = a.split("*", 1)[0].rstrip("/")
                b_prefix = b.split("*", 1)[0].rstrip("/")
                if a_prefix and b_prefix and (
                    a_prefix == b_prefix
                    or a_prefix.startswith(b_prefix + "/")
                    or b_prefix.startswith(a_prefix + "/")
                ):
                    return True
        return False

    @staticmethod
    def _finding_blocks(finding: Mapping[str, Any]) -> bool:
        """Return whether a Finding still blocks readiness/closure.

        ``resolved`` is both the worker-claim state and the canonical terminal
        state after independent closure.  The object-bound ``closure_ref`` is
        therefore the discriminator; a worker resolution alone remains a
        blocking Finding.
        """

        if not finding.get("blocking"):
            return False
        if finding.get("admitted") is not True:
            return False
        state = finding.get("state")
        return state in ("open", "unresolved") or (state == "resolved" and not finding.get("closure_ref"))

    def _ready_tasks(
        self, state: Mapping[str, Any], head: Optional[Mapping[str, Any]] = None
    ) -> List[str]:
        """Compute a conservative, reproducible ready frontier from state."""

        ready: List[str] = []
        if head is None:
            # Private callers may hand the compiler a state snapshot directly,
            # but an omitted HEAD must never disable the authority expected
            # HEAD check.  Public callers already supply this snapshot while
            # holding the Run lock; this fallback keeps the helper fail-closed
            # for tests and recovery code that call it directly.
            try:
                head = self._load_head()
            except KernelError:
                return ready
        try:
            # Readiness is a compiler consumer of the same graph contract as
            # the reducer and recovery reader.  Fail closed if a caller gives
            # it a forged/partial state instead of routing from an invalid
            # edge topology.
            self._validate_edges(state.get("edges", []), state.get("nodes", {}), exact=True)
        except KernelError:
            return ready
        if state.get("group", {}).get("status") != "open" or state.get("epoch", {}).get("status") != "open":
            return ready
        if state.get("status") not in ("active",):
            return ready
        section = state.get("metadata", {}).get("section_control")
        if isinstance(section, Mapping):
            current = section.get("groups", {}).get(state.get("group", {}).get("id"))
            if isinstance(current, Mapping) and current.get("epoch_id") == state.get("epoch", {}).get("id"):
                frontier = current.get("frontier")
                if isinstance(frontier, list) and all(isinstance(item, str) and _ID.fullmatch(item) for item in frontier):
                    return list(frontier)
        leases = state.get("leases", {})
        epoch_id = state.get("epoch", {}).get("id")
        budget = state.get("context_budget", {})
        try:
            self._validate_budget(budget)
        except KernelError:
            return ready
        active_scopes = [
            self._scope_paths(lease.get("write_scope"), "lease.write_scope")
            for lease in leases.values()
            if isinstance(lease, Mapping)
        ]
        for task_id, task in sorted(state.get("tasks", {}).items()):
            if task.get("status") not in ("planned", "ready"):
                continue
            if task.get("invalidated") or task.get("stop_requested"):
                continue
            if task.get("epoch_id") != epoch_id:
                continue
            freshness = task.get("freshness") or {}
            if not isinstance(freshness, Mapping) or freshness.get("epoch_id") != epoch_id:
                continue
            created_at_revision = freshness.get("created_at_revision")
            if (
                not isinstance(created_at_revision, int)
                or isinstance(created_at_revision, bool)
                or created_at_revision < 1
                or created_at_revision > state.get("revision", 0)
            ):
                continue
            if freshness.get("invalidated") or freshness.get("stale"):
                continue
            if task_id in leases:
                continue
            try:
                self._validate_live_task_authority(state, task, current_head=head)
            except KernelError:
                # Expiry, scope, proposal, expected-HEAD, and assignment
                # failures all mean await-human/replan; none may silently
                # become a ready route.
                continue
            task_scopes = self._scope_paths(task.get("write_scope"), "task.write_scope")
            if any(self._scopes_overlap(task_scopes, scope) for scope in active_scopes):
                continue
            if not any(
                edge.get("to") == "task:" + task_id
                and edge.get("type") == "authorizes"
                and str(edge.get("from", "")).split(":", 1)[0] in {"authority", "approval"}
                for edge in state.get("edges", [])
            ):
                continue
            blocked = False
            for edge in state.get("edges", []):
                if edge.get("to") != "task:" + task_id:
                    continue
                kind = edge.get("type")
                source = edge.get("from", "")
                if kind not in {"requires", "verdict-for", "converges"}:
                    continue
                if source.startswith("artifact:"):
                    artifact = state.get("artifacts", {}).get(source[len("artifact:") :])
                    if not artifact or artifact.get("invalidated") or artifact.get("status") not in ("available", "accepted"):
                        blocked = True
                elif source.startswith("task:"):
                    required = state.get("tasks", {}).get(source[len("task:") :])
                    if not required or required.get("invalidated") or required.get("status") != "succeeded":
                        blocked = True
                elif source.startswith("work-product:"):
                    required = state.get("tasks", {}).get(source[len("work-product:") :])
                    if (
                        not required
                        or required.get("invalidated")
                        or required.get("status") != "succeeded"
                        or not isinstance(required.get("result_ref"), Mapping)
                    ):
                        blocked = True
                elif source.startswith("review:"):
                    review = state.get("reviews", {}).get(source[len("review:") :])
                    if not review or review.get("verdict") != "pass":
                        blocked = True
            if any(
                finding.get("candidate_task_id") == task_id and self._finding_blocks(finding)
                for finding in state.get("findings", {}).values()
            ):
                blocked = True
            if not blocked:
                ready.append(task_id)
        return ready

    def _edge(self, state: Dict[str, Any], source: str, target: str, edge_type: str) -> None:
        edge = {"from": source, "to": target, "type": edge_type}
        if edge not in state["edges"]:
            proposed = list(state["edges"]) + [edge]
            self._validate_edges(proposed, state.get("nodes"), exact=True)
            state["edges"].append(edge)

    @staticmethod
    def _reverse_dependency_closure(state: Mapping[str, Any], task_id: str) -> List[str]:
        """Return downstream Tasks invalidated by one Task's stale outputs.

        The execution graph has no Task-to-Task edge.  Walk the canonical
        Task -> Artifact/work-product -> Task paths instead, and deliberately
        ignore authority/verdict provenance edges.  Returning a sorted list
        makes the invalidation record deterministic and auditable.
        """

        start = "task:" + task_id
        seen_nodes = {start}
        frontier = [start]
        while frontier:
            source = frontier.pop(0)
            for edge in state.get("edges", []):
                if not isinstance(edge, Mapping) or edge.get("from") != source:
                    continue
                if edge.get("type") not in {"produces", "requires", "converges"}:
                    continue
                target = edge.get("to")
                if not isinstance(target, str) or target in seen_nodes:
                    continue
                if not target.startswith(("artifact:", "work-product:", "task:")):
                    continue
                seen_nodes.add(target)
                frontier.append(target)
        return sorted(
            node.split(":", 1)[1]
            for node in seen_nodes
            if node.startswith("task:") and node != start
        )

    def _record_authority_edge(self, state: Dict[str, Any], command: Mapping[str, Any], target: str) -> None:
        """Represent command authority as a typed, acyclic provenance edge."""

        # The canonical execution graph has exactly Authority/Approval ->
        # Task authorizations.  Artifact, Review, and Finding records retain
        # the immutable command authority reference but do not manufacture a
        # broad capability edge to themselves.
        if not isinstance(target, str) or not target.startswith("task:"):
            return

        authority = command.get("authority_ref") or {}
        authority_node = "authority:" + _digest(authority)[7:23]
        state.setdefault("nodes", {})[authority_node] = "authority"
        if authority.get("approval_ref"):
            approval_node = "approval:" + _digest({"approval_ref": authority["approval_ref"]})[7:23]
            state["nodes"][approval_node] = "approval"
            self._edge(state, approval_node, target, "authorizes")
        self._edge(state, authority_node, target, "authorizes")

    def _entry_reducer(self, state: Dict[str, Any], command: Mapping[str, Any], stage: Optional[Path]) -> Dict[str, Any]:
        if stage is None:
            return state
        payload = command["payload"]
        objective = self._new_object(stage, "objective", payload["objective_ref"])
        self._add_object_ref(state, objective)
        state["objective_ref"] = _copy(payload["objective_ref"])
        state["objective_ref"]["object_digest"] = objective["digest"]
        state["entry_object_ref"] = objective
        state.setdefault("nodes", {})["objective:entry"] = "objective"
        return state

    def _validate_section_transition(self, command: Mapping[str, Any], state: Mapping[str, Any]) -> None:
        """Validate the closed-boundary source fixture without inventing a store.

        Its six named digests are source inputs, rather than objects created by
        this disposable Run; their names and payload bindings are therefore
        checked together at the kernel boundary.
        """
        operation, payload = command["command_type"], command["payload"]
        section_id = payload["section"]["id"]
        successor = operation == "open_section" and section_id != "S0"
        required = {
            "plan", "catalog", "checkpoint", "bundle", "lifecycle_prerequisite",
            "accepted_section_receipt" if successor else "closed_group_receipt",
        }
        refs = command.get("input_refs")
        if (not isinstance(refs, list) or len(refs) != len(required)
                or any(not isinstance(ref, Mapping) or set(ref) != {"kind", "digest"} for ref in refs)
                or {ref["kind"] for ref in refs} != required):
            raise AuthorizationError("section transition requires six bound source references")
        by_kind = {ref["kind"]: ref["digest"] for ref in refs}
        pre = payload["preconditions"]
        bindings = {
            "plan_digest": "plan", "catalog_digest": "catalog",
            "checkpoint_digest": "checkpoint", "bundle_digest": "bundle",
            "lifecycle_prerequisite_digest": "lifecycle_prerequisite",
            "accepted_section_receipt_digest" if successor else "closed_group_receipt_digest": "accepted_section_receipt" if successor else "closed_group_receipt",
        }
        if any(pre[field] != by_kind[kind] for field, kind in bindings.items()):
            raise AuthorizationError("section transition source digest binding is forged")
        self._validate_section_source_references(state, by_kind, command["command_type"], successor=successor)
        authority = command["authority_ref"]
        if (authority.get("execution_class") != "candidate-generic"
                or authority.get("expected_head") != command.get("expected_head")
                or authority.get("protected_fields") != command.get("protected_fields")
                or command.get("protected_fields") != ["section", "group", "epoch", "ready"]
                or command.get("scope") != ["source"]):
            raise AuthorizationError("section transition authority binding is invalid")
        group = payload["group"]
        if successor:
            prior = state.get("metadata", {}).get("section_control")
            pre = payload["preconditions"]
            expected_predecessor = "S%d" % (int(section_id[1:]) - 1)
            if (
                not isinstance(prior, Mapping)
                or state.get("status") != "paused_after_group"
                or state.get("leases")
                or any(task.get("status") == "running" for task in state.get("tasks", {}).values() if isinstance(task, Mapping))
                or prior.get("section_id") != expected_predecessor
                or prior.get("transition") != {"intent": "source-transition-fixture-passed", "state": "accepted"}
                or pre.get("predecessor_section_id") != expected_predecessor
                or not isinstance(prior.get("accepted_section_receipt"), Mapping)
                or prior["accepted_section_receipt"].get("digest") != by_kind["accepted_section_receipt"]
                or by_kind["lifecycle_prerequisite"] != by_kind["accepted_section_receipt"]
                or state.get("group", {}).get("bundle_ref", {}).get("digest") != by_kind["bundle"]
                or state.get("group", {}).get("checkpoint_ref", {}).get("digest") != by_kind["checkpoint"]
            ):
                raise LifecycleClosedError("open_section requires the current accepted immediate predecessor")
            receipt = self._load_ref_object(prior["accepted_section_receipt"], "accepted section receipt")
            receipt_payload = receipt.get("payload", {}).get("payload") if isinstance(receipt.get("payload"), Mapping) else None
            if (
                receipt.get("object_type") != "artifact" or not isinstance(receipt_payload, Mapping)
                or receipt_payload.get("schema") != "section-acceptance-receipt/v1"
                or receipt_payload.get("run_id") != self.run_id
                or receipt_payload.get("section_id") != expected_predecessor
                or receipt_payload.get("bundle_ref") != state["group"].get("bundle_ref")
                or receipt_payload.get("checkpoint_ref") != state["group"].get("checkpoint_ref")
            ):
                raise IntegrityBlockedError("accepted section receipt is not bound to the current HEAD boundary")
        elif operation == "open_section":
            if (state.get("status") != "paused_after_group" or state.get("group", {}).get("id") != "Bootstrap"
                    or pre.get("closed_group_id") != "Bootstrap" or group.get("id") != "contracts-and-schema"
                    or state.get("metadata", {}).get("section_control") is not None):
                raise LifecycleClosedError("open_section requires the closed Bootstrap boundary")
        else:
            section = state.get("metadata", {}).get("section_control")
            if (not isinstance(section, Mapping) or section.get("section_id") != payload["section"]["id"]
                    or state.get("status") != "paused_after_group" or pre.get("closed_group_id") != state.get("group", {}).get("id")
                    or group.get("id") == state.get("group", {}).get("id")):
                raise LifecycleClosedError("open_group requires an accepted closed section child")

    def _validate_section_source_references(
        self, state: Mapping[str, Any], refs: Mapping[str, str], operation: str, *, successor: bool = False
    ) -> None:
        """Bind S0 source digests to immutable attestations, never command text."""
        receipt_ref = state.get("metadata", {}).get("section_control", {}).get("accepted_section_receipt")
        for kind, digest in refs.items():
            if (operation == "open_group" and kind == "lifecycle_prerequisite") or (successor and kind in {"lifecycle_prerequisite", "accepted_section_receipt", "checkpoint", "bundle"}):
                if not isinstance(receipt_ref, Mapping) or receipt_ref.get("digest") != digest:
                    if successor and kind in {"checkpoint", "bundle"}:
                        current = state.get("group", {}).get(kind + "_ref")
                        if isinstance(current, Mapping) and current.get("digest") == digest:
                            continue
                    raise AuthorizationError("accepted-section receipt is not canonical")
                receipt = self._load_ref_object(receipt_ref, "accepted section receipt")
                payload = receipt.get("payload")
                if (receipt.get("object_type") != "artifact" or not isinstance(payload, Mapping)
                        or payload.get("kind") != "section-transition-receipt"
                        or not isinstance(payload.get("payload"), Mapping)
                        or payload["payload"].get("schema") != "section-acceptance-receipt/v1"
                        or payload["payload"].get("section_id") != state.get("metadata", {}).get("section_control", {}).get("section_id")
                        or not isinstance(payload["payload"].get("acceptance_evidence"), list)
                        or not payload["payload"]["acceptance_evidence"]):
                    raise IntegrityBlockedError("accepted section receipt object is malformed")
                continue
            attested = False
            for artifact in state.get("artifacts", {}).values():
                if not isinstance(artifact, Mapping) or not isinstance(artifact.get("digest"), str):
                    continue
                source = self._load_object(artifact["digest"])
                value = source.get("payload")
                if (source.get("object_type") != "artifact" or not isinstance(value, Mapping)
                        or value.get("kind") != "s0-source-attestation" or not isinstance(value.get("payload"), Mapping)):
                    continue
                attestation = value["payload"]
                if set(attestation) != {"ref_kind", "source_digest", "source_path"}:
                    continue
                if attestation.get("ref_kind") != kind or attestation.get("source_digest") != digest:
                    continue
                path = attestation.get("source_path")
                if not isinstance(path, str) or not path:
                    continue
                try:
                    if Path(path).is_file() and _digest_bytes(Path(path).read_bytes()) == digest:
                        attested = True
                        break
                except OSError:
                    continue
            if not attested:
                raise AuthorizationError("section source reference is unattested: %s" % kind)

    def _validate_section_acceptance_evidence(
        self, state: Mapping[str, Any], evidence: Any, section_state: Mapping[str, Any]
    ) -> None:
        """Require canonical, current-Run accepted results before a section claim.

        A digest-shaped command value is not evidence.  The accepted boundary
        may only cite an immutable artifact already catalogued by the current
        HEAD state, with the section/group/run identity and passed outcome in
        its payload.  This deliberately does not infer identity from artifact
        names, command text, or a SHA-256 shape.
        """

        if (
            not isinstance(evidence, list)
            or not evidence
            or any(not isinstance(item, str) or not _DIGEST.fullmatch(item) for item in evidence)
            or len(set(evidence)) != len(evidence)
        ):
            raise AuthorizationError("section acceptance requires unique digest-bound evidence")
        section_id = section_state.get("section_id")
        group_id = state.get("group", {}).get("id")
        if not isinstance(section_id, str) or not isinstance(group_id, str):
            raise IntegrityBlockedError("section acceptance state identity is malformed")
        for digest in evidence:
            ref = state.get("object_refs", {}).get(digest)
            if not isinstance(ref, Mapping) or ref.get("object_type") != "artifact":
                raise AuthorizationError("section acceptance evidence is not a current-run immutable artifact")
            artifact = self._load_ref_object(ref, "section acceptance evidence")
            payload = artifact.get("payload")
            value = payload.get("payload") if isinstance(payload, Mapping) else None
            if (
                not isinstance(payload, Mapping)
                or payload.get("kind") != "section-accepted-result"
                or not isinstance(value, Mapping)
                or value.get("schema") != "section-accepted-result/v1"
                or value.get("run_id") != self.run_id
                or value.get("section_id") != section_id
                or value.get("group_id") != group_id
                or value.get("result") != "passed"
            ):
                raise AuthorizationError("section acceptance evidence does not bind a passed current section result")
            artifact_id = payload.get("artifact_id")
            catalogued = state.get("artifacts", {}).get(artifact_id)
            if not isinstance(catalogued, Mapping) or catalogued.get("digest") != digest:
                raise AuthorizationError("section acceptance evidence is not catalogued by the current HEAD")

    def _section_transition_reducer(self, state: Dict[str, Any], command: Mapping[str, Any], stage: Path) -> Dict[str, Any]:
        payload = command["payload"]
        group, section = payload["group"], payload["section"]
        closed_predecessor_group = _copy(state["group"])
        state["group"] = {"id": group["id"], "status": "open", "next_group": None}
        state["epoch"] = {"id": group["first_epoch"], "status": "open", "group_id": group["id"], "boundary_reason": command["command_type"], "clear_before_next": False}
        # The legacy kernel validates contexts against its single current
        # group.  Preserve cross-group schedule history in section_control;
        # retain only the current context in that legacy collection.
        state["epoch_contexts"] = {group["first_epoch"]: {"id": group["first_epoch"], "status": "open", "group_id": group["id"], "boundary_reason": command["command_type"], "started_at_revision": state["revision"] + 1, "input_ref": None}}
        state["status"] = "active"
        prior_section = state.setdefault("metadata", {}).get("section_control")
        successor = command["command_type"] == "open_section" and section["id"] != "S0"
        # A section is a new Group namespace.  Only open_group preserves the
        # current section's prior Group schedule; a successor archives its
        # accepted predecessor and starts with its first frontier alone.
        groups = {} if successor else (_copy(prior_section.get("groups", {})) if isinstance(prior_section, Mapping) else {})
        groups[group["id"]] = {"epoch_id": group["first_epoch"], "frontier": _copy(payload["first_frontier"])}
        section_state = {
            "schema": "section-control-state/v1", "section_id": section["id"],
            "workflow_id": section["workflow_id"], "version": section["version"],
            "groups": groups,
            "transition": _copy(payload["transition"]),
        }
        if successor and isinstance(prior_section, Mapping):
            history = _copy(prior_section.get("section_history", []))
            history.append({
                "position": len(history),
                "section_id": prior_section["section_id"],
                "accepted_receipt_ref": _copy(prior_section["accepted_section_receipt"]),
                "bundle_ref": _copy(closed_predecessor_group["bundle_ref"]),
                "checkpoint_ref": _copy(closed_predecessor_group["checkpoint_ref"]),
            })
            section_state["section_history"] = history
            section_state["parent_accepted_section_receipt"] = _copy(prior_section["accepted_section_receipt"])
        elif command["command_type"] == "open_group" and isinstance(prior_section, Mapping):
            section_state["section_history"] = _copy(prior_section.get("section_history", []))
            parent_receipt = prior_section.get("accepted_section_receipt")
            if isinstance(parent_receipt, Mapping):
                section_state["parent_accepted_section_receipt"] = _copy(parent_receipt)
        state.setdefault("metadata", {})["section_control"] = section_state
        return state

    def entry(
        self,
        run_id_or_objective: Optional[Union[str, Mapping[str, Any]]] = None,
        objective_ref: Optional[Mapping[str, Any]] = None,
        workflow_version: str = "manual-bootstrap/v1",
        *,
        group_id: str = "bootstrap",
        epoch_id: str = "epoch-0001",
        authority_ref: Optional[Mapping[str, Any]] = None,
        aliases: Optional[Iterable[str]] = None,
        external_refs: Optional[Iterable[str]] = None,
        review_budget: Optional[Mapping[str, Any]] = None,
        objective: Optional[Mapping[str, Any]] = None,
        authority: Optional[Mapping[str, Any]] = None,
    ) -> Dict[str, Any]:
        """Create the genesis transaction.

        Both ``entry(objective, ...)`` on ``ControlKernel(root, run_id)`` and
        ``entry(run_id, objective, ...)`` on ``ControlKernel(root)`` are
        accepted to keep the fixture API convenient.
        """

        if objective is not None:
            if objective_ref is not None and objective_ref != objective:
                raise KernelError("objective_ref and objective disagree")
            objective_ref = objective
        if authority is not None:
            if authority_ref is not None and authority_ref != authority:
                raise KernelError("authority_ref and authority disagree")
            authority_ref = authority
        if self.run_id is None:
            if not isinstance(run_id_or_objective, str) or objective_ref is None:
                raise KernelError("entry(run_id, objective_ref, ...) is required")
            self.run_id = _id(run_id_or_objective, "run_id")
            objective = objective_ref
        else:
            objective = run_id_or_objective if run_id_or_objective is not None else objective_ref
        if not isinstance(objective, Mapping):
            raise KernelError("objective_ref is required")
        _id(group_id, "group_id")
        _id(epoch_id, "epoch_id")
        if not isinstance(workflow_version, str) or not workflow_version:
            raise KernelError("workflow_version is required")
        authority = authority_ref
        if authority is None:
            raise AuthorizationError("entry requires explicit authority_ref")
        payload = {"objective_ref": _copy(objective)}
        state = self._empty_state(
            objective,
            workflow_version,
            group_id,
            epoch_id,
            authority,
            aliases or [],
            external_refs or [],
            review_budget,
        )
        command = self._make_command(
            "entry",
            authority_ref=authority,
            payload=payload,
            idempotency_key="entry:" + self.run_id,
        )
        command["workflow_version"] = workflow_version
        return self._commit(command, self._entry_reducer, initial=True, initial_state=state)

    @staticmethod
    def _bare_digest(value: Any) -> str:
        if not isinstance(value, str):
            raise IntegrityBlockedError("artifact digest is missing")
        digest = value.split(":", 1)[1] if value.startswith("sha256:") else value
        if not re.fullmatch(r"[0-9a-f]{64}", digest):
            raise IntegrityBlockedError("artifact digest is not SHA-256 hex")
        return digest

    def _bundle_artifacts(self, state: Mapping[str, Any]) -> List[Dict[str, Any]]:
        result: List[Dict[str, Any]] = []
        for artifact_id, artifact in sorted(state.get("artifacts", {}).items()):
            if not isinstance(artifact, Mapping):
                raise IntegrityBlockedError("artifact record is malformed: %s" % artifact_id)
            path = artifact.get("path") or "artifacts/%s" % artifact_id
            version = artifact.get("version")
            if not isinstance(path, str) or not path:
                raise IntegrityBlockedError("artifact path is malformed: %s" % artifact_id)
            try:
                self._scope_paths(path, "artifact path")
            except KernelError as exc:
                raise IntegrityBlockedError("artifact path is malformed: %s" % artifact_id) from exc
            if "*" in path:
                raise IntegrityBlockedError("artifact path must be concrete: %s" % artifact_id)
            if not isinstance(version, str) or not version:
                raise IntegrityBlockedError("artifact version is missing: %s" % artifact_id)
            result.append({"path": path, "version": version, "digest": self._bare_digest(artifact.get("digest"))})
        return result

    def _build_bundle_payload(
        self,
        state: Mapping[str, Any],
        payload: Mapping[str, Any],
        context_epoch: Mapping[str, Any],
        bundle_id: str,
    ) -> Dict[str, Any]:
        body = {
            "schema": "artifact-bundle/v1",
            "bundle_id": bundle_id,
            "version": "v3",
            "run_id": self.run_id,
            "workflow_version": state["workflow_version"],
            "group_id": state["group"]["id"],
            # A closed Group bundle still describes the Epoch that produced
            # it.  Keep both identities and a lock-held state snapshot key so
            # a detached Bundle cannot be reused for another boundary.
            "epoch_id": state["epoch"]["id"],
            "closure_revision": context_epoch.get("closed_at_revision"),
            "state_ref": {
                "run_id": self.run_id,
                "revision": context_epoch.get("closed_at_revision"),
                "state_revision": context_epoch.get("closed_at_revision"),
            },
            "context_epoch": _copy(context_epoch),
            "canonical_artifacts": self._bundle_artifacts(state),
            "acceptance_evidence": _copy(payload.get("acceptance_evidence", [])),
            "approved_decisions": _copy(payload.get("approved_decisions", [])),
            "unresolved_items": _copy(payload.get("unresolved_items", [])),
            "invalidated_artifacts": _copy(payload.get("invalidated_artifacts", [])),
            "next_inputs": _copy(payload.get("next_inputs", [])),
            "context_budget": _copy(payload.get("context_budget") or state.get("context_budget")),
        }
        self._ensure_durable_payload(body, "artifact bundle")
        body["digest"] = self._bare_digest(_digest(body))
        return body

    def _review_terminal_reason(self, state: Mapping[str, Any], operation: str, finding_id: Optional[str] = None) -> Optional[str]:
        budget = state["review_budget"]
        now = _datetime.datetime.fromisoformat(_now().replace("Z", "+00:00"))
        deadline = _datetime.datetime.fromisoformat(str(budget["deadline"]).replace("Z", "+00:00"))
        if now >= deadline:
            return "time_exhausted"
        if operation == "open_review" and budget["rounds_used"] >= budget["max_rounds"]:
            return "rounds_exhausted"
        if operation == "accept_resolution_claim" and finding_id is not None and budget["finding_attempts"].get(finding_id, 0) >= budget["max_attempts_per_finding"]:
            return "attempts_exhausted"
        return None

    def _terminalize_review(self, state: Dict[str, Any], stage: Path, reason: str, unresolved: Optional[Sequence[str]] = None) -> None:
        if state.get("budget_terminal") is not None:
            return
        unresolved_ids = sorted(set(unresolved or [key for key, finding in state["findings"].items() if self._finding_blocks(finding)]))
        success_refs = []
        for task in state["tasks"].values():
            if task.get("result_status") == "success" and isinstance(task.get("result_ref"), Mapping):
                success_refs.append(_copy(task["result_ref"]))
        for finding in state["findings"].values():
            if isinstance(finding.get("closure_ref"), Mapping):
                success_refs.append(_copy(finding["closure_ref"]))
        terminal = {"reason": reason, "unresolved_finding_ids": unresolved_ids, "success_refs": success_refs, "budget": _copy(state["review_budget"]), "terminal_at_revision": state["revision"] + 1}
        ref = self._new_object(stage, "budget-terminal", terminal)
        self._add_object_ref(state, ref)
        terminal["object_ref"] = ref
        state["budget_terminal"] = terminal
        state.setdefault("terminal_history", []).append(_copy(terminal))
        state["status"] = "needs_user" if reason == "needs_user" else "terminal"
        for task_id, task in state["tasks"].items():
            if task.get("status") not in {"succeeded", "partial", "failed", "needs_decision"}:
                task["invalidated"] = True
                task["stop_requested"] = True
                task.setdefault("freshness", {})["invalidated"] = True
                task["status"] = "invalidated"
            if task_id in state.get("leases", {}):
                state["leases"].pop(task_id)
            if task.get("lease_status") == "leased":
                task["lease_status"] = "released"

    def _generic_reducer(self, state: Dict[str, Any], command: Mapping[str, Any], stage: Optional[Path]) -> Dict[str, Any]:
        operation = command["command_type"]
        payload = command["payload"]
        # Source fixtures are copied values, not a bypass around the durable
        # boundary.  Validate the complete payload before any object is staged.
        self._ensure_durable_payload(payload, operation)
        if operation in {"claim_task", "open_review", "validate_findings", "accept_resolution_claim"}:
            reason = self._review_terminal_reason(state, operation, payload.get("finding_id"))
            if reason is not None:
                self._terminalize_review(state, stage, reason)
                return state
        if operation in {"open_section", "open_group"}:
            return self._section_transition_reducer(state, command, stage)  # type: ignore[arg-type]
        if operation not in {"entry", "migrate_legacy"}:
            if state.get("group", {}).get("status") != "open" or state.get("status") in {"closed", "completed", "paused_after_group"}:
                raise LifecycleClosedError("closed Group/Run does not accept mutation")
            if state.get("epoch", {}).get("status") != "open" and operation not in {"open_epoch", "close_group"}:
                raise LifecycleClosedError("closed Epoch does not accept mutation")
        if operation == "approve_objective":
            candidate = _copy(payload["candidate_ref"])
            approval = _copy(payload["approval"])
            approval_id = approval["approval_id"]
            if approval_id in state.setdefault("objective_approvals", {}):
                raise KernelError("objective approval ID is immutable: %s" % approval_id)
            prior = _copy(state["objective_ref"])
            candidate_ref = self._new_object(stage, "objective-candidate", candidate)  # type: ignore[arg-type]
            self._add_object_ref(state, candidate_ref)
            approval_ref = self._new_object(stage, "objective-approval", approval)  # type: ignore[arg-type]
            self._add_object_ref(state, approval_ref)
            approved_ref = self._new_object(
                stage, "approved-objective",
                {"candidate_ref": candidate_ref, "objective": candidate, "approval_ref": approval_ref},
            )  # type: ignore[arg-type]
            self._add_object_ref(state, approved_ref)
            event = {
                "event": "objective-approved", "approval_id": approval_id,
                "run_id": self.run_id, "namespace": approval["namespace"],
                "approval_scope": approval["approval_scope"],
                "actor_id": approval["actor"]["actor_id"],
                "candidate_ref": candidate_ref, "approval_ref": approval_ref,
                "prior_objective": prior, "approved_version": candidate["version"],
            }
            event_ref = self._new_object(stage, "objective-approval-event", event)  # type: ignore[arg-type]
            self._add_object_ref(state, event_ref)
            state.setdefault("objective_history", []).append(prior)
            state["objective_approvals"][approval_id] = {
                "approval_ref": approval_ref, "candidate_ref": candidate_ref,
                "approved_ref": approved_ref, "event_ref": event_ref,
            }
            state.setdefault("objective_events", []).append({**event, "object_ref": event_ref})
            state["objective_ref"] = {
                **candidate, "object_digest": approved_ref["digest"],
                "candidate_ref": candidate_ref, "approval_ref": approval_ref, "event_ref": event_ref,
            }
            state.setdefault("nodes", {})["objective:" + candidate["version"]] = "objective"
            state["nodes"]["approval:" + approval_id] = "approval"
        elif operation == "publish_artifact":
            artifact_id = _id(payload["artifact_id"], "artifact_id")
            version = _id(payload["version"], "artifact_version")
            if artifact_id in state["artifacts"]:
                raise KernelError("artifact_id is immutable: %s" % artifact_id)
            artifact_path = payload.get("path") or "artifacts/%s" % artifact_id
            if not isinstance(artifact_path, str):
                raise AuthorizationError("artifact path escapes the managed scope")
            try:
                self._scope_paths(artifact_path, "artifact path")
            except KernelError as exc:
                raise AuthorizationError("artifact path escapes the managed scope") from exc
            if "*" in artifact_path:
                raise AuthorizationError("artifact path must be concrete")
            ref = self._new_object(stage, "artifact", {"artifact_id": artifact_id, "version": version, "kind": payload.get("kind", "artifact"), "payload": payload.get("value")})  # type: ignore[arg-type]
            self._add_object_ref(state, ref)
            state["artifacts"][artifact_id] = {
                "artifact_id": artifact_id,
                "version": version,
                "digest": ref["digest"],
                "object_ref": ref,
                "path": artifact_path,
                "status": "available",
                "epoch_id": state["epoch"]["id"],
            }
            state.setdefault("nodes", {})["artifact:" + artifact_id] = "artifact"
            self._record_authority_edge(state, command, "artifact:" + artifact_id)
        elif operation == "publish_task_package":
            task_id = _id(payload["task_id"], "task_id")
            if task_id in state["tasks"]:
                raise KernelError("task_id is immutable: %s" % task_id)
            assignment = _copy(payload.get("assignment") or {"role": "worker", "assignment_id": "worker-" + task_id})
            if (
                not isinstance(assignment, Mapping)
                or assignment.get("role") != "worker"
                or not isinstance(assignment.get("assignment_id"), str)
                or not assignment.get("assignment_id")
            ):
                raise AuthorizationError("task assignment role and id are required")
            package = _copy(payload["package"])
            try:
                self._validate_task_package_shape(package, "task package")
            except CommandValidationError as exc:
                raise ObjectValidationError(str(exc)) from exc
            self._ensure_durable_payload(package, "task package")
            self._validate_task_output_scope(package, command.get("authority_ref") or {})
            if package.get("freshness", {}).get("epoch_id") != state["epoch"]["id"]:
                raise ReviewProvenanceError("task package freshness does not match current Epoch")
            created_at_revision = package.get("freshness", {}).get("created_at_revision")
            if (
                not isinstance(created_at_revision, int)
                or isinstance(created_at_revision, bool)
                or created_at_revision < 1
                or created_at_revision > state["revision"] + 1
            ):
                raise StaleHeadError("task package freshness revision is future")
            if package.get("assignment") != assignment:
                raise AuthorizationError("task package assignment does not match command")
            if package.get("input_refs") != payload.get("input_refs"):
                raise ObjectValidationError("task package input_refs do not match command")
            ref = self._new_object(stage, "task-package", package)  # type: ignore[arg-type]
            self._add_object_ref(state, ref)
            state["tasks"][task_id] = {
                "task_id": task_id,
                "attempt_id": package.get("attempt_id", "attempt-" + task_id),
                "package_ref": ref,
                "assignment": assignment,
                "input_refs": _copy(package.get("input_refs", [])),
                "write_scope": _copy(package.get("write_scope", [])),
                "freshness": _copy(package.get("freshness")),
                "stop_conditions": _copy(package.get("stop_conditions", [])),
                "invalidated": bool(package.get("invalidated", False)),
                "stop_requested": bool(package.get("stop_requested", False)),
                "output_path": _copy(package.get("output_path")),
                "status": "ready",
                "epoch_id": state["epoch"]["id"],
                "sibling_group": payload.get("sibling_group"),
                # Keep the authority object that authorized this package in
                # the canonical Task record.  It is checked again by the
                # readiness compiler and claim reducer, rather than treating
                # an ``authorizes`` edge as a perpetual capability.
                "authority_ref": _copy(command.get("authority_ref")),
                "authority_digest": _digest(command.get("authority_ref")),
                "authority_expected_head": _copy(command.get("authority_ref", {}).get("expected_head")) if isinstance(command.get("authority_ref"), Mapping) else None,
                "authority_expected_head_revision": command.get("authority_ref", {}).get("expected_head_revision") if isinstance(command.get("authority_ref"), Mapping) else None,
                "proposal_digest": command.get("authority_ref", {}).get("proposal_digest") if isinstance(command.get("authority_ref"), Mapping) else None,
                "lease_status": "released",
                "result_ref": None,
                "graph_revision": state.get("graph_revision"),
                "replacement_task_id": None,
                "dependency_closure": [],
                "cancel_requested": False,
                "quarantine_reason": None,
                "quarantine_refs": [],
            }
            state["nodes"] = state.get("nodes", {})
            state["nodes"]["task:" + task_id] = "task"
            self._record_authority_edge(state, command, "task:" + task_id)
            for input_ref in payload.get("input_refs", []):
                if not isinstance(input_ref, Mapping):
                    raise IntegrityBlockedError("task input reference is malformed")
                safe_input = _safe_ref(input_ref, "task input reference")
                loaded_input = self._validate_input_ref_binding(state, input_ref, "task input reference")
                node = input_ref.get("node_id")
                if not node and input_ref.get("artifact_id"):
                    node = "artifact:" + str(input_ref["artifact_id"])
                if not node and input_ref.get("task_id"):
                    node = "task:" + str(input_ref["task_id"])
                if not node and (input_ref.get("digest") or input_ref.get("object_digest")):
                    ref_digest = input_ref.get("digest") or input_ref.get("object_digest")
                    for artifact_id, artifact in state.get("artifacts", {}).items():
                        if ref_digest in {
                            artifact.get("digest"),
                            (artifact.get("object_ref") or {}).get("digest"),
                        }:
                            node = "artifact:" + str(artifact_id)
                            break
                    if not node:
                        for required_task_id, required_task in state.get("tasks", {}).items():
                            if ref_digest == (required_task.get("package_ref") or {}).get("digest"):
                                node = "task:" + str(required_task_id)
                                break
                    if not node:
                        for required_task_id, required_task in state.get("tasks", {}).items():
                            if ref_digest == (required_task.get("result_ref") or {}).get("digest"):
                                node = "work-product:" + str(required_task_id)
                                break
                if node:
                    if node not in state.get("nodes", {}):
                        raise IntegrityBlockedError("task input references an unknown DAG node: %s" % node)
                    # A package object is a task's assignment, not a
                    # produced execution artifact.  Do not compile a
                    # forbidden Task -> Task edge from a bare package ref;
                    # callers must reference a work-product/artifact for a
                    # real dependency.  Explicit node claims remain errors.
                    if str(node).startswith("task:"):
                        if input_ref.get("node_id"):
                            raise IntegrityBlockedError("Task-to-Task input edges are forbidden; require a work-product/artifact")
                        if loaded_input.get("object_type") == "task-package":
                            continue
                        raise IntegrityBlockedError("Task-to-Task input edges are forbidden; require a work-product/artifact")
                    self._edge(state, str(node), "task:" + task_id, "requires")
                elif not input_ref.get("external"):
                    if loaded_input.get("object_type") == "task-package":
                        # Preserve the object-bound input for provenance while
                        # refusing to turn it into a Task-to-Task dependency.
                        continue
                    raise IntegrityBlockedError("task input reference cannot be resolved to a DAG node")
        elif operation == "migrate_legacy":
            # A converter may carry a copied A6 Run/Bundle/worker report into
            # one immutable migration transaction.  The payload is retained
            # as an object for auditability; it is never read from or written
            # back to the source tree by the kernel.
            if state.get("migration") is not None:
                raise LifecycleClosedError("legacy migration binding is immutable after first publication")
            migration = _copy(payload.get("migration") or {})
            if not isinstance(migration, Mapping):
                raise IntegrityBlockedError("legacy migration record is malformed")
            # ``apply`` is public and can be called without LegacyConverter.
            # Re-attest the physical bytes at the reducer boundary as well as
            # during envelope validation, so a mapping-only caller or a
            # source mutation between validation and staging cannot enter the
            # canonical history.
            self._validate_migration_attestation(migration, verify_physical=True)
            source_revision = migration.get("source_revision")
            if not isinstance(source_revision, int) or isinstance(source_revision, bool) or source_revision < 1:
                raise IntegrityBlockedError("legacy source revision is malformed")
            source_digests = _copy(migration.get("source_digests") or {})
            if not isinstance(source_digests, Mapping) or set(source_digests) != {"run", "bundle", "worker_report"}:
                raise IntegrityBlockedError("legacy source digests must bind all copied sources")
            for source_name, source_digest in source_digests.items():
                if not isinstance(source_digest, str) or not _DIGEST.fullmatch(source_digest):
                    raise IntegrityBlockedError("legacy source digest is malformed: %s" % source_name)
            bindings = migration.get("source_bindings")
            binding_required = {"source_run_id", "group_id", "epoch_id", "aliases", "source_revision", "run_status", "source_status", "epoch_status"}
            if not isinstance(bindings, Mapping) or set(bindings) != binding_required:
                raise IntegrityBlockedError("legacy source bindings are incomplete")
            if bindings.get("source_revision") != source_revision or bindings.get("run_status") not in {"open", "closed"} or not isinstance(bindings.get("source_status"), str) or not bindings.get("source_status") or bindings.get("epoch_status") != "closed":
                raise IntegrityBlockedError("legacy source closure/revision binding is invalid")
            if not isinstance(bindings.get("aliases"), list) or any(not isinstance(alias, str) or not alias for alias in bindings["aliases"]):
                raise IntegrityBlockedError("legacy source aliases are malformed")
            snapshot = migration.get("source_snapshot")
            if (
                not isinstance(snapshot, Mapping)
                or set(snapshot) != {"mode", "source_digests", "source_bindings"}
                or snapshot.get("mode") != "immutable"
                or snapshot.get("source_digests") != source_digests
                or snapshot.get("source_bindings") != bindings
            ):
                raise IntegrityBlockedError("legacy immutable source snapshot binding is invalid")
            source_run = migration.get("run")
            source_bundle = migration.get("bundle")
            source_report = migration.get("worker_report")
            if not isinstance(source_run, Mapping) or not isinstance(source_bundle, Mapping) or not isinstance(source_report, Mapping):
                raise IntegrityBlockedError("legacy source objects are incomplete")
            self._validate_legacy_source_binding(
                source_run,
                source_bundle,
                source_report,
                bindings,
                source_revision,
                IntegrityBlockedError,
            )

            def _mapping(value: Mapping[str, Any], *keys: str) -> Optional[Mapping[str, Any]]:
                for key in keys:
                    candidate = value.get(key)
                    if isinstance(candidate, Mapping):
                        return candidate
                return None

            def _scalar(value: Mapping[str, Any], *keys: str) -> Any:
                for key in keys:
                    candidate = value.get(key)
                    if candidate is not None:
                        return candidate
                return None

            source_run_id = _scalar(source_run, "run_id")
            if source_run_id is not None and source_run_id != bindings.get("source_run_id"):
                raise IntegrityBlockedError("legacy Run ID binding is invalid")
            bundle_run_id = _scalar(source_bundle, "run_id")
            if bundle_run_id is not None and bundle_run_id != bindings.get("source_run_id"):
                raise IntegrityBlockedError("legacy Bundle Run binding is invalid")
            report_run_id = _scalar(source_report, "run_id")
            if report_run_id is not None and report_run_id != bindings.get("source_run_id"):
                raise IntegrityBlockedError("legacy worker report Run binding is invalid")

            run_group = _mapping(source_run, "current_group", "group")
            if run_group is not None:
                if (run_group.get("id") or run_group.get("group_id")) != bindings.get("group_id"):
                    raise IntegrityBlockedError("legacy Group binding is invalid")
                run_status = run_group.get("status")
                if run_status is not None and run_status != bindings.get("source_status"):
                    raise IntegrityBlockedError("legacy source status binding is invalid")
                if run_status in ("open", "closed") and run_status != bindings.get("run_status"):
                    raise IntegrityBlockedError("legacy Group status binding is invalid")
            bundle_group = _scalar(source_bundle, "group_id", "current_group", "group")
            if isinstance(bundle_group, Mapping):
                bundle_group = bundle_group.get("id") or bundle_group.get("group_id")
            if bundle_group is not None and bundle_group != bindings.get("group_id"):
                raise IntegrityBlockedError("legacy Bundle Group binding is invalid")
            bundle_group_status = source_bundle.get("group_status")
            if bundle_group_status in ("open", "closed") and bundle_group_status != bindings.get("run_status"):
                raise IntegrityBlockedError("legacy Bundle Group status binding is invalid")

            run_epoch = _mapping(source_run, "current_epoch", "context_epoch", "epoch")
            if run_epoch is not None:
                if (run_epoch.get("id") or run_epoch.get("epoch_id")) != bindings.get("epoch_id"):
                    raise IntegrityBlockedError("legacy Epoch binding is invalid")
                if run_epoch.get("status") in ("open", "closed") and run_epoch.get("status") != bindings.get("epoch_status"):
                    raise IntegrityBlockedError("legacy Epoch status binding is invalid")
                if run_epoch.get("clear_before_next", run_epoch.get("clear_before_start")) is not True:
                    raise IntegrityBlockedError("legacy Epoch clear binding is invalid")
            bundle_epoch = _mapping(source_bundle, "context_epoch", "current_epoch", "epoch")
            if bundle_epoch is not None:
                if (bundle_epoch.get("id") or bundle_epoch.get("epoch_id")) != bindings.get("epoch_id") or bundle_epoch.get("status", "closed") != "closed" or bundle_epoch.get("clear_before_next", bundle_epoch.get("clear_before_start")) is not True:
                    raise IntegrityBlockedError("legacy Bundle closure binding is invalid")

            report_group = _scalar(source_report, "group_id", "group")
            if isinstance(report_group, Mapping):
                report_group = report_group.get("id") or report_group.get("group_id")
            if report_group is not None and report_group != bindings.get("group_id"):
                raise IntegrityBlockedError("legacy worker report Group binding is invalid")
            report_epoch = _scalar(source_report, "context_epoch", "epoch_id", "epoch")
            if isinstance(report_epoch, Mapping):
                report_epoch = report_epoch.get("id") or report_epoch.get("epoch_id")
            if report_epoch is not None and report_epoch != bindings.get("epoch_id"):
                raise IntegrityBlockedError("legacy worker report Epoch binding is invalid")
            report_status = source_report.get("status")
            if report_status is not None and report_status not in ("done", "partial", "blocked"):
                raise IntegrityBlockedError("legacy worker report closure binding is invalid")
            for source_name, source in (("Run", source_run), ("Bundle", source_bundle), ("worker report", source_report)):
                aliases = source.get("aliases")
                if aliases is None and isinstance(source.get("metadata"), Mapping):
                    aliases = source["metadata"].get("aliases")
                if aliases is not None and aliases != bindings.get("aliases"):
                    raise IntegrityBlockedError("legacy %s aliases binding is invalid" % source_name)
            field_mapping = migration.get("field_mapping") or {}
            if not isinstance(field_mapping, Mapping):
                raise IntegrityBlockedError("legacy field mapping is malformed")
            refs = []
            for object_type, key in (
                ("legacy-run", "run"),
                ("legacy-bundle", "bundle"),
                ("legacy-worker-report", "worker_report"),
            ):
                value = migration.get(key)
                ref = self._new_object(stage, object_type, {"source_digest": source_digests.get(key), "value": value})  # type: ignore[arg-type]
                self._add_object_ref(state, ref)
                refs.append(ref)
            state["migration"] = {
                "schema": "legacy-migration/v1",
                "source_revision": source_revision,
                "source_digests": source_digests,
                "source_bindings": _copy(bindings),
                "source_snapshot": _copy(snapshot),
                "source_attestation": _copy(migration.get("source_attestation")),
                "source_refs": refs,
                "field_mapping": _copy(field_mapping),
                "history_preserved": True,
                "cutover_status": "candidate",
            }
            # The copied A6 boundary is authoritative for the initial
            # lifecycle snapshot.  Do not reopen a source Epoch merely
            # because genesis starts with an open one: a conversion of a
            # closed A6 Epoch must resume paused at that exact boundary.
            source_group_status = bindings.get("run_status")
            source_epoch_status = bindings.get("epoch_status")
            state["group"]["status"] = source_group_status
            state["epoch"].update(
                {
                    "status": source_epoch_status,
                    "clear_before_next": True,
                    "boundary_reason": "legacy-migration",
                    "closed_at_revision": state["revision"] + 1,
                }
            )
            migrated_context = state.get("epoch_contexts", {}).get(state["epoch"]["id"])
            if isinstance(migrated_context, Mapping):
                migrated_context.update(
                    {
                        "status": source_epoch_status,
                        "boundary_reason": "legacy-migration",
                        "clear_before_next": True,
                        "closed_at_revision": state["revision"] + 1,
                    }
                )
            state["status"] = "paused_after_group" if source_group_status == "closed" else "paused_after_epoch"
            if source_group_status == "closed":
                state["group"]["clear_required"] = True
            # Preserve the old semantic records as references, while making
            # no attempt to reinterpret completed history as new attempts.
            for old_id, old_artifact in (migration.get("artifacts") or {}).items():
                raw_artifact_id = str(old_id)
                artifact_id = raw_artifact_id if _ID.fullmatch(raw_artifact_id) else "legacy-" + hashlib.sha256(raw_artifact_id.encode("utf-8")).hexdigest()[:16]
                artifact_id = _id(artifact_id, "legacy artifact_id")
                if artifact_id in state["artifacts"]:
                    continue
                ref = self._new_object(stage, "legacy-artifact", {"legacy_id": artifact_id, "value": old_artifact})  # type: ignore[arg-type]
                self._add_object_ref(state, ref)
                state["artifacts"][artifact_id] = {
                    "artifact_id": artifact_id,
                    "version": str(old_artifact.get("version", "legacy")) if isinstance(old_artifact, Mapping) else "legacy",
                    "path": old_artifact.get("path", "artifacts/%s" % artifact_id) if isinstance(old_artifact, Mapping) else "artifacts/%s" % artifact_id,
                    "digest": ref["digest"],
                    "object_ref": ref,
                    "status": "available",
                    "legacy": True,
                    "epoch_id": state["epoch"]["id"],
                }
                state.setdefault("nodes", {})["artifact:" + artifact_id] = "artifact"
            # A converted closed source boundary must retain the same durable
            # recovery evidence as a native close.  Build a new immutable
            # Bundle/Checkpoint object from the copied snapshot before the
            # migration transaction can publish its paused state.
            closed_at_revision = state["revision"] + 1
            migration_context = _copy(state["epoch"])
            migration_context.update(
                {
                    "status": "closed",
                    "clear_before_next": True,
                    "boundary_reason": "legacy-migration",
                    "closed_at_revision": closed_at_revision,
                }
            )
            bundle_payload = self._build_bundle_payload(
                state,
                {
                    "acceptance_evidence": ["legacy source snapshot"],
                    "approved_decisions": [],
                    "unresolved_items": [],
                    "invalidated_artifacts": [],
                    "next_inputs": [],
                    "context_budget": state.get("context_budget"),
                },
                migration_context,
                "%s-%s-migration-bundle" % (state["group"]["id"], state["epoch"]["id"]),
            )
            bundle_ref = self._new_object(stage, "artifact-bundle", bundle_payload)  # type: ignore[arg-type]
            self._add_object_ref(state, bundle_ref)
            checkpoint_payload = {
                "schema": "checkpoint/v1",
                "checkpoint_id": "cp-migration-%s" % state["epoch"]["id"],
                "run_id": self.run_id,
                "workflow_version": state["workflow_version"],
                "group_id": state["group"]["id"],
                "epoch_id": state["epoch"]["id"],
                "state_revision": closed_at_revision,
                "closure_revision": closed_at_revision,
                "state_ref": {
                    "run_id": self.run_id,
                    "revision": closed_at_revision,
                    "state_revision": closed_at_revision,
                },
                "bundle_ref": bundle_ref,
                "clear_before_start": True,
            }
            checkpoint_ref = self._new_object(stage, "checkpoint", checkpoint_payload)  # type: ignore[arg-type]
            self._add_object_ref(state, checkpoint_ref)
            state["epoch"].update(
                {
                    "bundle_ref": bundle_ref,
                    "checkpoint_ref": checkpoint_ref,
                    "closed_at_revision": closed_at_revision,
                }
            )
            if isinstance(migrated_context, Mapping):
                migrated_context.update(
                    {
                        "bundle_ref": bundle_ref,
                        "checkpoint_ref": checkpoint_ref,
                        "closed_at_revision": closed_at_revision,
                    }
                )
            if source_group_status == "closed":
                state["group"].update(
                    {
                        "bundle_ref": bundle_ref,
                        "checkpoint_ref": checkpoint_ref,
                        "closed_at_revision": closed_at_revision,
                    }
                )
        elif operation == "accept_task_result":
            task_id = payload["task_id"]
            task = state["tasks"].get(task_id)
            if task is None:
                raise NotFoundError("unknown task: %s" % task_id)
            if task.get("invalidated") or task.get("cancel_requested") or task.get("quarantine_reason"):
                # The worker may still return after an external action was
                # cancelled.  Keep that result outside the canonical graph;
                # accepting it would silently resurrect stale work.
                raise ResultAcceptanceError("late task result is quarantined after invalidation")
            if task.get("status") not in ("running", "leased"):
                raise ResultAcceptanceError("task result requires a running Task lease")
            if task.get("epoch_id") != state.get("epoch", {}).get("id"):
                raise ResultAcceptanceError("task result is stale for the current Epoch")
            freshness = task.get("freshness")
            if (
                not isinstance(freshness, Mapping)
                or freshness.get("epoch_id") != state.get("epoch", {}).get("id")
                or freshness.get("stale")
                or freshness.get("invalidated")
            ):
                raise ResultAcceptanceError("task result has stale or invalidated freshness")
            worker_assignment = payload.get("worker_assignment_id")
            expected_assignment = task["assignment"].get("assignment_id")
            if worker_assignment != expected_assignment:
                raise AuthorizationError("task assignment mismatch")
            lease = state.get("leases", {}).get(task_id)
            if not isinstance(lease, Mapping) or lease.get("assignment_id") != worker_assignment:
                raise ResultAcceptanceError("task result requires the worker's live lease")
            result = _copy(payload.get("result") or {})
            self._validate_scope(self._result_scope_paths(result), command.get("authority_ref") or {}, label="result.write_scope")
            assigned_scope = self._scope_paths(task.get("write_scope"), "task.write_scope")
            for path in self._result_scope_paths(result):
                if assigned_scope and not any(self._scope_contains(prefix, path) for prefix in assigned_scope):
                    raise AuthorizationError("result path is outside task write scope: %s" % path)
            if task.get("result_ref") is not None or task.get("status") in ("succeeded", "failed", "partial"):
                raise KernelError("task result is already accepted: %s" % task_id)
            result_status = result.get("status", "success")
            if result_status not in ("success", "partial", "failure", "failed", "needs_decision"):
                raise KernelError("invalid task result status")
            ref = self._new_object(stage, "work-product", {"task_id": task_id, "result": result})  # type: ignore[arg-type]
            self._add_object_ref(state, ref)
            work_node = "work-product:" + task_id
            state.setdefault("nodes", {})[work_node] = "work-product"
            self._edge(state, "task:" + task_id, work_node, "produces")
            task["result_ref"] = ref
            task["result_status"] = result_status
            task["status"] = (
                "succeeded"
                if result_status == "success"
                else "partial"
                if result_status == "partial"
                else "needs_decision"
                if result_status == "needs_decision"
                else "failed"
            )
            task["result_submitted_at_revision"] = state["revision"] + 1
            task["result_state"] = "result_submitted"
            task["completed_by"] = worker_assignment
            state.setdefault("leases", {}).pop(task_id, None)
            task["lease_status"] = "released"
        elif operation == "open_review_epoch":
            epoch_id = _id(payload["epoch_id"], "epoch_id")
            if epoch_id in state.get("epoch_contexts", {}):
                raise KernelError("Context Epoch ID is immutable: %s" % epoch_id)
            input_ref = _safe_ref(payload["input_ref"], "review epoch input_ref")
            loaded_input = self._validate_input_ref_binding(state, input_ref, "review epoch input_ref")
            if input_ref.get("object_type") is not None and loaded_input.get("object_type") != input_ref.get("object_type"):
                raise ReviewProvenanceError("review Epoch input object type does not match reference")
            boundary_reason = payload.get("boundary_reason") or "review-attempt"
            if not isinstance(boundary_reason, str) or not boundary_reason:
                raise KernelError("review epoch boundary_reason is required")
            reviewer_assignment_id = payload.get("reviewer_assignment_id")
            if not isinstance(reviewer_assignment_id, str) or not _ID.fullmatch(reviewer_assignment_id):
                raise ReviewProvenanceError("registered independent reviewer assignment is required")
            candidate_task_id = None
            for task_id, task in state.get("tasks", {}).items():
                if (task.get("package_ref") or {}).get("digest") == input_ref["digest"]:
                    candidate_task_id = task_id
                    break
            finding_id = None
            if candidate_task_id is None:
                for item_id, finding in state.get("findings", {}).items():
                    if (finding.get("resolution_ref") or {}).get("digest") == input_ref["digest"]:
                        candidate_task_id = finding.get("candidate_task_id")
                        finding_id = item_id
                        break
            if candidate_task_id is None:
                raise ReviewProvenanceError("review Epoch input is not bound to a Task package or Finding resolution")
            candidate_task = state.get("tasks", {}).get(candidate_task_id)
            if isinstance(candidate_task, Mapping) and reviewer_assignment_id == candidate_task.get("assignment", {}).get("assignment_id"):
                raise ReviewProvenanceError("reviewer must be independent from candidate worker")
            state.setdefault("epoch_contexts", {})[epoch_id] = {
                "id": epoch_id,
                "status": "open",
                "group_id": state["group"]["id"],
                "boundary_reason": boundary_reason,
                "started_at_revision": state["revision"] + 1,
                "input_ref": input_ref,
                "candidate_task_id": candidate_task_id,
                "finding_id": finding_id,
                "reviewer_assignment_id": reviewer_assignment_id,
            }
        elif operation in ("open_review", "accept_review"):
            review_id = _id(payload["review_id"], "review_id")
            candidate_task_id = payload["candidate_task_id"]
            task = state["tasks"].get(candidate_task_id)
            if task is None:
                raise NotFoundError("unknown candidate task: %s" % candidate_task_id)
            reviewer_assignment = payload["reviewer_assignment_id"]
            if not isinstance(reviewer_assignment, str) or not reviewer_assignment:
                raise AuthorizationError("reviewer assignment is required")
            if reviewer_assignment == task["assignment"].get("assignment_id"):
                raise AuthorizationError("reviewer must be independent from worker")
            fresh_epoch = payload.get("fresh_epoch_id")
            if not fresh_epoch or fresh_epoch == task.get("epoch_id"):
                raise AuthorizationError("review must use a fresh Context Epoch")
            epoch_context = state.get("epoch_contexts", {}).get(fresh_epoch)
            if not isinstance(epoch_context, Mapping) or epoch_context.get("status") != "open":
                raise AuthorizationError("review must reference a registered fresh Context Epoch")
            if epoch_context.get("group_id") != state["group"]["id"]:
                raise AuthorizationError("review Context Epoch belongs to another Group")
            if epoch_context.get("candidate_task_id") != candidate_task_id:
                raise ReviewProvenanceError("review Context Epoch is bound to another candidate")
            if epoch_context.get("reviewer_assignment_id") != reviewer_assignment:
                raise ReviewProvenanceError("reviewer assignment is not registered for this Epoch")
            review_kind = payload.get("review_kind", "initial")
            if review_kind not in ("initial", "closure"):
                raise KernelError("unsupported review kind")
            target_finding_id = payload.get("target_finding_id")
            if review_kind == "closure":
                if not isinstance(target_finding_id, str):
                    raise AuthorizationError("closure review must target a Finding")
                target = state["findings"].get(target_finding_id)
                if target is None or target.get("candidate_task_id") != candidate_task_id:
                    raise AuthorizationError("closure review Finding provenance mismatch")
                if target.get("state") != "resolved":
                    raise AuthorizationError("closure review requires a worker resolution claim")
                if epoch_context.get("finding_id") != target_finding_id or epoch_context.get("input_ref", {}).get("digest") != (target.get("resolution_ref") or {}).get("digest"):
                    raise ReviewProvenanceError("closure review Epoch is not bound to the Finding resolution")
                introduced_review = state["reviews"].get(target.get("introduced_review_id"))
                if introduced_review and fresh_epoch == introduced_review.get("fresh_epoch_id"):
                    raise AuthorizationError("closure review requires a fresh Context Epoch")
                if payload.get("findings"):
                    raise AuthorizationError("closure review cannot introduce a replacement Finding")
            if review_id in state["reviews"]:
                raise KernelError("review_id is immutable: %s" % review_id)
            findings = []
            blocking = False
            new_findings = 0
            state.setdefault("nodes", {})["review:" + review_id] = "review"
            self._record_authority_edge(state, command, "review:" + review_id)
            for supplied in payload.get("findings", []):
                finding = _copy(supplied)
                self._ensure_durable_payload(finding, "review finding")
                fingerprint_body = {
                    "requirement_ref": finding.get("requirement_ref"),
                    "description": finding.get("description"),
                    "severity": finding.get("severity", "major"),
                }
                computed_fingerprint = _digest(fingerprint_body)
                fingerprint = finding.get("fingerprint") or computed_fingerprint
                if fingerprint != computed_fingerprint:
                    raise IntegrityBlockedError("Finding fingerprint does not match its stable fields")
                finding_id = finding.get("finding_id") or "finding-" + fingerprint[7:23]
                finding_id = _id(finding_id, "finding_id")
                canonical_id = next((item_id for item_id, item in state["findings"].items() if item.get("fingerprint") == fingerprint), None)
                if canonical_id is not None and canonical_id != finding_id:
                    existing = state["findings"][canonical_id]
                    state.setdefault("metadata", {}).setdefault("review_duplicates", {}).setdefault(review_id, []).append({"supplied_finding_id": finding_id, "canonical_finding_id": canonical_id, "duplicate_of": canonical_id})
                    findings.append({key: _copy(existing.get(key)) for key in ("finding_id", "fingerprint", "requirement_ref", "description", "evidence", "severity", "blocking", "state")})
                    continue
                if finding_id in state["findings"]:
                    existing = state["findings"][finding_id]
                    if existing.get("fingerprint") != fingerprint:
                        raise IntegrityBlockedError("stable Finding fingerprint changed")
                    findings.append({key: _copy(existing.get(key)) for key in ("finding_id", "fingerprint", "requirement_ref", "description", "evidence", "severity", "blocking", "state")})
                    continue
                is_blocking = bool(finding.get("blocking", True))
                blocking = blocking or is_blocking
                record = {
                    "finding_id": finding_id,
                    "fingerprint": fingerprint,
                    "requirement_ref": finding.get("requirement_ref"),
                    "description": finding.get("description", ""),
                    "evidence": _copy(finding.get("evidence", [])),
                    "severity": finding.get("severity", "major"),
                    "blocking": is_blocking,
                    "state": "open",
                    "candidate_task_id": candidate_task_id,
                    "introduced_review_id": review_id,
                    "owner": finding.get("owner") or task["assignment"].get("assignment_id"),
                    "original_reviewer_assignment": reviewer_assignment,
                    "admitted": False,
                    "resolution_ref": None,
                    "closed_by": None,
                }
                finding_ref = self._new_object(stage, "finding", record)  # type: ignore[arg-type]
                self._add_object_ref(state, finding_ref)
                record["object_ref"] = finding_ref
                state["findings"][finding_id] = record
                new_findings += 1
                state.setdefault("nodes", {})["finding:" + finding_id] = "finding"
                self._edge(state, "review:" + review_id, "finding:" + finding_id, "produces")
                self._edge(state, "review:" + review_id, "finding:" + finding_id, "verdict-for")
                findings.append(
                    {
                        "finding_id": finding_id,
                        "fingerprint": fingerprint,
                        "requirement_ref": finding.get("requirement_ref"),
                        "description": finding.get("description", ""),
                        "evidence": _copy(finding.get("evidence", [])),
                        "severity": finding.get("severity", "major"),
                        "blocking": is_blocking,
                        "state": "open",
                    }
                )
            if review_kind == "closure" and target_finding_id:
                self._edge(state, "review:" + review_id, "finding:" + target_finding_id, "verdict-for")
            evidence_refs: List[Dict[str, Any]] = []
            if review_kind == "closure" and target_finding_id:
                target = state["findings"][target_finding_id]
                resolution_ref = target.get("resolution_ref")
                if not isinstance(resolution_ref, Mapping):
                    raise ReviewProvenanceError("closure review requires an object-bound resolution")
                evidence_refs.append(_copy(resolution_ref))
            for supplied in payload.get("findings", []):
                for ref in supplied.get("evidence_refs", []) if isinstance(supplied, Mapping) else []:
                    safe = _safe_ref(ref, "review evidence_ref")
                    loaded_evidence = self._load_object(safe["digest"])
                    if safe.get("object_type") is not None and loaded_evidence.get("object_type") != safe.get("object_type"):
                        raise ReviewProvenanceError("review evidence object type does not match reference")
                    evidence_refs.append(safe)
            review_payload = {
                "review_id": review_id,
                "candidate_task_id": candidate_task_id,
                "reviewer_assignment_id": reviewer_assignment,
                "fresh_epoch_id": fresh_epoch,
                "review_kind": review_kind,
                "target_finding_id": target_finding_id,
                "findings": findings,
                "candidate_ref": _copy(task["package_ref"]),
                "evidence_refs": evidence_refs,
                "provenance": {
                    "candidate_package_digest": task["package_ref"]["digest"],
                    "evidence_digests": [ref["digest"] for ref in evidence_refs],
                    "review_epoch_id": fresh_epoch,
                    "reviewer_assignment_id": reviewer_assignment,
                },
            }
            ref = self._new_object(stage, "review-package", review_payload)  # type: ignore[arg-type]
            self._add_object_ref(state, ref)
            verdict = "pending" if review_kind == "initial" and findings else ("pending" if review_kind == "closure" else "pass")
            state["reviews"][review_id] = {
                **review_payload,
                "package_ref": ref,
                "verdict": verdict,
                "status": "open" if findings or review_kind == "closure" else "closed",
            }
            state["verdicts"][review_id] = {"review_id": review_id, "verdict": verdict, "derived": True}
            epoch_context["status"] = "reviewed"
            epoch_context["review_id"] = review_id
            if new_findings:
                state["review_budget"]["rounds_used"] += 1
        elif operation == "validate_findings":
            review = state["reviews"].get(payload["review_id"])
            if not isinstance(review, Mapping):
                raise NotFoundError("unknown review: %s" % payload["review_id"])
            if review.get("review_kind") != "initial" or payload["review_id"] in state["finding_validations"]:
                raise AuthorizationError("review validation is single-assignment")
            task = state["tasks"].get(review.get("candidate_task_id"))
            validator = payload["validator_assignment_id"]
            epoch = state.get("epoch_contexts", {}).get(payload["fresh_epoch_id"])
            if not isinstance(task, Mapping) or not isinstance(epoch, Mapping) or epoch.get("status") != "open" or epoch.get("candidate_task_id") != review.get("candidate_task_id"):
                raise ReviewProvenanceError("Validator requires a fresh candidate-bound Epoch")
            if validator in (task.get("assignment", {}).get("assignment_id"), review.get("reviewer_assignment_id")) or epoch.get("reviewer_assignment_id") != validator:
                raise AuthorizationError("Validator must be distinct from Worker and Reviewer")
            candidates = {item["finding_id"]: item for item in review.get("findings", [])}
            outcomes = payload["outcomes"]
            if {item["candidate_id"] for item in outcomes} != set(candidates) or len(outcomes) != len(candidates):
                raise CommandValidationError("Validator must classify every candidate exactly once")
            validation_outcomes = []
            for outcome in outcomes:
                candidate_id = outcome["candidate_id"]
                finding = state["findings"].get(candidate_id)
                if not isinstance(finding, Mapping):
                    raise IntegrityBlockedError("review candidate is missing")
                if outcome["requirement_ref"] != finding.get("requirement_ref"):
                    raise ReviewProvenanceError("validation requirement does not match candidate")
                if outcome["disposition"] == "duplicate":
                    validation_outcomes.append({**_copy(outcome), "canonical_finding_id": candidate_id, "duplicate_of": candidate_id})
                    continue
                admitted = outcome["disposition"] == "required"
                finding["admitted"] = admitted
                finding["validator_assignment_id"] = validator
                finding["validation_disposition"] = outcome["disposition"]
                if not admitted:
                    finding["state"] = "invalid"
                duplicate = next((item for item in state.get("metadata", {}).get("review_duplicates", {}).get(payload["review_id"], []) if item.get("canonical_finding_id") == candidate_id), None)
                validation_outcomes.append({**_copy(outcome), "canonical_finding_id": candidate_id, "duplicate_of": duplicate.get("duplicate_of") if duplicate else None})
            evaluation = {"review_id": payload["review_id"], "review_package_ref": _copy(review["package_ref"]), "validator_assignment_id": validator, "fresh_epoch_id": payload["fresh_epoch_id"], "outcomes": validation_outcomes}
            evaluation_ref = self._new_object(stage, "evaluation-package", evaluation)  # type: ignore[arg-type]
            validation_ref = self._new_object(stage, "finding-validation", {**evaluation, "admissibility_verdict": "required-only"})  # type: ignore[arg-type]
            self._add_object_ref(state, evaluation_ref)
            self._add_object_ref(state, validation_ref)
            state["finding_validations"][payload["review_id"]] = {**evaluation, "evaluation_ref": evaluation_ref, "validation_ref": validation_ref}
            review["verdict"] = "pass"
            review["status"] = "closed"
            state["verdicts"][payload["review_id"]] = {"review_id": payload["review_id"], "verdict": "pass", "derived": True}
            epoch["status"] = "reviewed"
            if any(item["disposition"] == "required" for item in outcomes):
                task["status"] = "blocked_review"
            if any(item["disposition"] == "needs-user" for item in outcomes):
                self._terminalize_review(state, stage, "needs_user")
        elif operation == "terminal_review":
            if state.get("budget_terminal") is not None:
                raise LifecycleClosedError("review budget is already terminal")
            self._terminalize_review(state, stage, payload["reason"], payload["unresolved_finding_ids"])
        elif operation == "reopen_review":
            terminal = state.get("budget_terminal")
            if not isinstance(terminal, Mapping):
                raise LifecycleClosedError("review budget is not terminal")
            expected = payload["expected_head"]
            if expected.get("revision") != command.get("expected_head", {}).get("revision") or expected.get("transaction_digest") not in (command.get("expected_head", {}).get("transaction_digest"), command.get("expected_head", {}).get("digest")):
                raise StaleHeadError("reopen expected HEAD is stale")
            receipt = _copy(command["authority_ref"]["approval_receipt"])
            state.setdefault("metadata", {}).setdefault("reopen_history", []).append({
                "approval_id": payload["approval_id"], "receipt": receipt,
                "receipt_digest": _digest(receipt), "replacement_budget_digest": _digest(payload["replacement_budget"]),
                "expected_head": {"revision": expected["revision"], "transaction_digest": expected.get("transaction_digest") or expected.get("digest")},
                "prior_terminal_ref": _copy(terminal.get("object_ref")), "reopened_at_revision": state["revision"] + 1,
            })
            state["review_budget"] = _copy(payload["replacement_budget"])
            state["budget_terminal"] = None
            state["status"] = "active"
        elif operation == "accept_resolution_claim":
            finding_id = payload["finding_id"]
            finding = state["findings"].get(finding_id)
            if finding is None:
                raise NotFoundError("unknown Finding: %s" % finding_id)
            if finding.get("state") not in ("open", "unresolved"):
                raise KernelError("Finding is not claimable: %s" % finding_id)
            if finding.get("admitted") is not True or finding.get("validation_disposition") != "required":
                raise AuthorizationError("Finding requires fresh Validator admission")
            task = state["tasks"].get(finding["candidate_task_id"])
            if task is None or payload.get("worker_assignment_id") != task["assignment"].get("assignment_id"):
                raise AuthorizationError("resolution worker assignment mismatch")
            ref = self._new_object(stage, "finding-resolution", {"finding_id": finding_id, "evidence": payload.get("evidence", [])})  # type: ignore[arg-type]
            self._add_object_ref(state, ref)
            finding["state"] = "resolved"
            finding["resolution_ref"] = ref
            finding["resolution_claimed_by"] = payload["worker_assignment_id"]
            task["status"] = "fix_claimed"
            attempts = state["review_budget"]["finding_attempts"]
            attempts[finding_id] = attempts.get(finding_id, 0) + 1
        elif operation == "accept_finding_closure":
            finding_id = payload["finding_id"]
            finding = state["findings"].get(finding_id)
            if finding is None:
                raise NotFoundError("unknown Finding: %s" % finding_id)
            if finding.get("state") != "resolved":
                raise KernelError("Finding requires a resolution claim before review closure")
            if finding.get("closure_ref") is not None or finding.get("closed_by") is not None:
                raise ReviewProvenanceError("Finding closure is single-assignment")
            reviewer = payload.get("reviewer_assignment_id")
            if reviewer in (finding.get("original_reviewer_assignment"), finding.get("resolution_claimed_by")):
                raise AuthorizationError("fresh independent reviewer required")
            review = state["reviews"].get(finding["introduced_review_id"])
            if not review:
                raise IntegrityBlockedError("Finding review provenance is missing")
            closure_review_id = payload.get("review_id")
            if not isinstance(closure_review_id, str):
                raise AuthorizationError("a real closure Review package is required")
            closure_review = state["reviews"].get(closure_review_id)
            if not isinstance(closure_review, Mapping):
                raise AuthorizationError("closure Review package is missing")
            if closure_review.get("review_kind") != "closure" or closure_review.get("target_finding_id") != finding_id:
                raise AuthorizationError("closure Review provenance does not match Finding")
            if closure_review.get("candidate_task_id") != finding.get("candidate_task_id"):
                raise AuthorizationError("closure Review task provenance does not match Finding")
            if closure_review.get("reviewer_assignment_id") != reviewer:
                raise AuthorizationError("closure Review reviewer provenance does not match")
            if closure_review.get("fresh_epoch_id") != payload.get("fresh_epoch_id"):
                raise AuthorizationError("closure Review freshness does not match")
            closure_epoch = state.get("epoch_contexts", {}).get(payload.get("fresh_epoch_id"))
            if not isinstance(closure_epoch, Mapping) or closure_epoch.get("status") != "reviewed":
                raise AuthorizationError("closure Review must use a registered reviewed Epoch")
            package_ref = closure_review.get("package_ref")
            if not isinstance(package_ref, Mapping):
                raise IntegrityBlockedError("closure Review package reference is missing")
            package = self._load_object(package_ref.get("digest"))
            expected_package = {
                key: closure_review.get(key)
                for key in (
                    "review_id", "candidate_task_id", "reviewer_assignment_id", "fresh_epoch_id",
                    "review_kind", "target_finding_id", "findings", "candidate_ref", "evidence_refs", "provenance",
                )
            }
            if package.get("object_type") != "review-package" or package.get("payload") != expected_package:
                raise IntegrityBlockedError("closure Review package provenance is invalid")
            resolution_ref = finding.get("resolution_ref")
            if not isinstance(resolution_ref, Mapping):
                raise ReviewProvenanceError("Finding resolution object is missing")
            resolution_object = self._load_ref_object(resolution_ref, "finding.resolution_ref")
            resolution_payload = resolution_object.get("payload")
            if resolution_object.get("object_type") != "finding-resolution" or not isinstance(resolution_payload, Mapping) or resolution_payload.get("finding_id") != finding_id:
                raise ReviewProvenanceError("Finding resolution object is bound to another Finding")
            closure_evidence_refs = closure_review.get("evidence_refs")
            expected_resolution_digest = resolution_ref.get("digest") or resolution_ref.get("object_digest")
            if not isinstance(closure_evidence_refs, list) or len(closure_evidence_refs) != 1:
                raise ReviewProvenanceError("closure Review must carry exactly one evidence reference")
            closure_evidence_digest = closure_evidence_refs[0].get("digest") or closure_evidence_refs[0].get("object_digest") if isinstance(closure_evidence_refs[0], Mapping) else None
            if closure_evidence_digest != expected_resolution_digest or closure_evidence_refs[0] != resolution_ref:
                raise ReviewProvenanceError("closure Review evidence is not the same Finding resolution object")
            if payload.get("evidence") != closure_evidence_refs:
                raise ReviewProvenanceError("closure evidence must exactly reference the bound Review package evidence")
            task = state["tasks"].get(finding.get("candidate_task_id"))
            if not task:
                raise IntegrityBlockedError("Finding task provenance is missing")
            if payload.get("fresh_epoch_id") in (None, review.get("fresh_epoch_id"), task.get("epoch_id")):
                raise AuthorizationError("fresh rereview epoch is required")
            # ``resolved`` is the canonical five-state Finding value.  Closure
            # is proven by the separate, object-bound closure verdict rather
            # than by inventing a sixth ``closed`` state.
            finding["state"] = "resolved"
            finding["closed_by"] = reviewer
            finding["closure_evidence"] = _copy(payload.get("evidence", []))
            verdict_ref = self._new_object(
                stage,
                "finding-verdict",
                {
                    "finding_id": finding_id,
                    "review_id": closure_review_id,
                    "reviewer_assignment_id": reviewer,
                    "fresh_epoch_id": payload.get("fresh_epoch_id"),
                    "resolution_ref": _copy(resolution_ref),
                    "evidence": _copy(payload.get("evidence", [])),
                    "evidence_refs": _copy(payload.get("evidence", [])),
                    "closure_package_ref": _copy(package_ref),
                    "verdict": "pass",
                },
            )  # type: ignore[arg-type]
            self._add_object_ref(state, verdict_ref)
            finding["closure_ref"] = verdict_ref
            closure_review["verdict"] = "pass"
            closure_review["status"] = "closed"
            closure_epoch["status"] = "closed"
            state["verdicts"][closure_review_id] = {
                "review_id": closure_review_id,
                "verdict": "pass",
                "derived": True,
                "finding_id": finding_id,
                "reviewer_assignment_id": reviewer,
                "fresh_epoch_id": payload.get("fresh_epoch_id"),
                "resolution_ref": _copy(resolution_ref),
                "evidence_refs": _copy(payload.get("evidence", [])),
                "closure_package_ref": _copy(package_ref),
            }
            blockers = [
                item
                for item in state["findings"].values()
                if item.get("introduced_review_id") == review["review_id"]
                and item.get("blocking")
                and self._finding_blocks(item)
            ]
            if not blockers:
                review["verdict"] = "pass"
                review["status"] = "closed"
                state["verdicts"][review["review_id"]] = {
                    "review_id": review["review_id"],
                    "verdict": "pass",
                    "derived": True,
                }
                task = state["tasks"].get(review["candidate_task_id"])
                if task and task.get("status") in ("blocked_review", "fix_claimed"):
                    task["status"] = "succeeded"
        elif operation in ("invalidate_task", "replan_task"):
            task_id = payload["task_id"]
            task = state["tasks"].get(task_id)
            if not isinstance(task, Mapping):
                raise NotFoundError("unknown task: %s" % task_id)
            dependency_closure = self._reverse_dependency_closure(state, task_id)
            affected_task_ids = [task_id] + dependency_closure
            for affected_id in affected_task_ids:
                affected = state["tasks"].get(affected_id)
                if not isinstance(affected, Mapping):
                    raise IntegrityBlockedError("reverse dependency closure references an unknown Task")
                # Completed evidence remains addressable, but every affected
                # attempt is explicitly stale/invalidated.  No downstream
                # task can become ready from an obsolete producer.
                affected["invalidated"] = True
                affected["stop_requested"] = True
                affected.setdefault("freshness", {})["invalidated"] = True
                affected.setdefault("freshness", {})["stale"] = True
                if affected_id == task_id:
                    affected["dependency_closure"] = _copy(dependency_closure)
                else:
                    affected["dependency_closure"] = []
                lease = state.setdefault("leases", {}).get(affected_id)
                if isinstance(lease, Mapping):
                    # The external action is not assumed to be cancellable.
                    # Keep its lease visible, request cancellation, and mark
                    # any late result for quarantine rather than accepting it.
                    affected["cancel_requested"] = True
                    affected["quarantine_reason"] = "late result after %s" % operation
                    refs = []
                    if isinstance(affected.get("result_ref"), Mapping):
                        refs.append(_copy(affected["result_ref"]))
                    affected["quarantine_refs"] = refs
                    lease["cancel_requested"] = True
                    lease["quarantine"] = True
                    lease["quarantine_reason"] = affected["quarantine_reason"]
                else:
                    affected["cancel_requested"] = False
                    affected["quarantine_reason"] = None
                    affected["quarantine_refs"] = []
                    affected["status"] = "invalidated"
            task["replan_requested"] = operation == "replan_task"
            task["replan_reason"] = payload["reason"]
            # A fresh attempt is compiled only when the source has no active
            # lease.  If an external action is still running, the canonical
            # transition is cancellation/quarantine and an explicit later
            # replan; creating a ready duplicate would race that action.
            if operation == "replan_task" and task_id not in state.get("leases", {}):
                package_ref = task.get("package_ref")
                package_object = self._load_ref_object(package_ref, "replan source package")
                package_payload = package_object.get("payload")
                if package_object.get("object_type") != "task-package" or not isinstance(package_payload, Mapping):
                    raise IntegrityBlockedError("replan source package is not a Task package")
                base_id = "%s-replan-%s" % (task_id, state["revision"] + 1)
                replacement_id = base_id
                suffix = 2
                while replacement_id in state["tasks"]:
                    replacement_id = "%s-%s" % (base_id, suffix)
                    suffix += 1
                old_assignment = task.get("assignment", {}).get("assignment_id")
                replacement_assignment_id = "%s-replan-%s" % (old_assignment, state["revision"] + 1)
                replacement_package = _copy(package_payload)
                replacement_package.update(
                    {
                        "task_id": replacement_id,
                        "attempt_id": "attempt-%s" % replacement_id,
                        "assignment": {"role": "worker", "assignment_id": replacement_assignment_id},
                        "freshness": {
                            "epoch_id": state["epoch"]["id"],
                            "created_at_revision": state["revision"] + 1,
                            "stale": False,
                            "invalidated": False,
                        },
                        "invalidated": False,
                        "stop_requested": False,
                        "status": "ready",
                    }
                )
                self._validate_task_package_shape(replacement_package, "replan replacement package")
                self._ensure_durable_payload(replacement_package, "replan replacement package")
                self._validate_task_output_scope(replacement_package, command.get("authority_ref") or {})
                replacement_ref = self._new_object(stage, "task-package", replacement_package)  # type: ignore[arg-type]
                self._add_object_ref(state, replacement_ref)
                replacement_record = {
                    "task_id": replacement_id,
                    "attempt_id": replacement_package["attempt_id"],
                    "package_ref": replacement_ref,
                    "assignment": _copy(replacement_package["assignment"]),
                    "input_refs": _copy(replacement_package.get("input_refs", [])),
                    "write_scope": _copy(replacement_package.get("write_scope", [])),
                    "freshness": _copy(replacement_package["freshness"]),
                    "stop_conditions": _copy(replacement_package.get("stop_conditions", [])),
                    "invalidated": False,
                    "stop_requested": False,
                    "output_path": _copy(replacement_package.get("output_path")),
                    "status": "ready",
                    "epoch_id": state["epoch"]["id"],
                    "sibling_group": task.get("sibling_group"),
                    "authority_ref": _copy(command.get("authority_ref")),
                    "authority_digest": _digest(command.get("authority_ref")),
                    "authority_expected_head": _copy(command.get("authority_ref", {}).get("expected_head")) if isinstance(command.get("authority_ref"), Mapping) else None,
                    "authority_expected_head_revision": command.get("authority_ref", {}).get("expected_head_revision") if isinstance(command.get("authority_ref"), Mapping) else None,
                    "proposal_digest": command.get("authority_ref", {}).get("proposal_digest") if isinstance(command.get("authority_ref"), Mapping) else None,
                    "lease_status": "released",
                    "result_ref": None,
                    "graph_revision": state.get("graph_revision"),
                    "replacement_task_id": None,
                    "dependency_closure": [],
                    "cancel_requested": False,
                    "quarantine_reason": None,
                    "quarantine_refs": [],
                }
                state["tasks"][replacement_id] = replacement_record
                state.setdefault("nodes", {})["task:" + replacement_id] = "task"
                self._record_authority_edge(state, command, "task:" + replacement_id)
                old_node = "task:" + task_id
                for edge in list(state.get("edges", [])):
                    if edge.get("to") != old_node or edge.get("type") not in {"requires", "converges"}:
                        continue
                    self._edge(state, edge["from"], "task:" + replacement_id, edge["type"])
                task["replacement_task_id"] = replacement_id
        elif operation == "claim_task":
            task_id = payload["task_id"]
            task = state["tasks"].get(task_id)
            if task is None:
                raise NotFoundError("unknown task: %s" % task_id)
            assignment = payload.get("assignment_id")
            if task["assignment"].get("assignment_id") != assignment:
                raise AuthorizationError("task assignment mismatch")
            command_head = command.get("expected_head")
            current_head = {
                "revision": state.get("revision"),
                "transaction_digest": command_head.get("transaction_digest") if isinstance(command_head, Mapping) else None,
                "digest": command_head.get("digest") if isinstance(command_head, Mapping) else None,
            }
            self._validate_live_task_authority(state, task, current_head=current_head)
            if task_id not in self._ready_tasks(state, current_head):
                raise KernelError("task is not ready to claim")
            state.setdefault("leases", {})[task_id] = {
                "assignment_id": assignment,
                "write_scope": _copy(task.get("write_scope", [])),
                "status": "leased",
                "claimed_at_revision": state["revision"] + 1,
            }
            task["lease_status"] = "leased"
            task["status"] = "running"
        elif operation == "release_task":
            task_id = payload["task_id"]
            task = state["tasks"].get(task_id)
            if task is None:
                raise NotFoundError("unknown task: %s" % task_id)
            assignment = payload.get("assignment_id")
            lease = state.setdefault("leases", {}).get(task_id)
            if not lease or lease.get("assignment_id") != assignment:
                raise AuthorizationError("task lease mismatch")
            state["leases"].pop(task_id, None)
            if task.get("invalidated") or task.get("cancel_requested"):
                task["status"] = "invalidated"
            elif task.get("status") == "running":
                task["status"] = "ready"
            task["lease_status"] = "released"
        elif operation == "open_epoch":
            if state["epoch"].get("status") != "closed" or not state["epoch"].get("clear_before_next"):
                raise KernelError("a fresh Epoch requires a closed clear boundary")
            epoch_id = _id(payload["epoch_id"], "epoch_id")
            if epoch_id in state.get("epoch_contexts", {}):
                raise KernelError("Context Epoch ID is immutable: %s" % epoch_id)
            input_ref = _safe_ref(payload["input_ref"], "input_ref")
            loaded_input = self._validate_input_ref_binding(state, input_ref, "input_ref")
            if input_ref.get("object_type") is not None and loaded_input.get("object_type") != input_ref.get("object_type"):
                raise IntegrityBlockedError("Epoch input object type does not match reference")
            previous_epoch_id = state["epoch"]["id"]
            if previous_epoch_id in state.get("epoch_contexts", {}):
                state["epoch_contexts"][previous_epoch_id]["status"] = "closed"
            state["epoch"] = {"id": epoch_id, "status": "open", "group_id": state["group"]["id"], "boundary_reason": "fresh-epoch", "input_bundle": input_ref, "clear_before_next": False}
            state.setdefault("epoch_contexts", {})[epoch_id] = {
                "id": epoch_id,
                "status": "open",
                "group_id": state["group"]["id"],
                "boundary_reason": "fresh-epoch",
                "started_at_revision": state["revision"] + 1,
                "input_ref": input_ref,
            }
            state["status"] = "active"
        elif operation == "close_epoch":
            if state["epoch"].get("status") != "open":
                raise KernelError("Epoch is already closed")
            if state.get("leases") or any(task.get("status") == "running" for task in state.get("tasks", {}).values() if isinstance(task, Mapping)):
                raise LifecycleClosedError("Epoch cannot close while a lease or running Task is active")
            decisions = [
                task.get("task_id")
                for task in state.get("tasks", {}).values()
                if isinstance(task, Mapping) and task.get("status") == "needs_decision"
            ]
            if decisions:
                raise LifecycleClosedError("Epoch cannot close while a Task needs_decision: %s" % ", ".join(decisions))
            blockers = [item["finding_id"] for item in state["findings"].values() if self._finding_blocks(item)]
            if blockers:
                raise KernelError("blocking Findings remain: %s" % ", ".join(blockers))
            budget = _copy(payload.get("context_budget") or {})
            self._validate_budget(budget)
            # ``state.epoch`` carries the open Epoch's input reference for
            # orchestration, while the closure Bundle is a projection of the
            # completed boundary.  The canonical ``epoch_contexts`` record
            # retains that input provenance; project only the documented
            # closed-boundary fields into ``artifact-bundle.context_epoch``.
            context_epoch = {
                key: _copy(state["epoch"][key])
                for key in (
                    "id", "status", "group_id", "boundary_reason",
                    "clear_before_next", "closed_at_revision", "bundle_ref",
                    "checkpoint_ref",
                )
                if key in state["epoch"]
            }
            context_epoch["status"] = "closed"
            context_epoch["boundary_reason"] = payload.get("boundary_reason") or "epoch-close"
            context_epoch["clear_before_next"] = bool(payload.get("clear_before_next", True))
            context_epoch["closed_at_revision"] = state["revision"] + 1
            bundle_payload = self._build_bundle_payload(
                state,
                dict(payload, context_budget=budget),
                context_epoch,
                "%s-%s-bundle" % (state["group"]["id"], state["epoch"]["id"]),
            )
            ref = self._new_object(stage, "artifact-bundle", bundle_payload)  # type: ignore[arg-type]
            self._add_object_ref(state, ref)
            checkpoint_payload = {
                "schema": "checkpoint/v1",
                "checkpoint_id": "cp-%s" % state["epoch"]["id"],
                "run_id": self.run_id,
                "workflow_version": state["workflow_version"],
                "group_id": state["group"]["id"],
                "epoch_id": state["epoch"]["id"],
                "state_revision": state["revision"] + 1,
                "closure_revision": state["revision"] + 1,
                "state_ref": {
                    "run_id": self.run_id,
                    "revision": state["revision"] + 1,
                    "state_revision": state["revision"] + 1,
                },
                "bundle_ref": ref,
                "clear_before_start": bool(payload.get("clear_before_next", True)),
            }
            checkpoint_ref = self._new_object(stage, "checkpoint", checkpoint_payload)  # type: ignore[arg-type]
            self._add_object_ref(state, checkpoint_ref)
            state["epoch"].update({"status": "closed", "boundary_reason": context_epoch["boundary_reason"], "clear_before_next": bool(payload.get("clear_before_next", True)), "bundle_ref": ref, "checkpoint_ref": checkpoint_ref, "closed_at_revision": state["revision"] + 1})
            if state["epoch"]["id"] in state.get("epoch_contexts", {}):
                state["epoch_contexts"][state["epoch"]["id"]].update(
                    {
                        "status": "closed",
                        "bundle_ref": ref,
                        "checkpoint_ref": checkpoint_ref,
                        "closed_at_revision": state["revision"] + 1,
                        "clear_before_next": bool(payload.get("clear_before_next", True)),
                    }
                )
            state["context_budget"] = budget
            state["status"] = "paused_after_epoch"
        elif operation == "close_group":
            if state["epoch"].get("status") != "closed" or state["group"].get("status") != "open":
                raise KernelError("Group requires a closed Epoch")
            if state.get("leases") or any(task.get("status") == "running" for task in state.get("tasks", {}).values() if isinstance(task, Mapping)):
                raise LifecycleClosedError("Group cannot close while a lease or running Task is active")
            decisions = [
                task.get("task_id")
                for task in state.get("tasks", {}).values()
                if isinstance(task, Mapping) and task.get("status") == "needs_decision"
            ]
            if decisions:
                raise LifecycleClosedError("Group cannot close while a Task needs_decision: %s" % ", ".join(decisions))
            if any(self._finding_blocks(finding) for finding in state["findings"].values()):
                raise KernelError("Group has open blocking Findings")
            section_state = state.get("metadata", {}).get("section_control")
            accepting_section = isinstance(section_state, Mapping) and section_state.get("transition") == {"intent": "source-transition-fixture-passed", "state": "pending"}
            if accepting_section:
                evidence = payload.get("acceptance_evidence")
                if payload.get("unresolved_items") or payload.get("invalidated_artifacts"):
                    raise AuthorizationError("section acceptance requires non-empty digest-bound evidence and no unresolved or invalidated items")
                self._validate_section_acceptance_evidence(state, evidence, section_state)
            context_epoch = {
                "id": "group-close",
                "group_id": state["group"]["id"],
                "status": "closed",
                "boundary_reason": payload.get("boundary_reason") or "group-close",
                "clear_before_next": True,
                "closed_at_revision": state["revision"] + 1,
            }
            bundle_payload = self._build_bundle_payload(
                state,
                payload,
                context_epoch,
                "%s-group-bundle" % state["group"]["id"],
            )
            ref = self._new_object(stage, "artifact-bundle", bundle_payload)  # type: ignore[arg-type]
            self._add_object_ref(state, ref)
            checkpoint_payload = {
                "schema": "checkpoint/v1",
                "checkpoint_id": "cp-group-%s" % state["group"]["id"],
                "run_id": self.run_id,
                "workflow_version": state["workflow_version"],
                "group_id": state["group"]["id"],
                "epoch_id": state["epoch"]["id"],
                "state_revision": state["revision"] + 1,
                "closure_revision": state["revision"] + 1,
                "state_ref": {
                    "run_id": self.run_id,
                    "revision": state["revision"] + 1,
                    "state_revision": state["revision"] + 1,
                },
                "bundle_ref": ref,
                "clear_before_start": True,
            }
            checkpoint_ref = self._new_object(stage, "checkpoint", checkpoint_payload)  # type: ignore[arg-type]
            self._add_object_ref(state, checkpoint_ref)
            state["group"].update({"status": "closed", "next_group": payload.get("next_group"), "bundle_ref": ref, "checkpoint_ref": checkpoint_ref, "closed_at_revision": state["revision"] + 1, "clear_required": True})
            state["status"] = "paused_after_group"
            if accepting_section:
                receipt = self._new_object(stage, "artifact", {
                    "artifact_id": "section-acceptance-" + str(section_state["section_id"]), "version": "v1",
                    "kind": "section-transition-receipt",
                    "payload": {"schema": "section-acceptance-receipt/v1", "section_id": section_state["section_id"],
                                "run_id": self.run_id, "accepted_group_id": state["group"]["id"],
                                "accepted_at_revision": state["revision"] + 1,
                                "acceptance_evidence": _copy(payload["acceptance_evidence"]),
                                "bundle_ref": ref, "checkpoint_ref": checkpoint_ref},
                })
                self._add_object_ref(state, receipt)
                section_state["accepted_section_receipt"] = receipt
                section_state["transition"] = {"intent": "source-transition-fixture-passed", "state": "accepted"}
        else:
            raise KernelError("unknown command: %s" % operation)
        return state

    @staticmethod
    def _validate_budget(budget: Mapping[str, Any]) -> None:
        if not isinstance(budget, Mapping):
            raise BudgetError("context budget must be an object")
        required_fields = {
            "target", "normal_limit", "absolute_limit", "token_status", "token_count"
        }
        if set(budget) != required_fields:
            unknown = sorted(set(budget) - required_fields)
            missing = sorted(required_fields - set(budget))
            detail = []
            if unknown:
                detail.append("unsupported " + ", ".join(unknown))
            if missing:
                detail.append("missing " + ", ".join(missing))
            raise BudgetError("context budget fields are not schema-owned: %s" % "; ".join(detail))
        if budget.get("target") != 200000 or budget.get("normal_limit") != 300000 or budget.get("absolute_limit") != 500000:
            raise BudgetError("context budget limits must be 200K/300K/500K")
        status = budget.get("token_status")
        count = budget.get("token_count")
        if status not in TOKEN_STATUSES:
            raise BudgetError("token_status must be exact, estimated, or unavailable")
        if status == "unavailable" and count is not None:
            raise BudgetError("unavailable token status cannot carry a count")
        if status != "unavailable" and (not isinstance(count, int) or isinstance(count, bool) or count < 0):
            raise BudgetError("exact/estimated token status requires a non-negative count")
        if status != "unavailable" and count > 500000:
            raise BudgetError("absolute context budget is 500K")
        if status != "unavailable" and count > 300000:
            raise BudgetError("context budget over normal 300K limit; split or replan")

    def _command_for(self, operation: str, payload: Mapping[str, Any], **kwargs: Any) -> Dict[str, Any]:
        authority = kwargs.pop("authority_ref", None)
        if authority is None:
            raise AuthorizationError("authority_ref is required")
        command = self._make_command(operation, authority_ref=authority, payload=payload, **kwargs)
        if self.head_path.exists():
            # This preflight supplies only the current version.  Do not call
            # ``read_state`` here: it may rebuild a disposable projection
            # before the caller's complete command has passed validation.
            with self._lock():
                state, _ = self._load_current()
                command["workflow_version"] = state["workflow_version"]
        return command

    def apply(self, command: Mapping[str, Any]) -> Dict[str, Any]:
        """Apply an already formed orchestrator command."""
        value = self._normalize_command(command)
        state = self._commit(value, self._generic_reducer)
        # The public section seam is receipt-oriented: unlike historical
        # kernel callers it must expose an idempotent, canonical transaction
        # receipt rather than a mutable-looking state snapshot.
        if value.get("command_type") in {"open_section", "open_group"}:
            return _copy(self.last_receipt or {})
        return state

    commit = apply
    advance_head = apply

    def publish_artifact(
        self,
        artifact_id: str,
        version: str,
        value: Any,
        *,
        kind: str = "artifact",
        path: Optional[str] = None,
        authority_ref: Optional[Mapping[str, Any]] = None,
        idempotency_key: Optional[str] = None,
    ) -> Dict[str, Any]:
        return self.apply(self._command_for("publish_artifact", {"artifact_id": artifact_id, "version": version, "value": _copy(value), "kind": kind, "path": path}, authority_ref=authority_ref, idempotency_key=idempotency_key))

    def publish_task_package(
        self,
        task_id: str,
        package: Optional[Mapping[str, Any]] = None,
        *,
        assignment_id: Optional[str] = None,
        authority_ref: Optional[Mapping[str, Any]] = None,
        input_refs: Optional[Sequence[Mapping[str, Any]]] = None,
        sibling_group: Optional[str] = None,
        idempotency_key: Optional[str] = None,
    ) -> Dict[str, Any]:
        assignment = {"role": "worker", "assignment_id": assignment_id or "worker-" + task_id}
        supplied_package = _copy(package or {})
        # Read canonical state without rebuilding projections before the
        # package shape and durable-boundary checks in ``apply``.
        with self._lock():
            current_state, _ = self._load_current()
        package_value = dict(supplied_package)
        package_value.setdefault("task_id", task_id)
        package_value.setdefault("attempt_id", "attempt-" + task_id)
        package_value.setdefault("assignment", _copy(assignment))
        package_value.setdefault("input_refs", _copy(input_refs or []))
        package_value.setdefault("write_scope", [])
        package_value.setdefault("acceptance", [])
        package_value.setdefault(
            "freshness",
            {"epoch_id": current_state["epoch"]["id"], "created_at_revision": current_state["revision"] + 1},
        )
        package_value.setdefault("stop_conditions", [])
        package_value.setdefault("invalidated", False)
        package_value.setdefault("stop_requested", False)
        package_value.setdefault("output_path", None)
        package_value.setdefault("status", "ready")
        payload = {"task_id": task_id, "package": package_value, "assignment": assignment, "input_refs": _copy(input_refs or []), "sibling_group": sibling_group}
        digest_refs = [
            _copy(ref)
            for ref in (input_refs or [])
            if isinstance(ref, Mapping) and (ref.get("digest") or ref.get("object_digest"))
        ]
        return self.apply(self._command_for("publish_task_package", payload, authority_ref=authority_ref, input_refs=digest_refs, idempotency_key=idempotency_key))

    publish_task = publish_task_package

    def submit_task_result(
        self,
        task_id: str,
        result: Mapping[str, Any],
        *,
        worker_assignment_id: str,
        authority_ref: Optional[Mapping[str, Any]] = None,
        idempotency_key: Optional[str] = None,
    ) -> Dict[str, Any]:
        with self._lock():
            state, _ = self._load_current()
        task = state["tasks"].get(task_id)
        if not task:
            raise NotFoundError("unknown task: %s" % task_id)
        if task["assignment"].get("assignment_id") != worker_assignment_id:
            raise AuthorizationError("task assignment mismatch")
        submission_authority = state["authority"] if authority_ref is None else authority_ref
        return self._submit_inbox("worker-result", {
            "task_id": task_id,
            "result": _copy(result),
            "worker_assignment_id": worker_assignment_id,
        }, submission_authority, idempotency_key)

    submit_worker_result = submit_task_result

    def accept_task_result(
        self,
        task_id: str,
        result: Mapping[str, Any],
        *,
        worker_assignment_id: str,
        authority_ref: Optional[Mapping[str, Any]] = None,
        idempotency_key: Optional[str] = None,
    ) -> Dict[str, Any]:
        payload = {"task_id": task_id, "result": _copy(result), "worker_assignment_id": worker_assignment_id}
        return self.apply(self._command_for("accept_task_result", payload, authority_ref=authority_ref, idempotency_key=idempotency_key))

    def open_review(
        self,
        review_id: str,
        candidate_task_id: str,
        findings: Sequence[Mapping[str, Any]],
        *,
        reviewer_assignment_id: str,
        fresh_epoch_id: str,
        review_kind: str = "initial",
        target_finding_id: Optional[str] = None,
        authority_ref: Optional[Mapping[str, Any]] = None,
        idempotency_key: Optional[str] = None,
    ) -> Dict[str, Any]:
        normalized_findings = []
        for supplied in findings:
            finding = _copy(supplied)
            if isinstance(finding, Mapping):
                fingerprint_body = {
                    "requirement_ref": finding.get("requirement_ref"),
                    "description": finding.get("description"),
                    "severity": finding.get("severity"),
                }
                finding.setdefault("fingerprint", _digest(fingerprint_body))
                finding.setdefault("evidence", [])
                finding.setdefault("state", "open")
            normalized_findings.append(finding)
        payload = {
            "review_id": review_id,
            "candidate_task_id": candidate_task_id,
            "findings": normalized_findings,
            "reviewer_assignment_id": reviewer_assignment_id,
            "fresh_epoch_id": fresh_epoch_id,
            "review_kind": review_kind,
            "target_finding_id": target_finding_id,
        }
        return self.apply(self._command_for("open_review", payload, authority_ref=authority_ref, idempotency_key=idempotency_key))

    accept_review = open_review

    def validate_findings(self, review_id: str, outcomes: Sequence[Mapping[str, Any]], *, validator_assignment_id: str, fresh_epoch_id: str, authority_ref: Optional[Mapping[str, Any]] = None, idempotency_key: Optional[str] = None) -> Dict[str, Any]:
        """Admit a complete review candidate set through a fresh Validator."""
        return self.apply(self._command_for("validate_findings", {"review_id": review_id, "outcomes": _copy(list(outcomes)), "validator_assignment_id": validator_assignment_id, "fresh_epoch_id": fresh_epoch_id}, authority_ref=authority_ref, idempotency_key=idempotency_key))

    def terminal_review(self, reason: str, *, unresolved_finding_ids: Sequence[str], authority_ref: Optional[Mapping[str, Any]] = None, idempotency_key: Optional[str] = None) -> Dict[str, Any]:
        return self.apply(self._command_for("terminal_review", {"reason": reason, "unresolved_finding_ids": list(unresolved_finding_ids)}, authority_ref=authority_ref, idempotency_key=idempotency_key))

    def reopen_review(self, approval_id: str, replacement_budget: Mapping[str, Any], *, expected_head: Mapping[str, Any], authority_ref: Optional[Mapping[str, Any]] = None, idempotency_key: Optional[str] = None) -> Dict[str, Any]:
        return self.apply(self._command_for("reopen_review", {"approval_id": approval_id, "replacement_budget": _copy(replacement_budget), "expected_head": _copy(expected_head)}, authority_ref=authority_ref, idempotency_key=idempotency_key))

    def submit_resolution_claim(
        self,
        finding_id: str,
        evidence: Sequence[Any],
        *,
        worker_assignment_id: str,
        authority_ref: Optional[Mapping[str, Any]] = None,
        idempotency_key: Optional[str] = None,
    ) -> Dict[str, Any]:
        with self._lock():
            state, _ = self._load_current()
        finding = state["findings"].get(finding_id)
        if finding is None:
            raise NotFoundError("unknown Finding: %s" % finding_id)
        if finding.get("state") not in ("open", "unresolved"):
            raise AuthorizationError("Finding is not open for a worker resolution claim")
        if finding.get("admitted") is not True or finding.get("validation_disposition") != "required":
            raise AuthorizationError("Finding requires fresh Validator admission")
        task = state["tasks"].get(finding["candidate_task_id"])
        if not task or task["assignment"].get("assignment_id") != worker_assignment_id:
            raise AuthorizationError("resolution worker assignment mismatch")
        submission_authority = state["authority"] if authority_ref is None else authority_ref
        return self._submit_inbox("finding-resolution", {"finding_id": finding_id, "evidence": _copy(list(evidence)), "worker_assignment_id": worker_assignment_id}, submission_authority, idempotency_key)

    claim_finding_resolution = submit_resolution_claim

    def accept_resolution_claim(
        self,
        finding_id: str,
        evidence: Sequence[Any],
        *,
        worker_assignment_id: str,
        authority_ref: Optional[Mapping[str, Any]] = None,
        idempotency_key: Optional[str] = None,
    ) -> Dict[str, Any]:
        payload = {"finding_id": finding_id, "evidence": _copy(list(evidence)), "worker_assignment_id": worker_assignment_id}
        return self.apply(self._command_for("accept_resolution_claim", payload, authority_ref=authority_ref, idempotency_key=idempotency_key))

    def submit_finding_closure(
        self,
        finding_id: str,
        evidence: Sequence[Any],
        *,
        reviewer_assignment_id: str,
        fresh_epoch_id: str,
        review_id: Optional[str] = None,
        authority_ref: Optional[Mapping[str, Any]] = None,
        idempotency_key: Optional[str] = None,
    ) -> Dict[str, Any]:
        with self._lock():
            state, _ = self._load_current()
        finding = state["findings"].get(finding_id)
        if not finding:
            raise NotFoundError("unknown Finding: %s" % finding_id)
        if finding.get("state") != "resolved":
            raise AuthorizationError("Finding requires a worker resolution before closure review")
        if reviewer_assignment_id == finding.get("original_reviewer_assignment"):
            raise AuthorizationError("fresh reviewer required")
        if reviewer_assignment_id == finding.get("resolution_claimed_by"):
            raise AuthorizationError("worker cannot submit a closure review")
        review = state["reviews"].get(finding["introduced_review_id"])
        if review and fresh_epoch_id == review.get("fresh_epoch_id"):
            raise AuthorizationError("closure requires a fresh rereview epoch")
        task = state["tasks"].get(finding.get("candidate_task_id"))
        if task and fresh_epoch_id == task.get("epoch_id"):
            raise AuthorizationError("closure requires a fresh rereview epoch")
        if review_id is not None:
            closure_review = state["reviews"].get(review_id)
            if not closure_review or closure_review.get("review_kind") != "closure" or closure_review.get("target_finding_id") != finding_id:
                raise AuthorizationError("a real closure Review package is required")
            if closure_review.get("reviewer_assignment_id") != reviewer_assignment_id or closure_review.get("fresh_epoch_id") != fresh_epoch_id:
                raise AuthorizationError("closure Review provenance does not match")
        else:
            raise AuthorizationError("a real closure Review package is required")
        resolution_ref = finding.get("resolution_ref")
        closure_package = state["reviews"].get(review_id) if review_id is not None else None
        if not isinstance(resolution_ref, Mapping) or not isinstance(closure_package, Mapping) or list(evidence) != closure_package.get("evidence_refs"):
            raise ReviewProvenanceError("closure evidence must exactly reference the bound Review package")
        submission_authority = state["authority"] if authority_ref is None else authority_ref
        return self._submit_inbox("finding-closure", {"finding_id": finding_id, "evidence": _copy(list(evidence)), "reviewer_assignment_id": reviewer_assignment_id, "fresh_epoch_id": fresh_epoch_id, "review_id": review_id}, submission_authority, idempotency_key)

    submit_review_closure = submit_finding_closure

    def accept_finding_closure(
        self,
        finding_id: str,
        evidence: Sequence[Any],
        *,
        reviewer_assignment_id: str,
        fresh_epoch_id: str,
        review_id: Optional[str] = None,
        authority_ref: Optional[Mapping[str, Any]] = None,
        idempotency_key: Optional[str] = None,
    ) -> Dict[str, Any]:
        payload = {"finding_id": finding_id, "evidence": _copy(list(evidence)), "reviewer_assignment_id": reviewer_assignment_id, "fresh_epoch_id": fresh_epoch_id, "review_id": review_id}
        return self.apply(self._command_for("accept_finding_closure", payload, authority_ref=authority_ref, idempotency_key=idempotency_key))

    close_finding = accept_finding_closure

    def close_epoch(
        self,
        *,
        acceptance_evidence: Optional[Sequence[Any]] = None,
        approved_decisions: Optional[Sequence[Any]] = None,
        unresolved_items: Optional[Sequence[Any]] = None,
        invalidated_artifacts: Optional[Sequence[Any]] = None,
        next_inputs: Optional[Sequence[Any]] = None,
        token_status: str = "unavailable",
        token_count: Optional[int] = None,
        clear_before_next: bool = True,
        boundary_reason: Optional[str] = None,
        authority_ref: Optional[Mapping[str, Any]] = None,
        idempotency_key: Optional[str] = None,
    ) -> Dict[str, Any]:
        budget = dict(BUDGET_POLICY, token_status=token_status, token_count=token_count)
        payload = {"acceptance_evidence": list(acceptance_evidence or []), "approved_decisions": list(approved_decisions or []), "unresolved_items": list(unresolved_items or []), "invalidated_artifacts": list(invalidated_artifacts or []), "next_inputs": list(next_inputs or []), "context_budget": budget, "clear_before_next": clear_before_next, "boundary_reason": boundary_reason or "epoch-close"}
        return self.apply(self._command_for("close_epoch", payload, authority_ref=authority_ref, idempotency_key=idempotency_key))

    def close_group(
        self,
        *,
        acceptance_evidence: Optional[Sequence[Any]] = None,
        approved_decisions: Optional[Sequence[Any]] = None,
        unresolved_items: Optional[Sequence[Any]] = None,
        invalidated_artifacts: Optional[Sequence[Any]] = None,
        next_inputs: Optional[Sequence[Any]] = None,
        next_group: Optional[str] = None,
        boundary_reason: Optional[str] = None,
        authority_ref: Optional[Mapping[str, Any]] = None,
        idempotency_key: Optional[str] = None,
    ) -> Dict[str, Any]:
        payload = {"acceptance_evidence": list(acceptance_evidence or []), "approved_decisions": list(approved_decisions or []), "unresolved_items": list(unresolved_items or []), "invalidated_artifacts": list(invalidated_artifacts or []), "next_inputs": list(next_inputs or []), "next_group": next_group, "boundary_reason": boundary_reason or "group-close"}
        return self.apply(self._command_for("close_group", payload, authority_ref=authority_ref, idempotency_key=idempotency_key))

    def _submit_inbox(
        self,
        kind: str,
        payload: Mapping[str, Any],
        authority_ref: Mapping[str, Any],
        idempotency_key: Optional[str],
    ) -> Dict[str, Any]:
        if not isinstance(authority_ref, Mapping):
            raise AuthorizationError("authority_ref is required")
        self._ensure_durable_payload(authority_ref, "submission.authority_ref")
        self._ensure_durable_payload(payload, "submission")
        self._authority_ok(authority_ref, "submit:" + kind)
        key = idempotency_key or "submission-" + uuid.uuid4().hex
        envelope = {
            "schema": "dag-submission/v1",
            "run_id": self.run_id,
            "kind": kind,
            "actor": {"role": "worker" if "worker" in kind or "resolution" in kind else "reviewer"},
            "assignment": payload.get("worker_assignment_id") or payload.get("reviewer_assignment_id"),
            "authority_ref": _copy(authority_ref),
            "idempotency_key": key,
            "payload": _copy(payload),
        }
        envelope["digest"] = _digest(_without_digest(envelope))
        with self._lock():
            self.inbox_dir.mkdir(parents=True, exist_ok=True)
            path = self.inbox_dir / (envelope["digest"][7:] + ".json")
            if path.exists():
                old = _read_json(path)
                if old != envelope:
                    raise DuplicateCommandError("submission digest collision")
            else:
                _atomic_json(path, envelope)
            head = self._load_head()
        receipt = {"accepted": False, "submitted": True, "submission_digest": envelope["digest"], "head": _copy(head), "idempotency_key": key}
        self.last_receipt = receipt
        return receipt

    def submit_command(self, command: Mapping[str, Any]) -> Dict[str, Any]:
        """Persist an unaccepted command envelope in inbox/commands.

        This is intentionally different from :meth:`apply`: even an
        orchestrator-looking envelope is not accepted until ``apply`` runs
        under the per-Run lock and passes the expected-HEAD CAS.
        """

        value = self._normalize_command(command)
        self._validate_command_shape(value)
        self._ensure_durable_payload(value, "inbox.command")
        if value.get("run_id") != self.run_id:
            raise AuthorizationError("command Run ID mismatch")
        if value.get("actor", {}).get("role") not in {"controller", "worker", "reviewer", "arbiter", "orchestrator"}:
            raise AuthorizationError("invalid submitting role")
        self._authority_ok(
            value["authority_ref"],
            value["command_type"],
            protected_fields=value.get("protected_fields") or [],
        )
        requested_scope = value.get("scope") or value.get("payload", {}).get("write_scope") or value.get("payload", {}).get("scope")
        if value["command_type"] == "publish_task_package" and isinstance(value.get("payload", {}).get("package"), Mapping):
            package = value["payload"]["package"]
            requested_scope = requested_scope or package.get("write_scope") or package.get("output_path")
            self._validate_task_output_scope(package, value["authority_ref"])
        if value["command_type"] == "publish_artifact" and value["payload"].get("path") is not None:
            requested_scope = requested_scope or value["payload"].get("path")
        self._validate_scope(requested_scope, value["authority_ref"], label="inbox.command.scope")
        bound_assignment = value["authority_ref"].get("assignment_id") or value["authority_ref"].get("bound_assignment_id")
        if bound_assignment is not None and bound_assignment != value["actor"]["assignment_id"]:
            raise AuthorizationError("authority assignment does not match actor")
        if not self.head_path.exists():
            raise NotFoundError("cannot submit a command before the Run has a HEAD")
        with self._lock():
            state, _ = self._load_current()
        for index, ref in enumerate(value.get("input_refs") or []):
            self._validate_input_ref_binding(state, ref, "inbox.input_refs[%s]" % index)
        digest = _digest(value)
        envelope = {"schema": "dag-inbox-command/v1", "command": value, "digest": digest}
        with self._lock():
            destination = self.inbox_dir / "commands" / (digest[7:] + ".json")
            if destination.exists() and _read_json(destination) != envelope:
                raise DuplicateCommandError("inbox command digest collision")
            if not destination.exists():
                _atomic_json(destination, envelope)
            head = self._load_head()
        return {"accepted": False, "queued": True, "command_digest": digest, "head": _copy(head)}

    def read_state(self) -> Dict[str, Any]:
        try:
            with self._lock():
                state, head = self._load_current()
                if not self._projection_is_current(head):
                    self._rebuild_projection_locked(state, head)
                return _copy(state)
        except (IntegrityBlockedError, DAGCycleError) as exc:
            self._record_integrity_block(str(exc))
            raise IntegrityBlockedError(str(exc)) from exc

    def resume(
        self,
        *,
        expected_revision: Optional[int] = None,
        expected_state_revision: Optional[int] = None,
        expected_head_revision: Optional[int] = None,
        expected_head_digest: Optional[str] = None,
        expected_workflow_version: Optional[str] = None,
    ) -> Dict[str, Any]:
        try:
            # Keep HEAD, its parent-linked state, and any rebuilt projection in
            # one lock-held snapshot.  Returning a state read followed by a
            # separate HEAD read can otherwise expose revision N with HEAD
            # N+1 to a fresh reader.
            with self._lock():
                state, head = self._load_current()
                if not self._projection_is_current(head):
                    self._rebuild_projection_locked(state, head)
                revisions = [
                    value
                    for value in (expected_revision, expected_state_revision, expected_head_revision)
                    if value is not None
                ]
                if revisions and any(value != revisions[0] for value in revisions[1:]):
                    raise StaleHeadError("resume revision guards disagree")
                if expected_revision is None:
                    expected_revision = revisions[0] if revisions else None
                if expected_revision is not None and expected_revision != state["revision"]:
                    raise StaleHeadError("state revision is stale")
                if expected_head_digest is not None and expected_head_digest not in (head["transaction_digest"], head["digest"]):
                    raise StaleHeadError("HEAD digest is stale")
                if expected_workflow_version is not None and expected_workflow_version != state["workflow_version"]:
                    raise StaleHeadError("workflow version is stale")
                section = state.get("metadata", {}).get("section_control")
                if isinstance(section, Mapping):
                    accepted = section.get("transition") == {"intent": "source-transition-fixture-passed", "state": "accepted"}
                    receipt = section.get("accepted_section_receipt")
                    evidence = ({"state": "accepted", "receipt_digest": receipt["digest"],
                                 "head": {"revision": head["revision"], "transaction_digest": head["transaction_digest"]}}
                                if accepted and isinstance(receipt, Mapping) and isinstance(receipt.get("digest"), str)
                                else {"state": "pending"})
                    result = {
                        "schema": "section-status/v1", "section_id": section["section_id"],
                        "status": state["status"], "group": _copy(state["group"]), "epoch": _copy(state["epoch"]),
                        "ready": self._ready_tasks(state, head), "head": _copy(head),
                        "claims": {"source_transition_fixture_passed": accepted, "actual_a7": False, "activation": False, "full_ready": False},
                        "transition_evidence": evidence,
                    }
                    if section.get("section_history"):
                        result["section_history"] = _copy(section["section_history"])
                    return result
                return {
                    "run_id": state["run_id"],
                    "workflow_version": state["workflow_version"],
                    "graph_version": GRAPH_VERSION,
                    "revision": state["revision"],
                    "state_revision": state["revision"],
                    "status": state["status"],
                    "group": _copy(state["group"]),
                    "epoch": _copy(state["epoch"]),
                    "objective_ref": _copy(state["objective_ref"]),
                    "ready": self._ready_tasks(state, head),
                    "open_blocking_findings": [key for key, value in state["findings"].items() if self._finding_blocks(value)],
                    "head": _copy(head),
                }
        except (IntegrityBlockedError, DAGCycleError) as exc:
            self._record_integrity_block(str(exc))
            raise IntegrityBlockedError(str(exc)) from exc

    status = resume
    get_status = resume
    read_head = head

    def ready_tasks(self) -> List[str]:
        # Keep the state and canonical HEAD in one lock-held snapshot.  A
        # separate read_state()/head() pair can otherwise route a task using
        # an authority bound to a revision that is no longer current.
        try:
            with self._lock():
                state, head = self._load_current()
                if not self._projection_is_current(head):
                    self._rebuild_projection_locked(state, head)
                return self._ready_tasks(state, head)
        except (IntegrityBlockedError, DAGCycleError) as exc:
            self._record_integrity_block(str(exc))
            raise IntegrityBlockedError(str(exc)) from exc

    def validate_dag(self) -> bool:
        try:
            with self._lock():
                state, _ = self._load_current()
                self._validate_edges(state.get("edges", []), state.get("nodes", {}), exact=True)
                return True
        except (IntegrityBlockedError, DAGCycleError) as exc:
            self._record_integrity_block(str(exc))
            raise IntegrityBlockedError(str(exc)) from exc

    check_acyclic = validate_dag

    def claim_task(
        self,
        task_id: str,
        *,
        assignment_id: str,
        authority_ref: Optional[Mapping[str, Any]] = None,
        idempotency_key: Optional[str] = None,
    ) -> Dict[str, Any]:
        return self.apply(self._command_for("claim_task", {"task_id": task_id, "assignment_id": assignment_id}, authority_ref=authority_ref, idempotency_key=idempotency_key))

    def release_task(
        self,
        task_id: str,
        *,
        assignment_id: str,
        authority_ref: Optional[Mapping[str, Any]] = None,
        idempotency_key: Optional[str] = None,
    ) -> Dict[str, Any]:
        return self.apply(self._command_for("release_task", {"task_id": task_id, "assignment_id": assignment_id}, authority_ref=authority_ref, idempotency_key=idempotency_key))

    def invalidate_task(
        self,
        task_id: str,
        reason: str,
        *,
        authority_ref: Optional[Mapping[str, Any]] = None,
        idempotency_key: Optional[str] = None,
    ) -> Dict[str, Any]:
        """Record an explicit invalidation; late work is never routed silently."""

        return self.apply(
            self._command_for(
                "invalidate_task",
                {"task_id": task_id, "reason": reason},
                authority_ref=authority_ref,
                idempotency_key=idempotency_key,
            )
        )

    def replan_task(
        self,
        task_id: str,
        reason: str,
        *,
        authority_ref: Optional[Mapping[str, Any]] = None,
        idempotency_key: Optional[str] = None,
    ) -> Dict[str, Any]:
        """Mark a Task for an explicit replacement/replan transition."""

        return self.apply(
            self._command_for(
                "replan_task",
                {"task_id": task_id, "reason": reason},
                authority_ref=authority_ref,
                idempotency_key=idempotency_key,
            )
        )

    def open_epoch(
        self,
        epoch_id: str,
        input_ref: Mapping[str, Any],
        *,
        authority_ref: Optional[Mapping[str, Any]] = None,
        idempotency_key: Optional[str] = None,
    ) -> Dict[str, Any]:
        return self.apply(self._command_for("open_epoch", {"epoch_id": epoch_id, "input_ref": _copy(input_ref)}, authority_ref=authority_ref, input_refs=[input_ref], idempotency_key=idempotency_key))

    def open_review_epoch(
        self,
        epoch_id: str,
        input_ref: Mapping[str, Any],
        *,
        boundary_reason: str = "review-attempt",
        reviewer_assignment_id: Optional[str] = None,
        authority_ref: Optional[Mapping[str, Any]] = None,
        idempotency_key: Optional[str] = None,
    ) -> Dict[str, Any]:
        """Register a fresh, object-bound Context Epoch for a review attempt."""

        if not isinstance(reviewer_assignment_id, str) or not reviewer_assignment_id:
            raise ReviewProvenanceError("registered independent reviewer assignment is required")
        payload = {"epoch_id": epoch_id, "input_ref": _copy(input_ref), "boundary_reason": boundary_reason, "reviewer_assignment_id": reviewer_assignment_id}
        return self.apply(self._command_for("open_review_epoch", payload, authority_ref=authority_ref, input_refs=[input_ref], idempotency_key=idempotency_key))

    def rebuild_projection(self) -> Dict[str, Any]:
        with self._lock():
            try:
                state, head = self._load_current()
                self._rebuild_projection_locked(state, head)
                return {"revision": state["revision"], "head": _copy(head)}
            except (IntegrityBlockedError, DAGCycleError) as exc:
                self._record_integrity_block(str(exc))
                raise IntegrityBlockedError(str(exc)) from exc

    def recover(self) -> Dict[str, Any]:
        """Quarantine uncommitted staging and report published orphans.

        Recovery never advances HEAD and never rewrites a transaction chain.
        A valid HEAD's projections are rebuilt; a corrupt HEAD remains blocked.
        """

        with self._lock(create=True):
            quarantined: List[str] = []
            for stage in sorted(self.staging_dir.glob("stage-*")):
                destination = self.quarantine_dir / (stage.name + "-" + uuid.uuid4().hex)
                os.replace(str(stage), str(destination))
                quarantined.append(destination.name)

            def record_unanchored_transactions(reachable: set[str]) -> List[str]:
                orphaned: List[str] = []
                for tx_path in self.transactions_dir.glob("*.json"):
                    digest = "sha256:" + tx_path.stem
                    if digest in reachable:
                        continue
                    orphaned.append(digest)
                    marker = self.quarantine_dir / ("orphan-" + tx_path.stem + ".json")
                    if not marker.exists():
                        _atomic_json(
                            marker,
                            {
                                "schema": "orphan-transaction/v1",
                                "transaction_digest": digest,
                                "reason": "published without HEAD",
                                "source": str(tx_path.name),
                            },
                        )
                return sorted(orphaned)

            reachable: set[str] = set()
            orphaned: List[str] = []
            if self.head_path.exists():
                try:
                    state, head = self._load_current()
                    current = self._load_transaction(head["transaction_digest"])
                    while True:
                        reachable.add(current["digest"])
                        parent = current.get("parent")
                        if not parent:
                            break
                        current = self._load_transaction(parent.get("transaction_digest") or parent.get("digest"))
                    self._rebuild_projection_locked(state, head)
                except (IntegrityBlockedError, DAGCycleError) as exc:
                    self._record_integrity_block(str(exc))
                    raise IntegrityBlockedError(str(exc)) from exc
                orphaned = record_unanchored_transactions(reachable)
            else:
                # A crash while publishing the genesis transaction can leave
                # a valid transaction store without a HEAD at all.  Such
                # transactions are still orphan evidence; recovery must not
                # silently discard or adopt them as a new canonical history.
                orphaned = record_unanchored_transactions(reachable)
            return {"quarantined_staging": quarantined, "orphan_transactions": sorted(orphaned), "head_unchanged": True}

    def projections(self) -> Dict[str, Any]:
        try:
            with self._lock():
                state, head = self._load_current()
                if not self._projection_is_current(head):
                    self._rebuild_projection_locked(state, head)
                result: Dict[str, Any] = {}
                for name in ("run.json", "status.json", "ready.json"):
                    result[name] = _read_json(self.projection_dir / name)
                result["state"] = _copy(state)
                result["head"] = _copy(head)
                return result
        except (IntegrityBlockedError, DAGCycleError) as exc:
            self._record_integrity_block(str(exc))
            raise IntegrityBlockedError(str(exc)) from exc


# Names used by early A6R design notes and convenient for callers that do not
# want to know the implementation's historical class name.
A6RKernel = ControlKernel
ArtifactDAGControlKernel = ControlKernel
FileBackedKernel = ControlKernel


def route_lifecycle(mode: str, *, root: Optional[Union[str, os.PathLike]] = None, run_id: Optional[str] = None) -> Dict[str, Any]:
    """Return a safe one-off/managed route without implicitly creating state."""

    if mode == "one-off":
        return {"mode": "one-off", "managed": False, "kernel": None}
    if mode == "managed":
        if root is None or run_id is None:
            raise KernelError("managed routing requires root and run_id")
        return {"mode": "managed", "managed": True, "kernel": ControlKernel(root, run_id)}
    raise KernelError("unknown lifecycle mode: %s" % mode)


__all__ = [
    "A6RKernel",
    "ArtifactDAGControlKernel",
    "AuthorizationError",
    "BUDGET_POLICY",
    "BudgetError",
    "CommandValidationError",
    "ControlKernel",
    "DAGCycleError",
    "DuplicateCommandError",
    "FileBackedKernel",
    "InjectedCrash",
    "IntegrityBlockedError",
    "LifecycleClosedError",
    "KernelError",
    "NotFoundError",
    "ProtectedFieldError",
    "ObjectValidationError",
    "ResultAcceptanceError",
    "ReviewProvenanceError",
    "StaleHeadError",
    "canonical_digest",
    "route_lifecycle",
]
