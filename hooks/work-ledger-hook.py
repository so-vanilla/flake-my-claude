#!/usr/bin/env python3
"""Deterministic work-ledger CLI and Claude Code/Codex hook adapter.

This helper never invokes a model or summarizes a transcript.  It owns only
mechanical state: hashes, the active pointer, worker-report validation, runtime
receipts, and short event-specific hook output.
"""

from __future__ import annotations

import argparse
import contextlib
import datetime as dt
import fcntl
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import tempfile
from typing import Any, Iterator, Sequence


LEDGER_SCHEMA = "agent-work-ledger/v1"
REPORT_SCHEMA = "agent-worker-report/v1"
ACTIVE_RELATIVE = Path(".local/agent/workplans/active.json")
VALID_STATUSES = {"active", "paused", "complete", "blocked"}
VALID_REPORT_STATUSES = {"done", "partial", "blocked"}
REQUIRED_REPORT_HEADINGS = (
    "Summary",
    "Findings or changes",
    "Decisions for parent",
    "Validation",
    "Remaining or blocked",
    "Files changed",
)
IDENTIFIER = re.compile(r"^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$")


class LedgerError(RuntimeError):
    """A user-actionable ledger validation or setup failure."""


class HookBlock(RuntimeError):
    """A hook event that must be blocked through exit code 2 and stderr."""


def utc_now() -> str:
    return dt.datetime.now(dt.timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")


def hash_file(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(65536), b""):
            digest.update(chunk)
    return digest.hexdigest()


def safe_identifier(value: str, label: str) -> str:
    if not IDENTIFIER.fullmatch(value):
        raise LedgerError(f"invalid {label}: use 1-128 ASCII letters, digits, dot, underscore, or dash")
    return value


def run_git(args: Sequence[str], cwd: Path, check: bool = True) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        ["git", *args],
        cwd=str(cwd),
        check=check,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )


def find_repo_root(start: Path | None = None) -> Path:
    start = (start or Path.cwd()).resolve()
    result = run_git(["rev-parse", "--show-toplevel"], start, check=False)
    if result.returncode != 0:
        raise LedgerError("no Git repository root; repo-local durable state is unavailable")
    return Path(result.stdout.strip()).resolve()


def private_workspace_is_ignored(root: Path) -> bool:
    probe = ".local/agent/.work-ledger-ignore-probe"
    result = run_git(["check-ignore", "-q", "--no-index", probe], root, check=False)
    return result.returncode == 0


def require_private_workspace(root: Path) -> None:
    if not private_workspace_is_ignored(root):
        raise LedgerError(
            "repo-root .local/ is not ignored; configure an anchored /.local/ rule before private writes"
        )


def confined_path(root: Path, relative: str, label: str) -> Path:
    candidate = (root / relative).resolve()
    try:
        candidate.relative_to(root)
    except ValueError as exc:
        raise LedgerError(f"{label} escapes the repository root") from exc
    return candidate


def atomic_write_text(path: Path, text: str, mode: int = 0o600) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary = tempfile.mkstemp(prefix=f".{path.name}.", dir=str(path.parent))
    temporary_path = Path(temporary)
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8") as handle:
            handle.write(text)
            handle.flush()
            os.fsync(handle.fileno())
        os.chmod(temporary_path, mode)
        os.replace(temporary_path, path)
    finally:
        if temporary_path.exists():
            temporary_path.unlink()


def atomic_write_json(path: Path, value: Any) -> None:
    atomic_write_text(path, json.dumps(value, ensure_ascii=False, indent=2) + "\n")


@contextlib.contextmanager
def state_lock(root: Path) -> Iterator[None]:
    lock_path = root / ".local/agent/runtime/.work-ledger.lock"
    lock_path.parent.mkdir(parents=True, exist_ok=True)
    with lock_path.open("a+", encoding="utf-8") as handle:
        fcntl.flock(handle.fileno(), fcntl.LOCK_EX)
        try:
            yield
        finally:
            fcntl.flock(handle.fileno(), fcntl.LOCK_UN)


def active_path(root: Path) -> Path:
    return root / ACTIVE_RELATIVE


def read_json(path: Path, label: str) -> dict[str, Any]:
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
    except FileNotFoundError as exc:
        raise LedgerError(f"missing {label}: {path}") from exc
    except json.JSONDecodeError as exc:
        raise LedgerError(f"malformed {label}: {path}: {exc}") from exc
    if not isinstance(value, dict):
        raise LedgerError(f"{label} must be a JSON object: {path}")
    return value


def validate_active(root: Path, active: dict[str, Any], require_fresh_hash: bool = True) -> tuple[Path, list[str]]:
    errors: list[str] = []
    required = {
        "schema",
        "work_id",
        "ledger_path",
        "status",
        "checkpoint_seq",
        "checkpoint_at",
        "checkpoint_reason",
        "ledger_sha256",
        "next_action",
        "writer",
    }
    missing = sorted(required - set(active))
    if missing:
        errors.append(f"active pointer missing keys: {', '.join(missing)}")
    if active.get("schema") != LEDGER_SCHEMA:
        errors.append(f"active pointer schema must be {LEDGER_SCHEMA}")
    work_id = str(active.get("work_id", ""))
    try:
        safe_identifier(work_id, "work_id")
    except LedgerError as exc:
        errors.append(str(exc))
    if active.get("status") not in VALID_STATUSES:
        errors.append("active pointer status is invalid")
    if not isinstance(active.get("checkpoint_seq"), int) or active.get("checkpoint_seq", 0) < 1:
        errors.append("checkpoint_seq must be a positive integer")
    if not isinstance(active.get("next_action"), str) or not active.get("next_action", "").strip():
        errors.append("next_action must be non-empty")
    ledger_relative = str(active.get("ledger_path", ""))
    ledger = confined_path(root, ledger_relative, "ledger_path") if ledger_relative else root
    expected = root / ".local/agent/workplans" / work_id / "ledger.md"
    if ledger != expected.resolve():
        errors.append("ledger_path does not match work_id")
    if not ledger.is_file():
        errors.append(f"ledger file is missing: {ledger_relative}")
    elif require_fresh_hash and active.get("ledger_sha256") != hash_file(ledger):
        errors.append("ledger hash differs from the last checkpoint")
    writer = active.get("writer")
    if not isinstance(writer, dict) or not isinstance(writer.get("harness"), str):
        errors.append("writer must contain a harness")
    return ledger, errors


def load_active(root: Path, require_fresh_hash: bool = True) -> tuple[dict[str, Any], Path, list[str]]:
    active = read_json(active_path(root), "active ledger pointer")
    ledger, errors = validate_active(root, active, require_fresh_hash=require_fresh_hash)
    return active, ledger, errors


def ledger_template(work_id: str) -> str:
    return f"""# Work ledger: {work_id}

## Objective

Replace this line with the requested outcome and measurable completion condition.

## Constraints and authority

- Approved scope:
- Excluded scope:
- External or destructive authority:

## Ordered route

1. Classify and inspect.
2. Replace this route with the task-specific phases.

## Tickets and dependencies

| ticket | owner/model | dependencies | status | scope | acceptance |
| --- | --- | --- | --- | --- | --- |

## Decisions

Record human and material AI decisions with rationale and rejected alternatives.

## Progress and changed files

- Done:
- In progress:
- Remaining:
- Blocked or deferred:
- Changed files:

## Validation evidence

- Not run yet.

## Risks and unresolved questions

- None recorded yet.

## Exact next action

Replace this line before dispatch or stopping.
"""


def writer(harness: str, session_id: str) -> dict[str, str]:
    return {"harness": harness, "session_id": session_id}


def command_init(args: argparse.Namespace) -> dict[str, Any]:
    root = find_repo_root(Path(args.cwd) if args.cwd else None)
    require_private_workspace(root)
    work_id = safe_identifier(args.work_id, "work_id")
    if args.status not in VALID_STATUSES:
        raise LedgerError(f"status must be one of {', '.join(sorted(VALID_STATUSES))}")
    if not args.next_action.strip():
        raise LedgerError("next-action must be non-empty")
    with state_lock(root):
        if active_path(root).exists():
            raise LedgerError("an active ledger pointer already exists; use status/resume instead of guessing")
        ledger = root / ".local/agent/workplans" / work_id / "ledger.md"
        if ledger.exists():
            raise LedgerError(f"ledger already exists: {ledger.relative_to(root)}")
        atomic_write_text(ledger, ledger_template(work_id))
        timestamp = utc_now()
        active = {
            "schema": LEDGER_SCHEMA,
            "work_id": work_id,
            "ledger_path": str(ledger.relative_to(root)),
            "status": args.status,
            "checkpoint_seq": 1,
            "checkpoint_at": timestamp,
            "checkpoint_reason": "initialized",
            "ledger_sha256": hash_file(ledger),
            "next_action": args.next_action.strip(),
            "writer": writer(args.harness, args.session_id),
        }
        atomic_write_json(active_path(root), active)
    return active


def runtime_root(root: Path, work_id: str) -> Path:
    return root / ".local/agent/runtime" / work_id


def clear_checkpoint_requirements(root: Path, work_id: str, checkpoint_seq: int) -> None:
    base = runtime_root(root, work_id)
    if not base.exists():
        return
    for metadata_path in base.glob("*/agents/*.json"):
        try:
            metadata = read_json(metadata_path, "worker metadata")
        except LedgerError:
            continue
        if metadata.get("checkpoint_required"):
            metadata["checkpoint_required"] = False
            metadata["assimilated_at_seq"] = checkpoint_seq
            metadata["assimilated_at"] = utc_now()
            atomic_write_json(metadata_path, metadata)
    unclean = base / "unclean-session.json"
    if unclean.exists():
        unclean.unlink()


def command_checkpoint(args: argparse.Namespace) -> dict[str, Any]:
    root = find_repo_root(Path(args.cwd) if args.cwd else None)
    require_private_workspace(root)
    if args.status not in VALID_STATUSES:
        raise LedgerError(f"status must be one of {', '.join(sorted(VALID_STATUSES))}")
    if not args.reason.strip() or not args.next_action.strip():
        raise LedgerError("reason and next-action must be non-empty")
    with state_lock(root):
        active, ledger, errors = load_active(root, require_fresh_hash=False)
        structural = [error for error in errors if error != "ledger hash differs from the last checkpoint"]
        if structural:
            raise LedgerError("; ".join(structural))
        active.update(
            {
                "status": args.status,
                "checkpoint_seq": int(active["checkpoint_seq"]) + 1,
                "checkpoint_at": utc_now(),
                "checkpoint_reason": args.reason.strip(),
                "ledger_sha256": hash_file(ledger),
                "next_action": args.next_action.strip(),
                "writer": writer(args.harness, args.session_id),
            }
        )
        atomic_write_json(active_path(root), active)
        clear_checkpoint_requirements(root, active["work_id"], active["checkpoint_seq"])
    return active


def parse_frontmatter(text: str) -> tuple[dict[str, str], str]:
    if not text.startswith("---\n"):
        raise LedgerError("worker report must start with YAML frontmatter")
    end = text.find("\n---\n", 4)
    if end < 0:
        raise LedgerError("worker report frontmatter is not closed")
    metadata: dict[str, str] = {}
    for line in text[4:end].splitlines():
        if not line.strip() or line.lstrip().startswith("#"):
            continue
        if ":" not in line:
            raise LedgerError(f"invalid worker report frontmatter line: {line}")
        key, value = line.split(":", 1)
        key = key.strip()
        value = value.strip().strip("\"").strip("'")
        if not key or not value:
            raise LedgerError("worker report frontmatter keys and values must be non-empty")
        if key in metadata:
            raise LedgerError(f"duplicate worker report frontmatter key: {key}")
        metadata[key] = value
    return metadata, text[end + 5 :]


def validate_report(
    report: Path,
    work_id: str | None = None,
    agent_id: str | None = None,
    ticket_id: str | None = None,
) -> list[str]:
    errors: list[str] = []
    try:
        text = report.read_text(encoding="utf-8")
    except FileNotFoundError:
        return [f"worker report is missing: {report}"]
    try:
        metadata, body = parse_frontmatter(text)
    except LedgerError as exc:
        return [str(exc)]
    required = {"schema", "work_id", "agent_id", "ticket_id", "status"}
    missing = sorted(required - set(metadata))
    if missing:
        errors.append(f"worker report missing frontmatter: {', '.join(missing)}")
    if metadata.get("schema") != REPORT_SCHEMA:
        errors.append(f"worker report schema must be {REPORT_SCHEMA}")
    if metadata.get("status") not in VALID_REPORT_STATUSES:
        errors.append("worker report status must be done, partial, or blocked")
    for key, expected in (("work_id", work_id), ("agent_id", agent_id), ("ticket_id", ticket_id)):
        if expected is not None and metadata.get(key) != expected:
            errors.append(f"worker report {key} does not match expected {expected}")
    matches = list(re.finditer(r"(?m)^# ([^\n]+)\n", body))
    sections: dict[str, str] = {}
    for index, match in enumerate(matches):
        end = matches[index + 1].start() if index + 1 < len(matches) else len(body)
        sections[match.group(1).strip()] = body[match.end() : end].strip()
    for heading in REQUIRED_REPORT_HEADINGS:
        if heading not in sections:
            errors.append(f"worker report missing H1 heading: {heading}")
        elif not sections[heading]:
            errors.append(f"worker report heading is empty: {heading}")
    return errors


def command_validate(args: argparse.Namespace) -> dict[str, Any]:
    root = find_repo_root(Path(args.cwd) if args.cwd else None)
    if args.report:
        report = Path(args.report)
        if not report.is_absolute():
            report = confined_path(root, args.report, "report path")
        errors = validate_report(report, args.work_id, args.agent_id, args.ticket_id)
        result = {"valid": not errors, "kind": "worker-report", "path": str(report), "errors": errors}
    else:
        active, ledger, errors = load_active(root, require_fresh_hash=True)
        result = {
            "valid": not errors,
            "kind": "active-ledger",
            "work_id": active.get("work_id"),
            "ledger_path": str(ledger),
            "errors": errors,
        }
    if not result["valid"]:
        raise LedgerError(json.dumps(result, ensure_ascii=False))
    return result


def pending_worker_states(root: Path, active: dict[str, Any]) -> list[str]:
    base = runtime_root(root, str(active["work_id"]))
    if not base.exists():
        return []
    reasons: list[str] = []
    for metadata_path in base.glob("*/agents/*.json"):
        try:
            metadata = read_json(metadata_path, "worker metadata")
        except LedgerError as exc:
            reasons.append(str(exc))
            continue
        state = metadata.get("report_status", "dispatched")
        if metadata.get("checkpoint_required"):
            reasons.append(f"worker report needs root assimilation: {metadata.get('report_path', metadata_path)}")
        elif state in {"dispatched", "invalid"}:
            reasons.append(f"worker report state is {state}: {metadata.get('report_path', metadata_path)}")
    return reasons


def dirty_reasons(root: Path, active: dict[str, Any], ledger: Path) -> list[str]:
    reasons: list[str] = []
    if ledger.is_file() and active.get("ledger_sha256") != hash_file(ledger):
        reasons.append("ledger changed since the last checkpoint")
    reasons.extend(pending_worker_states(root, active))
    return reasons


def command_status(args: argparse.Namespace) -> dict[str, Any]:
    root = find_repo_root(Path(args.cwd) if args.cwd else None)
    active, ledger, structural_errors = load_active(root, require_fresh_hash=False)
    structural_errors = [error for error in structural_errors if error != "ledger hash differs from the last checkpoint"]
    unclean_path = runtime_root(root, str(active.get("work_id", "unknown"))) / "unclean-session.json"
    result = {
        "schema": active.get("schema"),
        "work_id": active.get("work_id"),
        "status": active.get("status"),
        "ledger_path": str(ledger.relative_to(root)) if ledger != root else str(ledger),
        "checkpoint_seq": active.get("checkpoint_seq"),
        "checkpoint_at": active.get("checkpoint_at"),
        "checkpoint_reason": active.get("checkpoint_reason"),
        "next_action": active.get("next_action"),
        "dirty_reasons": dirty_reasons(root, active, ledger) if not structural_errors else [],
        "unclean": unclean_path.exists(),
        "errors": structural_errors,
    }
    return result


def read_hook_payload() -> dict[str, Any]:
    raw = sys.stdin.read()
    if not raw.strip():
        return {}
    value = json.loads(raw)
    if not isinstance(value, dict):
        raise LedgerError("hook stdin must be one JSON object")
    return value


def hook_event(payload: dict[str, Any], explicit: str | None = None) -> str:
    event = explicit or payload.get("hook_event_name") or payload.get("eventName") or payload.get("event_name")
    if not isinstance(event, str) or not event:
        raise LedgerError("hook event name is missing")
    return event


def emit_context(event: str, message: str) -> dict[str, Any]:
    return {"hookSpecificOutput": {"hookEventName": event, "additionalContext": message}}


def emit_message(message: str) -> dict[str, Any]:
    return {"systemMessage": message}


def emit_block(reason: str) -> dict[str, Any]:
    return {"decision": "block", "reason": reason}


def session_id(payload: dict[str, Any]) -> str:
    value = payload.get("session_id") or payload.get("sessionId") or "unknown-session"
    return safe_identifier(re.sub(r"[^A-Za-z0-9._-]", "-", str(value))[:128], "session_id")


def agent_id(payload: dict[str, Any]) -> str:
    value = payload.get("agent_id") or payload.get("agentId")
    if not value:
        raise LedgerError("subagent hook payload has no agent_id")
    return safe_identifier(re.sub(r"[^A-Za-z0-9._-]", "-", str(value))[:128], "agent_id")


def append_event(root: Path, active: dict[str, Any], payload: dict[str, Any], event: str, outcome: str) -> None:
    sid = session_id(payload)
    path = runtime_root(root, str(active["work_id"])) / sid / "events.jsonl"
    path.parent.mkdir(parents=True, exist_ok=True)
    record = {
        "at": utc_now(),
        "event": event,
        "outcome": outcome,
        "session_id": sid,
        "turn_id": payload.get("turn_id") or payload.get("turnId"),
        "agent_id": payload.get("agent_id") or payload.get("agentId"),
        "checkpoint_seq": active.get("checkpoint_seq"),
    }
    with path.open("a", encoding="utf-8") as handle:
        handle.write(json.dumps(record, ensure_ascii=False, separators=(",", ":")) + "\n")
        handle.flush()
        os.fsync(handle.fileno())


def receipt_path(
    root: Path, active: dict[str, Any], payload: dict[str, Any], event: str
) -> Path:
    operation_id = str(payload.get("task_id") or payload.get("taskId") or "") if event == "TaskCompleted" else ""
    parts = [
        str(active.get("work_id", "")),
        session_id(payload),
        str(payload.get("agent_id") or payload.get("agentId") or ""),
        event,
        operation_id,
        str(active.get("checkpoint_seq", "")),
    ]
    key = hashlib.sha256("\0".join(parts).encode("utf-8")).hexdigest()
    return runtime_root(root, str(active["work_id"])) / session_id(payload) / "receipts" / f"{key}.json"


def first_correction_allowed(
    root: Path, active: dict[str, Any], payload: dict[str, Any], event: str
) -> bool:
    if payload.get("stop_hook_active") is True:
        return False
    receipt = receipt_path(root, active, payload, event)
    if receipt.exists():
        return False
    atomic_write_json(receipt, {"at": utc_now(), "event": event, "checkpoint_seq": active["checkpoint_seq"]})
    return True


def write_unclean(root: Path, active: dict[str, Any], payload: dict[str, Any], reasons: list[str], event: str) -> None:
    marker = runtime_root(root, str(active["work_id"])) / "unclean-session.json"
    atomic_write_json(
        marker,
        {
            "schema": "agent-work-ledger-unclean/v1",
            "at": utc_now(),
            "event": event,
            "session_id": session_id(payload),
            "checkpoint_seq": active.get("checkpoint_seq"),
            "reasons": reasons,
        },
    )


def worker_metadata_path(root: Path, active: dict[str, Any], payload: dict[str, Any]) -> Path:
    return runtime_root(root, str(active["work_id"])) / session_id(payload) / "agents" / f"{agent_id(payload)}.json"


def find_worker_metadata(root: Path, active: dict[str, Any], payload: dict[str, Any]) -> Path:
    direct = worker_metadata_path(root, active, payload)
    if direct.exists():
        return direct
    candidates = list(runtime_root(root, str(active["work_id"])).glob(f"*/agents/{agent_id(payload)}.json"))
    if len(candidates) == 1:
        return candidates[0]
    if not candidates:
        raise LedgerError("no SubagentStart metadata exists for this agent")
    raise LedgerError("ambiguous worker metadata for agent_id")


def assigned_ticket_id(payload: dict[str, Any]) -> str | None:
    direct = payload.get("ticket_id") or payload.get("ticketId")
    return safe_identifier(str(direct), "ticket_id") if direct else None


def handle_session_start(root: Path, active: dict[str, Any], ledger: Path, payload: dict[str, Any], event: str) -> dict[str, Any]:
    unclean = runtime_root(root, str(active["work_id"])) / "unclean-session.json"
    dirty = dirty_reasons(root, active, ledger)
    details = [
        f"Resume durable work from {active['ledger_path']}.",
        f"Status: {active['status']}; checkpoint: {active['checkpoint_seq']} at {active['checkpoint_at']}.",
        f"Exact next action: {active['next_action']}",
    ]
    if unclean.exists() or dirty:
        details.append("Warning: later work may be uncheckpointed; run route-work status and reconcile before dispatch.")
    append_event(root, active, payload, event, "context-injected")
    return emit_context(event, " ".join(details))


def handle_subagent_start(root: Path, active: dict[str, Any], ledger: Path, payload: dict[str, Any], event: str) -> dict[str, Any]:
    aid = agent_id(payload)
    ticket = assigned_ticket_id(payload)
    report = root / ".local/agent/reports" / str(active["work_id"]) / f"{aid}.md"
    metadata_path = worker_metadata_path(root, active, payload)
    if metadata_path.exists():
        metadata = read_json(metadata_path, "worker metadata")
    else:
        metadata = {
            "schema": "agent-worker-runtime/v1",
            "work_id": active["work_id"],
            "agent_id": aid,
            "agent_type": payload.get("agent_type") or payload.get("agentType"),
            "ticket_id": ticket,
            "session_id": session_id(payload),
            "started_at": utc_now(),
            "dispatch_ledger_sha256": hash_file(ledger),
            "dispatch_checkpoint_seq": active["checkpoint_seq"],
            "report_path": str(report.relative_to(root)),
            "report_status": "dispatched",
            "checkpoint_required": False,
        }
        atomic_write_json(metadata_path, metadata)
    append_event(root, active, payload, event, "worker-metadata-created")
    assignment_note = (
        f" Hook-observed ticket: {metadata['ticket_id']}."
        if metadata.get("ticket_id")
        else " The harness did not expose a root-assigned ticket ID; root must validate report ticket_id explicitly before assimilation."
    )
    message = (
        f"Write your required report to {metadata['report_path']}. "
        "Do not edit the main ledger or active pointer. Use schema agent-worker-report/v1 and the six required H1 sections."
        + assignment_note
    )
    return emit_context(event, message)


def handle_subagent_stop(root: Path, active: dict[str, Any], ledger: Path, payload: dict[str, Any], event: str) -> dict[str, Any]:
    try:
        metadata_path = find_worker_metadata(root, active, payload)
    except LedgerError as exc:
        reasons = [str(exc)]
        write_unclean(root, active, payload, reasons, event)
        append_event(root, active, payload, event, "failed-open-missing-worker-metadata")
        return emit_message(f"Worker stop could not be matched to its dispatch; root must reconcile: {exc}")
    metadata = read_json(metadata_path, "worker metadata")
    report = confined_path(root, str(metadata["report_path"]), "worker report path")
    expected_ticket = metadata.get("ticket_id")
    errors = validate_report(report, str(active["work_id"]), agent_id(payload), expected_ticket)
    if metadata.get("dispatch_ledger_sha256") != hash_file(ledger):
        errors.append("main ledger changed during the worker turn")
    if errors:
        metadata["report_status"] = "invalid"
        metadata["validation_errors"] = errors
        metadata["stopped_at"] = utc_now()
        atomic_write_json(metadata_path, metadata)
        reason = "Worker report is not valid: " + "; ".join(errors) + ". Repair only the assigned report, then stop again."
        if first_correction_allowed(root, active, payload, event):
            append_event(root, active, payload, event, "blocked-once-for-report")
            return emit_block(reason)
        write_unclean(root, active, payload, errors, event)
        append_event(root, active, payload, event, "failed-open-invalid-report")
        return emit_message("Worker stopped with an invalid report; root must reconcile: " + "; ".join(errors))
    metadata["report_status"] = "valid"
    metadata["checkpoint_required"] = True
    metadata["stopped_at"] = utc_now()
    metadata.pop("validation_errors", None)
    atomic_write_json(metadata_path, metadata)
    append_event(root, active, payload, event, "report-valid-checkpoint-required")
    ticket_note = " Root must also validate the expected ticket_id because some harnesses do not expose it to SubagentStart."
    return emit_message(f"Validated worker report {metadata['report_path']}; root must assimilate it and checkpoint.{ticket_note}")


def handle_checkpoint_gate(
    root: Path,
    active: dict[str, Any],
    ledger: Path,
    payload: dict[str, Any],
    event: str,
    allow_block: bool,
) -> dict[str, Any] | None:
    reasons = dirty_reasons(root, active, ledger)
    if not reasons:
        append_event(root, active, payload, event, "clean")
        return None
    if allow_block and first_correction_allowed(root, active, payload, event):
        append_event(root, active, payload, event, "blocked-once-for-checkpoint")
        return emit_block("Checkpoint required before continuing: " + "; ".join(reasons))
    write_unclean(root, active, payload, reasons, event)
    append_event(root, active, payload, event, "failed-open-dirty")
    return emit_message("Session is stopping with uncheckpointed work: " + "; ".join(reasons))


def handle_task_completed(
    root: Path,
    active: dict[str, Any],
    ledger: Path,
    payload: dict[str, Any],
    event: str,
) -> dict[str, Any] | None:
    """Block task completion once using Claude Code's event-specific contract."""
    reasons = dirty_reasons(root, active, ledger)
    if not reasons:
        append_event(root, active, payload, event, "clean")
        return None
    if first_correction_allowed(root, active, payload, event):
        append_event(root, active, payload, event, "blocked-once-for-checkpoint")
        raise HookBlock("Checkpoint required before completing the task: " + "; ".join(reasons))
    write_unclean(root, active, payload, reasons, event)
    append_event(root, active, payload, event, "failed-open-dirty")
    return emit_message("Task is completing with uncheckpointed work: " + "; ".join(reasons))


def handle_hook(args: argparse.Namespace) -> dict[str, Any] | None:
    payload = read_hook_payload()
    event = hook_event(payload, args.event)
    start_value = payload.get("cwd")
    try:
        root = find_repo_root(Path(start_value) if start_value else None)
    except LedgerError:
        return None
    path = active_path(root)
    if not path.exists():
        return None
    try:
        require_private_workspace(root)
        with state_lock(root):
            active, ledger, structural_errors = load_active(root, require_fresh_hash=False)
            structural_errors = [
                error for error in structural_errors if error != "ledger hash differs from the last checkpoint"
            ]
            if structural_errors:
                message = "Work-ledger pointer needs repair: " + "; ".join(structural_errors)
                return emit_context(event, message) if event in {"SessionStart", "SubagentStart"} else emit_message(message)
            if event == "SessionStart":
                return handle_session_start(root, active, ledger, payload, event)
            if event == "SubagentStart":
                return handle_subagent_start(root, active, ledger, payload, event)
            if event == "SubagentStop":
                return handle_subagent_stop(root, active, ledger, payload, event)
            if event == "TaskCompleted":
                return handle_task_completed(root, active, ledger, payload, event)
            if event == "Stop":
                return handle_checkpoint_gate(root, active, ledger, payload, event, allow_block=True)
            if event == "PreCompact":
                trigger = str(payload.get("trigger") or payload.get("source") or payload.get("reason") or "manual")
                return handle_checkpoint_gate(
                    root, active, ledger, payload, event, allow_block=trigger != "auto"
                )
            if event == "PostCompact":
                append_event(root, active, payload, event, "receipt-only")
                return None
            if event == "SessionEnd":
                return handle_checkpoint_gate(root, active, ledger, payload, event, allow_block=False)
            append_event(root, active, payload, event, "ignored")
            return None
    except HookBlock:
        raise
    except Exception as exc:  # Hooks must fail open and keep the harness usable.
        message = f"Work-ledger hook failed open: {exc}"
        return emit_context(event, message) if event in {"SessionStart", "SubagentStart"} else emit_message(message)


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    subparsers = parser.add_subparsers(dest="command", required=True)

    init = subparsers.add_parser("init", help="create one new active ledger")
    init.add_argument("--work-id", required=True)
    init.add_argument("--next-action", required=True)
    init.add_argument("--status", default="active")
    init.add_argument("--harness", default="manual")
    init.add_argument("--session-id", default="manual")
    init.add_argument("--cwd")
    init.add_argument("--json", action="store_true")

    checkpoint = subparsers.add_parser("checkpoint", help="hash the root-edited ledger and update the pointer")
    checkpoint.add_argument("--reason", required=True)
    checkpoint.add_argument("--next-action", required=True)
    checkpoint.add_argument("--status", default="active")
    checkpoint.add_argument("--harness", default="manual")
    checkpoint.add_argument("--session-id", default="manual")
    checkpoint.add_argument("--cwd")
    checkpoint.add_argument("--json", action="store_true")

    status = subparsers.add_parser("status", help="show active durable state")
    status.add_argument("--cwd")
    status.add_argument("--json", action="store_true")

    validate = subparsers.add_parser("validate", help="validate the active ledger or one worker report")
    validate.add_argument("--report")
    validate.add_argument("--work-id")
    validate.add_argument("--agent-id")
    validate.add_argument("--ticket-id")
    validate.add_argument("--cwd")
    validate.add_argument("--json", action="store_true")

    hook = subparsers.add_parser("hook", help="handle one harness hook event from stdin JSON")
    hook.add_argument("--harness", choices=("claude", "codex"), required=True)
    hook.add_argument("--event")
    return parser


def main(argv: Sequence[str] | None = None) -> int:
    raw = list(argv if argv is not None else sys.argv[1:])
    if raw and raw[0] != "--help" and raw[0].startswith("-"):
        raw.insert(0, "hook")
    parser = build_parser()
    args = parser.parse_args(raw)
    try:
        if args.command == "init":
            result = command_init(args)
        elif args.command == "checkpoint":
            result = command_checkpoint(args)
        elif args.command == "status":
            result = command_status(args)
        elif args.command == "validate":
            result = command_validate(args)
        else:
            result = handle_hook(args)
        if result is not None:
            print(json.dumps(result, ensure_ascii=False, indent=2))
        return 0
    except HookBlock as exc:
        print(str(exc), file=sys.stderr)
        return 2
    except (LedgerError, json.JSONDecodeError) as exc:
        if args.command == "hook":
            print(json.dumps({"systemMessage": f"Work-ledger hook failed open: {exc}"}, ensure_ascii=False))
            return 0
        print(f"work-ledger-hook: {exc}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
