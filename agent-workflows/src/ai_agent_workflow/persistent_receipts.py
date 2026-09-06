"""Durable command supervision with F1-compatible terminal receipts."""
from __future__ import annotations

import copy
import hashlib
import json
import os
import signal
import subprocess
import sys
import time
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Mapping

from .schema_validation import SchemaValidationError, validate_document


class PersistentReceiptError(RuntimeError):
    pass


class DuplicateReceiptConflict(PersistentReceiptError):
    pass


RETRY_CLASSES = {"none", "test-fixture-correction", "command-or-capture-retry", "package-or-report-correction"}


def _canonical(value: Any) -> bytes:
    return json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=False).encode()


def _digest(value: Any) -> str:
    return "sha256:" + hashlib.sha256(_canonical(value)).hexdigest()


def _utc() -> str:
    return datetime.now(timezone.utc).isoformat(timespec="microseconds").replace("+00:00", "Z")


def _schema(name: str) -> Mapping[str, Any]:
    return json.loads((Path(__file__).resolve().parents[2] / "schemas" / name).read_text())


def _private_dir(path: Path) -> None:
    path.mkdir(parents=True, exist_ok=True, mode=0o700)
    os.chmod(path, 0o700)


def _write_private(path: Path, data: bytes) -> None:
    _private_dir(path.parent)
    temp = path.with_name(".%s.%s.tmp" % (path.name, os.getpid()))
    descriptor = os.open(temp, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
    try:
        with os.fdopen(descriptor, "wb") as stream:
            stream.write(data)
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(temp, path)
        os.chmod(path, 0o600)
    finally:
        if temp.exists():
            temp.unlink()


class DeterministicProcessAdapter:
    """External-state-shaped fake used only at the public process boundary."""

    def __init__(self) -> None:
        self._queue: list[dict[str, Any]] = []
        self._handles: dict[int, dict[str, Any]] = {}
        self.identities: dict[int, dict[str, Any]] = {}
        self.spawn_count = self.live_count = self.reap_count = 0
        self.signals: list[tuple[str, int]] = []
        self.waits: list[tuple[int, int]] = []
        self.lifecycle: list[str] = []

    def queue(self, **outcome: Any) -> None:
        self._queue.append(dict(outcome))

    def prepare(self, command: list[str], cwd: str, environment: Mapping[str, str]) -> Mapping[str, Any]:
        if not self._queue:
            raise PersistentReceiptError("deterministic adapter has no queued outcome")
        self.spawn_count += 1
        handle = self.spawn_count
        outcome = self._queue.pop(0)
        outcome.update({"active": True, "released": False})
        self._handles[handle] = outcome
        identity = {"pid": handle, "process_group_id": handle, "birth_token": "deterministic-%d" % handle}
        self.identities[handle] = identity
        self.live_count += 1
        self.lifecycle.append("prepared")
        return copy.deepcopy(identity)

    def release(self, identity: Mapping[str, Any]) -> None:
        self._handles[int(identity["pid"])]["released"] = True
        self.lifecycle.append("released")

    def inspect(self, identity: Mapping[str, Any]) -> bool:
        handle = int(identity["pid"])
        actual = {key: identity.get(key) for key in ("pid", "process_group_id", "birth_token")}
        if self.identities.get(handle) != actual:
            raise PersistentReceiptError("persisted process identity is ambiguous")
        return bool(self._handles.get(handle, {}).get("active"))

    def observe(self, identity: Mapping[str, Any], policy: Mapping[str, Any], heartbeat: Any) -> Mapping[str, Any]:
        outcome = self._handles[int(identity["pid"])]
        heartbeat()
        if outcome.get("live"):
            return {"live": True}
        if outcome.get("timeout"):
            return {"timeout": True, "stdout": bytes(outcome.get("stdout", b"")), "stderr": bytes(outcome.get("stderr", b""))}
        outcome["active"] = False
        self.live_count -= 1
        return {"timeout": False, "exit_code": int(outcome.get("exit_code", 0)), "stdout": bytes(outcome.get("stdout", b"")), "stderr": bytes(outcome.get("stderr", b""))}

    def terminate_group(self, identity: Mapping[str, Any] | int, name: str) -> None:
        handle = int(identity if isinstance(identity, int) else identity["pid"])
        self.signals.append((name, handle))
        outcome = self._handles.get(handle)
        if outcome and outcome.get("active") and (name != "TERM" or not outcome.get("ignore_term")):
            outcome["active"] = False
            self.live_count -= 1

    def wait_group(self, identity: Mapping[str, Any] | int, seconds: int) -> bool:
        handle = int(identity if isinstance(identity, int) else identity["pid"])
        self.waits.append((seconds, handle))
        return not bool(self._handles.get(handle, {}).get("active"))

    def reap(self, identity: Mapping[str, Any] | int) -> Mapping[str, Any]:
        handle = int(identity if isinstance(identity, int) else identity["pid"])
        if self._handles.get(handle, {}).get("active"):
            raise PersistentReceiptError("cannot reap a live group")
        self.reap_count += 1
        result = self._handles.get(handle, {})
        return {"exit_code": result.get("exit_code"), "stdout": bytes(result.get("stdout", b"")), "stderr": bytes(result.get("stderr", b""))}


class PosixProcessAdapter:
    """Blocked-exec handshake plus process-group identity from the operating system."""

    WRAPPER = "import json,os,sys;f=int(sys.argv[1]);a=json.loads(sys.argv[2]);t=os.read(f,1);os.close(f);sys.exit(126) if t!=b'x' else os.execvpe(a[0],a,os.environ)"

    def __init__(self) -> None:
        self._processes: dict[int, subprocess.Popen[bytes]] = {}
        self._gates: dict[int, int] = {}

    @staticmethod
    def _birth(pid: int) -> str:
        result = subprocess.run(["ps", "-o", "lstart=", "-p", str(pid)], capture_output=True, text=True)
        if result.returncode or not result.stdout.strip():
            raise PersistentReceiptError("process birth token is unavailable")
        return result.stdout.strip()

    def prepare(self, command: list[str], cwd: str, environment: Mapping[str, str]) -> Mapping[str, Any]:
        read_fd, write_fd = os.pipe()
        os.set_inheritable(read_fd, True)
        env = os.environ.copy(); env.update(environment)
        try:
            process = subprocess.Popen([sys.executable, "-c", self.WRAPPER, str(read_fd), json.dumps(command)], cwd=cwd, env=env, stdout=subprocess.PIPE, stderr=subprocess.PIPE, start_new_session=True, pass_fds=(read_fd,))
        finally:
            os.close(read_fd)
        self._processes[process.pid] = process
        self._gates[process.pid] = write_fd
        return {"pid": process.pid, "process_group_id": os.getpgid(process.pid), "birth_token": self._birth(process.pid)}

    def release(self, identity: Mapping[str, Any]) -> None:
        descriptor = self._gates.pop(int(identity["pid"]))
        os.write(descriptor, b"x"); os.close(descriptor)

    def inspect(self, identity: Mapping[str, Any]) -> bool:
        if set(identity) != {"pid", "process_group_id", "birth_token", "boot_identity"}:
            raise PersistentReceiptError("persisted process identity is incomplete")
        try:
            group = os.getpgid(int(identity["pid"]))
        except ProcessLookupError:
            return False
        except PermissionError as error:
            raise PersistentReceiptError("process identity cannot be proven") from error
        if group != identity["process_group_id"] or self._birth(int(identity["pid"])) != identity["birth_token"]:
            raise PersistentReceiptError("persisted process identity does not match OS identity")
        return True

    def observe(self, identity: Mapping[str, Any], policy: Mapping[str, Any], heartbeat: Any) -> Mapping[str, Any]:
        process = self._processes.get(int(identity["pid"]))
        if process is None:
            return {"live": self.inspect(identity)}
        deadline = time.monotonic() + int(policy["timeout_seconds"])
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                return {"timeout": True, "stdout": b"", "stderr": b""}
            try:
                stdout, stderr = process.communicate(timeout=min(int(policy["heartbeat_seconds"]), remaining))
                return {"timeout": False, "exit_code": process.returncode, "stdout": stdout, "stderr": stderr}
            except subprocess.TimeoutExpired:
                heartbeat()

    def terminate_group(self, identity: Mapping[str, Any], name: str) -> None:
        try:
            os.killpg(int(identity["process_group_id"]), getattr(signal, "SIG" + name))
        except ProcessLookupError:
            pass

    def wait_group(self, identity: Mapping[str, Any], seconds: int) -> bool:
        deadline = time.monotonic() + seconds
        while time.monotonic() <= deadline:
            process = self._processes.get(int(identity["pid"]))
            if process is not None and process.poll() is not None:
                return True
            if process is None and not self.inspect(identity):
                return True
            time.sleep(min(0.01, max(0.0, deadline - time.monotonic())))
        return False

    def reap(self, identity: Mapping[str, Any]) -> Mapping[str, Any]:
        process = self._processes.get(int(identity["pid"]))
        if process is None:
            if self.inspect(identity):
                raise PersistentReceiptError("live group cannot be reaped")
            return {"exit_code": None, "stdout": b"", "stderr": b""}
        stdout, stderr = process.communicate()
        return {"exit_code": process.returncode, "stdout": stdout, "stderr": stderr}


class PersistentReceiptRunner:
    """Persist launch identity before the command becomes runnable."""

    def __init__(self, root: Path | str, *, adapter: Any | None = None, boot_id: str | None = None) -> None:
        self.root = Path(root)
        self.adapter = adapter or PosixProcessAdapter()
        self.boot_id = boot_id or self._boot()

    @staticmethod
    def _boot() -> str:
        try:
            return Path("/proc/sys/kernel/random/boot_id").read_text().strip()
        except OSError:
            result = subprocess.run(["sysctl", "-n", "kern.boottime"], capture_output=True, text=True)
            if result.returncode or not result.stdout.strip():
                raise PersistentReceiptError("boot identity is unavailable")
            return result.stdout.strip()

    def run(self, package: Mapping[str, Any], policy: Mapping[str, Any], *, fault: str | None = None) -> dict[str, Any]:
        self._validate(package, policy)
        existing = self._existing(package, policy)
        if existing is not None:
            return self._public(existing)
        started = time.monotonic()
        record = self._initial(package, policy, started)
        self._persist_new(record)
        if fault == "before-spawn":
            return self._terminal(record, policy, "interrupted", b"", b"", None, started)
        closure = package["execution_closure"]
        try:
            identity = dict(self.adapter.prepare(list(closure["command"]["argv"]), closure["command"]["cwd"], closure["environment"]))
            identity["boot_identity"] = self.boot_id
            record.update({"state": "prepared", "process_identity": identity})
            self._persist(record)
            if fault == "after-identity-pre-release":
                return {"schema": "persistent-execution-state/v1", "state": "prepared", "execution_closure_ref": copy.deepcopy(package["execution_closure_ref"]), "process_identity": copy.deepcopy(identity)}
            self.adapter.release(identity)
            record.update({"state": "running", "released_at": _utc(), "last_heartbeat_at": _utc()})
            self._persist(record)
        except Exception as error:
            if record.get("process_identity"):
                self._stop(record["process_identity"], policy); self.adapter.reap(record["process_identity"])
            return self._terminal(record, policy, "interrupted", b"", type(error).__name__.encode(), None, started)
        if fault == "after-release":
            return self._running(record)
        if fault == "after-spawn-pre-identity":
            self._stop(identity, policy); self.adapter.reap(identity)
            return self._terminal(record, policy, "interrupted", b"", b"", None, started)

        def heartbeat() -> None:
            record["last_heartbeat_at"] = _utc(); self._persist(record)

        observed = self.adapter.observe(identity, policy, heartbeat)
        if observed.get("live"):
            return self._running(record)
        if observed.get("timeout"):
            self._stop(identity, policy)
            reaped = self.adapter.reap(identity)
            return self._terminal(record, policy, "timed_out", observed.get("stdout", b"") or reaped["stdout"], observed.get("stderr", b"") or reaped["stderr"], reaped["exit_code"], started)
        reaped = self.adapter.reap(identity)
        status = "passed" if observed.get("exit_code") == 0 else "failed"
        if fault in {"output-capture", "terminal-publication"}:
            status = "interrupted"
        return self._terminal(record, policy, status, observed.get("stdout", b"") or reaped["stdout"], observed.get("stderr", b"") or reaped["stderr"], observed.get("exit_code"), started)

    def recover(self, package: Mapping[str, Any], policy: Mapping[str, Any]) -> dict[str, Any]:
        self._validate(package, policy)
        record = self._existing(package, policy)
        if record is None:
            raise PersistentReceiptError("missing receipt is ambiguous")
        if record.get("terminal_receipt"):
            return copy.deepcopy(record["terminal_receipt"])
        identity = record.get("process_identity")
        if not isinstance(identity, Mapping) or set(identity) != {"pid", "process_group_id", "birth_token", "boot_identity"}:
            raise PersistentReceiptError("started receipt has ambiguous process identity")
        if identity["boot_identity"] != self.boot_id:
            raise PersistentReceiptError("persisted identity belongs to another boot")
        live = self.adapter.inspect(identity)
        started = float(record["timing"]["started_monotonic"])
        if record["state"] == "prepared":
            if live:
                self._stop(identity, policy); self.adapter.reap(identity)
            return self._terminal(record, policy, "interrupted", b"", b"", None, started)
        if live:
            record["last_heartbeat_at"] = _utc(); record["provenance"]["loop_class"] = "recovery"; self._persist(record)
            return self._running(record)
        return self._terminal(record, policy, "interrupted", b"", b"", None, started)

    def _validate(self, package: Mapping[str, Any], policy: Mapping[str, Any]) -> None:
        closure_schema = _schema("execution-package-closure-v2.schema.json")
        try:
            validate_document(package, _schema("command-receipt-v1.schema.json")["$defs"]["execution_package"], {"execution-package-closure-v2.schema.json": closure_schema})
            validate_document(policy, _schema("supervision-policy-v1.schema.json"))
            validate_document(package["execution_closure"], closure_schema)
        except (KeyError, SchemaValidationError) as error:
            raise PersistentReceiptError("runner input contract is invalid") from error
        closure = package["execution_closure"]
        computed = _digest({key: copy.deepcopy(item) for key, item in closure.items() if key != "closure_digest"})
        if closure["closure_digest"] != computed or package["execution_closure_ref"] != {"id": closure["package_id"], "digest": computed} or package["package_id"] != closure["package_id"]:
            raise PersistentReceiptError("execution closure identity does not match its bytes")
        for field in ("timeout_seconds", "heartbeat_seconds", "grace_seconds", "terminal_publication_seconds", "signals"):
            if policy[field] != closure["supervision"][field]:
                raise PersistentReceiptError("policy does not match frozen closure")
        if policy["heartbeat_expiry_seconds"] <= policy["heartbeat_seconds"]:
            raise PersistentReceiptError("heartbeat expiry must exceed the heartbeat interval")
        if package["retry_class"] not in RETRY_CLASSES:
            raise PersistentReceiptError("retry class is unknown")

    def _key(self, package: Mapping[str, Any]) -> str:
        return hashlib.sha256(str(package["idempotency_key"]).encode()).hexdigest()

    def _fingerprint(self, package: Mapping[str, Any], policy: Mapping[str, Any]) -> str:
        return _digest({"package": package, "policy": policy})

    def _existing(self, package: Mapping[str, Any], policy: Mapping[str, Any]) -> dict[str, Any] | None:
        path = self.root / "receipts" / (self._key(package) + ".json")
        if not path.exists():
            return None
        try:
            value = json.loads(path.read_text())
        except (OSError, json.JSONDecodeError) as error:
            raise PersistentReceiptError("receipt is corrupt") from error
        if value.get("fingerprint") != self._fingerprint(package, policy):
            raise DuplicateReceiptConflict("idempotency key has changed payload")
        if value.get("terminal_receipt") is not None:
            timing = value.get("timing", {})
            phases = timing.get("phase_seconds")
            if not isinstance(phases, Mapping) or set(phases) != {"reserve", "launch", "supervision", "publication"} or sum(phases.values()) != timing.get("elapsed_seconds"):
                raise PersistentReceiptError("terminal phase attribution is inconsistent")
            captures = value.get("captures")
            if not isinstance(captures, Mapping) or set(captures) != {"stdout", "stderr"}:
                raise PersistentReceiptError("capture inventory is corrupt")
            for stream, capture in captures.items():
                relative = capture.get("report_safe_ref")
                expected = "captures/%s/%s.bin" % (stream, str(capture.get("digest", "")).removeprefix("sha256:"))
                if relative != expected or capture.get("stream") != stream:
                    raise PersistentReceiptError("capture identity is corrupt")
                path = self.root / relative
                try:
                    retained = path.read_bytes()
                except OSError as error:
                    raise PersistentReceiptError("capture is unavailable") from error
                if len(retained) != capture.get("retained_byte_count") or "sha256:" + hashlib.sha256(retained).hexdigest() != capture.get("digest") or (path.stat().st_mode & 0o777) != 0o600:
                    raise PersistentReceiptError("capture integrity is corrupt")
            payload = {key: copy.deepcopy(item) for key, item in value.items() if key not in {"payload_digest", "terminal_receipt"}}
            receipt = value["terminal_receipt"]
            unsigned_receipt = {key: copy.deepcopy(item) for key, item in receipt.items() if key != "receipt_digest"}
            if value.get("payload_digest") != _digest(payload) or receipt.get("payload_digest") != value.get("payload_digest") or receipt.get("receipt_digest") != _digest(unsigned_receipt):
                raise PersistentReceiptError("terminal receipt identity is corrupt")
            try:
                validate_document(receipt, _schema("command-receipt-v1.schema.json"))
            except SchemaValidationError as error:
                raise PersistentReceiptError("terminal receipt contract is corrupt") from error
        return value

    def _initial(self, package: Mapping[str, Any], policy: Mapping[str, Any], started: float) -> dict[str, Any]:
        return {"schema": "persistent-execution-record/v1", "state": "reserved", "terminal_count": 0, "fingerprint": self._fingerprint(package, policy), "idempotency_key_digest": self._key(package), "package": copy.deepcopy(package), "process_identity": None, "released_at": None, "last_heartbeat_at": None, "timing": {"started_at": _utc(), "started_monotonic": started}, "provenance": {"launch_class": "new-run", "loop_class": "initial", "outcome_class": "pending", "retry_class": package["retry_class"]}}

    def _persist_new(self, value: Mapping[str, Any]) -> None:
        path = self.root / "receipts" / (str(value["idempotency_key_digest"]) + ".json")
        _private_dir(path.parent)
        descriptor = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
        with os.fdopen(descriptor, "wb") as stream:
            stream.write(_canonical(value)); stream.flush(); os.fsync(stream.fileno())

    def _persist(self, value: Mapping[str, Any]) -> None:
        _write_private(self.root / "receipts" / (str(value["idempotency_key_digest"]) + ".json"), _canonical(value))

    def _stop(self, identity: Mapping[str, Any], policy: Mapping[str, Any]) -> None:
        for name in policy["signals"]:
            self.adapter.terminate_group(identity, name)
            if self.adapter.wait_group(identity, int(policy["grace_seconds"])):
                return
        if self.adapter.inspect(identity):
            raise PersistentReceiptError("group survived bounded escalation")

    def _capture(self, stream: str, data: bytes, policy: Mapping[str, Any]) -> dict[str, Any]:
        retained = data[: int(policy["output_limit_bytes"])]
        digest = hashlib.sha256(retained).hexdigest()
        relative = "captures/%s/%s.bin" % (stream, digest)
        _write_private(self.root / relative, retained)
        return {"stream": stream, "original_byte_count": len(data), "retained_byte_count": len(retained), "integrity": "truncated" if len(data) > len(retained) else "complete", "digest": "sha256:" + digest, "sensitivity": policy["sensitivity"], "report_safe_ref": relative}

    def _terminal(self, record: Mapping[str, Any], policy: Mapping[str, Any], status: str, stdout: bytes, stderr: bytes, exit_code: int | None, started: float) -> dict[str, Any]:
        if record.get("terminal_receipt"):
            return copy.deepcopy(record["terminal_receipt"])
        value = copy.deepcopy(record)
        ended = time.monotonic(); elapsed = max(0.0, ended - started)
        captures = {"stdout": self._capture("stdout", stdout, policy), "stderr": self._capture("stderr", stderr, policy)}
        value.update({"state": "terminal", "terminal_count": 1, "exit_code": exit_code, "captures": captures})
        value["provenance"]["outcome_class"] = status
        value["timing"].update({"ended_at": _utc(), "ended_monotonic": ended, "elapsed_seconds": elapsed, "phase_seconds": {"reserve": 0.0, "launch": 0.0, "supervision": elapsed, "publication": 0.0}})
        payload_digest = _digest({key: copy.deepcopy(item) for key, item in value.items() if key not in {"payload_digest", "terminal_receipt"}})
        package = value["package"]; closure = package["execution_closure"]
        receipt = {"schema": "command-receipt/v1", "receipt_id": package["package_id"], "shard_id": package["shard_id"], "candidate_digest": closure["candidate_ref"]["digest"], "execution_closure_digest": closure["closure_digest"], "idempotency_key": package["idempotency_key"], "payload_digest": payload_digest, "status": status, "coverage": sorted(package["coverage"]), "capture_state": {"stdout": captures["stdout"]["integrity"], "stderr": captures["stderr"]["integrity"]}, "terminal": True}
        receipt["receipt_digest"] = _digest(receipt)
        value.update({"payload_digest": payload_digest, "terminal_receipt": receipt})
        self._persist(value)
        return copy.deepcopy(receipt)

    @staticmethod
    def _running(value: Mapping[str, Any]) -> dict[str, Any]:
        return {"schema": "persistent-execution-state/v1", "state": "running", "execution_closure_ref": copy.deepcopy(value["package"]["execution_closure_ref"]), "process_identity": copy.deepcopy(value["process_identity"]), "last_heartbeat_at": value["last_heartbeat_at"]}

    def _public(self, value: Mapping[str, Any]) -> dict[str, Any]:
        if value.get("terminal_receipt"):
            return copy.deepcopy(value["terminal_receipt"])
        identity = value.get("process_identity")
        if not isinstance(identity, Mapping) or identity.get("boot_identity") != self.boot_id or not self.adapter.inspect(identity):
            raise PersistentReceiptError("nonterminal receipt identity is ambiguous")
        return self._running(value)


__all__ = ["DeterministicProcessAdapter", "DuplicateReceiptConflict", "PersistentReceiptError", "PersistentReceiptRunner", "PosixProcessAdapter"]
