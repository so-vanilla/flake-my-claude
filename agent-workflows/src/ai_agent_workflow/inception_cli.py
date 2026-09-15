"""Pre-Run intake and one-Skill handoff storage; never a Kernel authority.

This CLI records work, not semantic acceptance. It neither executes Skills nor
creates a Run. Rehearsal answers remain labelled mock across every handoff.
"""
from __future__ import annotations

import argparse
from contextlib import contextmanager
from datetime import datetime, timezone
import fcntl
import hashlib
import hmac
import json
import os
from pathlib import Path
import re
import shlex
import subprocess
import sys
import tempfile


SKILLS = (
    "entry", "discover-context", "classify-scope", "grill-purpose",
    "propose-true-purpose", "assess-feasibility-and-constraints", "approve-objective",
    "decompose-outcomes", "build-outcome-dependency-graph", "design-measurement",
    "define-targets", "capture-baseline", "validate-outcome-system",
    "select-workflow-profile", "reverse-engineer-current-system", "discover-practices",
    "specify-what-and-why", "explore-options", "design-solution", "design-contracts",
    "decompose-tasks", "sequence-and-parallelize", "prepare-worker-briefs",
    "plan-verification-and-recovery", "implementation-readiness-review",
)


class InceptionError(ValueError):
    pass


def now():
    return datetime.now(timezone.utc).isoformat()


def stamp(value):
    if not isinstance(value, str):
        raise InceptionError("timestamp must be a string")
    normalized = value[:-1] + "+00:00" if value.endswith("Z") else value
    # macOS/BSD `date ... +%z` commonly emits +0000.  Python 3.9 does not
    # accept that spelling even though it denotes the same timezone offset.
    if re.search(r"[+-][0-9]{4}$", normalized):
        normalized = normalized[:-2] + ":" + normalized[-2:]
    try:
        parsed = datetime.fromisoformat(normalized)
    except ValueError as error:
        raise InceptionError("invalid timestamp") from error
    if parsed.tzinfo is None or parsed.utcoffset() is None:
        raise InceptionError("timestamp requires a timezone")
    return parsed


def digest(data):
    return "sha256:" + hashlib.sha256(data).hexdigest()


def encoded(value):
    return (json.dumps(value, ensure_ascii=False, sort_keys=True, indent=2) + "\n").encode()


def regular(path):
    path = Path(os.path.abspath(path))
    if any(part.is_symlink() for part in (path, *path.parents)):
        raise InceptionError("symlink paths are not supported: %s" % path)
    if not path.is_file():
        raise InceptionError("missing regular file: %s" % path)
    return path


def reference(path):
    path = regular(path)
    return {"path": str(path), "version": "sha256-bytes/v1", "digest": digest(path.read_bytes())}


def checked(ref):
    if not isinstance(ref, dict) or set(ref) != {"path", "version", "digest"}:
        raise InceptionError("invalid physical reference")
    if ref["version"] != "sha256-bytes/v1" or not Path(ref["path"]).is_absolute():
        raise InceptionError("invalid physical reference version/path")
    path = regular(ref["path"])
    if digest(path.read_bytes()) != ref["digest"]:
        raise InceptionError("stale input: %s" % path)
    return path


def read_json(path):
    def unique(pairs):
        result = {}
        for key, value in pairs:
            if key in result:
                raise InceptionError("duplicate JSON key: %s" % key)
            result[key] = value
        return result
    return json.loads(regular(path).read_text(), object_pairs_hook=unique)


def runtime_snapshot(root):
    """Return the exact project-local runtime manifest selected by its wrapper."""
    supplied = os.environ.get("AGENT_WORKFLOW_RUNTIME_MANIFEST")
    if not supplied:
        return None
    path = regular(Path(supplied).resolve())
    expected = Path(root) / ".agent-workflow" / "SNAPSHOT-MANIFEST.json"
    if path != expected:
        raise InceptionError("runtime manifest must be the project-local snapshot")
    value = read_json(path)
    if (value.get("schema") != "agent-workflow-project-snapshot/v2"
            or not re.fullmatch(r"sha256:[0-9a-f]{64}", str(value.get("snapshot_digest")))):
        raise InceptionError("invalid project runtime snapshot manifest")
    if value.get("wrapper") != ".agent-workflow/bin/agent-workflow-inception":
        raise InceptionError("invalid project runtime wrapper binding")
    wrapper = regular(Path(root) / value["wrapper"])
    return {"manifest": reference(path), "snapshot_digest": value["snapshot_digest"],
            "wrapper": str(wrapper)}


def create_file(path, data):
    """Atomic create, never replace an existing checkpoint (even on retries)."""
    path = Path(path)
    fd, temporary = tempfile.mkstemp(prefix=".pending-", dir=str(path.parent))
    try:
        with os.fdopen(fd, "wb") as stream:
            stream.write(data)
            stream.flush()
            os.fsync(stream.fileno())
        os.link(temporary, path)
    finally:
        os.unlink(temporary)


def helper_proof(directory, record):
    """Bind a handoff to this helper-owned intake without exposing the key."""
    key_path = regular(Path(directory) / ".helper-key")
    key = key_path.read_bytes()
    if len(key) != 32:
        raise InceptionError("invalid helper key")
    unsigned = dict(record)
    unsigned.pop("helper_proof", None)
    return "hmac-sha256:" + hmac.new(key, encoded(unsigned), hashlib.sha256).hexdigest()


def safe_project(project):
    # Resolve a user-supplied /tmp alias once. Stored paths use /private/tmp on macOS.
    root = Path(project).resolve(strict=True)
    if not root.is_dir():
        raise InceptionError("project must be a directory")
    return root


def ignored(root, path):
    result = subprocess.run(["git", "-C", str(root), "check-ignore", "--no-index", "-q", str(path)],
                            capture_output=True)
    tracked = subprocess.run(["git", "-C", str(root), "ls-files", "--", str(path)], capture_output=True)
    if result.returncode != 0 or tracked.returncode != 0 or tracked.stdout:
        raise InceptionError("intake storage must be Git-ignored and untracked; configure .local/ explicitly")


def workdir(root, work_id):
    if not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9_-]{0,79}", work_id):
        raise InceptionError("invalid work id")
    target = root / ".local" / "agent" / "inception" / work_id
    if any(path.is_symlink() for path in (target, *target.parents)):
        raise InceptionError("symlink storage is not supported")
    ignored(root, target / "intake.json")
    return target


@contextmanager
def locked(directory):
    # Same-work single writer. No lock survives as ownership after process exit.
    lock = directory / ".lock"
    if lock.is_symlink():
        raise InceptionError("symlink lock is not supported")
    with open(lock, "a") as stream:
        fcntl.flock(stream.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
        yield


def init(project, work_id, request, mode="real", budget_seconds=None):
    root = safe_project(project)
    snapshot = runtime_snapshot(root)
    directory = workdir(root, work_id)
    raw = regular(Path(request).resolve()).read_bytes()
    if not raw.strip() or mode not in ("real", "rehearsal"):
        raise InceptionError("nonempty request and supported mode required")
    if budget_seconds is not None and (
            type(budget_seconds) is not int or budget_seconds <= 0):
        raise InceptionError("legacy budget must be a positive integer")
    directory.mkdir(parents=True, exist_ok=True)
    with locked(directory):
        intake_path = directory / "intake.json"
        if intake_path.exists():
            raise InceptionError("intake exists; resume it instead of creating a new frontier")
        if (directory / "request.txt").exists() or (directory / ".helper-key").exists():
            raise InceptionError("partial init; inspect retained request/key and choose a new work id")
        create_file(directory / "request.txt", raw)
        create_file(directory / ".helper-key", os.urandom(32))
        record = {"schema": "inception-intake/v2", "project": str(root), "work_id": work_id,
                  "mode": mode, "started_at": now(),
                  "loop_control": {"contract_version": "workflow-loop/v1",
                                   "progress_control": "iteration-and-evidence",
                                   "operation_timeout": "caller-owned",
                                   "metrics": "enabled"},
                  "request": reference(directory / "request.txt"), "objective": None,
                  "run": None, "authority": "candidate-recording-only", "route": list(SKILLS),
                  "runtime_snapshot": snapshot}
        # A v1 intake remains available only for explicit legacy recovery and
        # compatibility tests.  The ordinary CLI/API path creates v2 and has
        # no wall-clock progress budget.
        if budget_seconds is not None:
            record["schema"] = "inception-intake/v1"
            record.pop("loop_control")
            record["budget_seconds"] = budget_seconds
        create_file(intake_path, encoded(record))
        return {"intake": reference(intake_path), "next_skill": "entry", "mode": mode,
                "kernel_authority": False}


def load_intake(path):
    path = regular(Path(path).resolve())
    intake = read_json(path)
    if intake.get("schema") not in {"inception-intake/v1", "inception-intake/v2"} or intake.get("route") != list(SKILLS):
        raise InceptionError("unsupported intake schema/route")
    if intake.get("mode") not in ("real", "rehearsal") or intake.get("authority") != "candidate-recording-only":
        raise InceptionError("invalid intake mode/authority")
    if intake["schema"] == "inception-intake/v1":
        if type(intake.get("budget_seconds")) is not int or intake["budget_seconds"] <= 0:
            raise InceptionError("invalid legacy intake budget")
    elif intake.get("loop_control") != {
        "contract_version": "workflow-loop/v1",
        "progress_control": "iteration-and-evidence",
        "operation_timeout": "caller-owned",
        "metrics": "enabled",
    } or "budget_seconds" in intake:
        raise InceptionError("invalid loop-control intake")
    root = safe_project(intake["project"])
    if workdir(root, intake["work_id"]) != path.parent or path.name != "intake.json":
        raise InceptionError("intake location mismatch")
    key = regular(path.parent / ".helper-key")
    if len(key.read_bytes()) != 32:
        raise InceptionError("invalid helper key")
    checked(intake["request"])
    bound_snapshot = intake.get("runtime_snapshot")
    if bound_snapshot:
        if (not isinstance(bound_snapshot, dict)
                or set(bound_snapshot) != {"manifest", "snapshot_digest", "wrapper"}):
            raise InceptionError("invalid intake runtime snapshot binding")
        manifest = read_json(checked(bound_snapshot["manifest"]))
        if (manifest.get("snapshot_digest") != bound_snapshot["snapshot_digest"]
                or str(root / manifest.get("wrapper", "")) != bound_snapshot["wrapper"]):
            raise InceptionError("stale intake runtime snapshot binding")
    if runtime_snapshot(root) != bound_snapshot:
        raise InceptionError("use the same project-local runtime snapshot wrapper")
    return path, intake


def next_step(skill, status, mode):
    index = SKILLS.index(skill)
    if status != "recorded":
        return skill, None
    if skill == SKILLS[-1] or (skill == "approve-objective" and mode == "real"):
        return None, "requires_kernel_approval_and_group_closure"
    return SKILLS[index + 1], None


def resume(handoff, require_real=False):
    handoff = regular(Path(handoff).resolve())
    record = read_json(handoff)
    if record.get("schema") != "inception-handoff/v1":
        raise InceptionError("unsupported handoff schema")
    path, intake = load_intake(checked(record["intake"]))
    if handoff.parent != path.parent or record["mode"] != intake["mode"]:
        raise InceptionError("handoff location/mode mismatch")
    if record.get("skill") not in SKILLS or record.get("status") not in ("recorded", "blocked", "needs-input"):
        raise InceptionError("invalid handoff skill/status")
    sequence = record.get("sequence")
    if type(sequence) is not int or sequence < 1 or handoff.name != "handoff-%04d.json" % sequence:
        raise InceptionError("invalid handoff sequence/path")
    invariants = {"kernel_authority": False, "execution_authorized": False,
                  "compiler_status": "not-run", "group_acceptance": "not-performed",
                  "record_scope": "pre-run-draft"}
    if any(record.get(key) != value for key, value in invariants.items()):
        raise InceptionError("handoff cannot claim compiler/group acceptance or execution authority")
    expected, reason = next_step(record["skill"], record["status"], record["mode"])
    if record.get("next_skill") != expected or record.get("stop_reason") != reason:
        raise InceptionError("handoff route mismatch")
    valid_approvals = {"absent", "mock-only"} if intake["mode"] == "rehearsal" else {"absent", "human-receipt-recorded"}
    if record.get("approval") not in valid_approvals:
        raise InceptionError("handoff approval/mode mismatch")
    if require_real and intake["mode"] != "real":
        raise InceptionError("mock/rehearsal evidence cannot be used as real authority")
    for ref in record["inputs"] + [record["output"]]:
        checked(ref)
    if record.get("receipt"):
        checked(record["receipt"])
    if record.get("objective"):
        checked(record["objective"])
    if record.get("previous"):
        predecessor = read_json(checked(record["previous"]))
        if (predecessor.get("intake") != record["intake"]
                or predecessor.get("sequence") != sequence - 1
                or predecessor.get("next_skill") != record["skill"]):
            raise InceptionError("handoff predecessor/frontier mismatch")
        inherited = predecessor.get("inputs", []) + [predecessor["output"]]
        if predecessor.get("receipt"):
            inherited.append(predecessor["receipt"])
        if any(ref not in record["inputs"] for ref in inherited):
            raise InceptionError("handoff dropped inherited evidence")
        if record["skill"] != "approve-objective" and (
                record.get("objective") != predecessor.get("objective")
                or record.get("approval") != predecessor.get("approval")):
            raise InceptionError("handoff changed inherited objective/approval")
    elif sequence != 1 or record["skill"] != "entry" or record.get("approval") != "absent" or record.get("objective") is not None:
        raise InceptionError("initial handoff must be an unapproved entry draft")
    proof = record.get("helper_proof")
    if not isinstance(proof, str) or not hmac.compare_digest(proof, helper_proof(path.parent, record)):
        raise InceptionError("handoff lacks a valid helper-generated proof")
    if intake["schema"] == "inception-intake/v1":
        elapsed = (stamp(now()) - stamp(intake["started_at"])).total_seconds()
        record["remaining_seconds"] = max(0, intake["budget_seconds"] - elapsed)
        record["budget_exhausted"] = elapsed >= intake["budget_seconds"]
        record["progress_control"] = "legacy-wall-clock-budget"
    else:
        record["remaining_seconds"] = None
        record["budget_exhausted"] = False
        record["progress_control"] = intake["loop_control"]["progress_control"]
    latest = sorted(path.parent.glob("handoff-*.json"))[-1]
    record["stale_frontier"] = handoff != latest
    snapshot = intake.get("runtime_snapshot")
    wrapper = snapshot.get("wrapper") if isinstance(snapshot, dict) else None
    source_entry = ("PYTHONPATH=%s %s -B -m ai_agent_workflow.inception_cli" %
                    (shlex.quote(str(Path(__file__).resolve().parents[1])), shlex.quote(sys.executable)))
    command = shlex.quote(wrapper) if isinstance(wrapper, str) and Path(wrapper).is_absolute() else source_entry
    record["invocation"] = ("Use this exact first run resume command: %s resume --handoff %s. "
                            "Only if it returns frontier_status=helper-verified, use $%s and "
                            "execute only that Skill." % (command, shlex.quote(str(handoff)),
                                                           record["next_skill"])) if (record["next_skill"]
                            and not record["budget_exhausted"] and not record["stale_frontier"]) else None
    record["kernel_authority"] = False
    record["frontier_status"] = "helper-verified"
    return record


def save(intake_path, skill, output, status, previous=None, inputs=(), receipt=None,
         started_at=None, checks=(), unresolved=()):
    path, intake = load_intake(intake_path)
    if skill not in SKILLS or status not in ("recorded", "needs-input", "blocked"):
        raise InceptionError("unknown skill/status")
    end = now()
    start = started_at or end
    if not stamp(intake["started_at"]) <= stamp(start) <= stamp(end):
        raise InceptionError("step timestamp outside intake lifetime")
    out_ref = reference(Path(output).resolve())
    if not regular(out_ref["path"]).read_bytes().strip():
        raise InceptionError("empty output cannot be saved")
    with locked(path.parent):
        existing = sorted(path.parent.glob("handoff-*.json"))
        prior = resume(previous) if previous else None
        if existing and (not previous or Path(previous).resolve() != existing[-1]):
            raise InceptionError("stale frontier; use the latest handoff")
        if not existing and previous:
            raise InceptionError("foreign previous handoff")
        if prior and prior["intake"] != reference(path):
            raise InceptionError("previous belongs to another intake")
        expected = prior["next_skill"] if prior else "entry"
        if skill != expected:
            raise InceptionError("expected one selected Skill: %s" % expected)
        elapsed = (stamp(end) - stamp(intake["started_at"])).total_seconds()
        if (intake["schema"] == "inception-intake/v1"
                and elapsed >= intake["budget_seconds"] and status == "recorded"):
            raise InceptionError("budget exhausted; save blocked, never reset through clear")
        input_refs = list(prior["inputs"]) + [prior["output"]] if prior else []
        if prior and prior.get("receipt") and prior["receipt"] not in input_refs:
            input_refs.append(prior["receipt"])
        for item in inputs:
            ref = reference(Path(item).resolve())
            if ref not in input_refs:
                input_refs.append(ref)
        objective = prior.get("objective") if prior else None
        approval = prior.get("approval", "absent") if prior else "absent"
        receipt_ref = reference(Path(receipt).resolve()) if receipt else None
        if skill == "entry" and status == "recorded":
            payload = read_json(out_ref["path"])
            if set(payload) != {"raw_request_ref", "interpretation", "assumptions", "unknowns"}:
                raise InceptionError("entry must separate raw request, interpretation, assumptions and unknowns")
            if payload["raw_request_ref"] != intake["request"] or not isinstance(payload["interpretation"], str):
                raise InceptionError("entry request binding/interpretation mismatch")
            if not all(isinstance(payload[key], list) for key in ("assumptions", "unknowns")):
                raise InceptionError("entry assumptions/unknowns must be lists")
        if receipt_ref:
            answer = read_json(receipt_ref["path"])
            expected_source = "mock" if intake["mode"] == "rehearsal" else "human"
            if answer.get("source") != expected_source or answer.get("work_id") != intake["work_id"]:
                raise InceptionError("receipt source/work binding mismatch")
            checked(answer["subject"])
            if answer["subject"] != out_ref:
                raise InceptionError("receipt must bind this exact output")
        if skill == "approve-objective" and status == "recorded":
            if not receipt_ref or answer.get("decision") != "approve" or answer.get("explicit") is not True:
                raise InceptionError("explicit objective receipt required")
            if not answer.get("actor") or not answer.get("recorded_at"):
                raise InceptionError("receipt actor/time required")
            stamp(answer["recorded_at"])
            objective = out_ref
            approval = "mock-only" if intake["mode"] == "rehearsal" else "human-receipt-recorded"
        next_skill, stop_reason = next_step(skill, status, intake["mode"])
        record = {"schema": "inception-handoff/v1", "intake": reference(path),
                  "sequence": len(existing) + 1, "mode": intake["mode"], "skill": skill,
                  "status": status, "output": out_ref, "inputs": input_refs,
                  "previous": reference(Path(previous).resolve()) if previous else None,
                  "objective": objective, "approval": approval, "receipt": receipt_ref,
                  "checks": list(checks), "unresolved": list(unresolved), "started_at": start,
                  "ended_at": end, "step_seconds": (stamp(end) - stamp(start)).total_seconds(),
                  "next_skill": next_skill, "kernel_authority": False,
                  "record_scope": "pre-run-draft", "compiler_status": "not-run",
                  "stop_reason": stop_reason,
                  "group_acceptance": "not-performed", "execution_authorized": False}
        record["helper_proof"] = helper_proof(path.parent, record)
        target = path.parent / ("handoff-%04d.json" % record["sequence"])
        create_file(target, encoded(record))
        # Persistence verification is mandatory before recommending clear.
        result = resume(target)
        result["handoff"] = reference(target)
        return result


def main(argv=None):
    argv = list(sys.argv[1:] if argv is None else argv)
    if argv and argv[0] == "runtime":
        from .inception_runtime import main as runtime_main
        return runtime_main(argv[1:])
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    start = commands.add_parser("init")
    start.add_argument("--project", required=True)
    start.add_argument("--work-id", required=True)
    start.add_argument("--request", required=True)
    start.add_argument("--mode", choices=("real", "rehearsal"), default="real")
    start.add_argument("--legacy-budget-seconds", type=int)
    write = commands.add_parser("save")
    write.add_argument("--intake", required=True)
    write.add_argument("--skill", choices=SKILLS, required=True)
    write.add_argument("--output", required=True)
    write.add_argument("--status", choices=("recorded", "needs-input", "blocked"), required=True)
    write.add_argument("--previous")
    write.add_argument("--input", action="append", default=[])
    write.add_argument("--receipt")
    write.add_argument("--started-at", required=True)
    write.add_argument("--check", action="append", default=[])
    write.add_argument("--unresolved", action="append", default=[])
    read = commands.add_parser("resume")
    read.add_argument("--handoff", required=True)
    read.add_argument("--require-real", action="store_true")
    args = parser.parse_args(argv)
    try:
        if args.command == "init":
            result = init(args.project, args.work_id, args.request, args.mode, args.legacy_budget_seconds)
        elif args.command == "resume":
            result = resume(args.handoff, args.require_real)
        else:
            result = save(args.intake, args.skill, args.output, args.status, args.previous,
                          args.input, args.receipt, args.started_at, args.check, args.unresolved)
        print(json.dumps(result, ensure_ascii=False, sort_keys=True, indent=2))
        return 0
    except (InceptionError, OSError, ValueError, KeyError, TypeError) as error:
        print("error: %s" % error, file=sys.stderr)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
