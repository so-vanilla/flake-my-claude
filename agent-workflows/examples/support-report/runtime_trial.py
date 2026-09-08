"""Deterministic adapter trial, not a claim of fresh LLM Skill execution.

All B/C/D compiler and Kernel operations are real. Only human responses are
mock. The parent supplies Group audits and actual execution/review separately.
"""
from datetime import datetime, timezone
import json
from pathlib import Path
import sys
import time

SOURCE = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(SOURCE / "agent-workflows/src"))
from ai_agent_workflow.inception_cli import create_file, encoded
from ai_agent_workflow.inception_runtime import InceptionRuntime, file_ref
from ai_agent_workflow.runtime_approval import approval_context, adopt_approved_objective


def put(project, name, value, version="v1"):
    path = project / ".local/agent/support-report-trial" / name
    path.parent.mkdir(parents=True, exist_ok=True)
    if path.exists():
        if path.read_bytes() != encoded(value):
            raise ValueError("immutable trial input differs: " + str(path))
    else:
        create_file(path, encoded(value))
    return {**file_ref(path), "version": version}


def initialize(project):
    project = Path(project).resolve()
    project.mkdir(parents=True, exist_ok=True)
    request = (Path(__file__).parent / "REQUEST.md").read_text()
    args = {"intake_ref": put(project, "intake.json", {"raw_request": request}, "intake"),
            "candidate_ref": put(project, "objective.json", {"objective": "毎朝のCSVから期限超過問い合わせを漏れなく安全に優先順で確認できる", "acceptance": request}, "v001"),
            "proposal_ref": put(project, "purpose-proposal.json", {"options": ["期限超過レポートCLI", "外部SaaS連携"], "recommendation": "期限超過レポートCLI", "reason": "有限時間・標準ライブラリ・外部変更禁止の制約に適合"}),
            "actor_ref": put(project, "actor.json", {"actor_id": "mock-business-owner", "source": "mock"}), "mode": "rehearsal"}
    receipt = {**approval_context(project, "support-report", **args), "receipt_id": "mock-objective-response",
               "decision": "approve", "explicit": True, "issued_at": datetime.now(timezone.utc).isoformat()}
    args["receipt_ref"] = put(project, "approval.json", receipt)
    semantics = [
        {"raw_request": request, "interpretation": "問い合わせ優先順位のCLI", "assumptions": ["CSVローカル入力"], "unknowns": []},
        {"facts": [{"claim": "外部サービス禁止・標準ライブラリのみ", "source_ref": {"path": str(Path(args["intake_ref"]["path"]).relative_to(project)), "digest": args["intake_ref"]["digest"], "selector": "raw-request"}}]},
        {"depth": "project", "operation": "implementation", "owner": "mock-business-owner", "reversibility": "isolated-source"},
        {"material_unknowns": [], "inquiry_complete": True, "resolution_reason": "入力・境界・エラー・日時精度は明記済み。目的選択のみmock回答で固定"},
        {"options": [{"purpose": "ローカルCLI", "tradeoff": "短時間・手元データのみ"}, {"purpose": "SaaS通知", "tradeoff": "外部認証と稼働監視が必要"}]},
        {"owner": "mock-business-owner", "constraints": ["Python標準ライブラリ", "networkなし", "1200秒以内の試験"], "feasible": True},
    ]
    steps = []
    for index, values in enumerate(semantics, 1):
        ref = put(project, "B%d-input.json" % index, values)
        physical = {"path": str(Path(ref["path"]).relative_to(project)), "digest": ref["digest"], "selector": "B%d-input" % index}
        values = {**values, "input_refs": [physical], "candidate_ref": physical, "version": "v1"}
        if index == 4:
            values["resolution_refs"] = [physical]
        steps.append(("group.B.B%d" % index, values))
    args["preapproval_steps"] = steps
    args["budget_seconds"] = 1200
    put(project, "adoption-inputs.json", args)
    started = time.monotonic()
    kernel = adopt_approved_objective(project, "support-report", **args)
    print(json.dumps({"phase": "B1-B7-actual", "seconds": time.monotonic()-started, "head": kernel.head(), "project": str(project)}, ensure_ascii=False))
    return args


def plan_group(project, group):
    from runtime_plan import prepare
    project = Path(project).resolve()
    args = json.loads((project / ".local/agent/support-report-trial/adoption-inputs.json").read_text())
    steps = prepare(project, SOURCE, objective_ref=args["candidate_ref"], owner_ref=args["actor_ref"], receipt_ref=args["receipt_ref"])
    for qid, inputs in steps:
        if qid.split(".")[1] != group:
            continue
        runtime = InceptionRuntime(project, "support-report")
        if runtime.status()["next_id"] != qid:
            if qid.split(".")[-1] in runtime.records():
                continue
            raise ValueError("trial frontier differs from planned step")
        number = int(qid.split(".")[-1][1:])
        if qid.startswith("group.D.") and number > 1:
            prior = runtime.records()["D%d" % (number - 1)]
            inputs["input_refs"].append(runtime._record_file_ref(prior))
        if qid == "group.D.D12":
            inputs["evidence_refs"] = [runtime._record_file_ref(runtime.records()["D%d" % n]) for n in range(1, 12)]
        for task in inputs.get("tasks", []) + inputs.get("briefs", []):
            task["stop"] = {"source_ref": file_ref(project / ".local/agent/support-report-plan/D8-inputs.json")}
        if qid == "group.D.D6":
            receipt = {"source": "mock", "decision": "approve", "explicit": True, "scope": "option-selection",
                       "actor_id": "mock-business-owner", "objective_digest": args["candidate_ref"]["digest"],
                       "options_digest": runtime.records()["D5"]["ref"]["digest"], "selected_option": "validated-records"}
            ref = put(project, "option-approval.json", receipt)
            inputs["approved_option_receipt"] = {"receipt_ref": ref, "source": "mock", "decision": "approve", "explicit": True,
                                                  "scope": "option-selection", "actor_ref": args["actor_ref"]}
            inputs["design"]["selected_option"] = "validated-records"
            # The durable Kernel stores a physical policy reference, not
            # opaque security prose; the original design remains available.
            inputs["design"]["security"] = {"source_ref": file_ref(project / ".local/agent/support-report-plan/D6-inputs.json")}
        started = time.monotonic()
        status = runtime.step(qid, inputs, actor_ref=args["actor_ref"])
        print(json.dumps({"phase": qid, "seconds": time.monotonic()-started, "head": status["head"], "next": status["next_id"]}))


def audited_close(project, rationale):
    """Record the caller's review, never infer semantic acceptance from hashes."""
    runtime = InceptionRuntime(project, "support-report")
    group = runtime.state["group"]["id"]
    from ai_agent_workflow.inception_runtime import ROUTES
    records = runtime.records(group)
    audit = {"group_id": group, "objective_digest": runtime.state["objective_ref"]["digest"],
             "alignment": "aligned", "artifact_refs": [records[key]["ref"] for key in ROUTES[group]],
             "reviewer": "parent-agent-source-review", "rationale": rationale}
    ref = put(Path(project).resolve(), group + "-audit.json", audit)
    started = time.monotonic()
    print(json.dumps({"phase": group + "-closure", "status": runtime.close(ref), "seconds": time.monotonic()-started}, ensure_ascii=False))


if __name__ == "__main__":
    if sys.argv[1] == "init":
        initialize(sys.argv[2])
    elif sys.argv[1] == "close":
        audited_close(sys.argv[2], sys.argv[3])
    else:
        plan_group(sys.argv[2], sys.argv[1])
