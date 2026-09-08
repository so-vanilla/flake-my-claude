"""Replay physical draft handoffs in fresh processes, NOT 25 AI Skill executions.

Creates a new disposable project. Never copies an implementation/expected answer.
The user must separately authorize a sample worker to implement the final brief.
"""
import argparse
from datetime import datetime, timezone
import json
import os
from pathlib import Path
import subprocess
import sys
import time

SOURCE = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(SOURCE / "src"))
from ai_agent_workflow.inception_cli import SKILLS, reference


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--project", required=True)
    args = parser.parse_args()
    project = Path(args.project).resolve()
    project.mkdir()  # Refuse an existing directory; never alter an existing repo.
    (project / ".gitignore").write_text(".local/\n__pycache__/\n")
    subprocess.run(["git", "init", "-q", str(project)], check=True, timeout=10)
    request = project / "REQUEST.md"
    request.write_bytes((Path(__file__).parent / "REQUEST.md").read_bytes())
    drafts = project / "drafts"
    drafts.mkdir()
    env = {**os.environ, "PYTHONPATH": str(SOURCE / "src"), "PYTHONDONTWRITEBYTECODE": "1"}
    timings = []

    def call(*argv):
        start = time.monotonic()
        timestamp = datetime.now(timezone.utc).isoformat()
        result = subprocess.run([sys.executable, "-B", "-m", "ai_agent_workflow.inception_cli", *argv],
                                env=env, capture_output=True, text=True, timeout=15)
        timings.append({"command": list(argv), "started_at": timestamp,
                        "ended_at": datetime.now(timezone.utc).isoformat(),
                        "seconds": time.monotonic() - start, "exit_code": result.returncode})
        if result.returncode:
            raise RuntimeError(result.stderr)
        return json.loads(result.stdout)

    def write(name, value):
        path = drafts / name
        path.write_text(json.dumps(value, ensure_ascii=False, indent=2) + "\n")
        return path

    initialized = call("init", "--project", str(project), "--work-id", "support-report",
                       "--request", str(request), "--mode", "rehearsal", "--budget-seconds", "1800")
    intake_path = initialized["intake"]["path"]
    intake = json.loads(Path(intake_path).read_text())
    source = reference(request)
    bodies = [
        {"raw_request_ref": intake["request"], "interpretation": "期限超過の見落としを減らすローカルCSV CLI",
         "assumptions": [], "unknowns": ["期限と同時刻を含めるかの確認"]},
        {"facts": [{"fact": "新規CLI、標準ライブラリのみ、closedも検証", "source": source}],
         "inferences": [], "assumptions": [], "unknowns": []},
        {"depth": "bounded", "operation": "change", "ownership": "isolated-sample",
         "reversibility": "disposable", "excluded": ["network", "deploy", "git commit"]},
        {"question": "期限と同時刻は超過に含めますか？", "answer": "含めない。要求通り厳密に前だけ。",
         "source": "mock", "material_unknowns": [], "resolution_reason": "明記済み仕様をモックで確認"},
        {"options": [{"id": "report", "tradeoff": "ローカルの朝次一覧、低い運用負担"},
                     {"id": "notify", "tradeoff": "自動通知だが外部接続と運用権限が必要"}],
         "recommendation": "report", "selection": "not-approved"},
        {"hard_constraints": ["no network", "stdlib", "no partial stdout"],
         "soft_constraints": ["小さなCLI"], "assumptions": [], "open_questions": []},
        {"purpose": "サポート担当が期限超過を優先順で確認できる", "selected_option": "report",
         "observable_change": "決めた時刻に同じCSVから同じ正しい一覧を得る", "non_goals": ["自動通知", "配布"],
         "constraints_ref": source, "reopen_when": "抽出基準や外部接続が変わる場合"},
        {"outcomes": [{"id": "accuracy", "state": "対象と順序が要求通り"},
                      {"id": "safety", "state": "不正入力から部分レポートを出さない"},
                      {"id": "usability", "state": "README例で実行できる"}]},
        {"nodes": ["accuracy", "safety", "usability"], "edges": [["accuracy", "usability"], ["safety", "usability"]],
         "convergence_owner": "sample-parent"},
        {"method": "別実装の受入テストとfresh review", "sources": [source],
         "guard": "件数だけでなく対象id・並び順・stdout/stderr・exit codeを確認"},
        {"target": "全受入ケース合格", "deadline": "implementation 8min + review 4min",
         "guard_metrics": ["no network", "no partial output", "no dependency"]},
        {"baseline": "unavailable", "reason": "新規案件に既存実装はない。0件成功と捏造しない"},
        {"trace": {"accuracy": "filter/order acceptance", "safety": "invalid input acceptance",
                   "usability": "README replay"}, "open_questions": []},
        {"profile": "feature-bounded", "lifecycle_owner": "flake-my-claude.agent-workflows",
         "mode": "rehearsal-drafts-only"},
        {"entrypoints": [], "domain": "support ticket", "data_flow": "CSV -> validate all -> filter -> sort -> render",
         "external_contract": source, "existing_tests": [], "adjacent": "CLI parser and file error behavior"},
        {"constraints": ["Python stdlib", "unittest", "no network or external writes"], "sources": [source]},
        {"canonical_spec": source, "behavior": "REQUEST.md全条件", "errors": "exit2/stderr/no stdout",
         "accepted_precision": "seconds or 1-6 fraction digits", "csv_dialect": "strict doubled quotes"},
        {"options": ["single module", "package", "manual processing/status quo"],
         "recommendation": "single module", "reason": "小規模で配布依存なし", "source": "mock-selection"},
        {"architecture": "single sla_report module", "responsibilities": ["parse", "validate", "filter/sort", "render"],
         "error_boundary": "validate complete input before stdout", "approval_source": "mock"},
        {"cli": "python3 -m sla_report INPUT --as-of ISO [--format json|markdown]",
         "json": {"count": "integer", "tickets": "original five columns"}, "source": source},
        {"tasks": [{"id": "implement", "write_scope": ["sla_report.py", "tests/", "README.md", "tickets.csv"],
                    "acceptance": "REQUEST.md", "forbidden": ["drafts/", ".local/", "REQUEST.md"]}]},
        {"order": ["implement", "independent acceptance and review", "required fix if any"],
         "parallel": "independent reviewer and acceptance can read frozen candidate together"},
        {"task": "Implement REQUEST.md without modifying it or these planning artifacts.",
         "request_ref": source, "write_scope": ["sla_report.py", "tests/", "README.md", "tickets.csv"],
         "time_budget_seconds": 480, "authority": "user-authorized sandbox test, NOT this mock objective"},
        {"verification": ["worker unittest", "independent acceptance", "fresh review"],
         "recovery": "retain artifacts in disposable project; no live changes", "review_round_limit": 2,
         "fix_wave_limit": 1, "product_fix_attempt_limit": 5},
        {"disposition": "candidate-only", "compiler_status": "not-run", "group_acceptance": "not-performed",
         "required_before_real_E": ["actual objective approval", "kernel-bound Group closure", "D compiler readiness"],
         "sample_execution_authority": "separate user authorization, not workflow acceptance"},
    ]
    last = None
    for skill, body in zip(SKILLS, bodies):
        start = datetime.now(timezone.utc).isoformat()
        output = write(skill + ".json", body)
        argv = ["save", "--intake", intake_path, "--skill", skill, "--output", str(output),
                "--status", "recorded", "--started-at", start, "--check", "fixture draft bytes present"]
        if last:
            argv += ["--previous", last["handoff"]["path"]]
        if skill in ("grill-purpose", "approve-objective"):
            receipt = write(skill + "-mock-receipt.json", {"source": "mock", "work_id": "support-report",
                "subject": reference(output), "decision": "approve" if skill == "approve-objective" else "answer",
                "explicit": True, "actor": "sample-mock", "recorded_at": start})
            argv += ["--receipt", str(receipt)]
        last = call(*argv)
        resumed = call("resume", "--handoff", last["handoff"]["path"])
        assert resumed["mode"] == "rehearsal" and not resumed["execution_authorized"]
    result = {"scope": "mechanical draft replay; not AI Skill/Kernel acceptance",
              "calls": timings, "last_handoff": last["handoff"],
              "helper_seconds": sum(item["seconds"] for item in timings),
              "implementation_brief": str(drafts / "prepare-worker-briefs.json")}
    (project / "rehearsal-result.json").write_text(json.dumps(result, ensure_ascii=False, indent=2) + "\n")
    print(json.dumps({key: value for key, value in result.items() if key != "calls"}, ensure_ascii=False, indent=2))


if __name__ == "__main__":
    main()
