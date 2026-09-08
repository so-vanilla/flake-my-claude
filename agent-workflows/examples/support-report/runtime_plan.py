"""Prepare source-bound C/D inputs for a deterministic support-report test driver.

This module writes planning material in a supplied disposable project. It does
not run a model, invoke Skills, approve a real objective, or dispatch workers.
The caller invokes the real OutcomeSystemV1/PlanningSystemV1 compilers and saves
their actual outputs. Only the human purpose/option responses are mock data.
"""
from __future__ import annotations

import ast
import copy
import hashlib
import json
from pathlib import Path
from typing import Any, Mapping


def outcome_owner(owner_ref: Mapping[str, Any], *, actor_id: str | None = None) -> dict[str, Any]:
    """Adapt the versioned runtime actor to Group C's strict owner vocabulary."""
    return {
        "kind": owner_ref.get("kind", "system"),
        "stable_id": actor_id or owner_ref.get("stable_id", "support-report-driver"),
        "role": owner_ref.get("role", "disposable-rehearsal-owner"),
        "path": owner_ref["path"], "digest": owner_ref["digest"],
    }


def prepare(project: Path, source_root: Path, *, objective_ref: Mapping[str, Any],
            owner_ref: Mapping[str, Any], receipt_ref: Mapping[str, Any]) -> list[tuple[str, dict[str, Any]]]:
    """Return C1-C6 then D1-D12 inputs, using real file hashes and mock choices.

    All returned project references are relative to ``project``. D1's workflow
    reference is relative to ``source_root``, as required by PlanningSystemV1.
    The caller supplies the already persisted objective, runtime owner, and
    mock purpose receipt references; D6 preserves that exact owner as actor.
    Use ``rehearsal:...`` namespace and ``candidate-generic`` authority scope.
    """
    project, source_root = Path(project).resolve(), Path(source_root).resolve()
    if project == source_root or project in source_root.parents or source_root in project.parents:
        raise ValueError("the disposable project must be separate from the workflow source")
    project.mkdir(parents=True, exist_ok=True)
    base = project / ".local/agent/support-report-plan"

    def physical(ref: Mapping[str, Any]) -> dict[str, Any]:
        copied = copy.deepcopy(dict(ref))
        path = Path(copied["path"])
        path = path if path.is_absolute() else project / path
        if "sha256:" + hashlib.sha256(path.read_bytes()).hexdigest() != copied["digest"]:
            raise ValueError("supplied reference does not bind physical bytes: " + str(path))
        return copied

    objective, actor, purpose_receipt = map(physical, (objective_ref, owner_ref, receipt_ref))
    actor_path = Path(actor["path"])
    actor_document = json.loads((actor_path if actor_path.is_absolute() else project / actor_path).read_text(encoding="utf-8"))
    c_owner = outcome_owner(actor, actor_id=actor_document.get("actor_id"))

    def save(name: str, value: Any) -> dict[str, str]:
        path = base / name
        for ancestor in [path, *path.parents]:
            if ancestor == project:
                break
            if ancestor.is_symlink():
                raise ValueError("planning output cannot follow a symlink")
        path.parent.mkdir(parents=True, exist_ok=True)
        raw = value.encode("utf-8") if isinstance(value, str) else (json.dumps(value, ensure_ascii=False, sort_keys=True, indent=2) + "\n").encode("utf-8")
        if path.exists() and path.read_bytes() != raw:
            raise ValueError("existing planning material differs: " + str(path))
        if not path.exists():
            path.write_bytes(raw)
        return {"path": path.relative_to(project).as_posix(), "version": "v1", "digest": "sha256:" + hashlib.sha256(raw).hexdigest()}

    sample = source_root / "agent-workflows/examples/support-report"
    request = save("sources/REQUEST.md", (sample / "REQUEST.md").read_text(encoding="utf-8"))
    acceptance_text = (sample / "acceptance.py").read_text(encoding="utf-8")
    acceptance = save("sources/acceptance.py", acceptance_text)
    tree = ast.parse(acceptance_text)
    checks = [node.name for node in ast.walk(tree) if isinstance(node, ast.FunctionDef) and node.name.startswith("test_")]
    if not checks:
        raise ValueError("acceptance source has no named business checks")
    origin = save("source-provenance.json", {
        "driver": "deterministic compiler test driver; no model Skill orchestration",
        "request_source": str(sample / "REQUEST.md"), "request_snapshot": request,
        "acceptance_source": str(sample / "acceptance.py"), "acceptance_snapshot": acceptance,
        "objective_ref": objective, "mock_purpose_receipt_ref": purpose_receipt,
        "acceptance_methods": checks,
    })
    predicate = save("acceptance-predicate.json", {
        "required": "All business checks pass against the implemented CLI, with no errors or failures.",
        "checks": checks, "source_refs": [request, acceptance],
        "command": "python3 .local/agent/support-report-plan/sources/acceptance.py .",
        "manual_checks": ["README usage and supported datetime precision agree with REQUEST.md", "implementation imports only Python standard library"],
    })
    guard = save("measurement-guard.json", {
        "rule": "Freeze external acceptance source. Do not count compiler validation as product correctness.",
        "source_refs": [acceptance, request], "check_count": len(checks),
        "failure_policy": "Missing tests, changed checks, skipped cases, timeouts, stderr traceback or partial stdout cannot count as success.",
    })
    outcome_id = "support-report-reliable-priority"
    outcome_map = {
        "schema": "outcome-map/v1", "outcomes": [{
            "outcome_id": outcome_id,
            "achieved_state": "担当者が固定基準時刻に対する未完了の期限超過問い合わせを、優先度・実時刻・ID順で再現可能に確認でき、壊れた入力を正常なレポートと誤認しない。",
            "why_required": "毎朝の問い合わせ対応順序を正しく決めるため。",
            "objective_contribution": ["correct-priority", "deterministic-report", "invalid-input-safety"],
            "exclusion_conditions": ["実サービス接続、実運用の効果測定、実ユーザー承認は含まない。"],
            "owner_ref": c_owner, "acceptance_predicate_refs": [predicate],
        }], "coverage_refs": [request, predicate],
    }
    graph = {"schema": "outcome-dependency-graph/v1", "node_ids": [outcome_id], "edges": [], "joins": [],
             "runtime_checks": ["unknown_endpoint", "orphan", "cycle", "ownerless_join"]}
    measurement = {"schema": "measurement-plan/v1", "outcome_id": outcome_id, "strategy": "direct_metric",
                   "rationale": "固定入力と固定as-ofによる外部CLI受入検査で、利用者が観測する出力・終了状態を直接検証する。"}
    target_detail = {"unit": "passing business checks", "formula": "passed / all required checks = 1; failures = errors = skipped = 0",
                     "source": acceptance, "frequency": "after implementation and each relevant repair", "window": "this disposable rehearsal before delivery",
                     "guard": guard, "required_check_count": len(checks), "local": False}
    target_ref = save("target.json", target_detail)
    target = {"schema": "target-set/v1", "outcome_id": outcome_id, "target_ref": target_ref}
    save("baseline-observation-context.json", {
        "implementation_present": (project / "sla_report").exists(),
        "status": "unavailable", "reason": "No comparable completed external acceptance run is supplied at planning time; absence is not a measured zero.",
        "source_refs": [origin, acceptance],
    })
    observation = {"schema": "measurement-observation/v1", "observation_id": "support-report-preimplementation",
                   "status": "unavailable", "value": None, "reason": {"code": "incomparable"}}
    baseline = {"schema": "baseline/v1", "target_ref": target_ref, "availability": "unavailable", "value": None, "reason": "incomparable_condition"}
    c_values = [
        {"outcome_map": outcome_map, "required_contributions": ["correct-priority", "deterministic-report", "invalid-input-safety"]},
        {"dependency_graph": graph}, {"measurement_plan": measurement, "gaming_guard_ref": guard},
        {"target_set": target, "target": target_detail}, {"target_set": target, "observation": observation},
        {"trace": {"objective": objective, "outcome_map": outcome_map, "dependency_graph": graph,
                   "measurement_plan": measurement, "target_set": target, "baseline": baseline}},
    ]
    result = []
    for number, values in enumerate(c_values, 1):
        values = {"objective_ref": objective, **values}
        save(f"C{number}-inputs.json", values)
        result.append((f"group.C.C{number}", values))

    spec = {
        "behavior": "open/in_progress かつ due_at < as-of を抽出。同時刻は除外。priority critical/high/normal/low、due_at実時刻、id文字列の順に整列。",
        "scenarios": ["日本語とカンマを含むCSVをJSONで報告", "UTC Zとoffsetが同じ瞬間なら同じ結果", "headerのみならcount=0", "Markdownでは縦棒と改行をエスケープ"],
        "capabilities": ["固定as-of必須", "json既定またはmarkdown", "元の5列をJSON tickets内に保持", "件数とヘルプを表示"],
        "constraints": ["Python標準ライブラリのみ", "ネットワーク・外部サービスなし", "秒・小数秒1〜6桁、timezone必須", "CSVはカンマ区切り・二重引用符・内部引用符二重化"],
        "non_goals": ["問い合わせ更新", "優先度の自動推測", "常駐処理", "実行時刻の暗黙利用"],
        "edges": ["closed行も全検証", "小数秒7桁以上は拒否", "未引用フィールド内の引用符は拒否", "改行入り引用済みsummaryは保存", "行幅過不足は拒否"],
        "errors": "列不足、空/重複id、未知priority/status、不正またはnaive日時、不正CSV、ファイル不存在はexit 2・原因をstderrへ。CSV値エラーは行と項目を示し、stdoutは空。成功exit 0。",
        "acceptance": {"source_ref": predicate, "business_checks": checks}, "questions": [],
    }
    specification = save("specification.json", spec)
    inventory_text = {
        "entrypoints": "新規CLI python3 -m sla_report。既存プロダクトへの結合はない。",
        "domain_terms": "問い合わせid、priority、status、期限due_at、summary、基準時刻as-of。",
        "data_control_flows": "CSV全行検証→期限超過抽出→安定した3キー整列→一括出力。",
        "external_contracts": "入力CSV、CLI引数、JSON/Markdown、終了コードとstderr。",
        "tests": f"外部受入ソースに{len(checks)}件の名前付きビジネス検査。",
        "constraints": "標準ライブラリ限定、ネットワークと外部変更禁止、隔離プロジェクト限定。",
        "prior_decisions": "目的と選択肢応答のみ隔離試験用モック。実ユーザー承認として利用しない。",
        "verification_surfaces": "外部CLI検査、プロジェクト内単体テスト、READMEのコマンド再実行。",
    }
    inventory = {key: {"description": value, "source_refs": [request, acceptance, origin], "material_gap": False} for key, value in inventory_text.items()}
    options = [
        {"name": "validated-records", "description": "全行を検証してからレコード配列を抽出・整列し、一括で描画する。", "tradeoff": "メモリは入力件数に比例するが、部分出力を避けやすく責務を分離できる。", "falsification_condition": "要求規模で配列保持が利用可能メモリを超える場合。"},
        {"name": "temporary-spool", "description": "検証済み行を一時領域に保持し、検証終了後に並べ替えて出力する。", "tradeoff": "メモリ制約に強いが、一時ファイル管理と障害復旧が増える。", "falsification_condition": "外部整列の複雑さが今回の小さなCLIの保守負担を超える場合。"},
    ]
    options_ref = save("options.json", options)
    option_receipt = save("mock-option-response.json", {"source": "mock", "explicit": True, "decision": "approve", "scope": "option-selection",
                        "actor_ref": actor, "selected_option": "validated-records", "options_ref": options_ref,
                        "purpose_receipt_ref": purpose_receipt, "boundary": "isolated deterministic test response, not real user approval"})
    approved_option = {"receipt_ref": option_receipt, "source": "mock", "explicit": True, "decision": "approve", "scope": "option-selection", "actor_ref": actor}
    design = {
        "architecture": "sla_report package with CLI adapter and pure validation/filtering/rendering functions; validated-records option.",
        "responsibilities": "CLI owns arguments/exit codes; loader owns strict CSV structure; datetime parser owns timezone/precision; selection owns ordering; renderers own escaping.",
        "interfaces": "main(argv)->int; parse_timestamp(text,field,row)->aware datetime; load_tickets(path)->records; select_overdue(records,as_of)->records; render_json/render_markdown->str.",
        "flow": "Parse arguments and as-of; load and validate all rows including closed; filter; sort normalized instants; render once; write stdout only after success.",
        "errors": "Use a domain input error carrying row and field; CLI catches expected input/I/O failures and emits concise stderr with exit 2.",
        "compatibility": "JSON original strings preserved; timezone conversion only for comparison. Stable five-column output and deterministic ordering.",
        "migration": "New isolated package; no existing data/schema/configuration migration.",
        "observability": "Explicit stderr input diagnostics and process exit status; no telemetry or network.",
        "security": "Treat CSV text as data; no eval, shell execution, credential access, or network. Escape Markdown delimiters/newlines.",
        "test_seams": "Timestamp and CSV validators as pure/local functions; external acceptance through subprocess CLI.", "unresolved": [],
    }
    design_ref = save("design.json", design)
    ticket_schema = {"type": "object", "additionalProperties": False,
                     "required": ["id", "priority", "status", "due_at", "summary"],
                     "properties": {field: {"type": "string"} for field in ("id", "priority", "status", "due_at", "summary")}}
    schema = save("report-json-schema.json", {"type": "object", "additionalProperties": False, "required": ["count", "tickets"],
        "properties": {"count": {"type": "integer", "minimum": 0}, "tickets": {"type": "array", "items": ticket_schema}}})
    examples = save("contract-examples.json", {"as_of": "2026-09-06T09:00:00+09:00", "input": "id,priority,status,due_at,summary\nA,high,open,2026-09-05T23:00:00Z,問い合わせ\n",
        "expected": {"count": 1, "tickets": [{"id": "A", "priority": "high", "status": "open", "due_at": "2026-09-05T23:00:00Z", "summary": "問い合わせ"}]}, "source_refs": [request, acceptance]})
    contracts = [{"owner": actor["path"], "version": "v1", "consumes": ["CSV five required columns", "required timezone-aware as-of", "json or markdown"],
        "produces": ["whole report on stdout, exit 0", "input diagnostic on stderr, empty stdout, exit 2"], "schema_ref": schema, "fixture_refs": [examples, acceptance],
        "failures": spec["errors"], "idempotency": "Same bytes and arguments produce identical report; no state mutation.",
        "backward_compatibility": "Initial interface; do not add or rename JSON fields.", "mutable_writes": ["sla_report", "tests", "README.md"]}]
    tasks = [
        {"task_id": "parse-select", "files": ["sla_report/__init__.py", "sla_report/core.py"], "interface": "validated ticket loading, aware datetime parsing, deterministic overdue selection", "checks": ["CSV quoting and row width", "1-6 fractional digits", "closed row validation", "priority/time/id ordering"]},
        {"task_id": "cli-render", "files": ["sla_report/__main__.py"], "interface": "CLI arguments, whole-output JSON/Markdown render, stderr and exit handling", "checks": ["no partial stdout", "Japanese and Markdown escaping", "help and required as-of"]},
        {"task_id": "tests-docs", "files": ["tests/test_sla_report.py", "README.md"], "interface": "repeatable stdlib unit tests and executable CLI usage examples", "checks": ["all external acceptance methods", "document precision and error semantics"]},
    ]
    for task in tasks:
        task.update({"inputs": [specification, design_ref, examples], "outputs": task["files"], "stop": "Stop on source drift, unclear requirement, external access, or writes outside assigned files.",
                     "report_path": ".local/agent/support-report-plan/reports/" + task["task_id"] + ".json", "parent_outcome_contribution": outcome_id})
    briefs = [{"task_id": t["task_id"], "purpose_ref": objective, "task": t["interface"], "interface": t["interface"], "write_scope": t["files"],
               "checks": t["checks"], "stop": t["stop"], "report_path": t["report_path"], "exploration_refs": [specification, design_ref, acceptance]} for t in tasks]
    gates = {
        "test": {"required": True, "check": "project unit tests and frozen external acceptance pass"},
        "review": {"required": True, "check": "independent source review against REQUEST and acceptance; save actual findings"},
        "finding_validation": {"required": True, "check": "reproduce findings; repair required defects within finite budget"},
        "e2e": {"required": True, "check": "CLI subprocess acceptance through public python -m interface"},
        "dry_run": {"required": True, "check": "replay README example with disposable input"},
        "rollback": {"required": True, "check": "retain pre-change file snapshot; no live state involved"},
        "post_check": {"required": True, "check": "no external writes; compare frozen acceptance source digest"},
        "activation": {"required": False, "reason": "no installed or live runtime activation authorized"},
        "git": {"required": False, "reason": "commit/push prohibited by request"},
        "external": {"required": False, "reason": "network and external services prohibited"},
    }
    workflow_path = "agent-workflows/workflows/feature-bounded.json"
    workflow = {"path": workflow_path, "version": "v1", "digest": "sha256:" + hashlib.sha256((source_root / workflow_path).read_bytes()).hexdigest()}
    d_values = [
        {"domain": "software", "workflow_id": "feature-bounded", "workflow_manifest_ref": workflow},
        {"inventory": inventory},
        {"guidance_sources": [request, acceptance], "constraints": [{"rule": rule, "source_refs": [request, acceptance]} for rule in spec["constraints"]]},
        {"specification": spec}, {"options": options, "status_quo": {"name": "manual-prioritization", "cost": "Each morning requires manual filtering and ordering with inconsistent error handling."}, "recommendation": "validated-records"},
        {"approved_option_receipt": approved_option, "design": design}, {"contracts": contracts}, {"tasks": tasks},
        {"tasks": tasks, "edges": [{"from": "parse-select", "to": "cli-render"}, {"from": "cli-render", "to": "tests-docs"}], "parallel_batches": [], "convergence": []},
        {"briefs": briefs}, {"task_budgets": [{"task_id": t["task_id"], "wall_clock_minutes": 15, "review_rounds": 2, "fix_attempts": 2} for t in tasks], "gates": gates},
        {"evidence_refs": [specification, design_ref, predicate, option_receipt], "open_risks": [], "upstream_route": None},
    ]
    for number, values in enumerate(d_values, 1):
        values = {"input_refs": [request, acceptance, objective, origin], **values}
        save(f"D{number}-inputs.json", values)
        result.append((f"group.D.D{number}", values))
    return result
