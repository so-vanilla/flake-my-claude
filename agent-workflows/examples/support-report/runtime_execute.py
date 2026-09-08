"""Parent-operated real Kernel task driver for the isolated sample.

No model or shell execution is simulated here: workers execute separately and
the parent supplies their physical result/check receipts after observing them.
"""
import json
from pathlib import Path
import subprocess
import sys
import time

from runtime_trial import put, SOURCE
from ai_agent_workflow.inception_cli import create_file
from ai_agent_workflow.inception_runtime import InceptionRuntime, file_ref, verify_refs


def dispatch(project, task_id):
    runtime = InceptionRuntime(project, "support-report")
    runtime._budget()
    if runtime.state["group"]["id"] != "E" or runtime.state["group"]["status"] != "open":
        raise ValueError("actual D closure and E entry required")
    records = runtime.records()
    planned = records["D8"]["value"]["compiled"]["output"]["tasks"]
    ids = [task["task_id"] for task in planned]
    task = next(task for task in planned if task["task_id"] == task_id)
    for before in ids[:ids.index(task_id)]:
        if runtime.state["tasks"].get(before, {}).get("status") != "succeeded":
            raise ValueError("planned predecessor is not completed: " + before)
    assignment = "worker-" + task_id
    authority = {"approved": True, "run_id": "support-report", "scopes": ["publish_task_package", "claim_task", "accept_task_result"],
                 "write_scopes": task["files"] + [task["report_path"]],
                 "human_receipt": runtime.approval["receipt"], "expires_at": runtime.state["review_budget"]["deadline"]}
    package = {"write_scope": task["files"] + [task["report_path"]], "acceptance": task["checks"],
               "stop_conditions": ["scope mismatch", "source drift", "deadline expired"],
               "output_path": task["report_path"]}
    refs = [records["D8"]["ref"], records["D10"]["ref"], records["D12"]["ref"]]
    state = runtime.kernel.publish_task_package(task_id, package, assignment_id=assignment,
                                               authority_ref=authority, input_refs=refs)
    state = runtime.kernel.claim_task(task_id, assignment_id=assignment, authority_ref=authority)
    report = {"project": str(Path(project).resolve()), "task": task,
              "package_ref": state["tasks"][task_id]["package_ref"], "assignment_id": assignment,
              "deadline": state["review_budget"]["deadline"], "authority": authority}
    ref = put(Path(project).resolve(), task_id + "-dispatch.json", report)
    print(json.dumps(ref))


def accept(project, task_id):
    project = Path(project).resolve()
    runtime = InceptionRuntime(project, "support-report")
    dispatch = json.loads((project / ".local/agent/support-report-trial" / (task_id + "-dispatch.json")).read_text())
    task = dispatch["task"]
    report = project / task["report_path"]
    actual = json.loads(report.read_text())
    if actual.get("status") != "success" or not actual.get("checks"):
        raise ValueError("worker must supply actual successful check evidence")
    refs = [file_ref(project / path) for path in task["files"]]
    result = {"status": "success", "changed_paths": task["files"] + [task["report_path"]],
              "output_refs": refs, "check_receipt_ref": file_ref(report)}
    submission_authority = {**dispatch["authority"], "scopes": ["submit:worker-result"]}
    runtime.kernel.submit_task_result(task_id, result, worker_assignment_id=dispatch["assignment_id"], authority_ref=submission_authority)
    state = runtime.kernel.accept_task_result(task_id, result, worker_assignment_id=dispatch["assignment_id"], authority_ref=dispatch["authority"])
    print(json.dumps({"task": task_id, "status": state["tasks"][task_id]["status"], "head": runtime.kernel.head()}))


def verify(project, _unused=None):
    project = Path(project).resolve()
    runtime = InceptionRuntime(project, "support-report")
    if len(runtime.state["tasks"]) != 3 or any(task["status"] != "succeeded" for task in runtime.state["tasks"].values()):
        raise ValueError("all three planned tasks must actually succeed")
    files = sorted(path for task in runtime.state["tasks"].values() for path in task["write_scope"])
    outputs = [file_ref(project / path) for path in files]
    acceptance = SOURCE / "agent-workflows/examples/support-report/acceptance.py"
    checks = []
    for name, command in [("external-acceptance", [sys.executable, "-B", str(acceptance), str(project)]),
                          ("unit-tests", [sys.executable, "-B", "-m", "unittest", "discover", "-s", "tests", "-v"])]:
        start = time.monotonic()
        result = subprocess.run(command, cwd=project, capture_output=True, text=True, timeout=60)
        path = project / ".local/agent/support-report-trial" / (name + ".log")
        create_file(path, (result.stdout + result.stderr).encode())
        checks.append({"name": name, "exit_code": result.returncode, "seconds": time.monotonic()-start, "log_ref": file_ref(path)})
    value = {"schema": "support-report-verification/v1", "checks": checks, "output_refs": outputs,
             "acceptance_ref": file_ref(acceptance), "objective_digest": runtime.state["objective_ref"]["digest"],
             "status": "passed" if all(item["exit_code"] == 0 for item in checks) else "failed"}
    ref = put(project, "verification.json", value)
    print(json.dumps({"status": value["status"], "receipt_ref": ref, "checks": checks}))
    if value["status"] != "passed":
        raise SystemExit(2)


def finalize(project, validation_path):
    """Accept supplied independent required-only dispositions, then close E."""
    from ai_agent_workflow.runtime_closure import close_runtime_group
    project = Path(project).resolve()
    runtime = InceptionRuntime(project, "support-report")
    kernel = runtime.kernel
    validation_ref = file_ref(Path(validation_path).resolve())
    validation = json.loads(Path(validation_ref["path"]).read_text())
    verified_path = project / ".local/agent/support-report-trial/verification.json"
    candidate_ref = file_ref(verified_path)
    candidate = json.loads(verified_path.read_text())
    verify_refs(candidate, project)
    if candidate["status"] != "passed" or validation.get("candidate_ref") != candidate_ref or validation.get("status") != "accepted" or validation.get("required_findings") != []:
        raise ValueError("actual independent validation of passing unchanged candidate required")
    verify_refs(validation, project)
    review_refs = validation["review_refs"]
    if len(review_refs) != 2:
        raise ValueError("two review axes required")
    reviews = [json.loads(Path(ref["path"]).read_text()) for ref in review_refs]
    if {review.get("axis") for review in reviews} != {"architecture-safety", "operability-quality"}:
        raise ValueError("review axes do not cover this candidate")
    if any(review.get("candidate_ref") != candidate_ref for review in reviews):
        raise ValueError("reviews must bind the exact frozen candidate")
    supplied_ids = [finding["finding_id"] for review in reviews for finding in review["findings"]]
    outcomes = validation.get("outcomes", [])
    if (len(supplied_ids) != len(set(supplied_ids))
            or sorted(supplied_ids) != sorted(item["candidate_id"] for item in outcomes)
            or any(item["disposition"] in {"required", "needs-user"} for item in outcomes)):
        raise ValueError("each finding needs exactly one non-required Validator disposition before close")
    if len({review.get("reviewer") for review in reviews} | {validation.get("validator")}) != 3:
        raise ValueError("review and Validator must be distinct")
    def authority(operation):
        return {"approved": True, "run_id": "support-report", "scopes": [operation],
                "human_receipt": runtime.state["objective_ref"]["approval_ref"]["digest"]}
    task_ref = runtime.state["tasks"]["tests-docs"]["package_ref"]
    for index, review in enumerate(reviews, 1):
        epoch, review_id = "E-review-%d" % index, "sample-review-%d" % index
        kernel.open_review_epoch(epoch, task_ref, reviewer_assignment_id=review["reviewer"], authority_ref=authority("open_review_epoch"))
        findings = []
        for finding in review["findings"]:
            evidence = finding["evidence"]
            findings.append({**finding, "evidence": evidence if isinstance(evidence, list) else [evidence]})
        kernel.open_review(review_id, "tests-docs", findings, reviewer_assignment_id=review["reviewer"], fresh_epoch_id=epoch, authority_ref=authority("open_review"))
        if findings:
            validator_epoch = "E-validator-%d" % index
            kernel.open_review_epoch(validator_epoch, task_ref, reviewer_assignment_id=validation["validator"], authority_ref=authority("open_review_epoch"))
            ids = {finding["finding_id"] for finding in findings}
            kernel.validate_findings(review_id, [item for item in outcomes if item["candidate_id"] in ids], validator_assignment_id=validation["validator"], fresh_epoch_id=validator_epoch, authority_ref=authority("validate_findings"))
    value = {"objective_digest": runtime.state["objective_ref"]["digest"], "result": "verified", "candidate_ref": candidate_ref,
             "review_refs": review_refs, "validation_ref": validation_ref,
             "task_result_refs": [task["result_ref"] for task in kernel.read_state()["tasks"].values()]}
    state = kernel.publish_artifact("runtime-E-integration", "v1", value, kind="runtime-integration", authority_ref=authority("publish_artifact"))
    product_ref = state["artifacts"]["runtime-E-integration"]["object_ref"]
    state = kernel.publish_artifact("runtime-audit-E", "v1", {"objective_digest": state["objective_ref"]["digest"], "group_id": "E",
        "alignment": "aligned", "artifact_refs": [product_ref], "reviewer": validation["validator"],
        "rationale": "全3実装結果・固定外部受入・2軸独立評価を単一評価者が確認"}, kind="runtime-group-audit", authority_ref=authority("publish_artifact"))
    result = close_runtime_group(kernel, {"approved": True, "human_receipt": state["objective_ref"]["approval_ref"]["digest"]}, next_group="H",
                                 evidence_refs=[product_ref, state["artifacts"]["runtime-audit-E"]["object_ref"]])
    print(json.dumps({"group": result["state"]["group"], "head": kernel.head(), "closure_report_ref": result["closure_report_ref"]}))


if __name__ == "__main__":
    {"dispatch": dispatch, "accept": accept, "verify": verify, "finalize": finalize}[sys.argv[1]](sys.argv[2], sys.argv[3] if len(sys.argv) > 3 else None)
