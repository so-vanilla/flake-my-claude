"""Real-filesystem pre-Run persistence tests, not kernel acceptance fixtures."""
import contextlib
import io
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "src"))
from ai_agent_workflow import inception_cli as cli


class InceptionCliTests(unittest.TestCase):
    def test_documented_b_entrypoints_delegate_to_real_selector_api(self):
        from ai_agent_workflow.objective_system import ObjectiveSystemV1
        compiler = ObjectiveSystemV1()
        names = ("capture_intake", "discover_context", "classify_scope", "continue_inquiry",
                 "propose_options", "assess_feasibility", "prepare_approval")
        for number, name in enumerate(names, 1):
            with patch.object(compiler, "compile", return_value={"sentinel": number}) as dispatch:
                self.assertEqual(getattr(compiler, name)({}, {}, {}), {"sentinel": number})
                dispatch.assert_called_once_with("group.B.B%d" % number, {}, {}, {})

    def test_genesis_revision_is_integer_not_boolean(self):
        from ai_agent_workflow.objective_system import ObjectiveSystemV1, ObjectiveSystemError
        # A boolean compares equal to zero in Python but is not a Kernel revision.
        ref = {"path": ".local/source.json", "digest": "sha256:" + "a" * 64, "selector": "source"}
        authority = {"owner_ref": {"owner_kind": "system", "owner_id": "test", "authority_ref": ref},
                     "source_refs": [ref]}
        with self.assertRaises(ObjectiveSystemError):
            ObjectiveSystemV1().compile("group.B.B1", {}, authority,
                                        {"revision": False, "transaction_digest": None})

    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="inception-test-")
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name).resolve()
        subprocess.run(["git", "init", "-q", str(self.root)], check=True)
        (self.root / ".gitignore").write_text(".local/\n")
        self.request = self.root / "REQUEST.md"
        self.request.write_text("問い合わせの期限超過を一覧化。外部送信は禁止。\n")
        self.start = cli.init(self.root, "sample", self.request, "rehearsal")
        self.intake_path = Path(self.start["intake"]["path"])
        self.intake = cli.read_json(self.intake_path)

    def output(self, name, value):
        path = self.root / name
        path.write_text(json.dumps(value, ensure_ascii=False))
        return path

    def entry(self, status="recorded"):
        output = self.output("entry.json", {
            "raw_request_ref": self.intake["request"], "interpretation": "期限一覧CLI",
            "assumptions": [], "unknowns": ["期限の扱い"]})
        return cli.save(self.intake_path, "entry", output, status)

    def test_init_no_fake_head_run_or_approval(self):
        self.assertIsNone(self.intake["run"])
        self.assertIsNone(self.intake["objective"])
        self.assertNotIn("expected_head", self.intake)
        self.assertEqual(Path(self.intake["request"]["path"]).read_bytes(), self.request.read_bytes())
        self.assertFalse((self.root / ".local/agent/runs").exists())

    def test_init_duplicate_preserves_request_and_loop_contract(self):
        before = self.intake_path.read_bytes()
        with self.assertRaisesRegex(cli.InceptionError, "exists"):
            cli.init(self.root, "sample", self.request, budget_seconds=9999)
        self.assertEqual(self.intake_path.read_bytes(), before)

    def test_normal_intake_uses_iteration_control_without_wall_clock_budget(self):
        self.assertEqual("inception-intake/v2", self.intake["schema"])
        self.assertNotIn("budget_seconds", self.intake)
        self.assertEqual("iteration-and-evidence", self.intake["loop_control"]["progress_control"])
        out = self.entry()
        with patch.object(cli, "now", return_value="2099-01-01T00:00:00+00:00"):
            resumed = cli.resume(out["handoff"]["path"])
        self.assertFalse(resumed["budget_exhausted"])
        self.assertIsNone(resumed["remaining_seconds"])
        self.assertEqual("iteration-and-evidence", resumed["progress_control"])
        self.assertIsNotNone(resumed["invocation"])

    def test_ignore_required_before_request_saved(self):
        (self.root / ".gitignore").write_text("!.local/\n!.local/**\n")
        with self.assertRaisesRegex(cli.InceptionError, "ignored"):
            cli.init(self.root, "not-ignored", self.request)
        self.assertFalse((self.root / ".local/agent/inception/not-ignored").exists())

    def test_traversal_and_symlink_storage_refused(self):
        with self.assertRaisesRegex(cli.InceptionError, "work id"):
            cli.init(self.root, "../escape", self.request)
        (self.intake_path.parent.parent / "alias").symlink_to(self.intake_path.parent)
        with self.assertRaisesRegex(cli.InceptionError, "symlink"):
            cli.init(self.root, "alias", self.request)

    def test_atomic_no_overwrite(self):
        path = self.root / "immutable"
        cli.create_file(path, b"original")
        with self.assertRaises(FileExistsError):
            cli.create_file(path, b"replacement")
        self.assertEqual(path.read_bytes(), b"original")

    def test_entry_schema_checks_actual_content(self):
        wrong = self.output("wrong.json", {"raw": "new invented request"})
        with self.assertRaisesRegex(cli.InceptionError, "separate"):
            cli.save(self.intake_path, "entry", wrong, "recorded")
        self.assertEqual(list(self.intake_path.parent.glob("handoff-*.json")), [])

    def test_one_skill_save_and_fresh_process_resume(self):
        out = self.entry()
        command = [sys.executable, "-B", "-m", "ai_agent_workflow.inception_cli", "resume",
                   "--handoff", out["handoff"]["path"]]
        import os
        env = {**os.environ, "PYTHONPATH": str(ROOT / "src")}
        result = subprocess.run(command, capture_output=True, text=True, env=env, timeout=10)
        self.assertEqual(result.returncode, 0, result.stderr)
        saved = json.loads(result.stdout)
        self.assertEqual(saved["next_skill"], "discover-context")
        self.assertEqual(saved["mode"], "rehearsal")
        self.assertEqual(saved["compiler_status"], "not-run")
        self.assertEqual(saved["frontier_status"], "helper-verified")
        self.assertIn("first run resume", saved["invocation"])
        self.assertFalse(saved["execution_authorized"])
        self.assertEqual(len(list(self.intake_path.parent.glob("handoff-*.json"))), 1)

    def test_structurally_valid_handwritten_handoff_has_no_helper_authority(self):
        out = self.entry()
        path = Path(out["handoff"]["path"])
        record = cli.read_json(path)
        del record["helper_proof"]
        path.write_text(json.dumps(record))
        with self.assertRaisesRegex(cli.InceptionError, "helper-generated proof"):
            cli.resume(path)

    def test_timestamp_accepts_rfc3339_and_common_shell_offset(self):
        self.assertEqual(cli.stamp("2026-09-06T12:34:56Z").utcoffset().total_seconds(), 0)
        self.assertEqual(cli.stamp("2026-09-06T12:34:56+09:00").utcoffset().total_seconds(), 9 * 3600)
        self.assertEqual(cli.stamp("2026-09-06T12:34:56+0900").utcoffset().total_seconds(), 9 * 3600)

    def test_timestamp_still_requires_explicit_valid_timezone(self):
        with self.assertRaisesRegex(cli.InceptionError, "requires a timezone"):
            cli.stamp("2026-09-06T12:34:56")
        with self.assertRaisesRegex(cli.InceptionError, "invalid timestamp"):
            cli.stamp("not-a-time")

    def test_skipping_skill_and_stale_frontier_refused(self):
        first = self.entry()
        output = self.output("context.json", {"facts": ["empty sample"]})
        with self.assertRaisesRegex(cli.InceptionError, "expected one"):
            cli.save(self.intake_path, "design-solution", output, "recorded", first["handoff"]["path"])
        second = cli.save(self.intake_path, "discover-context", output, "recorded", first["handoff"]["path"])
        with self.assertRaisesRegex(cli.InceptionError, "stale frontier"):
            cli.save(self.intake_path, "classify-scope", output, "recorded", first["handoff"]["path"])
        self.assertEqual(second["next_skill"], "classify-scope")

    def test_blocked_resumes_same_skill(self):
        out = self.entry("blocked")
        self.assertEqual(out["next_skill"], "entry")
        self.assertEqual(out["status"], "blocked")

    def test_modified_output_and_raw_request_refused(self):
        out = self.entry()
        output = Path(out["output"]["path"])
        original = output.read_bytes()
        output.write_text("modified")
        with self.assertRaisesRegex(cli.InceptionError, "stale input"):
            cli.resume(out["handoff"]["path"])
        output.write_bytes(original)
        Path(self.intake["request"]["path"]).write_text("modified request")
        with self.assertRaisesRegex(cli.InceptionError, "stale input"):
            cli.resume(out["handoff"]["path"])

    def test_budget_does_not_reset_on_resume_or_save(self):
        start = cli.init(self.root, "legacy", self.request, "rehearsal", budget_seconds=1800)
        intake_path = Path(start["intake"]["path"])
        intake = cli.read_json(intake_path)
        output = self.output("legacy-entry.json", {
            "raw_request_ref": intake["request"], "interpretation": "legacy",
            "assumptions": [], "unknowns": []})
        out = cli.save(intake_path, "entry", output, "recorded")
        with patch.object(cli, "now", return_value="2099-01-01T00:00:00+00:00"):
            resumed = cli.resume(out["handoff"]["path"])
            self.assertTrue(resumed["budget_exhausted"])
            self.assertEqual(resumed["remaining_seconds"], 0)
            self.assertIsNone(resumed["invocation"])
            with self.assertRaisesRegex(cli.InceptionError, "budget exhausted"):
                cli.save(intake_path, "discover-context", self.request, "recorded", out["handoff"]["path"])
            blocked = cli.save(intake_path, "discover-context", self.request, "blocked", out["handoff"]["path"])
            self.assertEqual(blocked["next_skill"], "discover-context")

    def test_rehearsal_cannot_be_real(self):
        out = self.entry()
        with self.assertRaisesRegex(cli.InceptionError, "rehearsal"):
            cli.resume(out["handoff"]["path"], require_real=True)

    def through_b6(self):
        last = self.entry()
        for skill in cli.SKILLS[1:6]:
            output = self.output(skill + ".json", {"draft": skill})
            last = cli.save(self.intake_path, skill, output, "recorded", last["handoff"]["path"])
        return last

    def approval_receipt(self, output, source="mock"):
        return self.output("approval.json", {"source": source, "work_id": "sample",
            "subject": cli.reference(output), "explicit": True, "decision": "approve",
            "actor": "sample-human-mock", "recorded_at": cli.now()})

    def test_mock_receipt_binds_candidate_and_cannot_impersonate_human(self):
        last = self.through_b6()
        objective = self.output("objective.json", {"purpose": "過期限を見逃さない"})
        bad = self.approval_receipt(objective, "human")
        with self.assertRaisesRegex(cli.InceptionError, "source/work"):
            cli.save(self.intake_path, "approve-objective", objective, "recorded", last["handoff"]["path"], receipt=bad)
        receipt = self.approval_receipt(objective)
        approved = cli.save(self.intake_path, "approve-objective", objective, "recorded", last["handoff"]["path"], receipt=receipt)
        self.assertEqual(approved["approval"], "mock-only")
        self.assertEqual(approved["group_acceptance"], "not-performed")
        self.assertFalse(approved["kernel_authority"])

    def test_absent_and_wrong_subject_receipts_refused(self):
        last = self.through_b6()
        objective = self.output("objective.json", {"purpose": "過期限を見逃さない"})
        with self.assertRaisesRegex(cli.InceptionError, "receipt required"):
            cli.save(self.intake_path, "approve-objective", objective, "recorded", last["handoff"]["path"])
        receipt = self.approval_receipt(self.request)
        with self.assertRaisesRegex(cli.InceptionError, "exact output"):
            cli.save(self.intake_path, "approve-objective", objective, "recorded", last["handoff"]["path"], receipt=receipt)

    def test_real_b7_stops_before_unaccepted_c1(self):
        # This is a synthetic human receipt only for the isolated unit test.
        # It is not evidence of any real user's objective approval.
        other = cli.init(self.root, "real-test", self.request)
        self.intake_path = Path(other["intake"]["path"])
        self.intake = cli.read_json(self.intake_path)
        last = self.through_b6()
        objective = self.output("objective.json", {"purpose": "過期限を見逃さない"})
        receipt = self.approval_receipt(objective, "human")
        data = cli.read_json(receipt)
        data["work_id"] = "real-test"
        receipt.write_text(json.dumps(data))
        out = cli.save(self.intake_path, "approve-objective", objective, "recorded", last["handoff"]["path"], receipt=receipt)
        self.assertIsNone(out["next_skill"])
        self.assertEqual(out["stop_reason"], "requires_kernel_approval_and_group_closure")

    def test_d12_cannot_dispatch_group_e(self):
        last = self.through_b6()
        objective = self.output("objective.json", {"purpose": "過期限を見逃さない"})
        receipt = self.approval_receipt(objective)
        last = cli.save(self.intake_path, "approve-objective", objective, "recorded", last["handoff"]["path"], receipt=receipt)
        for skill in cli.SKILLS[7:]:
            output = self.output(skill + ".json", {"draft": skill})
            last = cli.save(self.intake_path, skill, output, "recorded", last["handoff"]["path"])
        self.assertIsNone(last["next_skill"])
        self.assertFalse(last["execution_authorized"])
        self.assertEqual(last["compiler_status"], "not-run")
        self.assertEqual(last["approval"], "mock-only")

    def test_approval_provenance_survives_multiple_downstream_steps(self):
        last = self.through_b6()
        objective = self.output("objective.json", {"purpose": "過期限を見逃さない"})
        receipt = self.approval_receipt(objective)
        last = cli.save(self.intake_path, "approve-objective", objective, "recorded", last["handoff"]["path"], receipt=receipt)
        for skill in cli.SKILLS[7:9]:
            output = self.output(skill + ".json", {"draft": skill})
            last = cli.save(self.intake_path, skill, output, "recorded", last["handoff"]["path"])
        receipt.write_text("changed approval")
        with self.assertRaisesRegex(cli.InceptionError, "stale input"):
            cli.resume(last["handoff"]["path"])

    def test_forged_acceptance_or_execution_route_is_refused(self):
        out = self.entry()
        path = Path(out["handoff"]["path"])
        before = cli.read_json(path)
        for key, forged in (("compiler_status", "passed"), ("group_acceptance", "accepted"),
                            ("execution_authorized", True), ("next_skill", "execution-preflight"),
                            ("sequence", 100), ("approval", "human-receipt-recorded")):
            path.write_text(json.dumps({**before, key: forged}))
            with self.assertRaises(cli.InceptionError):
                cli.resume(path)
        path.write_text(json.dumps(before))

    def test_rewritten_initial_frontier_and_dropped_evidence_refused(self):
        out = self.entry()
        path = Path(out["handoff"]["path"])
        before = cli.read_json(path)
        forged = {**before, "skill": "classify-scope", "next_skill": "grill-purpose"}
        path.write_text(json.dumps(forged))
        with self.assertRaisesRegex(cli.InceptionError, "initial handoff"):
            cli.resume(path)
        path.write_text(json.dumps(before))
        second = cli.save(self.intake_path, "discover-context", self.request, "recorded", path)
        second_path = Path(second["handoff"]["path"])
        changed = cli.read_json(second_path)
        changed["inputs"] = []
        second_path.write_text(json.dumps(changed))
        with self.assertRaisesRegex(cli.InceptionError, "dropped inherited"):
            cli.resume(second_path)

    def test_error_exit_has_no_partial_stdout(self):
        stdout, stderr = io.StringIO(), io.StringIO()
        with contextlib.redirect_stdout(stdout), contextlib.redirect_stderr(stderr):
            status = cli.main(["resume", "--handoff", str(self.root / "absent")])
        self.assertEqual(status, 2)
        self.assertEqual(stdout.getvalue(), "")
        self.assertIn("error:", stderr.getvalue())

    def test_top_level_runtime_repair_forwarding_is_exact_and_read_only(self):
        argv = [
            "repair-e", "--project", str(self.root), "--run-id", "sample",
            "--action", "status", "--inputs", str(self.root / "status.json"),
        ]
        before = sorted((path.relative_to(self.root), path.read_bytes())
                        for path in self.root.rglob("*") if path.is_file())
        with patch("ai_agent_workflow.inception_runtime.main", return_value=17) as runtime_main:
            self.assertEqual(cli.main(["runtime", *argv]), 17)
        runtime_main.assert_called_once_with(argv)
        after = sorted((path.relative_to(self.root), path.read_bytes())
                       for path in self.root.rglob("*") if path.is_file())
        self.assertEqual(before, after)


if __name__ == "__main__":
    unittest.main()
