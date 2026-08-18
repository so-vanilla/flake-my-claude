from concurrent.futures import ThreadPoolExecutor
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


HELPER = Path(__file__).resolve().parents[1] / "work-ledger-hook.py"


class WorkLedgerHookTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary.name)
        subprocess.run(["git", "init", "-q", str(self.root)], check=True)
        (self.root / ".gitignore").write_text("/.local/\n", encoding="utf-8")

    def tearDown(self) -> None:
        self.temporary.cleanup()

    def run_helper(self, *arguments: str, payload=None, expected=0):
        result = subprocess.run(
            [sys.executable, str(HELPER), *arguments],
            cwd=self.root,
            input=json.dumps(payload) if payload is not None else None,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )
        self.assertEqual(result.returncode, expected, result.stderr or result.stdout)
        return result

    def initialize(self):
        result = self.run_helper(
            "init",
            "--work-id",
            "test-work",
            "--next-action",
            "Create tickets",
            "--harness",
            "codex",
            "--session-id",
            "root-session",
            "--json",
        )
        return json.loads(result.stdout)

    def active(self):
        return json.loads((self.root / ".local/agent/workplans/active.json").read_text(encoding="utf-8"))

    def ledger(self):
        active = self.active()
        return self.root / active["ledger_path"]

    def hook(self, event: str, **extra):
        payload = {
            "hook_event_name": event,
            "cwd": str(self.root),
            "session_id": "root-session",
            **extra,
        }
        result = self.run_helper("hook", "--harness", "codex", payload=payload)
        return json.loads(result.stdout) if result.stdout.strip() else None

    def write_report(self, agent="agent-one", work="test-work", ticket="T1", empty_heading=None):
        report = self.root / ".local/agent/reports" / work / f"{agent}.md"
        report.parent.mkdir(parents=True, exist_ok=True)
        sections = [
            "Summary",
            "Findings or changes",
            "Decisions for parent",
            "Validation",
            "Remaining or blocked",
            "Files changed",
        ]
        body = "\n".join(
            f"# {heading}\n\n{'Evidence.' if heading != empty_heading else ''}\n" for heading in sections
        )
        report.write_text(
            "---\n"
            "schema: agent-worker-report/v1\n"
            f"work_id: {work}\n"
            f"agent_id: {agent}\n"
            f"ticket_id: {ticket}\n"
            "status: done\n"
            "---\n\n"
            + body,
            encoding="utf-8",
        )
        return report

    def test_hook_without_active_ledger_is_noop_and_creates_no_state(self):
        output = self.hook("SessionStart", source="startup")
        self.assertIsNone(output)
        self.assertFalse((self.root / ".local").exists())

    def test_init_refuses_unignored_private_workspace(self):
        (self.root / ".gitignore").write_text("", encoding="utf-8")
        result = self.run_helper(
            "init",
            "--work-id",
            "test-work",
            "--next-action",
            "Plan",
            expected=1,
        )
        self.assertIn("not ignored", result.stderr)
        self.assertFalse((self.root / ".local").exists())

    def test_init_status_validate_and_checkpoint(self):
        initial = self.initialize()
        self.assertEqual(initial["schema"], "agent-work-ledger/v1")
        self.assertEqual(initial["checkpoint_seq"], 1)
        self.run_helper("validate", "--json")
        self.ledger().write_text(self.ledger().read_text(encoding="utf-8") + "\nUpdated.\n", encoding="utf-8")
        result = self.run_helper(
            "checkpoint",
            "--reason",
            "ticketed",
            "--next-action",
            "Dispatch worker",
            "--status",
            "active",
            "--json",
        )
        checkpoint = json.loads(result.stdout)
        self.assertEqual(checkpoint["checkpoint_seq"], 2)
        self.assertEqual(checkpoint["next_action"], "Dispatch worker")
        status = json.loads(self.run_helper("status", "--json").stdout)
        self.assertEqual(status["dirty_reasons"], [])

    def test_session_start_injects_bounded_recovery_pointer(self):
        self.initialize()
        output = self.hook("SessionStart", source="compact")
        context = output["hookSpecificOutput"]["additionalContext"]
        self.assertIn(".local/agent/workplans/test-work/ledger.md", context)
        self.assertIn("Exact next action: Create tickets", context)
        self.assertLess(len(context), 800)

    def test_subagent_start_creates_unique_metadata_and_report_paths(self):
        self.initialize()
        first = self.hook("SubagentStart", agent_id="agent-one", agent_type="workflow_worker", prompt="WORK_TICKET_ID=T1")
        second = self.hook("SubagentStart", agent_id="agent-two", agent_type="workflow_reviewer", ticket_id="T2")
        self.assertIn("test-work/agent-one.md", first["hookSpecificOutput"]["additionalContext"])
        self.assertIn("test-work/agent-two.md", second["hookSpecificOutput"]["additionalContext"])
        self.assertIn("root must validate report ticket_id explicitly", first["hookSpecificOutput"]["additionalContext"])
        metadata = list((self.root / ".local/agent/runtime/test-work/root-session/agents").glob("*.json"))
        self.assertEqual({path.stem for path in metadata}, {"agent-one", "agent-two"})

    def test_missing_report_blocks_once_then_fails_open(self):
        self.initialize()
        self.hook("SubagentStart", agent_id="agent-one", prompt="WORK_TICKET_ID=T1")
        first = self.hook("SubagentStop", agent_id="agent-one", stop_hook_active=False)
        self.assertEqual(first["decision"], "block")
        second = self.hook("SubagentStop", agent_id="agent-one", stop_hook_active=True)
        self.assertNotIn("decision", second)
        self.assertTrue((self.root / ".local/agent/runtime/test-work/unclean-session.json").exists())

    def test_valid_worker_report_marks_checkpoint_required(self):
        self.initialize()
        self.hook("SubagentStart", agent_id="agent-one", prompt="WORK_TICKET_ID=T1")
        self.write_report()
        output = self.hook("SubagentStop", agent_id="agent-one")
        self.assertIn("Validated worker report", output["systemMessage"])
        status = json.loads(self.run_helper("status", "--json").stdout)
        self.assertTrue(any("needs root assimilation" in item for item in status["dirty_reasons"]))
        self.ledger().write_text(self.ledger().read_text(encoding="utf-8") + "\nAssimilated T1.\n", encoding="utf-8")
        self.run_helper(
            "checkpoint",
            "--reason",
            "worker-report-assimilated",
            "--next-action",
            "Review",
        )
        clean = json.loads(self.run_helper("status", "--json").stdout)
        self.assertEqual(clean["dirty_reasons"], [])

    def test_report_identity_and_required_headings_are_validated(self):
        self.initialize()
        report = self.write_report(work="wrong-work", empty_heading="Validation")
        result = self.run_helper(
            "validate",
            "--report",
            str(report),
            "--work-id",
            "test-work",
            "--agent-id",
            "agent-one",
            "--ticket-id",
            "T1",
            "--json",
            expected=1,
        )
        self.assertIn("work_id does not match", result.stderr)
        self.assertIn("heading is empty: Validation", result.stderr)

    def test_manual_precompact_blocks_once_and_checkpoint_repairs(self):
        self.initialize()
        self.ledger().write_text(self.ledger().read_text(encoding="utf-8") + "\nDirty.\n", encoding="utf-8")
        blocked = self.hook("PreCompact", trigger="manual", stop_hook_active=False)
        self.assertEqual(blocked["decision"], "block")
        self.run_helper(
            "checkpoint",
            "--reason",
            "before-manual-compact",
            "--next-action",
            "Compact",
        )
        self.assertIsNone(self.hook("PreCompact", trigger="manual"))

    def test_automatic_precompact_never_blocks_and_marks_unclean(self):
        self.initialize()
        self.ledger().write_text(self.ledger().read_text(encoding="utf-8") + "\nDirty.\n", encoding="utf-8")
        output = self.hook("PreCompact", trigger="auto")
        self.assertNotIn("decision", output)
        self.assertTrue((self.root / ".local/agent/runtime/test-work/unclean-session.json").exists())
        resumed = self.hook("SessionStart", source="compact")
        self.assertIn("uncheckpointed", resumed["hookSpecificOutput"]["additionalContext"])

    def test_dirty_stop_blocks_once_and_session_end_never_blocks(self):
        self.initialize()
        self.ledger().write_text(self.ledger().read_text(encoding="utf-8") + "\nDirty.\n", encoding="utf-8")
        first = self.hook("Stop", turn_id="turn-one", stop_hook_active=False)
        self.assertEqual(first["decision"], "block")
        second = self.hook("Stop", turn_id="turn-one", stop_hook_active=True)
        self.assertNotIn("decision", second)
        ended = self.hook("SessionEnd")
        self.assertNotIn("decision", ended)

    def test_dirty_task_completed_blocks_with_exit_code_two_once(self):
        self.initialize()
        self.ledger().write_text(self.ledger().read_text(encoding="utf-8") + "\nDirty.\n", encoding="utf-8")
        payload = {
            "hook_event_name": "TaskCompleted",
            "cwd": str(self.root),
            "session_id": "root-session",
            "task_id": "task-one",
            "task_subject": "Finish implementation",
        }
        first = self.run_helper("hook", "--harness", "claude", payload=payload, expected=2)
        self.assertEqual(first.stdout, "")
        self.assertIn("Checkpoint required before completing the task", first.stderr)
        other_task = {**payload, "task_id": "task-two", "task_subject": "Finish review"}
        other = self.run_helper("hook", "--harness", "claude", payload=other_task, expected=2)
        self.assertIn("Checkpoint required before completing the task", other.stderr)
        second = self.run_helper("hook", "--harness", "claude", payload=payload)
        self.assertIn("Task is completing with uncheckpointed work", json.loads(second.stdout)["systemMessage"])

    def test_worker_ledger_mutation_is_reported_not_repaired(self):
        self.initialize()
        self.hook("SubagentStart", agent_id="agent-one", prompt="WORK_TICKET_ID=T1")
        self.write_report()
        self.ledger().write_text(self.ledger().read_text(encoding="utf-8") + "\nWorker mutation.\n", encoding="utf-8")
        output = self.hook("SubagentStop", agent_id="agent-one")
        self.assertEqual(output["decision"], "block")
        self.assertIn("main ledger changed", output["reason"])
        self.assertIn("Worker mutation", self.ledger().read_text(encoding="utf-8"))

    def test_concurrent_subagent_starts_preserve_json_state(self):
        self.initialize()

        def start(index):
            payload = {
                "hook_event_name": "SubagentStart",
                "cwd": str(self.root),
                "session_id": "parallel-session",
                "agent_id": f"agent-{index}",
                "ticket_id": f"T{index}",
            }
            return self.run_helper("hook", "--harness", "codex", payload=payload)

        with ThreadPoolExecutor(max_workers=10) as pool:
            list(pool.map(start, range(10)))
        metadata = list((self.root / ".local/agent/runtime/test-work/parallel-session/agents").glob("*.json"))
        self.assertEqual(len(metadata), 10)
        for path in metadata:
            json.loads(path.read_text(encoding="utf-8"))

    def test_automatic_stop_rejects_wrong_ticket(self):
        self.initialize()
        self.hook("SubagentStart", agent_id="agent-one", ticket_id="T1")
        self.write_report(ticket="WRONG-TICKET")
        output = self.hook("SubagentStop", agent_id="agent-one")
        self.assertEqual(output["decision"], "block")
        self.assertIn("ticket_id does not match expected T1", output["reason"])

    def test_root_explicit_validation_rejects_wrong_ticket_when_hook_input_omits_assignment(self):
        self.initialize()
        self.hook("SubagentStart", agent_id="agent-one")
        report = self.write_report(ticket="WRONG-TICKET")
        automatic = self.hook("SubagentStop", agent_id="agent-one")
        self.assertIn("must also validate the expected ticket_id", automatic["systemMessage"])
        manual = self.run_helper(
            "validate",
            "--report",
            str(report),
            "--work-id",
            "test-work",
            "--agent-id",
            "agent-one",
            "--ticket-id",
            "T1",
            "--json",
            expected=1,
        )
        self.assertIn("ticket_id does not match expected T1", manual.stderr)

    def test_stop_receipt_is_stable_across_multiple_dirty_edits(self):
        self.initialize()
        self.ledger().write_text(self.ledger().read_text(encoding="utf-8") + "\nDirty one.\n", encoding="utf-8")
        first = self.hook("Stop", stop_hook_active=False)
        self.assertEqual(first["decision"], "block")
        self.ledger().write_text(self.ledger().read_text(encoding="utf-8") + "\nDirty two.\n", encoding="utf-8")
        second = self.hook("Stop", stop_hook_active=False)
        self.assertNotIn("decision", second)

    def test_subagent_stop_without_start_leaves_unclean_marker(self):
        self.initialize()
        output = self.hook("SubagentStop", agent_id="unknown-agent")
        self.assertNotIn("decision", output)
        marker = self.root / ".local/agent/runtime/test-work/unclean-session.json"
        self.assertTrue(marker.exists())
        self.assertIn("no SubagentStart metadata", marker.read_text(encoding="utf-8"))

    def test_duplicate_frontmatter_key_is_rejected(self):
        self.initialize()
        report = self.write_report()
        text = report.read_text(encoding="utf-8").replace("status: done\n", "status: done\nstatus: partial\n")
        report.write_text(text, encoding="utf-8")
        result = self.run_helper("validate", "--report", str(report), "--json", expected=1)
        self.assertIn("duplicate worker report frontmatter key: status", result.stderr)


if __name__ == "__main__":
    unittest.main()
