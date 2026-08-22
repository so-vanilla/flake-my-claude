from __future__ import annotations

import importlib.util
import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock


MODULE_PATH = Path(__file__).resolve().parents[1] / "scripts" / "agent-workflow-init.py"
SPEC = importlib.util.spec_from_file_location("agent_workflow_init", MODULE_PATH)
assert SPEC and SPEC.loader
workflow_init = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = workflow_init
SPEC.loader.exec_module(workflow_init)


class FixtureRunner(workflow_init.CommandRunner):
    """Use real local Git while faking harnesses and the Claude marketplace."""

    def __init__(
        self,
        *,
        missing: str | None = None,
        doctor_exit: int = 0,
        plugin_present: bool = True,
        plugin_project_enabled: bool = True,
    ) -> None:
        self.missing = missing
        self.doctor_exit = doctor_exit
        self.plugin_present = plugin_present
        self.plugin_project_enabled = plugin_project_enabled
        self.calls: list[tuple[tuple[str, ...], Path | None]] = []

    def run(self, argv, *, cwd=None):
        command = tuple(argv)
        self.calls.append((command, cwd))
        executable = command[0]
        if executable == self.missing:
            return workflow_init.CommandResult(127, stderr=f"{executable}: not found")
        if command == ("codex", "--version"):
            return workflow_init.CommandResult(0, stdout="codex 0.145.0\n")
        if command == ("claude", "--version"):
            return workflow_init.CommandResult(0, stdout="claude 1.2.3\n")
        if command == ("bun", "--version"):
            return workflow_init.CommandResult(0, stdout="1.2.0\n")
        if command[-1:] == ("--doctor",):
            return workflow_init.CommandResult(self.doctor_exit, stderr="fixture doctor failure")
        if command == (
            "claude",
            "plugin",
            "install",
            "superpowers@claude-plugins-official",
            "--scope",
            "project",
        ):
            assert cwd is not None
            settings = cwd / ".claude" / "settings.json"
            settings.parent.mkdir(parents=True, exist_ok=True)
            enabled = (
                {"superpowers@claude-plugins-official": True}
                if self.plugin_project_enabled
                else {"unrelated@example": True}
            )
            settings.write_text(json.dumps({"enabledPlugins": enabled}) + "\n", encoding="utf-8")
            return workflow_init.CommandResult(0, stdout="installed\n")
        if command == ("claude", "plugin", "list", "--json"):
            return workflow_init.CommandResult(
                0,
                stdout=json.dumps(
                    [{"id": "superpowers@claude-plugins-official"}]
                    if self.plugin_present
                    else []
                ),
            )
        return super().run(argv, cwd=cwd)


class AgentWorkflowInitTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory(prefix="agent-workflow-init-test-")
        self.root = Path(self.temporary.name)
        self.source_root = self.root / "initializer-source"
        self.upstream = self.root / "upstream"
        self.remote = self.root / "upstream.git"
        self.source_root.mkdir()
        self._write_templates()
        self._write_upstream()
        self._git(self.upstream, "init", "-q", "--initial-branch=main")
        self._git(self.upstream, "add", "-f", ".")
        self._git(
            self.upstream,
            "-c",
            "user.name=Fixture",
            "-c",
            "user.email=fixture@example.invalid",
            "commit",
            "-qm",
            "fixture upstream",
        )
        self._git(self.root, "init", "-q", "--bare", str(self.remote))
        self._git(self.upstream, "remote", "add", "origin", str(self.remote))
        self._git(self.upstream, "push", "-q", "origin", "main")
        self._git(self.upstream, "push", "-q", "origin", "main:refs/heads/v2")
        self.manifest_path = self._write_manifest()

    def tearDown(self) -> None:
        self.temporary.cleanup()

    @staticmethod
    def _git(cwd: Path, *args: str) -> str:
        completed = subprocess.run(
            ["git", *args], cwd=cwd, check=True, text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE
        )
        return completed.stdout

    def _write_templates(self) -> None:
        for harness in ("codex", "claude"):
            skill = self.source_root / "templates" / "project-workflow" / harness / "workflow-status"
            skill.mkdir(parents=True)
            (skill / "SKILL.md").write_text(f"# {harness} workflow status\n", encoding="utf-8")
        shared = self.source_root / "templates" / "project-workflow" / "shared" / "workflow-status"
        shared.mkdir(parents=True)
        (shared / "phases.json").write_text('{"schema":"fixture"}\n', encoding="utf-8")

    def _write_upstream(self) -> None:
        files = {
            "dist/codex/.codex/config.toml": 'model_provider = "amazon-bedrock"\nmodel = "openai.gpt-5.5"\n',
            "dist/codex/.codex/tools/aidlc.ts": "// fixture\n",
            "dist/codex/.agents/skills/aidlc/SKILL.md": "# aidlc\n",
            "dist/codex/aidlc/active-space": "default\n",
            "dist/codex/AGENTS.md": "# AI-DLC\n",
            "dist/codex/.gitignore": "aidlc/.aidlc-sessions/\n",
            "dist/claude/.claude/settings.json": '{"env":{"MODEL_PROVIDER":"bedrock"}}\n',
            "dist/claude/.claude/tools/aidlc.ts": "// fixture\n",
            "dist/claude/aidlc/active-space": "default\n",
            "skills/brainstorming/SKILL.md": "# brainstorming\n",
            "skills/test-driven-development/SKILL.md": "# tdd\n",
        }
        for relative, contents in files.items():
            path = self.upstream / relative
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(contents, encoding="utf-8")

    def _write_manifest(self) -> Path:
        manifest = {
            "schema": "project-workflows/v1",
            "selection_state": ".local/agent/workflow-selection.json",
            "workflows": {
                "aidlc": {
                    "display_name": "AI-DLC",
                    "upstream": {"repository": str(self.remote), "ref": "refs/heads/v2"},
                    "selections": {
                        "codex": {
                            "label": "official AI-DLC Codex distribution",
                            "payload": [
                                "dist/codex/.codex",
                                "dist/codex/.agents",
                                "dist/codex/aidlc",
                                "dist/codex/AGENTS.md",
                                "dist/codex/.gitignore",
                            ],
                            "payload_targets": {
                                "dist/codex/.codex": ".codex",
                                "dist/codex/.agents": ".agents",
                                "dist/codex/aidlc": "aidlc",
                                "dist/codex/AGENTS.md": "AGENTS.md",
                                "dist/codex/.gitignore": ".gitignore",
                            },
                            "workflow_status_template": "templates/project-workflow/codex/workflow-status",
                            "workflow_status_target": ".agents/skills/workflow-status",
                            "workflow_status_shared": "templates/project-workflow/shared/workflow-status/phases.json",
                            "requirements": [
                                {"command": "codex", "minimum_version": "0.145.0"},
                                {"command": "bun"},
                            ],
                            "model_settings": ["dist/codex/.codex/config.toml"],
                            "doctor": ["bun", ".codex/tools/aidlc.ts", "--doctor"],
                            "manual_gates": [],
                            "completion_gates": ["Confirm Codex hook trust."],
                        },
                        "claude": {
                            "label": "official AI-DLC Claude Code distribution",
                            "payload": ["dist/claude/.claude", "dist/claude/aidlc"],
                            "payload_targets": {
                                "dist/claude/.claude": ".claude",
                                "dist/claude/aidlc": "aidlc",
                            },
                            "workflow_status_template": "templates/project-workflow/claude/workflow-status",
                            "workflow_status_target": ".claude/skills/workflow-status",
                            "workflow_status_shared": "templates/project-workflow/shared/workflow-status/phases.json",
                            "requirements": [{"command": "claude"}, {"command": "bun"}],
                            "model_settings": ["dist/claude/.claude/settings.json"],
                            "doctor": ["bun", ".claude/tools/aidlc.ts", "--doctor"],
                            "manual_gates": [],
                        },
                    },
                },
                "superpowers": {
                    "display_name": "Superpowers",
                    "upstream": {"repository": str(self.remote), "ref": "refs/heads/main"},
                    "selections": {
                        "codex": {
                            "label": "approved adapter",
                            "payload": ["skills"],
                            "payload_targets": {"skills": ".agents/skills/superpowers"},
                            "workflow_status_template": "templates/project-workflow/codex/workflow-status",
                            "workflow_status_target": ".agents/skills/workflow-status",
                            "workflow_status_shared": "templates/project-workflow/shared/workflow-status/phases.json",
                            "requirements": [{"command": "codex"}],
                            "adapter": "project-local-skills-copy",
                            "manual_gates": [],
                            "completion_gates": ["Fresh Codex discovery required."],
                        },
                        "claude": {
                            "label": "official project plugin",
                            "payload": [],
                            "workflow_status_template": "templates/project-workflow/claude/workflow-status",
                            "workflow_status_target": ".claude/skills/workflow-status",
                            "workflow_status_shared": "templates/project-workflow/shared/workflow-status/phases.json",
                            "requirements": [{"command": "claude"}],
                            "project_cli": [
                                "claude",
                                "plugin",
                                "install",
                                "superpowers@claude-plugins-official",
                                "--scope",
                                "project",
                            ],
                            "project_settings": ".claude/settings.json",
                            "plugin_inventory": ["claude", "plugin", "list", "--json"],
                            "manual_gates": ["Cache is user-local."],
                        },
                    },
                },
            },
        }
        path = self.source_root / "manifest.json"
        path.write_text(json.dumps(manifest), encoding="utf-8")
        return path

    def _target(self, name: str) -> Path:
        target = self.root / name
        target.mkdir()
        self._git(target, "init", "-q", "--initial-branch=main")
        return target

    def _initializer(self, **kwargs):
        return workflow_init.WorkflowInitializer(
            manifest_path=self.manifest_path,
            source_root=self.source_root,
            runner=kwargs.pop("runner", FixtureRunner()),
            **kwargs,
        )

    def _options(self, target: Path, agent: str, workflow: str, **kwargs):
        return workflow_init.RunOptions(
            workflow_init.Selection(agent=agent, workflow=workflow), target, **kwargs
        )

    def test_interactive_selection_has_no_defaults(self) -> None:
        answers = iter(["codex", "superpowers"])
        selection = workflow_init.choose_selection(
            None, None, interactive=True, input_func=lambda _prompt: next(answers)
        )
        self.assertEqual(selection, workflow_init.Selection("codex", "superpowers"))
        with self.assertRaises(workflow_init.InputRequired):
            workflow_init.choose_selection(None, None, interactive=False)

    def test_every_selection_dry_runs_with_complete_preview_and_no_target_writes(self) -> None:
        for agent, workflow in (
            ("codex", "aidlc"),
            ("claude", "aidlc"),
            ("codex", "superpowers"),
            ("claude", "superpowers"),
        ):
            with self.subTest(agent=agent, workflow=workflow):
                target = self._target(f"dry {agent} {workflow}")
                (target / ".git/info/exclude").write_text("/.local/\n", encoding="utf-8")
                result = self._initializer().run(
                    self._options(target, agent, workflow, dry_run=True, yes=True)
                )
                self.assertEqual(result.action, "dry-run")
                rendered = "\n".join(result.messages)
                self.assertIn("Resolved upstream commit:", rendered)
                self.assertIn("WRITE .local/agent/workflow-selection.json", rendered)
                self.assertIn("WRITE .local/agent/workflow-selection.json [IGNORED", rendered)
                if agent == "codex":
                    self.assertIn("TRACKED/trackable", rendered)
                if workflow == "aidlc":
                    self.assertIn("MODEL/PROVIDER PAYLOAD", rendered)
                if (agent, workflow) == ("codex", "aidlc"):
                    self.assertIn("WRITE .codex/config.toml", rendered)
                if (agent, workflow) == ("claude", "aidlc"):
                    self.assertIn("WRITE .claude/settings.json", rendered)
                if (agent, workflow) == ("claude", "superpowers"):
                    self.assertIn("CLI-OWNED PROJECT OPERATION", rendered)
                if agent == "codex":
                    self.assertIn("COMPLETION GATE/INCOMPLETE", rendered)
                self.assertFalse((target / ".local").exists())
                self.assertFalse((target / ".agents").exists())
                self.assertFalse((target / ".claude").exists())

    def test_fully_qualified_v2_branch_resolves_then_uses_detached_switch(self) -> None:
        target = self._target("qualified v2")
        runner = FixtureRunner()
        result = self._initializer(runner=runner).run(
            self._options(target, "claude", "aidlc", dry_run=True, yes=True)
        )
        self.assertEqual(result.action, "dry-run")
        git_commands = [call[0] for call in runner.calls if call[0][0] == "git"]
        self.assertTrue(
            any(
                "rev-parse" in command
                and "refs/remotes/origin/v2^{commit}" in command
                for command in git_commands
            )
        )
        switches = [command for command in git_commands if "switch" in command]
        self.assertEqual(len(switches), 1)
        self.assertIn("--detach", switches[0])
        self.assertRegex(switches[0][-1], r"^[0-9a-f]{40}$")
        self.assertFalse(any("checkout" in command for command in git_commands))

    def test_codex_superpowers_install_target_with_spaces_and_idempotence(self) -> None:
        target = self._target("target with spaces")
        initializer = self._initializer()
        first = initializer.run(self._options(target, "codex", "superpowers", yes=True))
        self.assertEqual(first.action, "incomplete")
        self.assertEqual(first.exit_code, 1)
        self.assertTrue((target / ".agents/skills/superpowers/brainstorming/SKILL.md").is_file())
        self.assertTrue((target / ".agents/skills/workflow-status/phases.json").is_file())
        state = json.loads((target / ".local/agent/workflow-selection.json").read_text())
        self.assertEqual(state["adapter"], "project-local-skills-copy")
        self.assertEqual(state["agent"], "codex")
        self.assertEqual(state["workflow"], "superpowers")
        before = sorted(path.relative_to(target).as_posix() for path in target.rglob("*") if path.is_file())
        second = initializer.run(self._options(target, "codex", "superpowers", yes=True))
        after = sorted(path.relative_to(target).as_posix() for path in target.rglob("*") if path.is_file())
        self.assertEqual(second.action, "incomplete")
        self.assertIn("IDEMPOTENT:", "\n".join(second.messages))
        self.assertEqual(before, after)

    def test_idempotent_rerun_stops_on_mutated_payload_bytes(self) -> None:
        target = self._target("payload drift")
        initializer = self._initializer()
        initializer.run(self._options(target, "codex", "superpowers", yes=True))
        payload = target / ".agents/skills/superpowers/brainstorming/SKILL.md"
        payload.write_text("# user mutation\n", encoding="utf-8")
        with self.assertRaisesRegex(workflow_init.InitializerError, "installation drift detected"):
            initializer.run(self._options(target, "codex", "superpowers", yes=True))
        self.assertEqual(payload.read_text(encoding="utf-8"), "# user mutation\n")

    def test_transaction_re_reads_written_bytes_before_reporting_success(self) -> None:
        target = self._target("post transaction verification")
        original_link = workflow_init.os.link
        state_suffix = ".local/agent/workflow-selection.json"
        payload = target / ".agents/skills/superpowers/brainstorming/SKILL.md"

        def tamper_after_final_link(source, destination, **kwargs):
            result = original_link(source, destination, **kwargs)
            if str(source).endswith(state_suffix):
                payload.write_text("# concurrent mutation\n", encoding="utf-8")
            return result

        with mock.patch.object(workflow_init.os, "link", side_effect=tamper_after_final_link):
            with self.assertRaisesRegex(workflow_init.InitializerError, "post-transaction byte verification"):
                self._initializer().run(self._options(target, "codex", "superpowers", yes=True))
        self.assertFalse((target / ".agents").exists())
        self.assertFalse((target / ".local").exists())

    def test_codex_aidlc_copies_official_settings_and_runs_doctor(self) -> None:
        target = self._target("codex aidlc")
        runner = FixtureRunner()
        result = self._initializer(runner=runner).run(
            self._options(target, "codex", "aidlc", yes=True)
        )
        self.assertEqual(result.action, "incomplete")
        self.assertEqual(result.exit_code, 1)
        self.assertEqual(
            (target / ".codex/config.toml").read_text(encoding="utf-8"),
            'model_provider = "amazon-bedrock"\nmodel = "openai.gpt-5.5"\n',
        )
        self.assertTrue((target / "AGENTS.md").is_file())
        self.assertTrue((target / ".gitignore").is_file())
        self.assertTrue(
            any(call[0] == ("bun", ".codex/tools/aidlc.ts", "--doctor") for call in runner.calls)
        )
        self.assertIn("INCOMPLETE: Confirm Codex hook trust.", "\n".join(result.messages))

    def test_aidlc_idempotent_rerun_repeats_doctor_and_can_recover(self) -> None:
        target = self._target("doctor rerun")
        runner = FixtureRunner(doctor_exit=1)
        initializer = self._initializer(runner=runner)
        first = initializer.run(self._options(target, "claude", "aidlc", yes=True))
        self.assertEqual(first.action, "incomplete")
        runner.doctor_exit = 0
        second = initializer.run(self._options(target, "claude", "aidlc", yes=True))
        self.assertEqual(second.action, "idempotent")
        self.assertEqual(
            sum(call[0] == ("bun", ".claude/tools/aidlc.ts", "--doctor") for call in runner.calls),
            2,
        )

    def test_claude_aidlc_full_apply_and_idempotence(self) -> None:
        target = self._target("claude aidlc")
        runner = FixtureRunner()
        initializer = self._initializer(runner=runner)
        first = initializer.run(self._options(target, "claude", "aidlc", yes=True))
        self.assertEqual(first.action, "installed")
        self.assertTrue((target / ".claude/tools/aidlc.ts").is_file())
        self.assertTrue((target / ".claude/skills/workflow-status/phases.json").is_file())
        second = initializer.run(self._options(target, "claude", "aidlc", yes=True))
        self.assertEqual(second.action, "idempotent")
        self.assertEqual(
            sum(call[0] == ("bun", ".claude/tools/aidlc.ts", "--doctor") for call in runner.calls),
            2,
        )

    def test_collision_stops_before_any_target_write(self) -> None:
        target = self._target("collision")
        collision = target / ".agents/skills/superpowers/brainstorming/SKILL.md"
        collision.parent.mkdir(parents=True)
        collision.write_text("user owned\n", encoding="utf-8")
        with self.assertRaises(workflow_init.CollisionError):
            self._initializer().run(self._options(target, "codex", "superpowers", yes=True))
        self.assertEqual(collision.read_text(encoding="utf-8"), "user owned\n")
        self.assertFalse((target / ".local/agent/workflow-selection.json").exists())

    def test_different_existing_selection_stops_without_migration(self) -> None:
        target = self._target("selection conflict")
        initializer = self._initializer()
        initializer.run(self._options(target, "codex", "superpowers", yes=True))
        with self.assertRaises(workflow_init.SelectionConflict):
            initializer.run(self._options(target, "claude", "superpowers", yes=True))
        state = json.loads((target / ".local/agent/workflow-selection.json").read_text())
        self.assertEqual((state["agent"], state["workflow"]), ("codex", "superpowers"))

    def test_changed_resolved_revision_requires_explicit_upgrade(self) -> None:
        target = self._target("upgrade required")
        initializer = self._initializer()
        initializer.run(self._options(target, "codex", "superpowers", yes=True))
        state_path = target / ".local/agent/workflow-selection.json"
        state = json.loads(state_path.read_text())
        state["upstream"]["commit"] = "0" * 40
        state_path.write_text(json.dumps(state), encoding="utf-8")
        with self.assertRaises(workflow_init.UpgradeRequired):
            initializer.run(self._options(target, "codex", "superpowers", yes=True))

    def test_failed_fetch_and_missing_prerequisite_leave_no_partial_state(self) -> None:
        target = self._target("failed fetch")
        broken = json.loads(self.manifest_path.read_text())
        broken["workflows"]["superpowers"]["upstream"]["repository"] = str(self.root / "missing.git")
        broken_path = self.source_root / "broken.json"
        broken_path.write_text(json.dumps(broken), encoding="utf-8")
        with self.assertRaises(workflow_init.CommandFailure):
            workflow_init.WorkflowInitializer(
                manifest_path=broken_path, source_root=self.source_root, runner=FixtureRunner()
            ).run(self._options(target, "codex", "superpowers", yes=True))
        self.assertFalse((target / ".local").exists())
        self.assertFalse((target / ".agents").exists())

        second = self._target("missing prereq")
        with self.assertRaises(workflow_init.CommandFailure):
            self._initializer(runner=FixtureRunner(missing="bun")).run(
                self._options(second, "codex", "aidlc", yes=True)
            )
        self.assertFalse((second / ".local").exists())
        self.assertFalse((second / ".codex").exists())

    def test_claude_superpowers_uses_injectable_official_cli_not_json_rewrite(self) -> None:
        target = self._target("claude plugin")
        runner = FixtureRunner()
        result = self._initializer(runner=runner).run(
            self._options(target, "claude", "superpowers", yes=True)
        )
        self.assertEqual(result.action, "installed")
        self.assertTrue(
            any(
                call[0]
                == (
                    "claude",
                    "plugin",
                    "install",
                    "superpowers@claude-plugins-official",
                    "--scope",
                    "project",
                )
                for call in runner.calls
            )
        )
        self.assertEqual(
            json.loads((target / ".claude/settings.json").read_text())["enabledPlugins"],
            {"superpowers@claude-plugins-official": True},
        )
        self.assertTrue((target / ".claude/skills/workflow-status/phases.json").is_file())
        self.assertTrue((target / ".local/agent/workflow-selection.json").is_file())

    def test_claude_superpowers_idempotence_rechecks_inventory_without_reinstall(self) -> None:
        target = self._target("claude idempotence")
        runner = FixtureRunner()
        initializer = self._initializer(runner=runner)
        initializer.run(self._options(target, "claude", "superpowers", yes=True))
        install_command = (
            "claude",
            "plugin",
            "install",
            "superpowers@claude-plugins-official",
            "--scope",
            "project",
        )
        inventory_command = ("claude", "plugin", "list", "--json")
        installs_before = sum(call[0] == install_command for call in runner.calls)
        inventories_before = sum(call[0] == inventory_command for call in runner.calls)
        result = initializer.run(self._options(target, "claude", "superpowers", yes=True))
        self.assertEqual(result.action, "idempotent")
        self.assertEqual(sum(call[0] == install_command for call in runner.calls), installs_before)
        self.assertEqual(sum(call[0] == inventory_command for call in runner.calls), inventories_before + 1)

        runner.plugin_present = False
        with self.assertRaisesRegex(workflow_init.InitializerError, "plugin inventory"):
            initializer.run(self._options(target, "claude", "superpowers", yes=True))
        self.assertEqual(sum(call[0] == install_command for call in runner.calls), installs_before)

    def test_claude_global_inventory_cannot_substitute_for_project_enablement(self) -> None:
        target = self._target("claude project scope required")
        with self.assertRaisesRegex(workflow_init.InitializerError, "project settings do not enable"):
            self._initializer(runner=FixtureRunner(plugin_project_enabled=False)).run(
                self._options(target, "claude", "superpowers", yes=True)
            )
        self.assertFalse((target / ".claude").exists())
        self.assertFalse((target / ".local").exists())

    def test_claude_cli_inventory_failure_restores_absent_settings_and_directories(self) -> None:
        target = self._target("claude inventory rollback")
        with self.assertRaisesRegex(workflow_init.InitializerError, "plugin inventory"):
            self._initializer(runner=FixtureRunner(plugin_present=False)).run(
                self._options(target, "claude", "superpowers", yes=True)
            )
        self.assertFalse((target / ".claude").exists())
        self.assertFalse((target / ".local").exists())

    def test_claude_cli_post_operation_transaction_failure_restores_exact_settings(self) -> None:
        target = self._target("claude transaction rollback")
        settings = target / ".claude/settings.json"
        settings.parent.mkdir(parents=True)
        original_settings = b'{ "unrelated": true }\n'
        settings.write_bytes(original_settings)
        unrelated = target / ".claude/keep.txt"
        unrelated.write_text("keep\n", encoding="utf-8")
        original_link = workflow_init.os.link
        state_suffix = ".local/agent/workflow-selection.json"

        def fail_selection_state(source, destination, **kwargs):
            if str(source).endswith(state_suffix):
                raise OSError("fixture state move failure")
            return original_link(source, destination, **kwargs)

        with mock.patch.object(workflow_init.os, "link", side_effect=fail_selection_state):
            with self.assertRaisesRegex(OSError, "fixture state move failure"):
                self._initializer(runner=FixtureRunner()).run(
                    self._options(target, "claude", "superpowers", yes=True)
                )
        self.assertEqual(settings.read_bytes(), original_settings)
        self.assertEqual(unrelated.read_text(encoding="utf-8"), "keep\n")
        self.assertFalse((target / ".claude/skills").exists())
        self.assertFalse((target / ".local").exists())

    def test_concurrent_leaf_creation_is_not_overwritten_or_rolled_back(self) -> None:
        target = self._target("leaf race")
        original_link = workflow_init.os.link
        raced = False

        def create_leaf_first(source, destination, **kwargs):
            nonlocal raced
            if not raced and str(source).endswith("brainstorming/SKILL.md"):
                raced = True
                descriptor = os.open(
                    destination,
                    os.O_WRONLY | os.O_CREAT | os.O_EXCL,
                    0o600,
                    dir_fd=kwargs["dst_dir_fd"],
                )
                os.write(descriptor, b"concurrent owner\n")
                os.close(descriptor)
            return original_link(source, destination, **kwargs)

        with mock.patch.object(workflow_init.os, "link", side_effect=create_leaf_first):
            with self.assertRaisesRegex(workflow_init.CollisionError, "target changed during install"):
                self._initializer().run(
                    self._options(target, "codex", "superpowers", yes=True)
                )
        concurrent = target / ".agents/skills/superpowers/brainstorming/SKILL.md"
        self.assertEqual(concurrent.read_bytes(), b"concurrent owner\n")
        self.assertFalse((target / ".local/agent/workflow-selection.json").exists())

    def test_parent_symlink_swap_cannot_redirect_install_or_rollback(self) -> None:
        target = self._target("parent race")
        external = self.root / "external"
        external.mkdir()
        write = workflow_init.PlannedWrite(
            workflow_init.PurePosixPath("owned/nested/file.txt"),
            None,
            b"owned bytes\n",
            "race fixture",
        )
        original_link = workflow_init.os.link
        swapped = False

        def swap_parent(source, destination, **kwargs):
            nonlocal swapped
            if not swapped:
                swapped = True
                (target / "owned").rename(target / "owned-anchored")
                (target / "owned").symlink_to(external, target_is_directory=True)
            return original_link(source, destination, **kwargs)

        with mock.patch.object(workflow_init.os, "link", side_effect=swap_parent):
            with self.assertRaises(workflow_init.InitializerError):
                self._initializer()._apply_transaction(target, [write])
        self.assertEqual(list(external.iterdir()), [])
        self.assertFalse((target / "owned-anchored/nested/file.txt").exists())

    def test_noninteractive_apply_requires_yes_after_preview(self) -> None:
        target = self._target("needs yes")
        with self.assertRaises(workflow_init.InputRequired):
            self._initializer().run(self._options(target, "codex", "superpowers"))
        self.assertFalse((target / ".local").exists())


if __name__ == "__main__":
    unittest.main()
