from __future__ import annotations

import json
import os
import stat
import tempfile
import unittest
from contextlib import redirect_stderr, redirect_stdout
from io import StringIO
from pathlib import Path

from ai_agent_workflow.config_cli import main as config_main
from ai_agent_workflow.config_policy import CodexConfigManager, ConfigError


class CodexConfigManagerTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name)
        self.policy = self.root / "model-policy.json"
        self.policy.write_text(
            json.dumps(
                {
                    "schema": "agent-model-policy/v1",
                    "default": {
                        "model": "gpt-5.6-luna",
                        "reasoning_effort": "max",
                    },
                    "exceptional_arbiter": {
                        "model": "gpt-5.6-sol",
                        "reasoning_effort": "high",
                    },
                    "automatic_fallback": False,
                    "config_apply": "explicit-user-command-only",
                    "runtime_compatibility": "requires-fresh-codex-validation",
                }
            )
            + "\n"
        )
        self.config = self.root / ".codex" / "config.toml"
        self.manager = CodexConfigManager(self.config, self.policy)

    def tearDown(self) -> None:
        self.temp.cleanup()

    def test_plan_apply_and_rollback_preserve_unmanaged_content(self) -> None:
        self.config.parent.mkdir()
        original = (
            "# app-owned comment\n"
            'model = "gpt-5.6-sol"   # managed model\n'
            'model_reasoning_effort = "xhigh"\n'
            'unknown_key = "keep-me"\n'
            "\n"
            '[projects."/work/private"]\n'
            'trust_level = "trusted"\n'
            "\n"
            "[notice]\n"
            'hide_model_migration_prompt = true\n'
        )
        self.config.write_text(original)
        os.chmod(self.config, 0o600)

        plan = self.manager.plan()
        self.assertTrue(plan["changed"])
        self.assertEqual(
            plan["changed_keys"], ["model", "model_reasoning_effort"]
        )
        self.assertNotIn("keep-me", json.dumps(plan))
        with self.assertRaisesRegex(ConfigError, "runtime compatibility"):
            self.manager.apply(
                expected_before_digest=plan["before_digest"],
                runtime_compatibility_confirmed=False,
            )

        receipt = self.manager.apply(
            expected_before_digest=plan["before_digest"],
            runtime_compatibility_confirmed=True,
            timestamp="20260903T200000+0900",
        )
        updated = self.config.read_text()
        self.assertIn('model = "gpt-5.6-luna"   # managed model\n', updated)
        self.assertIn('model_reasoning_effort = "max"\n', updated)
        self.assertIn('unknown_key = "keep-me"\n', updated)
        self.assertIn('[projects."/work/private"]\ntrust_level = "trusted"\n', updated)
        self.assertEqual(stat.S_IMODE(self.config.stat().st_mode), 0o600)

        backup = Path(receipt["backup_path"])
        self.assertEqual(backup.read_text(), original)
        self.assertEqual(stat.S_IMODE(backup.stat().st_mode), 0o600)
        doctor = self.manager.doctor()
        self.assertTrue(doctor["matches_policy"])
        self.assertNotIn("keep-me", json.dumps(doctor))

        rollback = self.manager.rollback(
            backup,
            expected_current_digest=receipt["after_digest"],
            expected_current_identity=receipt["after_identity"],
        )
        self.assertEqual(self.config.read_text(), original)
        self.assertEqual(rollback["restored_digest"], plan["before_digest"])

    def test_apply_stops_on_concurrent_change(self) -> None:
        self.config.parent.mkdir()
        self.config.write_text('model = "gpt-5.6-sol"\n')
        os.chmod(self.config, 0o600)
        plan = self.manager.plan()
        self.config.write_text('model = "gpt-5.6-sol"\nnew_app_key = true\n')

        with self.assertRaisesRegex(ConfigError, "changed after plan"):
            self.manager.apply(
                expected_before_digest=plan["before_digest"],
                runtime_compatibility_confirmed=True,
            )

    def test_apply_revalidates_bytes_immediately_before_publish(self) -> None:
        self.config.parent.mkdir()
        self.config.write_text('model = "gpt-5.6-sol"\n')
        os.chmod(self.config, 0o600)
        plan = self.manager.plan()
        backup = self.config.with_name(
            self.config.name + ".backup.20260903T200010+0900"
        )

        def interleave(operation: str, _path: Path) -> None:
            if operation == "config-apply":
                self.config.write_text(
                    'model = "gpt-5.6-sol"\nconcurrent_unknown = true\n'
                )

        self.manager._before_commit = interleave  # type: ignore[method-assign]
        with self.assertRaisesRegex(ConfigError, "changed during apply"):
            self.manager.apply(
                expected_before_digest=plan["before_digest"],
                runtime_compatibility_confirmed=True,
                timestamp="20260903T200010+0900",
            )
        self.assertIn("concurrent_unknown = true", self.config.read_text())
        self.assertFalse(backup.exists())

    def test_apply_revalidates_identity_even_when_bytes_match(self) -> None:
        self.config.parent.mkdir()
        original = b'model = "gpt-5.6-sol"\n'
        self.config.write_bytes(original)
        os.chmod(self.config, 0o600)
        plan = self.manager.plan()
        original_inode = self.config.stat().st_ino

        def interleave(operation: str, _path: Path) -> None:
            if operation == "config-apply":
                replacement = self.root / "same-bytes-replacement"
                replacement.write_bytes(original)
                os.chmod(replacement, 0o600)
                os.replace(replacement, self.config)

        self.manager._before_commit = interleave  # type: ignore[method-assign]
        with self.assertRaisesRegex(ConfigError, "identity changed during apply"):
            self.manager.apply(
                expected_before_digest=plan["before_digest"],
                runtime_compatibility_confirmed=True,
                timestamp="20260903T200020+0900",
            )
        self.assertEqual(self.config.read_bytes(), original)
        self.assertNotEqual(self.config.stat().st_ino, original_inode)

    def test_backup_destination_is_atomic_non_overwrite(self) -> None:
        self.config.parent.mkdir()
        self.config.write_text('model = "gpt-5.6-sol"\n')
        os.chmod(self.config, 0o600)
        plan = self.manager.plan()
        other = b"independent backup owner\n"
        backup = self.config.with_name(
            self.config.name + ".backup.20260903T200030+0900"
        )

        def interleave(operation: str, path: Path) -> None:
            if operation == "backup-create":
                self.assertEqual(path, backup)
                path.write_bytes(other)

        self.manager._before_commit = interleave  # type: ignore[method-assign]
        with self.assertRaisesRegex(ConfigError, "backup path already exists"):
            self.manager.apply(
                expected_before_digest=plan["before_digest"],
                runtime_compatibility_confirmed=True,
                timestamp="20260903T200030+0900",
            )
        self.assertEqual(backup.read_bytes(), other)
        self.assertEqual(self.config.read_text(), 'model = "gpt-5.6-sol"\n')

    def test_rollback_revalidates_bytes_and_identity_before_publish(self) -> None:
        self.config.parent.mkdir()
        self.config.write_text('model = "gpt-5.6-sol"\n')
        os.chmod(self.config, 0o600)
        plan = self.manager.plan()
        receipt = self.manager.apply(
            expected_before_digest=plan["before_digest"],
            runtime_compatibility_confirmed=True,
            timestamp="20260903T200040+0900",
        )
        backup = Path(receipt["backup_path"])

        def interleave(operation: str, _path: Path) -> None:
            if operation == "config-rollback":
                self.config.write_text(
                    'model = "gpt-5.6-luna"\nconcurrent_unknown = true\n'
                )

        self.manager._before_commit = interleave  # type: ignore[method-assign]
        with self.assertRaisesRegex(ConfigError, "changed during rollback"):
            self.manager.rollback(
                backup,
                expected_current_digest=receipt["after_digest"],
                expected_current_identity=receipt["after_identity"],
            )
        self.assertIn("concurrent_unknown = true", self.config.read_text())
        self.assertTrue(backup.exists())

    def test_missing_config_creates_only_portable_keys_as_mode_0600(self) -> None:
        plan = self.manager.plan()
        self.assertFalse(plan["exists"])
        receipt = self.manager.apply(
            expected_before_digest=plan["before_digest"],
            runtime_compatibility_confirmed=True,
            timestamp="20260903T200100+0900",
        )
        self.assertIsNone(receipt["backup_path"])
        self.assertEqual(
            self.config.read_text(),
            'model = "gpt-5.6-luna"\nmodel_reasoning_effort = "max"\n',
        )
        self.assertEqual(stat.S_IMODE(self.config.stat().st_mode), 0o600)
        self.assertEqual(receipt["rollback_action"]["kind"], "restore_absent")
        rollback = self.manager.rollback_absent(
            expected_current_digest=receipt["after_digest"],
            expected_current_identity=receipt["after_identity"],
        )
        self.assertEqual(rollback["restored_state"], "absent")
        self.assertFalse(self.config.exists())

    def test_missing_config_create_does_not_overwrite_concurrent_creator(self) -> None:
        plan = self.manager.plan()
        concurrent = b'private_app_key = "keep"\n'

        def interleave(operation: str, _path: Path) -> None:
            if operation == "config-create":
                self.config.parent.mkdir(parents=True, exist_ok=True)
                self.config.write_bytes(concurrent)
                os.chmod(self.config, 0o600)

        self.manager._before_commit = interleave  # type: ignore[method-assign]
        with self.assertRaisesRegex(ConfigError, "appeared during apply"):
            self.manager.apply(
                expected_before_digest=plan["before_digest"],
                runtime_compatibility_confirmed=True,
            )
        self.assertEqual(self.config.read_bytes(), concurrent)

    def test_absent_rollback_stops_if_created_file_changed(self) -> None:
        plan = self.manager.plan()
        receipt = self.manager.apply(
            expected_before_digest=plan["before_digest"],
            runtime_compatibility_confirmed=True,
        )
        concurrent = b'model = "gpt-5.6-luna"\nconcurrent_unknown = true\n'

        def interleave(operation: str, _path: Path) -> None:
            if operation == "config-remove":
                self.config.write_bytes(concurrent)

        self.manager._before_commit = interleave  # type: ignore[method-assign]
        with self.assertRaisesRegex(ConfigError, "changed during absent rollback"):
            self.manager.rollback_absent(
                expected_current_digest=receipt["after_digest"],
                expected_current_identity=receipt["after_identity"],
            )
        self.assertEqual(self.config.read_bytes(), concurrent)

    def test_multiline_toml_is_rejected_without_modification(self) -> None:
        self.config.parent.mkdir()
        original = (
            'banner = """\n'
            "[not-a-table]\n"
            '"""\n'
            'model = "gpt-5.6-sol"\n'
            'model_reasoning_effort = "xhigh"\n'
        )
        self.config.write_text(original)
        os.chmod(self.config, 0o600)

        with self.assertRaisesRegex(ConfigError, "multiline TOML strings"):
            self.manager.plan()
        self.assertEqual(self.config.read_text(), original)

    def test_symlink_is_rejected_without_touching_target(self) -> None:
        target = self.root / "real-config.toml"
        target.write_text('model = "gpt-5.6-sol"\n')
        self.config.parent.mkdir()
        self.config.symlink_to(target)

        with self.assertRaisesRegex(ConfigError, "symlink"):
            self.manager.plan()
        self.assertEqual(target.read_text(), 'model = "gpt-5.6-sol"\n')

    def test_cli_plan_redacts_unmanaged_values_and_apply_requires_confirmation(self) -> None:
        self.config.parent.mkdir()
        self.config.write_text('model = "gpt-5.6-sol"\nprivate_token = "do-not-print"\n')
        os.chmod(self.config, 0o600)
        output = StringIO()
        with redirect_stdout(output):
            result = config_main(
                ["--policy", str(self.policy), "--config", str(self.config), "plan"]
            )
        self.assertEqual(result, 0)
        self.assertNotIn("do-not-print", output.getvalue())
        plan = json.loads(output.getvalue())

        error = StringIO()
        with redirect_stderr(error):
            result = config_main(
                [
                    "--policy",
                    str(self.policy),
                    "--config",
                    str(self.config),
                    "apply",
                    "--expected-before-digest",
                    plan["before_digest"],
                ]
            )
        self.assertEqual(result, 2)
        self.assertIn("runtime compatibility", error.getvalue())
        self.assertIn("do-not-print", self.config.read_text())

    def test_cli_can_restore_absent_pre_state_under_digest_and_identity(self) -> None:
        plan = self.manager.plan()
        output = StringIO()
        with redirect_stdout(output):
            result = config_main(
                [
                    "--policy",
                    str(self.policy),
                    "--config",
                    str(self.config),
                    "apply",
                    "--expected-before-digest",
                    plan["before_digest"],
                    "--confirm-runtime-compatible",
                ]
            )
        self.assertEqual(result, 0)
        receipt = json.loads(output.getvalue())

        output = StringIO()
        with redirect_stdout(output):
            result = config_main(
                [
                    "--policy",
                    str(self.policy),
                    "--config",
                    str(self.config),
                    "rollback",
                    "--restore-absent",
                    "--expected-current-digest",
                    receipt["after_digest"],
                    "--expected-current-identity",
                    receipt["after_identity"],
                ]
            )
        self.assertEqual(result, 0)
        self.assertEqual(json.loads(output.getvalue())["restored_state"], "absent")
        self.assertFalse(self.config.exists())


if __name__ == "__main__":
    unittest.main()
