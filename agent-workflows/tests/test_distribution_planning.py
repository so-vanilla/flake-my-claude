import json
import sys
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "src"))

from ai_agent_workflow.distribution_planning import (  # noqa: E402
    DistributionPlanner,
    DistributionPlanningError,
    NativeProjectionAdapter,
)


class DistributionPlanningTests(unittest.TestCase):
    def setUp(self):
        self.manifest = {
            "managed_sources": ["agent-workflows/src", "agent-workflows/schemas", "agent-workflows/scripts"],
            "managed_targets": [{"root": ".codex/skills", "content": "workflow-skill-directories", "mechanism": "programs.codex.skills"}],
            "app_owned_exclusions": [".codex/config.toml", ".codex/sessions", ".codex/plugins"],
        }
        self.source = [{"path": "agent-workflows/src/ai_agent_workflow/distribution_planning.py", "class": "portable_source", "version": "v1", "digest": "sha256:" + "a" * 64}]
        self.profile = {
            "path": "agent-workflows/manifests/codex-capability-profile-v1.json",
            "version": "v1",
            "provider": "codex",
            "supported_semantics": ["permissions", "skills"],
            "digest": "sha256:1ca40526d2a3b4909b3d4510994cd6e8798ee49aceb20d6479cacb366bbbb6ec",
        }

    def test_preview_is_deterministic_narrow_and_never_applies(self):
        preview = DistributionPlanner(self.manifest).preview(self.source, [{"path": ".codex/skills/distribution", "ownership": "managed", "replace": True}])
        self.assertEqual(preview["schema"], "distribution-preview/v1")
        self.assertFalse(preview["applied"])
        self.assertEqual(preview["actions"][0]["path"], ".codex/skills/distribution")
        self.assertEqual(
            preview["provenance"]["source_set_digest"],
            "sha256:3e4135dcecebe17baa2c75b20f3fa679120cd046b44d317aba5d9c09512a5010",
        )
        self.assertIn("backup", preview["requirements"])

    def test_refuses_unmanaged_secret_and_app_owned_destinations(self):
        planner = DistributionPlanner(self.manifest)
        for target in (
            {"path": ".codex/config.toml", "ownership": "managed"},
            {"path": ".codex/skills/private", "ownership": "secret"},
            {"path": ".codex/skills/private", "ownership": "unmanaged"},
        ):
            with self.subTest(target=target):
                with self.assertRaises(DistributionPlanningError):
                    planner.preview(self.source, [target])

    def test_preview_refuses_noncanonical_escaping_and_duplicate_identities(self):
        planner = DistributionPlanner(self.manifest)
        safe_target = [{"path": ".codex/skills/distribution", "ownership": "managed"}]
        for path in (
            "/agent-workflows/src/ai_agent_workflow/distribution_planning.py",
            "agent-workflows/src/../src/ai_agent_workflow/distribution_planning.py",
            "agent-workflows/src/../../private",
            "agent-workflows//src/ai_agent_workflow/distribution_planning.py",
            "agent-workflows/src\\ai_agent_workflow/distribution_planning.py",
        ):
            with self.subTest(source_path=path):
                source = [{**self.source[0], "path": path}]
                with self.assertRaises(DistributionPlanningError):
                    planner.preview(source, safe_target)

        for path in (
            "/.codex/skills/distribution",
            ".codex/skills/../config.toml",
            ".codex//skills/distribution",
            ".codex/skills/distribution/",
            ".codex\\skills\\distribution",
        ):
            with self.subTest(target_path=path):
                with self.assertRaises(DistributionPlanningError):
                    planner.preview(self.source, [{"path": path, "ownership": "managed"}])

        with self.assertRaises(DistributionPlanningError):
            planner.preview([self.source[0], dict(self.source[0])], safe_target)
        with self.assertRaises(DistributionPlanningError):
            planner.preview(self.source, [safe_target[0], dict(safe_target[0])])

    def test_preview_binds_complete_validated_source_set_to_identity_and_actions(self):
        second = {
            "path": "agent-workflows/schemas/distribution-preview-v1.schema.json",
            "class": "portable_source",
            "version": "v3",
            "digest": "sha256:" + "b" * 64,
        }
        target = [{"path": ".codex/skills/distribution", "ownership": "managed"}]
        planner = DistributionPlanner(self.manifest)
        preview = planner.preview([self.source[0], second], target)
        expected_set = "sha256:e74c54871cc760ea85588d28816c0f5d75a6852a19445f98a255ce436c99e2ef"
        self.assertEqual(preview["provenance"]["source_set_digest"], expected_set)
        self.assertEqual(
            preview["provenance"]["sources"],
            [second, self.source[0]],
        )
        self.assertEqual(preview["actions"][0]["source_set_digest"], expected_set)
        self.assertRegex(preview["preview_id"], r"^sha256:[0-9a-f]{64}$")

        changed_version = planner.preview([self.source[0], {**second, "version": "v4"}], target)
        changed_digest = planner.preview(
            [self.source[0], {**second, "digest": "sha256:" + "c" * 64}], target
        )
        for changed in (changed_version, changed_digest):
            self.assertNotEqual(changed["provenance"]["source_set_digest"], expected_set)
            self.assertNotEqual(changed["preview_id"], preview["preview_id"])

        for invalid in (
            {key: value for key, value in self.source[0].items() if key != "version"},
            {**self.source[0], "version": ""},
            {**self.source[0], "digest": "sha256:not-a-digest"},
        ):
            with self.subTest(invalid_source=invalid):
                with self.assertRaises(DistributionPlanningError):
                    planner.preview([invalid], target)

    def test_native_adapter_records_supported_semantics_and_exclusions_without_false_parity(self):
        artifact = {
            "path": "agent-workflows/generated/codex-native-projection-v1.json",
            "version": "v1",
            "provider": "codex",
            "source_set_digest": "sha256:3e4135dcecebe17baa2c75b20f3fa679120cd046b44d317aba5d9c09512a5010",
            "target_profile_digest": self.profile["digest"],
            "semantics": ["hooks", "permissions", "skills"],
            "digest": "sha256:624b012ebe1fc49639d8f9eb47657d665bc406c4307943b0ce57c1cf443185b1",
        }
        plan = NativeProjectionAdapter("codex", self.profile).plan(
            self.source, {"skills", "permissions", "hooks"}, staged_artifact=artifact
        )
        self.assertEqual(plan["compatibility"], "partial")
        self.assertEqual(plan["supported_semantics"], ["permissions", "skills"])
        self.assertEqual(plan["exclusions"], ["hooks"])
        self.assertFalse(plan["semantic_parity"])
        self.assertTrue(plan["staged"])
        self.assertEqual(plan["source_refs"], self.source)
        self.assertEqual(plan["target_profile"], self.profile)
        self.assertEqual(plan["staged_artifact"], artifact)

    def test_native_adapter_requires_digest_bound_profile_sources_and_staged_artifact(self):
        artifact = {
            "path": "agent-workflows/generated/codex-native-projection-v1.json",
            "version": "v1",
            "provider": "codex",
            "source_set_digest": "sha256:3e4135dcecebe17baa2c75b20f3fa679120cd046b44d317aba5d9c09512a5010",
            "target_profile_digest": self.profile["digest"],
            "semantics": ["permissions", "skills"],
            "digest": "sha256:d969310233eb84e6981b468ea0f2c1f339ed2d3613ab792ed9cf4ab7c737a058",
        }
        plan = NativeProjectionAdapter("codex", self.profile).plan(
            self.source, {"skills", "permissions"}, staged_artifact=artifact
        )
        self.assertTrue(plan["semantic_parity"])
        self.assertEqual(plan["compatibility"], "full")

        for profile in (
            {"supported_semantics": ["permissions", "skills"]},
            {**self.profile, "supported_semantics": ["hooks", "permissions", "skills"]},
            {**self.profile, "version": "v2"},
            {**self.profile, "digest": "sha256:" + "0" * 64},
        ):
            with self.subTest(profile=profile):
                with self.assertRaises(DistributionPlanningError):
                    NativeProjectionAdapter("codex", profile)

        adapter = NativeProjectionAdapter("codex", self.profile)
        for bad_artifact in (
            None,
            {**artifact, "digest": "sha256:" + "0" * 64},
            {**artifact, "version": "v2"},
            {**artifact, "source_set_digest": "sha256:" + "0" * 64},
            {**artifact, "target_profile_digest": "sha256:" + "0" * 64},
        ):
            with self.subTest(staged_artifact=bad_artifact):
                with self.assertRaises(DistributionPlanningError):
                    adapter.plan(self.source, {"skills", "permissions"}, staged_artifact=bad_artifact)
        for changed_source in (
            [{**self.source[0], "version": "v2"}],
            [{**self.source[0], "digest": "sha256:" + "b" * 64}],
            [self.source[0], dict(self.source[0])],
        ):
            with self.subTest(changed_source=changed_source):
                with self.assertRaises(DistributionPlanningError):
                    adapter.plan(changed_source, {"skills", "permissions"}, staged_artifact=artifact)

    def test_owner_manifest_parity_is_limited_to_portable_source_children(self):
        manifest = json.loads((ROOT / "manifests" / "owner-manifest.json").read_text())
        self.assertIn("agent-workflows/scripts", manifest["managed_sources"])
        self.assertNotIn(".codex", manifest["managed_sources"])
        self.assertNotIn(".claude", manifest["managed_sources"])
        preview = DistributionPlanner(manifest).preview(
            [{"path": "agent-workflows/scripts/run-persistent-receipt.py", "class": "portable_source", "version": "v1", "digest": "sha256:" + "b" * 64}],
            [{"path": ".codex/skills/workflow", "ownership": "managed"}],
        )
        self.assertFalse(preview["applied"])


if __name__ == "__main__":
    unittest.main()
