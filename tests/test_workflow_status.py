import json
from pathlib import Path
import unittest


ROOT = Path(__file__).resolve().parents[1]
CODEX_SKILL = ROOT / "templates/project-workflow/codex/workflow-status/SKILL.md"
CODEX_METADATA = ROOT / "templates/project-workflow/codex/workflow-status/agents/openai.yaml"
CLAUDE_SKILL = ROOT / "templates/project-workflow/claude/workflow-status/SKILL.md"
PHASES = ROOT / "templates/project-workflow/shared/workflow-status/phases.json"
FIXTURES = ROOT / "tests/fixtures/workflow-status"

REQUIRED_OUTPUT_FIELDS = (
    "Workflow:",
    "Harness:",
    "Full flow:",
    "Current position:",
    "Evidence:",
    "Next Skill/entry:",
    "Unverified:",
)


class WorkflowStatusTemplateTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.codex_skill = CODEX_SKILL.read_text(encoding="utf-8")
        cls.claude_skill = CLAUDE_SKILL.read_text(encoding="utf-8")
        cls.metadata = CODEX_METADATA.read_text(encoding="utf-8")
        cls.phases = json.loads(PHASES.read_text(encoding="utf-8"))

    def test_public_invocations_and_required_output_contract(self):
        self.assertIn("name: workflow-status", self.codex_skill)
        self.assertIn("$workflow-status", self.codex_skill)
        self.assertIn("name: workflow-status", self.claude_skill)
        self.assertIn("user-invocable: true", self.claude_skill)
        for skill in (self.codex_skill, self.claude_skill):
            positions = [skill.index(field) for field in REQUIRED_OUTPUT_FIELDS]
            self.assertEqual(positions, sorted(positions))
            self.assertIn("./phases.json", skill)

    def test_selection_contract_rejects_malformed_unknown_and_wrong_harness(self):
        valid = json.loads(
            (FIXTURES / "selection-valid-codex.json").read_text(encoding="utf-8")
        )
        mismatch = json.loads(
            (FIXTURES / "selection-harness-mismatch.json").read_text(
                encoding="utf-8"
            )
        )
        with self.assertRaises(json.JSONDecodeError):
            json.loads(
                (FIXTURES / "selection-malformed.json").read_text(encoding="utf-8")
            )
        self.assertEqual(valid["schema"], "project-workflow-selection/v1")
        self.assertEqual(valid["agent"], "codex")
        self.assertEqual(mismatch["agent"], "claude")
        for skill, harness in ((self.codex_skill, "codex"), (self.claude_skill, "claude")):
            self.assertIn("project-workflow-selection/v1", skill)
            self.assertIn(f"exactly `{harness}`", skill)
            self.assertIn("upstream.repository", skill)
            self.assertIn("upstream.ref", skill)
            self.assertIn("upstream.commit", skill)
            self.assertIn("do not run status\ncommands or infer progress", skill)
            self.assertIn("Next Skill/entry` set to `Unverified", skill)

    def test_codex_metadata_requires_explicit_invocation(self):
        self.assertIn('display_name: "Workflow Status"', self.metadata)
        self.assertIn("allow_implicit_invocation: false", self.metadata)
        self.assertIn("$workflow-status", self.metadata)

    def test_shared_phase_map_has_a_single_installed_relative_reference(self):
        installation = self.phases["installation"]
        self.assertEqual(installation["installed_relative_path"], "./phases.json")
        self.assertEqual(
            installation["template_source"],
            "templates/project-workflow/shared/workflow-status/phases.json",
        )
        self.assertIn("single shared source", installation["rule"])

    def test_aidlc_uses_official_status_and_models_validity_cases(self):
        aidlc = self.phases["aidlc"]
        self.assertEqual(
            aidlc["full_flow"],
            ["Initialization", "Ideation", "Inception", "Construction", "Operation"],
        )
        self.assertEqual(aidlc["official_status"]["codex"], "$aidlc --status")
        self.assertEqual(aidlc["official_status"]["claude"], "/aidlc --status")
        self.assertIn("aidlc-state.md", aidlc["state_file"])
        self.assertEqual(
            aidlc["required_status_fields"],
            ["Phase", "Current Stage", "Next Stage", "Status"],
        )
        self.assertEqual(
            set(aidlc["state_cases"]), {"valid", "unavailable", "invalid"}
        )
        self.assertEqual(
            aidlc["state_cases"]["valid"]["next_entry"]["codex"], "$aidlc --resume"
        )
        self.assertEqual(
            aidlc["state_cases"]["valid"]["next_entry"]["claude"], "/aidlc --resume"
        )
        for skill in (self.codex_skill, self.claude_skill):
            self.assertIn("authoritative read-only evidence", skill)
            self.assertIn("wins over artifact inference", skill)
            self.assertIn("invalid and unverified", skill)

    def test_aidlc_status_fixtures_cover_valid_unavailable_and_invalid(self):
        valid = (FIXTURES / "aidlc-valid-status.txt").read_text(encoding="utf-8")
        unavailable = (FIXTURES / "aidlc-unavailable-status.txt").read_text(
            encoding="utf-8"
        )
        invalid = (FIXTURES / "aidlc-invalid-status.txt").read_text(encoding="utf-8")
        field_labels = {
            line.split(":", 1)[0].strip()
            for line in valid.splitlines()
            if ":" in line
        }
        self.assertTrue(
            set(self.phases["aidlc"]["required_status_fields"]) <= field_labels
        )
        self.assertIn("No active AI-DLC workflow found.", unavailable)
        self.assertIn("Phase:          Unknown", invalid)
        self.assertIn("Status:         Unknown", invalid)

    def test_superpowers_covers_each_phase_and_marks_position_as_inference(self):
        superpowers = self.phases["superpowers"]
        phases = superpowers["full_flow"]
        self.assertEqual(
            [phase["id"] for phase in phases],
            ["design", "plan", "implementation", "review", "completion"],
        )
        self.assertIn(
            "no deterministic persistent state machine",
            superpowers["inference_policy"].lower(),
        )
        self.assertEqual(phases[0]["next_entry"]["codex"], "$brainstorming")
        self.assertEqual(
            phases[0]["next_entry"]["claude"], "/superpowers:brainstorming"
        )
        self.assertEqual(
            phases[1]["next_entry"]["claude"], "/superpowers:writing-plans"
        )
        self.assertEqual(
            phases[2]["next_entry"]["codex"], "$subagent-driven-development"
        )
        self.assertEqual(
            phases[3]["next_entry"]["claude"],
            "/superpowers:requesting-code-review",
        )
        self.assertEqual(
            phases[4]["following_entry"]["codex"],
            "$finishing-a-development-branch",
        )
        for skill in (self.codex_skill, self.claude_skill):
            self.assertIn("Current position` as `Inferred", skill)
            self.assertIn("does not prove phase completion", skill)

    def test_superpowers_fixture_keeps_ambiguous_progress_unverified(self):
        evidence = json.loads(
            (FIXTURES / "superpowers-evidence.json").read_text(encoding="utf-8")
        )
        self.assertTrue(evidence["design_artifact"].startswith("docs/superpowers/specs/"))
        self.assertTrue(evidence["plan_artifact"].startswith("docs/superpowers/plans/"))
        self.assertTrue(evidence["expected_position"].startswith("Inferred —"))
        self.assertIn("do not prove", evidence["expected_unverified"])

    def test_skills_are_read_only_status_reporters(self):
        prohibited_commands = ("git commit", "git push", "git add", "rm -", "mv ", "cp ")
        for skill in (self.codex_skill, self.claude_skill):
            self.assertIn("Never edit source, state, Git, or external systems.", skill)
            self.assertIn("Never start workers or\nadvance a workflow.", skill)
            self.assertIn("do not invoke it", skill)
            self.assertFalse(any(command in skill for command in prohibited_commands))


if __name__ == "__main__":
    unittest.main()
