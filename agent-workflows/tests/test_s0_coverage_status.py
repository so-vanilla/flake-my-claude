"""Public compatibility projection for the source-complete registry."""

from __future__ import annotations

import json
import sys
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "agent-workflows" / "src"))

from ai_agent_workflow.catalog import compile_registry  # noqa: E402
from ai_agent_workflow.implementation_status import (  # noqa: E402
    evaluate_implementation,
    project_plan_coverage,
)


MANIFEST = ROOT / "agent-workflows/manifests/implementation-status.json"


class S0CoverageStatusTests(unittest.TestCase):
    def test_all_group_profile_and_surface_entries_are_accepted(self):
        status = evaluate_implementation(MANIFEST, ROOT)
        projection = status["s0_coverage"]

        self.assertEqual(60, projection["counts"]["named_targets"])
        self.assertEqual(60, projection["counts"]["accepted_named"])
        self.assertEqual(23, projection["counts"]["profile_targets"])
        self.assertEqual(23, projection["counts"]["accepted_profiles"])
        self.assertEqual(11, projection["counts"]["additional_surfaces"])
        self.assertEqual(83, len(projection["source_inventory"]))
        self.assertEqual(83, len(projection["accepted_coverage"]))
        self.assertEqual([], projection["missing_or_invalid"])
        self.assertEqual([], projection["present_unaccepted"])
        self.assertTrue(
            all(item["state"] == "accepted" for item in projection["target_registry"].values())
        )
        self.assertTrue(
            all(item["state"] == "accepted" for item in projection["additional_surfaces"].values())
        )

    def test_plan_coverage_matches_compile_registry_identity_and_order(self):
        release = json.loads(
            (ROOT / "agent-workflows/manifests/source-release.json").read_text()
        )
        index = json.loads(
            (ROOT / "agent-workflows/manifests/source-evidence-index.json").read_text()
        )
        expected = project_plan_coverage(release, index, ROOT)
        physical = json.loads(
            (ROOT / "agent-workflows/manifests/plan-coverage.json").read_text()
        )
        registry = compile_registry(source_root=ROOT)

        self.assertEqual(expected, physical)
        self.assertEqual(
            [item["id"] for item in physical["source_inventory"]],
            sorted(registry["target_registry"]),
        )
        self.assertEqual(
            physical["accepted_coverage"], sorted(registry["accepted_coverage"])
        )

    def test_named_contract_count_is_not_the_physical_skill_directory_count(self):
        status = evaluate_implementation(MANIFEST, ROOT)
        coverage = status["coverage"]

        self.assertEqual(52, len(coverage["distributed_skills"]))
        self.assertEqual(
            ["group.F.F%d" % number for number in range(1, 9)],
            coverage["shared_protocol_operations"],
        )
        self.assertEqual(
            coverage["named_contracts"]["accepted"],
            len(coverage["distributed_skills"])
            + len(coverage["shared_protocol_operations"]),
        )

    def test_source_completion_does_not_grant_operational_readiness(self):
        status = evaluate_implementation(MANIFEST, ROOT)

        self.assertTrue(status["claims"]["s0_registry_ready"])
        self.assertTrue(status["claims"]["source_transition_fixture_passed"])
        self.assertTrue(status["claims"]["source_wide_integration_complete"])
        self.assertFalse(status["claims"]["actual_a7_handoff_complete"])
        self.assertFalse(status["claims"]["current_objective_approved"])
        self.assertFalse(status["claims"]["live_runtime_verified"])
        self.assertFalse(status["claims"]["nix_generation_build_verified"])
        self.assertFalse(status["claims"]["nix_rebuild_verified"])
        self.assertFalse(status["claims"]["migration_complete"])
        self.assertFalse(status["claims"]["activation_complete"])
        self.assertFalse(status["claims"]["full_workflow_ready"])


if __name__ == "__main__":
    unittest.main()
