"""Source-wide status is derived from physical, digest-bound evidence."""

from __future__ import annotations

import ast
import hashlib
import json
import shutil
import sys
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SRC = ROOT / "agent-workflows" / "src"
sys.path.insert(0, str(SRC))

from ai_agent_workflow.implementation_status import (  # noqa: E402
    ImplementationStatusError,
    _evaluate_implementation,
    evaluate_implementation,
    project_plan_coverage,
    render_documentation_projection,
    validate_documentation_projection,
)


MANIFEST = ROOT / "agent-workflows" / "manifests" / "implementation-status.json"
HTML = ROOT / "docs" / "ai-agent-workflow-usage.html"


def _digest(path: Path) -> str:
    return "sha256:" + hashlib.sha256(path.read_bytes()).hexdigest()


class ImplementationStatusTests(unittest.TestCase):
    def _write_manifest(self, value):
        temporary = tempfile.TemporaryDirectory()
        path = Path(temporary.name) / "implementation-status.json"
        path.write_text(json.dumps(value), encoding="utf-8")
        self.addCleanup(temporary.cleanup)
        return path

    def _copy_source_root(self) -> Path:
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        root = Path(temporary.name)
        shutil.copytree(ROOT / "agent-workflows", root / "agent-workflows")
        shutil.copytree(ROOT / "docs" / "plans", root / "docs" / "plans")
        return root

    def _evaluate_unreviewed_copy(self, root: Path):
        return _evaluate_implementation(
            root / "agent-workflows/manifests/implementation-status.json",
            root,
            require_current_lineage=False,
        )

    def test_current_source_release_is_complete_but_not_operationally_ready(self):
        status = evaluate_implementation(MANIFEST, ROOT)

        self.assertEqual(
            {"accepted": 60, "planned": 60, "remaining_ids": []},
            status["coverage"]["named_contracts"],
        )
        self.assertEqual(
            {"accepted": 23, "planned": 23, "remaining_ids": []},
            status["coverage"]["profile_steps"],
        )
        self.assertEqual(
            {"accepted": 11, "planned": 11, "remaining_ids": []},
            status["coverage"]["required_surfaces"],
        )
        self.assertEqual(52, len(status["coverage"]["distributed_skills"]))
        self.assertEqual(8, len(status["coverage"]["shared_protocol_operations"]))
        self.assertTrue(status["source_wide_integration_complete"])
        self.assertFalse(status["full_workflow_ready"])
        self.assertEqual([], status["pending_source_gates"])
        self.assertEqual(13, len(status["pending_operational_gates"]))
        self.assertEqual(0, status["required_source_findings"])
        for claim in (
            "actual_a7_handoff_complete",
            "current_objective_approved",
            "current_run_objective_achieved",
            "personal_profile_pilots_completed",
            "objective_audit_completed",
            "run_outcome_decided",
            "archive_or_continue_executed",
            "live_runtime_verified",
            "nix_generation_build_verified",
            "nix_rebuild_verified",
            "migration_complete",
            "activation_complete",
            "operational_adoption_approved",
            "full_workflow_ready",
        ):
            self.assertFalse(status["claims"][claim], claim)

    def test_status_manifest_is_not_a_second_coverage_registry(self):
        manifest = json.loads(MANIFEST.read_text(encoding="utf-8"))
        self.assertNotIn("coverage", manifest)
        self.assertNotIn("implemented_contracts", manifest)
        self.assertNotIn("implemented_profiles", manifest)

        tree = ast.parse(
            (SRC / "ai_agent_workflow/implementation_status.py").read_text(
                encoding="utf-8"
            )
        )
        integer_constants = {
            node.value
            for node in ast.walk(tree)
            if isinstance(node, ast.Constant) and type(node.value) is int
        }
        self.assertTrue({60, 23, 11}.isdisjoint(integer_constants))

    def test_plan_coverage_is_the_exact_canonical_compatibility_projection(self):
        release = json.loads(
            (ROOT / "agent-workflows/manifests/source-release.json").read_text()
        )
        index = json.loads(
            (ROOT / "agent-workflows/manifests/source-evidence-index.json").read_text()
        )
        coverage = json.loads(
            (ROOT / "agent-workflows/manifests/plan-coverage.json").read_text()
        )
        self.assertEqual(project_plan_coverage(release, index, ROOT), coverage)
        self.assertEqual(83, len(coverage["source_inventory"]))
        self.assertEqual(83, len(coverage["accepted_coverage"]))

    def test_rejects_release_index_fixture_and_gate_evidence_digest_drift(self):
        keys = (
            ("source_release_ref", None),
            ("source_evidence_index_ref", None),
            ("source_fixture_refs", 0),
        )
        for key, position in keys:
            with self.subTest(key=key):
                value = json.loads(MANIFEST.read_text(encoding="utf-8"))
                ref = value["source_authority"][key]
                if position is not None:
                    ref = ref[position]
                ref["digest"] = "sha256:" + "0" * 64
                with self.assertRaisesRegex(ImplementationStatusError, "digest mismatch"):
                    _evaluate_implementation(
                        self._write_manifest(value), ROOT, require_current_lineage=False
                    )

        value = json.loads(MANIFEST.read_text(encoding="utf-8"))
        value["source_gates"][0]["evidence_refs"][0]["digest"] = (
            "sha256:" + "0" * 64
        )
        with self.assertRaisesRegex(ImplementationStatusError, "digest mismatch"):
            _evaluate_implementation(
                self._write_manifest(value), ROOT, require_current_lineage=False
            )

    def test_rejects_comutated_release_bytes_and_plan_projection(self):
        root = self._copy_source_root()
        status_path = root / "agent-workflows/manifests/implementation-status.json"
        status = json.loads(status_path.read_text())
        release_path = root / "agent-workflows/manifests/source-release.json"
        release = json.loads(release_path.read_text())
        release["claims"]["canonical_source_projection_complete"] = False
        release_path.write_text(json.dumps(release), encoding="utf-8")
        status["source_authority"]["source_release_ref"]["digest"] = _digest(
            release_path
        )
        status_path.write_text(json.dumps(status), encoding="utf-8")
        with self.assertRaisesRegex(
            ImplementationStatusError, "differs from physical compilation"
        ):
            self._evaluate_unreviewed_copy(root)

        root = self._copy_source_root()
        status_path = root / "agent-workflows/manifests/implementation-status.json"
        status = json.loads(status_path.read_text())
        coverage_path = root / "agent-workflows/manifests/plan-coverage.json"
        coverage = json.loads(coverage_path.read_text())
        coverage["accepted_coverage"][0], coverage["accepted_coverage"][1] = (
            coverage["accepted_coverage"][1],
            coverage["accepted_coverage"][0],
        )
        coverage_path.write_text(json.dumps(coverage), encoding="utf-8")
        status["source_authority"]["plan_coverage_ref"]["digest"] = _digest(
            coverage_path
        )
        status_path.write_text(json.dumps(status), encoding="utf-8")
        with self.assertRaisesRegex(
            ImplementationStatusError, "differs from canonical source index"
        ):
            self._evaluate_unreviewed_copy(root)

    def test_h_skeleton_cannot_be_substituted_for_live_evidence(self):
        root = self._copy_source_root()
        status_path = root / "agent-workflows/manifests/implementation-status.json"
        status = json.loads(status_path.read_text())
        h_path = root / "agent-workflows/evidence/source-wide/h1-h2-h3-skeleton.json"
        evidence = json.loads(h_path.read_text())
        evidence["claim_boundary"]["live_runtime_evidence"] = True
        h_path.write_text(json.dumps(evidence), encoding="utf-8")
        status["source_authority"]["source_fixture_refs"][-1]["digest"] = _digest(
            h_path
        )
        status_path.write_text(json.dumps(status), encoding="utf-8")
        with self.assertRaisesRegex(
            ImplementationStatusError, "fixture evidence is not exact|source-only boundary"
        ):
            self._evaluate_unreviewed_copy(root)

    def test_source_evidence_cannot_forge_live_or_full_readiness(self):
        passed_live = json.loads(MANIFEST.read_text(encoding="utf-8"))
        passed_live["operational_gates"][1]["status"] = "passed"
        full = json.loads(MANIFEST.read_text(encoding="utf-8"))
        full["completion_claim"]["overall_status"] = "ready"
        full["completion_claim"]["full_workflow_ready"] = True
        for value in (passed_live, full):
            with self.subTest(claim=value["completion_claim"]):
                with self.assertRaisesRegex(ImplementationStatusError, "schema|operational"):
                    _evaluate_implementation(
                        self._write_manifest(value), ROOT, require_current_lineage=False
                    )

    def test_source_wide_claim_fails_for_pending_gate_or_required_finding(self):
        value = json.loads(MANIFEST.read_text(encoding="utf-8"))
        value["source_gates"][0]["status"] = "pending"
        value["source_gates"][0]["evidence_refs"] = []
        with self.assertRaisesRegex(ImplementationStatusError, "completion claim"):
            _evaluate_implementation(
                self._write_manifest(value), ROOT, require_current_lineage=False
            )

        root = self._copy_source_root()
        status_path = root / "agent-workflows/manifests/implementation-status.json"
        status = json.loads(status_path.read_text())
        aggregate_path = root / "agent-workflows/evidence/source-wide-integration-completed.json"
        aggregate = json.loads(aggregate_path.read_text())
        aggregate["required_source_findings"] = ["required-example"]
        aggregate_path.write_text(json.dumps(aggregate), encoding="utf-8")
        status["source_gates"][-1]["evidence_refs"][0]["digest"] = _digest(
            aggregate_path
        )
        status_path.write_text(json.dumps(status), encoding="utf-8")
        with self.assertRaisesRegex(ImplementationStatusError, "completion claim"):
            self._evaluate_unreviewed_copy(root)

    def test_html_is_exactly_rendered_and_rejects_stale_claims(self):
        status = evaluate_implementation(MANIFEST, ROOT)
        validate_documentation_projection(HTML, status)
        source = HTML.read_text(encoding="utf-8")
        self.assertEqual(source, render_documentation_projection(status, source))

        stale = source.replace("60 / 60", "29 / 60", 1)
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        path = Path(temporary.name) / "usage.html"
        path.write_text(stale, encoding="utf-8")
        with self.assertRaisesRegex(ImplementationStatusError, "disagree|stale"):
            validate_documentation_projection(path, status)


if __name__ == "__main__":
    unittest.main()
