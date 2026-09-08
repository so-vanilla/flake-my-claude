from __future__ import annotations

import ast
import hashlib
import json
import unittest
from pathlib import Path
from tempfile import TemporaryDirectory

from ai_agent_workflow.schema_validation import SchemaValidationError, validate_document
from ai_agent_workflow.source_wide_fixtures import (
    SourceWideFixtureError,
    SourceWideFixturesV1,
)

ROOT = Path(__file__).resolve().parents[2]
FIXTURES = ROOT / "agent-workflows/tests/fixtures/s8"
EVIDENCE = ROOT / "agent-workflows/evidence/source-wide"


def read_json(path: Path) -> dict:
    return json.loads(path.read_text(encoding="utf-8"))


def digest_bytes(path: Path) -> str:
    return "sha256:" + hashlib.sha256(path.read_bytes()).hexdigest()


def digest_document(value: dict) -> str:
    raw = json.dumps(
        value, sort_keys=True, separators=(",", ":"), ensure_ascii=True
    ).encode()
    return "sha256:" + hashlib.sha256(raw).hexdigest()


class SourceWideProfileFixtureTests(unittest.TestCase):
    def setUp(self) -> None:
        self.compiler = SourceWideFixturesV1(source_root=ROOT)

    def compile(self, name: str) -> dict:
        return self.compiler.compile_fixture(read_json(FIXTURES / (name + ".json")))

    def test_compiler_is_a_read_only_deterministic_source_fixture(self) -> None:
        path = ROOT / "agent-workflows/src/ai_agent_workflow/source_wide_fixtures.py"
        tree = ast.parse(path.read_text(encoding="utf-8"))
        forbidden_imports = {"os", "shutil", "socket", "subprocess"}
        forbidden_calls = {
            "mkdir",
            "open",
            "rename",
            "replace",
            "unlink",
            "write_bytes",
            "write_text",
        }
        imports = {
            alias.name.split(".", 1)[0]
            for node in ast.walk(tree)
            if isinstance(node, (ast.Import, ast.ImportFrom))
            for alias in (
                node.names
                if isinstance(node, ast.Import)
                else [ast.alias(node.module or "")]
            )
        }
        calls = {
            node.func.attr
            for node in ast.walk(tree)
            if isinstance(node, ast.Call) and isinstance(node.func, ast.Attribute)
        }
        self.assertFalse(imports & forbidden_imports)
        self.assertFalse(calls & forbidden_calls)

    def test_three_profiles_follow_exact_accepted_source_order(self) -> None:
        expected = {
            "profile-feature": ("feature", "feature-architectural", 20, 93, 8, 1, 0),
            "profile-bug-fix": ("bug-fix", "bug-fix-standard", 20, 93, 8, 2, 1),
            "profile-improvement": (
                "improvement",
                "improvement-measured",
                20,
                92,
                7,
                1,
                0,
            ),
        }
        for name, counts in expected.items():
            with self.subTest(name=name):
                result = self.compile(name)
                profile, workflow, stages, selectors, profile_steps, rounds, fixes = (
                    counts
                )
                self.assertEqual("source-candidate", result["status"])
                self.assertEqual(profile, result["profile"])
                self.assertEqual(workflow, result["workflow_id"])
                self.assertEqual(stages, len(result["source_order"]["stage_ids"]))
                self.assertEqual(selectors, len(result["source_order"]["selectors"]))
                self.assertEqual(
                    profile_steps, len(result["source_order"]["profile_selectors"])
                )
                self.assertEqual(
                    list("BCDEFGH"),
                    [item["group"] for item in result["common_implementation_refs"]],
                )
                self.assertTrue(
                    all(
                        not item["implementation_embedded"]
                        for item in result["common_implementation_refs"]
                    )
                )
                self.assertEqual(rounds, result["review_execution"]["rounds_used"])
                self.assertEqual(fixes, result["review_execution"]["fix_waves_used"])
                self.assertEqual(
                    0, result["review_execution"]["open_required_finding_count"]
                )
                self.assertEqual(
                    "source-candidate-accepted", result["review_execution"]["terminal"]
                )
                self.assertTrue(result["claim_boundary"]["source_candidate_only"])
                self.assertTrue(
                    all(
                        value is False
                        for key, value in result["claim_boundary"].items()
                        if key != "source_candidate_only"
                    )
                )
                unsigned = {
                    key: value
                    for key, value in result.items()
                    if key != "result_digest"
                }
                self.assertEqual(digest_document(unsigned), result["result_digest"])

    def test_required_findings_are_validated_material_and_deduplicated_before_fix(
        self,
    ) -> None:
        result = self.compile("profile-bug-fix")
        review = result["review_execution"]
        first, second = review["rounds"]
        self.assertEqual(2, first["raw_finding_count"])
        self.assertEqual(1, first["deduplicated_finding_count"])
        self.assertEqual(["bug-regression-gap"], first["required_fingerprints"])
        self.assertEqual("fix", first["route"])
        self.assertEqual([], second["required_fingerprints"])
        self.assertEqual("accepted", second["route"])
        self.assertEqual(
            ["bug-arch-regression-gap", "bug-ops-regression-gap"],
            first["finding_validation"]["dispositions"][0]["source_finding_ids"],
        )
        validation_index = review["trace"].index(
            {"kind": "finding-validation", "round": 1}
        )
        fix_index = review["trace"].index({"kind": "fix", "round": 1, "wave": 1})
        self.assertLess(validation_index, fix_index)
        self.assertEqual(2, review["shared_budget"]["maximum_review_rounds"])
        self.assertEqual(1, review["shared_budget"]["maximum_fix_waves"])

    def test_no_required_branches_converge_without_fix(self) -> None:
        for name in ("profile-feature", "profile-improvement"):
            with self.subTest(name=name):
                review = self.compile(name)["review_execution"]
                self.assertEqual(0, review["fix_waves_used"])
                self.assertNotIn("fix", [event["kind"] for event in review["trace"]])
                self.assertEqual("accepted", review["rounds"][-1]["route"])

    def test_projection_evidence_is_exact_deterministic_compilation(self) -> None:
        projections = {
            "profile-feature": "profile-feature.json",
            "profile-bug-fix": "profile-bug-fix.json",
            "profile-improvement": "profile-improvement.json",
        }
        for fixture, evidence in projections.items():
            with self.subTest(fixture=fixture):
                self.assertEqual(self.compile(fixture), read_json(EVIDENCE / evidence))

    def test_rejects_stale_reordered_cross_profile_and_dependency_skip(self) -> None:
        mutations = {}
        stale = read_json(FIXTURES / "profile-feature.json")
        stale["expected_git_head"] = "0" * 40
        mutations["stale"] = stale
        reordered = read_json(FIXTURES / "profile-feature.json")
        reordered["claimed_stage_order"][4], reordered["claimed_stage_order"][5] = (
            reordered["claimed_stage_order"][5],
            reordered["claimed_stage_order"][4],
        )
        mutations["reordered"] = reordered
        cross_profile = read_json(FIXTURES / "profile-feature.json")
        cross_profile["profile"] = "bug-fix"
        mutations["cross-profile"] = cross_profile
        dependency_skip = read_json(FIXTURES / "profile-feature.json")
        dependency_skip["claimed_stage_order"].pop(8)
        mutations["dependency-skip"] = dependency_skip
        profile_reordered = read_json(FIXTURES / "profile-feature.json")
        (
            profile_reordered["claimed_profile_order"][0],
            profile_reordered["claimed_profile_order"][1],
        ) = (
            profile_reordered["claimed_profile_order"][1],
            profile_reordered["claimed_profile_order"][0],
        )
        mutations["profile-reordered"] = profile_reordered
        for name, document in mutations.items():
            with self.subTest(name=name), self.assertRaises(SourceWideFixtureError):
                self.compiler.compile_profile_execution(document)

    def test_rejects_review_validity_materiality_dedup_and_budget_bypass(self) -> None:
        mutations = {}
        skipped = read_json(FIXTURES / "profile-bug-fix.json")
        skipped["review_plan"]["trace"].pop(2)
        mutations["validity-skip"] = skipped
        invalid = read_json(FIXTURES / "profile-bug-fix.json")
        del invalid["review_plan"]["rounds"][0]["reviews"][0]["findings"][0]["severity"]
        mutations["invalid-finding"] = invalid
        unclassified = read_json(FIXTURES / "profile-bug-fix.json")
        unclassified["review_plan"]["rounds"][0]["dispositions"] = []
        mutations["dedup-skip"] = unclassified
        immaterial = read_json(FIXTURES / "profile-bug-fix.json")
        immaterial["review_plan"]["rounds"][0]["dispositions"][0]["materiality"] = (
            "non-material"
        )
        mutations["materiality-skip"] = immaterial
        budget = read_json(FIXTURES / "profile-bug-fix.json")
        budget["review_plan"]["budget"]["maximum_review_rounds"] = 1
        mutations["round-budget"] = budget
        invented_fix = read_json(FIXTURES / "profile-bug-fix.json")
        invented_fix["review_plan"]["fix_waves"][0]["required_fingerprints"] = [
            "other-gap"
        ]
        mutations["fix-scope"] = invented_fix
        for name, document in mutations.items():
            with self.subTest(name=name), self.assertRaises(SourceWideFixtureError):
                self.compiler.compile_profile_execution(document)


class ColdResumeAndCompletionSkeletonTests(unittest.TestCase):
    def setUp(self) -> None:
        self.compiler = SourceWideFixturesV1(source_root=ROOT)

    def test_history_free_cold_resume_reconstructs_exact_next_action(self) -> None:
        result = self.compiler.compile_cold_resume(
            read_json(FIXTURES / "cold-resume.json")
        )
        self.assertEqual("history-free", result["history_mode"])
        self.assertEqual(
            {
                "action": "compile-source-stage",
                "stage_id": "execution-review",
                "kind": "common",
                "mode": "parallel",
                "selectors": ["group.E.E4", "group.E.E5"],
            },
            result["next_action"],
        )
        self.assertFalse(result["claim_boundary"]["current_run_evidence"])
        self.assertEqual(result, read_json(EVIDENCE / "cold-resume.json"))

    def test_cold_resume_rejects_stale_head_digest_and_conversation(self) -> None:
        mutations = {}
        stale_head = read_json(FIXTURES / "cold-resume.json")
        stale_head["observed_head"]["revision"] += 1
        mutations["stale-head"] = stale_head
        stale_digest = read_json(FIXTURES / "cold-resume.json")
        stale_digest["checkpoint_ref"]["digest"] = "sha256:" + "0" * 64
        mutations["stale-digest"] = stale_digest
        conversation = read_json(FIXTURES / "cold-resume.json")
        conversation["conversation_history"] = "the next step was review"
        mutations["conversation"] = conversation
        substitute = read_json(FIXTURES / "cold-resume.json")
        substitute["conversation_summary"] = "continue from E4"
        mutations["extra-conversation-substitute"] = substitute
        for name, document in mutations.items():
            with self.subTest(name=name), self.assertRaises(SourceWideFixtureError):
                self.compiler.compile_cold_resume(document)

    def test_cold_resume_rejects_rebound_dependency_skip_and_reordered_source_refs(
        self,
    ) -> None:
        for mutation in ("dependency-skip", "source-reorder"):
            with self.subTest(mutation=mutation), TemporaryDirectory() as directory:
                reference_root = Path(directory)
                relative = Path(
                    "agent-workflows/tests/fixtures/s8/cold-checkpoint.json"
                )
                target = reference_root / relative
                target.parent.mkdir(parents=True)
                checkpoint = read_json(FIXTURES / "cold-checkpoint.json")
                if mutation == "dependency-skip":
                    checkpoint["completed_stage_ids"].pop(7)
                else:
                    (
                        checkpoint["group_manifest_refs"][0],
                        checkpoint["group_manifest_refs"][1],
                    ) = (
                        checkpoint["group_manifest_refs"][1],
                        checkpoint["group_manifest_refs"][0],
                    )
                target.write_text(
                    json.dumps(checkpoint, indent=2) + "\n", encoding="utf-8"
                )
                request = read_json(FIXTURES / "cold-resume.json")
                request["checkpoint_ref"]["digest"] = digest_bytes(target)
                compiler = SourceWideFixturesV1(
                    source_root=ROOT, reference_root=reference_root
                )
                with self.assertRaises(SourceWideFixtureError):
                    compiler.compile_cold_resume(request)

    def test_h1_h2_h3_skeleton_cannot_impersonate_current_run_or_live_gates(
        self,
    ) -> None:
        result = self.compiler.compile_h_skeleton(
            read_json(FIXTURES / "h1-h2-h3-skeleton.json")
        )
        self.assertEqual(
            ["group.H.H1", "group.H.H2", "group.H.H3"], result["selectors"]
        )
        self.assertTrue(result["cannot_satisfy_current_run_live_gates"])
        self.assertTrue(
            all(value is False for value in result["current_run_gates"].values())
        )
        self.assertFalse(result["claim_boundary"]["objective_outcome_claimed"])
        self.assertFalse(result["claim_boundary"]["run_completion_claimed"])
        self.assertEqual(result, read_json(EVIDENCE / "h1-h2-h3-skeleton.json"))

    def test_schema_rejects_a_skeleton_promoted_to_live_evidence(self) -> None:
        result = self.compiler.compile_h_skeleton(
            read_json(FIXTURES / "h1-h2-h3-skeleton.json")
        )
        result["current_run_gates"]["objective_audit_completed"] = True
        schema = read_json(
            ROOT / "agent-workflows/schemas/source-wide-fixture-result-v1.schema.json"
        )
        with self.assertRaises(SchemaValidationError):
            validate_document(result, schema, schema["$defs"])

    def test_h_skeleton_rejects_stale_interface_or_current_run_substitute(self) -> None:
        stale = read_json(FIXTURES / "h1-h2-h3-skeleton.json")
        stale["lifecycle_interface_ref"]["digest"] = "sha256:" + "0" * 64
        with self.assertRaises(SourceWideFixtureError):
            self.compiler.compile_h_skeleton(stale)
        live = read_json(FIXTURES / "h1-h2-h3-skeleton.json")
        live["current_run_evidence"] = True
        with self.assertRaises(SourceWideFixtureError):
            self.compiler.compile_h_skeleton(live)


if __name__ == "__main__":
    unittest.main()
