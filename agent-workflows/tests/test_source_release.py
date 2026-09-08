"""Purpose and integrity tests for the canonical S8 source release."""

from __future__ import annotations

import ast
import copy
import hashlib
import json
import shutil
import sys
import unittest
from pathlib import Path
from tempfile import TemporaryDirectory


ROOT = Path(__file__).resolve().parents[2]
PACKAGE = ROOT / "agent-workflows"
sys.path.insert(0, str(PACKAGE / "src"))

from ai_agent_workflow.schema_validation import (  # noqa: E402
    SchemaValidationError,
    validate_document,
)
from ai_agent_workflow.source_release import (  # noqa: E402
    SourceReleaseError,
    SourceReleaseV1,
    serialize_manifest,
    verify_checked_in_source_release,
)


def raw(path: Path) -> str:
    return "sha256:" + hashlib.sha256(path.read_bytes()).hexdigest()


def read_json(path: Path) -> dict:
    return json.loads(path.read_text(encoding="utf-8"))


def write_json(path: Path, document: object) -> None:
    path.write_text(json.dumps(document, indent=2) + "\n", encoding="utf-8")


class SourceReleaseTests(unittest.TestCase):
    maxDiff = None

    def copy_source_root(self, directory: str) -> Path:
        target = Path(directory)
        shutil.copytree(PACKAGE, target / "agent-workflows")
        plans = target / "docs" / "plans"
        plans.mkdir(parents=True)
        for name in (
            "ai-agent-workflow-full-implementation-plan.md",
            "ai-agent-workflow-step-catalog.md",
        ):
            shutil.copy2(ROOT / "docs" / "plans" / name, plans / name)
        return target

    def test_compiles_exact_catalog_order_counts_and_reachability(self) -> None:
        compiled = SourceReleaseV1(source_root=ROOT).compile()
        release = compiled["source_release"]
        index = compiled["evidence_index"]
        catalog = read_json(PACKAGE / "catalog.yaml")
        expected_named = [
            "group.%s.%s" % (group, local_id)
            for group, declarations in catalog["named_contracts"].items()
            for local_id, _name in declarations
        ]
        expected_profiles = [
            "profile.%s.%s" % (profile, local_id)
            for profile, declarations in catalog["profile_steps"].items()
            for local_id, _name in declarations
        ]
        surface_manifest = read_json(PACKAGE / "manifests/additional-required-surfaces.json")
        expected_surfaces = [item["id"] for item in surface_manifest["surfaces"]]
        entries = index["entries"]

        self.assertEqual(expected_named + expected_profiles + expected_surfaces, [item["id"] for item in entries])
        self.assertEqual(len(entries), index["entry_count"])
        self.assertEqual(len(expected_named), release["coverage"]["named_contracts"]["accepted"])
        self.assertEqual(len(expected_profiles), release["coverage"]["profile_steps"]["accepted"])
        self.assertEqual(len(expected_surfaces), release["coverage"]["additional_required_surfaces"]["accepted"])
        for entry in entries:
            with self.subTest(identifier=entry["id"]):
                self.assertEqual("passed", entry["evidence"]["result"])
                self.assertEqual(entry["source"]["digest"], raw(ROOT / entry["source"]["path"]))
                self.assertEqual(entry["evidence"]["digest"], raw(ROOT / entry["evidence"]["path"]))
                self.assertTrue(entry["test_selector"].startswith("agent-workflows/tests/test_"))
                self.assertTrue(entry["reachability"])
                if entry["kind"] != "required-surface":
                    self.assertTrue(any(value["kind"] == "workflow" for value in entry["reachability"]))
                else:
                    self.assertEqual("surface-composition", entry["reachability"][0]["kind"])

    def test_checked_in_index_has_one_owner_and_is_deterministic(self) -> None:
        first = SourceReleaseV1(source_root=ROOT).compile()
        second = SourceReleaseV1(source_root=ROOT).compile()
        self.assertEqual(first, second)
        self.assertEqual(
            serialize_manifest(first["evidence_index"]),
            (PACKAGE / "manifests/source-evidence-index.json").read_bytes(),
        )
        self.assertEqual(
            serialize_manifest(first["source_release"]),
            (PACKAGE / "manifests/source-release.json").read_bytes(),
        )
        self.assertEqual(
            {
                "id": "source-release/v1",
                "path": "agent-workflows/manifests/source-release.json",
                "compiler": "ai_agent_workflow.source_release.SourceReleaseV1",
            },
            first["evidence_index"]["owner"],
        )
        self.assertEqual("passed", verify_checked_in_source_release(source_root=ROOT)["result"])

    def test_checked_in_documents_validate_against_closed_schemas(self) -> None:
        for document_name, schema_name in (
            ("source-release.json", "source-release-v1.schema.json"),
            ("source-evidence-index.json", "source-evidence-index-v1.schema.json"),
        ):
            with self.subTest(document=document_name):
                document = read_json(PACKAGE / "manifests" / document_name)
                schema = read_json(PACKAGE / "schemas" / schema_name)
                validate_document(document, schema, schema.get("$defs", {}))

    def test_compiler_contains_no_hard_coded_success_counts(self) -> None:
        tree = ast.parse((PACKAGE / "src/ai_agent_workflow/source_release.py").read_text(encoding="utf-8"))
        integer_literals = {
            node.value
            for node in ast.walk(tree)
            if isinstance(node, ast.Constant)
            and isinstance(node.value, int)
            and not isinstance(node.value, bool)
        }
        self.assertTrue({60, 23, 11}.isdisjoint(integer_literals))

    def test_required_only_policy_is_not_counted_as_a_group_manifest(self) -> None:
        release = SourceReleaseV1(source_root=ROOT).compile()["source_release"]
        paths = [item["path"] for item in release["inputs"]["group_manifests"]]
        self.assertNotIn(
            "agent-workflows/groups/required-only-feedback-execution-policy-v1.json",
            paths,
        )
        catalog = read_json(PACKAGE / "catalog.yaml")
        self.assertEqual(list(catalog["named_contracts"]), [read_json(ROOT / path)["group_id"].rsplit(".", 1)[-1] for path in paths])

    def test_excludes_worker_reports_current_run_and_private_absolute_paths(self) -> None:
        compiled = SourceReleaseV1(source_root=ROOT).compile()
        serialized = json.dumps(compiled, sort_keys=True)
        for forbidden in (
            ".local/agent/reports",
            '"Run/',
            "/" + "Users/",
            "current-run",
        ):
            with self.subTest(forbidden=forbidden):
                self.assertNotIn(forbidden, serialized)

    def test_identity_order_reachability_kind_evidence_and_result_mutations_fail_closed(self) -> None:
        mutations = (
            "missing",
            "duplicate",
            "extra",
            "renamed",
            "reordered",
            "unreachable",
            "cross-kind",
            "evidence-swapped",
            "non-passed",
        )
        for mutation in mutations:
            with self.subTest(mutation=mutation), TemporaryDirectory() as directory:
                root = self.copy_source_root(directory)
                if mutation in {"missing", "duplicate", "extra", "renamed", "reordered"}:
                    path = root / "agent-workflows/groups/planning.json"
                    manifest = read_json(path)
                    contracts = manifest["contracts"]
                    if mutation == "missing":
                        contracts.pop()
                    elif mutation == "duplicate":
                        contracts[-1] = copy.deepcopy(contracts[0])
                    elif mutation == "extra":
                        contracts.append(copy.deepcopy(contracts[-1]))
                    elif mutation == "renamed":
                        contracts[0]["qualified_id"] = "group.D.DX"
                    else:
                        contracts[0], contracts[1] = contracts[1], contracts[0]
                    write_json(path, manifest)
                elif mutation == "unreachable":
                    path = root / "agent-workflows/workflows/resume-interrupted-work.json"
                    manifest = read_json(path)
                    manifest["stages"][0]["selectors"] = ["group.F.F7"]
                    write_json(path, manifest)
                elif mutation == "cross-kind":
                    path = root / "agent-workflows/evidence/contracts/D1.json"
                    evidence = read_json(path)
                    evidence["implementation_kind"] = "shared-protocol-operation"
                    write_json(path, evidence)
                    manifest_path = root / "agent-workflows/groups/planning.json"
                    manifest = read_json(manifest_path)
                    manifest["contracts"][0]["receipt_ref"]["digest"] = raw(path)
                    write_json(manifest_path, manifest)
                elif mutation == "evidence-swapped":
                    path = root / "agent-workflows/profiles/feature.json"
                    manifest = read_json(path)
                    one = manifest["contracts"][0]["evidence_ref"]
                    two = manifest["contracts"][1]["evidence_ref"]
                    manifest["contracts"][0]["evidence_ref"] = two
                    manifest["contracts"][1]["evidence_ref"] = one
                    write_json(path, manifest)
                else:
                    path = root / "agent-workflows/evidence/contracts/D1.json"
                    evidence = read_json(path)
                    evidence["status"] = "failed"
                    write_json(path, evidence)
                    manifest_path = root / "agent-workflows/groups/planning.json"
                    manifest = read_json(manifest_path)
                    manifest["contracts"][0]["receipt_ref"]["digest"] = raw(path)
                    write_json(manifest_path, manifest)
                with self.assertRaises(SourceReleaseError):
                    SourceReleaseV1(source_root=root).compile()

    def test_same_path_source_change_with_co_mutated_manifest_and_evidence_is_rejected(self) -> None:
        with TemporaryDirectory() as directory:
            root = self.copy_source_root(directory)
            source_path = root / "agent-workflows/skills/select-workflow-profile/SKILL.md"
            source_path.write_bytes(source_path.read_bytes() + b"\n")
            changed_digest = raw(source_path)
            manifest_path = root / "agent-workflows/groups/planning.json"
            manifest = read_json(manifest_path)
            contract = manifest["contracts"][0]
            contract["source_ref"]["digest"] = changed_digest
            evidence_path = root / contract["receipt_ref"]["path"]
            evidence = read_json(evidence_path)
            evidence["implementation_ref"]["digest"] = changed_digest
            write_json(evidence_path, evidence)
            contract["receipt_ref"]["digest"] = raw(evidence_path)
            write_json(manifest_path, manifest)

            compiled = SourceReleaseV1(source_root=root).compile()
            self.assertEqual(changed_digest, compiled["evidence_index"]["entries"][21]["source"]["digest"])
            with self.assertRaisesRegex(SourceReleaseError, "checked-in evidence index"):
                SourceReleaseV1(source_root=root).verify_checked_in()

    def test_tampered_counts_reordering_and_false_completion_claims_are_rejected(self) -> None:
        for mutation in ("count", "index-order", "full-ready"):
            with self.subTest(mutation=mutation), TemporaryDirectory() as directory:
                root = self.copy_source_root(directory)
                release_path = root / "agent-workflows/manifests/source-release.json"
                index_path = root / "agent-workflows/manifests/source-evidence-index.json"
                release = read_json(release_path)
                if mutation == "count":
                    release["coverage"]["named_contracts"]["accepted"] += 1
                elif mutation == "full-ready":
                    release["claims"]["full_workflow_ready"] = True
                    schema = read_json(root / "agent-workflows/schemas/source-release-v1.schema.json")
                    with self.assertRaises(SchemaValidationError):
                        validate_document(release, schema, schema.get("$defs", {}))
                else:
                    index = read_json(index_path)
                    index["entries"][0], index["entries"][1] = index["entries"][1], index["entries"][0]
                    index["entries_digest"] = "sha256:" + hashlib.sha256(
                        json.dumps(index["entries"], sort_keys=True, separators=(",", ":"), ensure_ascii=True).encode()
                    ).hexdigest()
                    index_path.write_bytes(serialize_manifest(index))
                    release["evidence_index"]["digest"] = raw(index_path)
                    release["evidence_index"]["entries_digest"] = index["entries_digest"]
                release_path.write_bytes(serialize_manifest(release))
                with self.assertRaises(SourceReleaseError):
                    SourceReleaseV1(source_root=root).verify_checked_in()

    def test_evidence_swaps_noncanonical_paths_and_removed_selectors_fail_closed(self) -> None:
        for mutation in ("surface-evidence-swap", "worker-report-path", "removed-selector"):
            with self.subTest(mutation=mutation), TemporaryDirectory() as directory:
                root = self.copy_source_root(directory)
                if mutation in {"surface-evidence-swap", "worker-report-path"}:
                    path = root / "agent-workflows/manifests/additional-required-surfaces.json"
                    manifest = read_json(path)
                    if mutation == "surface-evidence-swap":
                        first = manifest["surfaces"][0]["evidence"]
                        second = manifest["surfaces"][1]["evidence"]
                        first["evidence"], second["evidence"] = second["evidence"], first["evidence"]
                        first["evidence_digest"], second["evidence_digest"] = second["evidence_digest"], first["evidence_digest"]
                    else:
                        manifest["surfaces"][0]["evidence"]["evidence"] = ".local/agent/reports/forged.json"
                    write_json(path, manifest)
                else:
                    path = root / "agent-workflows/tests/test_planning_system.py"
                    source = path.read_text(encoding="utf-8")
                    source = source.replace("class D1_ProfileSelectionTests", "class RemovedD1_ProfileSelectionTests", 1)
                    path.write_text(source, encoding="utf-8")
                with self.assertRaises(SourceReleaseError):
                    SourceReleaseV1(source_root=root).compile()


if __name__ == "__main__":
    unittest.main()
