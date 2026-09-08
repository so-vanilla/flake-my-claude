"""Public-seam coverage tests for the S0 registry."""

from __future__ import annotations

import unittest
import json
import hashlib
import shutil
from pathlib import Path
from tempfile import TemporaryDirectory
from unittest.mock import patch

from ai_agent_workflow.catalog import CatalogError, compile_registry
from ai_agent_workflow import section_transition_evidence
from ai_agent_workflow.section_transition_evidence import verify_current_canonical_lineage


class CatalogCoverageTests(unittest.TestCase):
    def _copy_source_root(self, directory: str) -> Path:
        root = Path(directory)
        checkout = Path(__file__).parents[2]
        shutil.copytree(checkout / "agent-workflows", root / "agent-workflows")
        shutil.copytree(checkout / "docs" / "plans", root / "docs" / "plans")
        return root

    def test_compiles_qualified_targets_and_rejects_ambiguous_alias(self) -> None:
        projection = compile_registry()

        self.assertEqual(60, projection["counts"]["named_targets"])
        self.assertEqual(23, projection["counts"]["profile_targets"])
        self.assertEqual(11, projection["counts"]["additional_surfaces"])
        self.assertIn("group.F.F1", projection["target_registry"])
        self.assertIn("profile.feature.F1", projection["target_registry"])
        with self.assertRaises(CatalogError):
            compile_registry(selector="F1")

    def test_current_catalog_uses_versioned_lineage_and_rejects_v1_as_current(self) -> None:
        projection = compile_registry()
        self.assertEqual(60, projection["counts"]["named_targets"])

        with TemporaryDirectory() as directory:
            root = self._copy_source_root(directory)
            legacy = root / "agent-workflows" / "manifests" / "legacy-inventory.json"
            legacy.write_text(
                json.dumps(
                    {
                        "schema": "catalog-contract-inventory/v1",
                        "catalog_source": "docs/plans/ai-agent-workflow-step-catalog.md",
                        "catalog_digest": "sha256:"
                        + hashlib.sha256(
                            (root / "docs/plans/ai-agent-workflow-step-catalog.md").read_bytes()
                        ).hexdigest(),
                    }
                ),
                encoding="utf-8",
            )
            with self.assertRaisesRegex(CatalogError, "current canonical lineage"):
                compile_registry(source_root=root, inventory_path=legacy)

    def test_current_lineage_is_accepted_with_successor_authority(self) -> None:
        root = Path(__file__).parents[2]
        lineage_path = root / "agent-workflows/evidence/current-canonical-lineage.json"
        inventory_path = root / "agent-workflows/manifests/catalog-contract-inventory.json"
        lineage = json.loads(lineage_path.read_text(encoding="utf-8"))
        inventory = json.loads(inventory_path.read_text(encoding="utf-8"))

        self.assertTrue(verify_current_canonical_lineage(root))
        self.assertEqual(
            "sha256:61b8328298b04c3f90b4532bb89a99fa378ef8ee7e5cb88f71eadb233a850057",
            lineage["implementation_status"]["digest"],
        )
        self.assertEqual(
            "sha256:2359bc900a2124995c7347ab6ddb4e5e7353a1eb79c648d40cbdaf4fa943013f",
            inventory["current_lineage"]["digest"],
        )
        self.assertEqual(
            inventory["current_lineage"]["digest"],
            section_transition_evidence._CURRENT_LINEAGE_DIGEST,
        )
        self.assertEqual(
            "sha256:ac46857a453bca782e41f8f332ce42016338a685376de5b43c2a702d019014ae",
            section_transition_evidence._CHECKED_IN_AUTHORITY_DIGEST,
        )
        self.assertEqual(
            "sha256:9e4999bc80f86f75f5c8089f4571d4072212f12b0fa2dc7fe3c60469a80b7c4b",
            section_transition_evidence._CURRENT_INVENTORY_DIGEST,
        )
        bound_paths = {
            lineage["predecessor_authority"]["path"],
            lineage["implementation_status"]["path"],
            *(item["path"] for item in lineage["canonical_inputs"]),
            *(item["path"] for item in lineage["corrected_candidate"]["records"]),
        }
        self.assertTrue(
            {
                "agent-workflows/evidence/current-canonical-lineage.json",
                "agent-workflows/manifests/catalog-contract-inventory.json",
                "agent-workflows/src/ai_agent_workflow/section_transition_evidence.py",
            }.isdisjoint(bound_paths)
        )

    def test_current_lineage_rejects_reverted_status_authority(self) -> None:
        with TemporaryDirectory() as directory:
            root = self._copy_source_root(directory)
            evidence_path = root / "agent-workflows/evidence/current-canonical-lineage.json"
            evidence = json.loads(evidence_path.read_text(encoding="utf-8"))
            evidence["implementation_status"]["digest"] = (
                "sha256:8bf35ef467371042aa15fd3d5386aabdf39994af72b45d612e4f1038a74767da"
            )
            evidence_path.write_text(
                json.dumps(evidence, sort_keys=True, separators=(",", ":")) + "\n",
                encoding="utf-8",
            )
            replacement_digest = "sha256:" + hashlib.sha256(evidence_path.read_bytes()).hexdigest()
            inventory_path = root / "agent-workflows/manifests/catalog-contract-inventory.json"
            inventory = json.loads(inventory_path.read_text(encoding="utf-8"))
            inventory["current_lineage"]["digest"] = replacement_digest
            inventory_path.write_text(
                json.dumps(inventory, sort_keys=True, separators=(",", ":")) + "\n",
                encoding="utf-8",
            )
            inventory_digest = "sha256:" + hashlib.sha256(inventory_path.read_bytes()).hexdigest()
            with patch.object(
                section_transition_evidence, "_CURRENT_LINEAGE_DIGEST", replacement_digest
            ), patch.object(
                section_transition_evidence, "_CURRENT_INVENTORY_DIGEST", inventory_digest
            ):
                self.assertFalse(verify_current_canonical_lineage(root))

    def test_current_lineage_rejects_noncanonical_candidate_records(self) -> None:
        for mutation in ("omit", "reorder", "duplicate", "substitute", "stale"):
            with self.subTest(mutation=mutation), TemporaryDirectory() as directory:
                root = self._copy_source_root(directory)
                evidence_path = root / "agent-workflows/evidence/current-canonical-lineage.json"
                evidence = json.loads(evidence_path.read_text(encoding="utf-8"))
                records = evidence["corrected_candidate"]["records"]
                if mutation == "omit":
                    records.pop()
                elif mutation == "reorder":
                    records[0], records[1] = records[1], records[0]
                elif mutation == "duplicate":
                    records[-1] = dict(records[0])
                elif mutation == "substitute":
                    records[0]["digest"] = "sha256:" + "0" * 64
                else:
                    target = root / records[0]["path"]
                    target.write_bytes(target.read_bytes() + b"\n")
                evidence_path.write_text(
                    json.dumps(evidence, sort_keys=True, separators=(",", ":")) + "\n",
                    encoding="utf-8",
                )
                replacement_digest = "sha256:" + hashlib.sha256(evidence_path.read_bytes()).hexdigest()
                inventory_path = root / "agent-workflows/manifests/catalog-contract-inventory.json"
                inventory = json.loads(inventory_path.read_text(encoding="utf-8"))
                inventory["current_lineage"]["digest"] = replacement_digest
                inventory_path.write_text(
                    json.dumps(inventory, sort_keys=True, separators=(",", ":")) + "\n",
                    encoding="utf-8",
                )
                inventory_digest = "sha256:" + hashlib.sha256(
                    inventory_path.read_bytes()
                ).hexdigest()
                with patch.object(
                    section_transition_evidence, "_CURRENT_LINEAGE_DIGEST", replacement_digest
                ), patch.object(
                    section_transition_evidence, "_CURRENT_INVENTORY_DIGEST", inventory_digest
                ):
                    self.assertFalse(verify_current_canonical_lineage(root))

    def test_current_lineage_trust_digest_rejects_status_and_inventory_co_mutation(self) -> None:
        with TemporaryDirectory() as directory:
            root = self._copy_source_root(directory)
            evidence_path = root / "agent-workflows/evidence/current-canonical-lineage.json"
            evidence = json.loads(evidence_path.read_text(encoding="utf-8"))
            status_ref = evidence["implementation_status"]
            status_path = root / status_ref["path"]
            status_path.write_bytes(status_path.read_bytes() + b"\n")
            status_ref["digest"] = "sha256:" + hashlib.sha256(status_path.read_bytes()).hexdigest()
            evidence_path.write_text(
                json.dumps(evidence, sort_keys=True, separators=(",", ":")) + "\n",
                encoding="utf-8",
            )
            inventory_path = root / "agent-workflows/manifests/catalog-contract-inventory.json"
            inventory = json.loads(inventory_path.read_text(encoding="utf-8"))
            inventory["current_lineage"]["digest"] = (
                "sha256:" + hashlib.sha256(evidence_path.read_bytes()).hexdigest()
            )
            inventory_path.write_text(
                json.dumps(inventory, sort_keys=True, separators=(",", ":")) + "\n",
                encoding="utf-8",
            )
            self.assertFalse(verify_current_canonical_lineage(root))
            with self.assertRaisesRegex(CatalogError, "current canonical lineage"):
                compile_registry(source_root=root)

    def test_explicit_source_root_uses_its_copied_registry_package(self) -> None:
        with TemporaryDirectory() as directory:
            root = self._copy_source_root(directory)
            projection = compile_registry(source_root=root)

        self.assertEqual(60, projection["counts"]["named_targets"])
        self.assertEqual(23, projection["counts"]["profile_targets"])
        self.assertEqual(11, projection["counts"]["additional_surfaces"])

    def test_explicit_registry_inputs_cannot_escape_the_caller_source_root(self) -> None:
        checkout = Path(__file__).parents[2]
        external_inputs = {
            "catalog_path": checkout / "agent-workflows" / "catalog.yaml",
            "surfaces_path": checkout
            / "agent-workflows"
            / "manifests"
            / "additional-required-surfaces.json",
            "coverage_path": checkout
            / "agent-workflows"
            / "manifests"
            / "plan-coverage.json",
            "inventory_path": checkout
            / "agent-workflows"
            / "manifests"
            / "catalog-contract-inventory.json",
        }
        with TemporaryDirectory() as directory:
            root = self._copy_source_root(str(Path(directory) / "caller"))
            for argument, external_path in external_inputs.items():
                with self.subTest(argument=argument), self.assertRaisesRegex(
                    CatalogError, "path escapes the caller source root"
                ):
                    compile_registry(source_root=root, **{argument: external_path})

    def test_source_accepts_all_surfaces_and_preserves_non_ready_claims(self) -> None:
        projection = compile_registry()

        self.assertEqual(60, projection["counts"]["accepted_named"])
        self.assertEqual(23, projection["counts"]["accepted_profiles"])
        self.assertEqual(83, len(projection["source_inventory"]))
        self.assertEqual(83, len(projection["accepted_coverage"]))
        self.assertFalse(projection["claims"]["full_workflow_ready"])
        self.assertFalse(projection["claims"]["actual_a7_handoff_complete"])
        self.assertTrue(
            all(
                item["state"] == "accepted"
                and item["evidence"]["schema"] == "coverage-receipt/v1"
                and item["evidence"]["result"] == "passed"
                for item in projection["additional_surfaces"].values()
            )
        )

    def test_fails_closed_for_unknown_or_duplicate_coverage_references(self) -> None:
        fixture_dir = Path(__file__).parent / "fixtures" / "s0"
        expected = {
            "coverage-unknown.json": "unknown source inventory target group.Z.Z1",
            "coverage-duplicate-source.json": "duplicate source inventory target group.A.A6",
        }
        for fixture, message in expected.items():
            with self.subTest(fixture=fixture):
                with self.assertRaisesRegex(CatalogError, message):
                    compile_registry(coverage_path=fixture_dir / fixture)

    def test_rejects_same_count_catalog_identity_drift(self) -> None:
        with TemporaryDirectory() as directory:
            root = self._copy_source_root(directory)
            catalog_path = root / "agent-workflows" / "catalog.yaml"
            catalog = json.loads(catalog_path.read_text(encoding="utf-8"))
            catalog["named_contracts"]["A"][0][0] = "AX"
            drifted = root / "agent-workflows" / "tests" / "fixtures" / "s0" / "drifted-catalog.yaml"
            drifted.write_text(json.dumps(catalog), encoding="utf-8")
            with self.assertRaisesRegex(CatalogError, "canonical inventory mismatch"):
                compile_registry(source_root=root, catalog_path=drifted)

    def test_rejects_catalog_and_inventory_co_mutation_against_accepted_source(self) -> None:
        mutations = (
            ("named_contracts", "A", 0, 0, "AX"),
            ("named_contracts", "A", 0, 1, "renamed-contract"),
            ("profile_steps", "feature", 0, 0, "F9"),
        )
        for collection, owner, index, field, value in mutations:
            with self.subTest(value=value), TemporaryDirectory() as directory:
                root = self._copy_source_root(directory)
                package = root / "agent-workflows"
                catalog = json.loads((package / "catalog.yaml").read_text(encoding="utf-8"))
                inventory = json.loads((package / "manifests" / "catalog-contract-inventory.json").read_text(encoding="utf-8"))
                changed_catalog = json.loads(json.dumps(catalog))
                changed_inventory = json.loads(json.dumps(inventory))
                changed_catalog[collection][owner][index][field] = value
                # A former co-mutation route: the verification manifest cannot
                # carry a replacement registry because its schema is closed.
                changed_inventory[collection] = changed_catalog[collection]
                catalog_path = package / "tests" / "fixtures" / "s0" / "changed-catalog.yaml"
                inventory_path = package / "tests" / "fixtures" / "s0" / "changed-inventory.json"
                catalog_path.write_text(json.dumps(changed_catalog), encoding="utf-8")
                inventory_path.write_text(json.dumps(changed_inventory), encoding="utf-8")
                with self.assertRaises(CatalogError):
                    compile_registry(
                        source_root=root,
                        catalog_path=catalog_path,
                        inventory_path=inventory_path,
                    )

    def test_projects_schema_bound_receipts_and_fail_closed_states(self) -> None:
        projection = compile_registry()
        accepted = projection["target_registry"]["group.A.A6"]
        self.assertEqual("accepted", accepted["state"])
        self.assertEqual("v1", accepted["version"])
        self.assertEqual("A6_WalkingSkeletonTests", accepted["acceptance_selector"])
        self.assertEqual("passed", accepted["receipt"]["result"])
        self.assertEqual("accepted", projection["target_registry"]["group.A.A1"]["state"])
        self.assertEqual("user-behavior", projection["target_registry"]["profile.feature.F1"]["name"])
        self.assertEqual("accepted", projection["additional_surfaces"]["surface.config-installer-guardrail"]["state"])
        self.assertEqual("agent-workflows/manifests/model-policy.json", projection["additional_surfaces"]["surface.model-policy"]["canonical_source"])

    def test_rejects_receipt_selector_not_bound_by_its_evidence(self) -> None:
        with TemporaryDirectory() as directory:
            root = self._copy_source_root(directory)
            package = root / "agent-workflows"
            coverage = json.loads(
                (package / "manifests" / "plan-coverage.json").read_text(encoding="utf-8")
            )
            coverage["source_inventory"][0]["receipt"]["selector"] = "unrelated-selector"
            forged = package / "tests" / "fixtures" / "s0" / "forged-coverage.json"
            forged.write_text(json.dumps(coverage), encoding="utf-8")
            with self.assertRaisesRegex(CatalogError, "receipt selector does not match evidence"):
                compile_registry(source_root=root, coverage_path=forged)

    def test_rejects_accepted_state_without_matching_surface_evidence(self) -> None:
        with TemporaryDirectory() as directory:
            root = self._copy_source_root(directory)
            package = root / "agent-workflows"
            surfaces = json.loads(
                (package / "manifests" / "additional-required-surfaces.json").read_text(encoding="utf-8")
            )
            surfaces["surfaces"][0]["evidence"] = None
            forged = package / "tests" / "fixtures" / "s0" / "forged-surfaces.json"
            forged.write_text(json.dumps(surfaces), encoding="utf-8")
            with self.assertRaisesRegex(CatalogError, "accepted surface lacks matching evidence"):
                compile_registry(source_root=root, surfaces_path=forged)

    def test_rejects_passing_receipt_omitted_from_accepted_coverage(self) -> None:
        with TemporaryDirectory() as directory:
            root = self._copy_source_root(directory)
            package = root / "agent-workflows"
            coverage = json.loads(
                (package / "manifests" / "plan-coverage.json").read_text(encoding="utf-8")
            )
            coverage["accepted_coverage"].pop()
            forged = package / "tests" / "fixtures" / "s0" / "forged-coverage.json"
            forged.write_text(json.dumps(coverage), encoding="utf-8")
            with self.assertRaisesRegex(CatalogError, "accepted coverage does not match passing receipts"):
                compile_registry(source_root=root, coverage_path=forged)

    def test_rejects_bare_f1_evidence_replayed_across_qualified_namespaces(self) -> None:
        with TemporaryDirectory() as directory:
            root = self._copy_source_root(directory)
            package = root / "agent-workflows"
            coverage = json.loads(
                (package / "manifests" / "plan-coverage.json").read_text(encoding="utf-8")
            )
            source = "docs/plans/ai-agent-workflow-step-catalog.md"
            source_digest = "sha256:" + hashlib.sha256((root / source).read_bytes()).hexdigest()
            evidence_relative = "agent-workflows/tests/fixtures/s0/F1.json"
            evidence_path = root / evidence_relative
            evidence = {"contract_id": "F1", "contract_name": "audit-group-purpose", "coverage_kind": "named-skill", "implementation_ref": {"path": source, "digest": source_digest}, "test_ref": {"selector": "F1_Test"}, "status": "passed"}
            evidence_path.write_text(json.dumps(evidence), encoding="utf-8")
            evidence_digest = "sha256:" + hashlib.sha256(evidence_path.read_bytes()).hexdigest()
            coverage["source_inventory"] = [{"id": "group.F.F1", "receipt": {"schema": "coverage-receipt/v1", "subject_id": "group.F.F1", "source": source, "source_digest": source_digest, "evidence": evidence_relative, "evidence_digest": evidence_digest, "selector": "F1_Test", "result": "passed"}}]
            coverage["accepted_coverage"] = ["group.F.F1"]
            coverage_path = package / "manifests" / "coverage.json"
            coverage_path.write_text(json.dumps(coverage), encoding="utf-8")
            with self.assertRaisesRegex(CatalogError, "ambiguous evidence contract id F1"):
                compile_registry(source_root=root, coverage_path=coverage_path)

    def test_qualified_f1_evidence_binds_only_its_namespace(self) -> None:
        with TemporaryDirectory() as directory:
            root = self._copy_source_root(directory)
            package = root / "agent-workflows"
            coverage = json.loads(
                (package / "manifests" / "plan-coverage.json").read_text(encoding="utf-8")
            )
            source = "docs/plans/ai-agent-workflow-step-catalog.md"
            source_digest = "sha256:" + hashlib.sha256((root / source).read_bytes()).hexdigest()
            evidence_relative = "agent-workflows/tests/fixtures/s0/group-f1.json"
            evidence_path = root / evidence_relative
            evidence = {"schema": "qualified-contract-acceptance/v1", "contract_id": "group.F.F1", "contract_name": "audit-group-purpose", "implementation_kind": "shared-protocol-operation", "implementation_ref": {"path": source, "digest": source_digest}, "test_ref": {"selector": "group.F.F1.acceptance"}, "status": "passed"}
            evidence_path.write_text(json.dumps(evidence), encoding="utf-8")
            evidence_digest = "sha256:" + hashlib.sha256(evidence_path.read_bytes()).hexdigest()
            receipt = {"schema": "coverage-receipt/v1", "subject_id": "group.F.F1", "source": source, "source_digest": source_digest, "evidence": evidence_relative, "evidence_digest": evidence_digest, "selector": "group.F.F1.acceptance", "result": "passed"}
            coverage["source_inventory"] = [{"id": "group.F.F1", "receipt": receipt}]
            coverage["accepted_coverage"] = ["group.F.F1"]
            coverage_path = package / "manifests" / "coverage.json"
            coverage_path.write_text(json.dumps(coverage), encoding="utf-8")
            projection = compile_registry(source_root=root, coverage_path=coverage_path)
            self.assertEqual("accepted", projection["target_registry"]["group.F.F1"]["state"])
            self.assertEqual("group.F.F1.acceptance", projection["target_registry"]["group.F.F1"]["acceptance_selector"])
            coverage["source_inventory"][0]["id"] = "profile.feature.F1"
            coverage["source_inventory"][0]["receipt"]["subject_id"] = "profile.feature.F1"
            coverage["accepted_coverage"] = ["profile.feature.F1"]
            coverage_path.write_text(json.dumps(coverage), encoding="utf-8")
            with self.assertRaisesRegex(CatalogError, "receipt subject does not match evidence"):
                compile_registry(source_root=root, coverage_path=coverage_path)


if __name__ == "__main__":
    unittest.main()
