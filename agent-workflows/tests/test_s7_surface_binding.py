"""Fail-closed source and purpose-evidence binding for all S7 surfaces."""

from __future__ import annotations

import copy
import hashlib
import json
import shutil
import unittest
from pathlib import Path
from tempfile import TemporaryDirectory

from ai_agent_workflow.catalog import CatalogError, compile_registry


ROOT = Path(__file__).resolve().parents[2]
MANIFEST = "agent-workflows/manifests/additional-required-surfaces.json"
EXPECTED = (
    (
        "surface.model-policy",
        "agent-workflows/manifests/model-policy.json",
        "sha256:eb7e45733239472c05158bc4611bb120a1fa60930a075ef08f6b0003c5060b50",
        "effective-model receipt",
        "model-policy",
        "agent-workflows/evidence/surfaces/personal/model-policy.json",
        "S7PersonalSurfaceTests.test_model_policy_surface",
    ),
    (
        "surface.config-installer-guardrail",
        "agent-workflows/config-installer.py",
        "sha256:e3b06c6768755654d119d8c60d17ac96ed3f005bb84d47be033064f42c3a9f17",
        "managed config merge",
        "config-installer-guardrail",
        "agent-workflows/evidence/surfaces/personal/config-installer-guardrail.json",
        "S7PersonalSurfaceTests.test_config_installer_surface",
    ),
    (
        "surface.owner-distribution-manifest",
        "agent-workflows/manifests/distribution.json",
        "sha256:2c15773ddc924ac721dae4ec077fa77462b2779f34037a5034fe810d8fed2ee1",
        "owner manifest",
        "owner-distribution-manifest",
        "agent-workflows/evidence/surfaces/personal/owner-distribution-manifest.json",
        "S7PersonalSurfaceTests.test_distribution_manifest_surface",
    ),
    (
        "surface.provider-projection",
        "agent-workflows/provider-projection.py",
        "sha256:cc918dd759d4a5c33a5aaab2239646fc9f507bca1a34de7f3095011bd4890504",
        "provider projection",
        "provider-projection",
        "agent-workflows/evidence/surfaces/personal/provider-projection.json",
        "S7PersonalSurfaceTests.test_provider_projection_surface",
    ),
    (
        "surface.company.classify-data",
        "agent-workflows/company/classify-data.py",
        "sha256:96d3f1bea4cc0a8c274a806682fb371af929b5b341b3da3cf019cfc61111e12b",
        "data policy",
        "classify-data",
        "agent-workflows/evidence/surfaces/company/classify-data.json",
        "CompanyGovernanceTests.test_all_seven_compile_in_exact_order_as_static_non_authorizing_candidates",
    ),
    (
        "surface.company.resolve-identity",
        "agent-workflows/company/resolve-identity.py",
        "sha256:41370c752859f9ed5262123ffcee59ebc3d89de9578c9a88473529da1b730b38",
        "identity receipt",
        "resolve-identity",
        "agent-workflows/evidence/surfaces/company/resolve-identity.json",
        "CompanyGovernanceTests.test_all_seven_compile_in_exact_order_as_static_non_authorizing_candidates",
    ),
    (
        "surface.company.authorize-tools",
        "agent-workflows/company/authorize-tools.py",
        "sha256:2529c3fa8591c1e298ab68ce1a080fc637df871a3e9af16858b5ba9783715e2a",
        "effective policy readback",
        "authorize-tools",
        "agent-workflows/evidence/surfaces/company/authorize-tools.json",
        "CompanyGovernanceTests.test_all_seven_compile_in_exact_order_as_static_non_authorizing_candidates",
    ),
    (
        "surface.company.approve-catalog",
        "agent-workflows/company/approve-catalog.py",
        "sha256:c38fbf8fb5a16f371168c7577490709b8ac76dea2a5dbad4b5c739e2f8429758",
        "approved manifest",
        "approve-catalog",
        "agent-workflows/evidence/surfaces/company/approve-catalog.json",
        "CompanyGovernanceTests.test_all_seven_compile_in_exact_order_as_static_non_authorizing_candidates",
    ),
    (
        "surface.company.evaluate-change",
        "agent-workflows/company/evaluate-change.py",
        "sha256:7760c6b6cdcc48c9c7e8c28fe0bea902e41fb3f95bcac888668cbc98fea72e56",
        "comparative eval",
        "evaluate-change",
        "agent-workflows/evidence/surfaces/company/evaluate-change.json",
        "CompanyGovernanceTests.test_all_seven_compile_in_exact_order_as_static_non_authorizing_candidates",
    ),
    (
        "surface.company.release-workflow",
        "agent-workflows/company/release-workflow.py",
        "sha256:4ef93aabfb6ed5f7d518f77ffc3d0f480f1c348abe883b521c9fac5f1bbaa710",
        "release receipt",
        "release-workflow",
        "agent-workflows/evidence/surfaces/company/release-workflow.json",
        "CompanyGovernanceTests.test_all_seven_compile_in_exact_order_as_static_non_authorizing_candidates",
    ),
    (
        "surface.company.audit-operation",
        "agent-workflows/company/audit-operation.py",
        "sha256:eb865164e896e4e638d0dd510564b0e8211566b018f7ed59f2035463e158ae00",
        "audit report",
        "audit-operation",
        "agent-workflows/evidence/surfaces/company/audit-operation.json",
        "CompanyGovernanceTests.test_all_seven_compile_in_exact_order_as_static_non_authorizing_candidates",
    ),
)


def _digest(path: Path) -> str:
    return "sha256:" + hashlib.sha256(path.read_bytes()).hexdigest()


class S7SurfaceBindingTests(unittest.TestCase):
    maxDiff = None

    def _copy_source_root(self, directory: str) -> Path:
        root = Path(directory)
        shutil.copytree(ROOT / "agent-workflows", root / "agent-workflows")
        shutil.copytree(ROOT / "docs" / "plans", root / "docs" / "plans")
        return root

    @staticmethod
    def _read_manifest(root: Path) -> dict:
        return json.loads((root / MANIFEST).read_text(encoding="utf-8"))

    @staticmethod
    def _write_json(path: Path, value: object) -> None:
        path.write_text(
            json.dumps(value, indent=2, sort_keys=True) + "\n", encoding="utf-8"
        )

    def _write_manifest(self, root: Path, manifest: dict) -> None:
        self._write_json(root / MANIFEST, manifest)

    def test_projects_exact_eleven_accepted_bindings_without_live_claims(self) -> None:
        projection = compile_registry()
        surfaces = projection["additional_surfaces"]
        self.assertEqual([item[0] for item in EXPECTED], list(surfaces))
        self.assertEqual(11, len(surfaces))
        for (
            identifier,
            source,
            source_digest,
            interface_ref,
            logical_selector,
            evidence,
            test_selector,
        ) in EXPECTED:
            with self.subTest(identifier=identifier):
                entry = surfaces[identifier]
                receipt = entry["evidence"]
                self.assertEqual("accepted", entry["state"])
                self.assertEqual("agent-workflows", entry["owner_module"])
                self.assertEqual("S7", entry["section_owner"])
                self.assertEqual(interface_ref, entry["interface_ref"])
                self.assertEqual(logical_selector, entry["acceptance_selector"])
                self.assertEqual(source, entry["canonical_source"])
                self.assertEqual(source_digest, entry["canonical_source_digest"])
                self.assertEqual(source_digest, _digest(ROOT / source))
                self.assertEqual(identifier, receipt["subject_id"])
                self.assertEqual(source, receipt["source"])
                self.assertEqual(source_digest, receipt["source_digest"])
                self.assertEqual(evidence, receipt["evidence"])
                self.assertEqual(_digest(ROOT / evidence), receipt["evidence_digest"])
                self.assertEqual(test_selector, receipt["selector"])
                self.assertEqual("passed", receipt["result"])
        self.assertFalse(projection["claims"]["full_workflow_ready"])
        self.assertFalse(projection["claims"]["actual_a7_handoff_complete"])

    def test_rejects_manifest_identity_order_and_binding_mutations(self) -> None:
        mutations = (
            "missing",
            "duplicate",
            "extra",
            "reordered",
            "renamed",
            "source-swapped",
            "evidence-swapped",
            "selector-swapped",
            "logical-selector-swapped",
            "digest-swapped",
            "state-swapped",
        )
        for mutation in mutations:
            with self.subTest(mutation=mutation), TemporaryDirectory() as directory:
                root = self._copy_source_root(directory)
                manifest = self._read_manifest(root)
                surfaces = manifest["surfaces"]
                if mutation == "missing":
                    surfaces.pop()
                elif mutation == "duplicate":
                    surfaces[-1] = copy.deepcopy(surfaces[0])
                elif mutation == "extra":
                    surfaces.append(copy.deepcopy(surfaces[-1]))
                elif mutation == "reordered":
                    surfaces[0], surfaces[1] = surfaces[1], surfaces[0]
                elif mutation == "renamed":
                    surfaces[0]["id"] = "surface.renamed"
                elif mutation == "source-swapped":
                    for field in ("canonical_source", "canonical_source_digest"):
                        surfaces[0][field], surfaces[1][field] = (
                            surfaces[1][field],
                            surfaces[0][field],
                        )
                    for field in ("source", "source_digest"):
                        surfaces[0]["evidence"][field], surfaces[1]["evidence"][field] = (
                            surfaces[1]["evidence"][field],
                            surfaces[0]["evidence"][field],
                        )
                elif mutation == "evidence-swapped":
                    for field in ("evidence", "evidence_digest"):
                        surfaces[0]["evidence"][field], surfaces[1]["evidence"][field] = (
                            surfaces[1]["evidence"][field],
                            surfaces[0]["evidence"][field],
                        )
                elif mutation == "selector-swapped":
                    surfaces[0]["evidence"]["selector"], surfaces[1]["evidence"]["selector"] = (
                        surfaces[1]["evidence"]["selector"],
                        surfaces[0]["evidence"]["selector"],
                    )
                elif mutation == "logical-selector-swapped":
                    surfaces[0]["acceptance_selector"], surfaces[1]["acceptance_selector"] = (
                        surfaces[1]["acceptance_selector"],
                        surfaces[0]["acceptance_selector"],
                    )
                elif mutation == "digest-swapped":
                    surfaces[0]["canonical_source_digest"] = "sha256:" + "0" * 64
                else:
                    surfaces[0]["state"] = "present_unaccepted"
                self._write_manifest(root, manifest)
                with self.assertRaises(CatalogError):
                    compile_registry(source_root=root)

    def test_rejects_same_path_changed_bytes_even_when_all_digests_are_rebound(self) -> None:
        with TemporaryDirectory() as directory:
            root = self._copy_source_root(directory)
            manifest = self._read_manifest(root)
            entry = manifest["surfaces"][0]
            source_path = root / entry["canonical_source"]
            source_path.write_bytes(source_path.read_bytes() + b"\n")
            changed_source_digest = _digest(source_path)
            entry["canonical_source_digest"] = changed_source_digest
            entry["evidence"]["source_digest"] = changed_source_digest
            evidence_path = root / entry["evidence"]["evidence"]
            evidence = json.loads(evidence_path.read_text(encoding="utf-8"))
            evidence["canonical_source"]["digest"] = changed_source_digest
            self._write_json(evidence_path, evidence)
            entry["evidence"]["evidence_digest"] = _digest(evidence_path)
            self._write_manifest(root, manifest)
            with self.assertRaisesRegex(CatalogError, "noncanonical canonical_source_digest"):
                compile_registry(source_root=root)

    def test_rejects_digest_bound_arbitrary_json_and_unknown_evidence_keys(self) -> None:
        for mutation in ("arbitrary", "unknown-key"):
            with self.subTest(mutation=mutation), TemporaryDirectory() as directory:
                root = self._copy_source_root(directory)
                manifest = self._read_manifest(root)
                entry = manifest["surfaces"][0]
                evidence_path = root / entry["evidence"]["evidence"]
                if mutation == "arbitrary":
                    evidence = {"schema": "arbitrary/v1", "result": "passed"}
                else:
                    evidence = json.loads(evidence_path.read_text(encoding="utf-8"))
                    evidence["proof"]["unreviewed_claim"] = True
                self._write_json(evidence_path, evidence)
                entry["evidence"]["evidence_digest"] = _digest(evidence_path)
                self._write_manifest(root, manifest)
                with self.assertRaises(CatalogError):
                    compile_registry(source_root=root)

    def test_rejects_cross_surface_evidence_with_a_matching_digest(self) -> None:
        with TemporaryDirectory() as directory:
            root = self._copy_source_root(directory)
            manifest = self._read_manifest(root)
            entry = manifest["surfaces"][0]
            other = manifest["surfaces"][1]
            evidence_path = root / entry["evidence"]["evidence"]
            other_evidence = json.loads(
                (root / other["evidence"]["evidence"]).read_text(encoding="utf-8")
            )
            self._write_json(evidence_path, other_evidence)
            entry["evidence"]["evidence_digest"] = _digest(evidence_path)
            self._write_manifest(root, manifest)
            with self.assertRaisesRegex(CatalogError, "surface evidence subject"):
                compile_registry(source_root=root)

    def test_rejects_result_live_and_approval_claim_forgery(self) -> None:
        for mutation in ("result", "live", "approval"):
            with self.subTest(mutation=mutation), TemporaryDirectory() as directory:
                root = self._copy_source_root(directory)
                manifest = self._read_manifest(root)
                entry = manifest["surfaces"][4]
                evidence_path = root / entry["evidence"]["evidence"]
                evidence = json.loads(evidence_path.read_text(encoding="utf-8"))
                if mutation == "result":
                    evidence["acceptance"]["result"] = "failed"
                elif mutation == "live":
                    evidence["claim_boundary"]["live_company_system_used"] = True
                else:
                    evidence["claim_boundary"]["approval_minted"] = True
                self._write_json(evidence_path, evidence)
                entry["evidence"]["evidence_digest"] = _digest(evidence_path)
                self._write_manifest(root, manifest)
                with self.assertRaises(CatalogError):
                    compile_registry(source_root=root)

    def test_rejects_selector_that_no_longer_exists_in_canonical_test_module(self) -> None:
        with TemporaryDirectory() as directory:
            root = self._copy_source_root(directory)
            test_module = root / "agent-workflows/tests/test_s7_personal_surfaces.py"
            source = test_module.read_text(encoding="utf-8")
            source = source.replace(
                "def test_model_policy_surface(self)",
                "def removed_model_policy_surface(self)",
                1,
            )
            test_module.write_text(source, encoding="utf-8")
            with self.assertRaisesRegex(CatalogError, "non-executable purpose test selector"):
                compile_registry(source_root=root)


if __name__ == "__main__":
    unittest.main()
