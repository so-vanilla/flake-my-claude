import copy
import hashlib
import json
import shutil
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch


ROOT = Path(__file__).resolve().parents[1]
REPOSITORY_ROOT = ROOT.parent
sys.path.insert(0, str(ROOT / "src"))

from ai_agent_workflow.section_transition_evidence import (  # noqa: E402
    verify_current_canonical_lineage,
    verify_section_transition_compatibility,
)
from ai_agent_workflow import section_transition_evidence  # noqa: E402
from ai_agent_workflow.catalog import compile_registry  # noqa: E402
from ai_agent_workflow.control_kernel import ControlKernel  # noqa: E402
from ai_agent_workflow.section_control_plane import (  # noqa: E402
    SectionControlPlaneV1,
    open_group,
    open_section,
    resume,
)


class SectionTransitionCompatibilityEvidenceTests(unittest.TestCase):
    _HISTORICAL_TEST_PATH = (
        "agent-workflows/tests/test_section_transition_compatibility.py"
    )
    _HISTORICAL_TEST_DIGEST = (
        "sha256:7b92590ad71cccbb2ca1bdd89da1d4f6a8ced970d74a6f396b752265eca7d5f0"
    )
    _AUTHORITY_COMMON_FIELDS = (
        "compatibility_id",
        "version",
        "workspace_git_head",
        "expected_head",
        "sources",
        "schemas",
        "inputs",
        "s0_close",
        "test_receipts",
        "review_validation_history",
        "claims",
    )

    def test_missing_fixed_authority_fails_closed(self):
        with tempfile.TemporaryDirectory() as directory:
            self.assertFalse(verify_section_transition_compatibility(Path(directory)))

    def test_successor_compatibility_accepts_current_s2_u_bytes(self):
        self.assertTrue(verify_section_transition_compatibility(REPOSITORY_ROOT))

    def test_downstream_lifecycle_test_is_outside_current_compatibility_bytes(self):
        lifecycle_path = "agent-workflows/tests/test_s1_lifecycle_integration.py"
        evidence = self._load(
            REPOSITORY_ROOT / "agent-workflows/evidence/compatibility/S0-S1-transition.json"
        )
        authority = self._load(
            REPOSITORY_ROOT
            / "agent-workflows/manifests/section-transition-compatibility-authority.json"
        )
        self.assertNotIn(lifecycle_path, {item["path"] for item in evidence["inputs"]})
        self.assertNotIn(lifecycle_path, {item["path"] for item in authority["inputs"]})

        root = self._source_copy()
        lifecycle_test = root / lifecycle_path
        lifecycle_test.write_text(
            lifecycle_test.read_text(encoding="utf-8") + "\n# later lifecycle-only change\n",
            encoding="utf-8",
        )
        self.assertTrue(verify_section_transition_compatibility(root))

    def test_current_compatibility_test_evolves_without_rewriting_historical_receipt(self):
        evidence = self._load(
            REPOSITORY_ROOT / "agent-workflows/evidence/compatibility/S0-S1-transition.json"
        )
        authority = self._load(
            REPOSITORY_ROOT
            / "agent-workflows/manifests/section-transition-compatibility-authority.json"
        )
        for document in (evidence, authority):
            recorded = [
                item
                for collection in (document["sources"], document["inputs"])
                for item in collection
                if item["path"] == self._HISTORICAL_TEST_PATH
            ]
            self.assertEqual(2, len(recorded))
            self.assertEqual(
                {self._HISTORICAL_TEST_DIGEST},
                {item["digest"] for item in recorded},
            )
        self.assertNotEqual(
            self._HISTORICAL_TEST_DIGEST,
            self._digest(REPOSITORY_ROOT / self._HISTORICAL_TEST_PATH),
        )
        self.assertTrue(verify_section_transition_compatibility(REPOSITORY_ROOT))

    def test_coherently_redigested_historical_test_receipt_is_rejected(self):
        root = self._source_copy()
        evidence_path = root / "agent-workflows/evidence/compatibility/S0-S1-transition.json"
        evidence = self._load(evidence_path)
        replacement = self._digest(root / self._HISTORICAL_TEST_PATH)
        self.assertNotEqual(self._HISTORICAL_TEST_DIGEST, replacement)
        for collection in (evidence["sources"], evidence["inputs"]):
            for item in collection:
                if item["path"] == self._HISTORICAL_TEST_PATH:
                    item["digest"] = replacement
        self._save(evidence_path, evidence)
        substituted_digest = self._redigest_compatibility(root)
        with patch.object(
            section_transition_evidence, "_AUTHORITY_DIGEST", substituted_digest
        ):
            self.assertFalse(verify_section_transition_compatibility(root))

    def test_s2_u_product_closure_is_bound_without_self_binding(self):
        evidence = self._load(
            REPOSITORY_ROOT / "agent-workflows/evidence/compatibility/S0-S1-transition.json"
        )
        refs = {
            item["path"]: item["digest"]
            for collection in (evidence["sources"], evidence["schemas"], evidence["inputs"])
            for item in collection
        }
        expected = {
            "agent-workflows/src/ai_agent_workflow/control_kernel.py":
                "sha256:dee09e3d769ae3399236f33acabca317af7c814ce4c67557fe0275aeb5d0e2e0",
            "agent-workflows/schemas/dag-command-v1.schema.json":
                "sha256:f5825cb55b0380fe97a8a0db090d74ad9a0b81a2015e5b1d25e02665af41a5cb",
            "agent-workflows/schemas/dag-state-v1.schema.json":
                "sha256:d58d79e0005ecf2a5929ed8f0ef74672eb21daac7bcaf57090c6d4562bda153d",
            "agent-workflows/schemas/objective-approval-v1.schema.json":
                "sha256:96f84e7dbb02b661a06219376dab9a540f9a6ad2545922a29827a8c29748c033",
            "agent-workflows/tests/test_control_kernel.py":
                "sha256:024e048887d461d6ad40ff2336d97e09c80c26ad1c4471e3ef57a722a8296e06",
            "agent-workflows/tests/test_objective_transition_compatibility.py":
                "sha256:2de754b933ec216c7ebf901308d42de0890df04d18dec506f95f40eb1ba75a0f",
            "agent-workflows/evidence/compatibility/S2-objective-transition.json":
                "sha256:e00285c0f2843ba52ea4df9f482c551e22953d73bd4d2f5a4d1ea7f448789cee",
        }
        self.assertEqual(expected, {path: refs[path] for path in expected})
        product_evidence = self._load(
            REPOSITORY_ROOT
            / "agent-workflows/evidence/compatibility/S2-objective-transition.json"
        )
        self.assertNotIn(
            "agent-workflows/evidence/compatibility/S2-objective-transition.json",
            product_evidence["bound_product_digests"],
        )

    def test_close_set_has_the_fresh_canonical_review_history(self):
        expected_history = [
            {
                "unit": "U-A",
                "candidate_digest": "sha256:a63392d56d23f6fb6c461db830d4f8dca0789b236f7894c9d62810d201a22ebd",
                "closure_validation": {
                    "path": ".local/agent/reports/ai-agent-workflow-full-implementation/s1-c-upstream-closure-source-001.md",
                    "digest": "sha256:23c605b904db6278de35902ec0bf9a9d8384f5ca6bfbd124a696e9131b92dab3",
                },
                "verdict": "accepted",
                "required_open": 0,
                "needs_user": 0,
                "test_evidence_debt": 0,
            },
            {
                "unit": "U-B",
                "candidate_digest": "sha256:1179cfe031b9539250099e1e1f005adaa4b486388555d2699c7ea94d193d7bc0",
                "closure_validation": {
                    "path": ".local/agent/reports/ai-agent-workflow-full-implementation/s1-c-upstream-closure-integration-001.md",
                    "digest": "sha256:522840980fa311e86ac3c0fad73812c232f8349da9275368eab08b930b6041b6",
                },
                "verdict": "accepted",
                "required_open": 0,
                "needs_user": 0,
                "test_evidence_debt": 0,
            },
            {
                "unit": "U-C",
                "candidate_digest": "sha256:a0e36ab8d9968deaccfb0c064d7f5c2d75ad8552a0dffc2d1d89f5d285a71352",
                "closure_security": {
                    "path": ".local/agent/reports/ai-agent-workflow-full-implementation/s1-c-upstream-closure-source-001.md",
                    "digest": "sha256:23c605b904db6278de35902ec0bf9a9d8384f5ca6bfbd124a696e9131b92dab3",
                },
                "closure_coverage": {
                    "path": ".local/agent/reports/ai-agent-workflow-full-implementation/s1-c-upstream-closure-integration-001.md",
                    "digest": "sha256:522840980fa311e86ac3c0fad73812c232f8349da9275368eab08b930b6041b6",
                },
                "closure_validation": {
                    "path": ".local/agent/reports/ai-agent-workflow-full-implementation/s1-c-upstream-closure-validation-001.md",
                    "digest": "sha256:6b5242e71a401605e24d7591c144f145d85aa56100b9e7617bce1ad0bcf4be35",
                },
                "verdict": "accepted",
                "required_open": 0,
                "needs_user": 0,
                "test_evidence_debt": 0,
            },
        ]
        evidence = self._load(
            REPOSITORY_ROOT / "agent-workflows/evidence/compatibility/S0-S1-transition.json"
        )
        authority = self._load(
            REPOSITORY_ROOT
            / "agent-workflows/manifests/section-transition-compatibility-authority.json"
        )
        self.assertEqual(
            [record["unit"] for record in evidence["review_validation_history"]],
            ["U-A", "U-B", "U-C"],
        )
        self.assertEqual(evidence["review_validation_history"], expected_history)
        self.assertEqual(authority["review_validation_history"], evidence["review_validation_history"])

    def test_u_c_review_history_rejects_noncanonical_records_after_rotation(self):
        def missing(history):
            history.pop()

        def reordered(history):
            history[1], history[2] = history[2], history[1]

        def duplicated(history):
            history.append(copy.deepcopy(history[-1]))

        def extra(history):
            record = copy.deepcopy(history[-1])
            record["unit"] = "U-D"
            history.append(record)

        def malformed(history):
            del history[-1]["closure_coverage"]

        def substituted(history):
            history[-1]["closure_security"] = copy.deepcopy(history[-1]["closure_coverage"])

        cases = {
            "missing": missing,
            "reordered": reordered,
            "duplicated": duplicated,
            "extra": extra,
            "malformed": malformed,
            "substituted": substituted,
        }
        for name, mutate in cases.items():
            with self.subTest(name=name):
                root = self._source_copy()
                evidence_path = root / "agent-workflows/evidence/compatibility/S0-S1-transition.json"
                evidence = self._load(evidence_path)
                mutate(evidence["review_validation_history"])
                self._save(evidence_path, evidence)
                replacement_digest = self._redigest_compatibility(root)
                self.assertFalse(verify_section_transition_compatibility(root))
                with patch.object(
                    section_transition_evidence,
                    "_AUTHORITY_DIGEST",
                    replacement_digest,
                ):
                    self.assertFalse(verify_section_transition_compatibility(root))

    def test_u_c_closure_report_bytes_require_coherent_replacement(self):
        for field in ("closure_security", "closure_coverage", "closure_validation"):
            with self.subTest(field=field):
                root = self._source_copy()
                evidence_path = root / "agent-workflows/evidence/compatibility/S0-S1-transition.json"
                evidence = self._load(evidence_path)
                report = root / evidence["review_validation_history"][-1][field]["path"]
                report.write_text(report.read_text(encoding="utf-8") + "\n", encoding="utf-8")
                replacement_digest = self._redigest_compatibility(root)
                self.assertFalse(verify_section_transition_compatibility(root))
                with patch.object(
                    section_transition_evidence,
                    "_AUTHORITY_DIGEST",
                    replacement_digest,
                ):
                    self.assertTrue(verify_section_transition_compatibility(root))
                    self.assertFalse(verify_current_canonical_lineage(root))

    def _source_copy(self):
        root = Path(tempfile.mkdtemp())
        self.addCleanup(shutil.rmtree, root, ignore_errors=True)
        shutil.copytree(REPOSITORY_ROOT / "agent-workflows", root / "agent-workflows")
        shutil.copytree(REPOSITORY_ROOT / "docs", root / "docs")
        reports = root / ".local/agent/reports/ai-agent-workflow-full-implementation"
        reports.mkdir(parents=True)
        for name in (
            "s1-c-upstream-closure-source-001.md",
            "s1-c-upstream-closure-integration-001.md",
            "s1-c-upstream-closure-validation-001.md",
        ):
            shutil.copy2(
                REPOSITORY_ROOT / ".local/agent/reports/ai-agent-workflow-full-implementation" / name,
                reports / name,
            )
        return root

    @staticmethod
    def _digest(path):
        return "sha256:" + hashlib.sha256(path.read_bytes()).hexdigest()

    @staticmethod
    def _load(path):
        return json.loads(path.read_text(encoding="utf-8"))

    @staticmethod
    def _save(path, value, *, pretty=False):
        if pretty:
            payload = json.dumps(value, indent=2, sort_keys=True) + "\n"
        else:
            payload = json.dumps(
                value, sort_keys=True, separators=(",", ":"), ensure_ascii=True
            ) + "\n"
        path.write_text(payload, encoding="utf-8")

    def _refresh_refs(self, root, values):
        for refs in values:
            for ref in refs:
                if ref["path"] == self._HISTORICAL_TEST_PATH:
                    continue
                ref["digest"] = self._digest(root / ref["path"])

    def _redigest_compatibility(self, root):
        evidence_path = root / "agent-workflows/evidence/compatibility/S0-S1-transition.json"
        manifest_path = root / "agent-workflows/manifests/section-transition-compatibility.json"
        authority_path = root / "agent-workflows/manifests/section-transition-compatibility-authority.json"
        evidence = self._load(evidence_path)
        manifest = self._load(manifest_path)
        authority = self._load(authority_path)

        self._refresh_refs(root, (evidence["sources"], evidence["schemas"], evidence["inputs"]))
        for ref in evidence["s0_close"].values():
            ref["digest"] = self._digest(root / ref["path"])
        for record in evidence["review_validation_history"]:
            for field in (
                "closure_security",
                "closure_coverage",
                "closure_validation",
            ):
                ref = record.get(field)
                if isinstance(ref, dict) and "path" in ref:
                    ref["digest"] = self._digest(root / ref["path"])
        self._save(evidence_path, evidence)

        manifest["evidence"] = {
            "path": "agent-workflows/evidence/compatibility/S0-S1-transition.json",
            "digest": self._digest(evidence_path),
        }
        self._save(manifest_path, manifest)

        for field in self._AUTHORITY_COMMON_FIELDS:
            authority[field] = copy.deepcopy(evidence[field])
        authority["fixture"] = {
            "kind": evidence["fixture"]["kind"],
            "public_interface": evidence["fixture"]["public_interface"],
            "predecessor": evidence["fixture"]["predecessor"],
            "target_rule": evidence["fixture"]["target_rule"],
        }
        authority["evidence"] = copy.deepcopy(manifest["evidence"])
        authority["manifest"] = {
            "path": "agent-workflows/manifests/section-transition-compatibility.json",
            "digest": self._digest(manifest_path),
        }
        self._save(authority_path, authority)
        return self._digest(authority_path)

    def _reserialize_evidence(self, root):
        evidence_path = root / "agent-workflows/evidence/compatibility/S0-S1-transition.json"
        manifest_path = root / "agent-workflows/manifests/section-transition-compatibility.json"
        authority_path = root / "agent-workflows/manifests/section-transition-compatibility-authority.json"
        self._save(evidence_path, self._load(evidence_path), pretty=True)
        manifest = self._load(manifest_path)
        manifest["evidence"]["digest"] = self._digest(evidence_path)
        self._save(manifest_path, manifest)
        authority = self._load(authority_path)
        authority["evidence"]["digest"] = self._digest(evidence_path)
        authority["manifest"]["digest"] = self._digest(manifest_path)
        self._save(authority_path, authority)

    def _reserialize_manifest(self, root):
        manifest_path = root / "agent-workflows/manifests/section-transition-compatibility.json"
        authority_path = root / "agent-workflows/manifests/section-transition-compatibility-authority.json"
        self._save(manifest_path, self._load(manifest_path), pretty=True)
        authority = self._load(authority_path)
        authority["manifest"]["digest"] = self._digest(manifest_path)
        self._save(authority_path, authority)

    def test_fixed_authority_must_be_a_regular_non_symlink_file(self):
        root = self._source_copy()
        authority = root / "agent-workflows/manifests/section-transition-compatibility-authority.json"
        target = root / "authority-copy.json"
        shutil.copy2(authority, target)
        authority.unlink()
        authority.symlink_to(target)
        self.assertFalse(verify_section_transition_compatibility(root))

    def test_caller_root_and_internal_directory_symlinks_fail_closed(self):
        root = self._source_copy()
        substituted_digest = self._redigest_compatibility(root)
        self.assertFalse(verify_section_transition_compatibility(root))
        supplied_root = root.parent / (root.name + "-symlink")
        supplied_root.symlink_to(root, target_is_directory=True)
        self.addCleanup(supplied_root.unlink, missing_ok=True)
        with patch.object(
            section_transition_evidence,
            "_AUTHORITY_DIGEST",
            substituted_digest,
        ):
            self.assertTrue(verify_section_transition_compatibility(root))
            self.assertFalse(verify_current_canonical_lineage(root))
            self.assertFalse(verify_section_transition_compatibility(supplied_root))

        root = self._source_copy()
        substituted_digest = self._redigest_compatibility(root)
        manifests = root / "agent-workflows/manifests"
        target = root / "manifests-target"
        manifests.rename(target)
        manifests.symlink_to(target, target_is_directory=True)
        with patch.object(
            section_transition_evidence,
            "_AUTHORITY_DIGEST",
            substituted_digest,
        ):
            self.assertFalse(verify_section_transition_compatibility(root))

    def test_json_boolean_and_integer_const_substitutions_fail_after_rotation(self):
        cases = (
            ("false-to-zero", ("claims", "actual_a7"), 0),
            ("true-to-one", ("claims", "source_transition_fixture_passed"), 1),
            ("exit-status-to-false", ("test_receipts", 0, "exit_status"), False),
            ("required-open-to-false", ("review_validation_history", 0, "required_open"), False),
        )
        for name, location, replacement in cases:
            with self.subTest(name=name):
                root = self._source_copy()
                evidence_path = root / "agent-workflows/evidence/compatibility/S0-S1-transition.json"
                manifest_path = root / "agent-workflows/manifests/section-transition-compatibility.json"
                authority_path = root / "agent-workflows/manifests/section-transition-compatibility-authority.json"
                evidence = self._load(evidence_path)
                target = evidence
                for key in location[:-1]:
                    target = target[key]
                target[location[-1]] = replacement
                self._save(evidence_path, evidence)
                if location[0] == "claims":
                    manifest = self._load(manifest_path)
                    manifest["claims"] = copy.deepcopy(evidence["claims"])
                    self._save(manifest_path, manifest)
                substituted_digest = self._redigest_compatibility(root)
                with patch.object(
                    section_transition_evidence,
                    "_AUTHORITY_DIGEST",
                    substituted_digest,
                ):
                    self.assertFalse(verify_section_transition_compatibility(root))

    def test_authority_path_and_digest_substrings_fail_after_rotation(self):
        for name, prohibited in (
            ("path", section_transition_evidence._AUTHORITY_PATH),
            ("digest", section_transition_evidence._AUTHORITY_DIGEST),
        ):
            with self.subTest(name=name):
                root = self._source_copy()
                evidence_path = root / "agent-workflows/evidence/compatibility/S0-S1-transition.json"
                evidence = self._load(evidence_path)
                evidence["check_receipts"][0]["command"] += " --embedded=" + prohibited
                self._save(evidence_path, evidence)
                substituted_digest = self._redigest_compatibility(root)
                with patch.object(
                    section_transition_evidence,
                    "_AUTHORITY_DIGEST",
                    substituted_digest,
                ):
                    self.assertFalse(verify_section_transition_compatibility(root))

    def test_compatibility_claims_match_public_catalog_coverage_counts(self):
        evidence = self._load(
            REPOSITORY_ROOT / "agent-workflows/evidence/compatibility/S0-S1-transition.json"
        )
        counts = compile_registry(source_root=REPOSITORY_ROOT)["counts"]
        self.assertEqual(
            {
                "accepted_named": 3,
                "named_targets": 60,
                "accepted_profiles": 0,
                "profile_targets": 23,
            },
            {
                key: evidence["claims"][key]
                for key in (
                    "accepted_named",
                    "named_targets",
                    "accepted_profiles",
                    "profile_targets",
                )
            },
        )
        self.assertEqual(
            {
                "accepted_named": 60,
                "named_targets": 60,
                "accepted_profiles": 23,
                "profile_targets": 23,
            },
            {
                key: counts[key]
                for key in (
                    "accepted_named",
                    "named_targets",
                    "accepted_profiles",
                    "profile_targets",
                )
            },
        )

    def test_fixed_verifier_rejects_persistent_full_redigest_matrix(self):
        cases = (
            "authority",
            "manifest",
            "evidence",
            "s0-index",
            "s0-parent-receipt",
            "compiler",
            "kernel",
            "status-schema",
            "compatibility-schema",
            "test-receipt",
            "claim",
            "expected-head",
            "review-validation",
        )
        temporary_controls = {
            "authority",
            "manifest",
            "evidence",
            "compiler",
            "kernel",
            "status-schema",
            "compatibility-schema",
            "test-receipt",
            "review-validation",
        }
        for mutation in cases:
            with self.subTest(mutation=mutation):
                root = self._source_copy()
                evidence_path = root / "agent-workflows/evidence/compatibility/S0-S1-transition.json"
                authority_path = root / "agent-workflows/manifests/section-transition-compatibility-authority.json"
                if mutation == "authority":
                    authority = self._load(authority_path)
                    authority["authority_id"] = "replacement-codex-root-v1"
                    self._save(authority_path, authority)
                elif mutation == "s0-index":
                    path = root / "agent-workflows/evidence/sections/S0/index.json"
                    self._save(path, self._load(path), pretty=True)
                elif mutation == "s0-parent-receipt":
                    path = root / "agent-workflows/evidence/sections/S0/accepted.json"
                    self._save(path, self._load(path), pretty=True)
                elif mutation in {"compiler", "kernel"}:
                    name = "section_control_plane.py" if mutation == "compiler" else "control_kernel.py"
                    path = root / "agent-workflows/src/ai_agent_workflow" / name
                    path.write_text(path.read_text(encoding="utf-8") + "\n# coherent replacement\n")
                elif mutation in {"status-schema", "compatibility-schema"}:
                    name = (
                        "section-status-v1.schema.json"
                        if mutation == "status-schema"
                        else "section-transition-compatibility-v1.schema.json"
                    )
                    path = root / "agent-workflows/schemas" / name
                    self._save(path, self._load(path), pretty=True)
                elif mutation in {"test-receipt", "claim", "expected-head"}:
                    evidence = self._load(evidence_path)
                    if mutation == "test-receipt":
                        evidence["test_receipts"][0]["tests_run"] += 1
                    elif mutation == "claim":
                        evidence["claims"]["actual_a7"] = True
                    else:
                        evidence["expected_head"] = {
                            "revision": 2,
                            "transaction_digest": "sha256:" + "4" * 64,
                            "synthetic": True,
                        }
                    self._save(evidence_path, evidence)
                elif mutation == "review-validation":
                    path = root / ".local/agent/reports/ai-agent-workflow-full-implementation/s1-c-upstream-closure-source-001.md"
                    path.write_text(path.read_text(encoding="utf-8") + "\n", encoding="utf-8")

                substituted_digest = self._redigest_compatibility(root)
                if mutation == "manifest":
                    self._reserialize_manifest(root)
                    substituted_digest = self._digest(authority_path)
                elif mutation == "evidence":
                    self._reserialize_evidence(root)
                    substituted_digest = self._digest(authority_path)

                self.assertFalse(verify_section_transition_compatibility(root))
                with patch.object(
                    section_transition_evidence,
                    "_AUTHORITY_DIGEST",
                    substituted_digest,
                ):
                    if mutation in temporary_controls:
                        self.assertTrue(verify_section_transition_compatibility(root))
                        self.assertFalse(verify_current_canonical_lineage(root))
                    else:
                        self.assertFalse(verify_section_transition_compatibility(root))

    def test_digest_graph_is_acyclic_and_authority_is_not_close_set_nominated(self):
        authority = self._load(
            REPOSITORY_ROOT / "agent-workflows/manifests/section-transition-compatibility-authority.json"
        )
        manifest = self._load(
            REPOSITORY_ROOT / "agent-workflows/manifests/section-transition-compatibility.json"
        )
        evidence = self._load(
            REPOSITORY_ROOT / "agent-workflows/evidence/compatibility/S0-S1-transition.json"
        )
        verifier_path = "agent-workflows/src/ai_agent_workflow/section_transition_evidence.py"
        authority_path = "agent-workflows/manifests/section-transition-compatibility-authority.json"
        self.assertNotIn(verifier_path, json.dumps(authority, sort_keys=True))
        self.assertNotIn(authority_path, json.dumps(manifest, sort_keys=True))
        self.assertNotIn(authority_path, json.dumps(evidence, sort_keys=True))
        self.assertNotIn("manifest", evidence)
        self.assertEqual(
            manifest["evidence"]["path"],
            "agent-workflows/evidence/compatibility/S0-S1-transition.json",
        )


class SectionTransitionCompatibilityIntegrationTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.run_root = Path(self.temp.name)
        self.kernel = ControlKernel(self.run_root, "compatibility-run")
        self.authority = {
            "approved": True,
            "scopes": [
                "entry",
                "publish_artifact",
                "close_epoch",
                "close_group",
                "source",
                "open_section",
                "open_group",
            ],
            "protected_fields": ["section", "group", "epoch", "ready"],
            "human_receipt": "compatibility-source-only",
            "execution_class": "candidate-generic",
        }
        self.kernel.entry(
            {"path": "objectives/compatibility.md", "version": "v1", "digest": "a" * 64},
            workflow_version="workflow/v1",
            group_id="Bootstrap",
            epoch_id="Bootstrap-E1",
            authority_ref=self.authority,
        )
        self.sources = {
            section_id: self._attest_sources(section_id, task_id)
            for section_id, task_id in (("S0", "S0.C1"), ("S1", "S1.A1"), ("S2", "S2.A1"))
        }
        self.kernel.close_epoch(authority_ref=self.authority)
        self.kernel.close_group(
            next_group="contracts-and-schema",
            authority_ref=self.authority,
        )

    def tearDown(self):
        self.temp.cleanup()

    def _attest_sources(self, section_id, task_id, *, label=None, group_id=None, epoch_id=None):
        label = label or section_id
        group_id = group_id or ("contracts-and-schema" if section_id == "S0" else "analysis")
        epoch_id = epoch_id or section_id + "-E1"
        tasks = [{
            "id": task_id,
            "section_id": section_id,
            "group_id": group_id,
            "epoch_id": epoch_id,
        }]
        contents = {
            "plan": ("plan-" + section_id).encode(),
            "catalog": json.dumps(
                tasks, sort_keys=True, separators=(",", ":"), ensure_ascii=True
            ).encode(),
            "checkpoint": ("checkpoint-" + section_id).encode(),
            "bundle": ("bundle-" + section_id).encode(),
            "lifecycle_prerequisite": ("lifecycle-" + section_id).encode(),
            "closed_group_receipt": ("receipt-" + section_id).encode(),
        }
        refs = {}
        for kind, content in contents.items():
            path = self.run_root / ("source-" + label + "-" + kind)
            path.write_bytes(content)
            digest = "sha256:" + hashlib.sha256(content).hexdigest()
            self.kernel.publish_artifact(
                "attest-" + label + "-" + kind,
                "v1",
                {"ref_kind": kind, "source_digest": digest, "source_path": str(path)},
                kind="s0-source-attestation",
                authority_ref=self.authority,
            )
            refs[kind] = digest
        refs["tasks"] = tasks
        return refs

    def _head_binding(self):
        head = self.kernel.head()
        return {
            "revision": head["revision"],
            "transaction_digest": head["transaction_digest"],
        }

    def _plan_authority(self, expected_head):
        return {
            "approved": True,
            "scopes": ["source", "open_section", "open_group"],
            "protected_fields": ["section", "group", "epoch", "ready"],
            "human_receipt": "compatibility-source-only",
            "expected_head": copy.deepcopy(expected_head),
            "execution_class": "candidate-generic",
        }

    def _compile_s0(self):
        refs = self.sources["S0"]
        expected_head = self._head_binding()
        plan = {
            "schema": "section-plan/v1",
            "section": {"id": "S0", "workflow_id": "workflow-main", "version": "v1"},
            "groups": [{
                "id": "contracts-and-schema",
                "depends_on": [],
                "first_epoch": "S0-E1",
                "first_frontier": ["S0.C1"],
            }],
            "closed_bootstrap": {
                "run_id": "compatibility-run",
                "workflow_version": "workflow/v1",
                "group_id": "Bootstrap",
                "status": "paused_after_group",
                "ready": [],
                "next_group": "contracts-and-schema",
                "clear_boundary": True,
                "plan_digest": refs["plan"],
                "catalog_digest": refs["catalog"],
                "checkpoint_ref": {"checkpoint_id": "bootstrap", "digest": refs["checkpoint"]},
                "bundle_ref": {"digest": refs["bundle"]},
                "lifecycle_prerequisite": {
                    "kind": "closed-bootstrap",
                    "group_id": "Bootstrap",
                    "receipt_digest": refs["lifecycle_prerequisite"],
                },
                "closed_group_receipt": {
                    "group_id": "Bootstrap",
                    "digest": refs["closed_group_receipt"],
                },
            },
            "authority": self._plan_authority(expected_head),
            "expected_head": expected_head,
            "plan": {"digest": refs["plan"]},
            "catalog": {"digest": refs["catalog"], "tasks": refs["tasks"]},
            "idempotency_key": "open-s0-contracts",
        }
        return SectionControlPlaneV1().compile_registry(plan)["open_section"]

    def _accept_current_section(self, section_id, group_id, next_section):
        artifact_id = "accepted-" + section_id
        self.kernel.publish_artifact(
            artifact_id,
            "v1",
            {
                "schema": "section-accepted-result/v1",
                "run_id": "compatibility-run",
                "section_id": section_id,
                "group_id": group_id,
                "result": "passed",
            },
            kind="section-accepted-result",
            authority_ref=self.authority,
        )
        digest = self.kernel.read_state()["artifacts"][artifact_id]["digest"]
        self.kernel.close_epoch(authority_ref=self.authority)
        self.kernel.close_group(
            next_group=next_section,
            acceptance_evidence=[digest],
            authority_ref=self.authority,
        )

    def _compile_successor(self, target_section):
        position = int(target_section[1:])
        predecessor_id = "S%d" % (position - 1)
        current = self.kernel.read_state()
        control = current["metadata"]["section_control"]
        refs = self.sources[target_section]
        expected_head = self._head_binding()
        plan = {
            "schema": "section-plan/v1",
            "section": {"id": target_section, "workflow_id": "workflow-main", "version": "v1"},
            "groups": [{
                "id": "analysis",
                "depends_on": [],
                "first_epoch": target_section + "-E1",
                "first_frontier": [target_section + ".A1"],
            }],
            "accepted_section": {
                "run_id": "compatibility-run",
                "workflow_version": "workflow/v1",
                "section_id": predecessor_id,
                "status": "paused_after_group",
                "ready": [],
                "next_section": target_section,
                "clear_boundary": True,
                "plan_digest": refs["plan"],
                "catalog_digest": refs["catalog"],
                "checkpoint_ref": current["group"]["checkpoint_ref"],
                "bundle_ref": current["group"]["bundle_ref"],
                "lifecycle_prerequisite": {
                    "kind": "accepted-section",
                    "section_id": predecessor_id,
                    "receipt_digest": control["accepted_section_receipt"]["digest"],
                },
                "accepted_section_receipt": control["accepted_section_receipt"],
                "section_history": copy.deepcopy(control.get("section_history", [])),
            },
            "authority": self._plan_authority(expected_head),
            "expected_head": expected_head,
            "plan": {"digest": refs["plan"]},
            "catalog": {"digest": refs["catalog"], "tasks": refs["tasks"]},
            "idempotency_key": "open-" + target_section.lower() + "-analysis",
        }
        return SectionControlPlaneV1().compile_registry(plan)["open_section"]

    def _compile_same_section_group(self, refs):
        state = self.kernel.read_state()
        section = state["metadata"]["section_control"]
        expected_head = self._head_binding()
        plan = {
            "schema": "section-plan/v1",
            "section": {"id": "S1", "workflow_id": "workflow-main", "version": "v1"},
            "groups": [{
                "id": "review", "depends_on": ["analysis"], "first_epoch": "S1-E2",
                "first_frontier": ["S1.R1"],
            }],
            "closed_bootstrap": {
                "run_id": "compatibility-run", "workflow_version": "workflow/v1",
                "group_id": "analysis", "status": "paused_after_group", "ready": [],
                "next_group": "review", "clear_boundary": True,
                "plan_digest": refs["plan"], "catalog_digest": refs["catalog"],
                "checkpoint_ref": {
                    "checkpoint_id": "checkpoint-s1-analysis",
                    "digest": refs["checkpoint"],
                },
                "bundle_ref": {"digest": refs["bundle"]},
                "lifecycle_prerequisite": {
                    "kind": "accepted-section", "section_id": "S1",
                    "receipt_digest": section["accepted_section_receipt"]["digest"],
                },
                "closed_group_receipt": {"group_id": "analysis", "digest": refs["closed_group_receipt"]},
            },
            "authority": self._plan_authority(expected_head),
            "expected_head": expected_head,
            "plan": {"digest": refs["plan"]}, "catalog": {"digest": refs["catalog"], "tasks": refs["tasks"]},
            "idempotency_key": "open-s1-review",
        }
        return SectionControlPlaneV1().compile_open_group(plan)

    def test_public_seams_open_exact_retry_cold_resume_and_generic_successor(self):
        open_section(self.kernel, self._compile_s0())
        self._accept_current_section("S0", "contracts-and-schema", "S1")

        s1_command = self._compile_successor("S1")
        first = open_section(self.kernel, s1_command)
        retry = open_section(self.kernel, copy.deepcopy(s1_command))
        self.assertEqual(retry, {
            "duplicate": True,
            "transaction_digest": first["transaction_digest"],
            "revision": first["revision"],
        })
        s1_head = self._head_binding()
        s1_status = resume(self.kernel, s1_head)
        cold = ControlKernel(self.run_root, "compatibility-run")
        self.assertEqual(resume(cold, s1_head), s1_status)
        self.assertEqual(s1_status["ready"], ["S1.A1"])
        self.assertEqual(
            [(item["position"], item["section_id"]) for item in s1_status["section_history"]],
            [(0, "S0")],
        )

        self._accept_current_section("S1", "analysis", "S2")
        open_section(self.kernel, self._compile_successor("S2"))
        s2_status = resume(self.kernel, self._head_binding())
        self.assertEqual(s2_status["ready"], ["S2.A1"])
        self.assertEqual(
            [(item["position"], item["section_id"]) for item in s2_status["section_history"]],
            [(0, "S0"), (1, "S1")],
        )
        self.assertEqual(
            s2_status["claims"],
            {
                "source_transition_fixture_passed": False,
                "actual_a7": False,
                "activation": False,
                "full_ready": False,
            },
        )

    def test_public_s1_same_section_group_preserves_history_through_cold_resume(self):
        open_section(self.kernel, self._compile_s0())
        self._accept_current_section("S0", "contracts-and-schema", "S1")
        open_section(self.kernel, self._compile_successor("S1"))
        refs = self._attest_sources(
            "S1", "S1.R1", label="S1-review", group_id="review", epoch_id="S1-E2"
        )
        self._accept_current_section("S1", "analysis", "review")
        command = self._compile_same_section_group(refs)

        receipt = open_group(self.kernel, command)
        current = self._head_binding()
        status_view = resume(self.kernel, current)
        self.assertEqual(receipt["revision"], current["revision"])
        self.assertEqual(status_view["group"]["id"], "review")
        self.assertEqual(status_view["ready"], ["S1.R1"])
        self.assertEqual(
            [(item["position"], item["section_id"]) for item in status_view["section_history"]],
            [(0, "S0")],
        )
        self.assertEqual(
            resume(ControlKernel(self.run_root, "compatibility-run"), current), status_view
        )


if __name__ == "__main__":
    unittest.main()
