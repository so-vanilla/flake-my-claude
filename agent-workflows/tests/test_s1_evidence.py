import copy
import hashlib
import json
import os
from pathlib import Path
import shutil
import tempfile
import unittest
from unittest.mock import patch


ROOT = Path(__file__).resolve().parents[2]
SRC = ROOT / "agent-workflows" / "src"
if str(SRC) not in os.sys.path:
    os.sys.path.insert(0, str(SRC))

from ai_agent_workflow.s0_evidence import evaluate_source_transition_fixture
from ai_agent_workflow.s1_evidence import (
    evaluate_s1_authority_candidate,
    evaluate_s1_close_documents,
    verify_s1_source_evidence,
)
from ai_agent_workflow.schema_validation import SchemaValidationError, validate_document


DIGESTS = ["sha256:" + format(index, "064x") for index in range(1, 80)]


def digest(path):
    return "sha256:" + hashlib.sha256(path.read_bytes()).hexdigest()


class S1EvidenceTests(unittest.TestCase):
    def setUp(self):
        self.root = Path(tempfile.mkdtemp())
        self.addCleanup(shutil.rmtree, self.root, ignore_errors=True)

    def _write(self, relative, value, *, raw=False):
        path = self.root / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        text = value if raw else json.dumps(value, sort_keys=True)
        path.write_text(text, encoding="utf-8")
        return {"path": relative, "digest": digest(path)}

    def _fixture(self):
        run_id = "s1-source-close-2026-09-04"
        group_id = "bootstrap-and-shared-closure"
        review = {
            "schema": "section-review/v1", "section_id": "S1",
            "group_id": group_id, "run_id": run_id, "verdict": "accepted",
            "findings": [{"id": "C-S1E-SCHEMA-001", "state": "required"}],
            "review_budget": {"max_fixes": 5, "consumed": [
                {"finding_id": "C-S1E-SCHEMA-001", "attempt": 1},
            ]},
        }
        review_ref = self._write("evidence/review.json", review)
        validation = {
            "schema": "finding-validation-set/v1", "section_id": "S1",
            "group_id": group_id, "run_id": run_id,
            "review_ref": review_ref,
            "outcomes": [{"finding_id": "C-S1E-SCHEMA-001", "state": "closed"}],
        }
        validation_ref = self._write("evidence/validation.json", validation)
        slice_refs = {"artifact_refs": {}, "result_refs": {}}
        bundle_artifacts = [review_ref, validation_ref]
        for field, kind, status in (
            ("artifact_refs", "artifact", "accepted"),
            ("result_refs", "result", "passed"),
        ):
            for suffix in "ABCD":
                slice_id = "S1-" + suffix
                document = {
                    "schema": "s1-slice-evidence/v1", "section_id": "S1",
                    "group_id": group_id, "run_id": run_id,
                    "slice_id": slice_id, "evidence_kind": kind, "status": status,
                }
                ref = self._write("evidence/%s-%s.json" % (kind, slice_id), document)
                slice_refs[field][slice_id] = {"digest": ref["digest"]}
                bundle_artifacts.append(ref)
        accepted = {
            "schema": "section-accepted-result/v1", "run_id": run_id,
            "section_id": "S1", "group_id": group_id, "result": "passed",
            **slice_refs,
            "review_ref": {"digest": review_ref["digest"]},
            "finding_validation_ref": {"digest": validation_ref["digest"]},
        }
        accepted_ref = self._write("evidence/accepted.json", accepted)
        bundle_artifacts.append(accepted_ref)
        next_ref = self._write("evidence/next-S2.json", {"input_id": "next-S2"})
        bundle = {
            "schema": "section-bundle/v1", "section_id": "S1", "run_id": run_id,
            "artifacts": bundle_artifacts, "next_section_inputs": [next_ref],
            "state_snapshot": {
                "accepted": [], "invalidated": [], "unresolved": [],
                "budget": {"review_ref": review_ref, "max_fixes": 5,
                           "consumed": review["review_budget"]["consumed"]},
                "next_section_inputs": [{"identity": "next-S2", "ref": next_ref}],
            },
        }
        bundle_ref = self._write("evidence/bundle.json", bundle)
        return {
            "refs": {"section_review": review_ref, "finding_validation": validation_ref,
                     "section_bundle": bundle_ref, "accepted_result": accepted_ref},
            "run_id": run_id, "group_id": group_id,
        }

    def _evaluate(self, fixture):
        return evaluate_s1_close_documents(
            self.root, fixture["refs"], expected_section_id="S1",
            expected_group_id=fixture["group_id"], expected_run_id=fixture["run_id"],
        )

    def _mutate(self, fixture, key, mutate, *, raw=None):
        ref = fixture["refs"][key]
        path = self.root / ref["path"]
        if raw is None:
            value = json.loads(path.read_text(encoding="utf-8"))
            mutate(value)
            path.write_text(json.dumps(value, sort_keys=True), encoding="utf-8")
        else:
            path.write_text(raw, encoding="utf-8")
        ref["digest"] = digest(path)

    def _mutate_review_and_rebind(self, fixture, mutate_review, mutate_outcome=None):
        review_ref = fixture["refs"]["section_review"]
        review_path = self.root / review_ref["path"]
        review = json.loads(review_path.read_text(encoding="utf-8"))
        mutate_review(review)
        review_path.write_text(json.dumps(review, sort_keys=True), encoding="utf-8")
        review_ref["digest"] = digest(review_path)

        validation_ref = fixture["refs"]["finding_validation"]
        validation_path = self.root / validation_ref["path"]
        validation = json.loads(validation_path.read_text(encoding="utf-8"))
        validation["review_ref"] = copy.deepcopy(review_ref)
        if mutate_outcome is not None:
            mutate_outcome(validation["outcomes"][0])
        validation_path.write_text(json.dumps(validation, sort_keys=True), encoding="utf-8")
        validation_ref["digest"] = digest(validation_path)

        accepted_ref = fixture["refs"]["accepted_result"]
        accepted_path = self.root / accepted_ref["path"]
        accepted = json.loads(accepted_path.read_text(encoding="utf-8"))
        accepted["review_ref"] = {"digest": review_ref["digest"]}
        accepted["finding_validation_ref"] = {"digest": validation_ref["digest"]}
        accepted_path.write_text(json.dumps(accepted, sort_keys=True), encoding="utf-8")
        accepted_ref["digest"] = digest(accepted_path)

        bundle_ref = fixture["refs"]["section_bundle"]
        bundle_path = self.root / bundle_ref["path"]
        bundle = json.loads(bundle_path.read_text(encoding="utf-8"))
        replacements = {ref["path"]: ref for ref in (review_ref, validation_ref, accepted_ref)}
        bundle["artifacts"] = [copy.deepcopy(replacements.get(ref["path"], ref)) for ref in bundle["artifacts"]]
        bundle["state_snapshot"]["budget"]["review_ref"] = copy.deepcopy(review_ref)
        bundle["state_snapshot"]["budget"]["consumed"] = copy.deepcopy(review["review_budget"]["consumed"])
        bundle_path.write_text(json.dumps(bundle, sort_keys=True), encoding="utf-8")
        bundle_ref["digest"] = digest(bundle_path)

    def test_revised_schemas_keep_physical_s0_valid_and_s0_replay_true(self):
        for document_name, schema_name in (
            ("review.json", "section-review-v1.schema.json"),
            ("validation.json", "finding-validation-set-v1.schema.json"),
            ("bundle.json", "section-bundle-v1.schema.json"),
            ("accepted.json", "section-accepted-result-v1.schema.json"),
        ):
            with self.subTest(document=document_name):
                schema = json.loads((ROOT / "agent-workflows/schemas" / schema_name).read_text())
                document = json.loads((ROOT / "agent-workflows/evidence/sections/S0" / document_name).read_text())
                validate_document(document, schema, registry=dict(schema.get("$defs", {})))
        self.assertTrue(evaluate_source_transition_fixture(ROOT))

        # Retain both physical S0/S1 replays while adding the closed S2 boundary.
        schema_root = ROOT / "agent-workflows" / "schemas"
        for section_id in ("S0", "S1"):
            for document_name, schema_name in (
                ("review.json", "section-review-v1.schema.json"),
                ("bundle.json", "section-bundle-v1.schema.json"),
                ("accepted.json", "section-accepted-result-v1.schema.json"),
            ):
                with self.subTest(section_id=section_id, document=document_name):
                    schema = json.loads((schema_root / schema_name).read_text())
                    document = json.loads(
                        (ROOT / "agent-workflows" / "evidence" / "sections" / section_id / document_name).read_text()
                    )
                    validate_document(document, schema, registry=dict(schema.get("$defs", {})))

        digest_ref = {"digest": DIGESTS[1]}
        ref = {"path": "evidence/ref.json", "digest": DIGESTS[1]}
        s2_documents = {
            "section-review-v1.schema.json": {
                "schema": "section-review/v1", "section_id": "S2", "group_id": "g",
                "run_id": "r", "verdict": "accepted", "findings": [],
                "review_budget": {"max_fixes": 5, "consumed": []},
            },
            "section-bundle-v1.schema.json": {
                "schema": "section-bundle/v1", "section_id": "S2", "run_id": "r",
                "artifacts": [ref] * 4, "next_section_inputs": [ref],
                "state_snapshot": {
                    "accepted": [], "invalidated": [], "unresolved": [],
                    "budget": {"review_ref": ref, "max_fixes": 1, "consumed": []},
                    "next_section_inputs": [],
                },
            },
            "section-accepted-result-v1.schema.json": {
                "schema": "section-accepted-result/v1", "run_id": "r", "section_id": "S2",
                "group_id": "g", "result": "passed",
                "artifact_refs": {key: digest_ref for key in ("S2-0", "S2-A", "S2-B", "S2-C", "S2-D")},
                "result_refs": {key: digest_ref for key in ("S2-0", "S2-A", "S2-B", "S2-C", "S2-D")},
                "review_ref": digest_ref, "finding_validation_ref": digest_ref,
            },
        }
        for schema_name, document in s2_documents.items():
            schema = json.loads((schema_root / schema_name).read_text())
            validate_document(document, schema, registry=dict(schema.get("$defs", {})))
            s3_document = copy.deepcopy(document)
            s3_document["section_id"] = "S3"
            with self.assertRaises(SchemaValidationError):
                validate_document(s3_document, schema, registry=dict(schema.get("$defs", {})))

        accepted_schema = json.loads(
            (schema_root / "section-accepted-result-v1.schema.json").read_text()
        )
        wrong_s2 = copy.deepcopy(s2_documents["section-accepted-result-v1.schema.json"])
        wrong_s2["artifact_refs"].pop("S2-0")
        wrong_s2["artifact_refs"]["S2-E"] = digest_ref
        with self.assertRaises(SchemaValidationError):
            validate_document(wrong_s2, accepted_schema, registry=dict(accepted_schema.get("$defs", {})))

    def test_honest_disposable_s1_close_documents_pass(self):
        self.assertTrue(self._evaluate(self._fixture()))

    def test_cross_identity_and_unknown_sections_fail(self):
        cases = (
            ("review-section", "section_review", lambda value: value.__setitem__("section_id", "S0")),
            ("validation-group", "finding_validation", lambda value: value.__setitem__("group_id", "other")),
            ("accepted-run", "accepted_result", lambda value: value.__setitem__("run_id", "other")),
            ("bundle-s2", "section_bundle", lambda value: value.__setitem__("section_id", "S2")),
        )
        for name, key, mutation in cases:
            with self.subTest(name=name):
                fixture = self._fixture()
                self._mutate(fixture, key, mutation)
                self.assertFalse(self._evaluate(fixture))

    def test_cross_section_and_non_exact_slice_sets_fail(self):
        for mutation in ("cross", "missing", "extra"):
            with self.subTest(mutation=mutation):
                fixture = self._fixture()
                def change(value):
                    refs = value["artifact_refs"]
                    if mutation == "cross":
                        refs["S0-A"] = refs.pop("S1-A")
                    elif mutation == "missing":
                        refs.pop("S1-A")
                    else:
                        refs["S1-E"] = {"digest": DIGESTS[50]}
                self._mutate(fixture, "accepted_result", change)
                self.assertFalse(self._evaluate(fixture))

    def test_duplicate_raw_key_and_duplicate_digest_mapping_fail(self):
        fixture = self._fixture()
        ref = fixture["refs"]["accepted_result"]
        raw = (self.root / ref["path"]).read_text(encoding="utf-8")
        raw = raw[:-1] + ',"section_id":"S1"}'
        self._mutate(fixture, "accepted_result", lambda value: None, raw=raw)
        self.assertFalse(self._evaluate(fixture))

        fixture = self._fixture()
        def duplicate(value):
            value["result_refs"]["S1-A"]["digest"] = value["artifact_refs"]["S1-A"]["digest"]
        self._mutate(fixture, "accepted_result", duplicate)
        self.assertFalse(self._evaluate(fixture))

    def test_wrong_nested_property_fails(self):
        fixture = self._fixture()
        self._mutate(
            fixture, "section_review",
            lambda value: value["review_budget"]["consumed"][0].__setitem__("unexpected", True),
        )
        self.assertFalse(self._evaluate(fixture))

    def test_caller_root_escape_and_symlink_fail(self):
        fixture = self._fixture()
        fixture["refs"]["section_review"]["path"] = "../review.json"
        self.assertFalse(self._evaluate(fixture))

        fixture = self._fixture()
        review_ref = fixture["refs"]["section_review"]
        original = self.root / review_ref["path"]
        target = self.root.parent / (self.root.name + "-review.json")
        target.write_bytes(original.read_bytes())
        self.addCleanup(target.unlink, missing_ok=True)
        original.unlink()
        original.symlink_to(target)
        self.assertFalse(self._evaluate(fixture))

    def test_symlinked_caller_root_and_intermediate_component_fail(self):
        fixture = self._fixture()
        linked_root = self.root.parent / (self.root.name + "-link")
        linked_root.symlink_to(self.root, target_is_directory=True)
        self.addCleanup(linked_root.unlink, missing_ok=True)
        self.assertFalse(evaluate_s1_close_documents(
            linked_root, fixture["refs"], expected_section_id="S1",
            expected_group_id=fixture["group_id"], expected_run_id=fixture["run_id"],
        ))

        fixture = self._fixture()
        evidence = self.root / "evidence"
        actual_evidence = self.root / "actual-evidence"
        evidence.rename(actual_evidence)
        evidence.symlink_to(actual_evidence, target_is_directory=True)
        self.assertFalse(self._evaluate(fixture))

    def test_required_close_requires_attempt_and_terminal_dispositions_match_exactly(self):
        fixture = self._fixture()
        self._mutate_review_and_rebind(
            fixture, lambda review: review["review_budget"].__setitem__("consumed", []),
        )
        self.assertFalse(self._evaluate(fixture))

        fixture = self._fixture()
        self._mutate_review_and_rebind(
            fixture,
            lambda review: review["findings"][0].__setitem__("state", "defer"),
            lambda outcome: outcome.__setitem__("state", "reject"),
        )
        self.assertFalse(self._evaluate(fixture))

        fixture = self._fixture()
        self._mutate_review_and_rebind(
            fixture,
            lambda review: review["review_budget"]["consumed"].append(
                {"finding_id": "UNKNOWN", "attempt": 1}
            ),
        )
        self.assertFalse(self._evaluate(fixture))

        fixture = self._fixture()
        self._mutate_review_and_rebind(
            fixture,
            lambda review: review["review_budget"].__setitem__("consumed", [
                {"finding_id": "C-S1E-SCHEMA-001", "attempt": 2},
                {"finding_id": "C-S1E-SCHEMA-001", "attempt": 1},
            ]),
        )
        self.assertFalse(self._evaluate(fixture))

    def test_s1_source_authority_schema_is_closed_and_does_not_bind_verifier_or_local_report(self):
        schema = json.loads((ROOT / "agent-workflows/schemas/s1-source-authority-v1.schema.json").read_text())
        ref = lambda path, index: {"path": path, "digest": DIGESTS[index]}
        contracts = {}
        ids = ["group.A." + value for value in ("A1", "A2", "A3", "A4", "A5", "A6", "A6R", "A7")]
        ids += ["group.F.F" + str(index) for index in range(1, 9)]
        for index, qualified_id in enumerate(ids):
            contracts[qualified_id] = {
                "source": ref("agent-workflows/skills/source-%s/SKILL.md" % index, index + 10),
                "evidence": ref("agent-workflows/evidence/contracts/%s.json" % qualified_id.rsplit(".", 1)[-1], index + 30),
                "selector": "tests/test_contracts.py::test_%s" % index,
            }
        authority = {
            "schema": "s1-source-authority/v1", "authority_version": "S1-source-authority/v1",
            "issuer": "codex-root",
            "fixture": {"section_id": "S1", "run_id": "s1-source-close-2026-09-04",
                        "group_id": "bootstrap-and-shared-closure", "execution_class": "source-only"},
            "close_index": ref("agent-workflows/evidence/sections/S1/index.json", 1),
            "accepted_s1_plan": ref("docs/plans/ai-agent-workflow-s1-section-plan.md", 2),
            "current_inputs": {
                "full_implementation_plan": ref("docs/plans/ai-agent-workflow-full-implementation-plan.md", 3),
                "step_catalog": ref("docs/plans/ai-agent-workflow-step-catalog.md", 4),
            },
            "s0_parent": {
                "index": ref("agent-workflows/evidence/sections/S0/index.json", 5),
                "result": ref("agent-workflows/evidence/sections/S0/section-result.json", 6),
                "bundle": ref("agent-workflows/evidence/sections/S0/bundle.json", 7),
                "checkpoint": ref("agent-workflows/evidence/sections/S0/checkpoint.json", 8),
            },
            "compatibility": {
                "evidence": ref("agent-workflows/evidence/compatibility/S0-S1-transition.json", 9),
                "plan": ref("docs/plans/ai-agent-workflow-s0-s1-transition-compatibility-plan.md", 10),
                "manifest": ref("agent-workflows/manifests/section-transition-compatibility.json", 11),
                "authority": ref("agent-workflows/manifests/section-transition-compatibility-authority.json", 12),
                "verifier": ref("agent-workflows/src/ai_agent_workflow/section_transition_evidence.py", 13),
            },
            "contracts": contracts,
            "canonical_findings": [{"id": "C-S1E-SCHEMA-001", "fingerprint": "schema-boundary/v1",
                                     "state": "closed", "attempts": [1]}],
            "expected_head": {"revision": 9, "transaction_digest": DIGESTS[60]},
            "checkpoint": {"checkpoint_id": "cp-s1", "digest": DIGESTS[61]},
            "allowed_next_input": {"identity": "next-S2", "ref": ref("agent-workflows/evidence/sections/S1/next-S2.json", 62)},
            "acceptance_test": {"command": "python3 agent-workflows/tests/test_s1_evidence.py",
                                "tests_run": 8, "exit_status": 0, "status": "passed"},
            "claim_vector": {
                "s0_registry_ready": True, "source_transition_fixture_passed": True,
                "s1_bootstrap_shared_lifecycle_source_complete": True,
                "named_contracts": {"accepted": 16, "total": 60},
                "profile_steps": {"accepted": 0, "total": 23},
                "actual_a7_handoff_complete": False, "migration_complete": False,
                "activation_complete": False, "source_wide_integration_complete": False,
                "full_workflow_ready": False,
            },
        }
        authority["trusted_schemas"] = {}
        for key, filename in (
            ("section_review", "section-review-v1.schema.json"),
            ("finding_validation", "finding-validation-set-v1.schema.json"),
            ("section_bundle", "section-bundle-v1.schema.json"),
            ("accepted_result", "section-accepted-result-v1.schema.json"),
            ("s1_authority", "s1-source-authority-v1.schema.json"),
        ):
            authority["trusted_schemas"][key] = self._write(
                "agent-workflows/schemas/" + filename,
                (ROOT / "agent-workflows/schemas" / filename).read_text(encoding="utf-8"),
                raw=True,
            )
        validate_document(authority, schema, registry=dict(schema["$defs"]))
        for mutation in ("nested-extra", "verifier", "local", "attempt-6"):
            with self.subTest(mutation=mutation):
                candidate = copy.deepcopy(authority)
                if mutation == "nested-extra":
                    candidate["checkpoint"]["unexpected"] = True
                elif mutation == "verifier":
                    candidate["contracts"][ids[0]]["source"]["path"] = "agent-workflows/src/ai_agent_workflow/s1_evidence.py"
                elif mutation == "attempt-6":
                    candidate["canonical_findings"][0]["attempts"] = [6]
                else:
                    candidate["contracts"][ids[0]]["evidence"]["path"] = ".local/agent/report.md"
                with self.assertRaises(SchemaValidationError):
                    validate_document(candidate, schema, registry=dict(schema["$defs"]))

    def test_physical_s1_candidate_passes_but_unanchored_fixed_replay_fails_closed(self):
        authority_path = ROOT / "agent-workflows/manifests/s1-source-authority.json"
        authority_ref = {"path": authority_path.relative_to(ROOT).as_posix(),
                         "digest": digest(authority_path)}
        self.assertTrue(evaluate_s1_authority_candidate(ROOT, authority_ref))
        self.assertTrue(verify_s1_source_evidence(ROOT))

        physical_root = self.root / "physical-candidate"
        shutil.copytree(ROOT / "agent-workflows", physical_root / "agent-workflows")
        shutil.copytree(ROOT / "docs", physical_root / "docs")
        copied_authority = physical_root / authority_ref["path"]
        copied_ref = {"path": authority_ref["path"], "digest": digest(copied_authority)}
        self.assertTrue(verify_s1_source_evidence(physical_root))

        replacements = (
            ("authority", authority_ref["path"]),
            ("index", "agent-workflows/evidence/sections/S1/index.json"),
            ("plan", "agent-workflows/evidence/sections/S1/plan.json"),
            ("catalog", "agent-workflows/evidence/sections/S1/catalog.json"),
            ("s0-index", "agent-workflows/evidence/sections/S0/index.json"),
            ("s0-result", "agent-workflows/evidence/sections/S0/section-result.json"),
            ("s0-bundle", "agent-workflows/evidence/sections/S0/bundle.json"),
            ("s0-checkpoint", "agent-workflows/evidence/sections/S0/checkpoint.json"),
            ("a-source", "agent-workflows/skills/bootstrap-classify-environment/SKILL.md"),
            ("a-evidence", "agent-workflows/evidence/contracts/A1.json"),
            ("f-source", "agent-workflows/src/ai_agent_workflow/closure_protocol.py"),
            ("f-evidence", "agent-workflows/evidence/contracts/F1.json"),
            ("finding-history", "agent-workflows/evidence/sections/S1/review.json"),
            ("checkpoint", "agent-workflows/evidence/sections/S1/checkpoint.json"),
            ("next-s2", "agent-workflows/evidence/sections/S1/next-S2.json"),
            ("test-receipt", "agent-workflows/evidence/sections/S1/transaction.json"),
        )
        for name, relative in replacements:
            with self.subTest(replacement=name):
                replaced = physical_root / relative
                original = replaced.read_bytes()
                replaced.write_bytes(original + b"\n")
                try:
                    self.assertFalse(evaluate_s1_authority_candidate(physical_root, copied_ref))
                    self.assertFalse(verify_s1_source_evidence(physical_root))
                finally:
                    replaced.write_bytes(original)

        semantic_mutations = (
            ("plan", "agent-workflows/evidence/sections/S1/plan.json",
             lambda value: value.__setitem__("status", "draft")),
            ("catalog", "agent-workflows/evidence/sections/S1/catalog.json",
             lambda value: value.__setitem__("section_id", "S2")),
            ("transaction", "agent-workflows/evidence/sections/S1/transaction.json",
             lambda value: value["provenance"].__setitem__("source", "other")),
            ("checkpoint", "agent-workflows/evidence/sections/S1/checkpoint.json",
             lambda value: value.__setitem__("closure_revision", 20)),
            ("next-s2", "agent-workflows/evidence/sections/S1/next-S2.json",
             lambda value: value.__setitem__("status", "accepted")),
            ("section-result", "agent-workflows/evidence/sections/S1/section-result.json",
             lambda value: value["recovery"].__setitem__("kind", "restore")),
        )
        for name, relative, mutate in semantic_mutations:
            with self.subTest(semantic_negative=name):
                node_path = physical_root / relative
                result_path = physical_root / "agent-workflows/evidence/sections/S1/section-result.json"
                index_path = physical_root / "agent-workflows/evidence/sections/S1/index.json"
                saved = {
                    path: path.read_bytes()
                    for path in {node_path, result_path, index_path, copied_authority}
                }
                try:
                    node = json.loads(node_path.read_text())
                    mutate(node)
                    node_path.write_text(json.dumps(node, sort_keys=True) + "\n", encoding="utf-8")
                    result = json.loads(result_path.read_text())
                    dependent_key = {
                        "plan": "plan_ref", "catalog": "catalog_ref",
                        "transaction": "transaction_ref", "checkpoint": "checkpoint_ref",
                    }.get(name)
                    if dependent_key is not None:
                        result[dependent_key]["digest"] = digest(node_path)
                        result_path.write_text(json.dumps(result, sort_keys=True) + "\n", encoding="utf-8")
                    index = json.loads(index_path.read_text())
                    rebound = {relative: digest(node_path)}
                    if dependent_key is not None:
                        rebound[result_path.relative_to(physical_root).as_posix()] = digest(result_path)
                    for document_ref in index["documents"]:
                        if document_ref["path"] in rebound:
                            document_ref["digest"] = rebound[document_ref["path"]]
                    index_path.write_text(json.dumps(index, sort_keys=True) + "\n", encoding="utf-8")
                    authority = json.loads(copied_authority.read_text())
                    authority["close_index"]["digest"] = digest(index_path)
                    if name == "checkpoint":
                        authority["checkpoint"]["digest"] = digest(node_path)
                    elif name == "next-s2":
                        authority["allowed_next_input"]["ref"]["digest"] = digest(node_path)
                    copied_authority.write_text(
                        json.dumps(authority, sort_keys=True) + "\n", encoding="utf-8"
                    )
                    semantic_ref = {"path": copied_ref["path"], "digest": digest(copied_authority)}
                    self.assertFalse(evaluate_s1_authority_candidate(physical_root, semantic_ref))
                finally:
                    for path, raw in saved.items():
                        path.write_bytes(raw)

        transaction_path = physical_root / "agent-workflows/evidence/sections/S1/transaction.json"
        transaction_path.write_text(
            json.dumps(json.loads(transaction_path.read_text()), indent=2, sort_keys=True) + "\n",
            encoding="utf-8",
        )
        result_path = physical_root / "agent-workflows/evidence/sections/S1/section-result.json"
        result = json.loads(result_path.read_text())
        result["transaction_ref"]["digest"] = digest(transaction_path)
        result_path.write_text(json.dumps(result, sort_keys=True) + "\n", encoding="utf-8")
        index_path = physical_root / "agent-workflows/evidence/sections/S1/index.json"
        index = json.loads(index_path.read_text())
        rebound = {
            transaction_path.relative_to(physical_root).as_posix(): digest(transaction_path),
            result_path.relative_to(physical_root).as_posix(): digest(result_path),
        }
        for document_ref in index["documents"]:
            if document_ref["path"] in rebound:
                document_ref["digest"] = rebound[document_ref["path"]]
        index_path.write_text(json.dumps(index, sort_keys=True) + "\n", encoding="utf-8")
        authority = json.loads(copied_authority.read_text())
        authority["close_index"]["digest"] = digest(index_path)
        copied_authority.write_text(json.dumps(authority, sort_keys=True) + "\n", encoding="utf-8")
        rotated_ref = {"path": copied_ref["path"], "digest": digest(copied_authority)}
        self.assertTrue(evaluate_s1_authority_candidate(physical_root, rotated_ref))
        self.assertFalse(verify_s1_source_evidence(physical_root))


if __name__ == "__main__":
    unittest.main()
