"""S0-E public integration acceptance boundaries."""

from __future__ import annotations

import copy
import hashlib
import json
import sys
import tempfile
import unittest
import shutil
from pathlib import Path
from unittest.mock import patch


ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "agent-workflows" / "src"))

from ai_agent_workflow.catalog import compile_registry  # noqa: E402
from ai_agent_workflow.control_kernel import (  # noqa: E402
    AuthorizationError,
    ControlKernel,
    InjectedCrash,
)
from ai_agent_workflow.section_control_plane import (  # noqa: E402
    SectionControlPlaneV1,
    open_group,
    open_section,
    resume,
    status,
)
from ai_agent_workflow.s0_evidence import (  # noqa: E402
    _validate_review_validation, _validate_state_snapshot, close_section_group,
    evaluate_source_transition_fixture, publish_section_accepted_result,
)
from ai_agent_workflow import s0_evidence  # noqa: E402


class S0IntegrationTests(unittest.TestCase):
    """Exercise the public S0 seam against a disposable kernel only."""

    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary.name)
        # A direct, disposable Run directory proves this fixture does not
        # create or use repository-style .local/agent/runs state.
        self.kernel = ControlKernel(self.root)
        self._artifact_serial = 0
        self.authority = {
            "approved": True,
            "scopes": [
                "entry", "publish_artifact", "close_epoch", "close_group",
                "source", "open_section", "open_group",
            ],
            "protected_fields": ["section", "group", "epoch", "ready"],
            "human_receipt": "s0-e-fixture-authority",
            "execution_class": "candidate-generic",
        }
        self.kernel.entry(
            "s0-e-fixture-run",
            {"path": "objectives/s0-e.md", "version": "v1", "digest": "a" * 64},
            workflow_version="workflow/v1",
            group_id="Bootstrap",
            epoch_id="Bootstrap-E1",
            authority_ref=self.authority,
        )
        self.refs = self._attest_sources(
            "section",
            [{"id": "S0.C1", "section_id": "S0", "group_id": "contracts-and-schema", "epoch_id": "S0-E1"}],
        )
        self.later_refs = self._attest_sources(
            "later",
            [{"id": "S0.D1", "section_id": "S0", "group_id": "Planning", "epoch_id": "S0-E2"}],
        )
        self.kernel.close_epoch(authority_ref=self.authority)
        self.kernel.close_group(
            next_group="contracts-and-schema", authority_ref=self.authority
        )

    def tearDown(self):
        self.temporary.cleanup()

    def _attest_sources(self, label, tasks):
        refs = {}
        for kind in (
            "plan", "catalog", "checkpoint", "bundle", "lifecycle_prerequisite",
            "closed_group_receipt",
        ):
            content = (
                json.dumps(
                    tasks,
                    sort_keys=True, separators=(",", ":"),
                ).encode("utf-8")
                if kind == "catalog" else ("s0-e-%s-" % label + kind).encode("utf-8")
            )
            path = self.root / ("source-%s-" % label + kind)
            path.write_bytes(content)
            digest = "sha256:" + hashlib.sha256(content).hexdigest()
            self.kernel.publish_artifact(
                "attest-%s-" % label + kind, "v1",
                {"ref_kind": kind, "source_digest": digest, "source_path": str(path)},
                kind="s0-source-attestation", authority_ref=self.authority,
            )
            refs[kind] = digest
        return refs

    def _open_section(self):
        fixture = json.loads(
            (ROOT / "agent-workflows/tests/fixtures/s0/contract-closed-bootstrap.json").read_text()
        )
        head = self.kernel.head()
        fixture["expected_head"] = {
            "revision": head["revision"], "transaction_digest": head["transaction_digest"],
        }
        fixture["authority"]["expected_head"] = copy.deepcopy(fixture["expected_head"])
        fixture["closed_bootstrap"]["run_id"] = "s0-e-fixture-run"
        fixture["plan"]["digest"] = fixture["closed_bootstrap"]["plan_digest"] = self.refs["plan"]
        fixture["catalog"]["digest"] = fixture["closed_bootstrap"]["catalog_digest"] = self.refs["catalog"]
        fixture["closed_bootstrap"]["checkpoint_ref"]["digest"] = self.refs["checkpoint"]
        fixture["closed_bootstrap"]["bundle_ref"]["digest"] = self.refs["bundle"]
        fixture["closed_bootstrap"]["lifecycle_prerequisite"]["receipt_digest"] = self.refs["lifecycle_prerequisite"]
        fixture["closed_bootstrap"]["closed_group_receipt"]["digest"] = self.refs["closed_group_receipt"]
        command = SectionControlPlaneV1().compile_registry(fixture)["open_section"]
        return open_section(self.kernel, command)

    def _open_group(self):
        fixture = json.loads(
            (ROOT / "agent-workflows/tests/fixtures/s0/contract-open-group.json").read_text()
        )
        head = self.kernel.head()
        fixture["expected_head"] = {
            "revision": head["revision"], "transaction_digest": head["transaction_digest"],
        }
        fixture["authority"]["expected_head"] = copy.deepcopy(fixture["expected_head"])
        fixture["closed_bootstrap"]["run_id"] = "s0-e-fixture-run"
        fixture["closed_bootstrap"]["group_id"] = "contracts-and-schema"
        fixture["closed_bootstrap"]["closed_group_receipt"]["group_id"] = "contracts-and-schema"
        fixture["closed_bootstrap"]["next_group"] = "Planning"
        fixture["groups"][0]["depends_on"] = ["contracts-and-schema"]
        refs = copy.deepcopy(self.later_refs)
        refs["lifecycle_prerequisite"] = self.kernel.read_state()["metadata"]["section_control"]["accepted_section_receipt"]["digest"]
        fixture["plan"]["digest"] = fixture["closed_bootstrap"]["plan_digest"] = refs["plan"]
        fixture["catalog"]["digest"] = fixture["closed_bootstrap"]["catalog_digest"] = refs["catalog"]
        fixture["closed_bootstrap"]["checkpoint_ref"]["digest"] = refs["checkpoint"]
        fixture["closed_bootstrap"]["bundle_ref"]["digest"] = refs["bundle"]
        fixture["closed_bootstrap"]["lifecycle_prerequisite"]["receipt_digest"] = refs["lifecycle_prerequisite"]
        fixture["closed_bootstrap"]["closed_group_receipt"]["digest"] = refs["closed_group_receipt"]
        return open_group(self.kernel, SectionControlPlaneV1().compile_open_group(fixture))

    def _fresh_direct_kernel(self):
        fresh = ControlKernel(self.root)
        # The public direct-Run entry form establishes this durable id; retain
        # it when reopening the same direct Run without switching layouts.
        fresh.run_id = self.kernel.run_id
        return fresh

    def test_f004_strict_publication_to_later_group_cold_recovery_chain(self):
        projection = compile_registry()
        self.assertEqual(60, projection["counts"]["named_targets"])
        self.assertEqual(23, projection["counts"]["profile_targets"])
        self.assertEqual(11, projection["counts"]["additional_surfaces"])
        self.assertEqual(60, projection["counts"]["accepted_named"])
        self.assertEqual(23, projection["counts"]["accepted_profiles"])

        before_open = self.kernel.head()
        opened = self._open_section()
        self.assertEqual(before_open["revision"] + 1, opened["revision"])
        open_head = self.kernel.head()
        pending = status(self.kernel, {
            "revision": open_head["revision"],
            "transaction_digest": open_head["transaction_digest"],
        })
        self.assertEqual("S0", pending["section_id"])
        self.assertEqual("contracts-and-schema", pending["group"]["id"])
        self.assertEqual("S0-E1", pending["epoch"]["id"])
        self.assertEqual(["S0.C1"], pending["ready"])
        self.assertEqual({"state": "pending"}, pending["transition_evidence"])
        self.assertFalse(pending["claims"]["source_transition_fixture_passed"])
        self.assertFalse(pending["claims"]["actual_a7"])
        self.assertFalse(pending["claims"]["activation"])
        self.assertFalse(pending["claims"]["full_ready"])

        document = self._strict_result()
        publish_section_accepted_result(
            self.kernel, "accepted-result", "v1", document, authority_ref=self.authority
        )
        evidence = self.kernel.read_state()["artifacts"]["accepted-result"]["digest"]
        self.kernel.close_epoch(authority_ref=self.authority)
        close_section_group(
            self.kernel,
            accepted_result_digest=evidence,
            next_group="Planning",
            authority_ref=self.authority,
        )
        accepted_head = self.kernel.head()
        accepted = status(self.kernel, {
            "revision": accepted_head["revision"],
            "transaction_digest": accepted_head["transaction_digest"],
        })
        self.assertTrue(accepted["claims"]["source_transition_fixture_passed"])
        self.assertFalse(accepted["claims"]["actual_a7"])
        self.assertFalse(accepted["claims"]["activation"])
        self.assertFalse(accepted["claims"]["full_ready"])
        self.assertEqual("accepted", accepted["transition_evidence"]["state"])
        self.assertEqual({
            "revision": accepted_head["revision"],
            "transaction_digest": accepted_head["transaction_digest"],
        }, accepted["transition_evidence"]["head"])
        receipt = accepted["transition_evidence"]["receipt_digest"]
        self.assertEqual(
            receipt,
            self.kernel.read_state()["metadata"]["section_control"]["accepted_section_receipt"]["digest"],
        )

        opened_later = self._open_group()
        self.assertEqual(accepted_head["revision"] + 1, opened_later["revision"])
        later_head = self.kernel.head()
        later = status(self.kernel, {
            "revision": later_head["revision"],
            "transaction_digest": later_head["transaction_digest"],
        })
        self.assertEqual("active", later["status"])
        self.assertEqual("Planning", later["group"]["id"])
        self.assertEqual("S0-E2", later["epoch"]["id"])
        self.assertEqual(["S0.D1"], later["ready"])
        self.assertEqual({"state": "pending"}, later["transition_evidence"])
        self.assertFalse(later["claims"]["source_transition_fixture_passed"])
        self.assertEqual(
            receipt,
            self.kernel.read_state()["metadata"]["section_control"]["parent_accepted_section_receipt"]["digest"],
        )

        exact_head = {
            "revision": later_head["revision"],
            "transaction_digest": later_head["transaction_digest"],
        }
        status_path = self.kernel.projection_dir / "status.json"
        status_path.write_text("forged", encoding="utf-8")
        forged_resume = resume(
            self._fresh_direct_kernel(), exact_head
        )
        self.assertEqual(later, forged_resume)
        self.assertEqual(exact_head, {
            "revision": self.kernel.head()["revision"],
            "transaction_digest": self.kernel.head()["transaction_digest"],
        })

        status_path.unlink()
        deleted_resume = resume(
            self._fresh_direct_kernel(), exact_head
        )
        self.assertEqual(later, deleted_resume)
        self.assertEqual(exact_head, {
            "revision": self.kernel.head()["revision"],
            "transaction_digest": self.kernel.head()["transaction_digest"],
        })
        self.assertFalse((self.root / ".local" / "agent" / "runs").exists())

    def test_f004_after_publish_before_head_recovery_quarantines_orphan_without_half_open_section(self):
        before = self.kernel.head()
        with self.kernel.fault("after_publish_before_head"):
            with self.assertRaises(InjectedCrash):
                self._open_section()
        self.assertEqual(before, self.kernel.head())
        recovery = self.kernel.recover()
        self.assertTrue(recovery["head_unchanged"])
        self.assertTrue(recovery["orphan_transactions"])
        self.assertEqual(before, self.kernel.head())
        self.assertEqual([], self.kernel.ready_tasks())
        self.assertNotIn("section_control", self.kernel.read_state().get("metadata", {}))
        self.assertFalse((self.root / ".local" / "agent" / "runs").exists())

    def _published_digest(self, artifact_id, kind, value):
        self._artifact_serial += 1
        artifact_id = "%s-%s" % (artifact_id, self._artifact_serial)
        self.kernel.publish_artifact(
            artifact_id, "v1", value, kind=kind, authority_ref=self.authority
        )
        return self.kernel.read_state()["artifacts"][artifact_id]["digest"]

    def _strict_result(self):
        artifact_refs, result_refs = {}, {}
        for slice_id in ("S0-A", "S0-B", "S0-C", "S0-D"):
            artifact_refs[slice_id] = {"digest": self._published_digest(
                "artifact-" + slice_id, "s0-accepted-artifact", {"slice_id": slice_id}
            )}
            result_refs[slice_id] = {"digest": self._published_digest(
                "result-" + slice_id, "s0-accepted-result", {"slice_id": slice_id}
            )}
        document = {
            "schema": "section-accepted-result/v1", "run_id": "s0-e-fixture-run",
            "section_id": "S0", "group_id": "contracts-and-schema", "result": "passed",
            "artifact_refs": artifact_refs, "result_refs": result_refs,
            "review_ref": {"digest": self._published_digest("section-review", "section-review", {
                "schema": "section-review/v1", "section_id": "S0", "group_id": "contracts-and-schema",
                "run_id": "s0-e-fixture-run", "verdict": "accepted", "findings": [],
                "review_budget": {"max_fixes": 5, "consumed": []},
            })},
            "finding_validation_ref": {"digest": None},
        }
        document["finding_validation_ref"]["digest"] = self._published_digest(
            "finding-validation", "finding-validation-set", {
                "schema": "finding-validation-set/v1", "section_id": "S0", "group_id": "contracts-and-schema",
                "run_id": "s0-e-fixture-run", "review_ref": document["review_ref"], "outcomes": [],
            }
        )
        return document

    def test_publication_boundary_rejects_a_minimal_result_without_distinct_ad_artifact_and_result_refs(self):
        self._open_section()
        before = self.kernel.head()
        with self.assertRaises(AuthorizationError):
            publish_section_accepted_result(
                self.kernel,
                "minimal-section-result",
                "v1",
                {
                    "schema": "section-accepted-result/v1",
                    "run_id": "s0-e-fixture-run",
                    "section_id": "S0",
                    "group_id": "contracts-and-schema",
                    "result": "passed",
                },
                authority_ref=self.authority,
            )
        self.assertEqual(before, self.kernel.head())

    def test_publication_boundary_publishes_strict_distinct_current_run_evidence(self):
        self._open_section()
        document = self._strict_result()
        publish_section_accepted_result(
            self.kernel, "accepted-result", "v1", document, authority_ref=self.authority
        )
        evidence = self.kernel.read_state()["artifacts"]["accepted-result"]["digest"]
        self.kernel.close_epoch(authority_ref=self.authority)
        close_section_group(self.kernel, accepted_result_digest=evidence,
                            next_group="Planning", authority_ref=self.authority)
        self.assertTrue(
            self.kernel.resume()["claims"]["source_transition_fixture_passed"]
        )

    def test_publication_boundary_rejects_substituted_slice_reference_without_publish(self):
        self._open_section()
        document = self._strict_result()
        document["result_refs"]["S0-D"] = copy.deepcopy(document["result_refs"]["S0-C"])
        before = self.kernel.head()
        with self.assertRaises(AuthorizationError):
            publish_section_accepted_result(
                self.kernel, "substituted-result", "v1", document, authority_ref=self.authority
            )
        self.assertEqual(before, self.kernel.head())

    def test_publication_boundary_rejects_placeholder_without_publish(self):
        self._open_section()
        document = self._strict_result()
        document["review_ref"]["digest"] = self._published_digest("placeholder", "section-review", {"schema": "section-review/v1"})
        before = self.kernel.head()
        with self.assertRaises(AuthorizationError):
            publish_section_accepted_result(self.kernel, "bad-placeholder", "v1", document, authority_ref=self.authority)
        self.assertEqual(before, self.kernel.head())

    def test_finding_join_rejects_open_omitted_invented_duplicate_and_invalid_attempt_history(self):
        review_ref = {"path": "agent-workflows/evidence/sections/S0/review.json", "digest": "sha256:" + "1" * 64}
        base_review = {"schema":"section-review/v1", "section_id":"S0", "group_id":"G", "run_id":"run", "verdict":"accepted", "findings":[{"id":"F1","state":"required"}], "review_budget":{"max_fixes":5,"consumed":[{"finding_id":"F1","attempt":1}]}}
        base_validation = {"schema":"finding-validation-set/v1", "section_id":"S0", "group_id":"G", "run_id":"run", "review_ref":review_ref, "outcomes":[{"finding_id":"F1","state":"closed"}]}
        _validate_review_validation(base_review, base_validation, run_id="run", group_id="G", review_ref=review_ref)
        for mutation in ("open", "needs-user", "omitted", "invented", "duplicate", "unbound", "gapped", "out-of-range"):
            review, validation = copy.deepcopy(base_review), copy.deepcopy(base_validation)
            if mutation == "open": validation["outcomes"][0]["state"] = "open"
            elif mutation == "needs-user": review["findings"][0]["state"] = "needs-user"; validation["outcomes"][0]["state"] = "needs-user"
            elif mutation == "omitted": validation["outcomes"] = []
            elif mutation == "invented": validation["outcomes"][0]["finding_id"] = "F2"
            elif mutation == "duplicate": validation["outcomes"] *= 2
            else: review["review_budget"]["consumed"] = [{"finding_id": "F2" if mutation == "unbound" else "F1", "attempt": 2 if mutation == "gapped" else 6 if mutation == "out-of-range" else 1}]
            with self.assertRaises(AuthorizationError, msg=mutation):
                _validate_review_validation(review, validation, run_id="run", group_id="G", review_ref=review_ref)

    def test_f002_attempt_history_is_contiguous_per_canonical_finding(self):
        review_ref = {"path": "agent-workflows/evidence/sections/S0/review.json", "digest": "sha256:" + "1" * 64}
        review = {"schema":"section-review/v1", "section_id":"S0", "group_id":"G", "run_id":"run", "verdict":"accepted", "findings":[{"id":"F1","state":"required"},{"id":"F2","state":"required"}], "review_budget":{"max_fixes":5,"consumed":[{"finding_id":"F1","attempt":1},{"finding_id":"F2","attempt":1}]}}
        validation = {"schema":"finding-validation-set/v1", "section_id":"S0", "group_id":"G", "run_id":"run", "review_ref":review_ref, "outcomes":[{"finding_id":"F1","state":"closed"},{"finding_id":"F2","state":"closed"}]}
        _validate_review_validation(review, validation, run_id="run", group_id="G", review_ref=review_ref)
        for consumed in ([{"finding_id":"F1","attempt":1},{"finding_id":"F2","attempt":2}], [{"finding_id":"F1","attempt":1},{"finding_id":"F1","attempt":3}], [{"finding_id":"F1","attempt":1},{"finding_id":"F1","attempt":1}]):
            rejected = copy.deepcopy(review); rejected["review_budget"]["consumed"] = consumed
            with self.assertRaises(AuthorizationError):
                _validate_review_validation(rejected, validation, run_id="run", group_id="G", review_ref=review_ref)
        single = copy.deepcopy(review); single["findings"] = [single["findings"][0]]; single["review_budget"]["consumed"] = [{"finding_id":"F1","attempt":1},{"finding_id":"F1","attempt":2}]; validation["outcomes"] = [validation["outcomes"][0]]
        _validate_review_validation(single, validation, run_id="run", group_id="G", review_ref=review_ref)

    def _source_close_set(self):
        root = Path(tempfile.mkdtemp())
        # A positive replay must carry the checked-in fixture and its fixed
        # authority package, rather than constructing a self-authorizing toy
        # closure.  Individual probes still mutate this caller-root copy.
        shutil.copytree(ROOT / "agent-workflows", root / "agent-workflows")
        docs = root / "docs" / "plans"
        docs.mkdir(parents=True)
        for name in (
            "ai-agent-workflow-full-implementation-plan.md",
            "ai-agent-workflow-s0-section-plan.md",
            "ai-agent-workflow-step-catalog.md",
        ):
            shutil.copy2(ROOT / "docs" / "plans" / name, docs / name)
        return root

    def _redigest_close_set(self, root):
        """Rebind every affected source reference after an adversarial edit."""
        evidence = root / "agent-workflows/evidence/sections/S0"
        index_path = evidence / "index.json"
        index = json.loads(index_path.read_text())
        original_bundle = json.loads((evidence / "bundle.json").read_text())
        path_by_digest = {
            ref["digest"]: ref["path"]
            for ref in [*index["refs"].values(), *original_bundle["artifacts"], *original_bundle["next_section_inputs"]]
        }

        def digest(path):
            return "sha256:" + hashlib.sha256((root / path).read_bytes()).hexdigest()

        def save(name, value):
            path = "agent-workflows/evidence/sections/S0/" + name
            (root / path).write_text(json.dumps(value, sort_keys=True), encoding="utf-8")
            return digest(path)

        def rebind(ref):
            ref["digest"] = digest(ref["path"])

        accepted = json.loads((evidence / "accepted.json").read_text())
        for field in ("artifact_refs", "result_refs"):
            for slice_id, ref in accepted[field].items():
                path = path_by_digest[ref["digest"]]
                ref["digest"] = digest(path)
        for field, name in (("review_ref", "review.json"), ("finding_validation_ref", "validation.json")):
            accepted[field]["digest"] = digest("agent-workflows/evidence/sections/S0/" + name)
        save("accepted.json", accepted)

        bundle = json.loads((evidence / "bundle.json").read_text())
        for ref in [*bundle["artifacts"], *bundle["next_section_inputs"]]:
            rebind(ref)
        snapshot = bundle.get("state_snapshot")
        if isinstance(snapshot, dict):
            for name in ("accepted", "invalidated", "unresolved", "next_section_inputs"):
                for member in snapshot.get(name, []):
                    rebind(member["ref"])
            rebind(snapshot["budget"]["review_ref"])
        bundle_digest = save("bundle.json", bundle)

        checkpoint = json.loads((evidence / "checkpoint.json").read_text())
        checkpoint["bundle_ref"]["digest"] = bundle_digest
        checkpoint_digest = save("checkpoint.json", checkpoint)

        result = json.loads((evidence / "section-result.json").read_text())
        result["bundle_ref"]["digest"] = bundle_digest
        result["checkpoint_ref"]["digest"] = checkpoint_digest
        for field, name in (("plan_ref", "plan.json"), ("catalog_ref", "catalog.json"), ("transaction_ref", "transaction.json")):
            result[field]["digest"] = digest("agent-workflows/evidence/sections/S0/" + name)
        updated_paths = {**path_by_digest, index["refs"]["accepted_result"]["digest"]: "agent-workflows/evidence/sections/S0/accepted.json", index["refs"]["section_bundle"]["digest"]: "agent-workflows/evidence/sections/S0/bundle.json", index["refs"]["checkpoint"]["digest"]: "agent-workflows/evidence/sections/S0/checkpoint.json"}
        for node in result["nodes"]:
            node["object_ref"]["digest"] = digest(updated_paths[node["object_ref"]["digest"]])

        def rebind_next_input_requires(graph):
            """Keep only next-input dependency targets aligned with their documents."""
            node_for_digest = {
                node["object_ref"]["digest"]: node["node_id"]
                for node in graph["nodes"]
            }
            input_targets = {
                node_for_digest[item["digest"]]: node_for_digest[
                    json.loads((root / item["path"]).read_text())["accepted_source_ref"]["digest"]
                ]
                for item in bundle["next_section_inputs"]
            }
            for edge in graph["edges"]:
                if edge["type"] == "requires" and edge["from"] in input_targets:
                    edge["to"] = input_targets[edge["from"]]

        rebind_next_input_requires(result)
        result_digest = save("section-result.json", result)

        for key, ref in index["refs"].items():
            ref["digest"] = digest(ref["path"])
        updated_paths[index["refs"]["section_result"]["digest"]] = "agent-workflows/evidence/sections/S0/section-result.json"
        for node in index["nodes"]:
            old = node["object_ref"]["digest"]
            node["object_ref"]["digest"] = result_digest if node["node_id"] == "result" else digest(path_by_digest[old])
        rebind_next_input_requires(index)
        index_path.write_text(json.dumps(index, sort_keys=True), encoding="utf-8")

    def _snapshot_context(self, root):
        """Load canonical close evidence for direct snapshot-predicate probes."""
        evidence = root / "agent-workflows/evidence/sections/S0"
        index = json.loads((evidence / "index.json").read_text())
        bundle = json.loads((evidence / "bundle.json").read_text())
        accepted = json.loads((evidence / "accepted.json").read_text())
        review = json.loads((evidence / "review.json").read_text())
        materialized = {item["digest"]: item for item in bundle["artifacts"]}
        inputs = bundle["next_section_inputs"]
        input_documents = {item["digest"]: json.loads((root / item["path"]).read_text()) for item in inputs}
        slice_digests, slice_documents = [], {}
        for field in ("artifact_refs", "result_refs"):
            for ref in accepted[field].values():
                slice_digests.append(ref["digest"])
                slice_documents[ref["digest"]] = json.loads((root / materialized[ref["digest"]]["path"]).read_text())
        return bundle["state_snapshot"], index["refs"], materialized, slice_digests, slice_documents, inputs, input_documents, review

    def test_f001_snapshot_predicate_rejects_omission_overlap_budget_and_input_disagreement(self):
        root = self._source_close_set()
        self.addCleanup(shutil.rmtree, root, ignore_errors=True)
        base = self._snapshot_context(root)
        for mutation in ("omit", "duplicate", "overlap", "terminal", "budget-max", "budget-history", "input"):
            with self.subTest(mutation=mutation):
                snapshot = copy.deepcopy(base[0])
                if mutation == "omit": del snapshot["accepted"]
                elif mutation == "duplicate": snapshot["accepted"].append(copy.deepcopy(snapshot["accepted"][0]))
                elif mutation == "overlap": snapshot["invalidated"].append(copy.deepcopy(snapshot["accepted"][0]))
                elif mutation == "terminal": snapshot["unresolved"].append(copy.deepcopy(snapshot["accepted"][0]))
                elif mutation == "budget-max": snapshot["budget"]["max_fixes"] += 1
                elif mutation == "budget-history": snapshot["budget"]["consumed"] = []
                else: snapshot["next_section_inputs"][0]["identity"] = "wrong-input"
                with self.assertRaises(AuthorizationError):
                    _validate_state_snapshot(snapshot, refs=base[1], materialized=base[2], slice_digests=base[3], slice_documents=base[4], inputs=base[5], input_documents=base[6], review=base[7])

    def test_f002_fixed_authority_rejects_each_full_redigest_substitution(self):
        cases = ("history-delete", "history-replace", "attempt-reset", "plan-placeholder", "catalog-placeholder", "caller-source", "coherent-head-transaction", "checkpoint-id", "checkpoint-digest", "input-id", "input-group", "input-digest", "index-raw")
        for mutation in cases:
            with self.subTest(mutation=mutation):
                root = self._source_close_set()
                self.addCleanup(shutil.rmtree, root, ignore_errors=True)
                evidence = root / "agent-workflows/evidence/sections/S0"
                if mutation == "history-delete":
                    review = json.loads((evidence / "review.json").read_text()); review["findings"] = review["findings"][1:]; review["review_budget"]["consumed"] = [x for x in review["review_budget"]["consumed"] if x["finding_id"] != "S0-W1-F001"]; (evidence / "review.json").write_text(json.dumps(review, sort_keys=True))
                    validation = json.loads((evidence / "validation.json").read_text()); validation["outcomes"] = validation["outcomes"][1:]; (evidence / "validation.json").write_text(json.dumps(validation, sort_keys=True))
                elif mutation == "history-replace":
                    review = json.loads((evidence / "review.json").read_text()); review["findings"][0]["id"] = "S0-REPLACED"; review["review_budget"]["consumed"][0]["finding_id"] = "S0-REPLACED"; (evidence / "review.json").write_text(json.dumps(review, sort_keys=True))
                    validation = json.loads((evidence / "validation.json").read_text()); validation["outcomes"][0]["finding_id"] = "S0-REPLACED"; (evidence / "validation.json").write_text(json.dumps(validation, sort_keys=True))
                elif mutation == "attempt-reset":
                    review = json.loads((evidence / "review.json").read_text()); review["review_budget"]["consumed"] = [x for x in review["review_budget"]["consumed"] if x["finding_id"] != "S0-W1-F001"]; review["review_budget"]["consumed"].append({"finding_id":"S0-W1-F001", "attempt":1}); (evidence / "review.json").write_text(json.dumps(review, sort_keys=True))
                elif mutation in {"plan-placeholder", "catalog-placeholder"}:
                    (evidence / ("plan.json" if mutation == "plan-placeholder" else "catalog.json")).write_text('{"schema":"s0-plan-evidence/v1","run_id":"s0-source-transition-2026-09-04","section_id":"S0"}')
                elif mutation == "caller-source":
                    path = root / "docs/plans/ai-agent-workflow-step-catalog.md"; path.write_text(path.read_text() + "\nchanged")
                elif mutation == "coherent-head-transaction":
                    result = json.loads((evidence / "section-result.json").read_text()); result["expected_head"]["revision"] = 2; result["expected_head"]["transaction_digest"] = "sha256:" + "4" * 64; (evidence / "section-result.json").write_text(json.dumps(result, sort_keys=True))
                    transaction = json.loads((evidence / "transaction.json").read_text()); transaction["expected_head"]["revision"] = 2; transaction["expected_head"]["transaction_digest"] = "sha256:" + "4" * 64; (evidence / "transaction.json").write_text(json.dumps(transaction, sort_keys=True))
                elif mutation == "checkpoint-id":
                    checkpoint = json.loads((evidence / "checkpoint.json").read_text()); checkpoint["checkpoint_id"] = "replacement-cp"; (evidence / "checkpoint.json").write_text(json.dumps(checkpoint, sort_keys=True))
                elif mutation == "checkpoint-digest":
                    checkpoint = json.loads((evidence / "checkpoint.json").read_text()); checkpoint["closure_revision"] = 2; (evidence / "checkpoint.json").write_text(json.dumps(checkpoint, sort_keys=True))
                elif mutation in {"input-id", "input-group", "input-digest"}:
                    item = json.loads((evidence / "next-S0-A.json").read_text())
                    if mutation == "input-id":
                        item["input_id"] = "replacement-input"
                    elif mutation == "input-group":
                        item["group_id"] = "replacement-group"
                    else:
                        # This strict document has no non-identity semantic field.
                        # Substitute another real accepted source while preserving the
                        # input identity, then rebind the complete closure below.
                        source = json.loads((evidence / "next-S0-B.json").read_text())
                        item["accepted_source_ref"]["digest"] = source["accepted_source_ref"]["digest"]
                    (evidence / "next-S0-A.json").write_text(json.dumps(item, sort_keys=True))
                else:
                    index = json.loads((evidence / "index.json").read_text()); index["run_id"] = "replacement-run"; (evidence / "index.json").write_text(json.dumps(index, sort_keys=True))
                self._redigest_close_set(root)
                if mutation == "input-digest":
                    traced_authority = json.loads((root / "agent-workflows/manifests/s0-source-transition-authority.json").read_text())
                    index = json.loads((evidence / "index.json").read_text())
                    bundle = json.loads((evidence / "bundle.json").read_text())
                    checkpoint = json.loads((evidence / "checkpoint.json").read_text())
                    traced_authority["close_index"]["digest"] = "sha256:" + hashlib.sha256((evidence / "index.json").read_bytes()).hexdigest()
                    traced_authority["checkpoint"] = {"checkpoint_id": checkpoint["checkpoint_id"], "digest": index["refs"]["checkpoint"]["digest"]}
                    traced_authority["allowed_next_inputs"] = [
                        {"identity": json.loads((root / item["path"]).read_text())["input_id"], "digest": item["digest"]}
                        for item in bundle["next_section_inputs"]
                    ]
                    # The trace substitutes matching authority only; the normal
                    # assertion below still exercises the immutable authority.
                    with patch.object(s0_evidence, "_load_source_transition_authority", return_value=traced_authority):
                        self.assertTrue(evaluate_source_transition_fixture(root))
                self.assertFalse(evaluate_source_transition_fixture(root))

    def test_source_root_replay_requires_the_complete_digest_bound_close_set(self):
        root = self._source_close_set()
        self.addCleanup(shutil.rmtree, root, ignore_errors=True)
        self.assertTrue(evaluate_source_transition_fixture(root))
        index = root / "agent-workflows/evidence/sections/S0/index.json"
        original = index.read_text()
        for mutation in ("delete", "tamper", "duplicate", "traversal", "glob", "windows", "trailing"):
            index.write_text(original)
            if mutation == "delete": (root / "agent-workflows/evidence/sections/S0/review.json").unlink()
            elif mutation == "tamper": (root / "agent-workflows/evidence/sections/S0/review.json").write_text("{}")
            else:
                data = json.loads(original)
                data["refs"]["checkpoint"]["path"] = {
                    "duplicate": data["refs"]["section_review"]["path"], "traversal": "../review.json",
                    "glob": "agent-workflows/evidence/sections/S0/*.json", "windows": "C:\\review.json",
                    "trailing": "agent-workflows/evidence/sections/S0/",
                }[mutation]
                index.write_text(json.dumps(data))
            self.assertFalse(evaluate_source_transition_fixture(root), mutation)
            if mutation in ("delete", "tamper"):
                # Rebuild a pristine close set for the next independent fault.
                shutil.rmtree(root); root = self._source_close_set()
                index = root / "agent-workflows/evidence/sections/S0/index.json"; original = index.read_text()

    def test_source_replay_requires_the_fixed_current_authority_lineage(self):
        root = self._source_close_set()
        self.addCleanup(shutil.rmtree, root, ignore_errors=True)
        current_path = root / "agent-workflows/manifests/s0-source-transition-current-authority.json"
        current = json.loads(current_path.read_text(encoding="utf-8"))

        self.assertEqual(
            {
                "path": "agent-workflows/manifests/s0-source-transition-authority.json",
                "digest": "sha256:673114ec3e1ea69fcd9580805b7f872895062253a0a1ccc352f40f281ccc3070",
            },
            current["predecessor_authority"],
        )
        self.assertEqual(
            {
                "path": "agent-workflows/evidence/current-canonical-lineage.json",
                "digest": "sha256:2359bc900a2124995c7347ab6ddb4e5e7353a1eb79c648d40cbdaf4fa943013f",
            },
            current["current_canonical_lineage"],
        )
        self.assertEqual(
            "sha256:4f9f4940cb820f4c71eeae770f6cd6d8ec3b0dce56e625642c431722f0bbf68e",
            s0_evidence._CURRENT_AUTHORITY_SCHEMA_DIGEST,
        )
        self.assertTrue(evaluate_source_transition_fixture(root))

    def test_source_replay_rejects_old_missing_stale_or_tampered_current_authority(self):
        for mutation in (
            "absent-current-authority",
            "missing-predecessor",
            "old-current-authority",
            "stale-plan",
            "stale-catalog",
            "schema-invalid-authority",
            "tampered-current-schema",
            "cross-root-authority",
        ):
            with self.subTest(mutation=mutation):
                root = self._source_close_set()
                self.addCleanup(shutil.rmtree, root, ignore_errors=True)
                manifests = root / "agent-workflows/manifests"
                current_path = manifests / "s0-source-transition-current-authority.json"
                if mutation == "absent-current-authority":
                    current_path.unlink()
                elif mutation == "missing-predecessor":
                    (manifests / "s0-source-transition-authority.json").unlink()
                elif mutation == "old-current-authority":
                    current_path.write_bytes(
                        (manifests / "s0-source-transition-authority.json").read_bytes()
                    )
                elif mutation in {"stale-plan", "stale-catalog"}:
                    name = {
                        "stale-plan": "ai-agent-workflow-full-implementation-plan.md",
                        "stale-catalog": "ai-agent-workflow-step-catalog.md",
                    }[mutation]
                    path = root / "docs/plans" / name
                    path.write_text(path.read_text(encoding="utf-8") + "\nstale\n", encoding="utf-8")
                elif mutation == "schema-invalid-authority":
                    current = json.loads(current_path.read_text(encoding="utf-8"))
                    current["caller_digest"] = "sha256:" + "0" * 64
                    current_path.write_text(json.dumps(current, sort_keys=True), encoding="utf-8")
                elif mutation == "tampered-current-schema":
                    schema_path = (
                        root
                        / "agent-workflows/schemas"
                        / "s0-source-transition-current-authority-v1.schema.json"
                    )
                    schema_path.write_text(
                        schema_path.read_text(encoding="utf-8") + "\n",
                        encoding="utf-8",
                    )
                else:
                    external = root.parent / (root.name + "-external-current-authority.json")
                    external.write_bytes(current_path.read_bytes())
                    self.addCleanup(external.unlink, missing_ok=True)
                    current_path.unlink()
                    current_path.symlink_to(external)
                self.assertFalse(evaluate_source_transition_fixture(root), mutation)

    def test_source_replay_rejects_coherently_redigested_current_lineage(self):
        root = self._source_close_set()
        self.addCleanup(shutil.rmtree, root, ignore_errors=True)
        plan_path = root / "docs/plans/ai-agent-workflow-full-implementation-plan.md"
        plan_path.write_text(plan_path.read_text(encoding="utf-8") + "\nreplacement\n", encoding="utf-8")
        replacement_digest = "sha256:" + hashlib.sha256(plan_path.read_bytes()).hexdigest()

        lineage_path = root / "agent-workflows/evidence/current-canonical-lineage.json"
        lineage = json.loads(lineage_path.read_text(encoding="utf-8"))
        next(
            item for item in lineage["canonical_inputs"]
            if item["role"] == "full-implementation-plan"
        )["digest"] = replacement_digest
        lineage_path.write_text(json.dumps(lineage, sort_keys=True), encoding="utf-8")

        current_path = root / "agent-workflows/manifests/s0-source-transition-current-authority.json"
        current = json.loads(current_path.read_text(encoding="utf-8"))
        current["current_canonical_lineage"]["digest"] = (
            "sha256:" + hashlib.sha256(lineage_path.read_bytes()).hexdigest()
        )
        current["current_sources"][0]["digest"] = replacement_digest
        current_path.write_text(json.dumps(current, sort_keys=True), encoding="utf-8")
        self.assertFalse(evaluate_source_transition_fixture(root))

    def test_f003_replay_rejects_raw_slices_and_non_exact_whole_graph(self):
        root = self._source_close_set()
        self.addCleanup(shutil.rmtree, root, ignore_errors=True)
        index_path = root / "agent-workflows/evidence/sections/S0/index.json"
        original = index_path.read_text()
        evidence = root / "agent-workflows/evidence/sections/S0"
        for mutation in ("raw_slice", "wrong_identity", "omit_node", "omit_edge", "extra_edge", "cycle", "checkpoint_mismatch", "dangling_input"):
            index_path.write_text(original)
            if mutation == "raw_slice": (evidence / "artifact-S0-A.json").write_text('{"slice":"raw"}')
            elif mutation == "wrong_identity": (evidence / "result-S0-A.json").write_text('{"schema":"s0-slice-evidence/v1","run_id":"fixture","section_id":"S0","group_id":"contracts-and-schema","slice_id":"S0-B","evidence_kind":"result","status":"passed"}')
            elif mutation == "checkpoint_mismatch": (evidence / "checkpoint.json").write_text('{"schema":"checkpoint/v1"}')
            elif mutation == "dangling_input": (evidence / "next-S0-A.json").write_text('{"schema":"s0-next-section-input/v1","run_id":"fixture","section_id":"S0","group_id":"contracts-and-schema","input_id":"next","accepted_source_ref":{"digest":"sha256:' + '0' * 64 + '"}}')
            else:
                data = json.loads(original)
                if mutation == "omit_node": data["nodes"] = data["nodes"][1:]
                elif mutation == "omit_edge": data["edges"] = data["edges"][1:]
                elif mutation == "extra_edge": data["edges"].append({**data["edges"][0], "edge_id":"extra"})
                elif mutation == "cycle": data["edges"].append({"edge_id":"cycle","from":data["edges"][0]["to"],"to":data["edges"][0]["from"],"type":"requires"})
                index_path.write_text(json.dumps(data))
            self.assertFalse(evaluate_source_transition_fixture(root), mutation)
            if mutation in {"raw_slice", "wrong_identity", "checkpoint_mismatch", "dangling_input"}:
                shutil.rmtree(root); root = self._source_close_set(); index_path = root / "agent-workflows/evidence/sections/S0/index.json"; original = index_path.read_text(); evidence = root / "agent-workflows/evidence/sections/S0"
        for mutation in ("duplicate_result_node", "duplicate_result_edge", "checkpoint_id_mismatch", "wrong_group_and_duplicate_input_id"):
            root = self._source_close_set()
            self.addCleanup(shutil.rmtree, root, ignore_errors=True)
            evidence = root / "agent-workflows/evidence/sections/S0"
            if mutation.startswith("duplicate_result"):
                result_path = evidence / "section-result.json"
                result = json.loads(result_path.read_text())
                field = "nodes" if mutation == "duplicate_result_node" else "edges"
                result[field].append(copy.deepcopy(result[field][0]))
                result_path.write_text(json.dumps(result, sort_keys=True), encoding="utf-8")
            elif mutation == "checkpoint_id_mismatch":
                checkpoint_path = evidence / "checkpoint.json"
                checkpoint = json.loads(checkpoint_path.read_text())
                checkpoint["checkpoint_id"] = "cp-s0-mismatch"
                checkpoint_path.write_text(json.dumps(checkpoint, sort_keys=True), encoding="utf-8")
            else:
                input_path = evidence / "next-S0-A.json"
                next_input = json.loads(input_path.read_text())
                next_input["group_id"] = "wrong-group"
                next_input["input_id"] = "next-S0-B"
                input_path.write_text(json.dumps(next_input, sort_keys=True), encoding="utf-8")
            self._redigest_close_set(root)
            self.assertFalse(evaluate_source_transition_fixture(root), mutation)

    def test_source_replay_rejects_missing_unsuccessful_or_tampered_acceptance_test(self):
        for mutation in ("missing", "failed", "command_tamper"):
            with self.subTest(mutation=mutation):
                root = self._source_close_set()
                self.addCleanup(shutil.rmtree, root, ignore_errors=True)
                transaction_path = root / "agent-workflows/evidence/sections/S0/transaction.json"
                transaction = json.loads(transaction_path.read_text())
                if mutation == "missing":
                    del transaction["acceptance_test"]
                elif mutation == "failed":
                    transaction["acceptance_test"]["exit_status"] = 1
                    transaction["acceptance_test"]["status"] = "failed"
                else:
                    transaction["acceptance_test"]["command"] += " -v"
                transaction_path.write_text(json.dumps(transaction, sort_keys=True), encoding="utf-8")
                self._redigest_close_set(root)
                self.assertFalse(evaluate_source_transition_fixture(root))


if __name__ == "__main__":
    unittest.main()
