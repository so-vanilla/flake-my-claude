import hashlib
import json
import sys
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "src"))

from ai_agent_workflow.section_control_plane import (  # noqa: E402
    SectionContractError,
    SectionControlPlaneV1,
    open_group,
    open_section,
    resume,
    status,
    validate_section_status,
)
from ai_agent_workflow.schema_validation import SchemaValidationError, validate_document  # noqa: E402


class KernelDouble:
    def __init__(self):
        self.applied = []
        self.status_calls = []
        self.resume_calls = []

    def apply(self, command):
        self.applied.append(command)
        return {"accepted": True, "receipt": "one"}

    def status(self, **kwargs):
        self.status_calls.append(kwargs)
        return self._status_document()

    def resume(self, **kwargs):
        self.resume_calls.append(kwargs)
        return self._status_document()

    @staticmethod
    def _status_document():
        return {
            "schema": "section-status/v1",
            "section_id": "S0",
            "status": "active",
            "group": {"id": "contracts-and-schema", "status": "open", "next_group": None},
            "epoch": {"id": "S0-E1", "status": "open", "group_id": "contracts-and-schema", "boundary_reason": "open_section", "clear_before_next": False},
            "ready": ["S0.C1"],
            "head": {"schema": "dag-head/v1", "run_id": "run-fixture", "workflow_version": "workflow/v1", "graph_version": "artifact-task-dag/v1", "role": "orchestrator", "revision": 7, "state_revision": 7, "transaction_digest": "sha256:" + "c" * 64, "digest": "sha256:" + "d" * 64},
            "claims": {"source_transition_fixture_passed": False, "actual_a7": False, "activation": False, "full_ready": False},
            "transition_evidence": {"state": "pending"},
        }


class SectionControlPlaneContractTests(unittest.TestCase):
    def test_compiles_closed_bootstrap_into_open_section_command(self):
        fixture = json.loads(
            (ROOT / "tests" / "fixtures" / "s0" / "contract-closed-bootstrap.json").read_text()
        )

        compiled = SectionControlPlaneV1().compile_registry(fixture)

        self.assertEqual(compiled["schema"], "section-control-plane/v1")
        self.assertEqual(compiled["schedule"]["first_frontier"], ["S0.C1"])
        self.assertEqual(compiled["open_section"]["schema"], "dag-command/v1")
        self.assertEqual(compiled["open_section"]["command_type"], "open_section")
        self.assertEqual(compiled["open_section"]["payload"]["transition"], {"intent": "source-transition-fixture-passed", "state": "pending"})
        self.assertFalse(compiled["status"]["claims"]["source_transition_fixture_passed"])
        self.assertFalse(compiled["status"]["claims"]["actual_a7"])

    def test_compiles_accepted_s0_into_strict_s1_successor_and_history(self):
        fixture = json.loads(
            (ROOT / "tests" / "fixtures" / "s1" / "contract-accepted-s0.json").read_text()
        )

        compiled = SectionControlPlaneV1().compile_registry(fixture)

        self.assertEqual(compiled["section"]["id"], "S1")
        self.assertEqual(compiled["schedule"]["first_frontier"], ["S1.A1"])
        self.assertEqual(
            {ref["kind"] for ref in compiled["open_section"]["input_refs"]},
            {"plan", "catalog", "checkpoint", "bundle", "lifecycle_prerequisite", "accepted_section_receipt"},
        )
        self.assertEqual(
            compiled["open_section"]["payload"]["preconditions"]["predecessor_section_id"], "S0"
        )
        self.assertEqual(
            compiled["status"]["section_history"],
            [
                {
                    "position": 0,
                    "section_id": "S0",
                    "accepted_receipt_ref": fixture["accepted_section"]["accepted_section_receipt"],
                    "bundle_ref": fixture["accepted_section"]["bundle_ref"],
                    "checkpoint_ref": fixture["accepted_section"]["checkpoint_ref"],
                }
            ],
        )
        kernel = KernelDouble()
        self.assertEqual(open_section(kernel, compiled["open_section"]), {"accepted": True, "receipt": "one"})
        self.assertEqual(kernel.applied, [compiled["open_section"]])
        schemas = {
            name: json.loads((ROOT / "schemas" / filename).read_text())
            for name, filename in {
                "plan": "section-plan-v1.schema.json",
                "control": "section-control-plane-v1.schema.json",
                "status": "section-status-v1.schema.json",
            }.items()
        }
        validate_document(fixture, schemas["plan"])
        validate_document(compiled, schemas["control"])
        validate_document(compiled["status"], schemas["status"])
        validate_section_status(compiled["status"])

    def test_rejects_non_immediate_or_incoherent_s1_successor_inputs(self):
        baseline = json.loads(
            (ROOT / "tests" / "fixtures" / "s1" / "contract-accepted-s0.json").read_text()
        )
        cases = []
        skip = json.loads(json.dumps(baseline))
        skip["section"]["id"] = "S2"
        cases.append(skip)
        reopen = json.loads(json.dumps(baseline))
        reopen["section"]["id"] = "S0"
        cases.append(reopen)
        backward = json.loads(json.dumps(baseline))
        backward["accepted_section"]["section_id"] = "S1"
        cases.append(backward)
        wrong_receipt = json.loads(json.dumps(baseline))
        wrong_receipt["accepted_section"]["lifecycle_prerequisite"]["receipt_digest"] = "sha256:" + "9" * 64
        cases.append(wrong_receipt)
        malformed_catalog = json.loads(json.dumps(baseline))
        malformed_catalog["catalog"]["tasks"][0]["section_id"] = "S0"
        malformed_catalog["catalog"]["digest"] = "sha256:" + hashlib.sha256(
            json.dumps(malformed_catalog["catalog"]["tasks"], sort_keys=True, separators=(",", ":"), ensure_ascii=True).encode("utf-8")
        ).hexdigest()
        malformed_catalog["accepted_section"]["catalog_digest"] = malformed_catalog["catalog"]["digest"]
        malformed_catalog["plan"]["digest"] = malformed_catalog["accepted_section"]["plan_digest"]
        cases.append(malformed_catalog)
        malformed_frontier = json.loads(json.dumps(baseline))
        malformed_frontier["groups"][0]["first_frontier"] = ["S1.Missing"]
        cases.append(malformed_frontier)

        for fixture in cases:
            with self.subTest(fixture=fixture["section"]["id"]):
                with self.assertRaises(SectionContractError):
                    SectionControlPlaneV1().compile_registry(fixture)

    def test_rejects_incoherent_s1_status_history(self):
        fixture = json.loads(
            (ROOT / "tests" / "fixtures" / "s1" / "contract-accepted-s0.json").read_text()
        )
        status_document = {
            "schema": "section-status/v1",
            "section_id": "S1",
            "claims": {"source_transition_fixture_passed": False, "actual_a7": False, "activation": False, "full_ready": False},
            "transition_evidence": {"state": "pending"},
            "section_history": [
                {
                    "position": 0,
                    "section_id": "S0",
                    "accepted_receipt_ref": fixture["accepted_section"]["accepted_section_receipt"],
                    "bundle_ref": fixture["accepted_section"]["bundle_ref"],
                    "checkpoint_ref": fixture["accepted_section"]["checkpoint_ref"],
                }
            ],
        }
        cases = []
        missing = json.loads(json.dumps(status_document))
        del missing["section_history"]
        cases.append(missing)
        empty = json.loads(json.dumps(status_document))
        empty["section_history"] = []
        cases.append(empty)
        gap = json.loads(json.dumps(status_document))
        gap["section_history"][0]["position"] = 1
        cases.append(gap)
        reverse = json.loads(json.dumps(status_document))
        reverse["section_history"][0]["section_id"] = "S1"
        cases.append(reverse)
        duplicate = json.loads(json.dumps(status_document))
        duplicate["section_history"].append(json.loads(json.dumps(duplicate["section_history"][0])))
        cases.append(duplicate)
        bad_path = json.loads(json.dumps(status_document))
        bad_path["section_history"][0]["accepted_receipt_ref"]["path"] = "objects/not-a-digest.json"
        cases.append(bad_path)
        for document in cases:
            with self.subTest(document=document.get("section_history")):
                with self.assertRaises(SchemaValidationError):
                    validate_section_status(document)

    def test_public_open_adapters_delegate_exactly_once_and_return_kernel_receipt(self):
        fixture = json.loads((ROOT / "tests" / "fixtures" / "s0" / "contract-closed-bootstrap.json").read_text())
        command = SectionControlPlaneV1().compile_registry(fixture)["open_section"]
        kernel = KernelDouble()

        self.assertEqual(open_section(kernel, command), {"accepted": True, "receipt": "one"})
        self.assertEqual(kernel.applied, [command])

        later = json.loads((ROOT / "tests" / "fixtures" / "s0" / "contract-open-group.json").read_text())
        group_command = SectionControlPlaneV1().compile_open_group(later)
        self.assertEqual(open_group(kernel, group_command), {"accepted": True, "receipt": "one"})
        self.assertEqual(kernel.applied, [command, group_command])

    def test_public_open_group_accepts_an_s1_same_section_closed_group_receipt_once(self):
        fixture = json.loads(
            (ROOT / "tests" / "fixtures" / "s0" / "contract-open-group.json").read_text()
        )
        fixture["section"]["id"] = "S1"
        fixture["groups"] = [{
            "id": "review", "depends_on": ["analysis"], "first_epoch": "S1-E2",
            "first_frontier": ["S1.R1"],
        }]
        fixture["closed_bootstrap"]["group_id"] = "analysis"
        fixture["closed_bootstrap"]["next_group"] = "review"
        fixture["closed_bootstrap"]["closed_group_receipt"]["group_id"] = "analysis"
        fixture["closed_bootstrap"]["lifecycle_prerequisite"] = {
            "kind": "accepted-section", "section_id": "S1",
            "receipt_digest": "sha256:" + "f" * 64,
        }
        fixture["catalog"]["tasks"] = [{
            "id": "S1.R1", "section_id": "S1", "group_id": "review", "epoch_id": "S1-E2",
        }]
        fixture["catalog"]["digest"] = "sha256:" + hashlib.sha256(
            json.dumps(fixture["catalog"]["tasks"], sort_keys=True, separators=(",", ":"), ensure_ascii=True).encode()
        ).hexdigest()
        fixture["closed_bootstrap"]["catalog_digest"] = fixture["catalog"]["digest"]

        command = SectionControlPlaneV1().compile_open_group(fixture)
        kernel = KernelDouble()

        self.assertEqual(open_group(kernel, command), {"accepted": True, "receipt": "one"})
        self.assertEqual(kernel.applied, [command])
        wrong_receipt = json.loads(json.dumps(command))
        wrong_receipt["input_refs"][-1]["kind"] = "accepted_section_receipt"
        with self.assertRaises(SchemaValidationError):
            open_group(kernel, wrong_receipt)
        self.assertEqual(kernel.applied, [command])

    def test_public_status_and_resume_only_forward_expected_head_to_kernel(self):
        kernel = KernelDouble()
        expected_head = {"revision": 7, "transaction_digest": "sha256:" + "c" * 64}

        self.assertEqual(status(kernel, expected_head)["status"], "active")
        self.assertEqual(resume(kernel, expected_head)["status"], "active")
        expected = {"expected_revision": 7, "expected_head_digest": expected_head["transaction_digest"]}
        self.assertEqual(kernel.status_calls, [expected])
        self.assertEqual(kernel.resume_calls, [expected])

    def test_public_status_and_resume_reject_incoherent_kernel_status(self):
        class BadKernel(KernelDouble):
            def status(self, **kwargs):
                result = super().status(**kwargs)
                result["claims"]["source_transition_fixture_passed"] = True
                return result
            def resume(self, **kwargs):
                result = super().resume(**kwargs)
                result["transition_evidence"] = {"state": "accepted", "receipt_digest": "sha256:" + "1" * 64, "head": {"revision": 7, "transaction_digest": "sha256:" + "c" * 64}}
                return result
        kernel = BadKernel()
        head = {"revision": 7, "transaction_digest": "sha256:" + "c" * 64}
        with self.assertRaises(SchemaValidationError): status(kernel, head)
        with self.assertRaises(SchemaValidationError): resume(kernel, head)
        self.assertEqual(len(kernel.status_calls), 1)
        self.assertEqual(len(kernel.resume_calls), 1)

    def test_rejects_unresolved_or_cross_section_frontier_before_constructing_command(self):
        fixture = json.loads((ROOT / "tests" / "fixtures" / "s0" / "contract-closed-bootstrap.json").read_text())
        fixture["groups"][0]["first_frontier"] = ["S99.A1"]
        with self.assertRaisesRegex(SectionContractError, "catalog"):
            SectionControlPlaneV1().compile_registry(fixture)

    def test_rejects_bare_duplicate_and_epoch_conflicting_frontiers_before_command_construction(self):
        baseline = json.loads((ROOT / "tests" / "fixtures" / "s0" / "contract-closed-bootstrap.json").read_text())
        cases = {
            "bare": ["A1"],
            "duplicate": ["S1.A1", "S1.A1"],
            "epoch": ["S1.A1"],
        }
        for name, frontier in cases.items():
            with self.subTest(name=name):
                fixture = json.loads(json.dumps(baseline))
                fixture["groups"][0]["first_frontier"] = frontier
                if name == "epoch":
                    fixture["groups"][0]["first_epoch"] = "S1-E9"
                with self.assertRaises(SectionContractError):
                    SectionControlPlaneV1().compile_registry(fixture)

    def test_rejects_missing_lifecycle_or_accepted_parent_bindings_before_constructing_command(self):
        fixture = json.loads((ROOT / "tests" / "fixtures" / "s0" / "contract-open-group.json").read_text())
        del fixture["closed_bootstrap"]["bundle_ref"]
        with self.assertRaisesRegex(SectionContractError, "bundle"):
            SectionControlPlaneV1().compile_open_group(fixture)

    def test_rejects_changed_digest_bound_lifecycle_and_parent_references(self):
        baseline = json.loads((ROOT / "tests" / "fixtures" / "s0" / "contract-open-group.json").read_text())
        for key in ("plan_digest", "catalog_digest"):
            with self.subTest(key=key):
                fixture = json.loads(json.dumps(baseline))
                fixture["closed_bootstrap"][key] = "sha256:" + "9" * 64
                with self.assertRaises(SectionContractError):
                    SectionControlPlaneV1().compile_open_group(fixture)
        for key in ("checkpoint_ref", "bundle_ref"):
            with self.subTest(key=key):
                fixture = json.loads(json.dumps(baseline))
                fixture["closed_bootstrap"][key]["digest"] = "not-a-digest"
                with self.assertRaises(SectionContractError):
                    SectionControlPlaneV1().compile_open_group(fixture)
        fixture = json.loads(json.dumps(baseline))
        fixture["closed_bootstrap"]["lifecycle_prerequisite"]["kind"] = "closed-bootstrap"
        with self.assertRaises(SectionContractError):
            SectionControlPlaneV1().compile_open_group(fixture)

    def test_schema_rejects_protected_nested_command_mutations(self):
        fixture = json.loads((ROOT / "tests" / "fixtures" / "s0" / "contract-closed-bootstrap.json").read_text())
        compiled = SectionControlPlaneV1().compile_registry(fixture)
        schema = json.loads((ROOT / "schemas" / "section-control-plane-v1.schema.json").read_text())
        mutations = [
            ("expected_head", {"revision": 7}),
            ("actor", {"role": "orchestrator"}),
            ("authority_ref", {"approved": True}),
            ("input_refs", [{"digest": "sha256:" + "a" * 64}]),
            ("payload", {"section": {"id": "S1"}}),
        ]
        for key, value in mutations:
            with self.subTest(key=key):
                document = json.loads(json.dumps(compiled))
                document["open_section"][key] = value
                with self.assertRaises(SchemaValidationError):
                    validate_document(document, schema)

    def test_plan_schedule_and_status_schemas_reject_nested_mutations(self):
        fixture = json.loads((ROOT / "tests" / "fixtures" / "s0" / "contract-closed-bootstrap.json").read_text())
        compiled = SectionControlPlaneV1().compile_registry(fixture)
        schemas = {name: json.loads((ROOT / "schemas" / file).read_text()) for name, file in {
            "plan": "section-plan-v1.schema.json", "schedule": "section-schedule-v1.schema.json", "status": "section-status-v1.schema.json"}.items()}
        bad_plan = json.loads(json.dumps(fixture))
        bad_plan["authority"]["expected_head"] = {"revision": 7}
        bad_schedule = json.loads(json.dumps(compiled["schedule"]))
        bad_schedule["groups"][0]["first_frontier"] = []
        bad_status = json.loads(json.dumps(compiled["status"]))
        bad_status["transition_evidence"] = {"state": "pending", "receipt_digest": "sha256:" + "1" * 64}
        for document, schema in ((bad_plan, schemas["plan"]), (bad_schedule, schemas["schedule"])):
            with self.assertRaises(SchemaValidationError):
                validate_document(document, schema)
        with self.assertRaises(SchemaValidationError):
            validate_section_status(bad_status)

    def test_future_true_status_requires_accepted_receipt_and_head_evidence(self):
        fixture = json.loads((ROOT / "tests" / "fixtures" / "s0" / "contract-closed-bootstrap.json").read_text())
        compiled = SectionControlPlaneV1().compile_registry(fixture)
        accepted = json.loads(json.dumps(compiled["status"]))
        accepted["claims"]["source_transition_fixture_passed"] = True
        accepted["transition_evidence"] = {"state": "accepted", "receipt_digest": "sha256:" + "1" * 64, "head": fixture["expected_head"]}
        schema = json.loads((ROOT / "schemas" / "section-status-v1.schema.json").read_text())
        validate_document(accepted, schema)
        validate_section_status(accepted)
        del accepted["transition_evidence"]["receipt_digest"]
        with self.assertRaises(SchemaValidationError):
            validate_section_status(accepted)

    def test_accepted_status_rejects_evidence_head_mismatch_at_standalone_and_public_seams(self):
        baseline = KernelDouble._status_document()
        baseline["claims"]["source_transition_fixture_passed"] = True
        baseline["transition_evidence"] = {
            "state": "accepted",
            "receipt_digest": "sha256:" + "1" * 64,
            "head": {
                "revision": baseline["head"]["revision"],
                "transaction_digest": baseline["head"]["transaction_digest"],
            },
        }
        expected_head = {
            "revision": baseline["head"]["revision"],
            "transaction_digest": baseline["head"]["transaction_digest"],
        }

        class MismatchedEvidenceKernel(KernelDouble):
            def __init__(self, document):
                super().__init__()
                self.document = document

            def status(self, **kwargs):
                self.status_calls.append(kwargs)
                return json.loads(json.dumps(self.document))

            def resume(self, **kwargs):
                self.resume_calls.append(kwargs)
                return json.loads(json.dumps(self.document))

        mismatches = {
            "revision": baseline["head"]["revision"] - 1,
            "transaction_digest": "sha256:" + "2" * 64,
        }
        for field, value in mismatches.items():
            document = json.loads(json.dumps(baseline))
            document["transition_evidence"]["head"][field] = value
            with self.subTest(seam="standalone", field=field):
                with self.assertRaisesRegex(SchemaValidationError, "transition_evidence.head"):
                    validate_section_status(document)
            for seam in (status, resume):
                with self.subTest(seam=seam.__name__, field=field):
                    kernel = MismatchedEvidenceKernel(document)
                    with self.assertRaisesRegex(SchemaValidationError, "transition_evidence.head"):
                        seam(kernel, expected_head)
                    self.assertEqual(kernel.applied, [])

    def test_public_status_and_resume_reject_returned_head_mismatch_with_caller_expected_head(self):
        returned_head = KernelDouble._status_document()["head"]
        expected_head = {
            "revision": returned_head["revision"],
            "transaction_digest": returned_head["transaction_digest"],
        }
        mismatches = {
            "revision": expected_head["revision"] - 1,
            "transaction_digest": "sha256:" + "2" * 64,
        }

        for field, value in mismatches.items():
            caller_head = json.loads(json.dumps(expected_head))
            caller_head[field] = value
            for seam in (status, resume):
                with self.subTest(seam=seam.__name__, field=field):
                    kernel = KernelDouble()
                    with self.assertRaisesRegex(SchemaValidationError, r"\$\.head"):
                        seam(kernel, caller_head)
                    self.assertEqual(kernel.applied, [])

    def test_public_adapter_rejects_tampered_cross_field_command_before_apply(self):
        fixture = json.loads((ROOT / "tests" / "fixtures" / "s0" / "contract-closed-bootstrap.json").read_text())
        command = SectionControlPlaneV1().compile_registry(fixture)["open_section"]
        command["input_refs"][3]["kind"] = "catalog"
        kernel = KernelDouble()
        with self.assertRaises(SchemaValidationError):
            open_section(kernel, command)
        self.assertEqual(kernel.applied, [])

    def test_rejects_catalog_digest_and_bootstrap_identity_tampering(self):
        fixture = json.loads((ROOT / "tests" / "fixtures" / "s0" / "contract-closed-bootstrap.json").read_text())
        cases = []
        bad_mapping = json.loads(json.dumps(fixture))
        bad_mapping["catalog"]["tasks"][0]["epoch_id"] = "S0-E9"
        cases.append(bad_mapping)
        bad_group = json.loads(json.dumps(fixture))
        bad_group["closed_bootstrap"]["group_id"] = "Other"
        bad_group["closed_bootstrap"]["closed_group_receipt"]["group_id"] = "Other"
        cases.append(bad_group)
        bad_next = json.loads(json.dumps(fixture))
        bad_next["closed_bootstrap"]["next_group"] = "Bootstrap"
        cases.append(bad_next)
        for document in cases:
            with self.assertRaises(SectionContractError):
                SectionControlPlaneV1().compile_registry(document)

    def test_rejects_later_parent_and_receipt_identity_tampering(self):
        fixture = json.loads((ROOT / "tests" / "fixtures" / "s0" / "contract-open-group.json").read_text())
        fixture["closed_bootstrap"]["lifecycle_prerequisite"]["section_id"] = "S9"
        with self.assertRaises(SectionContractError):
            SectionControlPlaneV1().compile_open_group(fixture)
        fixture = json.loads((ROOT / "tests" / "fixtures" / "s0" / "contract-open-group.json").read_text())
        fixture["closed_bootstrap"]["closed_group_receipt"]["group_id"] = "Other"
        with self.assertRaises(SectionContractError):
            SectionControlPlaneV1().compile_open_group(fixture)

    def test_rejects_an_unknown_group_dependency_without_constructing_a_command(self):
        fixture = json.loads(
            (ROOT / "tests" / "fixtures" / "s0" / "contract-closed-bootstrap.json").read_text()
        )
        fixture["groups"][0]["depends_on"] = ["missing-group"]

        with self.assertRaisesRegex(ValueError, "unknown group dependency"):
            SectionControlPlaneV1().compile_registry(fixture)

    def test_compiled_public_documents_validate_against_their_versioned_schemas(self):
        fixture = json.loads(
            (ROOT / "tests" / "fixtures" / "s0" / "contract-closed-bootstrap.json").read_text()
        )
        compiled = SectionControlPlaneV1().compile_registry(fixture)
        schemas = {
            name: json.loads((ROOT / "schemas" / filename).read_text())
            for name, filename in {
                "control": "section-control-plane-v1.schema.json",
                "plan": "section-plan-v1.schema.json",
                "status": "section-status-v1.schema.json",
                "schedule": "section-schedule-v1.schema.json",
            }.items()
        }

        validate_document(fixture, schemas["plan"])
        validate_document(compiled, schemas["control"])
        validate_document(compiled["schedule"], schemas["schedule"])
        validate_document(compiled["status"], schemas["status"])

    def test_constructs_a_later_group_as_one_atomic_command(self):
        fixture = json.loads(
            (ROOT / "tests" / "fixtures" / "s0" / "contract-open-group.json").read_text()
        )

        command = SectionControlPlaneV1().compile_open_group(fixture)

        self.assertEqual(command["schema"], "dag-command/v1")
        self.assertEqual(command["command_type"], "open_group")
        self.assertEqual(command["payload"]["group"]["id"], "Planning")
        self.assertEqual(command["payload"]["first_frontier"], ["S0.D1"])

    def test_rejects_a_cycle_in_the_declared_group_schedule(self):
        fixture = json.loads(
            (ROOT / "tests" / "fixtures" / "s0" / "contract-closed-bootstrap.json").read_text()
        )
        fixture["groups"].append(
            {"id": "Review", "depends_on": ["contracts-and-schema"], "first_epoch": "S0-E2", "first_frontier": ["S0.C1"]}
        )
        fixture["groups"][0]["depends_on"] = ["Review"]

        with self.assertRaisesRegex(ValueError, "cycle"):
            SectionControlPlaneV1().compile_registry(fixture)

    def test_candidate_generic_cannot_request_a_pilot_scope(self):
        fixture = json.loads(
            (ROOT / "tests" / "fixtures" / "s0" / "contract-closed-bootstrap.json").read_text()
        )
        fixture["authority"]["scopes"] = ["pilot", "open_section", "open_group"]

        with self.assertRaisesRegex(ValueError, "authority scopes"):
            SectionControlPlaneV1().compile_registry(fixture)

    def test_status_schema_requires_nonempty_history_for_s1_but_only_empty_history_for_s0(self):
        fixture = json.loads((ROOT / "tests" / "fixtures" / "s1" / "contract-accepted-s0.json").read_text())
        status_schema = json.loads((ROOT / "schemas" / "section-status-v1.schema.json").read_text())
        s1_status = SectionControlPlaneV1().compile_registry(fixture)["status"]
        missing = json.loads(json.dumps(s1_status))
        del missing["section_history"]
        empty = json.loads(json.dumps(s1_status))
        empty["section_history"] = []
        for document in (missing, empty):
            with self.subTest(document=document):
                with self.assertRaises(SchemaValidationError):
                    validate_document(document, status_schema)

        s0_fixture = json.loads((ROOT / "tests" / "fixtures" / "s0" / "contract-closed-bootstrap.json").read_text())
        s0_status = SectionControlPlaneV1().compile_registry(s0_fixture)["status"]
        validate_document(s0_status, status_schema)
        s0_status["section_history"] = [{}]
        with self.assertRaises(SchemaValidationError):
            validate_document(s0_status, status_schema)

    def test_compiler_rejects_the_schema_invalid_dual_predecessor_plan_before_constructing_a_command(self):
        fixture = json.loads((ROOT / "tests" / "fixtures" / "s1" / "contract-accepted-s0.json").read_text())
        fixture["closed_bootstrap"] = json.loads(
            (ROOT / "tests" / "fixtures" / "s0" / "contract-closed-bootstrap.json").read_text()
        )["closed_bootstrap"]

        with self.assertRaises(SectionContractError):
            SectionControlPlaneV1().compile_registry(fixture)

    def test_plan_and_status_schemas_reject_malformed_accepted_and_history_references(self):
        plan_fixture = json.loads((ROOT / "tests" / "fixtures" / "s1" / "contract-accepted-s0.json").read_text())
        plan_schema = json.loads((ROOT / "schemas" / "section-plan-v1.schema.json").read_text())
        status_schema = json.loads((ROOT / "schemas" / "section-status-v1.schema.json").read_text())
        status_document = SectionControlPlaneV1().compile_registry(plan_fixture)["status"]

        for mutation in (
            lambda value: value.pop("digest"),
            lambda value: value.update({"unexpected": True}),
            lambda value: value.update({"object_type": "checkpoint"}),
            lambda value: value.update({"path": "objects/not-a-digest.json"}),
        ):
            with self.subTest(schema="plan", mutation=mutation):
                document = json.loads(json.dumps(plan_fixture))
                mutation(document["accepted_section"]["accepted_section_receipt"])
                with self.assertRaises(SchemaValidationError):
                    validate_document(document, plan_schema)
            with self.subTest(schema="status", mutation=mutation):
                document = json.loads(json.dumps(status_document))
                mutation(document["section_history"][0]["accepted_receipt_ref"])
                with self.assertRaises(SchemaValidationError):
                    validate_document(document, status_schema)

    def test_public_open_adapter_rejects_schema_invalid_payload_and_preconditions_before_apply_for_s0_and_s1(self):
        fixtures = {
            "S0": ROOT / "tests" / "fixtures" / "s0" / "contract-closed-bootstrap.json",
            "S1": ROOT / "tests" / "fixtures" / "s1" / "contract-accepted-s0.json",
        }
        for section, path in fixtures.items():
            for nested_key in ("payload", "preconditions"):
                with self.subTest(section=section, nested_key=nested_key):
                    command = SectionControlPlaneV1().compile_registry(json.loads(path.read_text()))["open_section"]
                    target = command["payload"] if nested_key == "payload" else command["payload"]["preconditions"]
                    target["unexpected"] = True
                    kernel = KernelDouble()
                    with self.assertRaises(SchemaValidationError):
                        open_section(kernel, command)
                    self.assertEqual(kernel.applied, [])

    def test_public_status_and_resume_reject_non_versioned_or_extra_status_fields(self):
        class InvalidStatusKernel(KernelDouble):
            def status(self, **kwargs):
                result = super().status(**kwargs)
                result["schema"] = "wrong/v1"
                result["unexpected"] = True
                return result

            def resume(self, **kwargs):
                result = super().resume(**kwargs)
                result["claims"]["unexpected"] = True
                return result

        kernel = InvalidStatusKernel()
        head = {"revision": 7, "transaction_digest": "sha256:" + "c" * 64}
        with self.assertRaises(SchemaValidationError):
            status(kernel, head)
        with self.assertRaises(SchemaValidationError):
            resume(kernel, head)
        self.assertEqual(len(kernel.status_calls), 1)
        self.assertEqual(len(kernel.resume_calls), 1)
