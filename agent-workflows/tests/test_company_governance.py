from __future__ import annotations

import ast
import copy
import hashlib
import importlib.util
import json
import tempfile
import unittest
from pathlib import Path

from ai_agent_workflow.company_governance import CompanyGovernanceV1


ROOT = Path(__file__).resolve().parents[2]
HEAD = {"revision": 73, "transaction_digest": "sha256:" + "a" * 64}
COMPANY = "acme-test"
SELECTORS = CompanyGovernanceV1.selectors()
SCRIPTS = {
    "surface.company.classify-data": "classify-data.py",
    "surface.company.resolve-identity": "resolve-identity.py",
    "surface.company.authorize-tools": "authorize-tools.py",
    "surface.company.approve-catalog": "approve-catalog.py",
    "surface.company.evaluate-change": "evaluate-change.py",
    "surface.company.release-workflow": "release-workflow.py",
    "surface.company.audit-operation": "audit-operation.py",
}
CATALOG_ENTRY = {
    "name": "approved-skill",
    "source": "https://source.example.test/approved-skill",
    "version": "1.2.3",
    "digest": "sha256:" + "b" * 64,
    "license": "Apache-2.0",
    "review": {"status": "passed", "reviewer_type": "human", "reviewer_id": "reviewer-1"},
    "owner": "platform-team",
}


def canonical_digest(value):
    raw = json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=True).encode()
    return "sha256:" + hashlib.sha256(raw).hexdigest()


class CompanyGovernanceTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.reference_root = Path(self.temp.name)
        self.owner_ref = self.write_evidence("owner.json", {"schema": "company-owner/v1", "company_id": COMPANY, "owner_id": "owner-1"})
        self.authority_ref = self.write_evidence("authority.json", {"schema": "company-authority/v1", "company_id": COMPANY, "authority_id": "authority-1"})
        self.policy_ref = self.write_evidence(
            "effective-policy.json",
            {
                "schema": "company-effective-tool-policy/v1",
                "company_id": COMPANY,
                "identity": {"type": "agent", "id": "agent-1"},
                "scopes": {
                    "filesystem": ["company/acme-test/repo:read", "company/acme-test/repo:write"],
                    "network": ["packages.example.test:read"],
                    "external_actions": ["catalog:read"],
                    "production": [],
                },
                "readback_status": "effective",
                "source": "fixture",
            },
        )
        self.generic_ref = self.write_evidence("generic.json", {"schema": "company-evidence/v1", "company_id": COMPANY, "status": "passed"})
        self.rollback_ref = self.write_evidence("rollback.json", {"schema": "company-rollback-plan/v1", "company_id": COMPANY, "status": "verified"})
        self.audit_plan_ref = self.write_evidence(
            "audit-operation-plan.json",
            {
                "schema": "company-audit-operation-plan/v1",
                "company_id": COMPANY,
                "operation_id": "operation-1",
                "release_binding": {"release_id": "release-1", "artifact_digest": canonical_digest(CATALOG_ENTRY)},
                "expected_events": [
                    {"event_id": "request", "event_type": "request"},
                    {"event_id": "decision", "event_type": "decision"},
                    {"event_id": "incident", "event_type": "incident"},
                ],
                "retention_days": 365,
            },
        )
        self.authority = {
            "authority_ref": self.authority_ref,
            "company_id": COMPANY,
            "namespace": "company/acme-test/governance",
            "store_namespace": "company-store/acme-test",
            "scope": "fixture-only",
            "owner_ref": self.owner_ref,
            "policy_ref": self.policy_ref,
            "audit_plan_ref": self.audit_plan_ref,
        }
        self.compiler = CompanyGovernanceV1(source_root=ROOT, reference_root=self.reference_root)

    def tearDown(self):
        self.temp.cleanup()

    def write(self, relative, document):
        path = self.reference_root / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        raw = (json.dumps(document, indent=2, sort_keys=True) + "\n").encode()
        path.write_bytes(raw)
        return {"path": relative, "version": document["schema"], "digest": "sha256:" + hashlib.sha256(raw).hexdigest()}

    def write_evidence(self, name, document, *, company=COMPANY):
        return self.write("company-evidence/%s/%s" % (company, name), document)

    def write_artifact(self, name, document, *, company=COMPANY):
        return self.write("company-artifacts/%s/%s" % (company, name), document)

    def read(self, ref):
        return json.loads((self.reference_root / ref["path"]).read_text(encoding="utf-8"))

    @staticmethod
    def metrics(*, unavailable=None, target_unit=None):
        units = {"quality": "score", "cost": "usd", "latency": "ms", "failure": "percent", "rollback": "minutes"}
        baseline_values = {"quality": 80, "cost": 10, "latency": 120, "failure": 2, "rollback": 15}
        target_values = {"quality": 85, "cost": 9, "latency": 100, "failure": 1, "rollback": 10}
        baseline = {}
        target = {}
        for dimension in units:
            baseline[dimension] = {"status": "available", "value": baseline_values[dimension], "unit": units[dimension], "method": "fixed-%s-fixture" % dimension}
            target[dimension] = {"status": "available", "value": target_values[dimension], "unit": units[dimension], "method": "fixed-%s-fixture" % dimension}
        if unavailable:
            target[unavailable] = {"status": "unavailable", "value": None, "unit": units[unavailable], "method": "fixed-%s-fixture" % unavailable}
        if target_unit:
            target["latency"]["unit"] = target_unit
        return baseline, target

    def inputs(self, selector, refs, *, override=None):
        approval = None
        evidence = [self.generic_ref]
        if selector == "surface.company.classify-data":
            payload = {
                "dataset_id": "dataset-1",
                "data_class": "confidential",
                "retention": {"policy_id": "retention-365", "duration_days": 365, "disposition": "delete"},
                "region": {"allowed_regions": ["jp-east"], "selected_region": "jp-east", "policy_status": "satisfied"},
                "scan": {"status": "complete", "secret_finding_count": 1, "pii_finding_count": 1, "secret_categories": ["api-key"], "pii_categories": ["email"], "raw_values_included": False},
            }
        elif selector == "surface.company.resolve-identity":
            payload = {
                "identity": {"type": "agent", "id": "agent-1"},
                "delegation": {
                    "delegator": {"type": "human", "id": "human-1"},
                    "delegatee": {"type": "agent", "id": "agent-1"},
                    "scopes": {
                        "filesystem": ["company/acme-test/repo:read"],
                        "network": ["packages.example.test:read"],
                        "external_actions": ["catalog:read"],
                        "production": [],
                    },
                    "issued_at": "2026-09-05T00:00:00Z",
                    "expires_at": "2026-09-06T00:00:00Z",
                    "status": "active",
                },
                "evaluated_at": "2026-09-05T12:00:00Z",
            }
        elif selector == "surface.company.authorize-tools":
            payload = {
                "requested_scopes": {
                    "filesystem": ["company/acme-test/repo:read"],
                    "network": ["packages.example.test:read"],
                    "external_actions": ["catalog:read"],
                    "production": [],
                },
                "effective_policy_ref": self.policy_ref,
                "least_privilege": True,
            }
            evidence = [self.policy_ref]
        elif selector == "surface.company.approve-catalog":
            entry = copy.deepcopy(CATALOG_ENTRY)
            payload = {"entry": entry, "evaluated_at": "2026-09-05T12:00:00Z"}
            approval = self.write_evidence(
                "catalog-approval.json",
                {
                    "schema": "company-catalog-approval/v1", "company_id": COMPANY,
                    "actor": {"type": "enterprise", "id": "approval-service"}, "scope": "catalog-entry", "decision": "approved",
                    "entry_digest": canonical_digest(entry), "authority_digest": canonical_digest(self.authority), "expected_head": HEAD,
                    "issued_at": "2026-09-05T00:00:00Z", "expires_at": "2026-09-06T00:00:00Z",
                },
            )
        elif selector == "surface.company.evaluate-change":
            baseline, target = self.metrics()
            payload = {
                "evaluation_id": "eval-1",
                "catalog_entry_digest": self.read(refs[3])["payload"]["entry_digest"],
                "baseline": baseline,
                "target": target,
            }
        elif selector == "surface.company.release-workflow":
            artifact_digest = self.read(refs[4])["payload"]["catalog_entry_digest"]
            rollout = [
                {"name": "canary", "percentage": 5, "entry_criteria": ["approval valid"], "exit_criteria": ["guardrails pass"]},
                {"name": "staged", "percentage": 50, "entry_criteria": ["canary passes"], "exit_criteria": ["guardrails pass"]},
                {"name": "general", "percentage": 100, "entry_criteria": ["staged passes"], "exit_criteria": ["communication complete"]},
            ]
            canary = {"enabled": True, "cohort": "internal-testers", "percentage": 5, "success_criteria": ["no severe failures"], "observation_minutes": 60}
            rollback = {"available": True, "plan_ref": self.rollback_ref, "trigger_conditions": ["failure regression"], "restoration_target": "previous-digest"}
            communication = {"audiences": ["operators", "users"], "channel": "fixture-channel", "message_template": "release-template-v1", "owner": "release-owner"}
            payload = {
                "release_id": "release-1", "artifact_digest": artifact_digest, "rollout_stages": rollout,
                "canary": canary, "rollback": rollback, "communication": communication, "evaluated_at": "2026-09-05T12:00:00Z",
            }
            plan_digest = canonical_digest({"rollout_stages": rollout, "canary": canary, "rollback": rollback, "communication": communication})
            approval = self.write_evidence(
                "release-approval.json",
                {
                    "schema": "company-release-approval/v1", "company_id": COMPANY,
                    "actor": {"type": "human", "id": "release-approver"}, "scope": "release-candidate", "decision": "approved",
                    "release_id": "release-1", "artifact_digest": artifact_digest, "plan_digest": plan_digest,
                    "authority_digest": canonical_digest(self.authority), "expected_head": HEAD,
                    "issued_at": "2026-09-05T00:00:00Z", "expires_at": "2026-09-06T00:00:00Z",
                },
            )
            evidence = [self.rollback_ref]
        elif selector == "surface.company.audit-operation":
            event_refs = [
                self.write_evidence("event-request.json", {"schema": "company-audit-event/v1", "company_id": COMPANY, "event_id": "request"}),
                self.write_evidence("event-decision.json", {"schema": "company-audit-event/v1", "company_id": COMPANY, "event_id": "decision"}),
                self.write_evidence("event-incident.json", {"schema": "company-audit-event/v1", "company_id": COMPANY, "event_id": "incident"}),
            ]
            payload = {
                "operation_id": "operation-1", "operation_plan_ref": self.audit_plan_ref,
                "expected_event_ids": ["request", "decision", "incident"],
                "events": [
                    {"event_id": "request", "event_type": "request", "record_ref": event_refs[0], "retained": True},
                    {"event_id": "decision", "event_type": "decision", "record_ref": event_refs[1], "retained": True},
                    {"event_id": "incident", "event_type": "incident", "record_ref": event_refs[2], "retained": True},
                ],
                "unreadable_event_ids": [], "bypass_event_ids": [], "incident_event_ids": ["incident"],
                "retention": {"required_days": 365, "actual_days": 365, "status": "retained"},
            }
            evidence = [self.audit_plan_ref, *event_refs]
        else:
            raise AssertionError(selector)
        if override:
            override(payload)
        return {"predecessor_artifact_refs": refs, "evidence_refs": evidence, "payload": payload, "approval_receipt_ref": approval}

    def compile_chain(self, *, through=None):
        refs, candidates, inputs = [], [], {}
        for selector in SELECTORS:
            values = self.inputs(selector, refs)
            candidate = self.compiler.compile(selector, values, self.authority, HEAD)
            self.assertEqual("company-governance-candidate/v1", candidate["schema"], candidate)
            inputs[selector] = values
            refs.append(self.write_artifact("%s.json" % selector.rsplit(".", 1)[-1], candidate))
            candidates.append(candidate)
            if selector == through:
                break
        return refs, candidates, inputs

    def test_all_seven_compile_in_exact_order_as_static_non_authorizing_candidates(self):
        refs, candidates, _ = self.compile_chain()
        self.assertEqual(7, len(refs))
        self.assertEqual(list(SELECTORS), [item["surface_id"] for item in candidates])
        self.assertEqual(7, len({item["result_kind"] for item in candidates}))
        for candidate in candidates:
            self.assertTrue(candidate["non_mutating"] and candidate["source_only"])
            self.assertFalse(candidate["uses_personal_state"] or candidate["grants_approval"] or candidate["performs_external_lookup"] or candidate["performs_release"] or candidate["reports_live_audit"])
            self.assertEqual("company-store/acme-test", candidate["company_store_namespace"])
        self.assertEqual("not-decided", candidates[4]["payload"]["adoption_status"])
        self.assertEqual("not-executed", candidates[5]["payload"]["release_status"])
        self.assertEqual("fixture-only", candidates[6]["payload"]["audit_source"])

    def test_thin_scripts_bind_each_exact_selector_without_business_logic(self):
        refs = []
        for selector, name in SCRIPTS.items():
            path = ROOT / "agent-workflows/company" / name
            tree = ast.parse(path.read_text(encoding="utf-8"))
            assignments = [node for node in tree.body if isinstance(node, ast.Assign) and any(isinstance(target, ast.Name) and target.id == "SURFACE_ID" for target in node.targets)]
            self.assertEqual(selector, assignments[0].value.value)
            self.assertLessEqual(len(path.read_text(encoding="utf-8").splitlines()), 30)
            spec = importlib.util.spec_from_file_location("company_entry_" + name.replace("-", "_"), path)
            module = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(module)
            self.assertEqual(selector, module.SURFACE_ID)
            request = {"inputs": self.inputs(selector, refs), "authority": self.authority, "expected_head": HEAD}
            candidate = module.compile_candidate(request, source_root=ROOT, reference_root=self.reference_root)
            self.assertEqual(selector, candidate["surface_id"], candidate)
            refs.append(self.write_artifact("entry-%s.json" % name.removesuffix(".py"), candidate))

    def test_seven_source_evidence_files_bind_raw_script_digest_and_no_live_state(self):
        evidence_root = ROOT / "agent-workflows/evidence/surfaces/company"
        self.assertEqual({name.replace(".py", ".json") for name in SCRIPTS.values()}, {path.name for path in evidence_root.glob("*.json")})
        for selector, name in SCRIPTS.items():
            evidence = json.loads((evidence_root / name.replace(".py", ".json")).read_text(encoding="utf-8"))
            source = ROOT / evidence["canonical_source"]["path"]
            actual = "sha256:" + hashlib.sha256(source.read_bytes()).hexdigest()
            self.assertEqual("company-governance-surface-evidence/v1", evidence["schema"])
            self.assertEqual(selector, evidence["surface_id"])
            self.assertEqual(selector, evidence["acceptance"]["selector"])
            self.assertEqual("passed", evidence["acceptance"]["result"])
            self.assertEqual(actual, evidence["canonical_source"]["digest"])
            boundary = evidence["claim_boundary"]
            self.assertTrue(boundary["source_fixture_only"])
            for key in ("personal_state_used", "credentials_used", "live_company_system_used", "live_identity_used", "live_policy_used", "live_catalog_used", "live_evaluation_used", "live_release_used", "live_audit_used", "approval_minted"):
                self.assertFalse(boundary[key], (selector, key))

    def test_raw_secret_cross_company_and_malformed_context_refuse(self):
        values = self.inputs(SELECTORS[0], [])
        values["payload"]["password"] = "do-not-copy"
        secret_refusal = self.compiler.compile(SELECTORS[0], values, self.authority, HEAD)
        self.assertIn("blocked_raw_secret_input", secret_refusal["reason"])
        self.assertNotIn("do-not-copy", json.dumps(secret_refusal))
        cross = self.write("personal/acme-test/evidence.json", {"schema": "company-evidence/v1", "company_id": COMPANY})
        values = self.inputs(SELECTORS[0], []); values["evidence_refs"] = [cross]
        cross_refusal = self.compiler.compile(SELECTORS[0], values, self.authority, HEAD)
        self.assertIn("blocked_cross_company_or_personal_path", cross_refusal["reason"])
        self.assertNotIn(cross["path"], json.dumps(cross_refusal))
        self.assertIn("blocked_malformed_head", self.compiler.compile(SELECTORS[0], self.inputs(SELECTORS[0], []), self.authority, {"revision": "73"})["reason"])
        malformed = dict(self.authority, namespace="personal/acme-test")
        self.assertIn("blocked_company_namespace", self.compiler.compile(SELECTORS[0], self.inputs(SELECTORS[0], []), malformed, HEAD)["reason"])

    def test_expired_or_agent_self_delegation_refuses(self):
        refs, _, _ = self.compile_chain(through=SELECTORS[0])
        expired = self.inputs(SELECTORS[1], refs); expired["payload"]["evaluated_at"] = "2026-09-07T00:00:00Z"
        self.assertIn("blocked_expired_delegation", self.compiler.compile(SELECTORS[1], expired, self.authority, HEAD)["reason"])
        self_delegated = self.inputs(SELECTORS[1], refs); self_delegated["payload"]["delegation"]["delegator"] = {"type": "agent", "id": "agent-1"}
        self.assertIn("blocked_agent_self_delegation", self.compiler.compile(SELECTORS[1], self_delegated, self.authority, HEAD)["reason"])

    def test_tools_must_be_within_effective_policy_and_company_path(self):
        refs, _, _ = self.compile_chain(through=SELECTORS[1])
        broad = self.inputs(SELECTORS[2], refs); broad["payload"]["requested_scopes"]["production"] = ["deploy:write"]
        self.assertIn("blocked_tool_scope_broader_than_policy:production", self.compiler.compile(SELECTORS[2], broad, self.authority, HEAD)["reason"])
        personal = self.inputs(SELECTORS[2], refs); personal["payload"]["requested_scopes"]["filesystem"] = ["/Users/person/.config"]
        self.assertIn("blocked_tool_scope_broader_than_policy:filesystem", self.compiler.compile(SELECTORS[2], personal, self.authority, HEAD)["reason"])
        undelegated = self.inputs(SELECTORS[2], refs); undelegated["payload"]["requested_scopes"]["filesystem"] = ["company/acme-test/repo:write"]
        self.assertIn("blocked_tool_scope_broader_than_delegation:filesystem", self.compiler.compile(SELECTORS[2], undelegated, self.authority, HEAD)["reason"])

    def test_catalog_requires_known_license_review_and_external_approval(self):
        refs, _, _ = self.compile_chain(through=SELECTORS[2])
        unknown = self.inputs(SELECTORS[3], refs); unknown["payload"]["entry"]["license"] = "UNKNOWN"
        self.assertIn("blocked_unknown_catalog_license", self.compiler.compile(SELECTORS[3], unknown, self.authority, HEAD)["reason"])
        unapproved = self.inputs(SELECTORS[3], refs); unapproved["payload"]["entry"]["review"]["status"] = "pending"
        self.assertIn("blocked_unapproved_catalog_review", self.compiler.compile(SELECTORS[3], unapproved, self.authority, HEAD)["reason"])
        forged = self.inputs(SELECTORS[3], refs)
        approval = self.read(forged["approval_receipt_ref"]); approval["entry_digest"] = "sha256:" + "c" * 64
        forged["approval_receipt_ref"] = self.write_evidence("forged-catalog-approval.json", approval)
        self.assertIn("blocked_forged_catalog_approval", self.compiler.compile(SELECTORS[3], forged, self.authority, HEAD)["reason"])

    def test_evaluation_rejects_unavailable_or_incomparable_dimensions(self):
        refs, _, _ = self.compile_chain(through=SELECTORS[3])
        unavailable = self.inputs(SELECTORS[4], refs); unavailable["payload"]["target"]["cost"]["status"] = "unavailable"; unavailable["payload"]["target"]["cost"]["value"] = None
        self.assertIn("blocked_unavailable_evaluation_dimension:cost", self.compiler.compile(SELECTORS[4], unavailable, self.authority, HEAD)["reason"])
        incomparable = self.inputs(SELECTORS[4], refs); incomparable["payload"]["target"]["latency"]["unit"] = "seconds"
        self.assertIn("blocked_incomparable_evaluation:latency", self.compiler.compile(SELECTORS[4], incomparable, self.authority, HEAD)["reason"])

    def test_release_requires_canary_rollback_and_unforged_external_authority(self):
        refs, _, _ = self.compile_chain(through=SELECTORS[4])
        no_canary = self.inputs(SELECTORS[5], refs); no_canary["payload"]["canary"]["enabled"] = False
        self.assertIn("blocked_missing_canary", self.compiler.compile(SELECTORS[5], no_canary, self.authority, HEAD)["reason"])
        no_rollback = self.inputs(SELECTORS[5], refs); no_rollback["payload"]["rollback"]["available"] = False
        self.assertIn("blocked_missing_rollback", self.compiler.compile(SELECTORS[5], no_rollback, self.authority, HEAD)["reason"])
        forged = self.inputs(SELECTORS[5], refs)
        approval = self.read(forged["approval_receipt_ref"]); approval["release_id"] = "other-release"
        forged["approval_receipt_ref"] = self.write_evidence("forged-release-approval.json", approval)
        self.assertIn("blocked_forged_release_approval", self.compiler.compile(SELECTORS[5], forged, self.authority, HEAD)["reason"])

    def test_audit_refusal_preserves_missing_unreadable_bypass_and_unretained_ids(self):
        refs, _, _ = self.compile_chain(through=SELECTORS[5])
        values = self.inputs(SELECTORS[6], refs)
        values["payload"]["events"][1]["retained"] = False
        values["payload"]["events"] = values["payload"]["events"][:2]
        values["payload"]["unreadable_event_ids"] = ["incident"]
        values["payload"]["bypass_event_ids"] = ["decision"]
        refused = self.compiler.compile(SELECTORS[6], values, self.authority, HEAD)
        self.assertEqual("company-governance-refusal/v1", refused["schema"])
        self.assertEqual(["incident"], refused["details"]["missing_event_ids"])
        self.assertEqual(["incident"], refused["details"]["unreadable_event_ids"])
        self.assertEqual(["decision"], refused["details"]["bypassed_event_ids"])
        self.assertEqual(["decision"], refused["details"]["unretained_event_ids"])
        self.assertNotIn("complete", refused)

    def test_audit_expected_set_and_types_come_from_authority_bound_physical_plan(self):
        refs, _, _ = self.compile_chain(through=SELECTORS[5])
        subset = self.inputs(SELECTORS[6], refs)
        subset["payload"]["expected_event_ids"] = ["request", "decision"]
        subset["payload"]["events"] = subset["payload"]["events"][:2]
        subset["payload"]["incident_event_ids"] = []
        refused = self.compiler.compile(SELECTORS[6], subset, self.authority, HEAD)
        self.assertIn("blocked_audit_expected_set_mismatch", refused["reason"])
        self.assertEqual(["incident"], refused["details"]["missing_event_ids"])
        wrong_type = self.inputs(SELECTORS[6], refs)
        wrong_type["payload"]["events"][0]["event_type"] = "decision"
        self.assertIn("blocked_audit_event_type_mismatch", self.compiler.compile(SELECTORS[6], wrong_type, self.authority, HEAD)["reason"])
        wrong_operation = self.inputs(SELECTORS[6], refs)
        wrong_operation["payload"]["operation_id"] = "operation-2"
        self.assertIn("blocked_audit_operation_plan_mismatch", self.compiler.compile(SELECTORS[6], wrong_operation, self.authority, HEAD)["reason"])

    def test_stale_changed_path_and_semantically_forged_predecessor_refuse(self):
        refs, _, _ = self.compile_chain(through=SELECTORS[1])
        first = self.read(refs[0])
        first["payload"]["dataset_id"] = "changed"
        first["candidate_digest"] = canonical_digest({key: value for key, value in first.items() if key != "candidate_digest"})
        replacement = self.write_artifact("classify-data.json", first)
        stale_values = self.inputs(SELECTORS[2], refs)
        self.assertIn("blocked_digest_drift", self.compiler.compile(SELECTORS[2], stale_values, self.authority, HEAD)["reason"])
        forged = copy.deepcopy(first)
        forged["payload"]["scan"]["status"] = "partial"
        forged["candidate_digest"] = canonical_digest({key: value for key, value in forged.items() if key != "candidate_digest"})
        forged_ref = self.write_artifact("forged-classify-data.json", forged)
        second = self.read(refs[1]); second["predecessor_artifact_refs"] = [forged_ref]
        second["candidate_digest"] = canonical_digest({key: value for key, value in second.items() if key != "candidate_digest"})
        second_ref = self.write_artifact("forged-resolve-identity.json", second)
        values = self.inputs(SELECTORS[2], [forged_ref, second_ref])
        refusal = self.compiler.compile(SELECTORS[2], values, self.authority, HEAD)
        self.assertIn("blocked_persisted_candidate_semantics", refusal["reason"])
        self.assertNotEqual(refs[0]["digest"], replacement["digest"])


if __name__ == "__main__":
    unittest.main()
