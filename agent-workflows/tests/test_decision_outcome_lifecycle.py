from __future__ import annotations

import copy
import hashlib
import json
import tempfile
import unittest
from pathlib import Path

from ai_agent_workflow.decision_outcome_lifecycle import DecisionOutcomeLifecycleV1
from ai_agent_workflow.software_profiles import SoftwareProfileV1


ROOT = Path(__file__).resolve().parents[2]
HEAD = {"revision": 51, "transaction_digest": "sha256:" + "a" * 64}
G = tuple("group.G.G%d" % number for number in range(1, 7))
H = tuple("group.H.H%d" % number for number in range(1, 4))


def canonical_digest(value):
    raw = json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=True).encode()
    return "sha256:" + hashlib.sha256(raw).hexdigest()


class DecisionOutcomeLifecycleTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.reference_root = Path(self.temp.name)
        self.compiler = DecisionOutcomeLifecycleV1(source_root=ROOT, reference_root=self.reference_root)
        self.authority_ref = self.write("context/authority.json", {"schema": "decision-outcome-authority/v1", "authority_id": "authority-1"})
        self.owner_ref = self.write("context/owner.json", {"schema": "decision-outcome-owner/v1", "owner_id": "human-owner"})
        self.authority = {"authority_ref": self.authority_ref, "namespace": "decision-outcome-test", "scope": "fixture-only", "owner_ref": self.owner_ref}
        self.decision_ref = self.write("evidence/decision-source.json", {"schema": "decision-source/v1", "event_id": "event-1", "statement": "use exact replay"})
        self.generic_ref = self.write("evidence/generic.json", {"schema": "evidence/v1", "status": "passed"})
        self.objective_ref = self.write("evidence/objective.json", {"schema": "accepted-objective/v1", "status": "accepted", "objective_id": "objective-1"})
        self.outcome_ref = self.write("evidence/outcome.json", {"schema": "outcome-validation/v1", "status": "valid"})
        self.closure_ref = self.write("evidence/closure.json", {"schema": "closure-operation-result/v1", "status": "compiled", "selector": "group.F.F7"})
        self.profile_ref = self.make_profile_composition()

    def tearDown(self):
        self.temp.cleanup()

    def write(self, relative, document):
        path = self.reference_root / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        raw = (json.dumps(document, indent=2, sort_keys=True) + "\n").encode()
        path.write_bytes(raw)
        return {"path": relative, "version": document["schema"], "digest": "sha256:" + hashlib.sha256(raw).hexdigest()}

    def repo_ref(self, relative):
        raw = (ROOT / relative).read_bytes()
        document = json.loads(raw)
        return {"path": relative, "version": document["schema"], "digest": "sha256:" + hashlib.sha256(raw).hexdigest()}

    def profile_host_ref(self, selector):
        hosts = {"I1": "group.D.D2", "I2": "group.D.D2", "I3": "group.D.D2", "I4": "group.D.D6", "I5": "group.E.E3", "I6": "group.E.E9", "I7": "group.E.E9"}
        host = hosts[selector.rsplit(".", 1)[-1]]
        if host.startswith("group.D"):
            document = {"schema": "planning-system-artifact/v1", "qualified_id": host, "authority": self.authority, "expected_head": HEAD, "input_refs": [self.generic_ref], "output": {"kind": "accepted-test-host"}}
        else:
            document = {"schema": "execution-group-candidate/v1", "qualified_id": host, "loop_level": "artifact", "expected_head": HEAD, "authority_ref": {"authority_id": "authority-1", "lease_id": "lease-1", "epoch_id": "epoch-1"}, "non_mutating": True, "stage": host.rsplit(".", 1)[-1]}
            if host == "group.E.E3":
                document.update({"status": "submitted", "artifact_candidate": {}, "recovery": {}})
            else:
                document.update({"status": "verified", "new_findings_route": "E6"})
            document["candidate_digest"] = canonical_digest(document)
        return self.write("profile/hosts/%s.json" % selector.rsplit(".", 1)[-1], document)

    def make_profile_composition(self):
        compiler = SoftwareProfileV1(source_root=ROOT, reference_root=self.reference_root)
        selectors = tuple("profile.improvement.I%d" % number for number in range(1, 8))
        refs = []
        observation = {"availability": "available", "value": 10, "method": "request timer", "period": "one day", "environment": {"tier": "test"}, "variance": {"stdev": 1}}
        evidence_by_selector = {
            "profile.improvement.I1": {"metric": "latency", "availability": "unavailable", "value": None, "missingness": "not instrumented", "variance": {"status": "unavailable"}, "method": "request timer", "period": "one day", "environment": {"tier": "test"}},
            "profile.improvement.I2": {"change": "cache lookup", "mechanism": "avoid IO", "metric": "latency", "predicted_direction": "decrease", "falsification_condition": "median does not decrease"},
            "profile.improvement.I3": {"guardrails": [{"kind": kind, "metric": kind + " metric", "threshold": "no worse", "method": "same observation"} for kind in ("quality", "cost", "user-impact")]},
            "profile.improvement.I4": {"change_id": "cache-lookup", "approved_change": "add cache", "comparison_method": "same timer", "rollback": "remove cache"},
            "profile.improvement.I5": {"change_id": "cache-lookup", "scoped_paths": ["src/cache.py"]},
            "profile.improvement.I6": {"metric": "latency", "before": observation, "after": dict(observation, value=7)},
            "profile.improvement.I7": {"recommendation": "adopt", "rationale": "same-condition latency improved"},
        }
        for selector in selectors:
            evidence = copy.deepcopy(evidence_by_selector[selector])
            evidence_refs = [self.generic_ref]
            approval_ref = None
            if selector == "profile.improvement.I4":
                approval_ref = self.write("profile/approval-I4.json", {"schema": "human-profile-approval/v1", "profile": "improvement", "qualified_id": selector, "scope": "improvement-change", "actor": {"type": "human", "id": "user-1"}, "decision": "cache-lookup", "approved": True, "expected_head": HEAD})
            if selector == "profile.improvement.I5":
                action_ref = self.write("profile/action-I5.json", {"schema": "scoped-action-receipt/v1", "change_id": "cache-lookup", "scoped_paths": ["src/cache.py"], "status": "completed", "adoption_claimed": False})
                evidence["action_receipt_ref"] = action_ref
                evidence_refs.append(action_ref)
            values = {"profile": "improvement", "host_receipt_ref": self.profile_host_ref(selector), "predecessor_artifact_refs": refs, "evidence_refs": evidence_refs, "evidence": evidence, "human_approval_ref": approval_ref}
            candidate = compiler.compile(selector, values, self.authority, HEAD)
            self.assertEqual("software-profile-candidate/v1", candidate["schema"], candidate)
            refs.append(self.write("profile/artifacts/%s.json" % selector.rsplit(".", 1)[-1], candidate))
        composition = compiler.compose_profile("improvement", self.repo_ref("agent-workflows/workflows/improvement-measured.json"), refs, self.authority, HEAD)
        self.assertEqual("software-profile-composition/v1", composition["schema"], composition)
        return self.write("evidence/profile.json", composition)

    def inputs(self, selector, refs, *, outcome="achieved", objective_status="achieved", unverified=None, promotion_decision="adopt"):
        human = None
        if selector == "group.G.G1":
            evidence = [self.decision_ref]
            payload = {"run_id": "run-1", "period": "epoch-1", "groups": ["B", "C", "D", "E", "F"], "source_refs": [self.decision_ref], "searched_scopes": ["events", "checkpoints", "reports", "artifacts", "adrs"], "unreadable_scopes": [], "excluded_scopes": ["credentials"], "coverage_status": "complete"}
        elif selector == "group.G.G2":
            evidence = [self.decision_ref]
            payload = {"classifications": [{"candidate_ref": self.decision_ref, "status": "proposal", "evidence_refs": [self.decision_ref], "authority_ref": None, "approval_receipt_ref": None, "confidence": 0.9}]}
        elif selector == "group.G.G3":
            evidence = [self.decision_ref]
            payload = {"nodes": [{"candidate_ref": self.decision_ref, "disposition": "new", "chronology": 1}], "relationships": []}
        elif selector == "group.G.G4":
            evidence = [self.decision_ref]
            payload = {"records": [{"record_id": "decision-1", "graph_candidate_ref": self.decision_ref, "record_kind": "current-candidate", "context": "The lifecycle must survive context loss.", "decision": "Bind every persisted predecessor and replay its semantics.", "drivers": ["prevent forged history"], "options": ["digest only", "digest plus semantic replay"], "rationale_evidence_refs": [self.decision_ref], "rejected_alternatives": ["digest only"], "consequences": ["replay cost is bounded"], "owner": "human-owner", "revisit_trigger": "replay is too expensive", "source_refs": [self.decision_ref], "historical_rule_refs": []}]}
        elif selector == "group.G.G5":
            evidence = [self.generic_ref]
            action = {"record_id": "decision-1", "decision": promotion_decision, "target": {"path": "docs/decisions/decision-1.md", "version": "v1"}, "diff_digest": "sha256:" + "b" * 64}
            payload = {"candidate_ref": refs[3], "actions": [action]}
            event = {"schema": "decision-promotion-approval/v1", "actor": {"type": "human", "id": "user-1"}, "candidate_ref": refs[3], "actions": [action], "authority_digest": canonical_digest(self.authority), "expected_head": HEAD}
            human = self.write("events/promotion-%s.json" % promotion_decision, event)
        elif selector == "group.G.G6":
            evidence = [self.decision_ref, self.generic_ref]
            target = {"path": "docs/decisions/decision-1.md", "version": "v1"}
            payload = {"transactions": [{"record_id": "decision-1", "decision": "adopt", "source_event_refs": [self.decision_ref], "candidate_ref": refs[3], "approval_event_ref": self.read(refs[4])["human_event_ref"], "durable_target": target, "validation_refs": [self.generic_ref], "backlinks": [{"source_ref": self.decision_ref, "durable_target": target}], "supersedes_ref": None}], "write_requested": False, "source_events_immutable": True, "overwrite_existing": False}
        elif selector == "group.H.H1":
            statuses = {"delivery": "completed", "trajectory": "improved", "objective_state": objective_status}
            audit_refs = {dimension: self.write("evidence/audit-%s-%s.json" % (dimension, status), {"schema": "objective-audit-evidence/v1", "dimension": dimension, "status": status, "objective_ref": self.objective_ref}) for dimension, status in statuses.items()}
            evidence = [self.objective_ref, self.outcome_ref, self.closure_ref, self.profile_ref, *audit_refs.values(), self.g6_ref]
            payload = {"objective_ref": self.objective_ref, "outcome_system_ref": self.outcome_ref, "closure_ref": self.closure_ref, "profile_ref": self.profile_ref, "decision_promotion_ref": self.g6_ref, "delivery": {"status": statuses["delivery"], "evidence_refs": [audit_refs["delivery"]]}, "trajectory": {"status": statuses["trajectory"], "evidence_refs": [audit_refs["trajectory"]]}, "objective_state": {"status": statuses["objective_state"], "evidence_refs": [audit_refs["objective_state"]]}, "unverified_items": [] if unverified is None else unverified}
        elif selector == "group.H.H2":
            evidence = [self.generic_ref]
            payload = {"audit_ref": refs[0], "outcome": outcome, "reason": "human reviewed the objective audit", "remaining_tasks": []}
            event = {"schema": "human-run-outcome/v1", "actor": {"type": "human", "id": "user-1"}, **payload, "authority_digest": canonical_digest(self.authority), "expected_head": HEAD}
            human = self.write("events/run-outcome.json", event)
        elif selector == "group.H.H3":
            evidence = [self.generic_ref, self.decision_ref, self.g6_ref]
            payload = {"outcome_ref": refs[1], "mode": "archive", "retention": [{"artifact_ref": self.generic_ref, "retain": True, "rationale": "rollback evidence"}], "cleanup_candidates": [{"path": ".local/scratch", "reason": "candidate cleanup only", "requires_human_approval": True, "executed": False}], "decision_candidates": [self.decision_ref], "important_decisions": [{"candidate_ref": self.decision_ref, "promoted": True, "promotion_ref": self.g6_ref}], "rollback_artifact_refs": [self.generic_ref], "next_run_handoff_ref": None}
        else:
            raise AssertionError(selector)
        predecessors = refs[:H.index(selector)] if selector.startswith("group.H") else refs[:G.index(selector)]
        return {"predecessor_artifact_refs": predecessors, "evidence_refs": evidence, "payload": payload, "human_event_ref": human}

    def read(self, ref):
        return json.loads((self.reference_root / ref["path"]).read_text())

    def compile_chains(self, *, outcome="achieved", objective_status="achieved", unverified=None, promotion_decision="adopt"):
        g_refs, g_candidates, g_inputs = [], [], {}
        for selector in G:
            values = self.inputs(selector, g_refs, promotion_decision=promotion_decision)
            candidate = self.compiler.compile(selector, values, self.authority, HEAD)
            self.assertEqual("decision-outcome-candidate/v1", candidate["schema"], candidate)
            g_inputs[selector] = values
            g_refs.append(self.write("artifacts/%s.json" % selector.rsplit(".", 1)[-1], candidate))
            g_candidates.append(candidate)
        self.g6_ref = g_refs[-1]
        h_refs, h_candidates, h_inputs = [], [], {}
        for selector in H:
            context = g_refs if selector == "group.H.H1" else h_refs
            values = self.inputs(selector, context, outcome=outcome, objective_status=objective_status, unverified=unverified)
            candidate = self.compiler.compile(selector, values, self.authority, HEAD)
            self.assertEqual("decision-outcome-candidate/v1", candidate["schema"], candidate)
            h_inputs[selector] = values
            h_refs.append(self.write("artifacts/%s.json" % selector.rsplit(".", 1)[-1], candidate))
            h_candidates.append(candidate)
        return g_refs, h_refs, g_candidates + h_candidates, {**g_inputs, **h_inputs}

    def test_all_nine_selectors_compile_distinct_non_mutating_candidates(self):
        _, _, candidates, _ = self.compile_chains()
        self.assertEqual(9, len(candidates))
        self.assertEqual(9, len({item["result_kind"] for item in candidates}))
        self.assertTrue(all(not item["grants_approval"] and not item["performs_promotion"] and not item["performs_archive"] and not item["performs_cleanup"] and not item["objective_outcome_claimed"] for item in candidates))
        self.assertEqual({"objective_system", "outcome_system", "closure_protocol", "software_profiles"}, {key for key in candidates[0]["source_interfaces"] if key.endswith("system") or key in {"closure_protocol", "software_profiles"}})

    def test_every_selector_has_a_purpose_specific_typed_refusal(self):
        _, _, _, inputs = self.compile_chains()
        for selector in (*G, *H):
            broken = copy.deepcopy(inputs[selector])
            broken["payload"] = {}
            refused = self.compiler.compile(selector, broken, self.authority, HEAD)
            self.assertEqual("decision-outcome-refusal/v1", refused["schema"], selector)
            self.assertIn("blocked_incomplete_evidence:%s" % selector, refused["reason"], selector)

    def test_g1_partial_inventory_preserves_unreadable_scope_and_blocks_g2(self):
        values = self.inputs("group.G.G1", [])
        values["payload"]["unreadable_scopes"] = ["reports/private"]
        values["payload"]["coverage_status"] = "partial"
        partial = self.compiler.compile("group.G.G1", values, self.authority, HEAD)
        self.assertEqual("decision-outcome-candidate/v1", partial["schema"], partial)
        self.assertEqual(["reports/private"], partial["payload"]["unreadable_scopes"])
        partial_ref = self.write("artifacts/partial-G1.json", partial)
        self.assertIn("blocked_partial_inventory_predecessor", self.compiler.compile("group.G.G2", self.inputs("group.G.G2", [partial_ref]), self.authority, HEAD)["reason"])
        values["payload"]["coverage_status"] = "complete"
        self.assertIn("blocked_inventory_coverage_contradiction", self.compiler.compile("group.G.G1", values, self.authority, HEAD)["reason"])

    def test_exact_predecessor_chain_rejects_missing_reordered_cross_group_and_stale_context(self):
        g_refs, h_refs, _, _ = self.compile_chains()
        g3 = self.inputs("group.G.G3", g_refs[:1])
        self.assertIn("blocked_missing_or_extra_predecessor", self.compiler.compile("group.G.G3", g3, self.authority, HEAD)["reason"])
        g3 = self.inputs("group.G.G3", [g_refs[1], g_refs[0]])
        self.assertIn("blocked_reordered_predecessor", self.compiler.compile("group.G.G3", g3, self.authority, HEAD)["reason"])
        g2 = self.inputs("group.G.G2", [h_refs[0]])
        self.assertIn("blocked_cross_group_predecessor", self.compiler.compile("group.G.G2", g2, self.authority, HEAD)["reason"])
        stale_head = {"revision": 52, "transaction_digest": HEAD["transaction_digest"]}
        self.assertIn("blocked_stale_predecessor_head", self.compiler.compile("group.G.G2", self.inputs("group.G.G2", g_refs[:1]), self.authority, stale_head)["reason"])
        other_authority = dict(self.authority, namespace="other")
        self.assertIn("blocked_stale_predecessor_authority", self.compiler.compile("group.G.G2", self.inputs("group.G.G2", g_refs[:1]), other_authority, HEAD)["reason"])
        relationless = self.inputs("group.G.G3", g_refs[:2])
        relationless["payload"]["nodes"][0]["disposition"] = "duplicate"
        self.assertIn("blocked_disposition_relation_mismatch", self.compiler.compile("group.G.G3", relationless, self.authority, HEAD)["reason"])

    def test_same_path_changed_digest_and_semantically_forged_persisted_candidate_fail(self):
        g_refs, _, _, _ = self.compile_chains()
        first = self.read(g_refs[0])
        first["payload"]["period"] = "replacement-period"
        first["candidate_digest"] = canonical_digest({key: value for key, value in first.items() if key != "candidate_digest"})
        replacement_ref = self.write(g_refs[0]["path"], first)
        self.assertEqual(g_refs[0]["path"], replacement_ref["path"])
        self.assertNotEqual(g_refs[0]["digest"], replacement_ref["digest"])
        refusal = self.compiler.compile("group.G.G3", self.inputs("group.G.G3", [replacement_ref, g_refs[1]]), self.authority, HEAD)
        self.assertEqual("decision-outcome-refusal/v1", refusal["schema"])
        forged = copy.deepcopy(first)
        forged["payload"]["searched_scopes"] = []
        forged["candidate_digest"] = canonical_digest({key: value for key, value in forged.items() if key != "candidate_digest"})
        forged_ref = self.write("artifacts/forged-G1.json", forged)
        refusal = self.compiler.compile("group.G.G2", self.inputs("group.G.G2", [forged_ref]), self.authority, HEAD)
        self.assertIn("blocked_persisted_candidate_semantics", refusal["reason"])

    def test_g2_unknown_or_agent_only_approval_is_not_approved(self):
        g1 = self.compiler.compile("group.G.G1", self.inputs("group.G.G1", []), self.authority, HEAD)
        g1_ref = self.write("artifacts/approval-G1.json", g1)
        approval_document = {"schema": "decision-approval-receipt/v1", "actor": {"type": "agent", "id": "worker-1"}, "approved": True, "candidate_ref": self.decision_ref, "authority_ref": self.authority_ref, "authority_digest": canonical_digest(self.authority), "expected_head": HEAD}
        approval = self.write("evidence/agent-approval.json", approval_document)
        values = self.inputs("group.G.G2", [g1_ref])
        values["evidence_refs"] = [self.decision_ref, approval, self.authority_ref]
        values["payload"]["classifications"][0].update({"status": "approved-decision", "authority_ref": self.authority_ref, "approval_receipt_ref": approval})
        self.assertIn("blocked_unknown_or_agent_only_approval", self.compiler.compile("group.G.G2", values, self.authority, HEAD)["reason"])
        approval_document["actor"] = {"type": "human", "id": "user-1"}
        approval_document["candidate_ref"] = self.generic_ref
        approval = self.write("evidence/unbound-human-approval.json", approval_document)
        values["evidence_refs"] = [self.decision_ref, approval, self.authority_ref]
        values["payload"]["classifications"][0]["approval_receipt_ref"] = approval
        self.assertIn("blocked_unbound_approval_receipt", self.compiler.compile("group.G.G2", values, self.authority, HEAD)["reason"])

    def test_g5_rejects_agent_self_approval_and_g6_rejects_held_or_overwrite(self):
        g_refs = []
        for selector in G[:4]:
            result = self.compiler.compile(selector, self.inputs(selector, g_refs), self.authority, HEAD)
            g_refs.append(self.write("artifacts/guard-%s.json" % selector.rsplit(".", 1)[-1], result))
        values = self.inputs("group.G.G5", g_refs)
        event = self.read(values["human_event_ref"]); event["actor"]["type"] = "agent"
        values["human_event_ref"] = self.write("events/agent-self-approval.json", event)
        self.assertIn("blocked_agent_self_approval", self.compiler.compile("group.G.G5", values, self.authority, HEAD)["reason"])
        adopted = self.inputs("group.G.G5", g_refs)
        adopted_result = self.compiler.compile("group.G.G5", adopted, self.authority, HEAD)
        adopted_refs = [*g_refs, self.write("artifacts/source-bound-G5.json", adopted_result)]
        g6 = self.inputs("group.G.G6", adopted_refs)
        g6["payload"]["transactions"][0]["source_event_refs"] = [self.generic_ref]
        self.assertIn("blocked_promotion_source_binding", self.compiler.compile("group.G.G6", g6, self.authority, HEAD)["reason"])
        g6 = self.inputs("group.G.G6", adopted_refs)
        g6["payload"]["transactions"][0]["backlinks"][0]["source_ref"] = self.generic_ref
        self.assertIn("blocked_backlink_source_binding", self.compiler.compile("group.G.G6", g6, self.authority, HEAD)["reason"])
        held = self.inputs("group.G.G5", g_refs, promotion_decision="hold")
        held_result = self.compiler.compile("group.G.G5", held, self.authority, HEAD)
        g_refs.append(self.write("artifacts/guard-G5.json", held_result))
        g6 = self.inputs("group.G.G6", g_refs)
        self.assertIn("blocked_unapproved_promotion", self.compiler.compile("group.G.G6", g6, self.authority, HEAD)["reason"])
        g6["payload"]["overwrite_existing"] = True
        self.assertIn("blocked_mutating_promotion_request", self.compiler.compile("group.G.G6", g6, self.authority, HEAD)["reason"])

    def test_h1_separates_claims_and_h2_blocks_false_achievement(self):
        g_refs, h_refs, candidates, _ = self.compile_chains()
        audit = candidates[6]["payload"]
        self.assertEqual(("completed", "improved", "achieved"), (audit["delivery"]["status"], audit["trajectory"]["status"], audit["objective_state"]["status"]))
        values = self.inputs("group.H.H1", g_refs, objective_status="unknown", unverified=[])
        h1 = self.compiler.compile("group.H.H1", values, self.authority, HEAD)
        h1_ref = self.write("artifacts/unknown-H1.json", h1)
        h2 = self.inputs("group.H.H2", [h1_ref], outcome="achieved")
        self.assertIn("blocked_achieved_without_objective_state", self.compiler.compile("group.H.H2", h2, self.authority, HEAD)["reason"])
        values = self.inputs("group.H.H1", g_refs, unverified=[{"item_id": "runtime", "material": True, "reason": "not observed"}])
        h1 = self.compiler.compile("group.H.H1", values, self.authority, HEAD)
        h1_ref = self.write("artifacts/material-H1.json", h1)
        h2 = self.inputs("group.H.H2", [h1_ref], outcome="achieved")
        self.assertIn("blocked_achieved_with_material_unverified", self.compiler.compile("group.H.H2", h2, self.authority, HEAD)["reason"])
        shared = self.inputs("group.H.H1", g_refs)
        shared["payload"]["objective_state"]["evidence_refs"] = shared["payload"]["delivery"]["evidence_refs"]
        self.assertIn("blocked_cross_dimension_evidence", self.compiler.compile("group.H.H1", shared, self.authority, HEAD)["reason"])

    def test_h1_replays_persisted_g6_and_profile_composition_semantics(self):
        g_refs, _, _, _ = self.compile_chains()
        forged_g6 = self.read(g_refs[-1])
        forged_g6["payload"]["transactions"][0]["source_event_refs"] = [self.generic_ref]
        forged_g6["candidate_digest"] = canonical_digest({key: value for key, value in forged_g6.items() if key != "candidate_digest"})
        forged_g6_ref = self.write("artifacts/forged-G6.json", forged_g6)
        values = self.inputs("group.H.H1", g_refs)
        values["evidence_refs"] = [forged_g6_ref if ref == self.g6_ref else ref for ref in values["evidence_refs"]]
        values["payload"]["decision_promotion_ref"] = forged_g6_ref
        self.assertIn("blocked_persisted_candidate_semantics", self.compiler.compile("group.H.H1", values, self.authority, HEAD)["reason"])

        forged_profile = self.read(self.profile_ref)
        forged_profile["result_kinds"][0] = forged_profile["result_kinds"][-1]
        forged_profile["composition_digest"] = canonical_digest({key: value for key, value in forged_profile.items() if key != "composition_digest"})
        forged_profile_ref = self.write("evidence/forged-profile.json", forged_profile)
        values = self.inputs("group.H.H1", g_refs)
        values["evidence_refs"] = [forged_profile_ref if ref == self.profile_ref else ref for ref in values["evidence_refs"]]
        values["payload"]["profile_ref"] = forged_profile_ref
        self.assertIn("blocked_persisted_profile_replay_mismatch", self.compiler.compile("group.H.H1", values, self.authority, HEAD)["reason"])

    def test_h3_requires_promoted_decisions_retention_and_rollback_and_never_cleans(self):
        g_refs, h_refs, candidates, _ = self.compile_chains()
        values = self.inputs("group.H.H3", h_refs)
        values["payload"]["important_decisions"][0].update({"promoted": False, "promotion_ref": None})
        self.assertIn("blocked_unpromoted_important_decision", self.compiler.compile("group.H.H3", values, self.authority, HEAD)["reason"])
        values = self.inputs("group.H.H3", h_refs); values["payload"]["rollback_artifact_refs"] = []
        self.assertIn("blocked_missing_rollback_evidence", self.compiler.compile("group.H.H3", values, self.authority, HEAD)["reason"])
        values = self.inputs("group.H.H3", h_refs); values["payload"]["retention"] = []
        self.assertIn("blocked_missing_retention_evidence", self.compiler.compile("group.H.H3", values, self.authority, HEAD)["reason"])
        values = self.inputs("group.H.H3", h_refs); values["payload"]["retention"][0]["retain"] = False
        self.assertIn("blocked_unretained_rollback_artifact", self.compiler.compile("group.H.H3", values, self.authority, HEAD)["reason"])
        self.assertFalse(candidates[-1]["performs_archive"] or candidates[-1]["performs_cleanup"])
        self.assertTrue(all(not item["executed"] for item in candidates[-1]["payload"]["cleanup_candidates"]))

    def test_complete_lifecycle_composition_replays_both_chains(self):
        g_refs, h_refs, _, _ = self.compile_chains()
        result = self.compiler.compose_lifecycle(g_refs, h_refs, self.authority, HEAD)
        self.assertEqual("decision-outcome-lifecycle-composition/v1", result["schema"], result)
        self.assertEqual([*G, *H], result["selectors"])
        reordered = self.compiler.compose_lifecycle([g_refs[1], g_refs[0], *g_refs[2:]], h_refs, self.authority, HEAD)
        self.assertIn("blocked_reordered_g_artifact", reordered["reason"])


if __name__ == "__main__":
    unittest.main()
