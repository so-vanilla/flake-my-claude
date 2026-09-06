import copy
import hashlib
import json
import os
import subprocess
import tempfile
import unittest
from pathlib import Path
import sys


ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "src"))

from ai_agent_workflow.control_kernel import (  # noqa: E402
    AuthorizationError, CommandValidationError, ControlKernel, DuplicateCommandError,
    InjectedCrash, IntegrityBlockedError, StaleHeadError, canonical_digest,
)
from ai_agent_workflow.schema_validation import SchemaValidationError, validate_document  # noqa: E402


class ObjectiveTransitionCompatibilityTests(unittest.TestCase):
    def setUp(self):
        self.tempdir = tempfile.TemporaryDirectory()
        self.root = Path(self.tempdir.name)
        self.kernel = self._new_kernel(self.root)

    def tearDown(self):
        self.tempdir.cleanup()

    def test_evidence_six_path_aggregate_is_compact_canonical_json_without_lf(self):
        evidence = json.loads(
            (ROOT / "evidence" / "compatibility" / "S2-objective-transition.json").read_text()
        )
        physical_digests = {
            path: "sha256:" + hashlib.sha256((ROOT.parent / path).read_bytes()).hexdigest()
            for path in evidence["bound_product_digests"]
        }
        self.assertEqual(evidence["bound_product_digests"], physical_digests)
        canonical = json.dumps(
            physical_digests, sort_keys=True, separators=(",", ":")
        ).encode("utf-8")
        self.assertFalse(canonical.endswith(b"\n"))
        digest = "sha256:" + hashlib.sha256(canonical).hexdigest()
        self.assertEqual(evidence["bound_six_path_aggregate"], digest)
        self.assertNotEqual(
            evidence["bound_six_path_aggregate"],
            "sha256:" + hashlib.sha256(canonical + b"\n").hexdigest(),
        )

    @staticmethod
    def _new_kernel(root, fixture=True):
        kernel = ControlKernel(root, "run-s2-u")
        authority = {"status": "approved", "scopes": ["*"]}
        if fixture:
            authority["fixture_identity"] = {
                "schema": "canonical-fixture-identity/v1", "run_id": "run-s2-u",
                "namespace": "fixture:s2-u", "approval_scope": "fixture-only",
            }
        kernel.entry(
            {"path": "objectives/v001.md", "version": "v001", "digest": "sha256:" + "a" * 64},
            authority_ref=authority,
        )
        return kernel

    @staticmethod
    def _command(kernel, key="approve-s2-u"):
        candidate_digest = "sha256:" + "b" * 64
        proposal_digest = "sha256:" + "c" * 64
        receipt = {
            "schema": "human-approval-receipt/v1", "receipt_id": "receipt-s2-u",
            "approval_id": "approval-s2-u",
            "decision": "approve", "explicit": True, "source": "human",
            "actor_id": "human-fixture", "run_id": "run-s2-u",
            "namespace": "fixture:s2-u", "approval_scope": "fixture-only",
            "candidate_path": "objectives/v002.md", "candidate_version": "v002",
            "candidate_namespace": "fixture:s2-u", "candidate_digest": candidate_digest,
            "prior_objective_digest": "sha256:" + "a" * 64,
            "prior_objective_version": "v001", "proposal_digest": proposal_digest,
            "issued_at": "2026-09-05T00:00:00Z",
        }
        approval = {
            "schema": "objective-approval/v1", "approval_id": "approval-s2-u",
            "run_id": "run-s2-u", "namespace": "fixture:s2-u",
            "approval_scope": "fixture-only", "decision": "approve",
            "actor": {"kind": "human", "actor_id": "human-fixture"},
            "receipt": receipt, "candidate_digest": candidate_digest,
            "candidate_version": "v002", "prior_objective_digest": "sha256:" + "a" * 64,
            "prior_objective_version": "v001", "proposal_digest": proposal_digest,
        }
        authority = {
            "status": "approved", "scopes": ["approve_objective"],
            "run_id": "run-s2-u", "namespace": "fixture:s2-u",
            "approval_scope": "fixture-only", "actor_id": "human-fixture",
            "proposal_digest": proposal_digest, "human_receipt": receipt,
            "protected_fields": ["objective_ref"], "write_scopes": ["fixture:s2-u"],
        }
        return kernel.make_command(
            "approve_objective",
            {"candidate_ref": {"path": "objectives/v002.md", "version": "v002", "digest": candidate_digest, "namespace": "fixture:s2-u"},
             "prior_objective": {"version": "v001", "digest": "sha256:" + "a" * 64},
             "proposal_digest": proposal_digest, "approval": approval},
            authority_ref=authority, idempotency_key=key,
            protected_fields=["objective_ref"], scope=["fixture:s2-u"],
        )

    @staticmethod
    def _rewrite_current_transaction(kernel, mutate):
        head = json.loads(kernel.head_path.read_text())
        old_path = kernel.transactions_dir / (head["transaction_digest"][7:] + ".json")
        transaction = json.loads(old_path.read_text())
        mutate(transaction["state"])
        transaction["object_refs"] = copy.deepcopy(transaction["state"]["object_refs"])
        transaction.pop("digest")
        transaction["digest"] = canonical_digest(transaction)
        new_path = kernel.transactions_dir / (transaction["digest"][7:] + ".json")
        new_path.write_text(json.dumps(transaction, sort_keys=True, indent=2) + "\n")
        head["transaction_digest"] = transaction["digest"]
        head.pop("digest")
        head["digest"] = canonical_digest(head)
        kernel.head_path.write_text(json.dumps(head, sort_keys=True, indent=2) + "\n")

    def test_absent_explicit_human_receipt_refuses_without_mutation(self):
        before = self.kernel.head()
        command = self._command(self.kernel)
        command["payload"]["approval"] = {}
        with self.assertRaises(CommandValidationError):
            self.kernel.apply(command)
        self.assertEqual(self.kernel.head(), before)

    def test_implicit_ai_and_task_start_pseudo_approvals_refuse_without_mutation(self):
        for field, value in (("explicit", False), ("source", "ai"), ("source", "task-start")):
            command = self._command(self.kernel, "reject-" + str(value))
            command["payload"]["approval"]["receipt"][field] = value
            command["authority_ref"]["human_receipt"][field] = value
            before = self.kernel.head()
            with self.assertRaises(AuthorizationError):
                self.kernel.apply(command)
            self.assertEqual(self.kernel.head(), before)

        malformed = self._command(self.kernel, "malformed-receipt")
        malformed["payload"]["approval"]["receipt"]["issued_at"] = "not-a-date"
        malformed["authority_ref"]["human_receipt"]["issued_at"] = "not-a-date"
        before = self.kernel.head()
        with self.assertRaises(CommandValidationError):
            self.kernel.apply(malformed)
        self.assertEqual(self.kernel.head(), before)

    def test_wrong_candidate_namespace_scope_and_actor_refuse_without_mutation(self):
        mutations = [
            lambda c: c["payload"]["approval"].update(candidate_digest="sha256:" + "d" * 64),
            lambda c: c["authority_ref"].update(namespace="fixture:wrong"),
            lambda c: c.update(scope=["fixture:wrong"]),
            lambda c: c["authority_ref"].update(actor_id="wrong-human"),
        ]
        for index, mutate in enumerate(mutations):
            command = self._command(self.kernel, "wrong-%s" % index)
            mutate(command)
            before = self.kernel.head()
            with self.assertRaises((AuthorizationError, CommandValidationError)):
                self.kernel.apply(command)
            self.assertEqual(self.kernel.head(), before)

    def test_stale_head_and_changed_payload_retry_refuse_but_exact_retry_is_idempotent(self):
        stale = self._command(self.kernel, "stale")
        self.kernel.publish_artifact("advance", "v1", {}, authority_ref={"status": "approved", "scopes": ["*"]})
        before = self.kernel.head()
        with self.assertRaises(StaleHeadError):
            self.kernel.apply(stale)
        self.assertEqual(self.kernel.head(), before)

        command = self._command(self.kernel, "exact")
        first = self.kernel.apply(command)
        revision = first["revision"]
        self.assertEqual(self.kernel.apply(command)["revision"], revision)
        changed = copy.deepcopy(command)
        changed["command_id"] = "cmd-changed-retry"
        changed["payload"]["candidate_ref"]["path"] = "objectives/changed.md"
        with self.assertRaises(DuplicateCommandError):
            self.kernel.apply(changed)
        self.assertEqual(self.kernel.head()["revision"], revision)

    def test_human_receipt_is_bound_and_changed_semantic_reuse_refuses(self):
        first = self._command(self.kernel, "receipt-first")
        self.kernel.apply(first)
        changed = self._command(self.kernel, "receipt-reuse")
        changed["payload"]["prior_objective"] = {"version": "v002", "digest": "sha256:" + "b" * 64}
        approval = changed["payload"]["approval"]
        approval.update(approval_id="approval-second", candidate_version="v003",
                        prior_objective_version="v002", prior_objective_digest="sha256:" + "b" * 64)
        changed["payload"]["candidate_ref"].update(path="objectives/v003.md", version="v003", digest="sha256:" + "d" * 64)
        approval["candidate_digest"] = "sha256:" + "d" * 64
        changed["payload"]["approval"]["receipt"] = copy.deepcopy(first["payload"]["approval"]["receipt"])
        changed["authority_ref"]["human_receipt"] = copy.deepcopy(changed["payload"]["approval"]["receipt"])
        before = self.kernel.head()
        with self.assertRaises(AuthorizationError):
            self.kernel.apply(changed)
        self.assertEqual(self.kernel.head(), before)

    def test_semantic_schema_accepts_exact_envelope_and_rejects_closed_contract_violations(self):
        command_schema = json.loads((ROOT / "schemas" / "dag-command-v1.schema.json").read_text())
        approval_schema = json.loads((ROOT / "schemas" / "objective-approval-v1.schema.json").read_text())
        registry = dict(approval_schema["$defs"])
        valid = self._command(self.kernel, "schema-valid")
        validate_document(valid, command_schema, registry)
        invalids = []
        arbitrary_authority = copy.deepcopy(valid)
        arbitrary_authority["authority_ref"] = {"arbitrary": "authority"}
        invalids.append(arbitrary_authority)
        wrong_scope = copy.deepcopy(valid)
        wrong_scope["scope"] = ["*"]
        invalids.append(wrong_scope)
        empty_protected = copy.deepcopy(valid)
        empty_protected["protected_fields"] = []
        invalids.append(empty_protected)
        missing_receipt_binding = copy.deepcopy(valid)
        del missing_receipt_binding["payload"]["approval"]["receipt"]["approval_id"]
        invalids.append(missing_receipt_binding)
        for document in invalids:
            with self.assertRaises(SchemaValidationError):
                validate_document(document, command_schema, registry)

    def test_unmarked_run_and_objective_version_collisions_refuse_without_head_mutation(self):
        ordinary = self._new_kernel(self.root / "ordinary", fixture=False)
        before = ordinary.head()
        with self.assertRaises(AuthorizationError):
            ordinary.apply(self._command(ordinary, "ordinary-refusal"))
        self.assertEqual(ordinary.head(), before)

        same_current = self._command(self.kernel, "same-current-version")
        same_current["payload"]["candidate_ref"].update(version="v001", digest="sha256:" + "d" * 64)
        same_current["payload"]["approval"].update(candidate_version="v001", candidate_digest="sha256:" + "d" * 64)
        same_current["payload"]["approval"]["receipt"].update(candidate_version="v001", candidate_digest="sha256:" + "d" * 64)
        same_current["authority_ref"]["human_receipt"] = copy.deepcopy(same_current["payload"]["approval"]["receipt"])
        before = self.kernel.head()
        with self.assertRaises(AuthorizationError):
            self.kernel.apply(same_current)
        self.assertEqual(self.kernel.head(), before)

        self.kernel.apply(self._command(self.kernel, "establish-history"))
        historical = self._command(self.kernel, "historical-version")
        historical["payload"]["prior_objective"] = {"version": "v002", "digest": "sha256:" + "b" * 64}
        historical["payload"]["candidate_ref"].update(version="v001", digest="sha256:" + "d" * 64)
        historical["payload"]["approval"].update(
            approval_id="approval-history", candidate_version="v001", candidate_digest="sha256:" + "d" * 64,
            prior_objective_version="v002", prior_objective_digest="sha256:" + "b" * 64,
        )
        historical["payload"]["approval"]["receipt"].update(
            receipt_id="receipt-history", approval_id="approval-history", candidate_version="v001",
            candidate_digest="sha256:" + "d" * 64, prior_objective_version="v002",
            prior_objective_digest="sha256:" + "b" * 64,
        )
        historical["authority_ref"]["human_receipt"] = copy.deepcopy(historical["payload"]["approval"]["receipt"])
        before = self.kernel.head()
        with self.assertRaises(AuthorizationError):
            self.kernel.apply(historical)
        self.assertEqual(self.kernel.head(), before)

    def test_digest_consistent_malformed_and_crosswired_associations_refuse_cold_resume(self):
        for name, mutate in (
            ("malformed", lambda state: state["objective_approvals"].update({"approval-s2-u": {}})),
            ("crosswired", lambda state: state["objective_approvals"]["approval-s2-u"].update(
                event_ref=copy.deepcopy(state["objective_approvals"]["approval-s2-u"]["candidate_ref"]))),
        ):
            kernel = self._new_kernel(self.root / name)
            kernel.apply(self._command(kernel, "approve-" + name))
            self._rewrite_current_transaction(kernel, mutate)
            with self.assertRaises(IntegrityBlockedError):
                ControlKernel(self.root / name, "run-s2-u").resume()

    def test_reverse_lexical_two_approval_history_survives_cold_resume(self):
        first = self._command(self.kernel, "reverse-first")
        first["payload"]["approval"]["approval_id"] = "approval-z"
        first["payload"]["approval"]["receipt"].update(
            receipt_id="receipt-z", approval_id="approval-z",
        )
        first["authority_ref"]["human_receipt"] = copy.deepcopy(first["payload"]["approval"]["receipt"])
        self.kernel.apply(first)

        second = self._command(self.kernel, "reverse-second")
        second["payload"]["candidate_ref"].update(
            path="objectives/v003.md", version="v003", digest="sha256:" + "d" * 64,
        )
        second["payload"]["prior_objective"] = {
            "version": "v002", "digest": "sha256:" + "b" * 64,
        }
        second["payload"]["proposal_digest"] = "sha256:" + "e" * 64
        second["payload"]["approval"].update(
            approval_id="approval-a", candidate_digest="sha256:" + "d" * 64,
            candidate_version="v003", prior_objective_digest="sha256:" + "b" * 64,
            prior_objective_version="v002", proposal_digest="sha256:" + "e" * 64,
        )
        second["payload"]["approval"]["receipt"].update(
            receipt_id="receipt-a", approval_id="approval-a",
            candidate_path="objectives/v003.md", candidate_digest="sha256:" + "d" * 64,
            candidate_version="v003", prior_objective_digest="sha256:" + "b" * 64,
            prior_objective_version="v002", proposal_digest="sha256:" + "e" * 64,
            issued_at="2026-09-05T00:01:00Z",
        )
        second["authority_ref"].update(
            proposal_digest="sha256:" + "e" * 64,
            human_receipt=copy.deepcopy(second["payload"]["approval"]["receipt"]),
        )
        approved = self.kernel.apply(second)

        resumed = ControlKernel(self.root, "run-s2-u").resume()
        self.assertEqual(resumed["objective_ref"], approved["objective_ref"])

    def test_fault_recovery_cold_resume_corrupt_chain_and_old_version_history(self):
        for phase in ("before_publish", "after_publish_before_head", "after_head_before_projection"):
            kernel = self._new_kernel(self.root / phase)
            command = self._command(kernel, "fault-" + phase)
            with self.assertRaises(InjectedCrash):
                with kernel.fault(phase):
                    kernel.apply(command)
            report = kernel.recover()
            self.assertTrue(report["head_unchanged"])
            state = kernel.apply(command)
            self.assertEqual(state["objective_ref"]["version"], "v002")
            self.assertEqual(state["objective_history"][0]["version"], "v001")

        approved = self.kernel.apply(self._command(self.kernel))
        env = os.environ.copy()
        env["PYTHONPATH"] = str(ROOT / "src")
        code = "from ai_agent_workflow.control_kernel import ControlKernel; import json; print(json.dumps(ControlKernel(%r, 'run-s2-u').resume()))" % str(self.root)
        resumed = json.loads(subprocess.run([sys.executable, "-c", code], env=env, text=True, capture_output=True, check=True).stdout)
        self.assertEqual(resumed["objective_ref"], approved["objective_ref"])
        head = self.kernel.head()
        transaction = self.kernel.transactions_dir / (head["transaction_digest"][7:] + ".json")
        value = json.loads(transaction.read_text())
        value["state"]["objective_ref"]["version"] = "forged"
        transaction.write_text(json.dumps(value))
        with self.assertRaises(IntegrityBlockedError):
            self.kernel.read_state()


if __name__ == "__main__":
    unittest.main()
