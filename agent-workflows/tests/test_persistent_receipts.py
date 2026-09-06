import json
import os
import copy
import hashlib
import sys
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "src"))

from ai_agent_workflow.persistent_receipts import (  # noqa: E402
    DuplicateReceiptConflict,
    DeterministicProcessAdapter,
    PersistentReceiptError,
    PersistentReceiptRunner,
)
from ai_agent_workflow.execution_v2 import ExecutionClosureBuilder, ReceiptAggregator, RegressionFrontier  # noqa: E402
from ai_agent_workflow.schema_validation import validate_document  # noqa: E402


def digest(value):
    encoded = json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=False).encode()
    return "sha256:" + hashlib.sha256(encoded).hexdigest()


def package(key="receipt-key", command="probe", retry_class="none"):
    artifact_digest = "sha256:" + "a" * 64
    source = {
        "schema": "execution-package-input/v2", "package_id": "package-001", "contract_version": "workflow-execution/v2", "workspace_identity": "/workspace",
        "candidate_ref": {"id": "candidate-001", "digest": artifact_digest}, "test_refs": [{"id": "test-a", "digest": artifact_digest}], "fixture_refs": [],
        "schema_refs": [{"id": "schema", "digest": artifact_digest}], "config_refs": [], "lock_refs": [{"id": "lock", "digest": artifact_digest}],
        "toolchain": {"executable_digest": artifact_digest, "identity": "python-3.13"}, "command": {"argv": [command], "cwd": "/workspace"},
        "environment": {"LANG": "C.UTF-8"}, "isolation": {"cwd": "/workspace", "temporary_namespace": "tmp", "output_namespace": "out"},
        "resource_claims": {"read_paths": ["src"], "write_paths": [], "exclusive_resources": []},
        "supervision": {"timeout_seconds": 5, "grace_seconds": 1, "signals": ["TERM", "KILL"], "heartbeat_seconds": 1, "terminal_publication_seconds": 1}, "external_input_refs": [],
    }
    closure = ExecutionClosureBuilder().freeze(source)
    return {
        "schema": "execution-package/v1",
        "package_id": closure["package_id"],
        "execution_closure": closure,
        "execution_closure_ref": {"id": closure["package_id"], "digest": closure["closure_digest"]},
        "shard_id": "shard-001",
        "coverage": ["test-a"],
        "idempotency_key": key,
        "nonce": "nonce-001",
        "supervisor_lease": "lease-001",
        "retry_class": retry_class,
    }


def policy():
    return {
        "schema": "supervision-policy/v1",
        "timeout_seconds": 5,
        "heartbeat_seconds": 1,
        "heartbeat_expiry_seconds": 2,
        "grace_seconds": 1,
        "terminal_publication_seconds": 1,
        "output_limit_bytes": 4,
        "signals": ["TERM", "KILL"],
        "sensitivity": "confidential",
    }


class PersistentReceiptRunnerTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name)
        self.adapter = DeterministicProcessAdapter()
        self.runner = PersistentReceiptRunner(self.root, adapter=self.adapter, boot_id="boot-test")

    def tearDown(self):
        self.temp.cleanup()

    def test_terminal_receipt_is_reused_and_captures_only_bounded_owner_output_refs(self):
        self.adapter.queue(exit_code=0, stdout=b"abcdef", stderr=b"x")

        first = self.runner.run(package(), policy())
        reused = self.runner.run(package(), policy())

        self.assertEqual(first["status"], "passed")
        self.assertTrue(first["terminal"])
        self.assertEqual(reused, first)
        self.assertEqual(self.adapter.spawn_count, 1)
        self.assertNotIn("abcdef", json.dumps(first))
        envelope = json.loads(next((self.root / "receipts").iterdir()).read_text())
        stdout = envelope["captures"]["stdout"]
        stderr = envelope["captures"]["stderr"]
        self.assertEqual(stdout["integrity"], "truncated")
        self.assertEqual((stdout["original_byte_count"], stdout["retained_byte_count"]), (6, 4))
        self.assertNotEqual(stdout["report_safe_ref"], stderr["report_safe_ref"])
        capture = self.root / stdout["report_safe_ref"]
        self.assertEqual(capture.read_bytes(), b"abcd")
        self.assertEqual(oct(capture.stat().st_mode & 0o777), "0o600")
        self.assertEqual(oct(capture.parent.stat().st_mode & 0o777), "0o700")
        changed = package(command="different")
        with self.assertRaises(DuplicateReceiptConflict):
            self.runner.run(changed, policy())
        capture.write_bytes(b"corrupt")
        with self.assertRaises(PersistentReceiptError):
            self.runner.recover(package(), policy())

    def test_recovery_never_duplicates_live_fence_and_timeout_terminalizes_once(self):
        self.adapter.queue(live=True)
        started = self.runner.run(package(), policy())
        recovered = PersistentReceiptRunner(self.root, adapter=self.adapter, boot_id="boot-test").recover(package(), policy())
        self.assertEqual(started["state"], "running")
        self.assertEqual(recovered["process_identity"], started["process_identity"])
        self.assertEqual(self.adapter.spawn_count, 1)

        timeout_root = self.root / "timeout"
        adapter = DeterministicProcessAdapter()
        adapter.queue(timeout=True, stdout=b"late")
        timed = PersistentReceiptRunner(timeout_root, adapter=adapter, boot_id="boot-test").run(package("timeout-key"), policy())
        self.assertEqual(timed["status"], "timed_out")
        self.assertEqual(adapter.signals, [("TERM", 1)])
        self.assertEqual(adapter.waits, [(1, 1)])
        self.assertEqual(adapter.reap_count, 1)
        self.assertEqual(PersistentReceiptRunner(timeout_root, adapter=adapter, boot_id="boot-test").recover(package("timeout-key"), policy()), timed)

        kill_root = self.root / "kill"
        kill_adapter = DeterministicProcessAdapter()
        kill_adapter.queue(timeout=True, ignore_term=True)
        killed = PersistentReceiptRunner(kill_root, adapter=kill_adapter, boot_id="boot-test").run(package("kill-key"), policy())
        self.assertEqual(killed["status"], "timed_out")
        self.assertEqual(kill_adapter.signals, [("TERM", 1), ("KILL", 1)])
        self.assertEqual(kill_adapter.waits, [(1, 1), (1, 1)])
        self.assertEqual(kill_adapter.reap_count, 1)

    def test_fault_boundaries_fail_closed_without_a_second_live_execution(self):
        self.adapter.queue(live=True)
        failed = self.runner.run(package(), policy(), fault="after-spawn-pre-identity")
        self.assertEqual(failed["status"], "interrupted")
        self.assertEqual(self.adapter.spawn_count, 1)
        self.assertEqual(self.adapter.live_count, 0)
        self.assertEqual(self.runner.recover(package(), policy()), failed)

        pre_root = self.root / "before"
        pre = PersistentReceiptRunner(pre_root, adapter=DeterministicProcessAdapter(), boot_id="boot-test")
        before = pre.run(package("before-key"), policy(), fault="before-spawn")
        self.assertEqual(before["status"], "interrupted")

    def test_closure_digest_is_recomputed_and_terminal_identity_is_f1_compatible(self):
        self.adapter.queue(exit_code=0)
        receipt = self.runner.run(package(), policy())
        validate_document(receipt, json.loads((ROOT / "schemas/command-receipt-v1.schema.json").read_text()))
        self.assertEqual(receipt["candidate_digest"], package()["execution_closure"]["candidate_ref"]["digest"])
        tampered = package("tampered")
        tampered["execution_closure"]["command"]["argv"] = ["changed"]
        with self.assertRaises(PersistentReceiptError):
            self.runner.run(tampered, policy())

    def test_durable_identity_precedes_release_and_recovery_refuses_ambiguous_identity(self):
        self.adapter.queue(live=True)
        running = self.runner.run(package(), policy())
        self.assertEqual(self.adapter.lifecycle, ["prepared", "released"])
        persisted = json.loads(next((self.root / "receipts").iterdir()).read_text())
        self.assertEqual({key: persisted["process_identity"][key] for key in self.adapter.identities[1]}, self.adapter.identities[1])
        ambiguous = copy.deepcopy(persisted)
        ambiguous["process_identity"].pop("birth_token")
        next((self.root / "receipts").iterdir()).write_text(json.dumps(ambiguous))
        with self.assertRaises(PersistentReceiptError):
            PersistentReceiptRunner(self.root, adapter=self.adapter, boot_id="boot-test").recover(package(), policy())
        self.adapter.terminate_group(1, "KILL")
        self.adapter.wait_group(1, 1)
        self.adapter.reap(1)

    def test_timing_retry_and_capture_metadata_are_digest_bound(self):
        self.adapter.queue(exit_code=1, stdout=b"out", stderr=b"secret")
        receipt = self.runner.run(package(retry_class="command-or-capture-retry"), policy())
        envelope = json.loads(next((self.root / "receipts").iterdir()).read_text())
        timing = envelope["timing"]
        self.assertRegex(timing["started_at"], r"^20\d\d-\d\d-\d\dT.*Z$")
        self.assertGreaterEqual(timing["ended_monotonic"], timing["started_monotonic"])
        self.assertEqual(sum(timing["phase_seconds"].values()), timing["elapsed_seconds"])
        self.assertEqual(envelope["provenance"], {"launch_class": "new-run", "loop_class": "initial", "outcome_class": "failed", "retry_class": "command-or-capture-retry"})
        unsigned = {key: value for key, value in envelope.items() if key not in {"payload_digest", "terminal_receipt"}}
        self.assertEqual(envelope["payload_digest"], digest(unsigned))
        self.assertEqual(receipt["payload_digest"], envelope["payload_digest"])
        with self.assertRaises(PersistentReceiptError):
            self.runner.run(package("unknown", retry_class="made-up"), policy())
        envelope["timing"]["phase_seconds"]["supervision"] += 1
        next((self.root / "receipts").iterdir()).write_text(json.dumps(envelope))
        with self.assertRaises(PersistentReceiptError):
            self.runner.recover(package(retry_class="command-or-capture-retry"), policy())

    def test_frozen_shard_closure_ref_is_the_single_runner_input_seam(self):
        digest = "sha256:" + "a" * 64
        source = {
            "schema": "execution-package-input/v2", "package_id": "package-001", "contract_version": "workflow-execution/v2", "workspace_identity": "/workspace",
            "candidate_ref": {"id": "candidate-001", "digest": digest}, "test_refs": [{"id": "test-a", "digest": digest}], "fixture_refs": [],
            "schema_refs": [{"id": "schema", "digest": digest}], "config_refs": [], "lock_refs": [{"id": "lock", "digest": digest}],
            "toolchain": {"executable_digest": digest, "identity": "python-3.13"}, "command": {"argv": ["python3", "-m", "unittest"], "cwd": "/workspace"},
            "environment": {"LANG": "C.UTF-8"}, "isolation": {"cwd": "/workspace", "temporary_namespace": "tmp", "output_namespace": "out"},
            "resource_claims": {"read_paths": ["src"], "write_paths": [], "exclusive_resources": []},
            "supervision": {"timeout_seconds": 5, "grace_seconds": 1, "signals": ["TERM", "KILL"], "heartbeat_seconds": 1, "terminal_publication_seconds": 1}, "external_input_refs": [],
        }
        closure = ExecutionClosureBuilder().freeze(source)
        candidate = {"schema": "artifact-candidate/v1", "candidate_id": "candidate-001", "candidate_digest": digest, "execution_closure_digest": closure["closure_digest"], "regression_inventory": ["test-a"], "frozen": True}
        shard = {"shard_id": "shard-a", "members": ["test-a"], "command": {"argv": ["python3", "-m", "unittest", "test-a"]}, "resource_claims": {"read_paths": ["src"], "write_paths": [], "exclusive_resources": []}, "isolation": {"cwd": "/workspace/a", "temporary_namespace": "tmp-a", "output_namespace": "out-a"}}
        planned = RegressionFrontier().plan(candidate, closure, [shard])["shards"][0]
        runner_input = {"schema": "execution-package/v1", "package_id": planned["execution_closure_ref"]["id"], "execution_closure": planned["execution_closure"], "execution_closure_ref": planned["execution_closure_ref"], "shard_id": "shard-a", "coverage": ["test-a"], "idempotency_key": "run-shard-a", "nonce": "nonce-a", "supervisor_lease": "lease-a", "retry_class": "none"}
        schema = json.loads((ROOT / "schemas" / "command-receipt-v1.schema.json").read_text())
        closure_schema = json.loads((ROOT / "schemas" / "execution-package-closure-v2.schema.json").read_text())
        validate_document(runner_input, schema["$defs"]["execution_package"], {"execution-package-closure-v2.schema.json": closure_schema})
        self.assertEqual(runner_input["execution_closure_ref"]["digest"], planned["execution_closure"]["closure_digest"])
        self.adapter.queue(exit_code=0)
        receipt = self.runner.run(runner_input, policy())
        aggregate = ReceiptAggregator().aggregate(candidate, RegressionFrontier().plan(candidate, closure, [shard]), [receipt])
        self.assertTrue(aggregate["accepted"])
        self.assertEqual(aggregate["receipt_refs"][0]["execution_closure_ref"], runner_input["execution_closure_ref"])


if __name__ == "__main__":
    unittest.main()
