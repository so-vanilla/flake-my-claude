"""Public S2 B7 lifecycle integration against a disposable Kernel only."""
from __future__ import annotations

import copy
import hashlib
import json
import sys
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "src"))

from ai_agent_workflow.control_kernel import ControlKernel, StaleHeadError  # noqa: E402
from ai_agent_workflow.objective_system import ObjectiveSystemV1  # noqa: E402
from ai_agent_workflow.outcome_system import OutcomeSystemV1  # noqa: E402
from ai_agent_workflow.s2_lifecycle import LifecycleAdapterError, S2LifecycleAdapter  # noqa: E402


FIXTURE_PATH = "agent-workflows/tests/fixtures/s2/objective/lifecycle/adapter-positive.json"


class S2LifecycleIntegrationTests(unittest.TestCase):
    """A real B7 transaction never shares state with a live Run."""

    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.root = Path(self.temporary.name)
        self.kernel = ControlKernel(self.root, "fixture-run")
        self.fixture_bytes = (ROOT / "tests" / "fixtures" / "s2" / "objective" / "lifecycle" / "adapter-positive.json").read_bytes()
        self.fixture_digest = "sha256:" + hashlib.sha256(self.fixture_bytes).hexdigest()
        self.fixture = json.loads(self.fixture_bytes)
        self.kernel.entry(
            {"path": "fixture/objective-v001.md", "version": "v001", "digest": "sha256:" + "d" * 64},
            authority_ref={
                "approved": True, "scopes": ["entry"], "human_receipt": "fixture-entry",
                "fixture_identity": {"schema": "canonical-fixture-identity/v1", "run_id": "fixture-run", "namespace": "fixture:adapter-positive", "approval_scope": "fixture-only"},
            },
        )

    def tearDown(self):
        self.temporary.cleanup()

    def _command(self, head=None, *, namespace="fixture:adapter-positive"):
        ref = {"path": FIXTURE_PATH, "digest": self.fixture_digest, "selector": "fixture"}
        owner = {"owner_kind": "human", "owner_id": "fixture-owner", "authority_ref": copy.deepcopy(ref)}
        receipt = {"receipt_ref": copy.deepcopy(ref), "source": "human", "explicit": True, "decision": "approve", "actor_ref": owner}
        inputs = {
            "input_refs": [copy.deepcopy(ref)], "candidate_ref": copy.deepcopy(ref), "version": "v002",
            "candidate": {**copy.deepcopy(self.fixture["payload"]["candidate_ref"]), "namespace": namespace},
            "prior_objective": copy.deepcopy(self.fixture["payload"]["prior_objective"]),
            "approval_receipt": receipt, "namespace": namespace, "approval_scope": "fixture-only",
        }
        authority = {"owner_ref": owner, "source_refs": [copy.deepcopy(ref)]}
        current = head or self.kernel.head()
        expected_head = {"revision": current["revision"], "transaction_digest": current["transaction_digest"]}
        return ObjectiveSystemV1().compile("group.B.B7", inputs, authority, expected_head)

    def test_b7_uses_fresh_kernel_head_then_cold_resume_and_c02_drift_refusal(self):
        command = self._command()
        before = self.kernel.head()
        result = S2LifecycleAdapter().apply(command, self.kernel)
        self.assertEqual("v002", result["objective_ref"]["version"])
        self.assertNotEqual(before, self.kernel.head())
        cold = ControlKernel(self.root, "fixture-run")
        self.assertEqual("v002", cold.resume()["objective_ref"]["version"])
        c01 = json.loads((ROOT / "tests" / "fixtures" / "s2" / "foundation" / "epoch-measurement" / "valid-c01.json").read_text())
        c02 = json.loads((ROOT / "tests" / "fixtures" / "s2" / "foundation" / "epoch-measurement" / "valid-c02.json").read_text())
        c02["c01_bundle_ref"]["digest"] = "sha256:" + hashlib.sha256(
            json.dumps(c01, sort_keys=True, separators=(",", ":")).encode()
        ).hexdigest()
        self.assertEqual("C-01", OutcomeSystemV1().compose_c01(c01)["epoch_id"])
        self.assertEqual("C-02", OutcomeSystemV1().compose_c02(c02, c01)["epoch_id"])
        drifted = copy.deepcopy(c01); drifted["bundle_id"] = "c01-drifted"
        self.assertEqual("blocked_stale_input", OutcomeSystemV1().compose_c02(c02, drifted)["code"])

    def test_stale_command_and_live_namespace_refuse_without_mutation(self):
        stale = self._command({"revision": 1, "transaction_digest": "sha256:" + "b" * 64})
        before = self.kernel.head()
        with self.assertRaises(StaleHeadError):
            S2LifecycleAdapter().apply(stale, self.kernel)
        self.assertEqual(before, self.kernel.head())
        live = self._command(namespace="live:run")
        with self.assertRaises(LifecycleAdapterError):
            S2LifecycleAdapter().apply(live, self.kernel)
        self.assertEqual(before, self.kernel.head())
