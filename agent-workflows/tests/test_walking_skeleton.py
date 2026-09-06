import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
SRC = ROOT / "src"
sys.path.insert(0, str(SRC))

from ai_agent_workflow import (  # noqa: E402
    BUDGET_POLICY,
    InvariantError,
    StaleStateError,
    WorkflowStore,
    canonical_digest,
)


class A6_WalkingSkeletonTests(unittest.TestCase):
    def setUp(self):
        self.tempdir = tempfile.TemporaryDirectory()
        self.root = Path(self.tempdir.name)
        self.store = WorkflowStore(self.root)
        self.objective = {
            "path": "objectives/v001.md",
            "version": "v001",
            "digest": "a" * 64,
            "approval_status": "candidate",
        }

    def tearDown(self):
        self.tempdir.cleanup()

    def enter(self):
        return self.store.entry(
            "run-demo",
            self.objective,
            "manual-bootstrap/v1",
            aliases=["demo"],
            external_refs=["issue:42", "https://example.invalid/issue/42"],
            authority={"approved": ["isolated fixture"], "not_approved": ["live activation"]},
        )

    def close_epoch(self, revision, next_epoch="epoch-0002"):
        return self.store.close_epoch(
            "run-demo",
            revision,
            "design-to-implementation",
            ["tests: pending fixture"],
            ["three semantic layers; Epoch is runtime metadata"],
            [{"id": "later", "status": "open"}],
            [{"path": "old-summary.txt", "reason": "non-canonical"}],
            [{"path": "next/input.json", "version": "v1", "digest": "b" * 64}],
            next_epoch,
            token_status="unavailable",
        )

    def test_entry_artifact_epoch_resume_alias_and_stale_guards(self):
        state = self.enter()
        self.assertEqual(state["state_revision"], 1)
        state = self.store.produce_artifact(
            "run-demo", 1, "walking-skeleton", "v1", {"result": "ok"}, ["unit test"]
        )
        self.assertEqual(state["state_revision"], 2)
        with self.assertRaises(StaleStateError):
            self.store.produce_artifact("run-demo", 1, "other", "v1", {}, [])
        state, bundle = self.close_epoch(2)
        self.assertEqual(state["state_revision"], 3)
        self.assertEqual(bundle["context_budget"]["token_status"], "unavailable")
        self.assertIsNone(bundle["context_budget"]["token_count"])
        self.assertEqual(bundle["context_budget"]["target"], BUDGET_POLICY["target"])
        self.assertEqual(bundle["digest"], canonical_digest({k: v for k, v in bundle.items() if k != "digest"}))
        ref = state["current_epoch"]["bundle_ref"]
        resumed = self.store.resume("issue:42", expected_revision=3, expected_bundle_digest=ref["digest"])
        self.assertEqual(resumed["current_epoch"]["id"], "epoch-0001")
        with self.assertRaises(StaleStateError):
            self.store.resume("demo", expected_revision=2)
        with self.assertRaises(StaleStateError):
            self.store.resume("demo", expected_workflow_version="manual-bootstrap/v0")
        with self.assertRaises(StaleStateError):
            self.store.resume("demo", expected_bundle_digest="0" * 64)
        with self.assertRaises(InvariantError):
            self.store.update_objective("run-demo", 3, dict(self.objective, version="v002"))

    def test_same_group_epoch_continuation_and_group_clear(self):
        self.enter()
        self.store.produce_artifact("run-demo", 1, "first", "v1", {"x": 1})
        state, bundle = self.close_epoch(2)
        state = self.store.open_epoch(
            "run-demo",
            state["state_revision"],
            "epoch-0002",
            state["current_epoch"]["bundle_ref"],
            "implementation-to-independent-review",
            clear_before_start=True,
        )
        self.assertEqual(state["state_revision"], 4)
        self.assertEqual(state["current_group"]["id"], "bootstrap")
        self.assertEqual(state["current_epoch"]["group_id"], "bootstrap")
        self.assertEqual(state["current_epoch"]["input_bundle"]["digest"], bundle["digest"])
        with self.assertRaises(InvariantError):
            self.store.open_epoch(
                "run-demo",
                state["state_revision"],
                "epoch-invalid",
                state["current_epoch"]["input_bundle"],
                "invalid",
                clear_before_start=False,
            )
        self.store.produce_artifact("run-demo", 4, "second", "v1", {"x": 2})
        state, _ = self.close_epoch(5, next_epoch="epoch-0003")
        state, group_bundle = self.store.close_group(
            "run-demo",
            state["state_revision"],
            ["group fixture accepted"],
            ["group clear is common closure protocol"],
            [],
            [],
            [{"group": "next"}],
            next_group="next-group",
        )
        self.assertEqual(state["status"], "paused_after_group")
        self.assertEqual(state["current_group"]["status"], "closed")
        self.assertTrue(state["current_group"]["clear_required"])
        self.assertEqual(group_bundle["context_epoch"]["id"], "group-close")
        self.assertEqual(group_bundle["context_budget"]["absolute_limit"], 500000)

    def test_budget_validation_and_cli_fresh_reader(self):
        self.enter()
        self.store.produce_artifact("run-demo", 1, "artifact", "v1", {"value": True})
        with self.assertRaises(InvariantError):
            self.store.close_epoch(
                "run-demo", 2, "boundary", [], [], [], [], [], "epoch-0002",
                token_status="unavailable", token_count=1,
            )
        state, bundle = self.store.close_epoch(
            "run-demo", 2, "boundary", ["fixture"], [], [], [], [], "epoch-0002",
            token_status="estimated", token_count=123,
        )
        self.assertEqual(bundle["context_budget"]["token_status"], "estimated")
        env = os.environ.copy()
        env["PYTHONPATH"] = str(SRC)
        command = [
            sys.executable,
            "-m",
            "ai_agent_workflow.cli",
            "--root",
            str(self.root),
            "status",
            "run-demo",
            "--expected-revision",
            str(state["state_revision"]),
            "--expected-workflow-version",
            "manual-bootstrap/v1",
            "--expected-bundle-digest",
            bundle["digest"],
        ]
        result = subprocess.run(command, env=env, capture_output=True, text=True, check=True)
        current = json.loads(result.stdout)
        self.assertEqual(current["run_id"], "run-demo")
        self.assertEqual(current["state_revision"], 3)

    def test_schemas_have_the_required_contracts(self):
        for filename, required in {
            "run-state-v1.schema.json": {"schema", "run_id", "state_revision", "current_epoch"},
            "artifact-bundle-v1.schema.json": {
                "schema", "canonical_artifacts", "acceptance_evidence", "approved_decisions",
                "unresolved_items", "invalidated_artifacts", "next_inputs", "digest",
            },
            "skill-result-v1.schema.json": {"result", "context_epoch", "artifact_bundle", "next_skill_advice"},
            "worker-report-v1.schema.json": {"worker_id", "context_epoch", "artifact_refs", "verification"},
        }.items():
            schema = json.loads((ROOT / "schemas" / filename).read_text())
            self.assertTrue(required.issubset(set(schema["required"])), filename)


if __name__ == "__main__":
    unittest.main()
