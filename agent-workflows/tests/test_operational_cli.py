import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SRC = ROOT / "agent-workflows" / "src"
IMPLEMENTATION_MANIFEST = (
    ROOT / "agent-workflows" / "manifests" / "implementation-status.json"
)
sys.path.insert(0, str(SRC))

from ai_agent_workflow.control_kernel import canonical_digest  # noqa: E402
from ai_agent_workflow.implementation_status import evaluate_implementation  # noqa: E402
from ai_agent_workflow.migration import LegacyConverter, LegacyReader, PointerCutover  # noqa: E402


AUTHORITY = {"status": "approved", "scopes": ["*"]}


class OperationalCliTests(unittest.TestCase):
    def setUp(self):
        self.tempdir = tempfile.TemporaryDirectory()
        self.root = Path(self.tempdir.name)
        fixtures = ROOT / "agent-workflows" / "tests" / "fixtures" / "a6r"
        source_dir = self.root / "sources"
        source_dir.mkdir()
        copied = {}
        for name in ("legacy-run.json", "legacy-bundle.json", "legacy-worker-report.json"):
            target = source_dir / name
            value = json.loads((fixtures / name).read_text(encoding="utf-8"))
            if name == "legacy-run.json":
                value["aliases"] = ["operational-cli"]
                value["current_group"]["status"] = "open"
                value["current_epoch"]["closed_at_revision"] = 11
            elif name == "legacy-bundle.json":
                value.update({"group_id": "bootstrap", "aliases": ["operational-cli"], "state_revision": 11})
                value["context_epoch"].update(
                    {"group_id": "bootstrap", "status": "closed", "closed_at_revision": 11}
                )
            else:
                value.update(
                    {
                        "run_id": "legacy-a6",
                        "group_id": "bootstrap",
                        "status": "done",
                        "aliases": ["operational-cli"],
                        "state_revision": 11,
                    }
                )
            target.write_text(json.dumps(value, sort_keys=True), encoding="utf-8")
            copied[name] = target

        source_paths = {
            "run": copied["legacy-run.json"],
            "bundle": copied["legacy-bundle.json"],
            "worker_report": copied["legacy-worker-report.json"],
        }
        source_digests = {
            name: LegacyReader().read_with_digest(path)["source_digest"]
            for name, path in source_paths.items()
        }
        self.converted = LegacyConverter().convert(
            source_paths["run"],
            source_paths["bundle"],
            source_paths["worker_report"],
            destination=self.root / "new-kernel",
            run_id="operational-cli",
            authority_ref=AUTHORITY,
            expected_source_revision=11,
            expected_source_digests=source_digests,
        )
        self.pointer = PointerCutover(self.root / "active-pointer.json")
        old = {"revision": 11, "digest": "sha256:" + "c" * 64}
        new = self.converted["new_head"]
        source = self.converted["source_digests"]
        authority = {
            "status": "approved",
            "scopes": ["migration_cutover", "migration_rollback"],
            "migration_approval": True,
            "approval_ref": "operational-cli-test-cutover",
            "proposal_digest": self.pointer.proposal_digest(new, source, old),
            "run_id": self.converted["run_id"],
            "role": "orchestrator",
            "assignment_id": "orchestrator",
        }
        self.pointer.cutover(
            new,
            source_digests=source,
            old_pointer=old,
            expected_old_pointer=old,
            authority_ref=authority,
        )

    def tearDown(self):
        self.tempdir.cleanup()

    def _run(self, *arguments, include_implementation_manifest=True):
        environment = dict(os.environ)
        environment["PYTHONPATH"] = str(SRC)
        environment["PYTHONDONTWRITEBYTECODE"] = "1"
        command_arguments = list(arguments)
        if include_implementation_manifest and command_arguments[:1] in (["status"], ["resume"]):
            command_arguments.extend(
                [
                    "--implementation-manifest",
                    str(IMPLEMENTATION_MANIFEST),
                    "--source-root",
                    str(ROOT),
                ]
            )
        return subprocess.run(
            [sys.executable, "-m", "ai_agent_workflow", *command_arguments],
            cwd=ROOT,
            env=environment,
            text=True,
            capture_output=True,
            check=False,
        )

    def test_default_status_reads_the_active_a7_pointer(self):
        result = self._run("status", "--pointer", str(self.pointer.path))

        self.assertEqual(result.returncode, 0, result.stderr)
        value = json.loads(result.stdout)
        self.assertEqual(value["run_id"], "operational-cli")
        self.assertEqual(value["revision"], self.converted["kernel"].head()["revision"])
        self.assertEqual(value["head"]["transaction_digest"], self.converted["kernel"].head()["transaction_digest"])
        self.assertEqual(
            value["implementation"],
            evaluate_implementation(IMPLEMENTATION_MANIFEST, ROOT),
        )

    def test_default_resume_reads_the_same_active_a7_truth(self):
        status = self._run("status", "--pointer", str(self.pointer.path))
        resumed = self._run("resume", "--pointer", str(self.pointer.path))

        self.assertEqual(resumed.returncode, 0, resumed.stderr)
        self.assertEqual(json.loads(resumed.stdout), json.loads(status.stdout))

    def test_default_cli_fails_closed_when_active_target_is_not_new(self):
        pointer_value = self.pointer.read()
        pointer_value["active"] = "old"
        pointer_value["pointer_digest"] = canonical_digest(
            {key: value for key, value in pointer_value.items() if key != "pointer_digest"}
        )
        self.pointer.path.write_text(json.dumps(pointer_value), encoding="utf-8")

        result = self._run("status", "--pointer", str(self.pointer.path))

        self.assertEqual(result.returncode, 2)
        self.assertEqual(result.stdout, "")
        self.assertIn("active cutover target is not the new kernel", result.stderr)

    def test_default_cli_fails_closed_when_pointer_head_is_stale(self):
        current = self.converted["kernel"].read_state()
        self.converted["kernel"].open_epoch(
            "epoch-after-cutover",
            current["epoch"]["bundle_ref"],
            authority_ref=AUTHORITY,
        )

        result = self._run("resume", "--pointer", str(self.pointer.path))

        self.assertEqual(result.returncode, 2)
        self.assertEqual(result.stdout, "")
        self.assertIn("state revision is stale", result.stderr)

    def test_default_cli_does_not_expose_legacy_a6_mutations(self):
        result = self._run("entry")

        self.assertEqual(result.returncode, 2)
        self.assertEqual(result.stdout, "")
        self.assertIn("invalid choice: 'entry'", result.stderr)

    def test_default_cli_requires_an_explicit_pointer(self):
        result = self._run("status")

        self.assertEqual(result.returncode, 2)
        self.assertEqual(result.stdout, "")
        self.assertIn("the following arguments are required: --pointer", result.stderr)

    def test_default_cli_requires_the_implementation_manifest(self):
        result = self._run(
            "status",
            "--pointer",
            str(self.pointer.path),
            include_implementation_manifest=False,
        )

        self.assertEqual(result.returncode, 2)
        self.assertEqual(result.stdout, "")
        self.assertIn(
            "the following arguments are required: --implementation-manifest",
            result.stderr,
        )


if __name__ == "__main__":
    unittest.main()
