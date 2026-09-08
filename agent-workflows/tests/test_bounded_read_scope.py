import copy
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "src"))

from ai_agent_workflow.bounded_read_scope import (  # noqa: E402
    ReadScopeError,
    compile_macos_forbidden_read_profile,
    compile_read_scope,
    observe_read_scope,
    verify_read_scope_receipt,
)


class BoundedReadScopeTests(unittest.TestCase):
    def test_scope_and_complete_observation_are_digest_bound_without_os_claim(self):
        scope = compile_read_scope("/workspace/project", ["tests", "src"])
        self.assertEqual(scope["declared_paths"], ["src", "tests"])
        self.assertFalse(scope["os_isolation_enforced"])
        self.assertIn("not enforced", scope["limitation"])

        receipt = observe_read_scope(
            scope, ["tests/test_cli.py", "src/app.py"], observation_complete=True
        )
        self.assertEqual(verify_read_scope_receipt(scope, receipt), receipt)
        self.assertFalse(receipt["os_isolation_enforced"])

    def test_escape_partial_trace_and_forged_os_claim_are_refused(self):
        scope = compile_read_scope("/workspace/project", ["src"])
        for observed, complete in [(["../sibling/secret"], True), (["docs/secret"], True), (["src/app.py"], False)]:
            with self.subTest(observed=observed, complete=complete), self.assertRaises(ReadScopeError):
                observe_read_scope(scope, observed, observation_complete=complete)

        receipt = observe_read_scope(scope, ["src/app.py"], observation_complete=True)
        forged = copy.deepcopy(receipt)
        forged["os_isolation_enforced"] = True
        with self.assertRaises(ReadScopeError):
            verify_read_scope_receipt(scope, forged)

    def test_noncanonical_and_empty_scope_are_refused(self):
        for paths in ([], ["/absolute"], ["../sibling"], ["src/../other"], ["src", "src"]):
            with self.subTest(paths=paths), self.assertRaises(ReadScopeError):
                compile_read_scope("/workspace/project", paths)

    @unittest.skipUnless(sys.platform == "darwin" and shutil.which("sandbox-exec"), "macOS sandbox-exec fixture only")
    def test_macos_fixture_denies_known_sibling_but_allows_project_read(self):
        with tempfile.TemporaryDirectory(prefix="bounded-read-scope-") as root:
            parent = Path(root).resolve()
            project, sibling = parent / "project", parent / "sibling"
            project.mkdir(); sibling.mkdir()
            allowed, denied = project / "request.txt", sibling / "secret.txt"
            allowed.write_text("allowed", encoding="utf-8")
            denied.write_text("denied", encoding="utf-8")
            profile = compile_macos_forbidden_read_profile([sibling])
            self.assertIn(str(sibling), profile)
            permitted = subprocess.run(
                ["/usr/bin/sandbox-exec", "-p", profile, "/bin/cat", str(allowed)],
                capture_output=True, text=True, timeout=5,
            )
            blocked = subprocess.run(
                ["/usr/bin/sandbox-exec", "-p", profile, "/bin/cat", str(denied)],
                capture_output=True, text=True, timeout=5,
            )
            self.assertEqual((permitted.returncode, permitted.stdout), (0, "allowed"))
            self.assertNotEqual(blocked.returncode, 0)
            self.assertEqual(blocked.stdout, "")


if __name__ == "__main__":
    unittest.main()
