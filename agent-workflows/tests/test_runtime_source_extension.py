"""Current source compatibility never rewrites accepted historical bytes."""
import json
import shutil
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "agent-workflows/src"))
from ai_agent_workflow.runtime_source_extension import EVIDENCE_PATH, HISTORICAL_PATH, verify_runtime_source_extension


class RuntimeSourceExtensionTests(unittest.TestCase):
    def copy_sources(self):
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        root = Path(temporary.name).resolve()
        evidence = json.loads((ROOT / EVIDENCE_PATH).read_text())
        paths = {EVIDENCE_PATH, HISTORICAL_PATH, *evidence["current_products"], *evidence["validation_sources"]}
        for path in paths:
            (root / path).parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(ROOT / path, root / path)
        return root

    def test_current_bytes_match_explicit_extension(self):
        evidence = json.loads((ROOT / EVIDENCE_PATH).read_text())
        self.assertEqual(verify_runtime_source_extension(ROOT), evidence["current_products"])
        self.assertNotEqual(evidence["current_products"], evidence["historical_products"])
        self.assertFalse(evidence["historical_acceptance_rewritten"])

    def test_historical_or_current_tampering_is_refused(self):
        for path in (HISTORICAL_PATH, "agent-workflows/src/ai_agent_workflow/control_kernel.py", EVIDENCE_PATH):
            with self.subTest(path=path):
                root = self.copy_sources()
                (root / path).write_bytes((root / path).read_bytes() + b"\n")
                with self.assertRaises(ValueError):
                    verify_runtime_source_extension(root)

    def test_missing_extension_is_refused(self):
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        with self.assertRaises(OSError):
            verify_runtime_source_extension(Path(temporary.name))
