#!/usr/bin/env python3
"""Thin JSON-file adapter for PersistentReceiptRunner."""
import argparse
import json
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "src"))
from ai_agent_workflow.persistent_receipts import PersistentReceiptRunner  # noqa: E402

parser = argparse.ArgumentParser()
parser.add_argument("--package", required=True, type=Path)
parser.add_argument("--policy", required=True, type=Path)
parser.add_argument("--receipt-root", required=True, type=Path)
parser.add_argument("--recover", action="store_true")
args = parser.parse_args()
package = json.loads(args.package.read_text(encoding="utf-8"))
policy = json.loads(args.policy.read_text(encoding="utf-8"))
runner = PersistentReceiptRunner(args.receipt_root)
receipt = runner.recover(package, policy) if args.recover else runner.run(package, policy)
print(json.dumps(receipt, sort_keys=True))
