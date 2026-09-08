#!/usr/bin/env python3
"""Pure source entry point for surface.company.audit-operation."""

import json
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "agent-workflows/src"))

from ai_agent_workflow.company_governance import CompanyGovernanceV1  # noqa: E402

SURFACE_ID = "surface.company.audit-operation"


def compile_candidate(request, *, source_root=ROOT, reference_root=None):
    return CompanyGovernanceV1(source_root=source_root, reference_root=reference_root).compile(
        SURFACE_ID, request.get("inputs"), request.get("authority"), request.get("expected_head")
    )


def main():
    json.dump(compile_candidate(json.load(sys.stdin)), sys.stdout, sort_keys=True)
    sys.stdout.write("\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
