#!/usr/bin/env python3
"""Pure provider-projection entry point over the accepted planning seam."""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path
from typing import Any, Mapping, Optional, Sequence


SOURCE_ROOT = Path(__file__).resolve().parent
sys.path.insert(0, str(SOURCE_ROOT / "src"))

from ai_agent_workflow.distribution_planning import (  # noqa: E402
    DistributionPlanningError,
    NativeProjectionAdapter,
)


_REQUEST_KEYS = {
    "provider",
    "capability_profile",
    "sources",
    "semantics",
    "staged_artifact",
}


def plan_provider_projection(request: Mapping[str, Any]) -> dict[str, Any]:
    """Return a staged plan; the accepted adapter performs all integrity checks."""

    if not isinstance(request, Mapping) or set(request) != _REQUEST_KEYS:
        raise DistributionPlanningError(
            "projection request must contain exactly provider, capability_profile, "
            "sources, semantics and staged_artifact"
        )
    provider = request["provider"]
    sources = request["sources"]
    semantics = request["semantics"]
    if not isinstance(sources, list) or not isinstance(semantics, list):
        raise DistributionPlanningError("projection sources and semantics must be arrays")
    return NativeProjectionAdapter(provider, request["capability_profile"]).plan(
        sources,
        semantics,
        staged_artifact=request["staged_artifact"],
    )


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--request", required=True, type=Path)
    return parser


def main(argv: Optional[Sequence[str]] = None) -> int:
    args = _parser().parse_args(list(argv) if argv is not None else None)
    try:
        request = json.loads(args.request.read_text())
        result = plan_provider_projection(request)
        print(json.dumps(result, ensure_ascii=False, sort_keys=True, indent=2))
        return 0
    except (DistributionPlanningError, OSError, ValueError) as exc:
        print("error: %s" % exc, file=sys.stderr)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
