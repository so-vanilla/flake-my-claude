"""Read the active A7 control-kernel state selected by a cutover pointer."""

from __future__ import annotations

import argparse
import json
import os
import sys
from pathlib import Path
from typing import List, Optional

from .control_kernel import KernelError
from .implementation_status import ImplementationStatusError, evaluate_implementation
from .migration import NewReader, PointerCutover


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    subcommands = parser.add_subparsers(dest="command", required=True)
    for command, help_text in (
        ("status", "read the active A7 status"),
        ("resume", "read the active A7 recovery state"),
    ):
        query = subcommands.add_parser(command, help=help_text)
        query.add_argument(
            "--pointer",
            required=True,
            help="path to the active cutover-pointer/v1 document",
        )
        manifest_default = os.environ.get("AGENT_WORKFLOW_IMPLEMENTATION_MANIFEST")
        source_root_default = os.environ.get("AGENT_WORKFLOW_SOURCE_ROOT")
        query.add_argument(
            "--implementation-manifest",
            default=manifest_default,
            required=manifest_default is None,
            help="path to the digest-bound implementation status manifest",
        )
        query.add_argument(
            "--source-root",
            default=source_root_default,
            required=source_root_default is None,
            help="source root containing the canonical plans and implementation",
        )
    return parser


def main(argv: Optional[List[str]] = None) -> int:
    args = _parser().parse_args(argv)
    try:
        implementation = evaluate_implementation(
            Path(args.implementation_manifest), Path(args.source_root)
        )
        reader = NewReader()
        pointer = PointerCutover(args.pointer)
        result = reader.status(pointer=pointer) if args.command == "status" else reader.read(pointer=pointer)
        result["implementation"] = implementation
        print(json.dumps(result, ensure_ascii=False, sort_keys=True, indent=2))
        return 0
    except (ImplementationStatusError, KernelError, OSError) as exc:
        print("error: %s" % exc, file=sys.stderr)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
