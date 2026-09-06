"""Small JSON CLI around the A6 WorkflowStore walking skeleton."""

from __future__ import annotations

import argparse
import json
import sys
from typing import Any, Dict, List, Optional

from .core import WorkflowError, WorkflowStore


def _json_object(value: str) -> Dict[str, Any]:
    parsed = json.loads(value)
    if not isinstance(parsed, dict):
        raise ValueError("expected a JSON object")
    return parsed


def _json_list(value: str) -> List[Any]:
    parsed = json.loads(value)
    if not isinstance(parsed, list):
        raise ValueError("expected a JSON list")
    return parsed


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", required=True, help="repository root containing .local/agent")
    sub = parser.add_subparsers(dest="command", required=True)

    entry = sub.add_parser("entry")
    entry.add_argument("--run-id", required=True)
    entry.add_argument("--objective", required=True, help="objective reference JSON")
    entry.add_argument("--workflow-version", required=True)
    entry.add_argument("--group-id", default="bootstrap")
    entry.add_argument("--epoch-id", default="epoch-0001")
    entry.add_argument("--alias", action="append", default=[])
    entry.add_argument("--external-ref", action="append", default=[])

    artifact = sub.add_parser("produce-artifact")
    artifact.add_argument("--run-id", required=True)
    artifact.add_argument("--expected-revision", required=True, type=int)
    artifact.add_argument("--artifact-id", required=True)
    artifact.add_argument("--version", required=True)
    artifact.add_argument("--payload", required=True)

    close_epoch = sub.add_parser("close-epoch")
    close_epoch.add_argument("--run-id", required=True)
    close_epoch.add_argument("--expected-revision", required=True, type=int)
    close_epoch.add_argument("--boundary-reason", required=True)
    close_epoch.add_argument("--acceptance-evidence", required=True)
    close_epoch.add_argument("--approved-decisions", required=True)
    close_epoch.add_argument("--unresolved-items", required=True)
    close_epoch.add_argument("--invalidated-artifacts", required=True)
    close_epoch.add_argument("--next-inputs", required=True)
    close_epoch.add_argument("--next-epoch-id", required=True)

    open_epoch = sub.add_parser("open-epoch")
    open_epoch.add_argument("--run-id", required=True)
    open_epoch.add_argument("--expected-revision", required=True, type=int)
    open_epoch.add_argument("--epoch-id", required=True)
    open_epoch.add_argument("--input-bundle", required=True)
    open_epoch.add_argument("--boundary-reason", required=True)

    close_group = sub.add_parser("close-group")
    close_group.add_argument("--run-id", required=True)
    close_group.add_argument("--expected-revision", required=True, type=int)
    close_group.add_argument("--acceptance-evidence", required=True)
    close_group.add_argument("--approved-decisions", required=True)
    close_group.add_argument("--unresolved-items", required=True)
    close_group.add_argument("--invalidated-artifacts", required=True)
    close_group.add_argument("--next-inputs", required=True)
    close_group.add_argument("--next-group")

    for name in ("resume", "status"):
        query = sub.add_parser(name)
        query.add_argument("query")
        query.add_argument("--expected-revision", type=int)
        query.add_argument("--expected-workflow-version")
        query.add_argument("--expected-bundle-digest")
    return parser


def main(argv: Optional[List[str]] = None) -> int:
    args = _parser().parse_args(argv)
    store = WorkflowStore(args.root)
    try:
        if args.command == "entry":
            result = store.entry(
                args.run_id,
                _json_object(args.objective),
                args.workflow_version,
                group_id=args.group_id,
                epoch_id=args.epoch_id,
                aliases=args.alias,
                external_refs=args.external_ref,
            )
        elif args.command == "produce-artifact":
            result = store.produce_artifact(
                args.run_id,
                args.expected_revision,
                args.artifact_id,
                args.version,
                _json_object(args.payload),
            )
        elif args.command == "close-epoch":
            result, _ = store.close_epoch(
                args.run_id,
                args.expected_revision,
                args.boundary_reason,
                _json_list(args.acceptance_evidence),
                _json_list(args.approved_decisions),
                _json_list(args.unresolved_items),
                _json_list(args.invalidated_artifacts),
                _json_list(args.next_inputs),
                args.next_epoch_id,
            )
        elif args.command == "open-epoch":
            result = store.open_epoch(
                args.run_id,
                args.expected_revision,
                args.epoch_id,
                _json_object(args.input_bundle),
                args.boundary_reason,
            )
        elif args.command == "close-group":
            result, _ = store.close_group(
                args.run_id,
                args.expected_revision,
                _json_list(args.acceptance_evidence),
                _json_list(args.approved_decisions),
                _json_list(args.unresolved_items),
                _json_list(args.invalidated_artifacts),
                _json_list(args.next_inputs),
                args.next_group,
            )
        else:
            result = store.resume(
                args.query,
                expected_revision=args.expected_revision,
                expected_workflow_version=args.expected_workflow_version,
                expected_bundle_digest=args.expected_bundle_digest,
            )
        print(json.dumps(result, ensure_ascii=False, sort_keys=True, indent=2))
        return 0
    except (WorkflowError, ValueError, json.JSONDecodeError) as exc:
        print("error: %s" % exc, file=sys.stderr)
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
