"""Deterministic source-wide fixtures for the three software profiles.

The compiler proves only that accepted source contracts connect.  It never
reads or writes a current Run, advances HEAD, executes product work, mints an
approval, or turns the Group H source skeleton into live completion evidence.
"""

from __future__ import annotations

import copy
import hashlib
import json
import re
from collections.abc import Mapping
from pathlib import Path
from typing import Any

from .execution_v2 import FindingValidator, V2ContractError
from .schema_validation import SchemaValidationError, validate_document
from .workflow_composition import WorkflowCompositionError, WorkflowCompositionV1


class SourceWideFixtureError(ValueError):
    """A fixture is stale, ambiguous, reordered, or falsely authorizing."""


_DEFAULT_SOURCE_ROOT = Path(__file__).resolve().parents[3]
_FIXED_GIT_HEAD = "e5b87e08d0730e0f2a2c99c9a4883a172c0078e7"
_SHA256 = re.compile(r"^sha256:[0-9a-f]{64}$")
_AXES = ("architecture-safety", "integration-operability")
_GROUP_PATHS = {
    "B": "agent-workflows/groups/objective.json",
    "C": "agent-workflows/groups/outcomes.json",
    "D": "agent-workflows/groups/planning.json",
    "E": "agent-workflows/groups/execution.json",
    "F": "agent-workflows/groups/shared-closure.json",
    "G": "agent-workflows/groups/decisions.json",
    "H": "agent-workflows/groups/completion.json",
}
_PROFILE_WORKFLOWS = {
    "feature": "feature-architectural",
    "bug-fix": "bug-fix-standard",
    "improvement": "improvement-measured",
}
_ACCEPTED_SOURCE_DIGESTS = {
    "agent-workflows/catalog.yaml": "sha256:2fd1caaddf4509d2f56bd31a68b062290e70f2f2da33facf1d8187d476bb3f1d",
    "agent-workflows/workflows/feature-architectural.json": "sha256:d07f87b88421cd046af493ca0206f49fc67a3981bfc77f7ceb78f21a28380d8e",
    "agent-workflows/workflows/bug-fix-standard.json": "sha256:5944a15896cd3a28c0669cb6060915511c4dd44025a911639598647c0d403151",
    "agent-workflows/workflows/improvement-measured.json": "sha256:d545860609ca6b9844f6ffac45cf69cdc19c6e240824a5377f063f431b1cca9d",
    "agent-workflows/profiles/feature.json": "sha256:b7b82dbe8e3fdbddd290cbe4a84f169ebb06bb41d6a5c69fa67b7e058291c0b3",
    "agent-workflows/profiles/bug-fix.json": "sha256:25cc7ef5e6ab54762d3d8952df71c5ae2043bc0801a0d397f32ff448087b5de8",
    "agent-workflows/profiles/improvement.json": "sha256:86216b07c0271ffe9fe78b0709d1e151f29ba72f223a746c8bba307bf45385fd",
    "agent-workflows/groups/objective.json": "sha256:88ae5e0ad703935c5b15d8ca7c7127efd26103b8bc7e497e6ae832bc62027f12",
    "agent-workflows/groups/outcomes.json": "sha256:3c5c7d7d049082d57ff07ece95273bc004f0bf86e3768544ebabfbaac601270e",
    "agent-workflows/groups/planning.json": "sha256:754f7ac80f6f563df34757e058a7a5aa12e818db318e2ee852493a4702bed3e5",
    "agent-workflows/groups/execution.json": "sha256:2977b487dcc6a93ae5c7edb1dfb4e07f04a813a3c3d95d68e04923f0201a6e52",
    "agent-workflows/groups/shared-closure.json": "sha256:b56d74d6be80f031082f67f8811c70993c96ad99e82bb401bb2d2d229c0b837f",
    "agent-workflows/groups/decisions.json": "sha256:627e8a8b1e707f8799898e1d5fbf16cbdfc6a18995a17f7c4f5eb4ed4eb35e07",
    "agent-workflows/groups/completion.json": "sha256:bdc2c2e20e13179736711e4ecf51cc4fb8fdba1c312e4613d86e416f9c4b958a",
    "agent-workflows/groups/required-only-feedback-execution-policy-v1.json": "sha256:0ba45de33e42ab7b7127f62b35f04e168e2cb43300dcbcda8012c6f615f55c34",
    "agent-workflows/src/ai_agent_workflow/workflow_composition.py": "sha256:07562ee404381b9d40c7e7ef9e22f20af068c4e5b96461134e3684ee2cfb4fe6",
    "agent-workflows/src/ai_agent_workflow/software_profiles.py": "sha256:93cb44ac60fa95cc5545a0567968e59ebf984be4ac8ef8aac6fe61e6fe598d6a",
    "agent-workflows/src/ai_agent_workflow/execution_v2.py": "sha256:6dda9f347c4866a4da86d5b23102eecae7895f899f094be5f310d358edd92f9d",
    "agent-workflows/src/ai_agent_workflow/execution_group.py": "sha256:a6718485618f2d9d5f72b82b20d797c181829d0e990adfbc9e148765608fd2d2",
    "agent-workflows/src/ai_agent_workflow/closure_protocol.py": "sha256:5f6a7fd51b6d92733f03a36e5eaade48d16618ee771b05b38f5d97c875521b82",
    "agent-workflows/src/ai_agent_workflow/decision_outcome_lifecycle.py": "sha256:1c4b18db6b490c8e452b6c12d44059ef1352523689c7fa2d4c738dfa6a638e7d",
    "agent-workflows/manifests/additional-required-surfaces.json": "sha256:ffc43fe7f26e8d1ec34123d73f10e9f2cc08477665de51d6df9f4848e42f587f",
}


def _canonical_digest(value: Any) -> str:
    raw = json.dumps(
        value, sort_keys=True, separators=(",", ":"), ensure_ascii=True
    ).encode()
    return "sha256:" + hashlib.sha256(raw).hexdigest()


def _bytes_digest(value: bytes) -> str:
    return "sha256:" + hashlib.sha256(value).hexdigest()


def _require(condition: bool, reason: str) -> None:
    if not condition:
        raise SourceWideFixtureError(reason)


def _exact(value: Any, keys: set[str], reason: str) -> Mapping[str, Any]:
    _require(isinstance(value, Mapping) and set(value) == keys, reason)
    return value


def _identifier(value: Any, reason: str) -> str:
    _require(isinstance(value, str) and bool(value.strip()), reason)
    return value


def _head(value: Any) -> dict[str, Any]:
    head = _exact(
        value, {"revision", "transaction_digest"}, "cold resume HEAD is malformed"
    )
    _require(
        isinstance(head["revision"], int)
        and not isinstance(head["revision"], bool)
        and head["revision"] >= 0
        and isinstance(head["transaction_digest"], str)
        and _SHA256.fullmatch(head["transaction_digest"]) is not None,
        "cold resume HEAD is malformed",
    )
    return copy.deepcopy(dict(head))


class SourceWideFixturesV1:
    """Compile source-only execution, resume, and Group H skeleton evidence."""

    def __init__(
        self,
        *,
        source_root: Path | None = None,
        reference_root: Path | None = None,
        schema_path: Path | None = None,
    ) -> None:
        self.source_root = Path(source_root or _DEFAULT_SOURCE_ROOT).resolve()
        self.reference_root = Path(reference_root or self.source_root).resolve()
        path = Path(
            schema_path
            or self.source_root
            / "agent-workflows/schemas/source-wide-fixture-result-v1.schema.json"
        )
        try:
            self.schema = json.loads(path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError) as error:
            raise SourceWideFixtureError(
                f"cannot read source-wide result schema: {error}"
            ) from error

    def _read_bytes(self, relative: str) -> bytes:
        path = Path(relative)
        _require(
            not path.is_absolute() and ".." not in path.parts,
            "source ref path is unsafe",
        )
        for root in dict.fromkeys((self.reference_root, self.source_root)):
            resolved = (root / path).resolve()
            try:
                resolved.relative_to(root)
            except ValueError:
                continue
            try:
                return resolved.read_bytes()
            except OSError:
                continue
        raise SourceWideFixtureError(f"source ref is unavailable: {relative}")

    def _accepted_ref(self, path: str, version: str) -> dict[str, str]:
        _require(
            path in _ACCEPTED_SOURCE_DIGESTS,
            f"source is not in the accepted S8 set: {path}",
        )
        digest = _bytes_digest(self._read_bytes(path))
        _require(
            digest == _ACCEPTED_SOURCE_DIGESTS[path],
            f"accepted source digest drifted: {path}",
        )
        return {"path": path, "version": version, "digest": digest}

    def _load_ref(
        self, value: Any, *, accepted: bool = False
    ) -> tuple[Any, dict[str, str]]:
        ref = _exact(value, {"path", "version", "digest"}, "physical ref is malformed")
        path = _identifier(ref["path"], "physical ref path is missing")
        version = _identifier(ref["version"], "physical ref version is missing")
        _require(
            isinstance(ref["digest"], str)
            and _SHA256.fullmatch(ref["digest"]) is not None,
            "physical ref digest is malformed",
        )
        raw = self._read_bytes(path)
        _require(
            _bytes_digest(raw) == ref["digest"], f"physical ref digest drifted: {path}"
        )
        if accepted:
            _require(
                self._accepted_ref(path, version) == ref,
                f"physical ref does not bind the accepted source: {path}",
            )
        try:
            document = json.loads(raw.decode("utf-8"))
        except (UnicodeDecodeError, json.JSONDecodeError) as error:
            raise SourceWideFixtureError(f"physical ref is not JSON: {path}") from error
        _require(
            isinstance(document, Mapping) and document.get("schema") == version,
            f"physical ref schema/version mismatch: {path}",
        )
        return copy.deepcopy(document), copy.deepcopy(dict(ref))

    def _verify_declared_digest_ref(self, ref: Any) -> None:
        _require(
            isinstance(ref, Mapping) and {"path", "digest"}.issubset(ref),
            "manifest source ref is malformed",
        )
        value = ref
        _require(
            isinstance(value["digest"], str)
            and _SHA256.fullmatch(value["digest"]) is not None
            and _bytes_digest(self._read_bytes(value["path"])) == value["digest"],
            "manifest source ref digest drifted: {}".format(
                value.get("path", "unknown")
            ),
        )

    def _validate_s7_acceptance(self) -> dict[str, str]:
        ref = self._accepted_ref(
            "agent-workflows/manifests/additional-required-surfaces.json",
            "additional-required-surfaces/v1",
        )
        manifest, _ = self._load_ref(ref, accepted=True)
        surfaces = manifest.get("surfaces")
        _require(
            isinstance(surfaces, list) and len(surfaces) == 11,
            "S7 surface set is not exact",
        )
        ids: set[str] = set()
        for surface in surfaces:
            _require(
                isinstance(surface, Mapping)
                and surface.get("state") == "accepted"
                and surface.get("section_owner") == "S7"
                and surface.get("id") not in ids,
                "S7 surface is not uniquely accepted",
            )
            ids.add(surface["id"])
            _require(
                _bytes_digest(self._read_bytes(surface["canonical_source"]))
                == surface["canonical_source_digest"],
                "S7 canonical source digest drifted: {}".format(surface["id"]),
            )
            evidence = surface.get("evidence")
            _require(
                isinstance(evidence, Mapping)
                and evidence.get("subject_id") == surface["id"]
                and evidence.get("source") == surface["canonical_source"]
                and evidence.get("source_digest") == surface["canonical_source_digest"]
                and evidence.get("result") == "passed"
                and _bytes_digest(self._read_bytes(evidence["evidence"]))
                == evidence["evidence_digest"],
                "S7 acceptance receipt drifted: {}".format(surface["id"]),
            )
        return ref

    def _workflow_and_profile(
        self,
        profile: str,
        workflow_ref_value: Any,
        profile_ref_value: Any,
    ) -> tuple[dict[str, Any], dict[str, Any], dict[str, str], dict[str, str]]:
        _require(profile in _PROFILE_WORKFLOWS, "unknown source profile")
        self._accepted_ref(
            "agent-workflows/src/ai_agent_workflow/workflow_composition.py",
            "python-source/v1",
        )
        self._accepted_ref(
            "agent-workflows/src/ai_agent_workflow/software_profiles.py",
            "python-source/v1",
        )
        workflow, workflow_ref = self._load_ref(workflow_ref_value, accepted=True)
        profile_manifest, profile_ref = self._load_ref(profile_ref_value, accepted=True)
        _require(
            workflow_ref["path"]
            == f"agent-workflows/workflows/{_PROFILE_WORKFLOWS[profile]}.json",
            "cross-profile workflow ref",
        )
        _require(
            profile_ref["path"] == f"agent-workflows/profiles/{profile}.json",
            "cross-profile profile-manifest ref",
        )
        _require(
            workflow.get("profile") == profile
            and profile_manifest.get("profile") == profile
            and workflow.get("workflow_id") == _PROFILE_WORKFLOWS[profile],
            "cross-profile source binding",
        )
        try:
            receipt = WorkflowCompositionV1(source_root=self.source_root).validate(
                workflow
            )
        except WorkflowCompositionError as error:
            raise SourceWideFixtureError(
                f"workflow composition rejected: {error}"
            ) from error
        return receipt, copy.deepcopy(profile_manifest), workflow_ref, profile_ref

    def _profile_contract_refs(
        self,
        profile: str,
        manifest: Mapping[str, Any],
        workflow_receipt: Mapping[str, Any],
    ) -> list[dict[str, Any]]:
        contracts = manifest.get("contracts")
        _require(
            isinstance(contracts, list) and bool(contracts),
            "profile contract manifest is empty",
        )
        selectors = [
            item.get("qualified_id") for item in contracts if isinstance(item, Mapping)
        ]
        workflow_selectors = [
            item
            for item in workflow_receipt["selectors"]
            if item.startswith("profile.")
        ]
        _require(
            selectors == workflow_selectors,
            "profile contract order differs from workflow order",
        )
        result: list[dict[str, Any]] = []
        for index, contract in enumerate(contracts, 1):
            _require(
                contract.get("order") == index
                and contract.get("status") == "candidate-source"
                and contract.get("selector_ref", {}).get("selector")
                == "SoftwareProfileV1.compile({})".format(contract["qualified_id"]),
                "profile contract order or stable interface drifted",
            )
            self._verify_declared_digest_ref(contract.get("template_ref"))
            selector_ref = contract.get("selector_ref")
            _require(
                isinstance(selector_ref, Mapping)
                and set(selector_ref) == {"path", "digest", "selector"},
                "profile selector ref is malformed",
            )
            self._verify_declared_digest_ref(
                {"path": selector_ref["path"], "digest": selector_ref["digest"]}
            )
            self._verify_declared_digest_ref(contract.get("evidence_ref"))
            result.append(
                {
                    "qualified_id": contract["qualified_id"],
                    "host_selector": contract["host_selector"],
                    "template_ref": copy.deepcopy(contract["template_ref"]),
                    "selector_ref": copy.deepcopy(selector_ref),
                    "evidence_ref": copy.deepcopy(contract["evidence_ref"]),
                }
            )
        return result

    def _group_implementation_refs(
        self, workflow_receipt: Mapping[str, Any]
    ) -> list[dict[str, Any]]:
        selected = workflow_receipt["selectors"]
        result: list[dict[str, Any]] = []
        groups = list(workflow_receipt["common_scopes"])
        groups.insert(groups.index("E") + 1, "F")
        for group in groups:
            path = _GROUP_PATHS[group]
            manifest_ref = self._accepted_ref(
                path,
                {
                    "B": "group-b-manifest/v1",
                    "C": "group-c-manifest/v1",
                    "D": "group-d-manifest/v1",
                    "E": "group-e-manifest/v1",
                    "F": "shared-closure-manifest/v1",
                    "G": "group-g-manifest/v1",
                    "H": "group-h-manifest/v1",
                }[group],
            )
            manifest, _ = self._load_ref(manifest_ref, accepted=True)
            group_selectors = list(
                dict.fromkeys(
                    item for item in selected if item.startswith(f"group.{group}.")
                )
            )
            interface_refs: list[dict[str, str]] = []
            if group == "F":
                _require(
                    manifest.get("selectors", [])[: len(group_selectors)]
                    == group_selectors
                    and group_selectors
                    == [f"group.F.F{number}" for number in range(1, 8)],
                    "shared closure order or boundary drifted",
                )
                source = _exact(
                    manifest.get("source"),
                    {"path", "version"},
                    "closure source ref is malformed",
                )
                interface_refs.append(
                    self._accepted_ref(source["path"], "python-source/v1")
                )
            else:
                contracts = manifest.get("contracts")
                _require(
                    isinstance(contracts, list),
                    "Group manifest contracts are malformed",
                )
                manifest_selectors = [item.get("qualified_id") for item in contracts]
                _require(
                    manifest_selectors[: len(group_selectors)] == group_selectors,
                    f"Group {group} selector order or dependency coverage drifted",
                )
                by_selector = {item["qualified_id"]: item for item in contracts}
                for selector in group_selectors:
                    contract = by_selector[selector]
                    source_ref = contract.get("source_ref")
                    receipt_ref = contract.get("receipt_ref") or contract.get(
                        "evidence_ref"
                    )
                    self._verify_declared_digest_ref(source_ref)
                    self._verify_declared_digest_ref(receipt_ref)
                    selector_ref = contract.get("selector_ref")
                    _require(
                        isinstance(selector_ref, Mapping)
                        and set(selector_ref) == {"path", "digest", "selector"}
                        and isinstance(selector_ref["selector"], str)
                        and bool(selector_ref["selector"]),
                        f"Group stable selector interface drifted: {selector}",
                    )
                    self._verify_declared_digest_ref(
                        {"path": selector_ref["path"], "digest": selector_ref["digest"]}
                    )
                    ref = {
                        "path": selector_ref["path"],
                        "version": "python-source/v1",
                        "digest": selector_ref["digest"],
                    }
                    if ref not in interface_refs:
                        interface_refs.append(ref)
            result.append(
                {
                    "group": group,
                    "manifest_ref": manifest_ref,
                    "selectors": group_selectors,
                    "interface_refs": interface_refs,
                    "implementation_embedded": False,
                }
            )
        return result

    @staticmethod
    def _review_report(
        fixture_id: str,
        round_number: int,
        candidate_revision: int,
        review: Mapping[str, Any],
    ) -> dict[str, Any]:
        axis = review["axis"]
        return {
            "schema": "ordinary-review-report/v1",
            "report_id": f"{fixture_id}-r{round_number}-{axis}",
            "axis": axis,
            "actor_id": review["actor_id"],
            "context_epoch_id": review["context_epoch_id"],
            "package_digest": _canonical_digest(
                {"fixture_id": fixture_id, "round": round_number, "axis": axis}
            ),
            "candidate_digest": _canonical_digest(
                {"fixture_id": fixture_id, "candidate_revision": candidate_revision}
            ),
            "aggregate_digest": _canonical_digest(
                {
                    "fixture_id": fixture_id,
                    "candidate_revision": candidate_revision,
                    "kind": "review-aggregate",
                }
            ),
            "findings": copy.deepcopy(review["findings"]),
        }

    def _compile_review_plan(self, fixture_id: str, value: Any) -> dict[str, Any]:
        self._accepted_ref(
            "agent-workflows/src/ai_agent_workflow/execution_v2.py", "python-source/v1"
        )
        self._accepted_ref(
            "agent-workflows/src/ai_agent_workflow/execution_group.py",
            "python-source/v1",
        )
        plan = _exact(
            value,
            {"budget", "rounds", "fix_waves", "trace"},
            "review plan is malformed",
        )
        budget = _exact(
            plan["budget"],
            {"maximum_review_rounds", "maximum_fix_waves"},
            "shared review/fix budget is malformed",
        )
        _require(
            all(
                isinstance(item, int) and not isinstance(item, bool) and item > 0
                for item in budget.values()
            ),
            "shared review/fix budget is not finite and positive",
        )
        rounds = plan["rounds"]
        fixes = plan["fix_waves"]
        _require(
            isinstance(rounds, list)
            and rounds
            and isinstance(fixes, list)
            and len(rounds) <= budget["maximum_review_rounds"]
            and len(fixes) <= budget["maximum_fix_waves"],
            "shared review/fix budget is exhausted",
        )
        compiled_rounds: list[dict[str, Any]] = []
        expected_trace: list[dict[str, Any]] = []
        all_epochs: set[str] = set()
        required_by_round: dict[int, list[str]] = {}
        for position, round_value in enumerate(rounds, 1):
            item = _exact(
                round_value,
                {
                    "round",
                    "candidate_revision",
                    "reviews",
                    "dispositions",
                    "expected_route",
                },
                "review round is malformed",
            )
            _require(item["round"] == position, "review rounds are stale or reordered")
            _require(
                isinstance(item["candidate_revision"], int)
                and not isinstance(item["candidate_revision"], bool)
                and item["candidate_revision"]
                == len([fix for fix in fixes if fix.get("after_round", 0) < position]),
                "review candidate revision skipped a required fix",
            )
            reviews = item["reviews"]
            _require(
                isinstance(reviews, list) and len(reviews) == 2,
                "exactly two reviews are required",
            )
            normalized_reviews: list[dict[str, Any]] = []
            for axis, review_value in zip(_AXES, reviews):
                review = _exact(
                    review_value,
                    {"axis", "actor_id", "context_epoch_id", "findings"},
                    "review input is malformed",
                )
                _require(review["axis"] == axis, "review axes are missing or reordered")
                _identifier(review["actor_id"], "review actor is missing")
                epoch = _identifier(
                    review["context_epoch_id"], "review Epoch is missing"
                )
                _require(epoch not in all_epochs, "fresh rereview Epoch was not used")
                all_epochs.add(epoch)
                normalized_reviews.append(
                    self._review_report(
                        fixture_id, position, item["candidate_revision"], review
                    )
                )
                expected_trace.append(
                    {"kind": "review", "round": position, "axis": axis}
                )
            try:
                validation = FindingValidator().validate(
                    normalized_reviews,
                    item["dispositions"],
                    {
                        "remaining_seconds": budget["maximum_review_rounds"] - position,
                        "review_round": position,
                        "product_fix_attempt": len(
                            [
                                fix
                                for fix in fixes
                                if fix.get("after_round", 0) < position
                            ]
                        ),
                    },
                )
            except V2ContractError as error:
                raise SourceWideFixtureError(
                    f"review validity/dedup rejected: {error}"
                ) from error
            expected_trace.append({"kind": "finding-validation", "round": position})
            dispositions = validation["dispositions"]
            _require(
                all(
                    entry["classification"] != "required"
                    or entry["materiality"] == "material"
                    for entry in dispositions
                ),
                "a non-material finding cannot enter a required fix",
            )
            _require(
                not any(
                    entry["classification"] == "needs-user" for entry in dispositions
                ),
                "needs-user finding cannot produce a passing source fixture",
            )
            required = sorted(
                entry["fingerprint"]
                for entry in dispositions
                if entry["classification"] == "required"
            )
            required_by_round[position] = required
            route = "fix" if required else "accepted"
            _require(
                item["expected_route"] == route,
                "review route bypasses validated findings",
            )
            compiled_rounds.append(
                {
                    "round": position,
                    "candidate_revision": item["candidate_revision"],
                    "finding_validation": validation,
                    "raw_finding_count": len(validation["source_finding_ids"]),
                    "deduplicated_finding_count": len(dispositions),
                    "required_fingerprints": required,
                    "route": route,
                }
            )
        compiled_fixes: list[dict[str, Any]] = []
        for position, fix_value in enumerate(fixes, 1):
            fix = _exact(
                fix_value,
                {
                    "wave",
                    "after_round",
                    "required_fingerprints",
                    "fresh_rereview_round",
                },
                "fix wave is malformed",
            )
            _require(
                fix["wave"] == position
                and fix["after_round"] < len(rounds)
                and fix["fresh_rereview_round"] == fix["after_round"] + 1
                and fix["required_fingerprints"]
                == required_by_round.get(fix["after_round"])
                and bool(fix["required_fingerprints"]),
                "fix wave bypasses validity, materiality, dedup, or fresh rereview",
            )
            compiled_fixes.append(copy.deepcopy(dict(fix)))
        required_rounds = [
            number for number, values in required_by_round.items() if values
        ]
        _require(
            [fix["after_round"] for fix in compiled_fixes] == required_rounds,
            "required finding path skipped or invented a fix wave",
        )
        _require(
            compiled_rounds[-1]["route"] == "accepted",
            "review plan did not converge within its shared budget",
        )
        for fix in compiled_fixes:
            insert_at = next(
                index + 1
                for index, event in enumerate(expected_trace)
                if event == {"kind": "finding-validation", "round": fix["after_round"]}
            )
            expected_trace.insert(
                insert_at,
                {"kind": "fix", "round": fix["after_round"], "wave": fix["wave"]},
            )
        expected_trace.append({"kind": "converge", "round": len(rounds)})
        _require(
            plan["trace"] == expected_trace,
            "review validity or dependency step was skipped",
        )
        return {
            "shared_budget": copy.deepcopy(dict(budget)),
            "rounds_used": len(rounds),
            "fix_waves_used": len(fixes),
            "rounds": compiled_rounds,
            "fix_waves": compiled_fixes,
            "trace": copy.deepcopy(expected_trace),
            "guard_order": [
                "review-validity",
                "materiality",
                "fingerprint-deduplication",
                "fix-eligibility",
            ],
            "open_required_finding_count": 0,
            "terminal": "source-candidate-accepted",
        }

    @staticmethod
    def _claim_boundary() -> dict[str, bool]:
        return {
            "source_candidate_only": True,
            "current_run_evidence": False,
            "live_runtime_evidence": False,
            "grants_approval": False,
            "objective_outcome_claimed": False,
            "run_completion_claimed": False,
            "full_workflow_ready": False,
        }

    def _validated(self, result: dict[str, Any]) -> dict[str, Any]:
        result["result_digest"] = _canonical_digest(result)
        try:
            validate_document(result, self.schema, self.schema.get("$defs", {}))
        except SchemaValidationError as error:
            raise SourceWideFixtureError(
                f"source-wide result schema rejected: {error}"
            ) from error
        return result

    def compile_profile_execution(self, document: Mapping[str, Any]) -> dict[str, Any]:
        value = _exact(
            document,
            {
                "schema",
                "fixture_id",
                "expected_git_head",
                "profile",
                "workflow_manifest_ref",
                "profile_manifest_ref",
                "claimed_stage_order",
                "claimed_profile_order",
                "review_plan",
            },
            "profile fixture input is malformed",
        )
        _require(
            value["schema"] == "source-wide-profile-fixture-input/v1",
            "profile fixture schema is invalid",
        )
        fixture_id = _identifier(value["fixture_id"], "fixture id is missing")
        _require(
            value["expected_git_head"] == _FIXED_GIT_HEAD, "fixed Git HEAD is stale"
        )
        receipt, profile_manifest, workflow_ref, profile_ref = (
            self._workflow_and_profile(
                value["profile"],
                value["workflow_manifest_ref"],
                value["profile_manifest_ref"],
            )
        )
        workflow, _ = self._load_ref(workflow_ref, accepted=True)
        stage_order = [stage["stage_id"] for stage in workflow["stages"]]
        _require(
            value["claimed_stage_order"] == stage_order,
            "workflow stage order is stale, skipped, or reordered",
        )
        profile_contracts = self._profile_contract_refs(
            value["profile"], profile_manifest, receipt
        )
        profile_order = [item["qualified_id"] for item in profile_contracts]
        _require(
            value["claimed_profile_order"] == profile_order,
            "profile step order is stale, skipped, or reordered",
        )
        result = {
            "schema": "source-wide-fixture-result/v1",
            "result_kind": "profile-source-execution",
            "fixture_id": fixture_id,
            "status": "source-candidate",
            "profile": value["profile"],
            "workflow_id": receipt["workflow_id"],
            "expected_git_head": _FIXED_GIT_HEAD,
            "workflow_manifest_ref": workflow_ref,
            "profile_manifest_ref": profile_ref,
            "s7_surface_manifest_ref": self._validate_s7_acceptance(),
            "source_order": {
                "stage_ids": stage_order,
                "selectors": copy.deepcopy(receipt["selectors"]),
                "profile_selectors": profile_order,
            },
            "common_implementation_refs": self._group_implementation_refs(receipt),
            "profile_contract_refs": profile_contracts,
            "review_execution": self._compile_review_plan(
                fixture_id, value["review_plan"]
            ),
            "claim_boundary": self._claim_boundary(),
        }
        return self._validated(result)

    def compile_cold_resume(self, document: Mapping[str, Any]) -> dict[str, Any]:
        value = _exact(
            document,
            {
                "schema",
                "fixture_id",
                "expected_git_head",
                "checkpoint_ref",
                "observed_head",
                "history_mode",
                "conversation_history",
            },
            "cold resume request is malformed or contains a conversational substitute",
        )
        _require(
            value["schema"] == "source-wide-cold-resume-request/v1",
            "cold resume request schema is invalid",
        )
        _require(
            value["expected_git_head"] == _FIXED_GIT_HEAD, "fixed Git HEAD is stale"
        )
        _require(
            value["history_mode"] == "history-free"
            and value["conversation_history"] is None,
            "conversational history cannot substitute for a physical checkpoint",
        )
        checkpoint, checkpoint_ref = self._load_ref(value["checkpoint_ref"])
        checkpoint = _exact(
            checkpoint,
            {
                "schema",
                "checkpoint_id",
                "expected_git_head",
                "expected_head",
                "profile",
                "workflow_manifest_ref",
                "profile_manifest_ref",
                "group_manifest_refs",
                "completed_stage_ids",
            },
            "cold checkpoint is malformed",
        )
        _require(
            checkpoint["schema"] == "source-wide-cold-checkpoint/v1",
            "cold checkpoint schema is invalid",
        )
        _require(
            checkpoint["expected_git_head"] == _FIXED_GIT_HEAD,
            "cold checkpoint Git HEAD is stale",
        )
        expected_head = _head(checkpoint["expected_head"])
        _require(
            _head(value["observed_head"]) == expected_head,
            "cold resume observed HEAD is stale",
        )
        receipt, profile_manifest, workflow_ref, profile_ref = (
            self._workflow_and_profile(
                checkpoint["profile"],
                checkpoint["workflow_manifest_ref"],
                checkpoint["profile_manifest_ref"],
            )
        )
        self._profile_contract_refs(checkpoint["profile"], profile_manifest, receipt)
        group_refs = self._group_implementation_refs(receipt)
        expected_group_refs = [item["manifest_ref"] for item in group_refs]
        _require(
            checkpoint["group_manifest_refs"] == expected_group_refs,
            "cold checkpoint source refs are stale, skipped, or reordered",
        )
        workflow, _ = self._load_ref(workflow_ref, accepted=True)
        stage_ids = [stage["stage_id"] for stage in workflow["stages"]]
        completed = checkpoint["completed_stage_ids"]
        _require(
            isinstance(completed, list)
            and completed
            and len(completed) < len(stage_ids)
            and completed == stage_ids[: len(completed)],
            "cold checkpoint stage history is stale, skipped, or reordered",
        )
        stage = workflow["stages"][len(completed)]
        result = {
            "schema": "source-wide-fixture-result/v1",
            "result_kind": "cold-source-resume",
            "fixture_id": _identifier(value["fixture_id"], "fixture id is missing"),
            "status": "source-candidate",
            "checkpoint_ref": checkpoint_ref,
            "checkpoint_id": checkpoint["checkpoint_id"],
            "expected_git_head": _FIXED_GIT_HEAD,
            "observed_head": expected_head,
            "history_mode": "history-free",
            "workflow_manifest_ref": workflow_ref,
            "profile_manifest_ref": profile_ref,
            "group_manifest_refs": expected_group_refs,
            "s7_surface_manifest_ref": self._validate_s7_acceptance(),
            "completed_stage_ids": copy.deepcopy(completed),
            "next_action": {
                "action": "compile-source-stage",
                "stage_id": stage["stage_id"],
                "kind": stage["kind"],
                "mode": stage["mode"],
                "selectors": copy.deepcopy(stage["selectors"]),
            },
            "claim_boundary": self._claim_boundary(),
        }
        return self._validated(result)

    def compile_h_skeleton(self, document: Mapping[str, Any]) -> dict[str, Any]:
        value = _exact(
            document,
            {
                "schema",
                "fixture_id",
                "expected_git_head",
                "completion_manifest_ref",
                "lifecycle_interface_ref",
            },
            "Group H skeleton input is malformed",
        )
        _require(
            value["schema"] == "source-wide-h-skeleton-input/v1",
            "Group H skeleton schema is invalid",
        )
        _require(
            value["expected_git_head"] == _FIXED_GIT_HEAD, "fixed Git HEAD is stale"
        )
        manifest, manifest_ref = self._load_ref(
            value["completion_manifest_ref"], accepted=True
        )
        _require(
            manifest_ref
            == self._accepted_ref(_GROUP_PATHS["H"], "group-h-manifest/v1"),
            "Group H completion manifest is not the accepted source",
        )
        interface = _exact(
            value["lifecycle_interface_ref"],
            {"path", "version", "digest"},
            "Group H interface ref is malformed",
        )
        _require(
            interface
            == self._accepted_ref(
                "agent-workflows/src/ai_agent_workflow/decision_outcome_lifecycle.py",
                "python-source/v1",
            ),
            "Group H lifecycle interface is stale",
        )
        selectors = [item.get("qualified_id") for item in manifest.get("contracts", [])]
        _require(
            selectors == ["group.H.H1", "group.H.H2", "group.H.H3"],
            "Group H skeleton is incomplete or reordered",
        )
        for contract in manifest["contracts"]:
            self._verify_declared_digest_ref(contract["source_ref"])
            self._verify_declared_digest_ref(contract["evidence_ref"])
            selector_ref = contract["selector_ref"]
            _require(
                selector_ref["path"] == interface["path"]
                and selector_ref["digest"] == interface["digest"]
                and selector_ref["selector"].endswith(
                    "compile({})".format(contract["qualified_id"])
                ),
                "Group H stable interface binding drifted",
            )
        result = {
            "schema": "source-wide-fixture-result/v1",
            "result_kind": "h1-h2-h3-source-skeleton",
            "fixture_id": _identifier(value["fixture_id"], "fixture id is missing"),
            "status": "source-candidate",
            "expected_git_head": _FIXED_GIT_HEAD,
            "completion_manifest_ref": manifest_ref,
            "lifecycle_interface_ref": copy.deepcopy(dict(interface)),
            "selectors": selectors,
            "source_capabilities": [
                "compile-objective-audit-candidate",
                "compile-run-outcome-validation-candidate",
                "compile-archive-or-continue-candidate",
            ],
            "current_run_gates": {
                "objective_audit_completed": False,
                "run_outcome_decided": False,
                "archive_or_continue_executed": False,
                "live_runtime_observed": False,
                "current_head_advanced": False,
            },
            "cannot_satisfy_current_run_live_gates": True,
            "claim_boundary": self._claim_boundary(),
        }
        return self._validated(result)

    def compile_fixture(self, document: Mapping[str, Any]) -> dict[str, Any]:
        schema = document.get("schema") if isinstance(document, Mapping) else None
        if schema == "source-wide-profile-fixture-input/v1":
            return self.compile_profile_execution(document)
        if schema == "source-wide-cold-resume-request/v1":
            return self.compile_cold_resume(document)
        if schema == "source-wide-h-skeleton-input/v1":
            return self.compile_h_skeleton(document)
        raise SourceWideFixtureError("unknown source-wide fixture input schema")


__all__ = ["SourceWideFixtureError", "SourceWideFixturesV1"]
