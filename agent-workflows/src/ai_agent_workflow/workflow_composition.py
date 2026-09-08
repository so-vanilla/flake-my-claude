"""Deterministic, source-only validation for ``workflow-manifest/v1``."""

from __future__ import annotations

import hashlib
import json
from collections.abc import Iterable, Mapping
from pathlib import Path
from typing import Any, Optional

from .schema_validation import SchemaValidationError, validate_document


class WorkflowCompositionError(ValueError):
    """A workflow composition is ambiguous, incomplete, or unsafe."""


_DEFAULT_SOURCE_ROOT = Path(__file__).resolve().parents[3]
_COMMON_SCOPES = ("A", "B", "C", "D", "E", "G", "H")
_CATALOG_GROUPS = tuple("ABCDEFGH")
_PROFILES = ("feature", "bug-fix", "improvement")
_CLOSURE = tuple("group.F.F%d" % number for number in range(1, 8))
_RESUME = "group.F.F8"
_EXCEPTION = "group.E.E10"
_PROFILE_HOSTS = {
    "feature": {
        "F1": "group.D.D4", "F2": "group.D.D4", "F3": "group.D.D4",
        "F4": "group.D.D6", "F5": "group.E.E3", "F6": "group.E.E3",
        "F7": "group.E.E3", "F8": "group.E.E9",
    },
    "bug-fix": {
        "BGF1": "group.D.D2", "BGF2": "group.D.D2", "BGF3": "group.D.D2",
        "BGF4": "group.D.D2", "BGF5": "group.D.D2", "BGF6": "group.D.D5",
        "BGF7": "group.E.E3", "BGF8": "group.E.E9",
    },
    "improvement": {
        "I1": "group.D.D2", "I2": "group.D.D2", "I3": "group.D.D2",
        "I4": "group.D.D6", "I5": "group.E.E3", "I6": "group.E.E9",
        "I7": "group.E.E9",
    },
}

_PROFILE_GATES = {
    "feature": "design-approval",
    "bug-fix": "fix-option-approval",
    "improvement": "improvement-adoption-approval",
}


def _normal_contract(
    intent_class: str,
    *,
    profile: Optional[str] = None,
    protected: bool = False,
    planned_effect: str = "local-change",
    execute: bool = True,
    gates: tuple[str, ...] = (),
) -> dict[str, Any]:
    required = {
        "objective-approval",
        "decision-promotion-approval",
        "run-outcome-approval",
        *gates,
    }
    if profile is not None:
        required.add(_PROFILE_GATES[profile])
    if protected:
        required.add("risk-acceptance")
        if execute:
            required.add("execution-approval")
    scopes = ("B", "C", "D") + (("E",) if execute else ()) + ("G", "H")
    return {
        "intent_class": intent_class,
        "entry_mode": "new",
        "profile": profile,
        "risk_class": "protected" if protected else "standard",
        "planned_effect": planned_effect,
        "scopes": scopes,
        "gates": frozenset(required),
    }


_WORKFLOW_CONTRACTS = {
    "bootstrap-new-workflow": {
        "intent_class": "bootstrap", "entry_mode": "bootstrap", "profile": None,
        "risk_class": "protected", "planned_effect": "migration", "scopes": ("A",),
        "gates": frozenset({"risk-acceptance", "migration-approval"}),
    },
    "feature-bounded": _normal_contract("feature", profile="feature"),
    "feature-architectural": _normal_contract("feature", profile="feature", protected=True),
    "bug-fix-standard": _normal_contract("bug-fix", profile="bug-fix"),
    "bug-fix-hotfix": _normal_contract("bug-fix", profile="bug-fix", protected=True),
    "improvement-measured": _normal_contract("improvement", profile="improvement"),
    "research-discovery": _normal_contract("research", planned_effect="none", execute=False),
    "documentation-change": _normal_contract("documentation"),
    "refactor-behavior-preserving": _normal_contract("refactor"),
    "security-sensitive-change": _normal_contract("security", protected=True),
    "configuration-dotfiles-source": _normal_contract(
        "configuration", protected=True, gates=("dotfiles-ownership-approval",)
    ),
    "migration-cutover-plan": _normal_contract(
        "migration", protected=True, planned_effect="migration", execute=False,
        gates=("migration-approval",),
    ),
    "incident-response": _normal_contract(
        "incident", profile="bug-fix", protected=True, gates=("incident-authority",)
    ),
    "decision-consolidation": _normal_contract("decision", planned_effect="none", execute=False),
    "external-operation-plan": _normal_contract(
        "external-operation", protected=True, planned_effect="external-operation", execute=False,
        gates=("external-operation-approval",),
    ),
    "resume-interrupted-work": {
        "intent_class": "resume", "entry_mode": "resume", "profile": None,
        "risk_class": "standard", "planned_effect": "none", "scopes": (),
        "gates": frozenset(),
    },
    "execution-exception-arbitration": {
        "intent_class": "execution-exception", "entry_mode": "exception-resume", "profile": None,
        "risk_class": "protected", "planned_effect": "none", "scopes": (),
        "gates": frozenset({"risk-acceptance", "replacement-budget-approval"}),
    },
    "company-governed-change": _normal_contract(
        "company-change", protected=True, gates=("company-policy-approval",)
    ),
}
_WORKFLOW_IDS = tuple(_WORKFLOW_CONTRACTS)


def _read_json(path: Path, label: str) -> Any:
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise WorkflowCompositionError("cannot read %s %s: %s" % (label, path, error)) from error


def _digest(value: Any) -> str:
    payload = json.dumps(
        value, sort_keys=True, separators=(",", ":"), ensure_ascii=True
    ).encode("utf-8")
    return "sha256:" + hashlib.sha256(payload).hexdigest()


def _bytes_digest(value: bytes) -> str:
    """Return the digest of physical canonical source bytes."""

    return "sha256:" + hashlib.sha256(value).hexdigest()


class WorkflowCompositionV1:
    """Validate public workflow manifests against the canonical catalog contract."""

    def __init__(
        self,
        *,
        source_root: Optional[Path] = None,
        catalog_path: Optional[Path] = None,
        schema_path: Optional[Path] = None,
    ) -> None:
        self.source_root = Path(source_root or _DEFAULT_SOURCE_ROOT).resolve()
        package = self.source_root / "agent-workflows"
        self.catalog_path = Path(catalog_path or package / "catalog.yaml").resolve()
        self.schema_path = Path(
            schema_path or package / "schemas" / "workflow-manifest-v1.schema.json"
        ).resolve()
        self.execution_policy_schema_path = package / "schemas" / "workflow-execution-policy-v1.schema.json"
        catalog_schema_path = package / "schemas" / "catalog-v1.schema.json"
        self.catalog = _read_json(self.catalog_path, "catalog")
        self.schema = _read_json(self.schema_path, "workflow schema")
        self.execution_policy_schema = _read_json(
            self.execution_policy_schema_path, "workflow execution policy schema"
        )
        catalog_schema = _read_json(catalog_schema_path, "catalog schema")
        try:
            validate_document(self.catalog, catalog_schema)
        except SchemaValidationError as error:
            raise WorkflowCompositionError("catalog schema: %s" % error) from error
        if tuple(self.catalog["named_contracts"]) != _CATALOG_GROUPS:
            raise WorkflowCompositionError("catalog must contain canonical groups A-H in order")
        if tuple(self.catalog["profile_steps"]) != _PROFILES:
            raise WorkflowCompositionError("catalog must contain canonical profiles in order")
        self.common = {
            group: tuple("group.%s.%s" % (group, item[0]) for item in entries)
            for group, entries in self.catalog["named_contracts"].items()
        }
        self.profiles = {
            profile: tuple("profile.%s.%s" % (profile, item[0]) for item in entries)
            for profile, entries in self.catalog["profile_steps"].items()
        }
        self.targets = frozenset(
            selector
            for selectors in (*self.common.values(), *self.profiles.values())
            for selector in selectors
        )
        if len(self.targets) != sum(map(len, self.common.values())) + sum(map(len, self.profiles.values())):
            raise WorkflowCompositionError("catalog contains duplicate qualified selector identities")
        for profile, selectors in self.profiles.items():
            expected = {selector.rsplit(".", 1)[-1] for selector in selectors}
            if set(_PROFILE_HOSTS[profile]) != expected:
                raise WorkflowCompositionError("profile host contract does not match catalog for %s" % profile)
        self.catalog_digest = _digest(self.catalog)

    def _expected_common(self, scope: str) -> tuple[str, ...]:
        selectors = self.common[scope]
        return selectors[:-1] if scope == "E" else selectors

    @staticmethod
    def _require_gate(document: Mapping[str, Any], gate: str, reason: str) -> None:
        if gate not in document["required_human_gates"]:
            raise WorkflowCompositionError("%s requires %s" % (reason, gate))

    def _compile_execution_policy(self, document: Mapping[str, Any]) -> dict[str, Any]:
        """Resolve the normal-E policy source and compile its guarded transitions."""

        reference = document.get("execution_policy")
        if not isinstance(reference, Mapping):
            raise WorkflowCompositionError("normal E requires a digest-bound execution_policy")
        package = (self.source_root / "agent-workflows").resolve()
        relative_path = Path(reference["path"])
        if relative_path.is_absolute():
            raise WorkflowCompositionError("execution policy path must be source-relative")
        policy_path = (self.source_root / relative_path).resolve()
        try:
            policy_path.relative_to(package)
        except ValueError as error:
            raise WorkflowCompositionError("execution policy path escapes agent-workflows") from error
        try:
            policy_bytes = policy_path.read_bytes()
            policy = json.loads(policy_bytes.decode("utf-8"))
        except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
            raise WorkflowCompositionError("cannot read execution policy %s: %s" % (policy_path, error)) from error
        actual_digest = _bytes_digest(policy_bytes)
        if reference["digest"] != actual_digest:
            raise WorkflowCompositionError("execution policy digest does not match physical source bytes")
        try:
            validate_document(
                policy,
                self.execution_policy_schema,
                registry=dict(self.execution_policy_schema.get("$defs", {})),
            )
        except SchemaValidationError as error:
            raise WorkflowCompositionError("execution policy schema: %s" % error) from error
        if reference["version"] != policy["version"]:
            raise WorkflowCompositionError("execution policy reference version does not match source")

        review_join = policy["review_join"]
        if tuple(review_join["selectors"]) != self.common["E"][3:5]:
            raise WorkflowCompositionError("execution policy must exact-join E4 and E5")
        routes = {route["disposition"]: route for route in policy["validation"]["routes"]}
        if set(routes) != {"no-open-required", "required", "needs-user-or-unresolved"}:
            raise WorkflowCompositionError("execution policy must classify every required-feedback disposition")
        no_open = routes["no-open-required"]
        required = routes["required"]
        blocked = routes["needs-user-or-unresolved"]
        if (no_open["next_selector"], no_open["terminal_status"], no_open["pass"]) != (
            "group.E.E8", None, False
        ):
            raise WorkflowCompositionError("no-open-required must continue from E6 to E8")
        if (required["next_selector"], required["terminal_status"], required["pass"]) != (
            "group.E.E7", None, False
        ):
            raise WorkflowCompositionError("required must enter guarded E7 feedback")
        if (blocked["next_selector"], blocked["terminal_status"], blocked["pass"]) != (
            None, "needs-user-or-unresolved", False
        ):
            raise WorkflowCompositionError("needs-user or unresolved must terminate without pass")
        resolution = policy["resolution"]
        if (
            resolution["selector"],
            resolution["requires_disposition"],
            resolution["fresh_rereview"],
            resolution["return_to_selector"],
        ) != ("group.E.E7", "required", True, "group.E.E6"):
            raise WorkflowCompositionError("E7 must require a finding and return via fresh E6 validation")
        convergence = policy["convergence"]
        if (
            convergence["entry_selector"],
            convergence["next_selector"],
            convergence["new_candidate_selector"],
            convergence["accepted_terminal"],
        ) != ("group.E.E8", "group.E.E9", "group.E.E6", "accepted"):
            raise WorkflowCompositionError("execution policy convergence routing is invalid")
        if tuple(convergence["blocked_by_open"]) != ("required", "needs-user", "unresolved"):
            raise WorkflowCompositionError("E8/E9 must be blocked by every open required state")
        budget = policy["budget_terminal"]
        if (budget["status"], budget["non_dispatch"], budget["pass"]) != (
            "stopped-budget", True, False
        ):
            raise WorkflowCompositionError("finite budget terminal must be non-dispatch and never pass")

        blocked_by_open = list(convergence["blocked_by_open"])
        return {
            "policy_id": policy["policy_id"],
            "version": policy["version"],
            "path": reference["path"],
            "digest": actual_digest,
            "compiled_conditional_transitions": [
                {"from": list(review_join["selectors"]), "when": review_join["join"], "to": review_join["next_selector"]},
                {"from": "group.E.E6", "when": "no-open-required-needs-user-or-unresolved", "to": no_open["next_selector"]},
                {"from": "group.E.E6", "when": "open-required-without-needs-user-or-unresolved", "to": required["next_selector"]},
                {"from": "group.E.E6", "when": "needs-user-or-unresolved", "terminal": blocked["terminal_status"], "pass": blocked["pass"]},
                {"from": "group.E.E7", "when": "required-resolution-claim-and-fresh-rereview", "to": resolution["return_to_selector"], "fresh": True},
                {"from": "group.E.E8", "when": "no-open-" + "-or-".join(blocked_by_open), "to": convergence["next_selector"]},
                {"from": "group.E.E9", "when": "new-candidate", "to": convergence["new_candidate_selector"], "fresh": True},
                {"from": "group.E.E9", "when": "no-open-" + "-or-".join(blocked_by_open), "terminal": convergence["accepted_terminal"], "pass": True},
                {"from": "any-active-E-state", "when": "finite-budget-exhausted", "terminal": budget["status"], "non_dispatch": budget["non_dispatch"], "pass": budget["pass"]},
            ],
        }

    def validate(self, document: Mapping[str, Any]) -> dict[str, Any]:
        """Return a deterministic receipt, or fail before any live effect."""

        try:
            validate_document(document, self.schema, registry=dict(self.schema.get("$defs", {})))
        except SchemaValidationError as error:
            raise WorkflowCompositionError("workflow schema: %s" % error) from error
        stages = document["stages"]
        if document["workflow_id"] not in _WORKFLOW_IDS:
            raise WorkflowCompositionError("workflow_id is not in the canonical composition contract")
        contract = _WORKFLOW_CONTRACTS[document["workflow_id"]]
        for field in ("intent_class", "entry_mode", "profile", "risk_class", "planned_effect"):
            if document[field] != contract[field]:
                raise WorkflowCompositionError(
                    "%s must bind %s=%r" % (document["workflow_id"], field, contract[field])
                )
        actual_gates = frozenset(document["required_human_gates"])
        if actual_gates != contract["gates"]:
            raise WorkflowCompositionError(
                "%s human gates differ from its canonical contract (missing=%s, extra=%s)"
                % (
                    document["workflow_id"],
                    sorted(contract["gates"].difference(actual_gates)),
                    sorted(actual_gates.difference(contract["gates"])),
                )
            )
        stage_ids = [stage["stage_id"] for stage in stages]
        if len(stage_ids) != len(set(stage_ids)):
            raise WorkflowCompositionError("stage_id values must be unique")

        entry_mode = document["entry_mode"]
        profile = document["profile"]
        common_seen: set[str] = set()
        selector_positions: dict[str, int] = {}
        common_values: dict[str, list[str]] = {scope: [] for scope in _COMMON_SCOPES}
        common_stages: dict[str, list[Mapping[str, Any]]] = {scope: [] for scope in _COMMON_SCOPES}
        profile_values: list[str] = []
        selected: list[str] = []
        closed_scopes: list[str] = []
        open_scope: Optional[str] = None
        previous_scope_index = -1
        position = 0

        for stage in stages:
            selectors = stage["selectors"]
            for selector in selectors:
                if selector not in self.targets:
                    raise WorkflowCompositionError("unknown catalog selector %s" % selector)
                selected.append(selector)
                selector_positions.setdefault(selector, position)
                position += 1
            kind = stage["kind"]
            scope = stage["scope"]
            host = stage["host_selector"]

            if kind == "common":
                if scope not in _COMMON_SCOPES:
                    raise WorkflowCompositionError("common stage has invalid scope %s" % scope)
                if host is not None or any(not value.startswith("group.%s." % scope) for value in selectors):
                    raise WorkflowCompositionError("common stage selectors must match scope and have no host")
                if _EXCEPTION in selectors:
                    raise WorkflowCompositionError("E10 is exception-only")
                if scope != "E" and stage["mode"] != "serial":
                    raise WorkflowCompositionError("common Group %s must remain serial" % scope)
                if open_scope is None:
                    scope_index = _COMMON_SCOPES.index(scope)
                    if scope_index <= previous_scope_index:
                        raise WorkflowCompositionError("common groups are out of canonical order")
                    open_scope = scope
                    previous_scope_index = scope_index
                elif open_scope != scope:
                    raise WorkflowCompositionError("common group %s lacks exactly one closure" % open_scope)
                common_values[scope].extend(selectors)
                common_stages[scope].append(stage)
                for selector in selectors:
                    if selector in common_seen:
                        raise WorkflowCompositionError("duplicate common selector %s" % selector)
                    common_seen.add(selector)
            elif kind == "profile":
                if profile is None or scope != profile:
                    raise WorkflowCompositionError("profile stage does not match manifest profile")
                if open_scope is None:
                    raise WorkflowCompositionError("profile stage must be inside its host common group")
                expected_hosts = {
                    _PROFILE_HOSTS[profile].get(selector.rsplit(".", 1)[-1])
                    for selector in selectors
                }
                if None in expected_hosts or expected_hosts != {host}:
                    raise WorkflowCompositionError("profile stage has incorrect host binding")
                if host not in common_seen or host.split(".")[1] != open_scope:
                    raise WorkflowCompositionError("profile host must be a prior selector in the open common group")
                if not common_values[open_scope] or common_values[open_scope][-1] != host:
                    raise WorkflowCompositionError("profile stage moved beyond its host boundary")
                if "group.D.D1" not in common_seen:
                    raise WorkflowCompositionError("D1 must precede every profile step")
                if stage["mode"] != "serial":
                    raise WorkflowCompositionError("ordered profile stages must be serial")
                profile_values.extend(selectors)
            elif kind == "closure":
                if scope != "shared-closure" or host is not None or stage["mode"] != "serial":
                    raise WorkflowCompositionError("closure stage shape is invalid")
                if tuple(selectors) != _CLOSURE:
                    raise WorkflowCompositionError("closure must contain exact F1-F7 order")
                if open_scope is None:
                    raise WorkflowCompositionError("closure has no completed common group")
                if tuple(common_values[open_scope]) != self._expected_common(open_scope):
                    raise WorkflowCompositionError("common group %s is incomplete or out of order" % open_scope)
                closed_scopes.append(open_scope)
                open_scope = None
            elif kind == "resume":
                if entry_mode not in ("resume", "exception-resume"):
                    raise WorkflowCompositionError("F8 is restricted to an explicit resume entry")
                if scope != "resume" or host is not None or stage["mode"] != "serial" or selectors != [_RESUME]:
                    raise WorkflowCompositionError("resume stage must be resume-only F8")
                if open_scope is not None:
                    raise WorkflowCompositionError("F8 never closes a common group")
            elif kind == "exception":
                if entry_mode != "exception-resume":
                    raise WorkflowCompositionError("E10 is restricted to the explicit exception-resume workflow")
                if scope != "execution-exception" or host != _RESUME or stage["mode"] != "serial" or selectors != [_EXCEPTION]:
                    raise WorkflowCompositionError("exception stage must bind E10 to prior F8")
                if open_scope is not None:
                    raise WorkflowCompositionError("E10 cannot appear in a common group")
                if _RESUME not in selector_positions or selector_positions[_RESUME] > selector_positions[_EXCEPTION]:
                    raise WorkflowCompositionError("E10 requires an actually prior F8 selector")
            else:
                raise WorkflowCompositionError("unsupported stage kind %s" % kind)

        if open_scope is not None:
            raise WorkflowCompositionError("common group %s lacks exactly one closure" % open_scope)
        present_scopes = [scope for scope in _COMMON_SCOPES if common_values[scope]]
        if closed_scopes != present_scopes:
            raise WorkflowCompositionError("every common group requires exactly one ordered closure")
        if tuple(present_scopes) != contract["scopes"]:
            raise WorkflowCompositionError(
                "%s common scopes must be %s" % (document["workflow_id"], contract["scopes"])
            )
        if profile is None:
            if profile_values:
                raise WorkflowCompositionError("profile selectors require a manifest profile")
        elif tuple(profile_values) != self.profiles[profile]:
            raise WorkflowCompositionError("profile selectors are incomplete or out of canonical order")

        if entry_mode == "new":
            first = stages[0]
            if first["kind"] != "common" or first["scope"] != "B" or first["mode"] != "serial" or tuple(first["selectors"]) != self.common["B"]:
                raise WorkflowCompositionError("new workflow must start with complete serial B1-B7")
            self._require_gate(document, "objective-approval", "new workflow")
        elif entry_mode == "bootstrap":
            if document["workflow_id"] != "bootstrap-new-workflow" or document["intent_class"] != "bootstrap":
                raise WorkflowCompositionError("bootstrap entry is reserved for bootstrap-new-workflow")
            if len(stages) != 2 or [stage["kind"] for stage in stages] != ["common", "closure"] or present_scopes != ["A"]:
                raise WorkflowCompositionError("bootstrap workflow must be exact Group A plus closure")
            if stages[0]["mode"] != "serial" or tuple(stages[0]["selectors"]) != self.common["A"]:
                raise WorkflowCompositionError("bootstrap workflow requires exact canonical A1-A7 sequence")
        elif entry_mode == "resume":
            if document["workflow_id"] != "resume-interrupted-work" or document["intent_class"] != "resume":
                raise WorkflowCompositionError("resume entry is reserved for resume-interrupted-work")
            if len(stages) != 1 or stages[0]["kind"] != "resume":
                raise WorkflowCompositionError("resume workflow must contain only F8")
            if document["completion"] != {"mode": "dynamic", "terminal_selector": None}:
                raise WorkflowCompositionError("resume-only F8 requires dynamic completion")
        elif entry_mode == "exception-resume":
            if document["workflow_id"] != "execution-exception-arbitration" or document["intent_class"] != "execution-exception":
                raise WorkflowCompositionError("exception entry is reserved for execution-exception-arbitration")
            if len(stages) != 2 or [stage["kind"] for stage in stages] != ["resume", "exception"]:
                raise WorkflowCompositionError("exception workflow must be F8 then E10")
            if document["completion"] != {"mode": "advisory-stop", "terminal_selector": _EXCEPTION}:
                raise WorkflowCompositionError("exception workflow must stop advisably at E10")
            self._require_gate(document, "replacement-budget-approval", "exception workflow")

        if "group.A.A7" in selector_positions:
            if "group.A.A6R" not in selector_positions or selector_positions["group.A.A6R"] > selector_positions["group.A.A7"]:
                raise WorkflowCompositionError("A7 requires prior A6R")
            self._require_gate(document, "migration-approval", "A7")
        execution_policy: Optional[dict[str, Any]] = None
        if common_values["E"]:
            if "group.D.D12" not in selector_positions or selector_positions["group.D.D12"] > selector_positions["group.E.E1"]:
                raise WorkflowCompositionError("D12 must precede E1")
            expected_e_stages = (
                ("serial", self.common["E"][:3]),
                ("parallel", self.common["E"][3:5]),
                ("serial", self.common["E"][5:9]),
            )
            actual_e_stages = tuple(
                (stage["mode"], tuple(stage["selectors"])) for stage in common_stages["E"]
            )
            if actual_e_stages != expected_e_stages:
                raise WorkflowCompositionError("normal E must be E1-E3, parallel E4/E5, then E6-E9")
            execution_policy = self._compile_execution_policy(document)
        elif "execution_policy" in document:
            raise WorkflowCompositionError("workflows without normal E must not claim an execution policy")
        if common_values["G"]:
            self._require_gate(document, "decision-promotion-approval", "G5/G6")
        if common_values["H"]:
            self._require_gate(document, "run-outcome-approval", "H2")
        if document["planned_effect"] == "migration":
            self._require_gate(document, "migration-approval", "planned migration")
        if document["planned_effect"] == "external-operation":
            self._require_gate(document, "external-operation-approval", "planned external operation")
        if document["risk_class"] == "protected":
            self._require_gate(document, "risk-acceptance", "protected workflow")
        if document["source_only"] is not True:
            raise WorkflowCompositionError("workflow must remain source_only")

        if entry_mode in ("new", "bootstrap"):
            if document["completion"] != {"mode": "closed", "terminal_selector": "group.F.F7"}:
                raise WorkflowCompositionError("common workflow must close at F7")

        receipt = {
            "schema": "workflow-composition-validation/v1",
            "result": "passed",
            "workflow_id": document["workflow_id"],
            "manifest_digest": _digest(document),
            "catalog_digest": self.catalog_digest,
            "entry_mode": entry_mode,
            "intent_class": document["intent_class"],
            "profile": profile,
            "risk_class": document["risk_class"],
            "planned_effect": document["planned_effect"],
            "common_scopes": present_scopes,
            "required_human_gates": sorted(actual_gates),
            "stage_count": len(stages),
            "selector_count": len(selected),
            "closure_count": len(closed_scopes),
            "selectors": selected,
            "source_only": True,
        }
        if execution_policy is not None:
            receipt["execution_policy"] = execution_policy
        return receipt

    def validate_file(self, path: Path) -> dict[str, Any]:
        document = _read_json(Path(path), "workflow manifest")
        receipt = self.validate(document)
        if Path(path).stem != receipt["workflow_id"]:
            raise WorkflowCompositionError("manifest filename must match workflow_id")
        return receipt

    def validate_suite(self, manifests: Iterable[Mapping[str, Any]]) -> dict[str, Any]:
        receipts = sorted((self.validate(item) for item in manifests), key=lambda item: item["workflow_id"])
        workflow_ids = [item["workflow_id"] for item in receipts]
        if len(workflow_ids) != len(set(workflow_ids)):
            raise WorkflowCompositionError("workflow_id values must be unique in a suite")
        content = {"catalog_digest": self.catalog_digest, "receipts": receipts}
        return {
            "schema": "workflow-composition-suite-validation/v1",
            "result": "passed",
            "catalog_digest": self.catalog_digest,
            "manifest_count": len(receipts),
            "workflow_ids": workflow_ids,
            "receipts": receipts,
            "suite_digest": _digest(content),
        }

    def validate_directory(self, path: Optional[Path] = None) -> dict[str, Any]:
        directory = Path(path or self.source_root / "agent-workflows" / "workflows")
        files = sorted(directory.glob("*.json"))
        if not files:
            raise WorkflowCompositionError("workflow directory contains no JSON manifests")
        receipts = [self.validate_file(path) for path in files]
        workflow_ids = [item["workflow_id"] for item in receipts]
        if len(workflow_ids) != len(set(workflow_ids)):
            raise WorkflowCompositionError("workflow_id values must be unique in a directory")
        if set(workflow_ids) != set(_WORKFLOW_IDS):
            missing = sorted(set(_WORKFLOW_IDS).difference(workflow_ids))
            unknown = sorted(set(workflow_ids).difference(_WORKFLOW_IDS))
            raise WorkflowCompositionError(
                "workflow directory is incomplete or unknown (missing=%s, unknown=%s)"
                % (missing, unknown)
            )
        content = {"catalog_digest": self.catalog_digest, "receipts": receipts}
        return {
            "schema": "workflow-composition-suite-validation/v1",
            "result": "passed",
            "catalog_digest": self.catalog_digest,
            "manifest_count": len(receipts),
            "workflow_ids": sorted(workflow_ids),
            "receipts": sorted(receipts, key=lambda item: item["workflow_id"]),
            "suite_digest": _digest(content),
        }


def validate_workflow_document(document: Mapping[str, Any], *, source_root: Optional[Path] = None) -> dict[str, Any]:
    return WorkflowCompositionV1(source_root=source_root).validate(document)


def validate_workflow_file(path: Path, *, source_root: Optional[Path] = None) -> dict[str, Any]:
    return WorkflowCompositionV1(source_root=source_root).validate_file(path)


def validate_workflow_directory(*, source_root: Optional[Path] = None, path: Optional[Path] = None) -> dict[str, Any]:
    return WorkflowCompositionV1(source_root=source_root).validate_directory(path)


__all__ = [
    "WorkflowCompositionError", "WorkflowCompositionV1", "validate_workflow_directory",
    "validate_workflow_document", "validate_workflow_file",
]
