"""S0's deterministic, standard-library registry compiler."""

from __future__ import annotations

import ast
import hashlib
import json
import re
from pathlib import Path
from typing import Any, Mapping, Optional

from .schema_validation import SchemaValidationError, validate_document


class CatalogError(ValueError):
    """A catalog input is ambiguous, stale, incomplete, or otherwise unsafe."""


_ROOT = Path(__file__).resolve().parents[2]
_KINDS = {
    "skill",
    "shared-protocol-operation",
    "workflow-profile-step",
    "composed-on-demand-operation",
}
_GROUPS = frozenset("ABCDEFGH")
_PROFILES = frozenset(("feature", "bug-fix", "improvement"))
_SURFACE_BINDINGS = (
    {
        "id": "surface.model-policy",
        "canonical_source": "agent-workflows/manifests/model-policy.json",
        "canonical_source_digest": "sha256:eb7e45733239472c05158bc4611bb120a1fa60930a075ef08f6b0003c5060b50",
        "owner_module": "agent-workflows",
        "section_owner": "S7",
        "interface_ref": "effective-model receipt",
        "acceptance_selector": "model-policy",
        "evidence": "agent-workflows/evidence/surfaces/personal/model-policy.json",
        "test_module": "agent-workflows/tests/test_s7_personal_surfaces.py",
        "test_selector": "S7PersonalSurfaceTests.test_model_policy_surface",
    },
    {
        "id": "surface.config-installer-guardrail",
        "canonical_source": "agent-workflows/config-installer.py",
        "canonical_source_digest": "sha256:e3b06c6768755654d119d8c60d17ac96ed3f005bb84d47be033064f42c3a9f17",
        "owner_module": "agent-workflows",
        "section_owner": "S7",
        "interface_ref": "managed config merge",
        "acceptance_selector": "config-installer-guardrail",
        "evidence": "agent-workflows/evidence/surfaces/personal/config-installer-guardrail.json",
        "test_module": "agent-workflows/tests/test_s7_personal_surfaces.py",
        "test_selector": "S7PersonalSurfaceTests.test_config_installer_surface",
    },
    {
        "id": "surface.owner-distribution-manifest",
        "canonical_source": "agent-workflows/manifests/distribution.json",
        "canonical_source_digest": "sha256:2c15773ddc924ac721dae4ec077fa77462b2779f34037a5034fe810d8fed2ee1",
        "owner_module": "agent-workflows",
        "section_owner": "S7",
        "interface_ref": "owner manifest",
        "acceptance_selector": "owner-distribution-manifest",
        "evidence": "agent-workflows/evidence/surfaces/personal/owner-distribution-manifest.json",
        "test_module": "agent-workflows/tests/test_s7_personal_surfaces.py",
        "test_selector": "S7PersonalSurfaceTests.test_distribution_manifest_surface",
    },
    {
        "id": "surface.provider-projection",
        "canonical_source": "agent-workflows/provider-projection.py",
        "canonical_source_digest": "sha256:cc918dd759d4a5c33a5aaab2239646fc9f507bca1a34de7f3095011bd4890504",
        "owner_module": "agent-workflows",
        "section_owner": "S7",
        "interface_ref": "provider projection",
        "acceptance_selector": "provider-projection",
        "evidence": "agent-workflows/evidence/surfaces/personal/provider-projection.json",
        "test_module": "agent-workflows/tests/test_s7_personal_surfaces.py",
        "test_selector": "S7PersonalSurfaceTests.test_provider_projection_surface",
    },
    {
        "id": "surface.company.classify-data",
        "canonical_source": "agent-workflows/company/classify-data.py",
        "canonical_source_digest": "sha256:96d3f1bea4cc0a8c274a806682fb371af929b5b341b3da3cf019cfc61111e12b",
        "owner_module": "agent-workflows",
        "section_owner": "S7",
        "interface_ref": "data policy",
        "acceptance_selector": "classify-data",
        "evidence": "agent-workflows/evidence/surfaces/company/classify-data.json",
        "test_module": "agent-workflows/tests/test_company_governance.py",
        "test_selector": "CompanyGovernanceTests.test_all_seven_compile_in_exact_order_as_static_non_authorizing_candidates",
    },
    {
        "id": "surface.company.resolve-identity",
        "canonical_source": "agent-workflows/company/resolve-identity.py",
        "canonical_source_digest": "sha256:41370c752859f9ed5262123ffcee59ebc3d89de9578c9a88473529da1b730b38",
        "owner_module": "agent-workflows",
        "section_owner": "S7",
        "interface_ref": "identity receipt",
        "acceptance_selector": "resolve-identity",
        "evidence": "agent-workflows/evidence/surfaces/company/resolve-identity.json",
        "test_module": "agent-workflows/tests/test_company_governance.py",
        "test_selector": "CompanyGovernanceTests.test_all_seven_compile_in_exact_order_as_static_non_authorizing_candidates",
    },
    {
        "id": "surface.company.authorize-tools",
        "canonical_source": "agent-workflows/company/authorize-tools.py",
        "canonical_source_digest": "sha256:2529c3fa8591c1e298ab68ce1a080fc637df871a3e9af16858b5ba9783715e2a",
        "owner_module": "agent-workflows",
        "section_owner": "S7",
        "interface_ref": "effective policy readback",
        "acceptance_selector": "authorize-tools",
        "evidence": "agent-workflows/evidence/surfaces/company/authorize-tools.json",
        "test_module": "agent-workflows/tests/test_company_governance.py",
        "test_selector": "CompanyGovernanceTests.test_all_seven_compile_in_exact_order_as_static_non_authorizing_candidates",
    },
    {
        "id": "surface.company.approve-catalog",
        "canonical_source": "agent-workflows/company/approve-catalog.py",
        "canonical_source_digest": "sha256:c38fbf8fb5a16f371168c7577490709b8ac76dea2a5dbad4b5c739e2f8429758",
        "owner_module": "agent-workflows",
        "section_owner": "S7",
        "interface_ref": "approved manifest",
        "acceptance_selector": "approve-catalog",
        "evidence": "agent-workflows/evidence/surfaces/company/approve-catalog.json",
        "test_module": "agent-workflows/tests/test_company_governance.py",
        "test_selector": "CompanyGovernanceTests.test_all_seven_compile_in_exact_order_as_static_non_authorizing_candidates",
    },
    {
        "id": "surface.company.evaluate-change",
        "canonical_source": "agent-workflows/company/evaluate-change.py",
        "canonical_source_digest": "sha256:7760c6b6cdcc48c9c7e8c28fe0bea902e41fb3f95bcac888668cbc98fea72e56",
        "owner_module": "agent-workflows",
        "section_owner": "S7",
        "interface_ref": "comparative eval",
        "acceptance_selector": "evaluate-change",
        "evidence": "agent-workflows/evidence/surfaces/company/evaluate-change.json",
        "test_module": "agent-workflows/tests/test_company_governance.py",
        "test_selector": "CompanyGovernanceTests.test_all_seven_compile_in_exact_order_as_static_non_authorizing_candidates",
    },
    {
        "id": "surface.company.release-workflow",
        "canonical_source": "agent-workflows/company/release-workflow.py",
        "canonical_source_digest": "sha256:4ef93aabfb6ed5f7d518f77ffc3d0f480f1c348abe883b521c9fac5f1bbaa710",
        "owner_module": "agent-workflows",
        "section_owner": "S7",
        "interface_ref": "release receipt",
        "acceptance_selector": "release-workflow",
        "evidence": "agent-workflows/evidence/surfaces/company/release-workflow.json",
        "test_module": "agent-workflows/tests/test_company_governance.py",
        "test_selector": "CompanyGovernanceTests.test_all_seven_compile_in_exact_order_as_static_non_authorizing_candidates",
    },
    {
        "id": "surface.company.audit-operation",
        "canonical_source": "agent-workflows/company/audit-operation.py",
        "canonical_source_digest": "sha256:eb865164e896e4e638d0dd510564b0e8211566b018f7ed59f2035463e158ae00",
        "owner_module": "agent-workflows",
        "section_owner": "S7",
        "interface_ref": "audit report",
        "acceptance_selector": "audit-operation",
        "evidence": "agent-workflows/evidence/surfaces/company/audit-operation.json",
        "test_module": "agent-workflows/tests/test_company_governance.py",
        "test_selector": "CompanyGovernanceTests.test_all_seven_compile_in_exact_order_as_static_non_authorizing_candidates",
    },
)
_SURFACE_BINDINGS_BY_ID = {item["id"]: item for item in _SURFACE_BINDINGS}
_SURFACE_DECLARATION_FIELDS = (
    "id",
    "canonical_source",
    "canonical_source_digest",
    "owner_module",
    "section_owner",
    "interface_ref",
    "acceptance_selector",
)


def _resolve_source_root(source_root: Optional[Path]) -> Path:
    root = Path(source_root) if source_root is not None else _ROOT.parent
    try:
        resolved = root.resolve(strict=True)
    except OSError as error:
        raise CatalogError("cannot resolve caller source root %s" % root) from error
    if not (resolved / "agent-workflows").is_dir():
        raise CatalogError("caller source root lacks agent-workflows package")
    return resolved


def _root_path(source_root: Path, relative: Any, label: str) -> Path:
    if not isinstance(relative, str) or not relative or "\x00" in relative:
        raise CatalogError("%s path is malformed" % label)
    candidate = Path(relative)
    if candidate.is_absolute() or ".." in candidate.parts or "." in candidate.parts:
        raise CatalogError("%s path escapes the caller source root" % label)
    resolved = (source_root / candidate).resolve()
    try:
        resolved.relative_to(source_root)
    except ValueError as error:
        raise CatalogError("%s path escapes the caller source root" % label) from error
    return resolved


def _trusted_input_path(source_root: Path, path: Path, label: str) -> Path:
    """Resolve a compiler trust input without crossing the caller-root boundary."""

    candidate = Path(path)
    try:
        resolved = candidate.resolve(strict=True)
    except OSError as error:
        raise CatalogError("cannot resolve %s %s" % (label, candidate)) from error
    try:
        resolved.relative_to(source_root)
    except ValueError as error:
        raise CatalogError("%s path escapes the caller source root" % label) from error
    if not resolved.is_file():
        raise CatalogError("%s is not a file" % label)
    return resolved


def _read_json(path: Path) -> Any:
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise CatalogError("cannot read canonical JSON-compatible YAML %s: %s" % (path, error)) from error


def _digest(source_root: Path, path: str) -> str:
    candidate = _root_path(source_root, path, "digest-bound reference")
    if not candidate.is_file():
        raise CatalogError("missing digest-bound reference %s" % path)
    return "sha256:" + hashlib.sha256(candidate.read_bytes()).hexdigest()


def _validate(package_root: Path, document: Mapping[str, Any], schema_name: str) -> None:
    schema = _read_json(
        _trusted_input_path(
            package_root.parent,
            package_root / "schemas" / schema_name,
            "schema %s" % schema_name,
        )
    )
    try:
        validate_document(document, schema, registry=dict(schema.get("$defs", {})))
    except SchemaValidationError as error:
        raise CatalogError(str(error)) from error


def _validate_receipt(
    source_root: Path, package_root: Path, receipt: Mapping[str, Any], identifier: str
) -> None:
    _validate(package_root, receipt, "coverage-receipt-v1.schema.json")
    if receipt["subject_id"] != identifier:
        raise CatalogError("receipt subject does not match inventory target %s" % identifier)
    if _digest(source_root, receipt["source"]) != receipt["source_digest"]:
        raise CatalogError("stale receipt source for %s" % identifier)
    if _digest(source_root, receipt["evidence"]) != receipt["evidence_digest"]:
        raise CatalogError("stale receipt evidence for %s" % identifier)


def _validate_test_selector(
    source_root: Path, module_path: str, selector: str, identifier: str
) -> None:
    """Require one directly runnable unittest selector in its canonical module."""

    module = _root_path(source_root, module_path, "surface purpose test module")
    if not module.is_file():
        raise CatalogError("missing purpose test module for %s" % identifier)
    try:
        tree = ast.parse(module.read_text(encoding="utf-8"), filename=str(module))
    except (OSError, SyntaxError, UnicodeError) as error:
        raise CatalogError("cannot parse purpose test module for %s" % identifier) from error
    parts = selector.split(".")
    if len(parts) != 2 or not parts[1].startswith("test_"):
        raise CatalogError("non-executable purpose test selector for %s" % identifier)
    classes = [
        node
        for node in tree.body
        if isinstance(node, ast.ClassDef) and node.name == parts[0]
    ]
    if len(classes) != 1 or not any(
        (isinstance(base, ast.Name) and base.id == "TestCase")
        or (isinstance(base, ast.Attribute) and base.attr == "TestCase")
        for base in classes[0].bases
    ):
        raise CatalogError("non-executable purpose test selector for %s" % identifier)
    methods = [
        node
        for node in classes[0].body
        if isinstance(node, ast.FunctionDef) and node.name == parts[1]
    ]
    if len(methods) != 1:
        raise CatalogError("non-executable purpose test selector for %s" % identifier)
    arguments = methods[0].args
    if (
        [argument.arg for argument in arguments.args] != ["self"]
        or arguments.posonlyargs
        or arguments.kwonlyargs
        or arguments.vararg is not None
        or arguments.kwarg is not None
        or arguments.defaults
        or arguments.kw_defaults
    ):
        raise CatalogError("non-executable purpose test selector for %s" % identifier)


def _validate_surface_declaration(
    surface: Mapping[str, Any], binding: Mapping[str, str]
) -> Mapping[str, Any]:
    identifier = binding["id"]
    for field in _SURFACE_DECLARATION_FIELDS:
        if surface.get(field) != binding[field]:
            raise CatalogError("additional surface %s has noncanonical %s" % (identifier, field))
    if surface.get("state") != "accepted":
        raise CatalogError("additional surface %s is not source-accepted" % identifier)
    receipt = surface.get("evidence")
    if not isinstance(receipt, Mapping):
        raise CatalogError("accepted surface lacks matching evidence %s" % identifier)
    expected_receipt = {
        "schema": "coverage-receipt/v1",
        "subject_id": identifier,
        "source": binding["canonical_source"],
        "source_digest": binding["canonical_source_digest"],
        "evidence": binding["evidence"],
        "selector": binding["test_selector"],
        "result": "passed",
    }
    for field, expected in expected_receipt.items():
        if receipt.get(field) != expected:
            raise CatalogError("surface receipt %s does not match %s" % (field, identifier))
    return receipt


def _validate_surface_evidence_binding(
    source_root: Path,
    package_root: Path,
    receipt: Mapping[str, Any],
    binding: Mapping[str, str],
) -> None:
    """Validate the evidence body, not merely its caller-supplied digest."""

    identifier = binding["id"]
    evidence = _read_json(_root_path(source_root, receipt["evidence"], "surface evidence"))
    _validate(package_root, evidence, "required-surface-acceptance-v1.schema.json")
    if evidence["surface_id"] != identifier:
        raise CatalogError("surface evidence subject does not match %s" % identifier)
    canonical_source = evidence["canonical_source"]
    if (
        canonical_source["path"] != binding["canonical_source"]
        or canonical_source["digest"] != binding["canonical_source_digest"]
    ):
        raise CatalogError("surface evidence source does not match %s" % identifier)
    acceptance = evidence["acceptance"]
    test_selector = (
        acceptance["selector"]
        if evidence["schema"] == "s7-personal-surface-result/v1"
        else acceptance["test_selector"]
    )
    if test_selector != binding["test_selector"] or receipt["selector"] != test_selector:
        raise CatalogError("surface receipt selector does not match evidence for %s" % identifier)
    if acceptance["result"] != "passed" or receipt["result"] != "passed":
        raise CatalogError("surface receipt result does not match evidence for %s" % identifier)
    _validate_test_selector(
        source_root, binding["test_module"], test_selector, identifier
    )


def _validate_evidence_binding(
    source_root: Path, package_root: Path, receipt: Mapping[str, Any], target: Mapping[str, Any], aliases: Mapping[str, list[str]],
) -> None:
    identifier = target["id"]
    evidence = _read_json(_root_path(source_root, receipt["evidence"], "evidence"))
    contract_id = evidence.get("contract_id")
    qualified = evidence.get("schema") == "qualified-contract-acceptance/v1"
    if qualified:
        _validate(package_root, evidence, "qualified-contract-acceptance-v1.schema.json")
    if contract_id != (identifier if qualified else identifier.rsplit(".", 1)[-1]):
        raise CatalogError("receipt subject does not match evidence for %s" % identifier)
    if not qualified and len(aliases.get(contract_id, [])) != 1:
        raise CatalogError("ambiguous evidence contract id %s requires qualified binding" % contract_id)
    if evidence.get("contract_name") != target["name"]:
        raise CatalogError("receipt name does not match evidence for %s" % identifier)
    evidence_kind = {
        "skill": "named-skill",
        "shared-protocol-operation": "named-shared-protocol-operation",
        "workflow-profile-step": "profile-step",
        "composed-on-demand-operation": "composed-on-demand-operation",
    }[target["implementation_kind"]]
    if (evidence.get("implementation_kind") if qualified else evidence.get("coverage_kind")) != (target["implementation_kind"] if qualified else evidence_kind):
        raise CatalogError("receipt kind does not match evidence for %s" % identifier)
    implementation = evidence.get("implementation_ref", {})
    if implementation.get("path") != receipt["source"] or implementation.get("digest") != receipt["source_digest"]:
        raise CatalogError("receipt source does not match evidence for %s" % identifier)
    test_ref = evidence.get("test_ref", {})
    if test_ref.get("selector") != receipt["selector"]:
        raise CatalogError("receipt selector does not match evidence for %s" % identifier)
    if (evidence.get("status") == "passed") != (receipt["result"] == "passed"):
        raise CatalogError("receipt result does not match evidence for %s" % identifier)


def _inventory(source_root: Path, package_root: Path) -> Mapping[str, Any]:
    inventory = _read_json(
        _trusted_input_path(
            source_root,
            package_root / "manifests" / "catalog-contract-inventory.json",
            "catalog inventory",
        )
    )
    return inventory


def _validate_current_inventory(
    source_root: Path, package_root: Path, inventory: Mapping[str, Any]
) -> None:
    if inventory.get("schema") != "catalog-contract-inventory/v2":
        raise CatalogError("current canonical lineage requires catalog-contract-inventory/v2")
    _validate(package_root, inventory, "catalog-contract-inventory-v2.schema.json")
    from .section_transition_evidence import verify_current_canonical_lineage

    if not verify_current_canonical_lineage(source_root):
        raise CatalogError("current canonical lineage is not trusted")
    lineage = inventory["current_lineage"]
    if _digest(source_root, lineage["path"]) != lineage["digest"]:
        raise CatalogError("stale current canonical lineage binding")
    if _digest(source_root, inventory["catalog_source"]) != inventory["catalog_digest"]:
        raise CatalogError("stale digest-bound catalog inventory")


def _source_inventory(source_root: Path, inventory: Mapping[str, Any]) -> dict[str, Any]:
    """Parse only the accepted catalog document after its digest has been verified."""

    source = _root_path(source_root, inventory["catalog_source"], "catalog source")
    try:
        lines = source.read_text(encoding="utf-8").splitlines()
    except OSError as error:
        raise CatalogError("cannot read digest-bound catalog source: %s" % error) from error
    named: dict[str, list[list[str]]] = {group: [] for group in sorted(_GROUPS)}
    profiles: dict[str, list[list[str]]] = {profile: [] for profile in sorted(_PROFILES)}
    profile_by_section = {"8.1": "feature", "8.2": "bug-fix", "8.3": "improvement"}
    active_profile: Optional[str] = None
    for line in lines:
        section = re.match(r"^### (8\.[123])\b", line)
        if section:
            active_profile = profile_by_section[section.group(1)]
            continue
        contract = re.match(r"^### ([A-H])(\d+R?) `([^`]+)`$", line)
        if contract:
            named[contract.group(1)].append([contract.group(1) + contract.group(2), contract.group(3)])
            continue
        step = re.match(r"^\| ([A-Z]+\d+) ([^|]+) \|", line)
        if active_profile and step:
            display_name = re.sub(r"[^a-z0-9]+", "-", step.group(2).strip().lower()).strip("-")
            profiles[active_profile].append([step.group(1), display_name])
    if sum(map(len, named.values())) != 60 or sum(map(len, profiles.values())) != 23:
        raise CatalogError("cannot derive exact canonical inventory from accepted source")
    return {"named_contracts": named, "profile_steps": profiles}


def _entry(
    source_root: Path,
    identifier: str,
    name: str,
    kind: str,
    receipt: Optional[Mapping[str, Any]],
) -> dict[str, Any]:
    if kind not in _KINDS:
        raise CatalogError("unsupported implementation_kind %s" % kind)
    canonical_source = receipt["source"] if receipt else "docs/plans/ai-agent-workflow-step-catalog.md"
    canonical_source_digest = (
        receipt["source_digest"] if receipt else _digest(source_root, canonical_source)
    )
    accepted = receipt is not None and receipt["result"] == "passed"
    return {
        "id": identifier,
        "name": name,
        "version": "v1",
        "implementation_kind": kind,
        "canonical_source": canonical_source,
        "canonical_source_digest": canonical_source_digest,
        "canonical_composition_ref": "catalog/%s" % identifier,
        "owner_module": "agent-workflows.%s" % identifier.rsplit(".", 1)[0],
        "interface_ref": "interface/%s" % identifier,
        "acceptance_selector": "acceptance/%s" % identifier,
        "evidence_ref": receipt["evidence"] if receipt else None,
        "evidence_digest": receipt["evidence_digest"] if receipt else None,
        "receipt": dict(receipt) if receipt else None,
        "state": "accepted" if accepted else ("present_unaccepted" if receipt else "planned"),
        "section_owner": "S0.registry",
        "reachability": "catalog-qualified",
    }


def _surface_projection(
    source_root: Path, package_root: Path, surface: Mapping[str, Any]
) -> dict[str, Any]:
    """Accept only an exact source, evidence, and executable-purpose binding."""

    binding = _SURFACE_BINDINGS_BY_ID.get(surface.get("id"))
    if binding is None:
        raise CatalogError("unknown additional surface %s" % surface.get("id"))
    result = dict(surface)
    receipt = _validate_surface_declaration(surface, binding)
    _validate_receipt(source_root, package_root, receipt, surface["id"])
    _validate_surface_evidence_binding(source_root, package_root, receipt, binding)
    result["state"] = "accepted"
    return result


def compile_registry(
    *,
    source_root: Optional[Path] = None,
    selector: Optional[str] = None,
    catalog_path: Optional[Path] = None,
    surfaces_path: Optional[Path] = None,
    coverage_path: Optional[Path] = None,
    inventory_path: Optional[Path] = None,
    current_lineage: bool = True,
) -> dict[str, Any]:
    """Compile canonical S0 documents into a fail-closed coverage projection."""

    resolved_root = _resolve_source_root(source_root)
    package_root = resolved_root / "agent-workflows"
    catalog = _read_json(
        _trusted_input_path(
            resolved_root,
            catalog_path or package_root / "catalog.yaml",
            "catalog",
        )
    )
    surfaces = _read_json(
        _trusted_input_path(
            resolved_root,
            surfaces_path
            or package_root / "manifests" / "additional-required-surfaces.json",
            "additional surfaces manifest",
        )
    )
    coverage = _read_json(
        _trusted_input_path(
            resolved_root,
            coverage_path or package_root / "manifests" / "plan-coverage.json",
            "coverage manifest",
        )
    )
    _validate(package_root, catalog, "catalog-v1.schema.json")
    _validate(package_root, surfaces, "additional-required-surfaces-v1.schema.json")
    _validate(package_root, coverage, "plan-coverage-v1.schema.json")
    expected = (
        _read_json(
            _trusted_input_path(
                resolved_root, inventory_path, "catalog inventory"
            )
        )
        if inventory_path
        else _inventory(resolved_root, package_root)
    )
    if current_lineage:
        _validate_current_inventory(resolved_root, package_root, expected)
    else:
        _validate(package_root, expected, "catalog-contract-inventory-v2.schema.json")
        if _digest(resolved_root, expected["catalog_source"]) != expected["catalog_digest"]:
            raise CatalogError("stale digest-bound catalog inventory")
    source_derived = _source_inventory(resolved_root, expected)
    if set(catalog["named_contracts"]) != _GROUPS:
        raise CatalogError("unknown or missing canonical group")
    if set(catalog["profile_steps"]) != _PROFILES:
        raise CatalogError("unknown or missing canonical profile")

    if catalog["named_contracts"] != source_derived["named_contracts"] or catalog["profile_steps"] != source_derived["profile_steps"]:
        raise CatalogError("canonical inventory mismatch")

    inventory: dict[str, dict[str, Any]] = {}
    source_ids = [item["id"] for item in coverage["source_inventory"]]
    if len(source_ids) != len(set(source_ids)):
        duplicate = next(identifier for identifier in source_ids if source_ids.count(identifier) > 1)
        raise CatalogError("duplicate source inventory target %s" % duplicate)
    for item in coverage["source_inventory"]:
        identifier = item["id"]
        if not identifier.startswith(("group.", "profile.")):
            raise CatalogError("unknown source inventory target %s" % identifier)
        _validate_receipt(resolved_root, package_root, item["receipt"], identifier)
        inventory[identifier] = item["receipt"]

    targets: dict[str, dict[str, Any]] = {}
    aliases: dict[str, list[str]] = {}
    for group, contracts in catalog["named_contracts"].items():
        for contract in contracts:
            if not isinstance(contract, list) or len(contract) != 2:
                raise CatalogError("invalid canonical contract entry in group %s" % group)
            short, name = contract
            identifier = "group.%s.%s" % (group, short)
            if identifier in targets:
                raise CatalogError("duplicate target %s" % identifier)
            kind = "shared-protocol-operation" if group == "F" else "skill"
            targets[identifier] = _entry(
                resolved_root, identifier, name, kind, inventory.get(identifier)
            )
            aliases.setdefault(short, []).append(identifier)
    for profile, steps in catalog["profile_steps"].items():
        for short, name in steps:
            identifier = "profile.%s.%s" % (profile, short)
            if identifier in targets:
                raise CatalogError("duplicate target %s" % identifier)
            targets[identifier] = _entry(
                resolved_root,
                identifier,
                name,
                "workflow-profile-step",
                inventory.get(identifier),
            )
            aliases.setdefault(short, []).append(identifier)

    if len(targets) != 83:
        raise CatalogError("canonical targets must total 83, got %s" % len(targets))
    if len(surfaces["surfaces"]) != 11:
        raise CatalogError("additional surfaces must total 11")
    surface_ids = tuple(surface["id"] for surface in surfaces["surfaces"])
    expected_surface_ids = tuple(binding["id"] for binding in _SURFACE_BINDINGS)
    if surface_ids != expected_surface_ids:
        raise CatalogError("additional surfaces do not match canonical identity and order")

    unknown = sorted(set(inventory).difference(targets))
    if unknown:
        raise CatalogError("unknown source inventory target %s" % unknown[0])
    accepted = coverage["accepted_coverage"]
    if len(accepted) != len(set(accepted)):
        raise CatalogError("duplicate accepted coverage")
    for identifier in accepted:
        if identifier not in inventory or identifier not in targets:
            raise CatalogError("accepted target lacks receipt %s" % identifier)
        if inventory[identifier]["result"] != "passed":
            raise CatalogError("accepted target lacks passing receipt %s" % identifier)
    passing = {identifier for identifier, receipt in inventory.items() if receipt["result"] == "passed"}
    if set(accepted) != passing:
        raise CatalogError("accepted coverage does not match passing receipts")
    for identifier, receipt in inventory.items():
        _validate_evidence_binding(
            resolved_root, package_root, receipt, targets[identifier], aliases
        )
    for identifier, target in targets.items():
        target["state"] = "accepted" if identifier in passing else ("present_unaccepted" if identifier in inventory else "planned")
        if identifier in passing:
            target["acceptance_selector"] = inventory[identifier]["selector"]
        _validate(package_root, target, "coverage-target-v1.schema.json")

    if selector is not None:
        if selector not in targets:
            if selector in aliases:
                raise CatalogError("bare selector is not accepted at machine boundary: %s" % selector)
            raise CatalogError("unknown selector %s" % selector)
        selected = {selector: targets[selector]}
    else:
        selected = targets
    target_registry = dict(sorted(selected.items()))
    target_ids = set(target_registry)
    present = sorted(target_ids.intersection(inventory))
    accepted_ids = sorted(target_ids.intersection(passing))
    return {
        "schema": "coverage-projection/v1",
        "target_registry": target_registry,
        "additional_surfaces": {
            entry["id"]: _surface_projection(resolved_root, package_root, entry)
            for entry in surfaces["surfaces"]
        },
        "source_inventory": {identifier: inventory[identifier] for identifier in present},
        "accepted_coverage": accepted_ids,
        "missing_or_invalid": sorted(target_ids.difference(inventory)),
        "present_unaccepted": sorted(set(present).difference(accepted_ids)),
        "counts": {
            "named_targets": sum(key.startswith("group.") for key in targets),
            "profile_targets": sum(key.startswith("profile.") for key in targets),
            "additional_surfaces": len(surfaces["surfaces"]),
            "accepted_named": sum(key.startswith("group.") for key in accepted),
            "accepted_profiles": sum(key.startswith("profile.") for key in accepted),
        },
        "claims": coverage["claims"],
    }


__all__ = ["CatalogError", "compile_registry"]
