"""Deterministic source-only release and evidence-index compiler.

The compiler reads the accepted catalog plus the physical Group, Profile,
Workflow, and required-surface manifests.  It never reads a current Run,
worker report, generated provider state, or live configuration.  A successful
result proves only that the accepted source artifacts are mutually bound and
reachable; it does not prove rebuild, activation, adoption, or objective
completion.
"""

from __future__ import annotations

import ast
import hashlib
import json
from collections import defaultdict
from collections.abc import Mapping
from pathlib import Path
from typing import Any, Optional

from .schema_validation import SchemaValidationError, validate_document
from .workflow_composition import WorkflowCompositionError, WorkflowCompositionV1


class SourceReleaseError(ValueError):
    """A source-release input or checked-in projection is not trustworthy."""


_DEFAULT_SOURCE_ROOT = Path(__file__).resolve().parents[3]
_CATALOG_PATH = "agent-workflows/catalog.yaml"
_PLAN_PATH = "docs/plans/ai-agent-workflow-full-implementation-plan.md"
_STEP_CATALOG_PATH = "docs/plans/ai-agent-workflow-step-catalog.md"
_SURFACE_MANIFEST_PATH = "agent-workflows/manifests/additional-required-surfaces.json"
_INDEX_PATH = "agent-workflows/manifests/source-evidence-index.json"
_RELEASE_PATH = "agent-workflows/manifests/source-release.json"
_COMPILER_PATH = "agent-workflows/src/ai_agent_workflow/source_release.py"
_INDEX_OWNER = {
    "id": "source-release/v1",
    "path": _RELEASE_PATH,
    "compiler": "ai_agent_workflow.source_release.SourceReleaseV1",
}
_GROUP_PREFIX = "agent-workflows.group."
_PROFILE_PREFIX = "agent-workflows.profile."
_EXCLUDED_EVIDENCE_PREFIXES = (".local/", "runs/", "run/", "Run/")
_EVIDENCE_KINDS = {
    "skill": "named-skill",
    "shared-protocol-operation": "named-shared-protocol-operation",
    "workflow-profile-step": "profile-step",
    "composed-on-demand-operation": "composed-on-demand-operation",
}


def _require(condition: bool, message: str) -> None:
    if not condition:
        raise SourceReleaseError(message)


def _digest_bytes(value: bytes) -> str:
    return "sha256:" + hashlib.sha256(value).hexdigest()


def _semantic_digest(value: Any) -> str:
    encoded = json.dumps(
        value, sort_keys=True, separators=(",", ":"), ensure_ascii=True
    ).encode("utf-8")
    return _digest_bytes(encoded)


def serialize_manifest(document: Mapping[str, Any]) -> bytes:
    """Return the one physical JSON representation used for checked-in output."""

    return (json.dumps(document, indent=2, ensure_ascii=False) + "\n").encode("utf-8")


def _resolve_root(source_root: Optional[Path]) -> Path:
    candidate = Path(source_root or _DEFAULT_SOURCE_ROOT)
    try:
        root = candidate.resolve(strict=True)
    except OSError as error:
        raise SourceReleaseError("cannot resolve source root %s" % candidate) from error
    _require((root / "agent-workflows").is_dir(), "source root lacks agent-workflows")
    return root


def _safe_file(root: Path, relative: Any, label: str) -> Path:
    _require(isinstance(relative, str) and bool(relative), "%s path is malformed" % label)
    _require("\x00" not in relative, "%s path is malformed" % label)
    path = Path(relative)
    _require(
        not path.is_absolute() and "." not in path.parts and ".." not in path.parts,
        "%s path must remain source-relative" % label,
    )
    try:
        resolved = (root / path).resolve(strict=True)
        resolved.relative_to(root)
    except (OSError, ValueError) as error:
        raise SourceReleaseError("%s path escapes or is missing: %s" % (label, relative)) from error
    _require(resolved.is_file(), "%s is not a file: %s" % (label, relative))
    return resolved


def _relative(root: Path, path: Path) -> str:
    try:
        return path.resolve(strict=True).relative_to(root).as_posix()
    except (OSError, ValueError) as error:
        raise SourceReleaseError("physical manifest escapes source root: %s" % path) from error


def _read_json_path(path: Path, label: str) -> Mapping[str, Any]:
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
        raise SourceReleaseError("cannot read %s %s: %s" % (label, path, error)) from error
    _require(isinstance(value, Mapping), "%s must be a JSON object" % label)
    return value


def _read_json(root: Path, relative: str, label: str) -> Mapping[str, Any]:
    return _read_json_path(_safe_file(root, relative, label), label)


def _raw_digest(root: Path, relative: str, label: str) -> str:
    return _digest_bytes(_safe_file(root, relative, label).read_bytes())


def _ref(root: Path, relative: str, label: str) -> dict[str, str]:
    return {"path": relative, "digest": _raw_digest(root, relative, label)}


def _schema_filename(schema_id: str) -> str:
    if schema_id == "agent-workflow-contract-acceptance/v1":
        return "contract-acceptance-v1.schema.json"
    return schema_id.replace("/", "-") + ".schema.json"


def _validate_schema(
    root: Path, document: Mapping[str, Any], schema_filename: str, label: str
) -> None:
    schema = _read_json(
        root, "agent-workflows/schemas/" + schema_filename, "%s schema" % label
    )
    try:
        validate_document(document, schema, registry=dict(schema.get("$defs", {})))
    except SchemaValidationError as error:
        raise SourceReleaseError("%s schema: %s" % (label, error)) from error


def _validate_ref(root: Path, value: Any, label: str) -> dict[str, str]:
    _require(
        isinstance(value, Mapping)
        and isinstance(value.get("path"), str)
        and isinstance(value.get("digest"), str),
        "%s is not a digest-bound reference" % label,
    )
    actual = _raw_digest(root, value["path"], label)
    _require(actual == value["digest"], "%s raw digest drift" % label)
    return {"path": value["path"], "digest": actual}


def _catalog(root: Path) -> Mapping[str, Any]:
    catalog = _read_json(root, _CATALOG_PATH, "catalog")
    _validate_schema(root, catalog, "catalog-v1.schema.json", "catalog")
    _require(
        list(catalog) == ["schema", "named_contracts", "profile_steps"],
        "catalog top-level order or fields changed",
    )
    for collection_name in ("named_contracts", "profile_steps"):
        owners = catalog[collection_name]
        _require(isinstance(owners, Mapping) and bool(owners), "%s is empty" % collection_name)
        qualified: set[str] = set()
        aliases: set[tuple[str, str]] = set()
        for owner, declarations in owners.items():
            _require(isinstance(declarations, list) and bool(declarations), "empty catalog owner %s" % owner)
            for declaration in declarations:
                _require(
                    isinstance(declaration, list)
                    and len(declaration) == 2
                    and all(isinstance(value, str) and value for value in declaration),
                    "malformed catalog declaration for %s" % owner,
                )
                local_id, name = declaration
                prefix = "group" if collection_name == "named_contracts" else "profile"
                identifier = "%s.%s.%s" % (prefix, owner, local_id)
                _require(identifier not in qualified, "duplicate catalog identity %s" % identifier)
                _require((owner, local_id) not in aliases, "duplicate catalog alias %s.%s" % (owner, local_id))
                qualified.add(identifier)
                aliases.add((owner, local_id))
                _require(name.strip() == name, "catalog name has unstable whitespace for %s" % identifier)
    return catalog


def _manifest_documents(
    root: Path, directory: str, identity_prefix: str, label: str
) -> dict[str, tuple[str, Mapping[str, Any]]]:
    location = root / directory
    _require(location.is_dir(), "%s directory is missing" % label)
    found: dict[str, tuple[str, Mapping[str, Any]]] = {}
    for path in sorted(location.glob("*.json")):
        document = _read_json_path(path, "%s candidate" % label)
        identity_key = "group_id" if identity_prefix == _GROUP_PREFIX else "profile_id"
        identity = document.get(identity_key)
        # The required-only execution policy lives beside Group manifests but
        # is a Workflow policy, not a Group item.
        if not isinstance(identity, str) or not identity.startswith(identity_prefix):
            continue
        owner = identity[len(identity_prefix) :]
        _require(owner and owner not in found, "duplicate %s manifest %s" % (label, owner))
        found[owner] = (_relative(root, path), document)
    return found


def _test_selector_from_evidence(
    root: Path, evidence: Mapping[str, Any], identifier: str
) -> str:
    test_ref = evidence.get("test_ref")
    _require(isinstance(test_ref, Mapping), "evidence test_ref is missing for %s" % identifier)
    selector = test_ref.get("selector")
    _require(isinstance(selector, str) and selector, "evidence test selector is missing for %s" % identifier)
    path = test_ref.get("path")
    if path is not None:
        _validate_ref(root, test_ref, "test reference for %s" % identifier)
        selector = "%s::%s" % (path, selector)
    _validate_test_selector(root, selector, identifier)
    return selector


def _selector_target(source: str) -> tuple[str, Optional[str]]:
    parts = source.replace("::", ".").split(".")
    parts = [part for part in parts if part]
    _require(bool(parts), "test selector target is empty")
    if len(parts) == 1:
        return parts[0], None
    return parts[0], parts[1]


def _ast_has_selector(path: Path, target: str) -> bool:
    class_name, method_name = _selector_target(target)
    try:
        tree = ast.parse(path.read_text(encoding="utf-8"), filename=str(path))
    except (OSError, UnicodeDecodeError, SyntaxError) as error:
        raise SourceReleaseError("cannot parse test selector module %s: %s" % (path, error)) from error
    for node in tree.body:
        if isinstance(node, ast.ClassDef) and node.name == class_name:
            if method_name is None:
                return any(
                    isinstance(member, (ast.FunctionDef, ast.AsyncFunctionDef))
                    and member.name.startswith("test")
                    for member in node.body
                )
            return any(
                isinstance(member, (ast.FunctionDef, ast.AsyncFunctionDef))
                and member.name == method_name
                for member in node.body
            ) or _has_dynamic_test_binding(tree, class_name, method_name)
    return False


def _has_dynamic_test_binding(tree: ast.Module, class_name: str, method_name: str) -> bool:
    """Recognize the accepted catalog-derived profile selector generator."""

    if not method_name.startswith("test_"):
        return False
    functions = {
        node.name for node in tree.body if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef))
    }
    if not {"method_name", "selector_test"}.issubset(functions):
        return False
    for node in ast.walk(tree):
        if not isinstance(node, ast.Call) or not isinstance(node.func, ast.Name):
            continue
        if node.func.id != "setattr" or len(node.args) != 3:
            continue
        owner, name_factory, test_factory = node.args
        if not isinstance(owner, ast.Name) or owner.id != class_name:
            continue
        if not isinstance(name_factory, ast.Call) or not isinstance(name_factory.func, ast.Name):
            continue
        if not isinstance(test_factory, ast.Call) or not isinstance(test_factory.func, ast.Name):
            continue
        if name_factory.func.id == "method_name" and test_factory.func.id == "selector_test":
            return True
    return False


def _validate_test_selector(root: Path, selector: str, identifier: str) -> None:
    _require("::" in selector, "test selector is not source-qualified for %s" % identifier)
    module, target = selector.split("::", 1)
    _require(
        module.startswith("agent-workflows/tests/test_") and module.endswith(".py"),
        "test selector module is noncanonical for %s" % identifier,
    )
    path = _safe_file(root, module, "test selector for %s" % identifier)
    _require(_ast_has_selector(path, target), "test selector is not executable for %s" % identifier)


def _locate_test_selector(root: Path, target: str, identifier: str) -> str:
    matches: list[str] = []
    tests = root / "agent-workflows" / "tests"
    for path in sorted(tests.glob("test_*.py")):
        if _ast_has_selector(path, target):
            matches.append(_relative(root, path))
    _require(len(matches) == 1, "surface test selector is not uniquely executable for %s" % identifier)
    selector = "%s::%s" % (matches[0], target)
    _validate_test_selector(root, selector, identifier)
    return selector


def _validate_contract_evidence(
    root: Path,
    *,
    evidence_path: str,
    identifier: str,
    local_id: str,
    name: str,
    kind: str,
    source: Mapping[str, str],
) -> tuple[str, dict[str, str]]:
    _require(
        evidence_path.startswith("agent-workflows/evidence/contracts/")
        or evidence_path.startswith("agent-workflows/evidence/profiles/"),
        "evidence path is outside canonical source evidence for %s" % identifier,
    )
    evidence = _read_json(root, evidence_path, "evidence for %s" % identifier)
    schema_id = evidence.get("schema")
    _require(isinstance(schema_id, str), "evidence schema is missing for %s" % identifier)
    _validate_schema(root, evidence, _schema_filename(schema_id), "evidence for %s" % identifier)
    qualified = schema_id == "qualified-contract-acceptance/v1"
    _require(
        schema_id in {"qualified-contract-acceptance/v1", "agent-workflow-contract-acceptance/v1"},
        "unsupported evidence schema for %s" % identifier,
    )
    expected_evidence_id = identifier if qualified else local_id
    _require(evidence.get("contract_id") == expected_evidence_id, "evidence identity drift for %s" % identifier)
    _require(evidence.get("contract_name") == name, "evidence name drift for %s" % identifier)
    if qualified:
        _require(evidence.get("implementation_kind") == kind, "cross-kind evidence for %s" % identifier)
    else:
        _require(
            evidence.get("coverage_kind") == _EVIDENCE_KINDS[kind],
            "cross-kind legacy evidence for %s" % identifier,
        )
    _require(evidence.get("implementation_ref") == source, "evidence source binding drift for %s" % identifier)
    _require(evidence.get("status") == "passed", "evidence is not passed for %s" % identifier)
    selector = _test_selector_from_evidence(root, evidence, identifier)
    evidence_ref = _ref(root, evidence_path, "evidence for %s" % identifier)
    return selector, evidence_ref


def _group_items(
    root: Path, catalog: Mapping[str, Any]
) -> tuple[list[dict[str, Any]], list[dict[str, str]]]:
    physical = _manifest_documents(root, "agent-workflows/groups", _GROUP_PREFIX, "Group")
    expected_groups = list(catalog["named_contracts"])
    _require(set(physical) == set(expected_groups), "Group manifest set differs from catalog")

    expected_evidence = {
        "agent-workflows/evidence/contracts/%s.json" % local_id
        for declarations in catalog["named_contracts"].values()
        for local_id, _name in declarations
    }
    actual_evidence = {
        _relative(root, path)
        for path in (root / "agent-workflows/evidence/contracts").glob("*.json")
    }
    _require(actual_evidence == expected_evidence, "contract evidence file set differs from catalog")

    items: list[dict[str, Any]] = []
    manifest_refs: list[dict[str, str]] = []
    for group in expected_groups:
        manifest_path, manifest = physical[group]
        schema_id = manifest.get("schema")
        _require(isinstance(schema_id, str), "Group %s schema is missing" % group)
        _validate_schema(root, manifest, _schema_filename(schema_id), "Group %s manifest" % group)
        _require(manifest.get("group_id") == _GROUP_PREFIX + group, "Group manifest identity drift for %s" % group)
        manifest_ref = _ref(root, manifest_path, "Group %s manifest" % group)
        manifest_refs.append(manifest_ref)
        declarations = catalog["named_contracts"][group]
        expected_ids = ["group.%s.%s" % (group, local_id) for local_id, _name in declarations]

        if "contracts" in manifest:
            contracts = manifest["contracts"]
            _require(
                [contract.get("qualified_id") for contract in contracts] == expected_ids,
                "Group %s identity or order differs from catalog" % group,
            )
            _require(manifest.get("implementation_kind") == "skill", "Group %s has cross-kind manifest" % group)
        else:
            _require(group == "F", "non-F Group manifest lacks contracts")
            contracts = manifest.get("selectors")
            _require(contracts == expected_ids, "Group F selector identity or order differs from catalog")

        for order, ((local_id, name), contract_value) in enumerate(zip(declarations, contracts), 1):
            identifier = "group.%s.%s" % (group, local_id)
            if isinstance(contract_value, Mapping):
                contract = contract_value
                _require(contract.get("local_id") == local_id, "Group local identity drift for %s" % identifier)
                if "contract_name" in contract:
                    _require(contract["contract_name"] == name, "Group contract name drift for %s" % identifier)
                if "selector" in contract:
                    _require(contract["selector"] == name, "Group selector name drift for %s" % identifier)
                if "order" in contract:
                    _require(contract["order"] == order, "Group contract order drift for %s" % identifier)
                source = _validate_ref(root, contract.get("source_ref"), "source for %s" % identifier)
                _require(
                    Path(source["path"]).parent.name == name,
                    "Group source name differs from catalog for %s" % identifier,
                )
                for ref_name in ("selector_ref", "receipt_ref", "evidence_ref"):
                    value = contract.get(ref_name)
                    if isinstance(value, Mapping):
                        _validate_ref(root, value, "%s for %s" % (ref_name, identifier))
                kind = "skill"
            else:
                _require(contract_value == identifier and group == "F", "malformed Group selector %s" % identifier)
                source_path = manifest.get("source", {}).get("path")
                source = _ref(root, source_path, "shared source for %s" % identifier)
                kind = "shared-protocol-operation"

            evidence_path = "agent-workflows/evidence/contracts/%s.json" % local_id
            selector, evidence = _validate_contract_evidence(
                root,
                evidence_path=evidence_path,
                identifier=identifier,
                local_id=local_id,
                name=name,
                kind=kind,
                source=source,
            )
            items.append(
                {
                    "id": identifier,
                    "name": name,
                    "kind": kind,
                    "source": source,
                    "test_selector": selector,
                    "evidence": {**evidence, "result": "passed"},
                    "composition": {
                        "kind": "group-composition",
                        "id": _GROUP_PREFIX + group,
                        **manifest_ref,
                    },
                }
            )
    return items, manifest_refs


def _profile_items(
    root: Path,
    catalog: Mapping[str, Any],
    group_ids: set[str],
) -> tuple[list[dict[str, Any]], list[dict[str, str]]]:
    physical = _manifest_documents(root, "agent-workflows/profiles", _PROFILE_PREFIX, "Profile")
    expected_profiles = list(catalog["profile_steps"])
    _require(set(physical) == set(expected_profiles), "Profile manifest set differs from catalog")
    expected_evidence: set[str] = set()
    items: list[dict[str, Any]] = []
    manifest_refs: list[dict[str, str]] = []

    for profile in expected_profiles:
        manifest_path, manifest = physical[profile]
        schema_id = manifest.get("schema")
        _require(isinstance(schema_id, str), "Profile %s schema is missing" % profile)
        _validate_schema(root, manifest, _schema_filename(schema_id), "Profile %s manifest" % profile)
        _require(manifest.get("profile_id") == _PROFILE_PREFIX + profile, "Profile identity drift for %s" % profile)
        _require(manifest.get("profile") == profile, "Profile name drift for %s" % profile)
        _require(manifest.get("implementation_kind") == "workflow-profile-step", "Profile %s has cross-kind manifest" % profile)
        manifest_ref = _ref(root, manifest_path, "Profile %s manifest" % profile)
        manifest_refs.append(manifest_ref)
        declarations = catalog["profile_steps"][profile]
        expected_ids = ["profile.%s.%s" % (profile, local_id) for local_id, _name in declarations]
        contracts = manifest.get("contracts")
        _require(
            isinstance(contracts, list)
            and [contract.get("qualified_id") for contract in contracts] == expected_ids,
            "Profile %s identity or order differs from catalog" % profile,
        )
        for order, ((local_id, name), contract) in enumerate(zip(declarations, contracts), 1):
            identifier = "profile.%s.%s" % (profile, local_id)
            _require(contract.get("local_id") == local_id, "Profile local identity drift for %s" % identifier)
            _require(contract.get("contract_name") == name, "Profile contract name drift for %s" % identifier)
            _require(contract.get("order") == order, "Profile contract order drift for %s" % identifier)
            _require(contract.get("host_selector") in group_ids, "Profile host is unreachable for %s" % identifier)
            _validate_ref(root, contract.get("template_ref"), "template for %s" % identifier)
            source = _validate_ref(root, contract.get("selector_ref"), "source for %s" % identifier)
            _require(
                contract["selector_ref"].get("selector") == "SoftwareProfileV1.compile(%s)" % identifier,
                "Profile compiler selector drift for %s" % identifier,
            )
            evidence_ref = _validate_ref(root, contract.get("evidence_ref"), "evidence for %s" % identifier)
            expected_path = "agent-workflows/evidence/profiles/%s/%s.json" % (profile, local_id)
            _require(evidence_ref["path"] == expected_path, "Profile evidence path drift for %s" % identifier)
            expected_evidence.add(expected_path)
            selector, evidence = _validate_contract_evidence(
                root,
                evidence_path=expected_path,
                identifier=identifier,
                local_id=local_id,
                name=name,
                kind="workflow-profile-step",
                source=source,
            )
            _require(evidence == evidence_ref, "Profile evidence raw digest drift for %s" % identifier)
            items.append(
                {
                    "id": identifier,
                    "name": name,
                    "kind": "workflow-profile-step",
                    "source": source,
                    "test_selector": selector,
                    "evidence": {**evidence, "result": "passed"},
                    "composition": {
                        "kind": "profile-composition",
                        "id": _PROFILE_PREFIX + profile,
                        **manifest_ref,
                    },
                }
            )

    actual_evidence = {
        _relative(root, path)
        for path in (root / "agent-workflows/evidence/profiles").glob("*/*.json")
    }
    _require(actual_evidence == expected_evidence, "profile evidence file set differs from catalog")
    return items, manifest_refs


def _surface_items(
    root: Path,
) -> tuple[list[dict[str, Any]], dict[str, str]]:
    manifest = _read_json(root, _SURFACE_MANIFEST_PATH, "required-surface manifest")
    _validate_schema(
        root,
        manifest,
        "additional-required-surfaces-v1.schema.json",
        "required-surface manifest",
    )
    accepted_schema = _read_json(
        root,
        "agent-workflows/schemas/required-surface-acceptance-v1.schema.json",
        "required-surface evidence schema",
    )
    expected_documents = [alternative.get("const") for alternative in accepted_schema.get("oneOf", [])]
    _require(
        bool(expected_documents) and all(isinstance(value, Mapping) for value in expected_documents),
        "required-surface evidence schema lacks physical accepted identities",
    )
    expected_ids = [value["surface_id"] for value in expected_documents]
    surfaces = manifest.get("surfaces")
    _require(
        isinstance(surfaces, list) and [surface.get("id") for surface in surfaces] == expected_ids,
        "required-surface identity or order differs from accepted evidence schema",
    )
    manifest_ref = _ref(root, _SURFACE_MANIFEST_PATH, "required-surface manifest")
    expected_evidence_paths: set[str] = set()
    items: list[dict[str, Any]] = []
    for surface, accepted in zip(surfaces, expected_documents):
        identifier = surface["id"]
        _require(surface.get("state") == "accepted", "required surface is not accepted: %s" % identifier)
        _require(surface.get("owner_module") == "agent-workflows", "surface owner drift for %s" % identifier)
        _require(surface.get("section_owner") == "S7", "surface section drift for %s" % identifier)
        source = _ref(root, surface.get("canonical_source"), "source for %s" % identifier)
        _require(
            surface.get("canonical_source_digest") == source["digest"],
            "surface source raw digest drift for %s" % identifier,
        )
        receipt = surface.get("evidence")
        _require(isinstance(receipt, Mapping), "surface evidence receipt is missing for %s" % identifier)
        evidence_path = receipt.get("evidence")
        _require(
            isinstance(evidence_path, str)
            and evidence_path.startswith("agent-workflows/evidence/surfaces/"),
            "surface evidence path is noncanonical for %s" % identifier,
        )
        expected_evidence_paths.add(evidence_path)
        evidence = _read_json(root, evidence_path, "surface evidence for %s" % identifier)
        try:
            validate_document(evidence, accepted_schema, registry=dict(accepted_schema.get("$defs", {})))
        except SchemaValidationError as error:
            raise SourceReleaseError("surface evidence schema for %s: %s" % (identifier, error)) from error
        _require(evidence == accepted, "surface evidence is not the accepted physical record for %s" % identifier)
        evidence_ref = _ref(root, evidence_path, "surface evidence for %s" % identifier)
        expected_receipt = {
            "schema": "coverage-receipt/v1",
            "subject_id": identifier,
            "source": source["path"],
            "source_digest": source["digest"],
            "evidence": evidence_ref["path"],
            "evidence_digest": evidence_ref["digest"],
            "selector": receipt.get("selector"),
            "result": "passed",
        }
        _require(dict(receipt) == expected_receipt, "surface evidence receipt drift for %s" % identifier)
        _require(evidence.get("surface_id") == identifier, "surface evidence identity drift for %s" % identifier)
        _require(evidence.get("canonical_source") == source, "surface evidence source drift for %s" % identifier)
        acceptance = evidence.get("acceptance")
        _require(isinstance(acceptance, Mapping), "surface acceptance is missing for %s" % identifier)
        evidence_selector = acceptance.get("selector")
        if evidence.get("schema") == "company-governance-surface-evidence/v1":
            evidence_selector = acceptance.get("test_selector")
        _require(evidence_selector == receipt.get("selector"), "surface evidence selector drift for %s" % identifier)
        _require(acceptance.get("result") == "passed", "surface evidence is not passed for %s" % identifier)
        test_selector = _locate_test_selector(root, evidence_selector, identifier)
        name = surface.get("acceptance_selector")
        _require(isinstance(name, str) and name, "surface canonical name is missing for %s" % identifier)
        items.append(
            {
                "id": identifier,
                "name": name,
                "kind": "required-surface",
                "source": source,
                "test_selector": test_selector,
                "evidence": {**evidence_ref, "result": "passed"},
                "composition": {
                    "kind": "surface-composition",
                    "id": "additional-required-surfaces/v1",
                    **manifest_ref,
                },
            }
        )

    actual_evidence_paths = {
        _relative(root, path)
        for path in (root / "agent-workflows/evidence/surfaces").glob("*/*.json")
    }
    _require(
        actual_evidence_paths == expected_evidence_paths,
        "surface evidence file set differs from accepted manifest",
    )
    return items, manifest_ref


def _workflow_reachability(
    root: Path, expected_ids: set[str]
) -> tuple[dict[str, list[dict[str, str]]], list[dict[str, str]]]:
    try:
        validator = WorkflowCompositionV1(source_root=root)
        suite = validator.validate_directory(root / "agent-workflows/workflows")
    except WorkflowCompositionError as error:
        raise SourceReleaseError("accepted Workflow suite is invalid: %s" % error) from error
    _require(suite.get("result") == "passed", "accepted Workflow suite did not pass")
    reachability: dict[str, list[dict[str, str]]] = defaultdict(list)
    manifests: list[tuple[str, dict[str, str], Mapping[str, Any]]] = []
    for path in sorted((root / "agent-workflows/workflows").glob("*.json")):
        relative = _relative(root, path)
        document = _read_json_path(path, "Workflow manifest")
        workflow_id = document.get("workflow_id")
        _require(path.stem == workflow_id, "Workflow filename and identity differ")
        manifest_ref = _ref(root, relative, "Workflow %s manifest" % workflow_id)
        manifests.append((workflow_id, manifest_ref, document))
    manifests.sort(key=lambda value: value[0])
    _require(
        [value[0] for value in manifests] == suite.get("workflow_ids"),
        "Workflow suite ordering or identity drift",
    )
    for workflow_id, manifest_ref, document in manifests:
        for stage in document["stages"]:
            for selector in stage["selectors"]:
                _require(selector in expected_ids, "Workflow reaches cross-kind or unknown selector %s" % selector)
                reachability[selector].append(
                    {
                        "kind": "workflow",
                        "id": workflow_id,
                        "path": manifest_ref["path"],
                        "digest": manifest_ref["digest"],
                        "stage_id": stage["stage_id"],
                    }
                )
    missing = sorted(expected_ids.difference(reachability))
    _require(not missing, "accepted items are unreachable from Workflows: %s" % missing)
    return reachability, [value[1] for value in manifests]


def _clean_output_paths(document: Any) -> None:
    if isinstance(document, Mapping):
        for key, value in document.items():
            if key in {"path", "evidence"} and isinstance(value, str):
                _require(not value.startswith(_EXCLUDED_EVIDENCE_PREFIXES), "projection includes current Run or worker evidence")
                _require(not Path(value).is_absolute(), "projection includes a private absolute path")
            _clean_output_paths(value)
    elif isinstance(document, list):
        for value in document:
            _clean_output_paths(value)


class SourceReleaseV1:
    """Compile and verify the canonical source-release projection."""

    def __init__(self, *, source_root: Optional[Path] = None) -> None:
        self.source_root = _resolve_root(source_root)

    def compile(self) -> dict[str, Mapping[str, Any]]:
        root = self.source_root
        catalog = _catalog(root)
        group_items, group_manifests = _group_items(root, catalog)
        group_ids = {item["id"] for item in group_items}
        profile_items, profile_manifests = _profile_items(root, catalog, group_ids)
        surface_items, surface_manifest = _surface_items(root)

        selectable_ids = group_ids | {item["id"] for item in profile_items}
        reachability, workflow_manifests = _workflow_reachability(root, selectable_ids)
        entries: list[dict[str, Any]] = []
        for item in [*group_items, *profile_items, *surface_items]:
            workflow_refs = reachability.get(item["id"], [])
            if item["kind"] != "required-surface":
                _require(bool(workflow_refs), "item is unreachable from accepted Workflows: %s" % item["id"])
            witnesses = [item["composition"]]
            if workflow_refs:
                # One exact, deterministic accepted Workflow witness is enough
                # for replay; the compiler still validates the complete suite.
                witnesses.append(workflow_refs[0])
            entries.append(
                {
                    "id": item["id"],
                    "name": item["name"],
                    "kind": item["kind"],
                    "source": item["source"],
                    "test_selector": item["test_selector"],
                    "evidence": item["evidence"],
                    "reachability": witnesses,
                }
            )
        ids = [entry["id"] for entry in entries]
        _require(len(ids) == len(set(ids)), "source evidence index contains duplicate identities")

        evidence_index: dict[str, Any] = {
            "schema": "source-evidence-index/v1",
            "owner": dict(_INDEX_OWNER),
            "ordering": "catalog-groups-then-catalog-profiles-then-surface-manifest",
            "entry_count": len(entries),
            "entries": entries,
            "entries_digest": _semantic_digest(entries),
        }
        index_digest = _digest_bytes(serialize_manifest(evidence_index))
        named_count = len(group_items)
        profile_count = len(profile_items)
        surface_count = len(surface_items)
        release: dict[str, Any] = {
            "schema": "source-release/v1",
            "source_only": True,
            "compiler": _ref(root, _COMPILER_PATH, "source-release compiler"),
            "authority": {
                "plan": _ref(root, _PLAN_PATH, "implementation plan"),
                "step_catalog": _ref(root, _STEP_CATALOG_PATH, "step catalog"),
            },
            "inputs": {
                "catalog": _ref(root, _CATALOG_PATH, "catalog"),
                "schemas": [
                    _ref(
                        root,
                        "agent-workflows/schemas/source-release-v1.schema.json",
                        "source-release schema",
                    ),
                    _ref(
                        root,
                        "agent-workflows/schemas/source-evidence-index-v1.schema.json",
                        "source-evidence-index schema",
                    ),
                ],
                "group_manifests": group_manifests,
                "profile_manifests": profile_manifests,
                "workflow_manifests": workflow_manifests,
                "surface_manifest": surface_manifest,
            },
            "coverage": {
                "named_contracts": {
                    "catalog_total": sum(len(value) for value in catalog["named_contracts"].values()),
                    "manifest_total": named_count,
                    "accepted": named_count,
                    "complete": True,
                },
                "profile_steps": {
                    "catalog_total": sum(len(value) for value in catalog["profile_steps"].values()),
                    "manifest_total": profile_count,
                    "accepted": profile_count,
                    "complete": True,
                },
                "additional_required_surfaces": {
                    "manifest_total": surface_count,
                    "accepted": surface_count,
                    "complete": True,
                },
            },
            "evidence_index": {
                "path": _INDEX_PATH,
                "digest": index_digest,
                "owner": _INDEX_OWNER["id"],
                "entry_count": len(entries),
                "entries_digest": evidence_index["entries_digest"],
            },
            "claims": {
                "canonical_source_projection_complete": True,
                "source_wide_integration_complete": False,
                "actual_a7_handoff_complete": False,
                "current_objective_approved": False,
                "current_run_objective_achieved": False,
                "nix_rebuild_verified": False,
                "migration_complete": False,
                "activation_complete": False,
                "full_workflow_ready": False,
            },
        }
        _require(
            release["coverage"]["named_contracts"]["catalog_total"] == named_count,
            "named catalog and manifest counts differ",
        )
        _require(
            release["coverage"]["profile_steps"]["catalog_total"] == profile_count,
            "profile catalog and manifest counts differ",
        )
        _clean_output_paths(evidence_index)
        _clean_output_paths(release)
        _validate_schema(root, evidence_index, "source-evidence-index-v1.schema.json", "source evidence index")
        _validate_schema(root, release, "source-release-v1.schema.json", "source release")
        return {"source_release": release, "evidence_index": evidence_index}

    def verify_checked_in(self) -> dict[str, Any]:
        compiled = self.compile()
        index_bytes = serialize_manifest(compiled["evidence_index"])
        release_bytes = serialize_manifest(compiled["source_release"])
        actual_index = _safe_file(self.source_root, _INDEX_PATH, "checked-in evidence index").read_bytes()
        actual_release = _safe_file(self.source_root, _RELEASE_PATH, "checked-in source release").read_bytes()
        _require(actual_index == index_bytes, "checked-in evidence index differs from physical compilation")
        _require(actual_release == release_bytes, "checked-in source release differs from physical compilation")
        _require(
            compiled["source_release"]["evidence_index"]["digest"] == _digest_bytes(actual_index),
            "source release does not own the checked-in evidence index digest",
        )
        return {
            "schema": "source-release-verification/v1",
            "result": "passed",
            "release_digest": _digest_bytes(actual_release),
            "evidence_index_digest": _digest_bytes(actual_index),
            "entry_count": compiled["evidence_index"]["entry_count"],
            "coverage": compiled["source_release"]["coverage"],
        }


def compile_source_release(*, source_root: Optional[Path] = None) -> Mapping[str, Any]:
    return SourceReleaseV1(source_root=source_root).compile()["source_release"]


def compile_source_evidence_index(*, source_root: Optional[Path] = None) -> Mapping[str, Any]:
    return SourceReleaseV1(source_root=source_root).compile()["evidence_index"]


def verify_checked_in_source_release(*, source_root: Optional[Path] = None) -> Mapping[str, Any]:
    return SourceReleaseV1(source_root=source_root).verify_checked_in()


__all__ = [
    "SourceReleaseError",
    "SourceReleaseV1",
    "compile_source_evidence_index",
    "compile_source_release",
    "serialize_manifest",
    "verify_checked_in_source_release",
]
