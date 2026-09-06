"""Pure preview and native-projection planning; this module never applies a target."""
from __future__ import annotations

import hashlib
import json
from pathlib import Path, PurePosixPath
import re
from typing import Any, Iterable, Mapping

from .schema_validation import SchemaValidationError, validate_document


class DistributionPlanningError(ValueError):
    pass


_DIGEST_RE = re.compile(r"sha256:[0-9a-f]{64}")


def _under(path: str, root: str) -> bool:
    return path == root or path.startswith(root.rstrip("/") + "/")


def _canonical_relative_path(value: Any, *, label: str) -> str:
    if not isinstance(value, str) or not value or "\\" in value:
        raise DistributionPlanningError(f"{label} path is ambiguous")
    candidate = PurePosixPath(value)
    if candidate.is_absolute() or value.startswith("/"):
        raise DistributionPlanningError(f"{label} path must be relative")
    if value.endswith("/") or "//" in value or any(part in {".", ".."} for part in value.split("/")):
        raise DistributionPlanningError(f"{label} path is not canonical")
    canonical = candidate.as_posix()
    if canonical != value:
        raise DistributionPlanningError(f"{label} path is ambiguous")
    return canonical


def _require_unique_paths(items: Iterable[Mapping[str, Any]], *, label: str) -> None:
    paths = [item["path"] for item in items]
    if len(paths) != len(set(paths)):
        raise DistributionPlanningError(f"duplicate canonical {label} path")


def _digest_document(value: Any) -> str:
    encoded = json.dumps(value, sort_keys=True, separators=(",", ":")).encode("utf-8")
    return "sha256:" + hashlib.sha256(encoded).hexdigest()


def _validated_source_refs(
    sources: Iterable[Mapping[str, Any]], *, managed_roots: Iterable[str] | None = None
) -> list[dict[str, str]]:
    refs: list[dict[str, str]] = []
    roots = list(managed_roots) if managed_roots is not None else None
    for source in sources:
        if set(source) != {"path", "class", "version", "digest"}:
            raise DistributionPlanningError("source identity must contain exactly path, class, version and digest")
        path = _canonical_relative_path(source.get("path"), label="source")
        source_class = source.get("class")
        if source_class not in {"portable_source", "generated_native_projection"}:
            raise DistributionPlanningError("source class is not portable")
        version = source.get("version")
        if not isinstance(version, str) or not version or version.strip() != version:
            raise DistributionPlanningError("source version is required")
        digest = source.get("digest")
        if not isinstance(digest, str) or _DIGEST_RE.fullmatch(digest) is None:
            raise DistributionPlanningError("source digest is invalid")
        refs.append({"path": path, "class": source_class, "version": version, "digest": digest})
    _require_unique_paths(refs, label="source")
    if roots is not None:
        for ref in refs:
            if not any(_under(ref["path"], root) for root in roots):
                raise DistributionPlanningError("unknown source ownership")
    return sorted(refs, key=lambda item: item["path"])


def _schema(name: str) -> Mapping[str, Any]:
    return json.loads((Path(__file__).resolve().parents[2] / "schemas" / name).read_text())


class DistributionPlanner:
    """Convert owned portable inventory into a deterministic non-mutating preview."""

    def __init__(self, owner_manifest: Mapping[str, Any]) -> None:
        self.manifest = owner_manifest

    def preview(self, sources: Iterable[Mapping[str, Any]], targets: Iterable[Mapping[str, Any]]) -> dict[str, Any]:
        source_items = [dict(item) for item in sources]
        if not source_items:
            raise DistributionPlanningError("portable source inventory is required")
        roots = [_canonical_relative_path(root, label="managed source root") for root in self.manifest.get("managed_sources", [])]
        source_refs = _validated_source_refs(source_items, managed_roots=roots)
        source_set_digest = _digest_document(source_refs)
        actions = []
        target_items = [dict(item) for item in targets]
        target_roots = [_canonical_relative_path(item["root"], label="managed target root") for item in self.manifest.get("managed_targets", [])]
        exclusions = {_canonical_relative_path(path, label="app-owned exclusion") for path in self.manifest.get("app_owned_exclusions", [])}
        target_paths = [_canonical_relative_path(item.get("path"), label="target") for item in target_items]
        if len(target_paths) != len(set(target_paths)):
            raise DistributionPlanningError("duplicate canonical target path")
        for target, path in zip(target_items, target_paths):
            if target.get("ownership") != "managed":
                raise DistributionPlanningError("unmanaged or secret target is refused")
            if any(_under(path, excluded) for excluded in exclusions):
                raise DistributionPlanningError("app-owned target is preserved")
            if not any(_under(path, root) for root in target_roots):
                raise DistributionPlanningError("target is outside explicit managed child roots")
            actions.append({"path": path, "action": "replace" if target.get("replace") else "create",
                            "permissions": "owner-only", "source_set_digest": source_set_digest})
        preview = {"schema": "distribution-preview/v1", "applied": False,
                   "provenance": {"sources": source_refs, "source_set_digest": source_set_digest},
                   "actions": sorted(actions, key=lambda item: item["path"]),
                   "requirements": ["backup", "uninstall", "rollback", "doctor"],
                   "preserved": sorted(exclusions)}
        preview["preview_id"] = _digest_document(preview)
        try:
            validate_document(preview, _schema("distribution-preview-v1.schema.json"))
        except SchemaValidationError as error:
            raise DistributionPlanningError(str(error)) from error
        return preview


class NativeProjectionAdapter:
    """Describe one provider projection and unsupported semantics without conversion claims."""

    def __init__(self, provider: str, capability_profile: Mapping[str, Any]) -> None:
        if not isinstance(provider, str) or not provider or provider.strip() != provider:
            raise DistributionPlanningError("provider is required")
        if not isinstance(capability_profile, Mapping) or set(capability_profile) != {
            "path", "version", "provider", "supported_semantics", "digest"
        }:
            raise DistributionPlanningError("complete target capability profile is required")
        profile = dict(capability_profile)
        profile["path"] = _canonical_relative_path(profile.get("path"), label="target capability profile")
        if profile.get("provider") != provider:
            raise DistributionPlanningError("target capability profile provider does not match")
        if not isinstance(profile.get("version"), str) or not profile["version"] or profile["version"].strip() != profile["version"]:
            raise DistributionPlanningError("target capability profile version is required")
        capabilities = profile.get("supported_semantics")
        if (not isinstance(capabilities, list) or capabilities != sorted(set(capabilities))
                or any(not isinstance(item, str) or not item for item in capabilities)):
            raise DistributionPlanningError("target capability profile semantics are not canonical")
        digest = profile.pop("digest")
        if not isinstance(digest, str) or digest != _digest_document(profile):
            raise DistributionPlanningError("target capability profile digest does not bind its content")
        profile["digest"] = digest
        self.provider = provider
        self.profile = profile
        self.capabilities = set(capabilities)

    def plan(self, sources: Iterable[Mapping[str, Any]], semantics: Iterable[str], *,
             staged_artifact: Mapping[str, Any] | None = None) -> dict[str, Any]:
        source_refs = _validated_source_refs([dict(item) for item in sources])
        if not source_refs:
            raise DistributionPlanningError("portable source inventory is required")
        source_set_digest = _digest_document(source_refs)
        requested = sorted(set(semantics))
        if any(not isinstance(item, str) or not item for item in requested):
            raise DistributionPlanningError("requested semantics are invalid")
        if not isinstance(staged_artifact, Mapping) or set(staged_artifact) != {
            "path", "version", "provider", "source_set_digest", "target_profile_digest", "semantics", "digest"
        }:
            raise DistributionPlanningError("complete staged native projection artifact is required")
        artifact = dict(staged_artifact)
        artifact["path"] = _canonical_relative_path(artifact.get("path"), label="staged native projection artifact")
        if artifact.get("provider") != self.provider:
            raise DistributionPlanningError("staged artifact provider does not match")
        if not isinstance(artifact.get("version"), str) or not artifact["version"] or artifact["version"].strip() != artifact["version"]:
            raise DistributionPlanningError("staged artifact version is required")
        if artifact.get("source_set_digest") != source_set_digest:
            raise DistributionPlanningError("staged artifact source set does not match")
        if artifact.get("target_profile_digest") != self.profile["digest"]:
            raise DistributionPlanningError("staged artifact target profile does not match")
        if artifact.get("semantics") != requested:
            raise DistributionPlanningError("staged artifact semantics do not match")
        artifact_digest = artifact.pop("digest")
        if not isinstance(artifact_digest, str) or artifact_digest != _digest_document(artifact):
            raise DistributionPlanningError("staged artifact digest does not bind its content")
        artifact["digest"] = artifact_digest
        supported = sorted(set(requested) & self.capabilities)
        exclusions = sorted(set(requested) - self.capabilities)
        result = {"schema": "native-projection-plan/v1", "provider": self.provider,
                  "source_count": len(source_refs), "source_refs": source_refs,
                  "source_set_digest": source_set_digest, "target_profile": self.profile,
                  "supported_semantics": supported,
                  "exclusions": exclusions, "semantic_parity": not exclusions,
                  "compatibility": "full" if not exclusions else "partial", "staged": True,
                  "staged_artifact": artifact}
        try:
            validate_document(result, _schema("native-projection-plan-v1.schema.json"))
        except SchemaValidationError as error:
            raise DistributionPlanningError(str(error)) from error
        return result


__all__ = ["DistributionPlanner", "DistributionPlanningError", "NativeProjectionAdapter"]
