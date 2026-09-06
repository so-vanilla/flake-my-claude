"""A7 self-host handoff over immutable copies of the latest manual state."""

from __future__ import annotations

import copy
import hashlib
import json
import os
import re
import shutil
import tempfile
from pathlib import Path
from typing import Any, Dict, Mapping, Optional, Union

from .control_kernel import ControlKernel, KernelError, canonical_digest
from .migration import _load_copy


class HandoffError(KernelError):
    """The manual snapshot cannot be admitted as the A7 handoff truth."""


def _raw_digest(path: Path) -> str:
    return "sha256:" + hashlib.sha256(path.read_bytes()).hexdigest()


def _bare_digest(value: str) -> str:
    return value.split(":", 1)[1] if value.startswith("sha256:") else value


def _fsync_directory(path: Path) -> None:
    try:
        descriptor = os.open(str(path), os.O_RDONLY)
    except OSError:
        return
    try:
        os.fsync(descriptor)
    finally:
        os.close(descriptor)


class SelfHostHandoff:
    """Prepare, adopt, and freshly read one A7 manual-to-kernel handoff.

    A6's revision remains the immutable converter source.  The latest manual
    revision is attached as a separate handoff artifact, so an old Epoch's
    closure revision is never reinterpreted as the current Run revision.
    """

    _REQUIRED_SOURCES = frozenset({"run", "checkpoint", "evidence", "approval", "rehearsal"})
    _ARTIFACT_ID = "a7-self-host-handoff"
    _EPOCH_ID = "epoch-0003-a7-self-host-handoff"

    def __init__(self, kernel: ControlKernel) -> None:
        self.kernel = kernel

    @staticmethod
    def _objective_from_run(run: Mapping[str, Any], sources: Mapping[str, Path]) -> Dict[str, Any]:
        direct = run.get("objective_ref")
        if isinstance(direct, Mapping):
            objective = dict(direct)
        else:
            proposed = run.get("objective")
            if not isinstance(proposed, Mapping):
                raise HandoffError("manual Run objective binding is missing")
            digest = proposed.get("proposed_digest")
            if digest is None:
                objective_path = sources.get("objective")
                if objective_path is None or not objective_path.is_file():
                    raise HandoffError("objective source is required when the Run omits its digest")
                digest = _raw_digest(objective_path)
            objective = {
                "path": proposed.get("proposed_path"),
                "version": proposed.get("proposed_version"),
                "digest": digest,
                "approval_status": proposed.get("approval_status"),
            }
        for key in ("path", "version", "digest"):
            if not isinstance(objective.get(key), str) or not objective[key]:
                raise HandoffError("manual Run objective.%s is missing" % key)
        return objective

    @staticmethod
    def _validate_gates(evidence: Mapping[str, Any]) -> Dict[str, Any]:
        gates = evidence.get("gates")
        expected = {"G%s" % index for index in range(1, 9)}
        if not isinstance(gates, Mapping) or set(gates) != expected:
            raise HandoffError("A6R operational evidence must cover exactly G1-G8")
        result = copy.deepcopy(dict(gates))
        failed = []
        for gate_id, value in sorted(result.items()):
            if not isinstance(value, Mapping) or value.get("status") != "pass":
                failed.append(gate_id)
        if failed:
            raise HandoffError("A7 gates are not operationally verified: %s" % ", ".join(failed))
        return result

    @staticmethod
    def _require_raw_digest(value: Any, label: str) -> str:
        if not isinstance(value, str) or not re.fullmatch(r"sha256:[0-9a-f]{64}", value):
            raise HandoffError("%s must be a raw SHA-256 digest" % label)
        return value

    @classmethod
    def _validate_physical_target(cls, value: Any, label: str) -> Dict[str, str]:
        if not isinstance(value, Mapping) or set(value) != {"path", "digest"}:
            raise HandoffError("%s is malformed" % label)
        path = value.get("path")
        if not isinstance(path, str) or not path.startswith("/") or "\\" in path or any(part in {"", ".", ".."} for part in path.split("/")[1:]):
            raise HandoffError("%s path is not a physical target" % label)
        return {"path": path, "digest": cls._require_raw_digest(value.get("digest"), label + ".digest")}

    @classmethod
    def _validate_rehearsal_receipt(
        cls,
        receipt: Mapping[str, Any],
        *,
        run_id: str,
        workflow_version: str,
        accepted_source_digest: str,
        expected_migrated_head: Mapping[str, Any],
    ) -> Dict[str, Any]:
        required = {
            "schema", "run_id", "workflow_version", "accepted_source_digest",
            "physical_old_target", "physical_new_target", "migrated_head",
            "expected_pointer_cas",
        }
        if not isinstance(receipt, Mapping) or set(receipt) != required:
            raise HandoffError("migration/cutover/rollback rehearsal receipt is malformed")
        if receipt.get("schema") != "a7-migration-cutover-rollback-rehearsal/v1":
            raise HandoffError("migration/cutover/rollback rehearsal schema is invalid")
        if receipt.get("run_id") != run_id or receipt.get("workflow_version") != workflow_version:
            raise HandoffError("migration/cutover/rollback rehearsal Run/workflow binding does not match")
        if cls._require_raw_digest(receipt.get("accepted_source_digest"), "rehearsal accepted_source_digest") != accepted_source_digest:
            raise HandoffError("migration/cutover/rollback rehearsal accepted source does not match")
        old_target = cls._validate_physical_target(receipt.get("physical_old_target"), "rehearsal physical_old_target")
        new_target = cls._validate_physical_target(receipt.get("physical_new_target"), "rehearsal physical_new_target")
        if old_target == new_target:
            raise HandoffError("migration/cutover/rollback rehearsal targets must differ")
        migrated_head = receipt.get("migrated_head")
        if not isinstance(migrated_head, Mapping) or dict(migrated_head) != dict(expected_migrated_head):
            raise HandoffError("migration/cutover/rollback rehearsal migrated HEAD does not match")
        pointer_cas = receipt.get("expected_pointer_cas")
        if not isinstance(pointer_cas, Mapping) or set(pointer_cas) != {"pointer_path", "expected_old_target", "expected_new_target"}:
            raise HandoffError("migration/cutover/rollback rehearsal expected-pointer CAS is malformed")
        pointer_path = pointer_cas.get("pointer_path")
        if not isinstance(pointer_path, str) or not pointer_path.startswith("/") or "\\" in pointer_path or any(part in {"", ".", ".."} for part in pointer_path.split("/")[1:]):
            raise HandoffError("migration/cutover/rollback rehearsal pointer path is not physical")
        if cls._validate_physical_target(pointer_cas.get("expected_old_target"), "rehearsal expected old target") != old_target or cls._validate_physical_target(pointer_cas.get("expected_new_target"), "rehearsal expected new target") != new_target:
            raise HandoffError("migration/cutover/rollback rehearsal expected-pointer CAS does not bind targets")
        return copy.deepcopy(dict(receipt))

    @classmethod
    def _read_and_validate(
        cls,
        source_paths: Mapping[str, Path],
        *,
        expected_state_revision: int,
        accepted_source_digest: str,
        expected_migrated_head: Mapping[str, Any],
    ) -> Dict[str, Any]:
        if set(source_paths) not in (cls._REQUIRED_SOURCES, cls._REQUIRED_SOURCES | {"objective"}):
            raise HandoffError("handoff sources must bind run, checkpoint, evidence, approval, and optional objective")
        if not isinstance(expected_state_revision, int) or isinstance(expected_state_revision, bool) or expected_state_revision < 1:
            raise HandoffError("expected_state_revision is malformed")
        if not isinstance(expected_migrated_head, Mapping):
            raise HandoffError("expected migrated kernel HEAD is malformed")
        if any(not path.is_file() for path in source_paths.values()):
            raise HandoffError("handoff source is missing")

        values: Dict[str, Dict[str, Any]] = {}
        for name in cls._REQUIRED_SOURCES:
            try:
                values[name] = _load_copy(source_paths[name])[0]
            except Exception as exc:
                raise HandoffError("handoff %s source is unreadable" % name) from exc
        run = values["run"]
        checkpoint = values["checkpoint"]
        evidence = values["evidence"]
        approval = values["approval"]
        rehearsal = values["rehearsal"]

        run_id = run.get("run_id")
        workflow_version = run.get("workflow_version")
        if not isinstance(run_id, str) or not run_id or not isinstance(workflow_version, str) or not workflow_version:
            raise HandoffError("manual Run identity is incomplete")
        if run.get("state_revision") != expected_state_revision:
            raise HandoffError("manual Run state revision does not match expected revision")
        if checkpoint.get("run_id") != run_id or checkpoint.get("workflow_version") != workflow_version:
            raise HandoffError("Checkpoint Run/workflow binding does not match")
        if checkpoint.get("state_revision") != expected_state_revision:
            raise HandoffError("Checkpoint state revision does not match current Run")
        checkpoint_id = checkpoint.get("checkpoint_id") or checkpoint.get("id")
        if not isinstance(checkpoint_id, str) or not checkpoint_id:
            raise HandoffError("Checkpoint identity is missing")
        checkpoint_digest = _raw_digest(source_paths["checkpoint"])
        last_checkpoint = run.get("last_checkpoint")
        if not isinstance(last_checkpoint, Mapping):
            raise HandoffError("manual Run last_checkpoint binding is missing")
        if (
            (last_checkpoint.get("id") or last_checkpoint.get("checkpoint_id")) != checkpoint_id
            or last_checkpoint.get("state_revision") != expected_state_revision
            or last_checkpoint.get("checkpoint_digest") != checkpoint_digest
        ):
            raise HandoffError("manual Run last_checkpoint does not bind the copied Checkpoint")

        group = run.get("current_group") or run.get("group")
        epoch = run.get("context_epoch") or run.get("current_epoch") or run.get("epoch")
        if not isinstance(group, Mapping) or not isinstance(group.get("id"), str):
            raise HandoffError("manual Run Group identity is missing")
        if not isinstance(epoch, Mapping) or not isinstance(epoch.get("id"), str):
            raise HandoffError("manual Run Epoch identity is missing")
        if epoch.get("status") != "closed" or epoch.get("clear_before_next", epoch.get("clear_before_start")) is not True:
            raise HandoffError("manual Run does not end at a closed clear Epoch")
        if epoch.get("group_id") not in (None, group["id"]):
            raise HandoffError("manual Run Epoch Group binding does not match")
        closed_at_revision = epoch.get("closed_at_revision")
        if not isinstance(closed_at_revision, int) or isinstance(closed_at_revision, bool) or closed_at_revision < 1:
            raise HandoffError("manual Epoch closure revision is missing")

        if evidence.get("run_id") != run_id or evidence.get("workflow_version") != workflow_version:
            raise HandoffError("A6R evidence Run/workflow binding does not match")
        if evidence.get("status") != "passed":
            raise HandoffError("A6R operational evidence is not passed")
        gates = cls._validate_gates(evidence)
        accepted = cls._require_raw_digest(accepted_source_digest, "accepted_source_digest")
        if cls._require_raw_digest(evidence.get("accepted_source_digest"), "evidence accepted_source_digest") != accepted:
            raise HandoffError("A6R evidence accepted source does not match")
        rehearsal_receipt = cls._validate_rehearsal_receipt(
            rehearsal,
            run_id=run_id,
            workflow_version=workflow_version,
            accepted_source_digest=accepted,
            expected_migrated_head=expected_migrated_head,
        )
        if approval.get("run_id") != run_id or approval.get("workflow_version") != workflow_version:
            raise HandoffError("migration approval Run/workflow binding does not match")
        decision = approval.get("decision")
        if not isinstance(decision, Mapping) or decision.get("migration_approval") is not True:
            raise HandoffError("explicit migration approval is missing")
        a7 = decision.get("a7_self_host_handoff")
        if a7 is not True and (not isinstance(a7, str) or not a7.startswith("authorized")):
            raise HandoffError("A7 self-host authority is missing")

        objective = cls._objective_from_run(run, source_paths)
        aliases = run.get("aliases")
        if not isinstance(aliases, list) or any(not isinstance(alias, str) or not alias for alias in aliases):
            raise HandoffError("manual Run aliases are malformed")
        manual_status = run.get("status")
        if not isinstance(manual_status, str) or not manual_status:
            raise HandoffError("manual Run status is malformed")
        return {
            "run_id": run_id,
            "workflow_version": workflow_version,
            "manual_state_revision": expected_state_revision,
            "manual_status": manual_status,
            "manual_group_id": group["id"],
            "manual_epoch_id": epoch["id"],
            "manual_epoch_closed_at_revision": closed_at_revision,
            "checkpoint_id": checkpoint_id,
            "checkpoint_digest": checkpoint_digest,
            "objective_ref": objective,
            "aliases": copy.deepcopy(aliases),
            "accepted_source_digest": accepted,
            "gates": gates,
            "limitations": copy.deepcopy(evidence.get("limitations") or []),
            "rehearsal_receipt_digest": _raw_digest(source_paths["rehearsal"]),
            "rehearsal_migrated_head": copy.deepcopy(rehearsal_receipt["migrated_head"]),
        }

    def prepare(
        self,
        sources: Mapping[str, Union[str, os.PathLike]],
        *,
        destination: Union[str, os.PathLike],
        expected_state_revision: int,
        accepted_source_digest: str,
    ) -> Dict[str, Any]:
        source_paths = {name: Path(path).resolve() for name, path in sources.items()}
        prepared_head = self.kernel.head()
        validated = self._read_and_validate(
            source_paths,
            expected_state_revision=expected_state_revision,
            accepted_source_digest=accepted_source_digest,
            expected_migrated_head=prepared_head,
        )
        initial_source_digests = {
            name: _raw_digest(path) for name, path in source_paths.items()
        }
        destination_path = Path(destination).resolve()
        if destination_path.exists():
            raise HandoffError("handoff package destination already exists")
        if any(destination_path == path or destination_path in path.parents for path in source_paths.values()):
            raise HandoffError("handoff package destination must be separate from its sources")
        destination_path.parent.mkdir(parents=True, exist_ok=True)
        temporary = Path(tempfile.mkdtemp(prefix=".%s." % destination_path.name, dir=str(destination_path.parent))).resolve()
        try:
            copied_dir = temporary / "sources"
            copied_dir.mkdir()
            copied_paths: Dict[str, Path] = {}
            for name, source in sorted(source_paths.items()):
                suffix = source.suffix if source.suffix else ".bin"
                copied = copied_dir / (name + suffix)
                shutil.copy2(source, copied)
                with copied.open("rb") as handle:
                    os.fsync(handle.fileno())
                if _raw_digest(copied) != initial_source_digests[name]:
                    raise HandoffError("handoff source changed while copying: %s" % name)
                copied_paths[name] = copied
            if any(_raw_digest(path) != initial_source_digests[name] for name, path in source_paths.items()):
                raise HandoffError("handoff source changed during package preparation")
            # Validate the copied bytes and values, rather than trusting the
            # pre-copy read, before publishing the package directory.
            copied_validated = self._read_and_validate(
                copied_paths,
                expected_state_revision=expected_state_revision,
                accepted_source_digest=accepted_source_digest,
                expected_migrated_head=prepared_head,
            )
            if copied_validated != validated:
                raise HandoffError("handoff source identity changed while copying")
            if self.kernel.head() != prepared_head:
                raise HandoffError("kernel HEAD changed during handoff package preparation")
            source_refs = {
                name: {
                    "path": str(path.relative_to(temporary)),
                    "digest": _raw_digest(path),
                }
                for name, path in sorted(copied_paths.items())
            }
            manifest = {
                "schema": "a7-handoff-package/v1",
                **validated,
                "expected_kernel_head": prepared_head,
                "sources": source_refs,
            }
            manifest["manifest_digest"] = canonical_digest(manifest)
            manifest_path = temporary / "manifest.json"
            with manifest_path.open("w", encoding="utf-8") as handle:
                json.dump(manifest, handle, ensure_ascii=False, sort_keys=True, indent=2)
                handle.write("\n")
                handle.flush()
                os.fsync(handle.fileno())
            os.replace(str(temporary), str(destination_path))
            _fsync_directory(destination_path.parent)
        except Exception:
            shutil.rmtree(temporary, ignore_errors=True)
            raise
        return {
            "manifest_path": destination_path / "manifest.json",
            "manifest_digest": manifest["manifest_digest"],
            "source_digests": {name: value["digest"] for name, value in source_refs.items()},
        }

    @classmethod
    def _load_manifest(
        cls,
        manifest_path: Union[str, os.PathLike],
        *,
        expected_manifest_digest: str,
    ) -> Dict[str, Any]:
        path = Path(manifest_path).resolve()
        try:
            manifest = json.loads(path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError) as exc:
            raise HandoffError("handoff manifest is unreadable") from exc
        if not isinstance(manifest, dict) or manifest.get("schema") != "a7-handoff-package/v1":
            raise HandoffError("handoff manifest schema is invalid")
        recorded = manifest.get("manifest_digest")
        actual = canonical_digest({key: copy.deepcopy(value) for key, value in manifest.items() if key != "manifest_digest"})
        if recorded != actual or expected_manifest_digest != actual:
            raise HandoffError("handoff manifest digest mismatch")
        refs = manifest.get("sources")
        if not isinstance(refs, Mapping) or set(refs) not in (cls._REQUIRED_SOURCES, cls._REQUIRED_SOURCES | {"objective"}):
            raise HandoffError("handoff manifest source set is incomplete")
        source_paths: Dict[str, Path] = {}
        for name, ref in refs.items():
            if not isinstance(ref, Mapping) or set(ref) != {"path", "digest"}:
                raise HandoffError("handoff source reference is malformed")
            relative = ref.get("path")
            if not isinstance(relative, str) or not relative or "\\" in relative:
                raise HandoffError("handoff source path is malformed")
            candidate = (path.parent / relative).resolve()
            try:
                candidate.relative_to(path.parent)
            except ValueError as exc:
                raise HandoffError("handoff source path escapes the package") from exc
            if not candidate.is_file() or _raw_digest(candidate) != ref.get("digest"):
                raise HandoffError("handoff source digest mismatch: %s" % name)
            source_paths[name] = candidate
        validated = cls._read_and_validate(
            source_paths,
            expected_state_revision=manifest.get("manual_state_revision"),
            accepted_source_digest=manifest.get("accepted_source_digest"),
            expected_migrated_head=manifest.get("expected_kernel_head"),
        )
        for key, value in validated.items():
            if manifest.get(key) != value:
                raise HandoffError("handoff manifest binding mismatch: %s" % key)
        return manifest

    def adopt(
        self,
        manifest_path: Union[str, os.PathLike],
        *,
        expected_manifest_digest: str,
        authority_ref: Mapping[str, Any],
    ) -> Dict[str, Any]:
        manifest = self._load_manifest(
            manifest_path, expected_manifest_digest=expected_manifest_digest
        )
        state = self.kernel.read_state()
        if not isinstance(state.get("migration"), Mapping) or state["migration"].get("history_preserved") is not True:
            raise HandoffError("A7 requires an immutable A6 migration source")
        if state.get("run_id") != manifest["run_id"] or state.get("workflow_version") != manifest["workflow_version"]:
            raise HandoffError("manual snapshot does not identify this kernel Run")
        migration_binding = state["migration"].get("source_bindings") or {}
        if (
            state.get("group", {}).get("id") != manifest["manual_group_id"]
            or migration_binding.get("epoch_id") != manifest["manual_epoch_id"]
        ):
            raise HandoffError("manual snapshot Group/Epoch does not match the migrated source")
        kernel_objective = state.get("objective_ref") or {}
        for key in ("path", "version", "digest"):
            if _bare_digest(str(kernel_objective.get(key))) != _bare_digest(str(manifest["objective_ref"].get(key))):
                raise HandoffError("manual snapshot objective does not match the migrated source")
        if state.get("metadata", {}).get("aliases") != manifest.get("aliases"):
            raise HandoffError("manual snapshot aliases do not match the migrated source")
        if self._ARTIFACT_ID in state.get("artifacts", {}):
            current = self.status()
            if current.get("manifest_digest") != expected_manifest_digest:
                raise HandoffError("a different A7 handoff is already immutable")
            return current
        expected_head = manifest.get("expected_kernel_head")
        if not isinstance(expected_head, Mapping):
            raise HandoffError("handoff package expected kernel HEAD is missing")
        current_head = self.kernel.head()
        if state.get("status") == "paused_after_epoch" and state.get("epoch", {}).get("status") == "closed":
            if current_head != expected_head:
                raise HandoffError("kernel HEAD changed after handoff package preparation")
            input_ref = state.get("epoch", {}).get("bundle_ref")
            if not isinstance(input_ref, Mapping):
                raise HandoffError("migrated Epoch Bundle reference is missing")
            self.kernel.open_epoch(
                self._EPOCH_ID,
                input_ref,
                authority_ref=authority_ref,
                idempotency_key="a7-open-epoch:" + manifest["manifest_digest"],
            )
        elif state.get("status") == "active" and state.get("epoch", {}).get("id") == self._EPOCH_ID:
            transaction = self.kernel._load_transaction(current_head["transaction_digest"])
            parent = transaction.get("parent")
            if not isinstance(parent, Mapping):
                raise HandoffError("interrupted A7 Epoch has no parent HEAD")
            if (
                parent.get("revision") != expected_head.get("revision")
                or parent.get("transaction_digest") != expected_head.get("transaction_digest")
                or parent.get("state_digest") != expected_head.get("state_digest")
            ):
                raise HandoffError("interrupted A7 Epoch is not based on the prepared kernel HEAD")
        else:
            raise HandoffError("A7 requires the migrated closed Epoch or its interrupted A7 continuation")
        handoff_record = {
            "schema": "a7-self-host-handoff/v1",
            "manifest": copy.deepcopy(manifest),
            "rollback_point": {
                "kernel_head_before_handoff": copy.deepcopy(expected_head),
                "legacy_source_revision": state["migration"]["source_revision"],
                "manual_checkpoint_id": manifest["checkpoint_id"],
            },
        }
        committed = self.kernel.publish_artifact(
            self._ARTIFACT_ID,
            "v1",
            handoff_record,
            kind="self-host-handoff",
            authority_ref=authority_ref,
            idempotency_key="a7-adopt:" + manifest["manifest_digest"],
        )
        return self.status(expected_kernel_revision=committed["revision"])

    def status(self, *, expected_kernel_revision: Optional[int] = None) -> Dict[str, Any]:
        state = self.kernel.read_state()
        if expected_kernel_revision is not None and state.get("revision") != expected_kernel_revision:
            raise HandoffError("kernel revision changed after A7 handoff")
        artifact = state.get("artifacts", {}).get(self._ARTIFACT_ID)
        if not isinstance(artifact, Mapping) or not isinstance(artifact.get("object_ref"), Mapping):
            raise HandoffError("A7 handoff artifact is not present")
        try:
            wrapper = self.kernel._load_ref_object(artifact["object_ref"], "A7 handoff")
        except KernelError as exc:
            raise HandoffError("A7 handoff artifact is invalid") from exc
        payload = wrapper.get("payload", {}).get("payload")
        if not isinstance(payload, Mapping) or payload.get("schema") != "a7-self-host-handoff/v1":
            raise HandoffError("A7 handoff payload is invalid")
        manifest = payload.get("manifest")
        if not isinstance(manifest, Mapping):
            raise HandoffError("A7 handoff manifest is missing")
        epoch = state.get("epoch") or {}
        group = state.get("group") or {}
        next_group = group.get("next_group")
        next_group_can_begin = (
            epoch.get("id") == self._EPOCH_ID
            and epoch.get("status") == "closed"
            and epoch.get("clear_before_next") is True
            and group.get("status") == "closed"
            and isinstance(next_group, str)
            and bool(next_group)
        )
        return {
            "schema": "a7-self-host-status/v1",
            "run_id": state["run_id"],
            "workflow_version": state["workflow_version"],
            "objective_ref": copy.deepcopy(state["objective_ref"]),
            "manual_state_revision": manifest["manual_state_revision"],
            "manual_epoch_closed_at_revision": manifest["manual_epoch_closed_at_revision"],
            "kernel_state_revision": state["revision"],
            "checkpoint_id": manifest["checkpoint_id"],
            "manifest_digest": manifest["manifest_digest"],
            "next_group_can_begin": next_group_can_begin,
            "next_group": copy.deepcopy(next_group) if isinstance(next_group, str) else None,
            "rollback_point": copy.deepcopy(payload.get("rollback_point")),
            "head": self.kernel.head(),
        }


__all__ = ["HandoffError", "SelfHostHandoff"]
