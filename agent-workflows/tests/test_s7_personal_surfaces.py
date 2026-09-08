from __future__ import annotations

import ast
import copy
import hashlib
import importlib.util
import json
import os
import sys
import tempfile
import unittest
from contextlib import redirect_stderr, redirect_stdout
from io import StringIO
from pathlib import Path
from typing import Any, Mapping


ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "src"))

from ai_agent_workflow.config_policy import CodexConfigManager, ConfigError  # noqa: E402
from ai_agent_workflow.distribution_planning import DistributionPlanningError  # noqa: E402
from ai_agent_workflow.schema_validation import validate_document  # noqa: E402


def _load_script(filename: str, module_name: str):
    spec = importlib.util.spec_from_file_location(module_name, ROOT / filename)
    if spec is None or spec.loader is None:
        raise AssertionError("script module cannot be loaded")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


CONFIG_INSTALLER = _load_script("config-installer.py", "s7_config_installer")
PROVIDER_PROJECTION = _load_script("provider-projection.py", "s7_provider_projection")


def _digest_bytes(content: bytes) -> str:
    return "sha256:" + hashlib.sha256(content).hexdigest()


def _digest_document(document: Mapping[str, Any]) -> str:
    encoded = json.dumps(document, sort_keys=True, separators=(",", ":")).encode()
    return _digest_bytes(encoded)


def _capture_main(main, argv: list[str]) -> tuple[int, str, str]:
    stdout = StringIO()
    stderr = StringIO()
    with redirect_stdout(stdout), redirect_stderr(stderr):
        result = main(argv)
    return result, stdout.getvalue(), stderr.getvalue()


_OWNERSHIP_CLASSES = [
    "portable_source",
    "generated_projection",
    "managed_destination",
    "unmanaged_user_state",
    "app_owned_runtime",
    "secret_bearing_state",
    "cache",
]


def _validate_distribution_manifest(document: Mapping[str, Any]) -> None:
    expected_keys = {
        "schema",
        "lifecycle_owner",
        "source_status",
        "live_installation_claimed",
        "ownership_classes",
        "portable_source_roots",
        "generated_projection_roots",
        "managed_child_sets",
        "provider_roots",
        "portable_manifest_destination",
        "exclusions",
        "config_exception",
        "lifecycle_operations",
    }
    if set(document) != expected_keys:
        raise ValueError("distribution manifest top-level shape changed")
    if document["schema"] != "agent-workflow-distribution/v1":
        raise ValueError("distribution manifest schema changed")
    if document["lifecycle_owner"] != "flake-my-claude.agent-workflows":
        raise ValueError("distribution must have exactly one lifecycle owner")
    if document["source_status"] != "source-candidate-only":
        raise ValueError("distribution status is not source-only")
    if document["live_installation_claimed"] is not False:
        raise ValueError("distribution falsely claims a live installation")

    classes = document["ownership_classes"]
    if [item.get("id") for item in classes] != _OWNERSHIP_CLASSES:
        raise ValueError("ownership classes were collapsed, duplicated, or reordered")
    if any(set(item) != {"id", "authority", "mutation"} for item in classes):
        raise ValueError("ownership class shape changed")
    if len({item["id"] for item in classes}) != 7:
        raise ValueError("ownership class identity is not unique")
    mutation_by_class = {item["id"]: item["mutation"] for item in classes}
    if mutation_by_class != {
        "portable_source": "source-change-only",
        "generated_projection": "generator-only",
        "managed_destination": "explicit-child-transaction-only",
        "unmanaged_user_state": "refuse",
        "app_owned_runtime": "refuse",
        "secret_bearing_state": "refuse",
        "cache": "refuse",
    }:
        raise ValueError("ownership-class mutation boundaries changed")

    owner_manifest = json.loads((ROOT / "manifests" / "owner-manifest.json").read_text())
    if document["portable_source_roots"] != sorted(owner_manifest["managed_sources"]):
        raise ValueError("portable source roots diverge from the accepted owner manifest")
    if document["generated_projection_roots"] != ["agent-workflows/generated"]:
        raise ValueError("generated projection roots changed")

    child_sets = document["managed_child_sets"]
    if set(child_sets) != {"portable_manifest_files", "workflow_skill_directories"}:
        raise ValueError("managed child sets changed")
    expected_skills = sorted(
        path.name for path in (ROOT / "skills").iterdir() if path.is_dir()
    )
    if child_sets["workflow_skill_directories"] != expected_skills:
        raise ValueError("managed skill children are not exact")
    expected_manifests = sorted(
        path.name for path in (ROOT / "manifests").glob("*.json")
    )
    if child_sets["portable_manifest_files"] != expected_manifests:
        raise ValueError("managed manifest children are not exact")

    expected_provider_roots = {
        item["root"] for item in owner_manifest["managed_targets"] if item["root"].endswith("/skills")
    }
    providers = document["provider_roots"]
    if {item.get("path") for item in providers} != expected_provider_roots:
        raise ValueError("provider root set changed")
    if any(
        set(item)
        != {
            "path",
            "root_class",
            "managed_child_set",
            "mechanism",
            "unlisted_children_class",
        }
        or item["root_class"] != "app_owned_runtime"
        or item["managed_child_set"] != "workflow_skill_directories"
        or item["unlisted_children_class"] != "unmanaged_user_state"
        for item in providers
    ):
        raise ValueError("provider roots or their unknown children were claimed")
    destination = document["portable_manifest_destination"]
    if destination != {
        "path": ".local/share/agent-workflows",
        "root_class": "managed_destination",
        "managed_child_set": "portable_manifest_files",
        "mechanism": "home.file",
    }:
        raise ValueError("portable manifest destination changed")

    exclusion_map = {item.get("path"): item.get("class") for item in document["exclusions"]}
    if exclusion_map != {
        ".agents/cache": "cache",
        ".claude/cache": "cache",
        ".codex/auth.json": "secret_bearing_state",
        ".codex/cache": "cache",
        ".codex/config.toml": "app_owned_runtime",
        ".codex/plugins": "app_owned_runtime",
        ".codex/sessions": "secret_bearing_state",
        ".codex/state": "app_owned_runtime",
    }:
        raise ValueError("distribution exclusions changed")
    if document["config_exception"] != {
        "path": ".codex/config.toml",
        "owned_keys": ["model", "model_reasoning_effort"],
        "owner": "config-installer",
        "distribution_mutation": "refuse",
    }:
        raise ValueError("config ownership exception changed")

    operations = document["lifecycle_operations"]
    if set(operations) != {"backup", "doctor", "uninstall", "rollback"}:
        raise ValueError("required lifecycle operations changed")
    if operations["backup"] != {
        "required_before_replace": True,
        "preserve_unknown_bytes": True,
    }:
        raise ValueError("backup contract changed")
    if operations["doctor"] != {
        "writes": False,
        "checks": [
            "exact-child-ownership",
            "source-digest",
            "destination-digest",
            "exclusion-preservation",
        ],
    }:
        raise ValueError("doctor contract changed")
    if operations["uninstall"] != {
        "scope": "previously-owned-exact-children-only",
        "preserve_provider_roots": True,
    }:
        raise ValueError("uninstall contract changed")
    if operations["rollback"] != {
        "requires": [
            "backup",
            "expected-current-digest",
            "expected-current-identity",
        ],
        "conflict_action": "stop",
    }:
        raise ValueError("rollback contract changed")


def _require_managed_target(
    manifest: Mapping[str, Any], path: str, ownership_class: str
) -> None:
    if ownership_class != "managed_destination":
        raise ValueError("secret, unmanaged, app-owned, and cache targets are refused")
    exclusions = [item["path"] for item in manifest["exclusions"]]
    if any(path == excluded or path.startswith(excluded + "/") for excluded in exclusions):
        raise ValueError("excluded destination is refused")
    child_sets = manifest["managed_child_sets"]
    exact = {
        root["path"] + "/" + child
        for root in manifest["provider_roots"]
        for child in child_sets[root["managed_child_set"]]
    }
    destination = manifest["portable_manifest_destination"]
    exact.update(
        destination["path"] + "/" + child
        for child in child_sets[destination["managed_child_set"]]
    )
    if path not in exact:
        raise ValueError("target is not an exact managed child")


class S7PersonalSurfaceTests(unittest.TestCase):
    maxDiff = None

    def _assert_evidence(
        self,
        filename: str,
        *,
        surface_id: str,
        source_path: str,
        selector: str,
    ) -> Mapping[str, Any]:
        evidence_path = ROOT / "evidence" / "surfaces" / "personal" / filename
        evidence = json.loads(evidence_path.read_text())
        schema = json.loads(
            (ROOT / "schemas" / "s7-personal-surface-result-v1.schema.json").read_text()
        )
        validate_document(evidence, schema)
        self.assertEqual(evidence["surface_id"], surface_id)
        self.assertEqual(evidence["canonical_source"]["path"], source_path)
        self.assertEqual(
            evidence["canonical_source"]["digest"],
            _digest_bytes((ROOT.parent / source_path).read_bytes()),
        )
        self.assertEqual(evidence["acceptance"]["selector"], selector)
        self.assertEqual(evidence["acceptance"]["result"], "passed")
        self.assertEqual(
            evidence["acceptance"]["command"],
            "PYTHONPATH=agent-workflows/src python3 "
            "agent-workflows/tests/test_s7_personal_surfaces.py %s -v" % selector,
        )
        self.assertEqual(
            evidence["claim_boundary"],
            {
                "source_fixture_only": True,
                "live_apply": False,
                "build": False,
                "runtime_compatibility": "unverified",
            },
        )
        return evidence

    def test_model_policy_surface(self) -> None:
        policy_path = ROOT / "manifests" / "model-policy.json"
        receipt = CONFIG_INSTALLER.effective_model_fixture_receipt(policy_path)
        self.assertEqual(receipt["model"], "gpt-5.6-luna")
        self.assertEqual(receipt["reasoning_effort"], "max")
        self.assertFalse(receipt["automatic_fallback"])
        self.assertEqual(receipt["config_apply"], "explicit-user-command-only")
        self.assertEqual(receipt["unsupported_value_action"], "stop-without-substitution")
        self.assertEqual(receipt["runtime_compatibility"], "unverified")
        self.assertTrue(receipt["fixture_only"])
        self.assertEqual(receipt["source_digest"], _digest_bytes(policy_path.read_bytes()))

        canonical = json.loads(policy_path.read_text())
        mutations = []
        fallback = copy.deepcopy(canonical)
        fallback["automatic_fallback"] = True
        mutations.append(fallback)
        substitution = copy.deepcopy(canonical)
        substitution["default"]["model"] = "gpt-5.6-sol"
        mutations.append(substitution)
        reasoning = copy.deepcopy(canonical)
        reasoning["default"]["reasoning_effort"] = "high"
        mutations.append(reasoning)
        unsupported = copy.deepcopy(canonical)
        unsupported["unsupported_value_action"] = "substitute"
        mutations.append(unsupported)
        implicit = copy.deepcopy(canonical)
        implicit["config_apply"] = "automatic"
        mutations.append(implicit)
        with tempfile.TemporaryDirectory() as temporary:
            mutated = Path(temporary) / "model-policy.json"
            for mutation in mutations:
                mutated.write_text(json.dumps(mutation))
                with self.subTest(mutation=mutation):
                    with self.assertRaisesRegex(ConfigError, "stop without substitution"):
                        CONFIG_INSTALLER.effective_model_fixture_receipt(mutated)

        result, stdout, stderr = _capture_main(
            CONFIG_INSTALLER.main,
            ["--policy", str(policy_path), "effective-model-fixture"],
        )
        self.assertEqual((result, stderr), (0, ""))
        self.assertEqual(json.loads(stdout), receipt)
        evidence = self._assert_evidence(
            "model-policy.json",
            surface_id="surface.model-policy",
            source_path="agent-workflows/manifests/model-policy.json",
            selector="S7PersonalSurfaceTests.test_model_policy_surface",
        )
        self.assertEqual(evidence["proof"]["model"], receipt["model"])
        self.assertEqual(
            evidence["proof"]["runtime_compatibility"],
            receipt["runtime_compatibility"],
        )

    def test_config_installer_surface(self) -> None:
        policy_path = ROOT / "manifests" / "model-policy.json"
        secret = "S7-SECRET-MUST-NOT-PRINT"
        with tempfile.TemporaryDirectory() as temporary:
            temporary_root = Path(temporary)
            config = temporary_root / ".codex-fixture" / "config.toml"
            config.parent.mkdir()
            original = (
                "# app-owned prefix\r\n"
                'model = "S7-SECRET-MUST-NOT-PRINT" # managed\r\n'
                'model_reasoning_effort = "low"\r\n'
                'unknown_token = "S7-SECRET-MUST-NOT-PRINT"\r\n'
                '[projects."/private/fixture"]\r\n'
                'trust_level = "trusted"\r\n'
                "[notice]\r\n"
                "hide_model_migration_prompt = true\r\n"
            )
            config.write_bytes(original.encode())
            os.chmod(config, 0o600)

            result, stdout, stderr = _capture_main(
                CONFIG_INSTALLER.main,
                ["--policy", str(policy_path), "--config", str(config), "plan"],
            )
            self.assertEqual((result, stderr), (0, ""))
            self.assertNotIn(secret, stdout)
            self.assertNotIn(str(config), stdout)
            plan = json.loads(stdout)
            self.assertEqual(plan["path"], "<explicit-config-path>")
            self.assertEqual(plan["changed_keys"], ["model", "model_reasoning_effort"])
            self.assertEqual(plan["changes"]["model"]["current"], "<redacted>")

            result, stdout, stderr = _capture_main(
                CONFIG_INSTALLER.main,
                [
                    "--policy",
                    str(policy_path),
                    "--config",
                    str(config),
                    "apply",
                    "--expected-before-digest",
                    plan["before_digest"],
                    "--confirm-runtime-compatible",
                ],
            )
            self.assertEqual((result, stderr), (0, ""))
            self.assertNotIn(secret, stdout)
            receipt = json.loads(stdout)
            expected = original.replace(
                'model = "S7-SECRET-MUST-NOT-PRINT" # managed',
                'model = "gpt-5.6-luna" # managed',
            ).replace(
                'model_reasoning_effort = "low"',
                'model_reasoning_effort = "max"',
            )
            self.assertEqual(config.read_bytes(), expected.encode())
            backup = Path(receipt["backup_path"])
            self.assertEqual(backup.read_bytes(), original.encode())

            result, stdout, stderr = _capture_main(
                CONFIG_INSTALLER.main,
                ["--policy", str(policy_path), "--config", str(config), "doctor"],
            )
            self.assertEqual((result, stderr), (0, ""))
            self.assertNotIn(secret, stdout)
            self.assertTrue(json.loads(stdout)["matches_policy"])

            result, stdout, stderr = _capture_main(
                CONFIG_INSTALLER.main,
                [
                    "--policy",
                    str(policy_path),
                    "--config",
                    str(config),
                    "rollback",
                    "--backup",
                    str(backup),
                    "--expected-current-digest",
                    receipt["after_digest"],
                    "--expected-current-identity",
                    receipt["after_identity"],
                ],
            )
            self.assertEqual((result, stderr), (0, ""))
            self.assertNotIn(secret, stdout)
            self.assertEqual(config.read_bytes(), original.encode())

            stale_manager = CodexConfigManager(config, policy_path)
            stale_plan = stale_manager.plan()
            config.write_bytes(original.replace("app-owned prefix", "concurrent owner").encode())
            with self.assertRaisesRegex(ConfigError, "changed after plan"):
                stale_manager.apply(
                    expected_before_digest=stale_plan["before_digest"],
                    runtime_compatibility_confirmed=True,
                )

            config.write_bytes(original.encode())
            identity_manager = CodexConfigManager(config, policy_path)
            identity_plan = identity_manager.plan()

            def replace_with_same_bytes(operation: str, _path: Path) -> None:
                if operation == "config-apply":
                    replacement = temporary_root / "same-bytes-replacement"
                    replacement.write_bytes(original.encode())
                    os.chmod(replacement, 0o600)
                    os.replace(replacement, config)

            identity_manager._before_commit = replace_with_same_bytes  # type: ignore[method-assign]
            with self.assertRaisesRegex(ConfigError, "identity changed during apply"):
                identity_manager.apply(
                    expected_before_digest=identity_plan["before_digest"],
                    runtime_compatibility_confirmed=True,
                    timestamp="20260905T151500Z",
                )

            symlink = temporary_root / ".codex-fixture" / "linked-config.toml"
            target = temporary_root / "app-owned-target.toml"
            target.write_text('model = "untouched"\n')
            symlink.symlink_to(target)
            with self.assertRaisesRegex(ConfigError, "symlink"):
                CodexConfigManager(symlink, policy_path).plan()
            self.assertEqual(target.read_text(), 'model = "untouched"\n')

        result, stdout, stderr = _capture_main(
            CONFIG_INSTALLER.main,
            ["--policy", str(policy_path), "plan"],
        )
        self.assertEqual(result, 2)
        self.assertEqual(stdout, "")
        self.assertIn("explicit --config", stderr)
        source = (ROOT / "config-installer.py").read_text()
        self.assertNotIn("Path.home", source)
        self.assertNotIn("CODEX_HOME", source)
        self.assertNotIn("~/.codex", source)

        evidence = self._assert_evidence(
            "config-installer-guardrail.json",
            surface_id="surface.config-installer-guardrail",
            source_path="agent-workflows/config-installer.py",
            selector="S7PersonalSurfaceTests.test_config_installer_surface",
        )
        self.assertEqual(
            evidence["proof"]["managed_keys"],
            ["model", "model_reasoning_effort"],
        )
        self.assertFalse(evidence["proof"]["actual_user_config_accessed"])
        self.assertFalse(evidence["proof"]["secret_output"])

    def test_distribution_manifest_surface(self) -> None:
        path = ROOT / "manifests" / "distribution.json"
        manifest = json.loads(path.read_text())
        _validate_distribution_manifest(manifest)
        self.assertEqual(
            [item["id"] for item in manifest["ownership_classes"]],
            _OWNERSHIP_CLASSES,
        )
        self.assertFalse(manifest["live_installation_claimed"])
        _require_managed_target(
            manifest,
            ".codex/skills/approve-objective",
            "managed_destination",
        )
        for target, ownership_class in (
            (".codex/skills/private", "secret_bearing_state"),
            (".codex/skills/private", "unmanaged_user_state"),
            (".codex/config.toml", "app_owned_runtime"),
            (".codex/cache", "cache"),
            (".codex/skills/not-an-owned-child", "managed_destination"),
        ):
            with self.subTest(target=target, ownership_class=ownership_class):
                with self.assertRaises(ValueError):
                    _require_managed_target(manifest, target, ownership_class)

        multi_owner = copy.deepcopy(manifest)
        multi_owner["lifecycle_owner"] = [
            "flake-my-claude.agent-workflows",
            "provider-application",
        ]
        with self.assertRaisesRegex(ValueError, "exactly one lifecycle owner"):
            _validate_distribution_manifest(multi_owner)
        collapsed = copy.deepcopy(manifest)
        collapsed["ownership_classes"][5]["id"] = "app_owned_runtime"
        with self.assertRaisesRegex(ValueError, "collapsed"):
            _validate_distribution_manifest(collapsed)

        evidence = self._assert_evidence(
            "owner-distribution-manifest.json",
            surface_id="surface.owner-distribution-manifest",
            source_path="agent-workflows/manifests/distribution.json",
            selector="S7PersonalSurfaceTests.test_distribution_manifest_surface",
        )
        self.assertEqual(evidence["proof"]["ownership_classes"], _OWNERSHIP_CLASSES)
        self.assertFalse(evidence["proof"]["live_installation_claimed"])

    def test_provider_projection_surface(self) -> None:
        source = {
            "path": "agent-workflows/provider-projection.py",
            "class": "portable_source",
            "version": "v1",
            "digest": _digest_bytes((ROOT / "provider-projection.py").read_bytes()),
        }
        profile_without_digest = {
            "path": "agent-workflows/fixtures/codex-capability-profile-v1.json",
            "version": "v1",
            "provider": "codex",
            "supported_semantics": ["permissions", "skills"],
        }
        profile = {
            **profile_without_digest,
            "digest": _digest_document(profile_without_digest),
        }
        source_set_digest = _digest_document([source])
        artifact_without_digest = {
            "path": "agent-workflows/generated/codex-native-projection-v1.json",
            "version": "v1",
            "provider": "codex",
            "source_set_digest": source_set_digest,
            "target_profile_digest": profile["digest"],
            "semantics": ["hooks", "permissions", "skills"],
        }
        artifact = {
            **artifact_without_digest,
            "digest": _digest_document(artifact_without_digest),
        }
        request = {
            "provider": "codex",
            "capability_profile": profile,
            "sources": [source],
            "semantics": ["hooks", "permissions", "skills"],
            "staged_artifact": artifact,
        }

        with tempfile.TemporaryDirectory() as temporary:
            provider_state = Path(temporary) / "provider-state.json"
            sentinel = b"app-owned provider state\n"
            provider_state.write_bytes(sentinel)
            plan = PROVIDER_PROJECTION.plan_provider_projection(request)
            self.assertEqual(provider_state.read_bytes(), sentinel)
        self.assertTrue(plan["staged"])
        self.assertEqual(plan["compatibility"], "partial")
        self.assertFalse(plan["semantic_parity"])
        self.assertEqual(plan["supported_semantics"], ["permissions", "skills"])
        self.assertEqual(plan["exclusions"], ["hooks"])
        self.assertEqual(plan["staged_artifact"], artifact)

        for forged in (
            {**request, "staged_artifact": None},
            {
                **request,
                "staged_artifact": {
                    **artifact,
                    "digest": "sha256:" + "0" * 64,
                },
            },
            {
                **request,
                "staged_artifact": {
                    **artifact,
                    "source_set_digest": "sha256:" + "0" * 64,
                },
            },
            {
                **request,
                "staged_artifact": {
                    **artifact,
                    "semantics": ["permissions", "skills"],
                },
            },
            {
                **request,
                "capability_profile": {
                    **profile,
                    "supported_semantics": ["hooks", "permissions", "skills"],
                },
            },
        ):
            with self.subTest(forged=forged):
                with self.assertRaises(DistributionPlanningError):
                    PROVIDER_PROJECTION.plan_provider_projection(forged)

        source_text = (ROOT / "provider-projection.py").read_text()
        tree = ast.parse(source_text)
        imported_roots = {
            alias.name.split(".", 1)[0]
            for node in ast.walk(tree)
            if isinstance(node, (ast.Import, ast.ImportFrom))
            for alias in node.names
        }
        self.assertTrue(imported_roots.isdisjoint({"os", "shutil", "socket", "subprocess"}))
        forbidden_calls = {
            "chmod",
            "mkdir",
            "open",
            "rename",
            "replace",
            "touch",
            "unlink",
            "write_bytes",
            "write_text",
        }
        self.assertFalse(
            {
                node.func.attr
                for node in ast.walk(tree)
                if isinstance(node, ast.Call) and isinstance(node.func, ast.Attribute)
            }
            & forbidden_calls
        )

        evidence = self._assert_evidence(
            "provider-projection.json",
            surface_id="surface.provider-projection",
            source_path="agent-workflows/provider-projection.py",
            selector="S7PersonalSurfaceTests.test_provider_projection_surface",
        )
        self.assertEqual(evidence["proof"]["unsupported_semantics"], ["hooks"])
        self.assertFalse(evidence["proof"]["semantic_parity"])
        self.assertFalse(evidence["proof"]["provider_state_written"])


if __name__ == "__main__":
    unittest.main()
