import copy
import json
import sys
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "src"))

from ai_agent_workflow.schema_validation import (  # noqa: E402
    SchemaValidationError,
    validate_document,
)


DIGEST = "sha256:" + "a" * 64
APPROVAL_DIGEST = (
    "sha256:2f0ae19fac7b90778d1b5713c51071f1ab7e96ce179237a1e5892cf320dfaefb"
)
THREAT_PROFILE_DIGEST = (
    "sha256:81d2c7823b100e00f3d68e8f2c0877507064ffdc6e935f822130510106cfc955"
)
RESIDUALS = [
    "a malicious external process running as the same login UID may rename a project ancestor",
    "SIGKILL of the trusted parent after a publication syscall and before bookkeeping may leave bytes in a detached tree",
]


def load_schema(name):
    return json.loads((ROOT / "schemas" / name).read_text(encoding="utf-8"))


def registry(schemas):
    result = {schema["$id"]: schema for schema in schemas.values()}
    for schema in schemas.values():
        result.update(schema.get("$defs", {}))
    return result


def threat_profile():
    return {
        "schema": "macos-practical-threat-profile/v1",
        "profile_id": "macos-parent-writer-practical/v1",
        "approval_digest": APPROVAL_DIGEST,
        "trusted": [
            "Codex host",
            "parent Orchestrator",
            "same-UID parent runtime process",
        ],
        "adversarial": [
            "sandboxed task child",
            "malformed or stale caller input",
            "path substitution available to the sandboxed task child",
        ],
        "accepted_residuals": copy.deepcopy(RESIDUALS),
        "guarantees": [
            "sandboxed task child cannot read broker state",
            "sandboxed task child cannot mutate broker state",
            "task write scope is disjoint from broker state",
            "trusted parent authenticates the receipt and rejects caller-authored safety labels",
        ],
        "not_guaranteed": copy.deepcopy(RESIDUALS),
    }


def boundary():
    return {
        "writer": "trusted-same-uid-parent",
        "task_child_is_writer": False,
        "broker_state_root": "/workspace/.agent-workflow/runtime-e-broker",
        "task_write_roots": ["/workspace/out"],
        "task_write_scope_disjoint": True,
        "descriptor_anchored": True,
        "no_follow": True,
        "device_inode_revalidated": True,
        "pre_post_attachment_checks": True,
        "rollback": "defense-in-depth",
        "arbitrary_same_uid_atomicity": False,
        "publication_sigkill_atomicity": False,
    }


def release():
    return {
        "schema": "macos-task-process-release/v2",
        "engine": "/usr/bin/sandbox-exec",
        "profile_imports": ["dyld-support.sb"],
        "workspace_identity": "/workspace",
        "broker_state_root": "/workspace/.agent-workflow/runtime-e-broker",
        "system_read_roots": ["/System"],
        "runtime_read_roots": ["/usr/bin"],
        "project_read_roots": ["/workspace"],
        "write_roots": ["/workspace/out"],
        "command": {"argv": ["/usr/bin/python3", "task.py"], "cwd": "/workspace"},
        "execution_closure_ref": {"id": "package-1", "digest": DIGEST},
        "profile": "(version 1)\n(deny default)\n",
        "profile_digest": DIGEST,
        "os_isolation_enforced": True,
        "release_rule": "broker-only",
        "threat_profile": threat_profile(),
        "threat_profile_digest": THREAT_PROFILE_DIGEST,
        "state_writer_boundary": boundary(),
        "release_digest": DIGEST,
    }


def receipt():
    return {
        "schema": "macos-task-process-receipt/v2",
        "receipt_id": DIGEST,
        "broker_id": DIGEST,
        "e2_package_digest": DIGEST,
        "execution_closure_ref": {"id": "package-1", "digest": DIGEST},
        "command_identity": {
            "argv": ["/usr/bin/python3", "task.py"],
            "argv_digest": DIGEST,
            "cwd": "/workspace",
            "environment_digest": DIGEST,
        },
        "process_identity": {
            "pid": 100,
            "process_group_id": 100,
            "birth_token": "boot:100",
            "boot_identity": "boot-1",
        },
        "sandbox_identity": {
            "engine": "/usr/bin/sandbox-exec",
            "profile_imports": ["dyld-support.sb"],
            "profile_digest": DIGEST,
            "release_digest": DIGEST,
        },
        "declared_roots": {
            "project": ["/workspace"],
            "system": ["/System"],
            "runtime": ["/usr/bin"],
            "write": ["/workspace/out"],
        },
        "operations": [
            {
                "operation": "content-read",
                "path": "/workspace/input.txt",
                "expected": "allowed",
                "result": "allowed",
                "exit_code": 0,
            },
            {
                "operation": "enumerate",
                "path": "/private/outside",
                "expected": "denied",
                "result": "denied",
                "exit_code": 1,
            },
        ],
        "terminal_receipt_digest": DIGEST,
        "os_isolation_enforced": True,
        "enforcement": "macos-positive-read-allow-list",
        "caller_observations_accepted": False,
        "threat_profile": threat_profile(),
        "threat_profile_digest": THREAT_PROFILE_DIGEST,
        "state_writer_boundary": boundary(),
        "limitations_acknowledged": True,
        "receipt_digest": DIGEST,
        "broker_mac": "b" * 64,
    }


def receipt_ref():
    return {
        "schema": "macos-task-process-receipt-ref/v2",
        "receipt_id": DIGEST,
        "receipt_digest": DIGEST,
        "broker_id": DIGEST,
        "threat_profile_digest": THREAT_PROFILE_DIGEST,
    }


class MacOSTaskProcessSchemaTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.schemas = {
            "profile": load_schema("macos-practical-threat-profile-v1.schema.json"),
            "release": load_schema("macos-task-process-release-v2.schema.json"),
            "receipt": load_schema("macos-task-process-receipt-v2.schema.json"),
            "ref": load_schema("macos-task-process-receipt-ref-v2.schema.json"),
        }
        cls.registry = registry(cls.schemas)

    def assert_invalid(self, value, schema_name):
        with self.assertRaises(SchemaValidationError):
            validate_document(value, self.schemas[schema_name], self.registry)

    def test_positive_matrix_and_frozen_top_level_fields(self):
        fixtures = {
            "profile": threat_profile(),
            "release": release(),
            "receipt": receipt(),
            "ref": receipt_ref(),
        }
        expected = {
            "profile": {
                "schema", "profile_id", "approval_digest", "trusted",
                "adversarial", "accepted_residuals", "guarantees",
                "not_guaranteed",
            },
            "release": set(release()),
            "receipt": set(receipt()),
            "ref": {
                "schema", "receipt_id", "receipt_digest", "broker_id",
                "threat_profile_digest",
            },
        }
        for name, fixture in fixtures.items():
            with self.subTest(name=name):
                validate_document(fixture, self.schemas[name], self.registry)
                self.assertFalse(self.schemas[name]["additionalProperties"])
                self.assertEqual(set(self.schemas[name]["required"]), expected[name])

    def test_every_declared_object_schema_is_closed(self):
        def visit(value, path):
            if isinstance(value, dict):
                if value.get("type") == "object":
                    self.assertIs(
                        value.get("additionalProperties"),
                        False,
                        "%s is not closed" % path,
                    )
                for key, item in value.items():
                    visit(item, "%s.%s" % (path, key))
            elif isinstance(value, list):
                for index, item in enumerate(value):
                    visit(item, "%s[%d]" % (path, index))

        for name, schema in self.schemas.items():
            with self.subTest(name=name):
                visit(schema, name)

    def test_profile_rejects_omitted_extra_reordered_or_reworded_residuals(self):
        omitted = threat_profile()
        omitted.pop("accepted_residuals")
        self.assert_invalid(omitted, "profile")

        extra = threat_profile()
        extra["accepted_residuals_extra"] = copy.deepcopy(RESIDUALS)
        self.assert_invalid(extra, "profile")

        reordered = threat_profile()
        reordered["accepted_residuals"].reverse()
        self.assert_invalid(reordered, "profile")

        reworded = threat_profile()
        reworded["accepted_residuals"][0] += "."
        self.assert_invalid(reworded, "profile")

    def test_rejects_forged_approval_or_profile_digest(self):
        forged_approval = threat_profile()
        forged_approval["approval_digest"] = DIGEST
        self.assert_invalid(forged_approval, "profile")

        for name, fixture in (("release", release()), ("receipt", receipt()), ("ref", receipt_ref())):
            with self.subTest(name=name):
                fixture["threat_profile_digest"] = DIGEST
                self.assert_invalid(fixture, name)

    def test_rejects_false_strengthening_and_scope_claims(self):
        for field in (
            "arbitrary_same_uid_atomicity",
            "publication_sigkill_atomicity",
        ):
            for name, fixture in (("release", release()), ("receipt", receipt())):
                with self.subTest(name=name, field=field):
                    fixture["state_writer_boundary"][field] = True
                    self.assert_invalid(fixture, name)

        false_non_overlap = release()
        false_non_overlap["state_writer_boundary"]["task_write_scope_disjoint"] = False
        self.assert_invalid(false_non_overlap, "release")

        separate_principal = receipt()
        separate_principal["state_writer_boundary"]["writer"] = "distinct-os-principal"
        self.assert_invalid(separate_principal, "receipt")

    def test_rejects_v1_v2_grafting_and_mixing(self):
        cases = []
        v1_release = release()
        v1_release["schema"] = "macos-task-process-release/v1"
        cases.append(("release", v1_release))

        v1_receipt = receipt()
        v1_receipt["schema"] = "macos-task-process-receipt/v1"
        cases.append(("receipt", v1_receipt))

        v1_ref = receipt_ref()
        v1_ref["schema"] = "macos-task-process-receipt-ref/v1"
        cases.append(("ref", v1_ref))

        grafted = release()
        grafted["legacy_unprofiled"] = True
        cases.append(("release", grafted))

        for name, fixture in cases:
            with self.subTest(name=name):
                self.assert_invalid(fixture, name)


if __name__ == "__main__":
    unittest.main()
