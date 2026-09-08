import copy
import hashlib
import hmac
import json
import os
import shutil
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "src"))
sys.path.insert(0, str(ROOT / "tests"))

from ai_agent_workflow.execution_group import (  # noqa: E402
    ArtifactCandidateBuilder,
    ExecutionGroupV1,
)
from ai_agent_workflow.macos_task_process import (  # noqa: E402
    MacOSTaskProcessBroker,
    MacOSTaskProcessError,
    _AnchoredBrokerState,
    _canonical,
    _compile_macos_task_process_release_v1,
    _digest,
    compile_macos_task_process_release,
)
from test_execution_group import HEAD, authority, inputs  # noqa: E402


def _digest_bytes(path):
    return "sha256:" + hashlib.sha256(Path(path).read_bytes()).hexdigest()


class BrokerStateContainmentTests(unittest.TestCase):
    def _assert_empty(self, path):
        self.assertEqual(list(Path(path).rglob("*")), [])

    def _assert_no_receipts(self, path):
        self.assertEqual(list(Path(path).rglob("*.json")), [])
        self.assertEqual(list(Path(path).rglob("*.bin")), [])

    def _assert_no_files_or_bytes(self, path):
        files = [item for item in Path(path).rglob("*") if item.is_file()]
        self.assertEqual(files, [])

    def _detaching_state(self, prefix):
        temporary = tempfile.TemporaryDirectory(prefix=prefix)
        root = Path(temporary.name).resolve()
        project = root / "project"
        outside = root / "outside"
        project.mkdir()
        outside.mkdir()
        agent = project / ".agent-workflow"
        state = _AnchoredBrokerState(
            str(project), str(agent / "runtime-e-broker")
        )
        return temporary, project, outside, agent, state

    def _detach_on_attachment_check(
        self, state, agent, outside, operation, *, call_number=1
    ):
        original = state._assert_attached
        calls = 0

        def detach(parent_fd, parent_parts):
            nonlocal calls
            calls += 1
            if calls == call_number:
                agent.rename(outside / "detached-agent-workflow")
            return original(parent_fd, parent_parts)

        with mock.patch.object(state, "_assert_attached", side_effect=detach):
            with self.assertRaisesRegex(
                MacOSTaskProcessError,
                "detached|no longer available|state ancestor",
            ):
                operation(state)
        self.assertEqual(calls, call_number)

    def test_constructor_is_write_free_and_root_symlink_is_refused(self):
        with tempfile.TemporaryDirectory(prefix="broker-root-link-") as temporary:
            root = Path(temporary).resolve()
            project = root / "project"
            outside = root / "outside"
            project.mkdir()
            outside.mkdir()
            (project / ".agent-workflow").symlink_to(
                outside, target_is_directory=True
            )
            with self.assertRaisesRegex(MacOSTaskProcessError, "symlink"):
                MacOSTaskProcessBroker(
                    project / ".agent-workflow/runtime-e-broker",
                    capability=b"r" * 32,
                )
            self._assert_empty(outside)
            self._assert_no_receipts(project)

    def test_nested_and_broken_links_and_non_directory_are_refused(self):
        cases = ("nested-link", "broken-link", "non-directory")
        for case in cases:
            with self.subTest(case=case), tempfile.TemporaryDirectory(
                prefix="broker-state-shape-"
            ) as temporary:
                root = Path(temporary).resolve()
                project = root / "project"
                outside = root / "outside"
                project.mkdir()
                outside.mkdir()
                agent = project / ".agent-workflow"
                agent.mkdir()
                target = agent / "runtime-e-broker"
                if case == "nested-link":
                    target.symlink_to(outside, target_is_directory=True)
                elif case == "broken-link":
                    target.symlink_to(root / "missing", target_is_directory=True)
                else:
                    target.write_text("not a directory", encoding="utf-8")
                with self.assertRaisesRegex(
                    MacOSTaskProcessError, "symlink or non-directory"
                ):
                    MacOSTaskProcessBroker(target, capability=b"r" * 32)
                self._assert_empty(outside)
                self._assert_no_receipts(project)

    def test_controlled_ancestor_replacement_refuses_before_outside_write(self):
        with tempfile.TemporaryDirectory(prefix="broker-state-race-") as temporary:
            root = Path(temporary).resolve()
            project = root / "project"
            outside = root / "outside"
            project.mkdir()
            outside.mkdir()
            agent = project / ".agent-workflow"
            agent.mkdir()
            original_stat = os.stat
            replaced = False

            def replace_after_stat(path, *args, **kwargs):
                nonlocal replaced
                result = original_stat(path, *args, **kwargs)
                if (
                    not replaced
                    and path == ".agent-workflow"
                    and kwargs.get("dir_fd") is not None
                ):
                    replaced = True
                    agent.rename(project / ".agent-workflow-held")
                    agent.symlink_to(outside, target_is_directory=True)
                return result

            with mock.patch(
                "ai_agent_workflow.macos_task_process.os.stat",
                side_effect=replace_after_stat,
            ):
                with self.assertRaisesRegex(
                    MacOSTaskProcessError, "changed during traversal"
                ):
                    _AnchoredBrokerState(
                        str(project),
                        str(project / ".agent-workflow/runtime-e-broker"),
                    )
            self.assertTrue(replaced)
            self._assert_empty(outside)
            self._assert_no_receipts(project)

    def test_attached_state_is_revalidated_before_each_write(self):
        with tempfile.TemporaryDirectory(prefix="broker-state-detach-") as temporary:
            root = Path(temporary).resolve()
            project = root / "project"
            outside = root / "outside"
            project.mkdir()
            outside.mkdir()
            agent = project / ".agent-workflow"
            state = _AnchoredBrokerState(
                str(project), str(agent / "runtime-e-broker")
            )
            agent.rename(project / ".agent-workflow-held")
            agent.symlink_to(outside, target_is_directory=True)
            try:
                with self.assertRaisesRegex(MacOSTaskProcessError, "symlink"):
                    state.create("isolation-receipts/receipt.json", b"not-accepted")
            finally:
                state.close()
            self._assert_empty(outside)
            self._assert_no_receipts(project)

    def test_runner_capture_and_isolation_namespaces_reject_symlinks(self):
        destinations = (
            "command-runner",
            "command-runner/captures",
            "isolation-receipts",
        )
        for destination in destinations:
            with self.subTest(destination=destination), tempfile.TemporaryDirectory(
                prefix="broker-state-child-link-"
            ) as temporary:
                root = Path(temporary).resolve()
                project = root / "project"
                outside = root / "outside"
                project.mkdir()
                outside.mkdir()
                state_path = project / ".agent-workflow/runtime-e-broker"
                state = _AnchoredBrokerState(str(project), str(state_path))
                link = state_path / destination
                link.parent.mkdir(parents=True, exist_ok=True)
                link.symlink_to(outside, target_is_directory=True)
                relative = destination + "/receipt.json"
                try:
                    with self.assertRaisesRegex(MacOSTaskProcessError, "symlink"):
                        state.replace(relative, b"not-accepted")
                finally:
                    state.close()
                self._assert_empty(outside)
                self.assertEqual(list(state_path.rglob("*.json")), [])

    def test_post_parent_detach_refuses_runner_capture_and_isolation_writes(self):
        operations = (
            (
                "runner-create",
                lambda state: state.create(
                    "command-runner/receipts/reserved.json", b"runner"
                ),
            ),
            (
                "runner-replace",
                lambda state: state.replace(
                    "command-runner/receipts/current.json", b"runner-update"
                ),
            ),
            (
                "capture-replace",
                lambda state: state.replace(
                    "command-runner/captures/stdout/capture.bin", b"capture"
                ),
            ),
            (
                "isolation-create",
                lambda state: state.create(
                    "isolation-receipts/isolation.json", b"isolation"
                ),
            ),
        )
        for name, operation in operations:
            with self.subTest(operation=name):
                temporary, project, outside, agent, state = self._detaching_state(
                    "broker-post-parent-"
                )
                try:
                    self._detach_on_attachment_check(
                        state, agent, outside, operation
                    )
                finally:
                    state.close()
                try:
                    self._assert_no_files_or_bytes(outside)
                    self._assert_no_receipts(project)
                finally:
                    temporary.cleanup()

    def test_detach_after_staging_cleans_unpublished_replace_bytes(self):
        temporary, project, outside, agent, state = self._detaching_state(
            "broker-pre-replace-"
        )
        try:
            self._detach_on_attachment_check(
                state,
                agent,
                outside,
                lambda value: value.replace(
                    "command-runner/captures/stderr/capture.bin", b"sensitive"
                ),
                call_number=3,
            )
        finally:
            state.close()
        try:
            self._assert_no_files_or_bytes(outside)
            self._assert_no_receipts(project)
        finally:
            temporary.cleanup()

    def test_detach_after_publication_rolls_back_new_state_bytes(self):
        operations = (
            (
                "runner-create",
                lambda state: state.create(
                    "command-runner/receipts/reserved.json", b"runner"
                ),
            ),
            (
                "capture-replace",
                lambda state: state.replace(
                    "command-runner/captures/stdout/capture.bin", b"capture"
                ),
            ),
            (
                "isolation-create",
                lambda state: state.create(
                    "isolation-receipts/isolation.json", b"isolation"
                ),
            ),
        )
        for name, operation in operations:
            with self.subTest(operation=name):
                temporary, project, outside, agent, state = self._detaching_state(
                    "broker-post-publication-"
                )
                try:
                    # For both staged create and replace, the fourth check is
                    # immediately after the destination becomes visible.
                    self._detach_on_attachment_check(
                        state, agent, outside, operation, call_number=4
                    )
                finally:
                    state.close()
                try:
                    self._assert_no_files_or_bytes(outside)
                    self._assert_no_receipts(project)
                finally:
                    temporary.cleanup()


class MacOSTaskProcessPracticalContractTests(unittest.TestCase):
    def _closure(self, workspace, write_paths=None):
        python = str(Path(sys.executable).resolve(strict=True))
        value = {
            "schema": "execution-package-closure/v2",
            "package_id": "broker-contract-package",
            "contract_version": "workflow-execution/v2",
            "workspace_identity": str(workspace),
            "candidate_ref": {"id": "candidate", "digest": "sha256:" + "a" * 64},
            "test_refs": [{"id": "broker", "digest": "sha256:" + "b" * 64}],
            "fixture_refs": [],
            "schema_refs": [{"id": "broker-v2", "digest": "sha256:" + "c" * 64}],
            "config_refs": [],
            "lock_refs": [{"id": "python", "digest": "sha256:" + "d" * 64}],
            "toolchain": {"executable_digest": _digest_bytes(python), "identity": "python"},
            "command": {"argv": [python, "-c", "pass"], "cwd": str(workspace)},
            "environment": {},
            "isolation": {
                "cwd": str(workspace),
                "temporary_namespace": "tmp",
                "output_namespace": "out",
            },
            "resource_claims": {
                "read_paths": ["src"],
                "write_paths": list(write_paths or ["out"]),
                "exclusive_resources": [],
            },
            "supervision": {
                "timeout_seconds": 10,
                "grace_seconds": 1,
                "signals": ["TERM", "KILL"],
                "heartbeat_seconds": 1,
                "terminal_publication_seconds": 1,
            },
            "external_input_refs": [],
        }
        value["closure_digest"] = _digest(value)
        return value

    def _compile(self, workspace, write_paths=None, *, legacy=False):
        closure = self._closure(workspace, write_paths)
        compiler = (
            _compile_macos_task_process_release_v1
            if legacy
            else compile_macos_task_process_release
        )
        with mock.patch(
            "ai_agent_workflow.macos_task_process.sys.platform", "darwin"
        ), mock.patch.object(Path, "is_file", return_value=True):
            release = compiler(
                closure,
                broker_state_root=str(
                    workspace / ".agent-workflow" / "runtime-e-broker"
                ),
                system_read_roots=["/usr/bin"],
                runtime_read_roots=[str(Path(sys.executable).resolve().parent)],
            )
        package = {
            "execution_closure": closure,
            "execution_closure_ref": {
                "id": closure["package_id"],
                "digest": closure["closure_digest"],
            },
            "task_process_release": release,
        }
        return package, release

    def _seal_v2_fixture(self, workspace):
        package, release = self._compile(workspace)
        broker = MacOSTaskProcessBroker(
            release["broker_state_root"], capability=b"v" * 32, boot_id="boot-v2"
        )
        broker._state_for_release(release)
        terminal = {"receipt_digest": "sha256:" + "e" * 64}
        receipt = broker._seal(
            package,
            release,
            {
                "pid": 42,
                "process_group_id": 42,
                "birth_token": "birth-42",
                "boot_identity": "boot-v2",
            },
            [
                {
                    "operation": "content-read",
                    "path": str(workspace / "src"),
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
            terminal,
        )
        ref = {
            "schema": "macos-task-process-receipt-ref/v2",
            "receipt_id": receipt["receipt_id"],
            "receipt_digest": receipt["receipt_digest"],
            "broker_id": broker.broker_id,
            "threat_profile_digest": release["threat_profile_digest"],
        }
        return package, release, broker, receipt, ref

    def _replace_receipt(self, broker, receipt):
        unsigned = {
            key: copy.deepcopy(value)
            for key, value in receipt.items()
            if key not in {"receipt_digest", "broker_mac"}
        }
        receipt["receipt_digest"] = _digest(unsigned)
        authenticated = {
            key: copy.deepcopy(value)
            for key, value in receipt.items()
            if key != "broker_mac"
        }
        receipt["broker_mac"] = hmac.new(
            broker._capability, _canonical(authenticated), hashlib.sha256
        ).hexdigest()
        broker._state.replace(
            "isolation-receipts/"
            + receipt["receipt_id"].removeprefix("sha256:")
            + ".json",
            _canonical(receipt),
        )

    def test_new_release_is_exact_practical_v2_and_scope_is_disjoint(self):
        with tempfile.TemporaryDirectory(prefix="broker-v2-release-") as temporary:
            workspace = Path(temporary).resolve()
            (workspace / "src").mkdir()
            (workspace / "out").mkdir()
            _, release = self._compile(workspace)
            self.assertEqual(release["schema"], "macos-task-process-release/v2")
            self.assertEqual(
                release["threat_profile"]["profile_id"],
                "macos-parent-writer-practical/v1",
            )
            self.assertEqual(
                release["threat_profile"]["approval_digest"],
                "sha256:2f0ae19fac7b90778d1b5713c51071f1ab7e96ce179237a1e5892cf320dfaefb",
            )
            boundary = release["state_writer_boundary"]
            self.assertEqual(boundary["writer"], "trusted-same-uid-parent")
            self.assertFalse(boundary["task_child_is_writer"])
            self.assertTrue(boundary["task_write_scope_disjoint"])
            self.assertFalse(boundary["arbitrary_same_uid_atomicity"])
            self.assertFalse(boundary["publication_sigkill_atomicity"])

    def test_equal_containing_and_nested_task_write_scope_refuse_before_state(self):
        cases = (
            ".agent-workflow/runtime-e-broker",
            ".agent-workflow",
            ".agent-workflow/runtime-e-broker/child",
        )
        for write_path in cases:
            with self.subTest(write_path=write_path), tempfile.TemporaryDirectory(
                prefix="broker-v2-overlap-"
            ) as temporary:
                workspace = Path(temporary).resolve()
                (workspace / "src").mkdir()
                with self.assertRaisesRegex(
                    MacOSTaskProcessError, "overlaps parent-owned broker state"
                ):
                    self._compile(workspace, [write_path])
                self.assertFalse((workspace / ".agent-workflow").exists())

    def test_v2_receipt_binds_profile_boundary_and_limitations(self):
        with tempfile.TemporaryDirectory(prefix="broker-v2-receipt-") as temporary:
            workspace = Path(temporary).resolve()
            (workspace / "src").mkdir()
            (workspace / "out").mkdir()
            package, release, broker, _, ref = self._seal_v2_fixture(workspace)
            try:
                verified = broker.verify_for_e3(package, ref)
                self.assertEqual(verified["schema"], "macos-task-process-receipt/v2")
                self.assertEqual(
                    verified["threat_profile"], release["threat_profile"]
                )
                self.assertEqual(
                    verified["state_writer_boundary"],
                    release["state_writer_boundary"],
                )
                self.assertTrue(verified["limitations_acknowledged"])
                self.assertNotIn("broker_mac", verified)
            finally:
                broker.close()

    def test_parent_authenticated_receipt_rejects_safety_label_forgery(self):
        mutations = (
            ("missing-residual", lambda value: value["threat_profile"]["accepted_residuals"].pop()),
            ("same-uid-atomic", lambda value: value["state_writer_boundary"].__setitem__("arbitrary_same_uid_atomicity", True)),
            ("sigkill-atomic", lambda value: value["state_writer_boundary"].__setitem__("publication_sigkill_atomicity", True)),
            ("scope-overlap", lambda value: value["state_writer_boundary"].__setitem__("task_write_scope_disjoint", False)),
            ("separate-principal", lambda value: value["state_writer_boundary"].__setitem__("writer", "distinct-os-principal")),
        )
        for name, mutate in mutations:
            with self.subTest(name=name), tempfile.TemporaryDirectory(
                prefix="broker-v2-forgery-"
            ) as temporary:
                workspace = Path(temporary).resolve()
                (workspace / "src").mkdir()
                (workspace / "out").mkdir()
                package, _, broker, receipt, ref = self._seal_v2_fixture(workspace)
                try:
                    mutate(receipt)
                    self._replace_receipt(broker, receipt)
                    ref["receipt_digest"] = receipt["receipt_digest"]
                    with self.assertRaisesRegex(
                        MacOSTaskProcessError,
                        "practical contract|exact E2 package",
                    ):
                        broker.verify_for_e3(package, ref)
                finally:
                    broker.close()

    def test_historical_v1_reader_is_exact_pair_and_never_practical_acceptance(self):
        with tempfile.TemporaryDirectory(prefix="broker-v1-history-") as temporary:
            workspace = Path(temporary).resolve()
            (workspace / "src").mkdir()
            (workspace / "out").mkdir()
            package, release = self._compile(workspace, legacy=True)
            broker = MacOSTaskProcessBroker(
                release["broker_state_root"],
                capability=b"h" * 32,
                boot_id="boot-v1",
            )
            state = broker._state_for_release(release)
            terminal_digest = "sha256:" + "e" * 64
            receipt_id = _digest(
                {
                    "e2_package_digest": _digest(package),
                    "terminal_receipt_digest": terminal_digest,
                    "profile_digest": release["profile_digest"],
                }
            )
            receipt = {
                "schema": "macos-task-process-receipt/v1",
                "receipt_id": receipt_id,
                "broker_id": broker.broker_id,
                "e2_package_digest": _digest(package),
                "execution_closure_ref": copy.deepcopy(package["execution_closure_ref"]),
                "command_identity": {
                    "argv": copy.deepcopy(release["command"]["argv"]),
                    "argv_digest": _digest(release["command"]["argv"]),
                    "cwd": release["command"]["cwd"],
                    "environment_digest": _digest(package["execution_closure"]["environment"]),
                },
                "process_identity": {
                    "pid": 1,
                    "process_group_id": 1,
                    "birth_token": "historical",
                    "boot_identity": "boot-v1",
                },
                "sandbox_identity": {
                    "engine": release["engine"],
                    "profile_imports": copy.deepcopy(release["profile_imports"]),
                    "profile_digest": release["profile_digest"],
                    "release_digest": release["release_digest"],
                },
                "declared_roots": {
                    "project": copy.deepcopy(release["project_read_roots"]),
                    "system": copy.deepcopy(release["system_read_roots"]),
                    "runtime": copy.deepcopy(release["runtime_read_roots"]),
                    "write": copy.deepcopy(release["write_roots"]),
                },
                "operations": [],
                "terminal_receipt_digest": terminal_digest,
                "os_isolation_enforced": True,
                "enforcement": "macos-positive-read-allow-list",
                "caller_observations_accepted": False,
            }
            receipt["receipt_digest"] = _digest(receipt)
            receipt["broker_mac"] = hmac.new(
                broker._capability, _canonical(receipt), hashlib.sha256
            ).hexdigest()
            state.create(
                "isolation-receipts/"
                + receipt_id.removeprefix("sha256:")
                + ".json",
                _canonical(receipt),
            )
            ref = {
                "schema": "macos-task-process-receipt-ref/v1",
                "receipt_id": receipt_id,
                "receipt_digest": receipt["receipt_digest"],
                "broker_id": broker.broker_id,
            }
            try:
                historical = broker.verify_historical_v1(package, ref)
                self.assertEqual(historical["status"], "legacy-unprofiled")
                self.assertFalse(historical["practical_profile_accepted"])
                with self.assertRaisesRegex(
                    MacOSTaskProcessError, "broker-only macOS execution"
                ):
                    broker.verify_for_e3(package, ref)
                mixed = copy.deepcopy(ref)
                mixed["schema"] = "macos-task-process-receipt-ref/v2"
                mixed["threat_profile_digest"] = "sha256:" + "0" * 64
                with self.assertRaisesRegex(
                    MacOSTaskProcessError, "exact parent-broker pair"
                ):
                    broker.verify_historical_v1(package, mixed)
            finally:
                broker.close()


@unittest.skipUnless(
    sys.platform == "darwin" and shutil.which("sandbox-exec"), "macOS sandbox-exec only"
)
class MacOSTaskProcessIntegrationTests(unittest.TestCase):
    def test_actual_adapter_denies_two_siblings_and_binds_v2_receipt(self):
        with tempfile.TemporaryDirectory(prefix="macos-task-process-") as temporary:
            root = Path(temporary).resolve()
            project = root / "selected-project"
            sibling_a = root / "confidential-a"
            sibling_b = root / "confidential-b"
            for path in (project, sibling_a, sibling_b):
                path.mkdir()
            (project / "src").mkdir()
            (project / "out").mkdir()
            allowed_file = project / "src" / "request.txt"
            sibling_file = sibling_a / "secret.txt"
            allowed_file.write_text("declared", encoding="utf-8")
            sibling_file.write_text("confidential-a", encoding="utf-8")
            (sibling_b / "secret.txt").write_text("confidential-b", encoding="utf-8")

            adapter = project / "src" / "task_adapter.py"
            adapter.write_text(
                """import json, os, sys
allowed, sibling_file, sibling_dir, output = sys.argv[1:]
results = {}
def attempt(name, operation):
    try:
        operation()
    except OSError as error:
        results[name] = {\"result\": \"denied\", \"errno\": error.errno}
    else:
        results[name] = {\"result\": \"allowed\"}
attempt(\"declared-content\", lambda: open(allowed, \"rb\").read())
attempt(\"sibling-content\", lambda: open(sibling_file, \"rb\").read())
attempt(\"sibling-stat\", lambda: os.stat(sibling_file))
attempt(\"sibling-enumerate\", lambda: os.listdir(sibling_dir))
expected = {
    \"declared-content\": \"allowed\", \"sibling-content\": \"denied\",
    \"sibling-stat\": \"denied\", \"sibling-enumerate\": \"denied\",
}
with open(output, \"w\", encoding=\"utf-8\") as stream:
    json.dump(results, stream, sort_keys=True)
if {key: value[\"result\"] for key, value in results.items()} != expected:
    raise SystemExit(23)
""",
                encoding="utf-8",
            )
            output = project / "out" / "task-result.json"
            python = str(Path(sys.executable).resolve(strict=True))
            runtime_root = str(
                Path("/Library/Developer/CommandLineTools").resolve(strict=True)
            )
            system_roots = sorted(
                {
                    path
                    for path in (
                        "/System",
                        "/usr/lib",
                        "/usr/share",
                        "/usr/bin",
                        "/bin",
                        "/dev",
                    )
                    if Path(path).is_dir()
                }
            )
            package_source = {
                "schema": "execution-package-input/v2",
                "package_id": "macos-e2-task",
                "contract_version": "workflow-execution/v2",
                "workspace_identity": str(project),
                "candidate_ref": {"id": "candidate", "digest": "sha256:" + "a" * 64},
                "test_refs": [
                    {"id": "sandbox-integration", "digest": _digest_bytes(adapter)}
                ],
                "fixture_refs": [],
                "schema_refs": [
                    {"id": "task-adapter", "digest": _digest_bytes(adapter)}
                ],
                "config_refs": [],
                "lock_refs": [{"id": "python-runtime", "digest": "sha256:" + "b" * 64}],
                "toolchain": {
                    "executable_digest": _digest_bytes(python),
                    "identity": "python3.9",
                },
                "command": {
                    "argv": [
                        python,
                        "-I",
                        "-S",
                        str(adapter),
                        str(allowed_file),
                        str(sibling_file),
                        str(sibling_b),
                        str(output),
                    ],
                    "cwd": str(project),
                },
                "environment": {},
                "isolation": {
                    "cwd": str(project),
                    "temporary_namespace": "tmp",
                    "output_namespace": "out",
                },
                "resource_claims": {
                    "read_paths": ["src"],
                    "write_paths": ["out"],
                    "exclusive_resources": [],
                },
                "supervision": {
                    "timeout_seconds": 10,
                    "grace_seconds": 1,
                    "signals": ["TERM", "KILL"],
                    "heartbeat_seconds": 1,
                    "terminal_publication_seconds": 1,
                },
                "external_input_refs": [],
            }
            bound = authority(idempotency_key="macos-broker-task")
            preflight = ExecutionGroupV1().compile("group.E.E1", inputs(), bound, HEAD)
            broker_root = project / ".agent-workflow" / "runtime-e-broker"
            e2 = ExecutionGroupV1().compile(
                "group.E.E2",
                inputs(
                    preflight=preflight,
                    preflight_digest=preflight["candidate_digest"],
                    execution_package_input=package_source,
                    assigned_role="worker",
                    assigned_worker="worker-macos",
                    output_path="out/task",
                    write_scope=["out"],
                    non_goals=["sibling access"],
                    acceptance=["OS denial"],
                    stop_conditions=["sandbox unavailable"],
                    task_process_mode="macos-positive-allowlist",
                    broker_state_root=str(broker_root),
                    system_read_roots=system_roots,
                    runtime_read_roots=[runtime_root],
                ),
                bound,
                HEAD,
            )
            self.assertEqual(e2["status"], "issued", e2)
            package = e2["package"]
            self.assertEqual(
                package["task_process_release"]["schema"],
                "macos-task-process-release/v2",
            )
            self.assertTrue(package["task_process_release"]["os_isolation_enforced"])
            self.assertEqual(
                package["task_process_release"]["release_rule"], "broker-only"
            )
            self.assertNotIn(str(sibling_a), package["task_process_release"]["profile"])
            self.assertNotIn(str(sibling_b), package["task_process_release"]["profile"])

            runner_package, runner_policy = ArtifactCandidateBuilder.runner_inputs(
                package
            )
            broker = MacOSTaskProcessBroker(
                broker_root, capability=b"p" * 32, boot_id="integration-boot"
            )
            executed = broker.execute(
                package,
                runner_package,
                runner_policy,
                probes=[
                    {
                        "operation": "content-read",
                        "path": str(allowed_file),
                        "expected": "allowed",
                    },
                    {
                        "operation": "stat",
                        "path": str(allowed_file),
                        "expected": "allowed",
                    },
                    {
                        "operation": "enumerate",
                        "path": str(project / "src"),
                        "expected": "allowed",
                    },
                    {
                        "operation": "content-read",
                        "path": str(sibling_file),
                        "expected": "denied",
                    },
                    {
                        "operation": "stat",
                        "path": str(sibling_file),
                        "expected": "denied",
                    },
                    {
                        "operation": "enumerate",
                        "path": str(sibling_b),
                        "expected": "denied",
                    },
                ],
            )
            debug_captures = {
                str(path.relative_to(broker_root)): path.read_bytes()
                for path in broker_root.rglob("*.bin")
            }
            self.assertEqual(
                executed["terminal_receipt"]["status"], "passed", debug_captures
            )
            actual = json.loads(output.read_text(encoding="utf-8"))
            self.assertEqual(actual["declared-content"]["result"], "allowed")
            for operation in ("sibling-content", "sibling-stat", "sibling-enumerate"):
                self.assertEqual(actual[operation]["result"], "denied")

            receipt = broker.verify_for_e3(
                package, executed["isolation_receipt_ref"]
            )
            self.assertTrue(receipt["os_isolation_enforced"])
            self.assertFalse(
                receipt["caller_observations_accepted"]
            )
            self.assertTrue(receipt["limitations_acknowledged"])
            self.assertEqual(
                receipt["sandbox_identity"]["profile_digest"],
                package["task_process_release"]["profile_digest"],
            )
            self.assertEqual(
                set(receipt["process_identity"]),
                {"pid", "process_group_id", "birth_token", "boot_identity"},
            )
            self.assertEqual(
                {item["result"] for item in receipt["operations"]},
                {"allowed", "denied"},
            )
            tampered_ref = copy.deepcopy(executed["isolation_receipt_ref"])
            tampered_ref["threat_profile_digest"] = "sha256:" + "0" * 64
            with self.assertRaisesRegex(
                MacOSTaskProcessError, "parent-broker-owned"
            ):
                broker.verify_for_e3(package, tampered_ref)


if __name__ == "__main__":
    unittest.main()
