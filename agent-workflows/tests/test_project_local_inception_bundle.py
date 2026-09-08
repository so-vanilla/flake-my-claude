"""Focused tests for the reviewed create-only project-local snapshot."""
import importlib.util
import json
import os
from pathlib import Path
import re
import shutil
import stat
import subprocess
import sys
import tempfile
import unittest
from datetime import datetime, timezone


ROOT = Path(__file__).resolve().parents[2]
WORKFLOW_ROOT = ROOT / "agent-workflows"
EXAMPLE = WORKFLOW_ROOT / "examples/support-report"
sys.path[:0] = [str(WORKFLOW_ROOT / "src"), str(WORKFLOW_ROOT / "tests"), str(EXAMPLE)]
from ai_agent_workflow.runtime_approval import approval_context  # noqa: E402
from runtime_plan import prepare as prepare_plan  # noqa: E402
from test_runtime_execution import RuntimeExecutionIntegrationTests  # noqa: E402
INSTALLER_PATH = ROOT / "agent-workflows/scripts/install-project-local-inception.py"
GENERATOR_PATH = ROOT / "agent-workflows/scripts/generate-project-local-release-manifest.py"
RELEASE_PATH = ROOT / "agent-workflows/manifests/project-local-inception-release.json"
SPEC = importlib.util.spec_from_file_location("project_local_installer", INSTALLER_PATH)
installer = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(installer)


class ProjectLocalInceptionBundleTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="project-inception-")
        self.addCleanup(self.temporary.cleanup)
        self.base = Path(self.temporary.name).resolve()
        self.project = self.base / "project"
        self.project.mkdir()
        subprocess.run(["git", "init", "-q", str(self.project)], check=True)
        (self.project / ".gitignore").write_text(".local/\n", encoding="utf-8")

    def install(self, source=ROOT, **kwargs):
        return installer.install(self.project, source, **kwargs)

    def run_wrapper(self, *arguments, env=None):
        wrapper = self.project / ".agent-workflow/bin/agent-workflow-inception"
        return subprocess.run([str(wrapper), *map(str, arguments)], cwd=self.project,
                              capture_output=True, text=True, timeout=30, env=env)

    def run_wrapper_json(self, *arguments, env=None, timeout=30):
        wrapper = self.project / ".agent-workflow/bin/agent-workflow-inception"
        result = subprocess.run([str(wrapper), *map(str, arguments)], cwd=self.project,
                                capture_output=True, text=True, timeout=timeout, env=env)
        self.assertEqual(result.returncode, 0, result.stderr or result.stdout)
        return json.loads(result.stdout)

    def put_json(self, name, value, version="v1"):
        path = self.project / ".local/agent/product-path-inputs" / name
        path.parent.mkdir(parents=True, exist_ok=True)
        raw = (json.dumps(value, ensure_ascii=False, sort_keys=True, indent=2) + "\n").encode()
        path.write_bytes(raw)
        import hashlib
        return {"path": str(path), "version": version,
                "digest": "sha256:" + hashlib.sha256(raw).hexdigest()}

    def copy_release_source(self):
        target = self.base / ("source-%d" % len(list(self.base.glob("source-*"))))
        release = json.loads(RELEASE_PATH.read_text(encoding="utf-8"))
        for item in release["files"]:
            source = ROOT / item["source"]
            destination = target / item["source"]
            destination.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(source, destination)
        manifest = target / installer.RELEASE_MANIFEST
        manifest.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(RELEASE_PATH, manifest)
        return target, release

    def assert_refused_without_frontier(self, operation, outside=None):
        with self.assertRaises((installer.InstallError, OSError, ValueError)):
            operation()
        self.assertFalse((self.project / ".agent-workflow/SNAPSHOT-MANIFEST.json").exists())
        if outside is not None:
            self.assertEqual(list(outside.rglob("*")), [])

    def test_checked_in_release_manifest_is_current(self):
        result = subprocess.run([sys.executable, str(GENERATOR_PATH), "--check"], cwd=ROOT,
                                capture_output=True, text=True, timeout=30)
        self.assertEqual(result.returncode, 0, result.stderr)

    def test_installs_complete_release_projection_and_is_idempotent(self):
        first = self.install()
        second = self.install()
        self.assertEqual(first, second)
        self.assertEqual(first["schema"], "agent-workflow-project-snapshot/v2")
        self.assertEqual(first["release_manifest"], installer.RELEASE_MANIFEST.as_posix())
        self.assertTrue((self.project / ".agents/skills/entry/SKILL.md").is_file())
        self.assertTrue((self.project / ".agent-workflow/runtime/agent-workflows/src/ai_agent_workflow/inception_cli.py").is_file())
        self.assertTrue((self.project / ".agent-workflow/runtime/agent-workflows/schemas/objective-system-v1.schema.json").is_file())
        self.assertTrue((self.project / ".agent-workflow/runtime/agent-workflows/workflows/bug-fix-standard.json").is_file())
        self.assertTrue((self.project / ".agent-workflow/runtime/agent-workflows/groups/execution.json").is_file())
        self.assertTrue((self.project / ".agent-workflow/runtime/agent-workflows/catalog.yaml").is_file())
        wrapper = self.project / first["wrapper"]
        self.assertEqual(stat.S_IMODE(os.lstat(wrapper).st_mode), 0o755)
        self.assertEqual(self.run_wrapper("--help").returncode, 0)
        runtime_help = self.run_wrapper("runtime", "--help")
        self.assertEqual(runtime_help.returncode, 0, runtime_help.stderr)

    def test_wrapper_refuses_byte_mode_and_unlisted_destination_drift(self):
        self.install()
        runtime = self.project / ".agent-workflow/runtime/agent-workflows/src/ai_agent_workflow/inception_cli.py"
        original = runtime.read_bytes()
        runtime.write_bytes(original + b"\n# drift\n")
        self.assertIn("stale snapshot file", self.run_wrapper("--help").stderr)
        runtime.write_bytes(original)
        runtime.chmod(0o600)
        self.assertIn("stale snapshot mode", self.run_wrapper("--help").stderr)
        runtime.chmod(0o644)
        extra = runtime.parent / "unlisted.py"
        extra.write_text("pass\n", encoding="utf-8")
        self.assertIn("inventory mismatch", self.run_wrapper("--help").stderr)

    def test_changed_removed_and_added_inputs_are_refused_for_every_managed_root(self):
        release = json.loads(RELEASE_PATH.read_text(encoding="utf-8"))
        by_root = {}
        for root in release["managed_roots"]:
            matches = [item for item in release["files"] if
                       (root["kind"] == "file" and item["source"] == root["source"])
                       or (root["kind"] == "tree" and item["source"].startswith(root["source"] + "/"))]
            self.assertTrue(matches, root)
            by_root[root["source"]] = (root, matches[0])
        for root_name, (_, selected) in by_root.items():
            with self.subTest(root=root_name, mutation="changed"):
                source, _ = self.copy_release_source()
                (source / selected["source"]).write_bytes((source / selected["source"]).read_bytes() + b"\ndrift\n")
                self.assert_refused_without_frontier(lambda: self.install(source))
            with self.subTest(root=root_name, mutation="removed"):
                source, _ = self.copy_release_source()
                (source / selected["source"]).unlink()
                self.assert_refused_without_frontier(lambda: self.install(source))
        for root in release["managed_roots"]:
            if root["kind"] != "tree":
                continue
            with self.subTest(root=root["source"], mutation="added"):
                source, _ = self.copy_release_source()
                (source / root["source"] / "UNLISTED.release-test").write_text("extra\n", encoding="utf-8")
                self.assert_refused_without_frontier(lambda: self.install(source))

    def test_duplicate_absolute_escaping_and_type_changed_manifest_entries_are_refused(self):
        mutations = {}
        source, release = self.copy_release_source()
        mutations["duplicate"] = lambda value: value["files"].append(dict(value["files"][0]))
        mutations["absolute"] = lambda value: value["files"][0].update(source="/tmp/escape")
        mutations["escaping"] = lambda value: value["files"][0].update(source="agent-workflows/skills/../escape")
        mutations["type"] = lambda value: value["managed_roots"][0].update(kind="device")
        for label, mutate in mutations.items():
            with self.subTest(label=label):
                isolated, value = self.copy_release_source()
                mutate(value)
                (isolated / installer.RELEASE_MANIFEST).write_text(
                    json.dumps(value, ensure_ascii=False, sort_keys=True, indent=2) + "\n", encoding="utf-8")
                self.assert_refused_without_frontier(lambda: self.install(isolated))

    def test_existing_conflict_is_refused_before_managed_files_are_written(self):
        conflict = self.project / ".agents/skills/entry/SKILL.md"
        conflict.parent.mkdir(parents=True)
        conflict.write_text("user-owned\n", encoding="utf-8")
        self.assert_refused_without_frontier(self.install)
        self.assertEqual(conflict.read_text(encoding="utf-8"), "user-owned\n")

    def test_destination_root_nested_and_broken_symlinks_are_refused(self):
        cases = (
            Path(".agents"), Path(".agent-workflow"), Path(".agents/skills"),
            Path(".agents/skills/entry"), Path(".agent-workflow/runtime/agent-workflows"),
        )
        for index, relative in enumerate(cases):
            with self.subTest(relative=relative):
                project = self.base / ("symlink-project-%d" % index)
                project.mkdir()
                outside = self.base / ("outside-%d" % index)
                outside.mkdir()
                target = project / relative
                target.parent.mkdir(parents=True, exist_ok=True)
                target.symlink_to(outside, target_is_directory=True)
                self.project = project
                self.assert_refused_without_frontier(self.install, outside)
        project = self.base / "broken-project"
        project.mkdir()
        broken = project / ".agent-workflow"
        broken.symlink_to(self.base / "does-not-exist", target_is_directory=True)
        self.project = project
        self.assert_refused_without_frontier(self.install)

    def test_controlled_ancestor_replacement_race_is_refused_without_escape(self):
        outside = self.base / "race-outside"
        outside.mkdir()
        (self.project / ".agent-workflow").mkdir()

        def replace(project):
            original = project / ".agent-workflow"
            original.rename(project / ".agent-workflow-before-race")
            original.symlink_to(outside, target_is_directory=True)

        self.assert_refused_without_frontier(lambda: self.install(before_publish=replace), outside)

    def test_absolute_wrapper_beats_stale_path_and_source_can_disappear(self):
        source, _ = self.copy_release_source()
        manifest = self.install(source)
        source.rename(self.base / "source-offline")
        stale_bin = self.base / "stale-bin"
        stale_bin.mkdir()
        stale = stale_bin / "agent-workflow-inception"
        stale.write_text("#!/bin/sh\nexit 99\n", encoding="utf-8")
        stale.chmod(0o755)
        env = {**os.environ, "PATH": str(stale_bin) + os.pathsep + os.environ.get("PATH", "")}
        result = self.run_wrapper("runtime", "--help", env=env)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(manifest["release_manifest_digest"], json.loads(
            (self.project / ".agent-workflow/SNAPSHOT-MANIFEST.json").read_text())["release_manifest_digest"])

    @unittest.skipUnless(sys.platform == "darwin" and Path("/usr/bin/sandbox-exec").is_file(),
                         "installed Group E product path requires macOS sandbox-exec")
    def test_absolute_wrapper_drives_adopt_b_c_d_integrated_e_and_cold_resume(self):
        """No source import or bare PATH entry participates after installation."""
        import copy
        import hashlib

        run_id = "installed-product-path"
        intake = self.put_json("intake.json", {"request": "Produce one bounded local report."}, "intake")
        candidate = self.put_json("candidate.json", {"objective": "Produce one observable bounded local report."}, "v001")
        proposal = self.put_json("proposal.json", {"choice": "bounded local report"})
        actor = self.put_json("actor.json", {"actor_id": "installed-owner", "source": "mock"})
        adoption = {"intake_ref": intake, "candidate_ref": candidate, "proposal_ref": proposal,
                    "actor_ref": actor, "mode": "rehearsal"}
        receipt = {**approval_context(self.project, run_id, **adoption),
                   "receipt_id": "installed-product-receipt", "decision": "approve",
                   "explicit": True, "issued_at": datetime.now(timezone.utc).isoformat()}
        adoption["receipt_ref"] = self.put_json("approval.json", receipt)
        relative_candidate = {"path": str(Path(candidate["path"]).relative_to(self.project)),
                              "digest": candidate["digest"], "selector": "candidate"}
        semantics = [
            {"raw_request": "Produce one bounded local report.", "interpretation": "bounded local report",
             "assumptions": ["local input"], "unknowns": []},
            {"facts": [{"claim": "No external mutation", "source_ref": relative_candidate}]},
            {"depth": "project", "operation": "implementation", "owner": "installed-owner",
             "reversibility": "isolated-source"},
            {"material_unknowns": [], "inquiry_complete": True,
             "resolution_reason": "The physical candidate fixes the bounded scope."},
            {"options": [{"purpose": "bounded local report", "tradeoff": "local only"},
                         {"purpose": "external service", "tradeoff": "requires external authority"}]},
            {"owner": "installed-owner", "constraints": ["local only", "finite execution"],
             "feasible": True},
        ]
        adoption["preapproval_steps"] = []
        for number, semantic in enumerate(semantics, 1):
            values = {**copy.deepcopy(semantic), "input_refs": [relative_candidate],
                      "candidate_ref": relative_candidate, "version": "v1"}
            if number == 4:
                values["resolution_refs"] = [relative_candidate]
            adoption["preapproval_steps"].append(["group.B.B%d" % number, values])
        adoption["budget_seconds"] = 1200
        adoption_ref = self.put_json("adoption.json", adoption)

        # This fixture creator supplies only mock/human content. Every runtime
        # compiler and Kernel mutation below is an absolute wrapper process.
        planned = prepare_plan(self.project, ROOT, objective_ref=candidate,
                               owner_ref=actor, receipt_ref=adoption["receipt_ref"])
        planned_by_id = {qualified_id: values for qualified_id, values in planned}

        scope = [".local/agent/support-report-plan/reports/parse-select.json",
                 "sla_report/__init__.py", "sla_report/core.py"]
        for relative in scope:
            path = self.project / relative
            path.parent.mkdir(parents=True, exist_ok=True)
            path.touch()
        sibling = self.base / "confidential-sibling"
        sibling.mkdir()
        secret = sibling / "secret.txt"
        secret.write_text("must-not-be-readable", encoding="utf-8")
        adapter = self.project / ".local/agent/product-path-inputs/runtime-e-task.py"
        output = self.project / "sla_report/core.py"
        adapter.write_text(
            "import json, sys\n"
            "allowed, denied, output = sys.argv[1:]\n"
            "with open(allowed, encoding='utf-8') as stream: objective = json.load(stream)\n"
            "try:\n    open(denied, encoding='utf-8').read()\nexcept OSError:\n    blocked = True\nelse:\n    blocked = False\n"
            "with open(output, 'w', encoding='utf-8') as stream: stream.write('# installed wrapper output\\n')\n"
            "if not objective.get('objective') or not blocked: raise SystemExit(23)\n",
            encoding="utf-8")
        python = str(Path(sys.executable).resolve(strict=True))
        digest_bytes = lambda path: "sha256:" + hashlib.sha256(Path(path).read_bytes()).hexdigest()
        package = {
            "schema": "execution-package-input/v2", "package_id": "installed-parse-select",
            "contract_version": "workflow-execution/v2", "workspace_identity": str(self.project),
            "candidate_ref": {"id": "approved-objective", "digest": candidate["digest"]},
            "test_refs": [{"id": "installed-adapter", "digest": digest_bytes(adapter)}],
            "fixture_refs": [], "schema_refs": [{"id": "installed-task", "digest": digest_bytes(adapter)}],
            "config_refs": [], "lock_refs": [{"id": "python-runtime", "digest": digest_bytes(python)}],
            "toolchain": {"executable_digest": digest_bytes(python), "identity": "python3"},
            "command": {"argv": [python, "-I", "-S", str(adapter), candidate["path"], str(secret), str(output)],
                        "cwd": str(self.project)}, "environment": {},
            "isolation": {"cwd": str(self.project), "temporary_namespace": "tmp", "output_namespace": "out"},
            "resource_claims": {"read_paths": [".local/agent/product-path-inputs"], "write_paths": scope,
                                "exclusive_resources": []},
            "supervision": {"timeout_seconds": 10, "grace_seconds": 1, "signals": ["TERM", "KILL"],
                            "heartbeat_seconds": 1, "terminal_publication_seconds": 1},
            "external_input_refs": [],
        }
        # Reuse the runtime-E fixture builder with this project's physical
        # context.  It is an instance helper because required-E7 fixtures may
        # create evidence beneath ``self.project``.
        stage_inputs = RuntimeExecutionIntegrationTests.stage_inputs(self, package)
        stage_inputs["E4"]["worker_actor_id"] = "installed-runtime-worker"
        stage_inputs["E5"]["worker_actor_id"] = "installed-runtime-worker"
        stage_inputs["E6"]["observed_budget"]["remaining_seconds"] = 600
        execute_inputs = {
            "task_id": "parse-select", "execution_package_input": package,
            "worker_assignment_id": "installed-runtime-worker",
            "probes": [{"operation": "content-read", "path": candidate["path"], "expected": "allowed"},
                       {"operation": "content-read", "path": str(secret), "expected": "denied"}],
            "system_read_roots": [path for path in ("/System", "/usr/lib", "/usr/share", "/usr/bin", "/bin", "/dev") if Path(path).is_dir()],
            "runtime_read_roots": [str(Path("/Library/Developer/CommandLineTools").resolve(strict=True))],
            "stage_inputs": stage_inputs, "changed_paths": ["sla_report/core.py"],
        }
        execute_ref = self.put_json("execute-e.json", execute_inputs)

        source, _ = self.copy_release_source()
        manifest = self.install(source)
        source.rename(self.base / "reviewed-source-unavailable")
        stale_bin = self.base / "stale-bin"
        stale_bin.mkdir()
        stale_marker = self.base / "stale-executable-ran"
        stale = stale_bin / "agent-workflow-inception"
        stale.write_text("#!/bin/sh\ntouch '%s'\nexit 99\n" % stale_marker, encoding="utf-8")
        stale.chmod(0o755)
        env = {key: value for key, value in os.environ.items() if key not in {"PYTHONPATH", "PYTHONHOME"}}
        env["PATH"] = str(stale_bin) + os.pathsep + env.get("PATH", "")
        wrapper = str(self.project / manifest["wrapper"])

        secret_input = copy.deepcopy(execute_inputs)
        secret_input["stage_inputs"]["broker_capability"] = "caller-controlled"
        secret_ref = self.put_json("execute-e-with-secret.json", secret_input)
        secret_attempt = subprocess.run(
            [wrapper, "runtime", "execute-e", "--project", str(self.project),
             "--run-id", run_id, "--inputs", secret_ref["path"]],
            cwd=self.project, capture_output=True, text=True, timeout=30, env=env)
        self.assertEqual(secret_attempt.returncode, 2)
        self.assertIn("may not supply broker secrets or identity", secret_attempt.stdout)

        self.run_wrapper_json("runtime", "adopt", "--project", self.project, "--run-id", run_id,
                              "--inputs", adoption_ref["path"], env=env)

        def status():
            return self.run_wrapper_json("runtime", "status", "--project", self.project,
                                         "--run-id", run_id, env=env)

        def close_group(group, rationale):
            current = status()
            route = [group + str(number) for number in range(1, {"B": 7, "C": 6, "D": 12, "E": 9}[group] + 1)]
            if group == "E":
                route.remove("E7")
            audit = {"group_id": group, "objective_digest": candidate["digest"], "alignment": "aligned",
                     "artifact_refs": [current["record_refs"][item] for item in route],
                     "reviewer": "installed-wrapper-reviewer", "rationale": rationale}
            ref = self.put_json("audit-%s.json" % group, audit)
            return self.run_wrapper_json("runtime", "close", "--project", self.project,
                                         "--run-id", run_id, "--inputs", ref["path"], env=env)

        close_group("B", "Actual B compiler records bind the physical rehearsal approval.")
        self.run_wrapper_json("runtime", "advance", "--project", self.project, "--run-id", run_id, env=env)
        for group, count in (("C", 6), ("D", 12)):
            for number in range(1, count + 1):
                qualified_id = "group.%s.%s%d" % (group, group, number)
                values = copy.deepcopy(planned_by_id[qualified_id])
                current = status()
                if group == "D" and number > 1:
                    values["input_refs"].append(current["record_file_refs"]["D%d" % (number - 1)])
                if qualified_id == "group.D.D6":
                    receipt_value = {"source": "mock", "decision": "approve", "explicit": True,
                                     "scope": "option-selection", "actor_id": "installed-owner",
                                     "objective_digest": candidate["digest"],
                                     "options_digest": current["record_refs"]["D5"]["digest"],
                                     "selected_option": "validated-records"}
                    option_ref = self.put_json("runtime-option-approval.json", receipt_value)
                    values["approved_option_receipt"]["receipt_ref"] = option_ref
                    values["design"]["selected_option"] = "validated-records"
                    values["design"]["security"] = {"source_ref": current["record_file_refs"]["D5"]}
                for task in values.get("tasks", []) + values.get("briefs", []):
                    task["stop"] = {"source_ref": candidate}
                if qualified_id == "group.D.D12":
                    values["evidence_refs"] = [current["record_file_refs"]["D%d" % item] for item in range(1, 12)]
                step_ref = self.put_json("%s%d-runtime-input.json" % (group, number), values)
                stepped = self.run_wrapper_json("runtime", "step", "--project", self.project,
                                                "--run-id", run_id, "--qualified-id", qualified_id,
                                                "--inputs", step_ref["path"], "--actor", actor["path"], env=env)
                self.assertEqual(stepped["record_refs"].keys() >= {group + str(number)}, True)
            close_group(group, "All %s compiler records bind the current installed-runtime lineage." % group)
            self.run_wrapper_json("runtime", "advance", "--project", self.project, "--run-id", run_id, env=env)

        executed = self.run_wrapper_json("runtime", "execute-e", "--project", self.project,
                                         "--run-id", run_id, "--inputs", execute_ref["path"],
                                         env=env, timeout=180)
        installed_root = self.project / ".agent-workflow/runtime/agent-workflows/src"
        snapshot = json.loads((self.project / ".agent-workflow/SNAPSHOT-MANIFEST.json").read_text())
        for module_path in executed["runtime_module_paths"].values():
            module_path = Path(module_path)
            self.assertTrue(module_path.is_relative_to(installed_root), module_path)
            relative = module_path.relative_to(self.project).as_posix()
            self.assertEqual(digest_bytes(module_path), snapshot["files"][relative]["digest"])
        installed_skill = self.project / ".agents/skills/implementation-readiness-review/SKILL.md"
        skill_relative = installed_skill.relative_to(self.project).as_posix()
        self.assertEqual(digest_bytes(installed_skill), snapshot["files"][skill_relative]["digest"])
        self.assertFalse((self.project / ".agent-workflow/runtime/agent-workflows/skills/implementation-readiness-review/SKILL.md").exists())
        self.assertTrue((self.base / "reviewed-source-unavailable").is_dir())
        self.assertEqual(set(executed["stages"]), {"E1", "E2", "E3", "E4", "E5", "E6", "E8", "E9"})
        close_group("E", "Integrated no-required E execution and verification are persisted.")
        cold = status()
        self.assertEqual({key: cold["group"][key] for key in ("id", "next_group", "status")},
                         {"id": "E", "next_group": "H", "status": "closed"})
        self.assertFalse(stale_marker.exists())
        self.assertIn(wrapper, str(self.project / manifest["wrapper"]))

    def test_bundle_wrapper_creates_and_freshly_resumes_verified_handoff(self):
        manifest = self.install()
        request = self.project / "REQUEST.md"
        request.write_text("期限超過を一覧化する。外部送信は禁止。\n", encoding="utf-8")
        started = self.run_wrapper("init", "--project", self.project, "--work-id", "case",
                                   "--request", request, "--mode", "rehearsal")
        self.assertEqual(started.returncode, 0, started.stderr)
        intake = json.loads(started.stdout)["intake"]["path"]
        intake_value = json.loads(Path(intake).read_text(encoding="utf-8"))
        self.assertEqual(intake_value["runtime_snapshot"]["snapshot_digest"], manifest["snapshot_digest"])
        wrapper = str(self.project / manifest["wrapper"])
        self.assertEqual(intake_value["runtime_snapshot"]["wrapper"], wrapper)
        output = self.project / "entry-output.json"
        output.write_text(json.dumps({"raw_request_ref": intake_value["request"],
                                      "interpretation": "期限超過一覧", "assumptions": [], "unknowns": []}),
                          encoding="utf-8")
        saved = self.run_wrapper("save", "--intake", intake, "--skill", "entry",
                                 "--output", output, "--status", "recorded",
                                 "--started-at", intake_value["started_at"])
        self.assertEqual(saved.returncode, 0, saved.stderr)
        saved_value = json.loads(saved.stdout)
        self.assertIn("%s resume --handoff" % wrapper, saved_value["invocation"])
        resumed = self.run_wrapper("resume", "--handoff", saved_value["handoff"]["path"])
        self.assertEqual(resumed.returncode, 0, resumed.stderr)
        self.assertEqual(json.loads(resumed.stdout)["frontier_status"], "helper-verified")

    def test_project_local_operational_examples_do_not_use_bare_path_lookup(self):
        paths = [ROOT / "agent-workflows/README.md",
                 ROOT / "agent-workflows/skills/entry/references/inception-single-skill.md",
                 ROOT / "docs/ai-agent-workflow-manual.html",
                 ROOT / "docs/plans/inception-single-skill-operation.md"]
        bare_entrypoint = re.compile(
            r"(?<![/A-Za-z0-9_.-])agent-workflow-inception"
            r"(?=\s+(?:init|save|resume|runtime)\b)"
        )
        bare_runtime = re.compile(
            r"\bruntime\s+(?:adopt|status|step|close|advance)\b"
        )
        bound_runtime_prefixes = (
            ".agent-workflow/bin/agent-workflow-inception",
            "$INCEPTION_WRAPPER",
            "absolute wrapper's",
        )
        for command in ("agent-workflow-inception init",
                        "$ agent-workflow-inception runtime status"):
            self.assertIsNotNone(bare_entrypoint.search(command), command)
        for command in ("`runtime adopt`", "`runtime status --project /tmp/p`"):
            match = bare_runtime.search(command)
            self.assertIsNotNone(match, command)
            self.assertFalse(any(marker in command[:match.start()]
                                 for marker in bound_runtime_prefixes), command)
        for path in paths:
            contents = path.read_text(encoding="utf-8")
            for line_number, line in enumerate(contents.splitlines(), 1):
                if bare_entrypoint.search(line):
                    self.fail("bare project-local command at %s:%d" % (path, line_number))
                for match in bare_runtime.finditer(line):
                    prefix = line[:match.start()]
                    if any(marker in prefix for marker in bound_runtime_prefixes):
                        continue
                    source_checkout = ("source checkout" in line.lower()
                                       and "ai_agent_workflow.inception_cli" in prefix)
                    if source_checkout:
                        continue
                    self.fail("bare project-local runtime command at %s:%d" %
                              (path, line_number))


if __name__ == "__main__":
    unittest.main()
