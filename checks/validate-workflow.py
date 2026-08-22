#!/usr/bin/env python3
"""Static cross-file validation for the declarative agent workflow."""

from __future__ import annotations

import ast
import json
from pathlib import Path
import re
import sys


ROOT = Path(__file__).resolve().parents[1]


def require(condition: bool, message: str) -> None:
    if not condition:
        raise AssertionError(message)


def json_file(relative: str):
    return json.loads((ROOT / relative).read_text(encoding="utf-8"))


def frontmatter(relative: str):
    text = (ROOT / relative).read_text(encoding="utf-8")
    require(text.startswith("---\n"), f"missing frontmatter: {relative}")
    end = text.find("\n---\n", 4)
    require(end >= 0, f"unclosed frontmatter: {relative}")
    metadata = {}
    for line in text[4:end].splitlines():
        if not line.strip() or line.lstrip().startswith("#"):
            continue
        require(":" in line, f"invalid frontmatter line in {relative}: {line}")
        key, value = line.split(":", 1)
        metadata[key.strip()] = value.strip().strip('"').strip("'")
    return metadata


def route_catalog_skills():
    text = (ROOT / "skills/route-work/references/workflows.yaml").read_text(encoding="utf-8")
    skills = []
    for block in text.split('\n  - name: "')[1:]:
        name, remainder = block.split('"', 1)
        route = re.search(r'(?m)^    route_mode: "([^"]+)"$', remainder)
        invocation = re.search(r'(?m)^    upstream_invocation: "([^"]+)"$', remainder)
        require(route is not None and invocation is not None, f"incomplete route catalog entry: {name}")
        skills.append(
            {
                "name": name,
                "route_mode": route.group(1),
                "upstream_invocation": invocation.group(1),
            }
        )
    return skills


def simple_toml(relative: str):
    values = {}
    for line in (ROOT / relative).read_text(encoding="utf-8").splitlines():
        match = re.fullmatch(r'([A-Za-z0-9_]+)\s*=\s*"([^"]*)"', line.strip())
        if match:
            values[match.group(1)] = match.group(2)
    return values


def all_hook_handlers(config):
    for event, groups in config["hooks"].items():
        for group in groups:
            for handler in group["hooks"]:
                yield event, handler


def validate_aihero():
    manifest = json_file("manifests/aihero-skills.json")
    skills = manifest["skills"]
    routed = route_catalog_skills()
    names = [skill["name"] for skill in skills]
    routed_names = [skill["name"] for skill in routed]
    boundary = manifest["upstream"]["release_boundary"]
    require(len(names) == boundary["expected_skill_count"] == 25, "AI Hero count mismatch")
    require(len(names) == len(set(names)), "duplicate AI Hero Skill name")
    require(set(names) == set(routed_names), "route catalog and AI Hero manifest names differ")
    require(sum(len(skill["files"]) for skill in skills) == boundary["expected_file_count"] == 74, "AI Hero file count mismatch")
    require(all("SKILL.md" in skill["files"] for skill in skills), "AI Hero Skill without SKILL.md")
    manual_manifest = {
        skill["name"] for skill in skills if skill["invocation"]["claude_manual"]
    }
    manual_catalog = {
        skill["name"] for skill in routed if skill["upstream_invocation"] == "manual-only"
    }
    require(manual_manifest == manual_catalog and len(manual_catalog) == 14, "manual-only set mismatch")
    manifest_modes = {skill["name"]: skill["invocation"]["route_mode"] for skill in skills}
    catalog_modes = {skill["name"]: skill["route_mode"] for skill in routed}
    require(manifest_modes == catalog_modes, "route_mode mismatch")
    return manifest


def validate_cutover():
    cutover = json_file("manifests/workflow-cutover.json")
    entries = cutover["entries"]
    require(len(entries) == 46, "cutover inventory count changed without re-audit")
    require(len({entry["path"] for entry in entries}) == len(entries), "duplicate cutover path")
    require(
        all(entry["disposition"] in {"keep", "rewrite", "replace-with", "remove"} for entry in entries),
        "invalid cutover disposition",
    )
    require(len(cutover["cutover_conflicts"]) == 3, "unmanaged conflict list must be reviewed")
    cleanup = cutover["post_activation_cleanup"]
    require(cleanup["remove_only_if_empty_and_separately_authorized"] is True, "unsafe cleanup policy")
    for source in cutover["repository_sources"]["remove"]:
        path = ROOT / source.rstrip("/")
        if source.endswith("/"):
            require(not path.exists() or not any(item.is_file() for item in path.rglob("*")), f"obsolete source files remain: {source}")
        else:
            require(not path.exists(), f"obsolete source remains: {source}")
    return cutover


def validate_skills(manifest):
    local_names = [
        "route-work",
        "work-ledger",
        "record-decision",
        "self-verification",
        "use-repo-local-workspace",
    ]
    for name in local_names:
        metadata = frontmatter(f"skills/{name}/SKILL.md")
        require(metadata["name"] == name, f"local Skill name mismatch: {name}")
    require(len(local_names) + len(manifest["skills"]) == 30, "shared Skill total must be 30")


def validate_hooks_and_models():
    settings = json_file("settings.json")
    codex_hooks = json_file("codex/hooks.json")
    require(settings["model"] == "sonnet", "Claude root must use normal Sonnet")
    require(settings["effortLevel"] == "medium", "Claude root effort must be medium")
    require("CLAUDE_CODE_SUBAGENT_MODEL" not in settings.get("env", {}), "subagent model override is forbidden")
    require("CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS" not in settings.get("env", {}), "legacy agent-team flag remains")
    require(set(settings["hooks"]) == {"SessionStart", "SubagentStart", "SubagentStop", "TaskCompleted", "PreCompact", "PostCompact", "Stop", "SessionEnd"}, "Claude hook event set mismatch")
    require(set(codex_hooks["hooks"]) >= {"SessionStart", "SubagentStart", "SubagentStop", "PreCompact", "PostCompact", "Stop", "SessionEnd"}, "Codex hook event set incomplete")
    for config in (settings["hooks"], codex_hooks["hooks"]):
        for event, groups in config.items():
            for group in groups:
                for handler in group["hooks"]:
                    require(handler["type"] == "command", f"non-command hook: {event}")

    claude_expected = {
        "workflow-orchestrator-opus.md": ("opus", "high"),
        "workflow-architect-opus.md": ("opus", "high"),
        "workflow-explorer.md": ("sonnet", "medium"),
        "workflow-worker.md": ("sonnet", "medium"),
        "workflow-reviewer.md": ("sonnet", "high"),
        "workflow-verifier.md": ("sonnet", "medium"),
    }
    for filename, expected in claude_expected.items():
        metadata = frontmatter(f"agents/{filename}")
        require((metadata["model"], metadata["effort"]) == expected, f"Claude agent model mismatch: {filename}")

    codex_expected = {
        "workflow-explorer.toml": ("medium", "workspace-write"),
        "workflow-worker.toml": ("medium", "workspace-write"),
        "workflow-reviewer.toml": ("high", "workspace-write"),
        "workflow-verifier.toml": ("medium", "workspace-write"),
    }
    for filename, expected in codex_expected.items():
        metadata = simple_toml(f"codex/agents/{filename}")
        require(metadata["model"] == "gpt-5.6-terra", f"Codex worker is not Terra: {filename}")
        require((metadata["model_reasoning_effort"], metadata["sandbox_mode"]) == expected, f"Codex agent mismatch: {filename}")


def validate_helper_is_mechanical():
    helper = ROOT / "hooks/work-ledger-hook.py"
    tree = ast.parse(helper.read_text(encoding="utf-8"))
    forbidden_imports = {"requests", "urllib", "httpx", "socket", "anthropic", "openai"}
    imported = set()
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            imported.update(alias.name.split(".")[0] for alias in node.names)
        elif isinstance(node, ast.ImportFrom) and node.module:
            imported.add(node.module.split(".")[0])
    require(not (imported & forbidden_imports), "hook helper imports a network/model client")
    calls = []
    for node in ast.walk(tree):
        if not isinstance(node, ast.Call):
            continue
        if isinstance(node.func, ast.Attribute) and node.func.attr == "run" and node.args:
            calls.append(node.args[0])
    require(len(calls) == 1, "unexpected subprocess call count in hook helper")
    first = calls[0]
    require(isinstance(first, ast.List) and isinstance(first.elts[0], ast.Constant) and first.elts[0].value == "git", "hook helper may execute only git")


def validate_project_workflow_initializer():
    manifest = json_file("manifests/project-workflows.json")
    require(manifest.get("schema") == "project-workflows/v1", "initializer manifest schema mismatch")
    require(
        manifest.get("selection_state") == ".local/agent/workflow-selection.json",
        "initializer selection state path mismatch",
    )
    workflows = manifest.get("workflows")
    require(isinstance(workflows, dict), "initializer workflows must be an object")
    require(set(workflows) == {"aidlc", "superpowers"}, "initializer workflow set mismatch")

    script_relative = "scripts/agent-workflow-init.py"
    script = ROOT / script_relative
    require(script.is_file(), f"initializer script missing: {script_relative}")
    script_text = script.read_text(encoding="utf-8")
    require(
        'SCRIPT_ROOT = Path(__file__).resolve().parents[1]' in script_text,
        "initializer must derive its source root from its installed script path",
    )
    require(
        'DEFAULT_MANIFEST = SCRIPT_ROOT / "manifests" / "project-workflows.json"' in script_text,
        "initializer default manifest must remain store-source relative",
    )
    require("def _check_prerequisites(" in script_text, "initializer prerequisite checks are missing")

    expected_status_targets = {
        "codex": ".agents/skills/workflow-status",
        "claude": ".claude/skills/workflow-status",
    }
    expected_templates = {
        "codex": "templates/project-workflow/codex/workflow-status",
        "claude": "templates/project-workflow/claude/workflow-status",
    }
    shared_phase_map = "templates/project-workflow/shared/workflow-status/phases.json"
    require((ROOT / shared_phase_map).is_file(), "shared workflow-status phase map missing")

    for workflow_name, workflow in workflows.items():
        require(isinstance(workflow, dict), f"invalid initializer workflow: {workflow_name}")
        upstream = workflow.get("upstream")
        require(
            isinstance(upstream, dict)
            and isinstance(upstream.get("repository"), str)
            and isinstance(upstream.get("ref"), str)
            and upstream["ref"].startswith("refs/heads/")
            and upstream["ref"] != "refs/heads/",
            f"initializer upstream is incomplete: {workflow_name}",
        )
        selections = workflow.get("selections")
        require(isinstance(selections, dict), f"initializer selections missing: {workflow_name}")
        require(set(selections) == set(expected_status_targets), f"initializer selection matrix mismatch: {workflow_name}")
        for agent, config in selections.items():
            require(isinstance(config, dict), f"invalid initializer selection: {agent}/{workflow_name}")
            template = config.get("workflow_status_template")
            target = config.get("workflow_status_target")
            shared = config.get("workflow_status_shared")
            require(template == expected_templates[agent], f"workflow-status template mismatch: {agent}/{workflow_name}")
            require(target == expected_status_targets[agent], f"workflow-status target mismatch: {agent}/{workflow_name}")
            require(shared == shared_phase_map, f"workflow-status phase map mismatch: {agent}/{workflow_name}")
            require((ROOT / str(template) / "SKILL.md").is_file(), f"workflow-status Skill missing: {agent}")
            requirements = config.get("requirements")
            require(isinstance(requirements, list), f"initializer requirements missing: {agent}/{workflow_name}")
            require(
                all(isinstance(item, dict) and isinstance(item.get("command"), str) for item in requirements),
                f"invalid initializer requirement: {agent}/{workflow_name}",
            )

    for agent in expected_status_targets:
        aidlc = workflows["aidlc"]["selections"][agent]
        commands = {item["command"] for item in aidlc["requirements"]}
        require("bun" in commands, f"AI-DLC must retain bun prerequisite: {agent}")

    codex_aidlc = workflows["aidlc"]["selections"]["codex"]
    codex_superpowers = workflows["superpowers"]["selections"]["codex"]
    require(codex_aidlc.get("completion_gates"), "Codex AI-DLC trust gate must block completion")
    require(
        codex_superpowers.get("completion_gates"),
        "Codex Superpowers fresh-session discovery must block completion",
    )
    claude_superpowers = workflows["superpowers"]["selections"]["claude"]
    require(
        claude_superpowers.get("plugin_inventory") == ["claude", "plugin", "list", "--json"],
        "Claude Superpowers inventory must use structured JSON output",
    )
    require("dst_dir_fd=parent_fd" in script_text, "initializer placement is not parent-anchored")
    require("os.link(" in script_text, "initializer placement must be atomic and no-replace")
    require("_same_anchored_entry" in script_text, "initializer rollback is not inode-aware")


def validate_nix_surface():
    flake = (ROOT / "flake.nix").read_text(encoding="utf-8")
    require("force = true" not in flake, "Home Manager conflict is hidden with force")
    require("builtins.path" in flake and "sha256 = skill.nar_hash" in flake, "AI Hero NAR hashes are not enforced by Nix")
    require('mkSkillEntries ".claude/skills"' in flake, "Claude shared Skill mapping missing")
    require('mkSkillEntries ".agents/skills"' in flake, "Codex shared Skill mapping missing")
    local_skills = re.search(r"localSkillNames\s*=\s*\[(.*?)\];", flake, re.DOTALL)
    require(local_skills is not None, "local Skill list missing from Nix")
    require(
        '"workflow-status"' not in local_skills.group(1),
        "project-local workflow-status must not be installed as a global Skill",
    )
    require("mkAgentWorkflowInit" in flake, "initializer package helper missing")
    require("pkgs.writeShellApplication" in flake, "initializer must be a Nix executable package")
    require("pkgs.git" in flake and "pkgs.python3" in flake, "initializer runtime tools are not explicit")
    require(
        "${self}/scripts/agent-workflow-init.py" in flake,
        "initializer package must execute the repository-store source tree",
    )
    require('"agent-workflow-init" = mkAgentWorkflowInit pkgs;' in flake, "initializer package output missing")
    require('"agent-workflow-init" = {' in flake and 'type = "app";' in flake, "initializer app output missing")
    require(
        'program = "${self.packages.${system}.agent-workflow-init}/bin/agent-workflow-init";' in flake,
        "initializer app must point at the package executable",
    )
    home_packages = re.search(r"home\.packages\s*=\s*\[(.*?)\];", flake, re.DOTALL)
    require(home_packages is not None, "Home Manager package surface missing")
    require("agentWorkflowInit" in home_packages.group(1), "initializer is not exposed through Home Manager")
    require("pkgs.bun" in home_packages.group(1), "bun is not exposed through Home Manager")


def main():
    manifest = validate_aihero()
    cutover = validate_cutover()
    validate_skills(manifest)
    validate_hooks_and_models()
    validate_helper_is_mechanical()
    validate_project_workflow_initializer()
    validate_nix_surface()
    print(
        json.dumps(
            {
                "valid": True,
                "aihero_skills": len(manifest["skills"]),
                "shared_skills_per_harness": 30,
                "cutover_entries": len(cutover["entries"]),
                "unmanaged_conflicts": len(cutover["cutover_conflicts"]),
            },
            sort_keys=True,
        )
    )


if __name__ == "__main__":
    try:
        main()
    except Exception as exc:
        print(f"workflow validation failed: {exc}", file=sys.stderr)
        raise SystemExit(1)
