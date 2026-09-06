{
  description = "File-backed AI agent workflow configuration";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    {
      self,
      nixpkgs,
      ...
    }:
    let
      supportedSystems = [
        "aarch64-darwin"
        "x86_64-linux"
      ];
      forAllSystems =
        function:
        builtins.listToAttrs (
          map (system: {
            name = system;
            value = function system;
          }) supportedSystems
        );
      skillDirectory = builtins.readDir ./agent-workflows/skills;
      workflowSkillNames = builtins.attrNames (
        nixpkgs.lib.filterAttrs (_: type: type == "directory") skillDirectory
      );
      workflowSkills = builtins.listToAttrs (
        map (name: {
          inherit name;
          value = "${self}/agent-workflows/skills/${name}";
        }) workflowSkillNames
      );
      ownerManifest = builtins.fromJSON (
        builtins.readFile ./agent-workflows/manifests/owner-manifest.json
      );
      distributionManifest = builtins.fromJSON (
        builtins.readFile ./agent-workflows/manifests/distribution.json
      );
      implementationStatus = builtins.fromJSON (
        builtins.readFile ./agent-workflows/manifests/implementation-status.json
      );
      sourceRelease = builtins.fromJSON (
        builtins.readFile ./agent-workflows/manifests/source-release.json
      );
      portableManifestFiles = distributionManifest.managed_child_sets.portable_manifest_files;
      implementationPlanDigest = "sha256:${builtins.hashFile "sha256" ./docs/plans/ai-agent-workflow-full-implementation-plan.md}";
      catalogDigest = "sha256:${builtins.hashFile "sha256" ./docs/plans/ai-agent-workflow-step-catalog.md}";
      portableManifestEntries = builtins.listToAttrs (
        map (name: {
          name = ".local/share/agent-workflows/${name}";
          value.source = "${self}/agent-workflows/manifests/${name}";
        }) portableManifestFiles
      );
      mkHomeSkillEntries =
        root:
        builtins.listToAttrs (
          map (name: {
            name = "${root}/${name}";
            value = {
              source = workflowSkills.${name};
              recursive = true;
            };
          }) workflowSkillNames
        );
      mkWorkflowCli =
        pkgs:
        let
          python = pkgs.python3;
          workflowCli = pkgs.writeShellApplication {
            name = "agent-workflow";
            runtimeInputs = [ python ];
            text = ''
              export PYTHONPATH="${self}/agent-workflows/src''${PYTHONPATH:+:$PYTHONPATH}"
              export AGENT_WORKFLOW_IMPLEMENTATION_MANIFEST="${self}/agent-workflows/manifests/implementation-status.json"
              export AGENT_WORKFLOW_SOURCE_ROOT="${self}"
              exec python -m ai_agent_workflow "$@"
            '';
          };
          configCli = pkgs.writeShellApplication {
            name = "agent-workflow-config";
            runtimeInputs = [ python ];
            text = ''
              export PYTHONPATH="${self}/agent-workflows/src''${PYTHONPATH:+:$PYTHONPATH}"
              exec python -m ai_agent_workflow.config_cli \
                --policy ${self}/agent-workflows/manifests/model-policy.json "$@"
            '';
          };
          implementationStatusCli = pkgs.writeShellApplication {
            name = "agent-workflow-implementation-status";
            runtimeInputs = [ python ];
            text = ''
              export PYTHONPATH="${self}/agent-workflows/src''${PYTHONPATH:+:$PYTHONPATH}"
              exec python -m ai_agent_workflow.implementation_status \
                --manifest ${self}/agent-workflows/manifests/implementation-status.json \
                --source-root ${self} \
                --check-html ${self}/docs/ai-agent-workflow-usage.html "$@"
            '';
          };
        in
        pkgs.symlinkJoin {
          name = "agent-workflow-cli";
          paths = [ workflowCli configCli implementationStatusCli ];
        };
    in
    {
      inherit ownerManifest workflowSkillNames;

      packages = forAllSystems (system: {
        agent-workflow = mkWorkflowCli nixpkgs.legacyPackages.${system};
        default = self.packages.${system}.agent-workflow;
      });

      checks = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          module = self.homeManagerModules.default { inherit pkgs; };
          homeFiles = module.home.file;
          codexSkills = module.programs.codex.skills;
          expectedTargets = map (entry: entry.root) ownerManifest.managed_targets;
          installedPortableManifestFiles = builtins.filter (
            name: builtins.match "[.]local/share/agent-workflows/.*[.]json" name != null
          ) (builtins.attrNames homeFiles);
          modelPolicy = builtins.fromJSON (
            builtins.readFile ./agent-workflows/manifests/model-policy.json
          );
        in
        {
          workflow-contract =
            assert implementationStatus.source_authority.plan_ref.digest == implementationPlanDigest;
            assert implementationStatus.source_authority.catalog_ref.digest == catalogDigest;
            assert implementationStatus.completion_claim.source_wide_integration_complete;
            assert !implementationStatus.completion_claim.full_workflow_ready;
            assert sourceRelease.coverage.named_contracts.complete;
            assert sourceRelease.coverage.profile_steps.complete;
            assert sourceRelease.coverage.additional_required_surfaces.complete;
            assert workflowSkillNames == distributionManifest.managed_child_sets.workflow_skill_directories;
            assert builtins.length workflowSkillNames == builtins.length (builtins.attrNames workflowSkills);
            assert builtins.attrNames codexSkills == workflowSkillNames;
            assert builtins.length installedPortableManifestFiles == builtins.length portableManifestFiles;
            assert builtins.all (
              name: builtins.elem ".local/share/agent-workflows/${name}" installedPortableManifestFiles
            ) portableManifestFiles;
            assert !(module.programs.codex ? settings);
            assert !(homeFiles ? ".codex");
            assert !(homeFiles ? ".codex/config.toml");
            assert !(module ? xdg) || !(module.xdg ? configFile);
            assert modelPolicy.default.model == "gpt-5.6-luna";
            assert modelPolicy.default.reasoning_effort == "max";
            assert modelPolicy.automatic_fallback == false;
            assert modelPolicy.config_apply == "explicit-user-command-only";
            assert builtins.elem ".codex/skills" expectedTargets;
            assert builtins.elem ".claude/skills" expectedTargets;
            assert builtins.elem ".agents/skills" expectedTargets;
            pkgs.runCommand "agent-workflow-contract"
              {
                nativeBuildInputs = [ pkgs.python3 ];
              }
              ''
                python -m json.tool ${self}/agent-workflows/manifests/owner-manifest.json >/dev/null
                python -m json.tool ${self}/agent-workflows/manifests/model-policy.json >/dev/null
                python -m json.tool ${self}/agent-workflows/manifests/implementation-status.json >/dev/null
                PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=${self}/agent-workflows/src \
                  python -B -m ai_agent_workflow.implementation_status \
                    --manifest ${self}/agent-workflows/manifests/implementation-status.json \
                    --source-root ${self} \
                    --check-html ${self}/docs/ai-agent-workflow-usage.html >/dev/null
                PYTHONDONTWRITEBYTECODE=1 PYTHONPATH=${self}/agent-workflows/src \
                  python -B -m unittest discover -s ${self}/agent-workflows/tests -v
                ${mkWorkflowCli pkgs}/bin/agent-workflow --help >/dev/null
                ${mkWorkflowCli pkgs}/bin/agent-workflow-config --help >/dev/null
                ${mkWorkflowCli pkgs}/bin/agent-workflow-implementation-status \
                  > implementation-evaluation.json
                python -m json.tool implementation-evaluation.json >/dev/null
                touch "$out"
              '';
        }
      );

      homeManagerModules.default =
        { pkgs, ... }:
        {
          programs.claude-code.enable = true;
          programs.codex = {
            enable = true;
            package = pkgs.codex;
            skills = workflowSkills;
          };

          home.packages = [ (mkWorkflowCli pkgs) ];

          home.file =
            portableManifestEntries
            // mkHomeSkillEntries ".claude/skills"
            // mkHomeSkillEntries ".agents/skills";
        };
    };
}
