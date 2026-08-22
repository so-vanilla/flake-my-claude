{
  description = "Claude Code and Codex workflow configuration";

  inputs.aihero-skills = {
    url = "github:mattpocock/skills/8b78b531ab965735c5dc74f6f7a219e1e37326df";
    flake = false;
  };
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    {
      self,
      aihero-skills,
      nixpkgs,
      ...
    }:
    let
      aiHeroManifest = builtins.fromJSON (builtins.readFile ./manifests/aihero-skills.json);
      cutoverManifest = builtins.fromJSON (builtins.readFile ./manifests/workflow-cutover.json);
      upstreamPluginManifest = builtins.fromJSON (
        builtins.readFile (aihero-skills + "/.claude-plugin/plugin.json")
      );
      aiHeroSkills = aiHeroManifest.skills;
      aiHeroSkillNames = map (skill: skill.name) aiHeroSkills;
      upstreamPluginSkillNames = map builtins.baseNameOf upstreamPluginManifest.skills;
      localSkillNames = [
        "route-work"
        "work-ledger"
        "record-decision"
        "self-verification"
        "use-repo-local-workspace"
      ];
      sharedSkillNames = localSkillNames ++ aiHeroSkillNames;
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

      localSkills = map (name: {
        inherit name;
        source = "${self}/skills/${name}";
      }) localSkillNames;

      externalSkills = map (skill: {
        inherit (skill) name files;
        source = builtins.path {
          path = aihero-skills + "/${skill.subdir}";
          name = "aihero-${skill.name}";
          sha256 = skill.nar_hash;
        };
      }) aiHeroSkills;

      mkSkillEntries =
        target: skills:
        builtins.listToAttrs (
          map (skill: {
            name = "${target}/${skill.name}";
            value = {
              inherit (skill) source;
              recursive = true;
            };
          }) skills
        );

      mkFileEntries =
        target: sourceDir: names:
        builtins.listToAttrs (
          map (name: {
            name = "${target}/${name}";
            value.source = "${self}/${sourceDir}/${name}";
          }) names
        );

      # Keep the initializer's source tree in the Nix store. Its manifest and
      # project-local Skill templates are resolved relative to __file__ by the
      # Python implementation, so a copied standalone script is insufficient.
      mkAgentWorkflowInit =
        pkgs:
        pkgs.writeShellApplication {
          name = "agent-workflow-init";
          runtimeInputs = [
            pkgs.git
            pkgs.python3
          ];
          text = ''
            exec ${pkgs.python3}/bin/python ${self}/scripts/agent-workflow-init.py "$@"
          '';
        };
    in
    {
      inherit aiHeroSkillNames sharedSkillNames;
      aiHeroSkillManifest = aiHeroManifest;
      workflowCutoverManifest = cutoverManifest;

      packages = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          "agent-workflow-init" = mkAgentWorkflowInit pkgs;
        }
      );

      apps = forAllSystems (
        system: {
          "agent-workflow-init" = {
            type = "app";
            program = "${self.packages.${system}.agent-workflow-init}/bin/agent-workflow-init";
          };
        }
      );

      checks = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          files = (self.homeManagerModules.default { inherit pkgs; }).home.file;
          names = builtins.attrNames files;
          claudeSkillCount = builtins.length (
            builtins.filter (name: builtins.match "[.]claude/skills/.*" name != null) names
          );
          sharedSkillCount = builtins.length (
            builtins.filter (name: builtins.match "[.]agents/skills/.*" name != null) names
          );
          codexSkillCount = builtins.length (
            builtins.filter (name: builtins.match "[.]codex/skills/.*" name != null) names
          );
          allSourcesExist = builtins.all (
            name:
            !(files.${name} ? source)
            || builtins.isAttrs files.${name}.source
            || builtins.pathExists files.${name}.source
          ) names;
        in
        {
          workflow-contract =
            assert claudeSkillCount == 30;
            assert sharedSkillCount == 30;
            assert codexSkillCount == 30;
            assert allSourcesExist;
            pkgs.runCommand "flake-my-claude-workflow-contract"
              {
                nativeBuildInputs = [
                  pkgs.git
                  pkgs.python3
                ];
              }
              ''
                python ${self}/checks/validate-workflow.py
                python -m unittest discover -s ${self}/hooks/tests -v
                touch "$out"
              '';
        }
      );

      homeManagerModules.default =
        { pkgs, ... }:
        let
          expectedAiHeroCount = aiHeroManifest.upstream.release_boundary.expected_skill_count;
          expectedAiHeroFileCount = aiHeroManifest.upstream.release_boundary.expected_file_count;
          manifestAiHeroFileCount = builtins.foldl' (
            total: skill: total + builtins.length skill.files
          ) 0 aiHeroSkills;
          uniqueSharedSkillCount = builtins.length (
            builtins.attrNames (
              builtins.listToAttrs (
                map (name: {
                  inherit name;
                  value = true;
                }) sharedSkillNames
              )
            )
          );
          agentWorkflowInit = mkAgentWorkflowInit pkgs;
          workLedgerHook = pkgs.writeShellScript "work-ledger-hook" ''
            exec ${pkgs.python3}/bin/python ${self}/hooks/work-ledger-hook.py "$@"
          '';
          claudeAgentNames = [
            "workflow-orchestrator-opus.md"
            "workflow-architect-opus.md"
            "workflow-explorer.md"
            "workflow-worker.md"
            "workflow-reviewer.md"
            "workflow-verifier.md"
          ];
          codexAgentNames = [
            "workflow-explorer.toml"
            "workflow-worker.toml"
            "workflow-reviewer.toml"
            "workflow-verifier.toml"
          ];
        in
        assert builtins.length aiHeroSkills == expectedAiHeroCount;
        assert aiHeroSkillNames == upstreamPluginSkillNames;
        assert manifestAiHeroFileCount == expectedAiHeroFileCount;
        assert builtins.all (
          skill: builtins.all (file: builtins.pathExists "${skill.source}/${file}") skill.files
        ) externalSkills;
        assert builtins.length sharedSkillNames == uniqueSharedSkillCount;
        {
          programs.claude-code.enable = true;
          programs.codex = {
            enable = true;
            package = pkgs.codex;
            # Keep Codex configuration under the explicit home.file entries below.
            settings = null;
          };

          # AI-DLC requires bun in the interactive Home Manager environment.
          # The initializer retains its own per-selection prerequisite check.
          home.packages = [
            agentWorkflowInit
            pkgs.bun
          ];

          home.file = {
            ".claude/CLAUDE.md".source = "${self}/CLAUDE.md";
            ".claude/settings.json".source = "${self}/settings.json";

            ".claude/rules/output-style.md".source = "${self}/rules/output-style.md";
            ".claude/rules/operation-safety.md".source = "${self}/rules/operation-safety.md";
            ".claude/rules/nix-devenv.md".source = "${self}/rules/nix-devenv.md";
            ".claude/rules/codex-nix-config.md".source = "${self}/rules/codex-nix-config.md";

            ".claude/statusline.sh" = {
              source = "${self}/statusline.sh";
              executable = true;
            };
            ".claude/log-permission-request.sh" = {
              source = "${self}/log-permission-request.sh";
              executable = true;
            };
            ".claude/session-status.sh" = {
              source = "${self}/session-status.sh";
              executable = true;
            };
            ".claude/hooks/work-ledger-hook".source = workLedgerHook;

            ".codex/AGENTS.md".source = "${self}/codex/AGENTS.md";
            ".codex/hooks.json".source = "${self}/codex/hooks.json";
            ".codex/hooks/work-ledger-hook".source = workLedgerHook;

            ".local/share/licenses/mattpocock-skills/LICENSE".source = "${aihero-skills}/LICENSE";
            ".local/share/agent-skills/mattpocock-skills/manifest.json".source =
              "${self}/manifests/aihero-skills.json";
          }
          // mkSkillEntries ".claude/skills" (localSkills ++ externalSkills)
          // mkSkillEntries ".agents/skills" (localSkills ++ externalSkills)
          // mkSkillEntries ".codex/skills" (localSkills ++ externalSkills)
          // mkFileEntries ".claude/agents" "agents" claudeAgentNames
          // mkFileEntries ".codex/agents" "codex/agents" codexAgentNames;
        };
    };
}
