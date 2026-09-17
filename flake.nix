{
  description = "Minimal AI agent instructions and skills";

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
      agentInstructions = ./AGENTS.md;
      rubberDuckSkill = ./skills/rubber-duck;
    in
    {
      checks = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          module = self.homeManagerModules.default { inherit pkgs; };
          claude = module.programs.claude-code;
          codex = module.programs.codex;
        in
        {
          minimal-agent-config =
            assert codex.context == agentInstructions;
            assert builtins.attrNames codex.skills == [ "rubber-duck" ];
            assert claude.context == agentInstructions;
            assert builtins.attrNames claude.skills == [ "rubber-duck" ];
            assert claude.settings.skillOverrides.rubber-duck == "user-invocable-only";
            assert !(codex ? settings);
            assert !(module ? home);
            pkgs.runCommand "minimal-agent-config" { } ''
              test -f ${agentInstructions}
              test -f ${rubberDuckSkill}/SKILL.md
              test -f ${rubberDuckSkill}/agents/openai.yaml
              touch "$out"
            '';
        }
      );

      homeManagerModules.default =
        { pkgs, ... }:
        {
          programs.claude-code = {
            enable = true;
            context = agentInstructions;
            skills.rubber-duck = rubberDuckSkill;
            settings.skillOverrides.rubber-duck = "user-invocable-only";
          };

          programs.codex = {
            enable = true;
            package = pkgs.codex;
            context = agentInstructions;
            skills.rubber-duck = rubberDuckSkill;
          };
        };
    };
}
