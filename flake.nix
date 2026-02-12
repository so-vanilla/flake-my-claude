{
  description = "Claude Code configuration";

  outputs = { self, ... }: {
    homeManagerModules.default = { ... }: {
      programs.claude-code.enable = true;

      home.file = {
        ".claude/CLAUDE.md".source = "${self}/CLAUDE.md";
        ".claude/settings.json".source = "${self}/settings.json";
        ".claude/commands/init-personal.md".source = "${self}/commands/init-personal.md";
        ".claude/commands/init-work.md".source = "${self}/commands/init-work.md";
        ".claude/commands/worktree.md".source = "${self}/commands/worktree.md";
        ".claude/commands/nix-check.md".source = "${self}/commands/nix-check.md";
        ".claude/statusline.sh" = {
          source = "${self}/statusline.sh";
          executable = true;
        };
      };
    };
  };
}
