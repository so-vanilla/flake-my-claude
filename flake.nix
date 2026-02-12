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
        ".claude/commands/commit.md".source = "${self}/commands/commit.md";
        ".claude/commands/pr.md".source = "${self}/commands/pr.md";
        ".claude/commands/test.md".source = "${self}/commands/test.md";
        ".claude/commands/format.md".source = "${self}/commands/format.md";
        ".claude/commands/explore.md".source = "${self}/commands/explore.md";
        ".claude/commands/cleanup.md".source = "${self}/commands/cleanup.md";
        ".claude/commands/dep-update.md".source = "${self}/commands/dep-update.md";
        ".claude/commands/security-review.md".source = "${self}/commands/security-review.md";
        ".claude/commands/changelog.md".source = "${self}/commands/changelog.md";
        ".claude/statusline.sh" = {
          source = "${self}/statusline.sh";
          executable = true;
        };
      };
    };
  };
}
