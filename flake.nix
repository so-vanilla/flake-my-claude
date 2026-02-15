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
        ".claude/commands/perm-review.md".source = "${self}/commands/perm-review.md";
        ".claude/commands/team.md".source = "${self}/commands/team.md";
        ".claude/team/init-team.sh" = { source = "${self}/team/init-team.sh"; executable = true; };
        ".claude/team/spawn-worker.sh" = { source = "${self}/team/spawn-worker.sh"; executable = true; };
        ".claude/team/send-message.sh" = { source = "${self}/team/send-message.sh"; executable = true; };
        ".claude/team/wait-workers.sh" = { source = "${self}/team/wait-workers.sh"; executable = true; };
        ".claude/team/read-buffer.sh" = { source = "${self}/team/read-buffer.sh"; executable = true; };
        ".claude/team/cleanup-team.sh" = { source = "${self}/team/cleanup-team.sh"; executable = true; };
        ".claude/statusline.sh" = {
          source = "${self}/statusline.sh";
          executable = true;
        };
        ".claude/session-status.sh" = {
          source = "${self}/session-status.sh";
          executable = true;
        };
        ".claude/log-permission-request.sh" = {
          source = "${self}/log-permission-request.sh";
          executable = true;
        };
      };
    };
  };
}
