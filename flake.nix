{
  description = "Claude Code configuration";

  outputs = { self, ... }: {
    homeManagerModules.default = { ... }: {
      home.file = {
        ".claude/CLAUDE.md".source = "${self}/CLAUDE.md";
        ".claude/settings.json".source = "${self}/settings.json";

        ".claude/commands/init-personal.md".source = "${self}/commands/init-personal.md";
        ".claude/commands/init-work.md".source = "${self}/commands/init-work.md";
        ".claude/commands/worktree.md".source = "${self}/commands/worktree.md";
        ".claude/commands/nix-check.md".source = "${self}/commands/nix-check.md";
        ".claude/commands/edit-claude.md".source = "${self}/commands/edit-claude.md";
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
        ".claude/commands/code-patrol.md".source = "${self}/commands/code-patrol.md";

        ".claude/rules/output-style.md".source = "${self}/rules/output-style.md";
        ".claude/rules/operation-safety.md".source = "${self}/rules/operation-safety.md";
        ".claude/rules/plan-file.md".source = "${self}/rules/plan-file.md";
        ".claude/rules/nix-devenv.md".source = "${self}/rules/nix-devenv.md";

        ".claude/skills/plan-task/SKILL.md".source = "${self}/skills/plan-task/SKILL.md";
        ".claude/skills/plan-task/plan-file-template.md".source = "${self}/skills/plan-task/plan-file-template.md";
        ".claude/skills/self-verification-loop/SKILL.md".source = "${self}/skills/self-verification-loop/SKILL.md";
        ".claude/skills/self-verification-loop/loop-patterns.md".source = "${self}/skills/self-verification-loop/loop-patterns.md";
        ".claude/skills/self-verification-loop/review-quorum.md".source = "${self}/skills/self-verification-loop/review-quorum.md";
        ".claude/skills/self-verification-loop/smart-goal-template.md".source = "${self}/skills/self-verification-loop/smart-goal-template.md";
        ".claude/skills/self-verification-loop/verification-report-template.md".source = "${self}/skills/self-verification-loop/verification-report-template.md";

        ".claude/agents/quick-explorer.md".source = "${self}/agents/quick-explorer.md";
        ".claude/agents/deep-explorer.md".source = "${self}/agents/deep-explorer.md";
        ".claude/agents/log-analyzer.md".source = "${self}/agents/log-analyzer.md";
        ".claude/agents/test-failure-analyzer.md".source = "${self}/agents/test-failure-analyzer.md";
        ".claude/agents/change-reviewer.md".source = "${self}/agents/change-reviewer.md";
        ".claude/agents/security-reviewer.md".source = "${self}/agents/security-reviewer.md";
        ".claude/agents/plan-checker.md".source = "${self}/agents/plan-checker.md";
        ".claude/agents/implementation-agent.md".source = "${self}/agents/implementation-agent.md";
        ".claude/agents/check-runner.md".source = "${self}/agents/check-runner.md";
        ".claude/agents/final-auditor.md".source = "${self}/agents/final-auditor.md";

        ".claude/team/init-team.sh" = { source = "${self}/team/init-team.sh"; executable = true; };
        ".claude/team/spawn-worker.sh" = { source = "${self}/team/spawn-worker.sh"; executable = true; };
        ".claude/team/send-message.sh" = { source = "${self}/team/send-message.sh"; executable = true; };
        ".claude/team/wait-workers.sh" = { source = "${self}/team/wait-workers.sh"; executable = true; };
        ".claude/team/read-buffer.sh" = { source = "${self}/team/read-buffer.sh"; executable = true; };
        ".claude/team/cleanup-team.sh" = { source = "${self}/team/cleanup-team.sh"; executable = true; };
        ".claude/team/team-msg.sh" = { source = "${self}/team/team-msg.sh"; executable = true; };
        ".claude/team/setup-env.sh" = { source = "${self}/team/setup-env.sh"; executable = true; };

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
