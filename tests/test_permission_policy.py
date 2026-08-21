import json
from pathlib import Path
import unittest


SETTINGS = Path(__file__).resolve().parents[1] / "settings.json"

SENSITIVE_ENV_READS = {
    "Read(**/.env)",
    "Read(**/.env.local)",
    "Read(**/.env.development.local)",
    "Read(**/.env.test.local)",
    "Read(**/.env.production.local)",
}

SECRET_AND_KEY_READS = {
    "Read(**/credentials.json)",
    "Read(**/secrets.yaml)",
    "Read(**/secrets.yml)",
    "Read(**/*.pem)",
    "Read(**/*.key)",
    "Read(**/*.p12)",
    "Read(**/*.pfx)",
    "Read(**/*.keystore)",
    "Read(**/id_rsa)",
    "Read(**/id_ed25519)",
    "Read(**/.ssh/config)",
    "Read(**/.netrc)",
    "Read(**/.npmrc)",
    "Read(**/.pypirc)",
}

RETAINED_PATH_WRITES = {
    "Edit(~/.ssh/**)",
    "Write(~/.ssh/**)",
    "Edit(~/.gnupg/**)",
    "Write(~/.gnupg/**)",
    "Edit(~/.emacs.d/**)",
    "Write(~/.emacs.d/**)",
    "Edit(~/.nix-defexpr/**)",
    "Write(~/.nix-defexpr/**)",
    "Edit(~/.nix-profile/**)",
    "Write(~/.nix-profile/**)",
    "Edit(~/.claude/settings.json)",
    "Write(~/.claude/settings.json)",
    "Edit(~/.claude/keybindings.json)",
    "Write(~/.claude/keybindings.json)",
    "Edit(//etc/**)",
    "Write(//etc/**)",
    "Edit(//nix/**)",
    "Write(//nix/**)",
    "Edit(//usr/**)",
    "Write(//usr/**)",
    "Edit(//var/**)",
    "Write(//var/**)",
    "Edit(//boot/**)",
    "Write(//boot/**)",
    "Edit(//sys/**)",
    "Write(//sys/**)",
    "Edit(//proc/**)",
    "Write(//proc/**)",
}

RETAINED_DESTRUCTIVE_COMMANDS = {
    "Bash(git push --force *)",
    "Bash(git push --force)",
    "Bash(git push -f *)",
    "Bash(git push -f)",
    "Bash(git reset --hard *)",
    "Bash(git reset --hard)",
    "Bash(git clean -f *)",
    "Bash(git clean -f)",
    "Bash(git clean -fd *)",
    "Bash(git clean -df *)",
    "Bash(git checkout -- .)",
    "Bash(git checkout .)",
    "Bash(git restore .)",
    "Bash(git branch -D *)",
    "Bash(git branch -d --force *)",
    "Bash(git rebase -i *)",
    "Bash(git rebase --interactive *)",
    "Bash(rm -rf /*)",
    "Bash(rm -rf /)",
    "Bash(rm -rf ~/*)",
    "Bash(rm -rf ~/)",
    "Bash(chmod -R *)",
    "Bash(chown -R *)",
    "Bash(sudo *)",
    "Bash(su *)",
    "Bash(kill -9 *)",
    "Bash(shutdown *)",
    "Bash(reboot *)",
    "Bash(dd *)",
}

RELAXED_DENIES = {
    "Edit(~/.config/**)",
    "Write(~/.config/**)",
    "Edit(~/.local/**)",
    "Write(~/.local/**)",
    "Edit(~/.claude/settings.local.json)",
    "Write(~/.claude/settings.local.json)",
    "Edit(~/.claude/skills/**)",
    "Write(~/.claude/skills/**)",
    "Bash(pkill *)",
    "Bash(killall *)",
    "Bash(systemctl *)",
    "Bash(npm install -g *)",
    "Bash(npm i -g *)",
    "Bash(pip install *)",
    "Bash(pip3 install *)",
    "Bash(gem install *)",
    "Bash(cargo install *)",
    "Bash(nix-env --install *)",
    "Bash(nix-env -i *)",
    "Bash(nix-env --uninstall *)",
    "Bash(nix-env -e *)",
    "Bash(nix-env --upgrade *)",
    "Bash(nix-env -u *)",
}
for directory in (
    "Desktop",
    "Documents",
    "Downloads",
    "Music",
    "Pictures",
    "Videos",
    "Templates",
    "Public",
):
    RELAXED_DENIES.update(
        {
            f"Edit(~/{directory}/**)",
            f"Write(~/{directory}/**)",
        }
    )

APPROVED_DENIES = (
    SENSITIVE_ENV_READS
    | SECRET_AND_KEY_READS
    | RETAINED_PATH_WRITES
    | RETAINED_DESTRUCTIVE_COMMANDS
)


class PermissionPolicyTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        settings = json.loads(SETTINGS.read_text(encoding="utf-8"))
        cls.permissions = settings["permissions"]
        cls.deny = set(cls.permissions["deny"])

    def test_default_mode_and_allow_list_remain_unexpanded(self):
        self.assertEqual(self.permissions["defaultMode"], "default")
        self.assertEqual(self.permissions["allow"], [])

    def test_sensitive_environment_and_credential_reads_remain_denied(self):
        self.assertTrue(SENSITIVE_ENV_READS <= self.deny)
        self.assertTrue(SECRET_AND_KEY_READS <= self.deny)
        self.assertNotIn("Read(**/.env.*)", self.deny)
        self.assertNotIn("Read(**/.env.example)", self.deny)
        self.assertNotIn("Read(**/.env.sample)", self.deny)

    def test_home_manager_system_and_destructive_protections_remain_denied(self):
        self.assertTrue(RETAINED_PATH_WRITES <= self.deny)
        self.assertTrue(RETAINED_DESTRUCTIVE_COMMANDS <= self.deny)

    def test_only_approved_classes_are_relaxed(self):
        self.assertFalse(RELAXED_DENIES & self.deny)
        self.assertEqual(self.deny, APPROVED_DENIES)
        self.assertEqual(len(self.permissions["deny"]), len(self.deny))


if __name__ == "__main__":
    unittest.main()
