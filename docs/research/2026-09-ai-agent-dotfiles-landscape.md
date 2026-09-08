# 個人dotfilesとしてのAIエージェント設定 — 2026年9月スコーピング調査

調査基準日: 2026-09-01（Asia/Tokyo）
状態: research complete / popularity snapshot / no installation or configuration decision

## 1. 結論

個人のAIエージェントdotfilesには、2026年9月時点で「誰もがコピーする一つの標準repo」はない。可視性とAI設定の厚みを両方持つ主要例には `omerxx/dotfiles`、`jessfraz/dotfiles`、`nicknisi/dotfiles` があるが、それぞれOpenCodeの役割分担と出荷workflow、厚いCodex policy、短いClaude instructions + on-demand Skillsという異なる方向を取る。複数toolの正本共有、platform差分、mutable state、backup、dry-run、uninstallまでを扱う設計は、さらに小さなrepoへ分散している。

したがって、見るべきものは一つの総合順位ではなく、次の二軸である。

1. **可視性の代理指標**: stars、forks、watch subscribers、更新日。これは利用者数、品質、安全性、効果を証明しない。
2. **設計上の参考度**: 正本の置き方、agent別adapter、mutable state分離、権限、hook、MCP、secret、install/update/rollbackの扱い。

本調査の推奨は、人気repoを丸ごと導入することではない。複数repoから次の型を選んで、自分の要件として再実装することである。

- 短い共通方針を一つの正本に置き、`CLAUDE.md`、`AGENTS.md`、`GEMINI.md`等は薄いnative adapterにする。
- toolごとの意味差は生成・変換・除外リストで明示し、単純symlinkを「互換性」とみなさない。
- `~/.claude` や `~/.codex` 全体をrepoへsymlinkしない。管理対象の子だけをlink/copyし、session、database、auth、cache、trust、UI stateをlocalに残す。
- public dotfilesにはportable defaultだけを置き、secret、絶対path、project trust、private Skills/MCP credentialを分離する。
- installはpreview、conflict detection、backup、ownership記録、uninstall、post-install verificationを持つ。
- personal dotfilesの権限設定は、その人のrisk appetiteであり、会社policyの初期値にはしない。

## 2. 問いと対象境界

### 問い

- AIエージェント設定を含む個人dotfilesとして、現在可視性が高いものは何か。
- 実際にどのファイルを管理し、どこをmachine-localに残しているか。
- どの構造が再利用でき、どの設定はそのままコピーすると危険か。
- 個人の工夫を会社用標準へ移す際、何が不足するか。

### 採用条件

`eligible` 候補は次をすべて満たすrepoとした。

1. 個人の開発環境またはdotfilesであることをrepo自身が示す。
2. Claude Code、Codex、Gemini CLI、Pi等のAIエージェント設定が現在のtreeで読める。
3. Skill配布、agent framework、awesome list、vendor exampleがrepoの主目的ではない。個人dotfilesが多数のSkillsも含む場合は、repo全体の主目的とAI subtreeの比重を見て `boundary` として分ける。
4. GitHub APIとdefault branchの実ファイルを2026-09-01に確認できる。

`holman/dotfiles` と `thoughtbot/dotfiles` は、それぞれ7,767 stars / 3,221 forks、8,171 stars / 1,774 forksの古典的dotfiles参照先だったが、現在treeに対象のAI-agent設定pathを確認できなかったためランキングから除外した。ただし `haacked/dotfiles` が `holman/dotfiles` 由来の構造を明記しており、AI以前のdotfile配布慣行が現在のagent設定へ継承されている証拠にはなる。[holman/dotfiles](https://github.com/holman/dotfiles) [thoughtbot/dotfiles](https://github.com/thoughtbot/dotfiles) [haacked README](https://github.com/haacked/dotfiles/blob/895d4b06a963c72c7ed269a4aa40d97bf04463b2/README.md)

Superpowers、AI-DLC、Skill catalog、agent harness、awesome list、prompt集は、前回本編の方法論・distribution調査には含まれるが、今回の「個人dotfiles」ランキングからは除外した。

## 3. 調査方法と限界

GitHub repository search（`dotfiles in:name fork:false archived:false` のstars上位50件）、複数のcode search、indexed web search、既知候補のcurrent tree確認を組み合わせた。候補ごとにGitHub REST APIから stars、forks、`subscribers_count`、`pushed_at` を採取し、主要候補は代表AI pathのcommit履歴も確認した。設計比較はshallow cloneしたdefault branchのcommitを固定して行った。人気度の採取時刻は **2026-09-01T10:59:25Z**。リンク先の現在値は将来変わる。

この調査は全GitHubの完全列挙ではない。一つのcode-search経路は調査途中でrate limitに達したが、独立したrepository上位探索、code search、web index、候補repoのmanual tree inspectionで補完した。private repo、検索indexに出ないrepo、他言語の記述、dotfiles以外の名前を持つ個人設定は過小評価される。また、stars/forksから実install数、active use、成果、securityを推定しない。

「よく参考にされる」の直接的な統一指標は存在しない。本調査では forksをcopy/派生の弱い代理、starsを可視性の代理、READMEの明示的なinspirationを直接証拠として扱う。

## 4. 人気度snapshotと位置づけ

watchはGitHub APIの `subscribers_count` を用いた。GitHubの `watchers_count` は現在starsと同値になるため使っていない。表はstars順の可視性landscapeを見せるため、主要適格候補に加えて「高starsだがAI面が狭い例」と「Skill distributionとの境界例」もstatus付きで含む。設計上の推奨順位ではない。

| stars順 | repo | stars | forks | watch | pushed_at (UTC) | AI path活動 | status | 本調査での位置づけ |
| ---: | --- | ---: | ---: | ---: | --- | --- | --- | --- |
| 1 | [omerxx/dotfiles](https://github.com/omerxx/dotfiles) | 3,584 | 284 | 53 | 2026-04-13 | 2 commits | eligible | OpenCode agents/commands/Skill/TUI/editor統合 |
| 2 | [jessfraz/dotfiles](https://github.com/jessfraz/dotfiles) | 3,562 | 507 | 113 | 2026-08-31 | 49 commits | eligible | 厚いCodex instructions + Nix config |
| 3 | [nicknisi/dotfiles](https://github.com/nicknisi/dotfiles) | 2,986 | 373 | 79 | 2026-09-01 | 75 commits | eligible | 高認知かつ継続更新されるClaude/Pi設定 |
| 4 | [linkarzu/dotfiles-latest](https://github.com/linkarzu/dotfiles-latest) | 1,536 | 155 | 16 | 2026-08-26 | 3 commits | eligible | 高認知だがAI面は狭いOpenCode permissions |
| 5 | [freekmurze/dotfiles](https://github.com/freekmurze/dotfiles) | 1,000 | 201 | 32 | 2026-08-17 | 20 commits | boundary | 個人dotfilesだが大規模Skill catalogとの境界例 |
| 6 | [yutkat/dotfiles](https://github.com/yutkat/dotfiles) | 990 | 49 | 10 | 2026-09-01 | 5 commits | eligible | Mise/Nix、Claude/Codex/OpenCode native paths |
| 7 | [joshsymonds/nix-config](https://github.com/joshsymonds/nix-config) | 835 | 121 | 7 | 2026-09-01 | 10 commits | eligible | Home Manager、Claude/Codex、厚い個人policy |
| 8 | [citypaul/.dotfiles](https://github.com/citypaul/.dotfiles) | 723 | 93 | 10 | 2026-08-15 | 3 commits | boundary | 個人dotfilesだがSkill catalog境界例 |
| 9 | [sapegin/dotfiles](https://github.com/sapegin/dotfiles) | 534 | 51 | 8 | 2026-08-31 | 3 commits | eligible | Cursor MCPの狭い参考例 |
| 10 | [joshukraine/dotfiles](https://github.com/joshukraine/dotfiles) | 422 | 49 | 3 | 2026-08-12 | — | eligible | Claude lifecycle/UI統合、runtime-state事故記録 |
| 11 | [TechDufus/dotfiles](https://github.com/TechDufus/dotfiles) | 407 | 65 | 4 | 2026-08-31 | 13 commits | eligible | Ansible managed manifest、複数harness |
| 12 | [rgomezcasas/dotfiles](https://github.com/rgomezcasas/dotfiles) | 362 | 44 | 16 | 2026-08-19 | 1 commit | eligible | 公開AI面が薄くprivate依存 |
| 13 | [JDevlieghere/dotfiles](https://github.com/JDevlieghere/dotfiles) | 304 | 55 | 13 | 2026-08-28 | 3 commits | eligible | 短いClaude global instructions |
| 14 | [ahmedelgabri/dotfiles](https://github.com/ahmedelgabri/dotfiles) | 291 | 18 | 8 | 2026-08-31 | 3 commits | eligible | Nix + Pi/Claude/Codex/OpenCodeの短い例 |
| 15 | [meain/dotfiles](https://github.com/meain/dotfiles) | 284 | 10 | 10 | 2026-09-01 | 20 commits | eligible | Stow、Claude hooks、status/notification |
| 16 | [ryoppippi/dotfiles](https://github.com/ryoppippi/dotfiles) | 260 | 5 | 2 | 2026-09-01 | 20 commits | eligible | Nix、Claude/Codex/OpenCode/Cursor |
| 17 | [jackfranklin/dotfiles](https://github.com/jackfranklin/dotfiles) | 254 | 33 | 14 | 2026-09-01 | 17 commits | eligible | Claude/Pi、非破壊MCP同期 |
| 18 | [wcygan/dotfiles](https://github.com/wcygan/dotfiles) | 194 | 16 | 8 | 2026-08-30 | — | eligible | portable template、Skill pin、recovery |
| 19 | [liby/dotfiles](https://github.com/liby/dotfiles) | 149 | 12 | 2 | 2026-09-01 | — | eligible | partial merge、keyring、security hooks |
| 20 | [haacked/dotfiles](https://github.com/haacked/dotfiles) | 131 | 22 | 5 | 2026-08-29 | — | eligible | cross-agent render、exclusion、MCP inventory |

`pushed_at` はrepo全体、`AI path活動` は代表AI設定ファイル一つの全commit件数である。後者はAI subtreeの広さや更新日を表さず、`—` は同じ方法の履歴件数を採取していないことを表す。

この表から分かるのは、「可視性の高いdotfiles」と「AI-agent運用設計が厚いdotfiles」は一致しないことである。4位はAI面が狭く、5位と8位はSkill-distributionとの境界にある。一方、18–20位には上位repoにないruntime ownership、cross-agent semantic conversion、pin/recoveryがある。

### 4.1 設計別reference shortlist

次は実際の参照回数や優劣の順位ではない。本調査者が一次資料を読んで、設計課題ごとの参照先を分類したshortlistである。

| 設計課題 | まず読む候補 | 見る箇所 |
| --- | --- | --- |
| 短いalways-on + on-demand | `nicknisi`、`liby` | [短いglobal instructions](https://github.com/nicknisi/dotfiles/blob/4f9d3afe824a597eef9a093612fb9dbcbce2b62c/home/.claude/CLAUDE.md)、[routing map](https://github.com/liby/dotfiles/blob/0346449a94932aa21fbce31df1f4875d9c53ffd4/AGENTS.md#L26-L50) |
| 厚いglobal policy | `jessfraz`、`joshsymonds` | [Codex AGENTS.md](https://github.com/jessfraz/dotfiles/blob/852ea909fde7390e6e44c5a2a1bde4a7088ad871/.codex/AGENTS.md)、[Claude policy](https://github.com/joshsymonds/nix-config/blob/5203a334d8302478ad2a1efd9e8876265b200573/home-manager/claude-code/CLAUDE.md) |
| cross-agent projection | `haacked`、`freekmurze` | [renderer contract](https://github.com/haacked/dotfiles/blob/895d4b06a963c72c7ed269a4aa40d97bf04463b2/ai/README.md)、[tool別Skill link](https://github.com/freekmurze/dotfiles/blob/09219e07aab14ea34375e85fd9597b63ca88f239/bin/link-agent-skills) |
| runtime ownership | `joshukraine`、`wcygan` | [directory-folding事故と修復](https://github.com/joshukraine/dotfiles/blob/157799742c2d0f455feac89c485991a68a1e6605/README.md)、[portable Codex template](https://github.com/wcygan/dotfiles/blob/e2c9eb73afa27b06594bb24912a07728eea6b936/config/codex/config.toml) |
| permissions / hooks | `nicknisi`、`meain` | [allow/deny/ask](https://github.com/nicknisi/dotfiles/blob/4f9d3afe824a597eef9a093612fb9dbcbce2b62c/home/.claude/settings.json)、[lifecycle hooks](https://github.com/meain/dotfiles/blob/1bb42a69a70569f01128b0adb573d6b17e68e06c/claude/.claude/settings.json) |
| MCP / secrets | `jackfranklin`、`liby` | [MCP source](https://github.com/jackfranklin/dotfiles/blob/a16ec499e98d0fbae9f595285bba4988f1b6362a/claude/mcp.json)、[security hooks](https://github.com/liby/dotfiles/blob/0346449a94932aa21fbce31df1f4875d9c53ffd4/.chezmoitemplates/claude/settings.json#L37-L95) |
| install / recovery | `TechDufus`、`haacked` | [managed manifest](https://github.com/TechDufus/dotfiles/blob/56a0085acb2bc4f835ce6f20aa7a1fe5a80ecc16/roles/codex/tasks/main.yml)、[installer contract](https://github.com/haacked/dotfiles/blob/895d4b06a963c72c7ed269a4aa40d97bf04463b2/ai/README.md) |
| terminal / editor loop | `omerxx`、`denolfe` | [Neovim integration](https://github.com/omerxx/dotfiles/blob/97b2213ad0e61c7a036290752e23497920978d55/nvim/lua/plugins/opencode.lua)、[local plugins](https://github.com/denolfe/dotfiles/tree/800fa363fa2c7d2474e6511f4d5feb6eb57bd04b/claude/local-plugins) |

## 5. 候補別の参考ポイント

### 5.1 `omerxx/dotfiles`: OpenCodeを役割分担と出荷loopまで統合

GNU Stowで個人環境を配り、OpenCodeには6つの専門agent、build/scan commands、出荷用Skill、TUI keybindingsを置き、Neovimからも呼び出す。単なるmodel指定ではなく、requirements、architecture、implementation、test、reviewを役割へ分け、人間のterminal/editor loopへ接続した高可視性の実例である。[README](https://github.com/omerxx/dotfiles/blob/97b2213ad0e61c7a036290752e23497920978d55/README.md) [OpenCode tree](https://github.com/omerxx/dotfiles/tree/97b2213ad0e61c7a036290752e23497920978d55/opencode) [agents](https://github.com/omerxx/dotfiles/tree/97b2213ad0e61c7a036290752e23497920978d55/opencode/agent) [commands](https://github.com/omerxx/dotfiles/tree/97b2213ad0e61c7a036290752e23497920978d55/opencode/command) [Neovim integration](https://github.com/omerxx/dotfiles/blob/97b2213ad0e61c7a036290752e23497920978d55/nvim/lua/plugins/opencode.lua)

ただし `ship` Skillはcommit、rebase、push、PR作成、automated reviewまでを一続きに実行する個人workflowである。[ship Skill](https://github.com/omerxx/dotfiles/blob/97b2213ad0e61c7a036290752e23497920978d55/opencode/skills/ship/SKILL.md) 役割分担とUI統合は参考になるが、外部write権限と完了条件は自分または会社のapproval boundaryに合わせて再設計する必要がある。repoのstarsは主にdotfiles全体への指標であり、2026年3月に追加されたAI subtree単体の採用数とは読めない。

### 5.2 `nicknisi/dotfiles`: 人気とAI設定の継続更新を両立

READMEはstarter kitではなく本人の実dotfilesと明記し、Miseで `home/` をhome directoryへ配置する。Claudeのglobal instructionsは比較的短く、詳細workflowをon-demand Skillsへ出す。settingsはallow/deny/ask、credential path deny、SessionEnd hook、statusline、plugin marketplaceを持ち、代表settings pathに75 commitsの継続更新がある。[README](https://github.com/nicknisi/dotfiles/blob/4f9d3afe824a597eef9a093612fb9dbcbce2b62c/README.md) [CLAUDE.md](https://github.com/nicknisi/dotfiles/blob/4f9d3afe824a597eef9a093612fb9dbcbce2b62c/home/.claude/CLAUDE.md) [settings.json](https://github.com/nicknisi/dotfiles/blob/4f9d3afe824a597eef9a093612fb9dbcbce2b62c/home/.claude/settings.json)

人気だけでなくAI pathの更新履歴が厚い主要参照例である。ただし個人の絶対path、実験flag、広いcommand allowを含み、policy値はコピー対象ではない。

### 5.3 `jessfraz/dotfiles`: 可視性は最大級、AI設定はCodex中心

探索landscapeではstarsが2位、forksとwatch subscribersが最大である。instructionは `.codex/AGENTS.md` が中心で、outcome、approval boundary、tool preference、review、testing、language別規約、final handoffまでを一つのalways-on文書に持つ。Nix側ではmodel、reasoning、features、plugins、MCP、sandbox、project trustをattrsetからCodex TOMLへ生成する。[pinned AGENTS.md](https://github.com/jessfraz/dotfiles/blob/852ea909fde7390e6e44c5a2a1bde4a7088ad871/.codex/AGENTS.md) [Nix Codex config](https://github.com/jessfraz/dotfiles/blob/852ea909fde7390e6e44c5a2a1bde4a7088ad871/nix/codex-config.nix)

参考になるのは、権限境界と完了時の検証を具体化し、宣言的にnative configを生成している点である。一方、約16 KBの単一global文書にはtask-specificな技術規約も大量に入り、minimal always-on / progressive disclosureの参照例にはならない。appが書き戻すmutable configをimmutable symlinkへ投影する場合の衝突にも注意が要る。また個人向けのrebase/force-push許可や周辺papercut修正権限は、そのまま会社標準へ移せない。

### 5.4 `freekmurze/dotfiles`: 個人dotfilesとSkill catalogの境界

短い共通 `AGENTS.md` をClaudeの `CLAUDE.md` とCodexの `AGENTS.md` へ投影し、provider非依存と判断したSkillsだけを明示listでCodexへ個別linkする。tool固有Skillを除外する発想と、Codex自身が所有する `.system` 等を残す点はcross-harness配布の参考になる。[AGENTS.md](https://github.com/freekmurze/dotfiles/blob/09219e07aab14ea34375e85fd9597b63ca88f239/config/claude/AGENTS.md) [settings.json](https://github.com/freekmurze/dotfiles/blob/09219e07aab14ea34375e85fd9597b63ca88f239/config/claude/settings.json) [cross-harness links](https://github.com/freekmurze/dotfiles/blob/09219e07aab14ea34375e85fd9597b63ca88f239/bin/link-agent-skills)

一方、23個のSkill directory、複数plugin、広いshell許可を含むため、今回の中心である「Skill提供ではない個人設定」からはboundaryに置いた。installerはClaude側のSkills/agents directoryを置換し、Codex側でも一部を除く既存Skill entryを削除する。[installer](https://github.com/freekmurze/dotfiles/blob/09219e07aab14ea34375e85fd9597b63ca88f239/bin/install-claude-code) 共有sourceと除外listは参考になるが、ownership manifest、preview、backupなしで置換する実装はコピーしない。

### 5.5 `yutkat/dotfiles`: 複数agentをnative pathへ宣言的に配る

Mise/Nix/Home Managerを併用し、Claude、Codex、OpenCodeをそれぞれのnative directoryへ配布する。Codex global rulesは比較的短く、local規則の優先を明記する。一つの万能fileへ全toolを押し込まず、packageとmutable dotfileを分ける参照例である。[README](https://github.com/yutkat/dotfiles/blob/119f1d647370f4e7b35ebb6b4165949e7c85a666/README.md) [Codex AGENTS.md](https://github.com/yutkat/dotfiles/blob/119f1d647370f4e7b35ebb6b4165949e7c85a666/.config/codex/AGENTS.md) [Mise config](https://github.com/yutkat/dotfiles/blob/119f1d647370f4e7b35ebb6b4165949e7c85a666/.config/mise/config.toml)

directory単位のlinkは簡潔だが、agent appが同じdirectoryへruntime stateを書き込む場合のownership確認が必要である。

### 5.6 `joshsymonds/nix-config`: Nixと厚い個人policy

個人のdesktop/server/macOSを管理するNix configurationで、Home Manager moduleからClaude CodeとCodexを導入し、global instructionsも配る。authority、反論、完了条件、変更範囲、言語・PR運用を詳細に定義する「厚いalways-on」の別例である。[README](https://github.com/joshsymonds/nix-config/blob/5203a334d8302478ad2a1efd9e8876265b200573/README.md) [Home Manager common](https://github.com/joshsymonds/nix-config/blob/5203a334d8302478ad2a1efd9e8876265b200573/home-manager/common.nix) [Claude global instructions](https://github.com/joshsymonds/nix-config/blob/5203a334d8302478ad2a1efd9e8876265b200573/home-manager/claude-code/CLAUDE.md)

章構成とauthority定義は参考になるが、個人の価値観と仕事環境が強く、会社baselineや他人のglobal promptとしてcopyするものではない。

### 5.7 `TechDufus/dotfiles`: managed manifestと衝突回避

Ansible roleをClaude、Codex、Cursor、OpenCodeごとに分け、source/destination、managed manifest、既存unmanaged設定の衝突検出、stale managed entryのcleanupを実装する。設定内容だけでなく「何を自分が所有し、何を消してよいか」を配布コードへ落とした例である。[README](https://github.com/TechDufus/dotfiles/blob/56a0085acb2bc4f835ce6f20aa7a1fe5a80ecc16/README.md) [Codex role](https://github.com/TechDufus/dotfiles/blob/56a0085acb2bc4f835ce6f20aa7a1fe5a80ecc16/roles/codex/tasks/main.yml) [Claude role](https://github.com/TechDufus/dotfiles/blob/56a0085acb2bc4f835ce6f20aa7a1fe5a80ecc16/roles/claude/tasks/main.yml)

個人dotfilesでもdistribution lifecycleを持てる好例だが、Ansible role全体は環境固有であり、ownership contractを借りるのがよい。

### 5.8 `joshukraine/dotfiles`: Claude Codeを日常UIまで統合

GNU Stowでglobal `CLAUDE.md`、`settings.json`、permission preset、Skills、keybinding、statuslineを管理する。default permissionsへproject/type別overlayを重ねる構成、tmux/Neovimとの併用、checkpointやautopilot等のlifecycle Skillsが特徴である。[README](https://github.com/joshukraine/dotfiles/blob/157799742c2d0f455feac89c485991a68a1e6605/README.md) [permission presets](https://github.com/joshukraine/dotfiles/tree/157799742c2d0f455feac89c485991a68a1e6605/claude/.claude/presets) [settings.json](https://github.com/joshukraine/dotfiles/blob/157799742c2d0f455feac89c485991a68a1e6605/claude/.claude/settings.json)

特に価値が高いのは、`~/.claude` 自体をsymlinkするとsession、project、history等のruntime stateがdotfiles側へ漏れる「directory folding」事故と、その修復をREADMEに記録している点である。これは管理対象を子path単位にする強い実例である。

### 5.9 `meain/dotfiles`: hooks中心の個人workflow

GNU StowでClaude packageを選択的にlinkし、Notification、Stop、UserPromptSubmit、PostToolUse hooksでtask summary、status、通知を更新する。promptだけでなく日々のfeedback loopをdotfiles化する例である。[README](https://github.com/meain/dotfiles/blob/1bb42a69a70569f01128b0adb573d6b17e68e06c/README.md) [Claude settings](https://github.com/meain/dotfiles/blob/1bb42a69a70569f01128b0adb573d6b17e68e06c/claude/.claude/settings.json) [CLAUDE.md](https://github.com/meain/dotfiles/blob/1bb42a69a70569f01128b0adb573d6b17e68e06c/claude/.claude/CLAUDE.md)

絶対path、勤務先文脈、dangerous mode等への結合も強い。hookのevent設計だけを読み、commandや権限は再設計すべきである。

### 5.10 `ryoppippi/dotfiles`: Nixとshared fragmentsの混成

Nix/Home Managerでagent binariesと設定を配り、Claudeはshared instruction fragmentsを参照し、Codexはtool-native global rulesを別に持つ。Claude/Codex/OpenCode/Cursorを一つの個人環境で共存させる実例である。[README](https://github.com/ryoppippi/dotfiles/blob/77450476da6882e94114decf44e79ba55735456e/README.md) [Codex AGENTS.md](https://github.com/ryoppippi/dotfiles/blob/77450476da6882e94114decf44e79ba55735456e/codex/AGENTS.md) [Claude CLAUDE.md](https://github.com/ryoppippi/dotfiles/blob/77450476da6882e94114decf44e79ba55735456e/claude/CLAUDE.md)

共通sourceから全projectionを生成する方式ではないため、shared fragmentsとtool固有入口のdrift監視が必要になる。

### 5.11 `jackfranklin/dotfiles`: MCPを非破壊に同期

Claude settingsとMCP sourceを分離し、live `~/.claude.json` と比較してmanaged serverを追加・更新する一方、未管理serverを自動削除しない。global `CLAUDE.md` は空で、always-onよりpermissions、MCP、Skills、Pi extensionsへ重心がある。[README](https://github.com/jackfranklin/dotfiles/blob/a16ec499e98d0fbae9f595285bba4988f1b6362a/README.md) [Claude settings](https://github.com/jackfranklin/dotfiles/blob/a16ec499e98d0fbae9f595285bba4988f1b6362a/claude/settings.json) [MCP source](https://github.com/jackfranklin/dotfiles/blob/a16ec499e98d0fbae9f595285bba4988f1b6362a/claude/mcp.json)

MCP ownershipを部分管理する参考になる。会社用にはsecret scope、server allowlist、OAuth owner、egress、削除・revocationまで追加する必要がある。

### 5.12 `wcygan/dotfiles`: portable templateとmachine-local stateを分ける

`config/agents/AGENTS.md` を共通正本として `~/.agents/AGENTS.md` と `~/.codex/AGENTS.md` へlinkする。Codex `config.toml` はportable templateとして「存在しない時だけcopy」し、project trustや生成stateはlocalに残す。reusable Skillsは別repoでpinし、dotfiles固有Skillsだけを本repoに置く。[README](https://github.com/wcygan/dotfiles/blob/e2c9eb73afa27b06594bb24912a07728eea6b936/README.md) [shared AGENTS.md](https://github.com/wcygan/dotfiles/blob/e2c9eb73afa27b06594bb24912a07728eea6b936/config/agents/AGENTS.md) [Codex template](https://github.com/wcygan/dotfiles/blob/e2c9eb73afa27b06594bb24912a07728eea6b936/config/codex/config.toml)

構造は参考になるが、公開template自体は `approval_policy = "never"` と `sandbox_mode = "danger-full-access"` を組み合わせる。これは個人の選択であり、安全なportable defaultではない。構造とpolicy値を分けて読む必要がある。

### 5.13 `liby/dotfiles`: routing、partial merge、security boundaryを一体化

chezmoiでClaude/Codex/Pi等を管理し、always-on entrypointには「どの狭いowner文書を次に読むか」というrouting mapを置く。Codex live TOMLはmanaged keyをdeep-overlayし、repoが完全所有するsubtreeだけをreplaceして、その他のtool-written値を保持する。credential storeはkeyring、workspace rootの`.env`系はread-onlyにし、Claude hookでpolicyとsecret scanを実行する。[repo snapshot](https://github.com/liby/dotfiles/tree/0346449a94932aa21fbce31df1f4875d9c53ffd4) [routing AGENTS.md](https://github.com/liby/dotfiles/blob/0346449a94932aa21fbce31df1f4875d9c53ffd4/AGENTS.md#L26-L50) [Codex partial merge](https://github.com/liby/dotfiles/blob/0346449a94932aa21fbce31df1f4875d9c53ffd4/dot_codex/modify_private_config.toml#L1-L21) [Claude settings](https://github.com/liby/dotfiles/blob/0346449a94932aa21fbce31df1f4875d9c53ffd4/.chezmoitemplates/claude/settings.json#L37-L95)

「最小の常時routing + 必要時の狭い文書 + deterministic policy + mutable merge」の合成例として参考度が高い。一方、Mac、YubiKey、envchain、独自CLIへの結合が強く、具体的なsecret/hook実装を他環境へそのまま移植するものではない。

### 5.14 `haacked/dotfiles`: cross-agent portabilityを「変換問題」として扱う

Claude/Codexでinstructions、Skills、subagents、MCP inventoryを共通化しつつ、platform installerでlayoutとmetadataを変換する。Markdown agentをCodex TOMLへrenderし、provider-neutral execution tierをClaude/Codex modelへmapする。Codexに意味的互換性がないSkillsは明示的な除外リストへ置く。[AI architecture](https://github.com/haacked/dotfiles/blob/895d4b06a963c72c7ed269a4aa40d97bf04463b2/ai/README.md) [model tiers](https://github.com/haacked/dotfiles/blob/895d4b06a963c72c7ed269a4aa40d97bf04463b2/ai/codex/model-tiers.conf) [Codex exclusions](https://github.com/haacked/dotfiles/blob/895d4b06a963c72c7ed269a4aa40d97bf04463b2/ai/codex/excluded-skills.txt)

installerはregular fileとunmanaged symlinkを保全し、所有するlink/generated fileだけをuninstallする。MCPは一つのinventoryから各platformのnative registrationへ投影する。[installer contract](https://github.com/haacked/dotfiles/blob/895d4b06a963c72c7ed269a4aa40d97bf04463b2/ai/README.md) [MCP inventory](https://github.com/haacked/dotfiles/blob/895d4b06a963c72c7ed269a4aa40d97bf04463b2/ai/mcp-servers.sh)

### 5.15 `Jxck/dotfiles`: 共通正本とpermission同期

`.agents/AGENTS.md` と `.agents/skills/` を正本にし、Claude/Codexのnative pathへsymlinkする。Claude `settings.json` とCodex rulesを同期するSkillも持つ。[README](https://github.com/Jxck/dotfiles/blob/a8f03bcf745e1f6f1c2fdfb8035c6c71fbecf684/README.md) [shared AGENTS.md](https://github.com/Jxck/dotfiles/blob/a8f03bcf745e1f6f1c2fdfb8035c6c71fbecf684/.agents/AGENTS.md) [permission-sync](https://github.com/Jxck/dotfiles/blob/a8f03bcf745e1f6f1c2fdfb8035c6c71fbecf684/.agents/skills/permission-sync/SKILL.md)

一方、公開されたCodex configにはuser固有絶対path、trusted projects、desktop state、marketplace cache pathが含まれる。[config.toml](https://github.com/Jxck/dotfiles/blob/a8f03bcf745e1f6f1c2fdfb8035c6c71fbecf684/.codex/config.toml) 機密とは限らないが、portable sourceとapp-written stateを分離しない場合のdrift/privacy/他machine適用リスクを示す反例になる。

### 5.16 `denolfe/dotfiles`: pluginと人間の操作環境まで含む

DotbotでClaudeのglobal instructions、Skills、commandsを配置し、local pluginとしてgit guardrail、turn review、status、sound、auto-approve等を管理する。Pi設定やtmux/keyboard統合も同じdotfilesに含む。[README](https://github.com/denolfe/dotfiles/blob/800fa363fa2c7d2474e6511f4d5feb6eb57bd04b/README.md) [Claude global instructions](https://github.com/denolfe/dotfiles/blob/800fa363fa2c7d2474e6511f4d5feb6eb57bd04b/claude/CLAUDE.md) [local plugins](https://github.com/denolfe/dotfiles/tree/800fa363fa2c7d2474e6511f4d5feb6eb57bd04b/claude/local-plugins)

agent configurationをpromptだけでなく、terminal/UI/feedback loopまで含めて考える参考になる。ただしauto-approveはguardrailの強さと失敗時影響を個別に監査し、plugin単位で採否を決めるべきである。

### 5.17 `himkt/config`: 細粒度permissionsとrules分割

miseによるmacOS/Linux bootstrapの一部としてClaude settings、rules、Skills、hook、Codex rulesを配置する。always-on文書をrule fragmentへ分け、allow/deny/askをcommand patternで詳細に管理する。[README](https://github.com/himkt/config/blob/13a3f925b6e565087179b58dc25e4e9f13a885e6/README.md) [Claude settings](https://github.com/himkt/config/blob/13a3f925b6e565087179b58dc25e4e9f13a885e6/claude/settings.json) [rules](https://github.com/himkt/config/tree/13a3f925b6e565087179b58dc25e4e9f13a885e6/claude/rules)

deny listだけでなくaskを別に置く構造は参考になるが、`Edit`、`Write`、`git add`、`git commit`等の広いallowも含む。command string matchingはshellの意味やtool更新で挙動が変わるため、OS sandboxや外部action gateの代替にしない。

### 5.18 `benswift/.dotfiles`: shared instructions、local config、private extension

Claude global instructionsをCodex `AGENTS.md`にもlinkし、Geminiには `CLAUDE.md` と `GEMINI.md` をcontext filenameとして読ませる。一方、Codex model、trusted projects、UI stateを含む `config.toml` はmachine-localに残す。個人的なSkillsはprivate plugin repoへ分離し、Claude pluginとCodex per-skill linkから利用する。[README](https://github.com/benswift/.dotfiles/blob/a3813d90a0efca489cc420eeb616ba83e7fe4f5d/README.md) [Claude settings](https://github.com/benswift/.dotfiles/blob/a3813d90a0efca489cc420eeb616ba83e7fe4f5d/claude/settings.json) [Gemini settings](https://github.com/benswift/.dotfiles/blob/a3813d90a0efca489cc420eeb616ba83e7fe4f5d/gemini/settings.json)

public portable coreとprivate personal knowledgeを分ける形が参考になる。pluginを別にしただけではsupply-chain安全性は自動的に得られないため、commit pin、更新確認、uninstallを別途持つ必要がある。

### 5.19 `ushironoko/dotfiles`: 低starsだが最もdistributionに近い個人実装

Claude/Codex/Piを同じ個人dotfiles managerで扱い、config、AGENTS、agents、hooks、rules、Skillsをdurable componentごとにselective child linkする。既存custom state、runtime database、plugin cacheをdirectoryごと置換しない。Codex native hookへClaude lifecycleを対応付け、unsupported eventは同じoutcomeになる別手段へ落とす。[Codex README](https://github.com/ushironoko/dotfiles/blob/f44da264736c4923eb1860101c1e0579132e607e/codex/README.md) [repo AGENTS.md](https://github.com/ushironoko/dotfiles/blob/f44da264736c4923eb1860101c1e0579132e607e/AGENTS.md) [mapping source](https://github.com/ushironoko/dotfiles/blob/f44da264736c4923eb1860101c1e0579132e607e/dotfiles.config.ts)

これは「dotfiles」と「小規模な個人distribution」の境界例である。ただし主目的はなお本人の環境管理で、general-purpose frameworkの配布ではないため対象に含めた。複雑さ、test/update費用、harness releaseへの追随負担が高く、最小構成のdefaultには向かない。

## 6. 横断パターン

| pattern | 良い実装形 | 参照候補 | 失敗しやすい形 |
| --- | --- | --- | --- |
| 共通方針 | provider-neutralな短い正本 + native root file | nicknisi、liby、wcygan | 全toolへ同じ巨大fileを無条件link |
| platform差分 | renderer、adapter、除外list、compatibility test | TechDufus、haacked、ushironoko | pathが同じなので意味も同じと仮定 |
| runtime state | 管理対象の子だけlink/copy | joshukraine、ushironoko、wcygan | `~/.claude` / `~/.codex` 全体をsymlink |
| permissions | portable policyとlocal approval/trustを分離 | nicknisi、liby、joshukraine | 他人のallow listやdanger modeをコピー |
| hooks | lifecycle event、timeout、fail-open/closed、trustを明記 | meain、ushironoko、benswift | hook scriptを置くだけでactiveとみなす |
| MCP | inventoryは共有、credential/env/registerはnative/local | jackfranklin、haacked | public configへtokenやuser pathを埋め込む |
| Skills | common core、provider-specific exclusion、private layer | haacked、benswift、wcygan | 全Skillを全harnessへsymlink |
| install/update | dry-run、conflict、backup、owner、uninstall、verify | TechDufus、haacked、ushironoko | `curl | sh`だけを運用契約にする |
| 人間UI | tmux/editor/status/notificationをagent loopと一緒に設計 | joshukraine、denolfe、benswift | prompt文だけを「運用」と考える |

実装を横断すると、(1) 最小always-on invariant + routing、(2) project-local instructions、(3) on-demand rules/docs/Skills、(4) deterministic permissions/hooks、(5) tool-owned runtime state、の5層に分けると整理しやすい。この5層は公開repoからの設計上の統合推論であり、効果順位を示すbenchmark結果ではない。

### 6.1 実質的な共通形は「source + projections」

複数harnessを使う場合、正本を各toolのhomeへ直接置くのではなく、repo内のportable sourceからtool-native surfaceへ投影する形が強い。投影にはsymlinkだけでなく、render、copy-if-missing、merge、exclusionが含まれる。設定formatの共通性と、権限・hook・subagent・modelの意味的互換性を分けて扱う必要がある。

### 6.2 always-on最小化はdotfilesでも必要

公開例には、global instructionへ言語別規約や長いworkflowを積むものと、短い共通原則 + rules/Skillsへ分けるものが混在する。人気度はこの設計判断の正しさを示さない。本編の結論と整合する初期形は、常時読むのをidentity、authority、safety、verification、routingに絞り、技術別・task別手順をon-demandへ出すことである。

### 6.3 mutable configは「dotfileである」と決めつけない

現在のagent appは `settings.json` や `config.toml` にproject trust、UI、marketplace、生成stateを書き足すことがある。従来のeditor configのように全面symlinkすると、repo汚染、別machineへの誤配布、app write failureが起きる。portable fragmentをcopy/mergeし、app-owned sectionをlocalに保つ方が堅い。

### 6.4 個人dotfilesは運用知識の一次資料だが、標準そのものではない

公開dotfilesは、authorが実際に使うためvendor docsにないfailure modeをよく記録する。directory folding、tool間のallowed-tools差、project trustの混入、hook trust、provider別model tier等は特に有用である。一方、個人の権限、private path、employer tool、language preference、auto-commit方針も混ざる。アイデアのsourceとして読み、policy値は再採択する。

## 7. コピーしてはいけないもの

- `approval_policy = "never"`、`danger-full-access`、bypass/auto-approveを、脅威モデルなしでコピーする。
- 他人のtrusted project、absolute path、MCP endpoint、marketplace、shell aliasを残す。
- `git commit`、`git push`、force-push、外部messageをglobal allowへ入れる。
- public repoのSkill/pluginをbranch tipや`@latest`のまま自動更新する。
- `~/.claude`、`~/.codex`、`~/.gemini`全体をrepoへlinkする。
- Claude用hook/permission/agent metadataをCodexへ機械的にcopyし、欠落したenforcementを見落とす。
- READMEのinstall one-linerを読まずに実行する。bootstrapはpackage、shell、OS defaults、private serviceまで変更し得る。
- starsをsecurity review、maintenance capacity、license compatibility、会社採用実績とみなす。

## 8. 自分用dotfilesへの推奨blueprint

```text
agent-config/
├── common/
│   ├── AGENTS.md                 # 短い不変条件・authority・verification
│   ├── skills/                   # provider-neutralなon-demand手順
│   └── policy/                   # 意図レベルの権限定義
├── claude/
│   ├── CLAUDE.md                 # common import + Claude固有差分
│   ├── settings.portable.json    # secret/local stateなし
│   └── hooks/
├── codex/
│   ├── AGENTS.md                 # common projection + Codex固有差分
│   ├── config.portable.toml
│   ├── rules/
│   └── hooks/
├── gemini/
│   └── settings.portable.json
├── private.example/              # schema/exampleのみ公開
├── manifests/                    # source、commit、license、compatibility
└── bin/
    ├── plan
    ├── install
    ├── doctor
    ├── diff
    └── uninstall
```

運用上は次をdefaultにする。

1. `plan`/dry-runでdestination、conflict、replace対象、permission差分を表示する。
2. destination root directoryは実directoryとして残し、管理対象の子だけを投影する。
3. portable source、generated projection、app-owned local stateをmanifestで区別する。
4. external Skill/plugin/MCPはsource commit、license、permission surface、update日、rollbackを記録する。
5. install後にfile kind、link target、schema、hook trust、agent discovery、secret absenceをdoctorで確認する。
6. updateは自動適用せず、source diffとregression checkを通す。
7. 一つのtask setで、always-on量、成功率、手戻り、人間時間、policy prompt、失敗復旧を測る。

## 9. 会社用へ持ち上げる時に追加するもの

個人dotfilesの構造をそのまま会社の全員へ配らない。以下はdotfiles一次資料だけからの事実ではなく、個人設定の欠落を[本編の会社用operating model](./2026-09-ai-agent-operations-research.md#5-会社用の推奨operating-model)と[Security / authority調査](./2026-09-ai-agent-operations-research.md#11-securityとauthority)に照らした規範的な合成提案である。

- platform/security/legal ownerと、team/user overrideのRACI。
- approved source catalog、license/provenance、version pin、段階rollout、expiry。
- secret broker、identity、least privilege、network egress、sandboxのprompt外強制。
- central policyとproject policyの優先順位、exception申請、期限、監査receipt。
- model/harness/versionごとのcompatibility matrixとregression eval。
- install/update/uninstall/rollbackのfleet管理と、app-owned local stateのmigration。
- telemetryのdata minimization、retention、access、redaction。
- outcome、trajectory、human time、failure/recovery costによる効果測定。

会社標準の配布単位は「社員のhome directoryを完全再現するdotfiles」ではなく、portable agent policy、approved components、native adapters、local enrollmentの組合せがよい。個人のshell/editor/UI統合はoptional layerへ残す。

## 10. このリポジトリへの示唆

本repoのようにNix/Home ManagerからClaude/Codex設定を配る場合も、宣言的管理の対象を増やすほど良いとは限らない。特にapp自身が更新するconfig、trust、session、cacheをimmutableなNix store linkへ置くと、UIからの更新やmachine-local trustと衝突し得る。

次の設計仮説が、この追加調査と本編の両方に整合する。

1. Home Managerはportable source、共通instructions、静的rules、管理対象hook/Skillの投影を担う。
2. app-written configはwritable local fileとして生成・mergeし、宣言側はportable keyだけを所有する。
3. common sourceからClaude/Codex native surfaceをpackage時に生成し、差分と除外をtestする。
4. always-onにはroute、authority、safety、verificationだけを残し、詳細手順は必要時に読むSkillへ置く。
5. AI-DLC等のlifecycle distributionは全taskのdotfile defaultにせず、複数session、監査、handoff、明示stageが必要なtaskで昇格する。判断基準は[本編のTask / risk decision matrix](./2026-09-ai-agent-operations-research.md#6-task--risk-decision-matrix)を使う。

これは採用・実装決定ではない。現行設定の変更、Skill移動、Nix module変更、framework導入は別工程で、実際のsource/generated/runtime境界を棚卸しした後に決める。

## 11. 追加候補と今後の追跡

高starsでも、AI-agent運用全体の参考ではなく特定surfaceだけを見る候補がある。

- [linkarzu/dotfiles-latest](https://github.com/linkarzu/dotfiles-latest): 高認知だがAI面はOpenCode external-directory permissions等の狭い例。
- [sapegin/dotfiles](https://github.com/sapegin/dotfiles): Cursor MCP構成の小さな参照例。
- [JDevlieghere/dotfiles](https://github.com/JDevlieghere/dotfiles): 簡潔さとcommit許可に絞った短いClaude global instructions。
- [freekmurze/dotfiles](https://github.com/freekmurze/dotfiles): 共通instructionsとtool別Skill除外は参考になるが、大規模Skill catalogと破壊的installerを含むboundary。詳細は5.4節。
- [citypaul/.dotfiles](https://github.com/citypaul/.dotfiles): 個人dotfilesではあるが、AI subtreeは大規模なSkill/agent catalogでもあり、今回の中心からは外した境界例。

starsは低いが、特定の設計を深掘りする際に参考になる候補もある。

- [gwenwindflower/dotfiles](https://github.com/gwenwindflower/dotfiles): chezmoi templateから複数agent rootを生成し、one-shot環境ではsymlinkをmaterializeする。
- [scottjrainey/dotfiles](https://github.com/scottjrainey/dotfiles): public Nix layerとoptional private sibling repoを分離する。
- [Daiki48/dotfiles](https://github.com/Daiki48/dotfiles): Codex live configをlocalに残し、shared settingのmigrationとbackupを行う。
- [thuringia/dotfiles](https://github.com/thuringia/dotfiles): shared Codex configとUI-owned local configをdiff/promoteする。
- [oysteinkrog/dotfiles](https://github.com/oysteinkrog/dotfiles): external Skill source、commit、content hash、local edit検知をlockに持つ。

これらは人気候補としてではなく、特定patternのarchitecture referencesとして追跡するのが妥当である。

`paulirish/dotfiles` は検出されたClaude設定が一Skill配下のproject-local file、`fatih/dotfiles` のroot `AGENTS.md` はrepo-local guidance、`steipete/dotfiles` は現在のAI-agent設定を確認できなかったため除外した。Skill catalog、awesome list、他人のdotfiles archiveも同じく除外した。

## 12. 停止線

本調査で行ったのは、公開repoとGitHub metadataの読み取り、候補分類、設計比較、報告書作成までである。dotfilesのcloneは一時調査directoryに限定した。user homeへのinstall、設定変更、Nix/Home Manager変更、Skill/plugin/MCP追加、commit、push、activation、live permission testは行っていない。
