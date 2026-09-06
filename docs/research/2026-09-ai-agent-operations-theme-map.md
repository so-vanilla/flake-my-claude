# AIエージェント運用のテーマ地図（2026年9月調査入口）

Status: theme selection only / research not completed / no adoption decision

基準日: 2026-09-01（Asia/Tokyo）

## 1. この文書の目的と停止線

個人用および会社用のAIエージェント運用を抜本的に定義する前に、広域調査を何の比較として行うかを固定する。この文書は結論、製品選定、導入計画ではない。一次情報から、独立して調べるべきテーマ、比較軸、情報源の入口を選定する。

対象はcoding agentだけに限定しない。ただし、検証可能なground truth、Git差分、test、review、releaseという観測点が揃い、方法論の議論も先行しているため、software developmentを主な実験場として扱う。そこで得た型をresearch、support、operations、finance等へ一般化する際は、各domain固有の権限、成功条件、data handlingで再検証する。

今回は次の地点で止める。

- 15の調査テーマと境界を定義する。
- 「小さなSkillを必要時だけ呼ぶ構成」と「SuperpowersやAWS AI-DLCのようなフレームワーク型ディストリビューション」の比較を中心課題に置く。
- 個人用と会社用で評価関数が変わる箇所を明示する。
- 主要プロバイダ／コミュニティが現在どの方向を公式に提示しているかを、本調査で検証する仮説として整理する。
- 実際の優劣、採用、設定変更、インストール、ベンチマーク実行には進まない。

「2026年9月現在」は未来を含む月全体ではなく、上記基準日までに公開・取得できた資料だけを指す。変化が速い領域なので、本調査時には各ページの更新日、リリース、既定値を再確認する。

一次情報であることと、現在の規範であることは同義ではない。過去の設計記事、現行仕様、製品説明、実験報告、自社事例はすべて一次情報になり得るが、証拠としての役割は異なる。特にAnthropicの2024年版 [Building effective agents](https://www.anthropic.com/engineering/building-effective-agents) には、当時説明したtooling landscapeが変化しており、現行approachは [Managed Agentsの設計記事](https://www.anthropic.com/engineering/managed-agents) と現行documentationを参照するよう注記がある。したがって、古い一次情報を「2026年の公式推奨」へ自動昇格させない。

## 2. 最初に分けるべき層

同じ「AIエージェントの仕組み」に見えても、次の層は代替関係ではない。比較時に混ぜない。

| 層 | 役割 | 例 | 比較で問うこと |
| --- | --- | --- | --- |
| Harness / product | モデル、会話、ツール実行、権限、sandboxを提供 | Codex、Claude Code、Gemini CLI、GitHub Copilot、Kiro | 実行能力、権限、環境、管理面 |
| Always-on instruction | 全セッションで必要な規約を与える | `AGENTS.md`、`CLAUDE.md`、steering | 本当に毎回必要か、適用範囲は正しいか |
| Skill | 必要時に手順・知識・scriptをロードする | Agent Skills、各製品のSkill | 発見・明示起動・コンテキスト費用・副作用 |
| Hook / policy | eventに対して決定的な処理・拒否・監査を行う | PreToolUse、Copilot hooks、managed policy | 指示か強制か、失敗時挙動、監査可能性 |
| Plugin / distribution | Skills、hooks、agents、connectors等を配布する | Codex/Claude plugins、Superpowers | 配布、versioning、更新、供給網 |
| Methodology / lifecycle | 要求から運用までの進め方を定義する | AI-DLC、Superpowers、BMAD、SDD | 工程所有、成果物、human gate、適応性 |
| Spec / artifact harness | 意図を永続成果物と段階に変換する | Spec Kit、OpenSpec、Kiro Specs | source of truth、変更・再開・検証 |
| Orchestration runtime | single/multi-agentの実行グラフを構成する | OpenAI Agents SDK、Google ADK、Microsoft Agent Framework | 決定的経路とLLM判断、state、trace |
| Protocol / standard | tool・agent・指示の相互運用面を定義する | MCP、A2A、Agent Skills、AGENTS.md | portableなのは形式か意味論か |
| AgentOps / governance | 評価、観測、identity、policy、費用、監査 | evals、traces、AI controls | 本番で説明・制御・改善できるか |

## 3. 全テーマ共通の比較軸

本調査では、各候補を機能一覧ではなく同じ評価表で比較する。

1. **対象範囲**: 一作業、変更単位、リポジトリ、製品、SDLC全体のどこまでを所有するか。
2. **起動とrouting**: 明示起動、自然言語でのbest-effort選択、event-driven、自動常駐のどれか。
3. **コンテキスト負荷**: 常時ロード量、Skill metadata、遅延ロード、tool schema、履歴成長、compaction。
4. **強制力**: modelへの指示、workflow上のgate、hook、sandbox、IAM/policyのどこで保証するか。
5. **適応性**: 小変更／brownfield／greenfield／高リスク案件で工程の幅と深さを変えられるか。
6. **成果物と状態**: spec、plan、task、decision、test、audit、checkpoint、resume tokenをどう残すか。
7. **検証可能性**: acceptance criteria、tests、independent review、trajectory eval、再現可能なevidence。
8. **人間の役割**: 質問、承認、例外処理、最終責任、複数stakeholder合意をどこに置くか。
9. **実行分離**: subagent、worktree、container、cloud sandbox、権限境界、writer ownership。
10. **可観測性**: tool call、agent step、費用、失敗、approval、変更理由を追跡できるか。
11. **security**: least privilege、prompt injection、secret、network、identity、non-repudiation、supply chain。
12. **移植性**: harness、model、OS、IDE、cloud、protocolを替えても何が残るか。
13. **変更容易性**: fork/customize可能性、upstream追従、migration、breaking change、lock/pin。
14. **経済性**: token、model call、並列度、待ち時間、人間レビュー時間、失敗・手戻り費用。
15. **運用適合**: 個人の速度・理解・手触りと、会社の標準化・統制・監査・教育のどちらに効くか。
16. **成果**: task successだけでなく、品質、lead time、変更失敗率、学習、保守性にどう影響するか。
17. **時点とsupersession**: 公開日、最終更新日、取得日、対象version、後継文書、deprecated/maintenance注記を記録し、どのsourceが現在の位置づけを上書きするか。
18. **証拠種別**: normativeな仕様・policy、現行product documentation、設計解説、empiricalな実験／benchmark、正式なresearch paper、self-reported case study、marketing claimを分け、同じ重みで扱わない。

## 4. 調査テーマ

### T01. 「agent」「workflow」「automation」の定義と自律度

- **なぜ独立テーマか**: 用語の定義が違うまま比較すると、決定的workflowと自律agent、coding productとagent applicationを同列に扱ってしまう。すべてのテーマの母集団を決める。
- **代表的な問い**: 何をもってagentと呼ぶか。手順を人間が決めるworkflowと、モデルが経路を決めるagentの境界はどこか。自律度をtaskごとに変えるべきか。
- **比較軸**: 制御主体、step決定方法、停止条件、時間軸、権限、失敗回復、人間介入点。
- **追うべき一次情報**: 歴史的な分類としての [Anthropic: Building effective agents（2024、現行approachへのsupersession注記あり）](https://www.anthropic.com/engineering/building-effective-agents)、現行設計としての [Anthropic: Managed Agents（2026）](https://www.anthropic.com/engineering/managed-agents)、[Google ADK: Agents](https://github.com/google/adk-docs/blob/main/docs/agents/index.md)、[OpenAI Agents SDK: orchestration](https://openai.github.io/openai-agents-python/multi_agent/)、[Microsoft Agent Framework](https://github.com/microsoft/agent-framework)。
- **重複／境界**: T06はagent数と委譲、T10は許される自律度、T05はloop内部の問題解決patternを扱う。

### T02. コンテキスト設計: 最小常駐、progressive disclosure、just-in-time取得

- **なぜ独立テーマか**: 常時指示の量はprocessの厳密さとは別問題であり、attention、token、古い規約の干渉、発見精度を直接左右する。
- **代表的な問い**: `AGENTS.md` / `CLAUDE.md`に残すべき最小情報は何か。Skill descriptionだけを常駐させるか、完全に手動起動にするか。referenceやtool schemaはいつ読むか。
- **比較軸**: startup token、各turnへの反復、signal/noise、path-scoped rules、明示起動、retrieval精度、compaction後の保持、cache効率。
- **追うべき一次情報**: [Anthropic: Extend Claude Code](https://code.claude.com/docs/en/features-overview)、[Anthropic: Effective context engineering](https://www.anthropic.com/engineering/effective-context-engineering-for-ai-agents)、[Agent Skills specification: progressive disclosure](https://agentskills.io/specification#progressive-disclosure)、[OpenAI: Build skills](https://learn.chatgpt.com/docs/build-skills)、[OpenAI: AGENTS.md](https://learn.chatgpt.com/docs/agent-configuration/agents-md)、[OpenAI: current model guidance](https://developers.openai.com/api/docs/guides/latest-model)。
- **重複／境界**: T03はworkflowがどれだけ広く工程を所有するか。T07は複数turn/sessionに残すstate。ここでは一回の推論へ何を入れるかを扱う。

### T03. 小さなSkillの合成 vs フレームワーク型ディストリビューション

- **なぜ独立テーマか**: 今回の中心課題。粒度の小さい明示Skill群と、初期instructionから工程・成果物・gateまで所有する方法論では、単なるtoken差以上に、判断権、再現性、学習負荷、更新単位が異なる。
- **代表的な問い**: `diagnose`、`plan`、`tdd`、`review`等を必要時だけ組み合わせる方がよいか。SuperpowersやAI-DLCが持つ一貫したphase ownershipはどのtaskで費用を上回るか。両者を混ぜると二重orchestratorにならないか。
- **比較軸**: 常時ロード量、routingの確実性、workflow coverage、phase/gate、適応幅、artifact/state、重複指示、escape hatch、教育効果、導入・更新費用。
- **追うべき一次情報**: [Superpowers](https://github.com/obra/superpowers)、[AWS AI-DLC workflows](https://github.com/awslabs/aidlc-workflows)、[AWS: adaptive AI-DLC workflows](https://aws.amazon.com/blogs/devops/open-sourcing-adaptive-workflows-for-ai-driven-development-life-cycle-ai-dlc/)、[Anthropic: extension choice and loading](https://code.claude.com/docs/en/features-overview)、[OpenAI: Plugins](https://learn.chatgpt.com/docs/plugins)。
- **重複／境界**: T02はloading、T04はspec/artifact、T14は配布・更新。T03は「誰が一つのtaskのlifecycleを所有するか」に限定する。

### T04. Intent / Spec / Artifact-driven development

- **なぜ独立テーマか**: chat履歴だけで進める方法と、要求・design・task・changeをversioned artifactにする方法では、レビュー、再開、並行作業、監査の単位が変わる。
- **代表的な問い**: specは実装前の固定契約か、実装と往復するliving artifactか。greenfieldとbrownfieldで必要なartifactは違うか。specの過剰生成をどう避けるか。
- **比較軸**: artifact graph、source of truth、差分、traceability、constitution/standards、brownfield対応、archive、変更同期、human reviewability。
- **追うべき一次情報**: [GitHub Spec Kit](https://github.github.com/spec-kit/)、[OpenSpec](https://github.com/Fission-AI/OpenSpec)、[Kiro Specs and harness overview](https://kiro.dev/docs/)、[BMAD getting started](https://docs.bmad-method.org/start/build-your-first-change/)、[AWS AI-DLC methodology](https://aws.amazon.com/blogs/devops/ai-driven-development-life-cycle/)。
- **重複／境界**: T07はruntime/checkpoint、T11はGit/CIへの接続。ここでは意図を何の成果物へ変換するかを扱う。

### T05. 言説と問題解決methodの系譜: promptからcontext・harness engineeringへ

- **なぜ独立テーマか**: 「今の正解」を判断するには、2022年以降のprompt/reasoning、tool-use loop、autonomous agent、reflection、RAG/memory、multi-agent、workflow、context engineering、spec-driven development、harness engineering、managed agentという言説が、何を解決し、何が後継に置き換えられたかを追う必要がある。framework名が違っても内部では同じpatternを再利用するため、製品比較から切り離して検証する。
- **代表的な問い**: plan-firstと直接実行の切替条件は何か。self-reflectionは独立検証の代わりになるか。長時間の自律loopから、外部state・eval・human gateを持つharnessへ重点が移ったのか。context engineeringやSkillのprogressive disclosureはprompt engineeringをどう拡張したか。model能力向上で不要になったprompt儀式と、依然必要な運用controlは何か。
- **比較軸**: 公開時点、問題設定、loop構造、外部feedback、ground truth、stateの所在、反復上限、error recovery、hidden reasoning依存、追加call費用、production evidence、後継文書。
- **追うべき一次情報**: [ReAct](https://arxiv.org/abs/2210.03629)、[Self-Refine](https://papers.nips.cc/paper/2023/hash/91edff07232fb1b55a505a9e9f6c0ff3-Abstract-Conference.html)、[Reflexion](https://arxiv.org/abs/2303.11366)、歴史的な自律agent実装としての [AutoGPT](https://github.com/Significant-Gravitas/AutoGPT)、歴史資料としての [Anthropic: Building effective agents（2024）](https://www.anthropic.com/engineering/building-effective-agents)、[Anthropic: Context engineering](https://www.anthropic.com/engineering/effective-context-engineering-for-ai-agents)、現行設計との変化を見る [Anthropic: Managed Agents（2026）](https://www.anthropic.com/engineering/managed-agents)、[OpenAI: Codex as an open agent harness](https://developers.openai.com/blog/codex-as-a-platform)、[12-Factor Agents](https://github.com/humanlayer/12-factor-agents)、[Claude Code best practices](https://code.claude.com/docs/en/best-practices)。
- **重複／境界**: T09は測定と独立証拠、T02は一回の推論へ入れるcontext、T03はpatternを束ねるdistributionを扱う。T05は言説の年代順カタログではなく、「主張された問題・解法・証拠・supersession」の変遷を調べる。歴史的言説を「現在も有効」と仮定しない。

### T06. Single-agent、subagent、multi-agent、agent-as-tool

- **なぜ独立テーマか**: 複数agentは方法論そのものではなく、context isolation、parallelism、専門性、相互批評を実装するtopologyである。常に品質向上するとは限らない。
- **代表的な問い**: 一つの強いagentに任せる境界はどこか。並列化可能性を誰が判定するか。agent間通信とroot統合の費用は何か。reviewerの独立性をどう保つか。
- **比較軸**: delegation topology、context分離、共有state、通信方向、writer ownership、並列度、競合、nested delegation、model routing、synthesis品質。
- **追うべき一次情報**: [OpenAI: Subagents](https://learn.chatgpt.com/docs/agent-configuration/subagents)、[Claude Code: subagents and agent teams](https://code.claude.com/docs/en/agents)、[Google ADK workflows](https://github.com/google/adk-docs/blob/main/docs/workflows/index.md)、[Microsoft Agent Framework](https://github.com/microsoft/agent-framework)、[Anthropic multi-agent research system](https://www.anthropic.com/engineering/built-multi-agent-research-system)。
- **重複／境界**: T01は定義、T08は遠隔agent間protocol、T11はworktree/file競合。ここでは分業構造を扱う。

### T07. 長時間作業、memory、durable state、resumeとhandoff

- **なぜ独立テーマか**: context windowを長くすることと、作業状態を正しく永続化することは別である。会社運用では担当交代・監査・障害復旧、個人運用では中断再開に直結する。
- **代表的な問い**: chat、summary、ledger、artifact、event log、task graphのどれをsource of truthにするか。compaction後に何を再読するか。stale memoryや自己生成memoryをどう検証するか。
- **比較軸**: state schema、durability、resume determinism、checkpoint頻度、ownership、staleness、provenance、human edit、cross-session/cross-agent handoff。
- **追うべき一次情報**: [Anthropic: Harness design for long-running application development](https://www.anthropic.com/engineering/harness-design-long-running-apps)、[Anthropic: Effective context engineering](https://www.anthropic.com/engineering/effective-context-engineering-for-ai-agents)、[OpenAI API: conversation state and compaction](https://developers.openai.com/api/docs/guides/conversation-state)、[AWS AI-DLC generated artifacts](https://github.com/awslabs/aidlc-workflows)、[MCP 2026-07-28 specification](https://modelcontextprotocol.io/specification/2026-07-28)。
- **重複／境界**: T04は設計成果物、T09はtrace、T11はGit状態。memoryを無条件に真とみなさない。

### T08. 指示・Skill・tool・agentの相互運用標準

- **なぜ独立テーマか**: vendor-neutralなfile/protocolが増えているが、形式互換、発見互換、意味論互換、権限互換は同じではない。lock-in評価の基礎になる。
- **代表的な問い**: Agent SkillsとAGENTS.mdはどこまでportableか。MCPはtool/data接続、A2Aはagent間協調をどう分担するか。protocol対応がsecurityや運用品質を保証するか。
- **比較軸**: scope、version negotiation、discovery、capability declaration、auth、consent、task lifecycle、error model、extension、conformance test、vendor extension。
- **追うべき一次情報**: [Agent Skills specification](https://agentskills.io/specification)、[AGENTS.md](https://agents.md/)、[MCP specification 2026-07-28](https://modelcontextprotocol.io/specification/2026-07-28)、[A2A Protocol v1.0](https://a2a-protocol.org/v1.0.0/)、[NIST AI Agent Standards Initiative](https://www.nist.gov/artificial-intelligence/ai-agent-standards-initiative)。
- **重複／境界**: T14はpackage/update supply chain、T10はidentity/authorization。protocolをmethodologyと呼ばない。

### T09. Verification、eval、observability、evidence

- **なぜ独立テーマか**: 「agentが完了と言った」と、受入条件を独立証拠で満たしたことは異なる。model、prompt、tool、workflowの変更を継続的に比較する土台になる。
- **代表的な問い**: 最終出力とtrajectoryのどちらを評価するか。task-specific evalをどう作るか。nondeterminismを何回測るか。production traceを評価datasetへ戻すか。
- **比較軸**: outcome/trajectory、deterministic checks、LLM judge、human review、baseline、variance、failure taxonomy、trace schema、privacy、regression gate。
- **追うべき一次情報**: [OpenAI: Evaluate agent workflows](https://developers.openai.com/api/docs/guides/agent-evals)、[Google Vertex AI: evaluate agents](https://docs.cloud.google.com/vertex-ai/generative-ai/docs/agent-engine/evaluate)、[Anthropic engineering: agent evals](https://www.anthropic.com/engineering/demystifying-evals-for-ai-agents)、[NIST AI 800-2 draft](https://nvlpubs.nist.gov/nistpubs/ai/NIST.AI.800-2.ipd.pdf)、[OpenTelemetry GenAI agent semantic conventions](https://github.com/open-telemetry/semantic-conventions-genai/blob/main/docs/gen-ai/gen-ai-agent-spans.md)。
- **重複／境界**: T05のself-evaluationは一手法にすぎない。T15は組織成果、T11はCI gateを扱う。

### T10. Security、権限、identity、sandbox、human gate

- **なぜ独立テーマか**: prompt上の禁止と、toolを到達不能にする制御は保証が異なる。agentの価値はaction能力と結びつくため、blast radius設計を後付けにできない。
- **代表的な問い**: read/write/network/secretをtaskごとにどう絞るか。高影響actionの承認はいつ必要か。agent identityと人間の委任をどう記録するか。prompt injectionやmemory/tool poisoningをどう扱うか。
- **比較軸**: least privilege、sandbox boundary、deny/allow precedence、approval UX、hook enforcement、credential scope、egress、audit/non-repudiation、fail-open/closed、incident recovery。
- **追うべき一次情報**: [OpenAI: Agent approvals & security](https://learn.chatgpt.com/docs/agent-approvals-security)、[Claude Code permissions](https://code.claude.com/docs/en/permissions)、[GitHub Copilot hooks](https://docs.github.com/en/copilot/concepts/agents/hooks)、[MCP security principles](https://modelcontextprotocol.io/specification/2026-07-28#security-and-trust--safety)、[NIST agent identity and authorization](https://www.nist.gov/news-events/news/2026/02/new-concept-paper-identity-and-authority-software-agents)、[OWASP Agentic Top 10](https://genai.owasp.org/2025/12/09/owasp-top-10-for-agentic-applications-the-benchmark-for-agentic-security-in-the-age-of-autonomous-ai/)。
- **重複／境界**: T12は組織policyの所有、T08はprotocol、T14はsupply chain。HITLは「毎回確認」ではなくrisk-based gateとして調べる。

### T11. Git、worktree、CI、review、releaseへの統合

- **なぜ独立テーマか**: agent内部のworkflowがよくても、実際のdelivery flowで差分衝突、test抜け、レビュー不能、無断publishを起こせば運用として失敗する。
- **代表的な問い**: branch/worktreeをagentごとに分けるか。commit/push/PR/deployのauthority gateをどう分けるか。CI failureから誰が再開するか。生成物とsourceをどう扱うか。
- **比較軸**: isolation、diff size、commit ownership、merge order、CI integration、review independence、rollback、deployment gate、artifact retention、live validation。
- **追うべき一次情報**: [OpenAI Codex Git worktrees](https://learn.chatgpt.com/docs/environments/git-worktrees)、[Claude Code best practices](https://code.claude.com/docs/en/best-practices)、[GitHub Copilot cloud agent](https://docs.github.com/en/copilot/concepts/agents/coding-agent/about-coding-agent)、[Superpowers](https://github.com/obra/superpowers)、[AWS AI-DLC workflows](https://github.com/awslabs/aidlc-workflows)。
- **重複／境界**: T03はlifecycle ownership、T09は品質evidence、T10は外部変更権限。repository changeとactivation/publishを同じ完了にしない。

### T12. 個人用と会社用の運用モデル

- **なぜ独立テーマか**: 個人では速度、可塑性、理解、手元の回復容易性が中心だが、会社では共有source of truth、権限分離、監査、例外、教育、調達、data handlingが追加される。同じ設定の規模拡大ではない。
- **代表的な問い**: 何をuser-local、project-shared、organization-managedに置くか。中央標準とteam自治の境界はどこか。誰がSkill、plugin、MCP、model、policyのownerか。例外をどう期限付きで認めるか。
- **比較軸**: scope/precedence、RBAC、managed settings、approved catalog、audit、data residency/retention、onboarding、support、exception process、bus factor、cross-team portability。
- **追うべき一次情報**: [OpenAI: Admin rollout guide](https://learn.chatgpt.com/docs/enterprise/admin-setup)、[OpenAI: Governance](https://learn.chatgpt.com/docs/enterprise/governance)、[Claude Code: organization setup](https://code.claude.com/docs/en/admin-setup)、[GitHub: Agent management for enterprises](https://docs.github.com/en/copilot/concepts/agents/enterprise-management)、[DORA 2025 report](https://dora.dev/research/2025/dora-report/)、[AWS AI-DLC methodology](https://aws.amazon.com/blogs/devops/ai-driven-development-life-cycle/)。
- **重複／境界**: T10はcontrolの技術、T14は配布、T15は成果測定。個人設定を会社標準へ無検証で昇格しない。

### T13. Model / effort routing、token・latency・人間時間の経済性

- **なぜ独立テーマか**: workflowの費用は入力tokenだけではない。高性能root、安価なworker、並列call、長いartifact、人間の質問待ち、失敗時の手戻りを合わせて評価する必要がある。
- **代表的な問い**: task complexityでmodel/effortをどう変えるか。subagent化は壁時計を短縮しても総費用を増やすか。常時prompt削減とprompt cacheはどう影響するか。安価な検証agentは信頼できるか。
- **比較軸**: input/output/cache token、call数、parallelism、latency、rate limit、human wait/review、failure/retry、quality-adjusted cost、provider portability。
- **追うべき一次情報**: [OpenAI: current model guidance](https://developers.openai.com/api/docs/guides/latest-model)、[OpenAI: cost optimization](https://developers.openai.com/api/docs/guides/cost-optimization)、[Claude Code: model configuration](https://code.claude.com/docs/en/model-config)、[Google ADK model support](https://github.com/google/adk-docs/blob/main/docs/agents/models/index.md)。
- **重複／境界**: T02はcontext設計、T06はdelegation、T15はbusiness outcome。最安model選定だけのテーマにしない。

### T14. 配布、移植、versioning、更新、supply chain

- **なぜ独立テーマか**: 方法論の内容が良くても、installerが既存設定を上書きする、upstreamを追えない、複数harnessで意味がずれる、依存pluginが広い権限を持つなら運用できない。
- **代表的な問い**: sourceとgenerated distributionをどう分けるか。pin、署名、release、migration、rollbackをどう行うか。project-localとuser/global installをどう使い分けるか。fork差分をどう監査するか。
- **比較軸**: install scope、manifest、lock/pin、provenance、license、generated artifacts、upgrade semantics、compatibility matrix、uninstall、conflict detection、offline/air-gap。
- **追うべき一次情報**: [OpenAI: Plugins](https://learn.chatgpt.com/docs/plugins)、[Claude Code: Plugins](https://code.claude.com/docs/en/plugins)、[GitHub Copilot customization cheat sheet](https://docs.github.com/en/copilot/reference/customization-cheat-sheet)、[Agent Skills specification](https://agentskills.io/specification)、[Spec Kit](https://github.github.com/spec-kit/)、各OSSのrelease/changelog。
- **重複／境界**: T08はopen format/protocol、T03はworkflow粒度、T10は悪意ある供給物への防御。導入済みとliveで有効を分ける。

### T15. 導入効果、開発者能力、組織変革の測定

- **なぜ独立テーマか**: benchmarkや生成速度が上がっても、品質、保守、team学習、delivery安定性が悪化する可能性がある。会社用の「正解」は局所task successだけでは決められない。
- **代表的な問い**: 何をbaselineにするか。AIは強い組織能力を増幅するのか、弱点を補うのか。レビュー負担、理解度、skill atrophy、満足度、lead timeをどう測るか。個人の主観的価値をどう扱うか。
- **比較軸**: task success、quality/defect、lead time、deployment frequency、change failure、review time、cognitive load、learning、maintainability、ROI、adoption/retention。
- **追うべき一次情報**: [DORA 2025: State of AI-assisted Software Development](https://dora.dev/research/2025/dora-report/)、[NIST: evaluation guidelines](https://www.nist.gov/caisi/guidelines)、各プロバイダのagent eval資料、導入対象組織自身のrepository/CI/incident/feedback data。
- **重複／境界**: T09はagent単体・workflowのeval、T12は運用制度。vendorの事例紹介を一般化せず、社内baselineと反証可能な指標を作る。

## 5. 2026-09-01時点の「現在の型」として検証する仮説

以下は採用結論ではない。各公式資料が現在強調している方向を、本調査で実験・反証するための仮説に変換したもの。

| 提供者／コミュニティ | 公式surfaceから読み取れる検証仮説 | 主に対応するテーマ | 一次情報 |
| --- | --- | --- | --- |
| OpenAI / Codex | reusableなagent loop/harnessと、業務applicationが所有するcontext・business rule・tool・approvalを分離する。その上でtask-specific Skillをprogressive disclosureし、subagent、eval、sandbox/approvalを必要に応じて組み合わせる方向が基準候補 | T02, T06, T09, T10, T13 | [Codex as a platform](https://developers.openai.com/blog/codex-as-a-platform)、[Skills](https://learn.chatgpt.com/docs/build-skills)、[AGENTS.md](https://learn.chatgpt.com/docs/agent-configuration/agents-md)、[Subagents](https://learn.chatgpt.com/docs/agent-configuration/subagents)、[Agent evals](https://developers.openai.com/api/docs/guides/agent-evals) |
| Anthropic / Claude Code | 現行Claude Codeではalways-on context、on-demand Skill、isolated subagent、deterministic hookを役割分担する。一方、2024年の「simple composable patterns」は歴史的baselineとしてのみ扱い、2026年のManaged Agentsが示すdurable session・交換可能なharness・sandbox分離との継承／変更を検証する | T01, T02, T03, T06, T07, T10 | [Extend Claude Code](https://code.claude.com/docs/en/features-overview)、[Building effective agents（2024、supersession注記）](https://www.anthropic.com/engineering/building-effective-agents)、[Managed Agents（2026）](https://www.anthropic.com/engineering/managed-agents)、[Context engineering](https://www.anthropic.com/engineering/effective-context-engineering-for-ai-agents) |
| Google | deterministicなSequential/Parallel/LoopとLLM-driven routingを同じADKで使い分け、eval・deployまで含める。遠隔agent相互運用はA2Aで分離する方向が基準候補 | T01, T06, T08, T09 | [ADK](https://github.com/google/adk-docs)、[ADK workflows](https://github.com/google/adk-docs/blob/main/docs/workflows/index.md)、[A2A](https://a2a-protocol.org/v1.0.0/) |
| GitHub / Copilot | custom instructions、custom agents、Skills、hooks、MCPを別のcustomization primitiveとして階層化し、会社ではenterprise controlsとauditを重ねる方向が基準候補 | T02, T06, T10, T12, T14 | [Customization cheat sheet](https://docs.github.com/en/copilot/reference/customization-cheat-sheet)、[Enterprise agent management](https://docs.github.com/en/copilot/concepts/agents/enterprise-management) |
| Microsoft | 新規のproduction-grade agent applicationはMicrosoft Agent Frameworkのtyped/graph workflowを中心にし、AutoGenはmaintenance/migration対象として扱う方向が基準候補 | T01, T06, T09, T14 | [Microsoft Agent Framework](https://github.com/microsoft/agent-framework)、[AutoGen](https://github.com/microsoft/autogen) |
| AWS / AI-DLC | AI主導・人間承認・repository artifactを持つend-to-end methodologyを、案件のpathway/complexityに応じて幅と深さを変えるadaptive workflowとして使う方向が基準候補 | T03, T04, T07, T10, T12 | [AI-DLC methodology](https://aws.amazon.com/blogs/devops/ai-driven-development-life-cycle/)、[Adaptive workflows](https://aws.amazon.com/blogs/devops/open-sourcing-adaptive-workflows-for-ai-driven-development-life-cycle-ai-dlc/)、[repository](https://github.com/awslabs/aidlc-workflows) |
| Superpowers | 小さなSkillsから構成されるが、初期instructionがbrainstorming、plan、TDD、review等の一貫したmethodology利用を要求する「composableだがlifecycle-owning」型として検証する | T03, T05, T11 | [obra/superpowers](https://github.com/obra/superpowers) |
| Spec Kit | intentをSpec → Plan → Tasks → Implementへ変換するartifact-firstなprocess harnessとして、step実行とautomation、organization catalogを検証する | T04, T12, T14 | [GitHub Spec Kit](https://github.github.com/spec-kit/) |
| OpenSpec | brownfieldを含むfluid/iterativeなartifact-guided change workflowとして、重いSDDとの差を検証する | T04, T07, T14 | [Fission-AI/OpenSpec](https://github.com/Fission-AI/OpenSpec) |
| BMAD | complexity別track、specialized agent、phase/workflow、fresh contextを使うfull-SDLC型として、成果物量とguided operationの価値を検証する | T03, T04, T06, T12 | [BMAD Method docs](https://docs.bmad-method.org/)、[Getting started](https://docs.bmad-method.org/start/build-your-first-change/) |
| 標準化コミュニティ | Agent Skills / AGENTS.md / MCP / A2Aがinstruction、tool/context、agent間通信を別々に標準化しつつあるため、形式互換と実運用互換を分けて検証する | T08, T10, T14 | [Agent Skills](https://agentskills.io/specification)、[AGENTS.md](https://agents.md/)、[MCP](https://modelcontextprotocol.io/specification/2026-07-28)、[A2A](https://a2a-protocol.org/v1.0.0/)、[NIST initiative](https://www.nist.gov/artificial-intelligence/ai-agent-standards-initiative) |

## 6. 個人用と会社用で変える比較の重み

| 観点 | 個人用で重くする | 会社用で重くする |
| --- | --- | --- |
| 標準化 | 自分が理解・修正できる最小規約 | team間の共通入口、owner、例外process |
| Context | 低token、手動起動、手触り | repository標準、managed policy、確実なdiscovery |
| 自律度 | すぐcourse-correctできる対話性 | risk tier、authority、separation of duties |
| 成果物 | 中断再開に十分な最小記録 | traceability、監査、handoff、retention |
| Security | local secret・誤操作・復旧 | IAM、data handling、egress、audit、incident response |
| 配布 | local Skillを素早く変更 | approved catalog、pin、署名、rollout/rollback |
| 評価 | 自分の代表task、満足度、時間 | team baseline、品質、delivery、variance、ROI |
| Framework | ceremonyが価値を上回る案件だけ | 複数人合意、再現性、教育、規制が必要な案件 |

会社用でも一律に重いframeworkを使うとは限らず、個人用でも高リスクな外部操作には強制controlが必要である。違いは機能の有無ではなく、default、owner、証拠、例外処理の重みとして検証する。

## 7. 本調査の推奨順序

### Wave 1: 中心的な設計選択

1. T01で用語と自律度を固定する。
2. T02とT03を同時に比較し、context費用とlifecycle ownershipを分離する。
3. T12で個人・会社の評価関数とauthority boundaryを定義する。
4. T15で「良い運用」を測るoutcomeを先に決める。

### Wave 2: workflowの信頼性

1. T04、T05、T07でspec、問題解決loop、durable stateを比較する。
2. T09、T10、T11でverification、security、delivery gateを設計する。
3. 小bug、brownfieldの横断feature、greenfield、高リスク変更の4 task archetypeで比較条件を揃える。

### Wave 3: 規模化と持続性

1. T06、T08でmulti-agentとprotocolの必要条件を調べる。
2. T13、T14で費用、配布、更新、移植を比較する。
3. provider/OSSごとの結論ではなく、task archetype × 個人/会社 × risk tierのdecision matrixへ統合する。

## 8. 本調査で最低限集める証拠

各candidateについて、READMEの思想だけでなく次を取得する。

- 基準日時点のofficial docs、repository commit、release/tag、license。
- 各sourceの公開日、最終更新日、取得日、対象version、後継／deprecated／maintenance注記と、主張が現在も有効かの判定。
- 各主張の証拠種別（normative specification/policy、current product docs、design narrative、empirical experiment、research paper、case study、marketing）と、その種別に見合う重み。
- install後に常時ロードされるfile/metadata/tool schemaと、必要時だけロードされる内容。
- phase、state、artifact、approval、resume、uninstall/upgradeの実装証拠。
- 同一task、同一repository snapshot、同一model/effortでの複数回run。
- task success、test/review evidence、token/call/latency、人間介入時間、失敗・復旧経路。
- security上のreachable tools、permission、network、secret、log、identity。
- 個人での操作性と、会社での配布・管理・監査を別々に評価した結果。

採用候補のREADMEに書かれた自己評価は「設計意図」、provider事例は「事例」、provider自身の実験は「条件付きのempirical evidence」として扱い、比較結果そのものにはしない。古い文書に明示的な後継参照がある場合は、古い文書を歴史・系譜の証拠として残しつつ、現行方針の根拠には後継文書を使う。

## 9. 一次情報インデックス

### Providers

- OpenAI: [Codex Skills](https://learn.chatgpt.com/docs/build-skills) / [AGENTS.md](https://learn.chatgpt.com/docs/agent-configuration/agents-md) / [Subagents](https://learn.chatgpt.com/docs/agent-configuration/subagents) / [Plugins](https://learn.chatgpt.com/docs/plugins) / [Agent evals](https://developers.openai.com/api/docs/guides/agent-evals) / [Security](https://learn.chatgpt.com/docs/agent-approvals-security) / [Enterprise rollout](https://learn.chatgpt.com/docs/enterprise/admin-setup)
- Anthropic: [Building effective agents（2024、歴史資料・現行approachへの注記あり）](https://www.anthropic.com/engineering/building-effective-agents) / [Managed Agents（2026、現行参照先）](https://www.anthropic.com/engineering/managed-agents) / [Context engineering](https://www.anthropic.com/engineering/effective-context-engineering-for-ai-agents) / [Claude Code extensions](https://code.claude.com/docs/en/features-overview) / [Best practices](https://code.claude.com/docs/en/best-practices) / [Managed settings](https://code.claude.com/docs/en/server-managed-settings)
- Google: [ADK docs repository](https://github.com/google/adk-docs) / [ADK workflows](https://github.com/google/adk-docs/blob/main/docs/workflows/index.md) / [Vertex agent evaluation](https://docs.cloud.google.com/vertex-ai/generative-ai/docs/agent-engine/evaluate) / [A2A](https://a2a-protocol.org/v1.0.0/) / [Gemini CLI](https://github.com/google-gemini/gemini-cli)
- GitHub/Microsoft: [Copilot customization](https://docs.github.com/en/copilot/reference/customization-cheat-sheet) / [Copilot hooks](https://docs.github.com/en/copilot/concepts/agents/hooks) / [Enterprise agent management](https://docs.github.com/en/copilot/concepts/agents/enterprise-management) / [Microsoft Agent Framework](https://github.com/microsoft/agent-framework) / [AutoGen](https://github.com/microsoft/autogen)
- AWS: [AI-DLC methodology](https://aws.amazon.com/blogs/devops/ai-driven-development-life-cycle/) / [Adaptive workflow launch](https://aws.amazon.com/blogs/devops/open-sourcing-adaptive-workflows-for-ai-driven-development-life-cycle-ai-dlc/) / [AI-DLC repository](https://github.com/awslabs/aidlc-workflows) / [Kiro docs](https://kiro.dev/docs/) / [AgentCore guide](https://docs.aws.amazon.com/bedrock-agentcore/latest/devguide/what-is-bedrock-agentcore.html)

### Methodologies and OSS communities

- [Superpowers](https://github.com/obra/superpowers)
- [GitHub Spec Kit](https://github.github.com/spec-kit/)
- [OpenSpec](https://github.com/Fission-AI/OpenSpec)
- [BMAD Method](https://docs.bmad-method.org/)
- [AI Hero](https://www.aihero.dev/)
- [12-Factor Agents](https://github.com/humanlayer/12-factor-agents)
- [LangGraph](https://github.com/langchain-ai/langgraph)
- [OpenHands](https://github.com/All-Hands-AI/OpenHands)

### Open standards, security, evaluation

- [Agent Skills specification](https://agentskills.io/specification)
- [AGENTS.md](https://agents.md/)
- [MCP specification 2026-07-28](https://modelcontextprotocol.io/specification/2026-07-28)
- [A2A Protocol v1.0](https://a2a-protocol.org/v1.0.0/)
- [NIST AI Agent Standards Initiative](https://www.nist.gov/artificial-intelligence/ai-agent-standards-initiative)
- [NIST AI 800-2 initial public draft](https://nvlpubs.nist.gov/nistpubs/ai/NIST.AI.800-2.ipd.pdf)
- [OWASP Agentic AI](https://genai.owasp.org/resource/agentic-ai-threats-and-mitigations/)
- [OpenTelemetry GenAI semantic conventions](https://github.com/open-telemetry/semantic-conventions-genai)
- [DORA 2025 research](https://dora.dev/research/2025/dora-report/)

### Methodの系譜を追うprimary research

- [ReAct: Synergizing Reasoning and Acting in Language Models](https://arxiv.org/abs/2210.03629)
- [Self-Refine: Iterative Refinement with Self-Feedback](https://papers.nips.cc/paper/2023/hash/91edff07232fb1b55a505a9e9f6c0ff3-Abstract-Conference.html)
- [Reflexion: Language Agents with Verbal Reinforcement Learning](https://arxiv.org/abs/2303.11366)
- [AutoGPT](https://github.com/Significant-Gravitas/AutoGPT)
