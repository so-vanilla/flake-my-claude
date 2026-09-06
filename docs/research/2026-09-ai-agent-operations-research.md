# AIエージェント運用の型 — 2026年9月一次資料調査

Status: research complete / operating hypothesis / no adoption or configuration decision

基準日: 2026-09-01（Asia/Tokyo）

## 1. 結論

2026年9月時点で、個人にも会社にもそのまま適用できる単一の「標準methodology」は存在しない。一方、本調査では主要provider、open standard、community methodologyの現行一次資料から、運用architectureに共通して現れるdesign directionを次のように統合推論した。これはprovider共同標準やhead-to-headの効果実証ではない。

推奨する基本形は、次の5層である。

1. **最小のalways-on kernel**: 全taskで変わらないauthority、safety、source of truth、完了条件、より詳しい手順の探し方だけを常時読む。
2. **just-in-time capability**: diagnose、research、plan、TDD、review、release等のtask-specific Skillとreferenceを、必要時だけ明示的または限定的なimplicit routingで読む。
3. **task/risk router**: 曖昧さ、変更範囲、時間軸、外部影響、stakeholder数、規制性に応じて、直接実行、単一Skill、artifact-driven workflow、full lifecycle frameworkを選ぶ。
4. **prompt外のcontrol plane**: permission、sandbox、identity、secret、network、approval、hook、CI、deployment gateをmodelへの文章指示だけに依存させない。
5. **durable evidence plane**: spec、decision、task state、event/trace、test、review、checkpoint、auditをchat contextの外へ残し、resume、handoff、eval、改善に使う。

この形は「小さなSkillだけ」でも「常にfull framework」でもない。**軽いdefaultと、条件付きの工程昇格**である。

### 中心論点への答え

「小さなSkillを必要時に呼び、常時読まれるSkillを最小限にする」ことは、本調査の初期operating hypothesisとして通常のdefault候補になる。最終採用はSection 7の同条件比較で、verified success、rework、人間時間、policy violation等を測って反証する。OpenAIのSkillはname/descriptionだけを初期contextへ置き、選択後に完全な`SKILL.md`を読むprogressive disclosureを採る。初期Skill一覧にはcontext budgetがあり、各Skillを一つのjobへ絞ることも推奨される。一方、`AGENTS.md`はwork開始前に読まれ、project階層のinstruction chainへ常時入る。この違い自体が、always-on ruleとon-demand procedureを分ける根拠になる。[OpenAI: Build skills](https://learn.chatgpt.com/docs/build-skills) / [OpenAI: AGENTS.md](https://learn.chatgpt.com/docs/agent-configuration/agents-md)

Anthropicも、良いcontext engineeringを「望む結果に必要な最小のhigh-signal token集合」と表現し、file pathやquery等の軽いidentifierからruntimeにcontextを取得するjust-in-time戦略とprogressive disclosureを説明する。ただし、開始時に読む`CLAUDE.md`とruntime探索を組み合わせるhybridも明示している。したがって「最小」は「短ければよい」ではなく、期待挙動を満たす十分な情報を、適切な時点とscopeへ置くことを意味する。[Anthropic: Effective context engineering](https://www.anthropic.com/engineering/effective-context-engineering-for-ai-agents)

ただし、これだけでは長時間・高曖昧・多人数・高risk taskを統治できない。Superpowersはcomposable Skillsから構成されながら、initial instructionによりbrainstorming、design approval、plan、TDD、subagent development、reviewという完全methodologyを自動適用する。AWS AI-DLCは33 stage、adaptive scope/depth、approval gate、audit trail、resumeを持つ。つまり両者は「Skillとframework」の反対側ではなく、**Skill等を配布単位に使いながらlifecycle ownershipを取るframework**である。[Superpowers](https://github.com/obra/superpowers) / [AWS AI-DLC workflows](https://github.com/awslabs/aidlc-workflows)

したがって本当の選択は、次の問いで行う。

- taskの進め方を人間と個別Skillの組み合わせが所有するか、frameworkがphaseとgateごと所有するか。
- stateをchatとGitだけで持つか、workflow engine、artifact graph、event logで持つか。
- 規約への従属をmodelの判断に委ねるか、deterministic hook/policy/CIで強制するか。
- frameworkの一貫性が、context、ceremony、更新、debug、lock-inの費用を上回るか。

### 推奨する昇格表

| tier | taskの特徴 | default運用 | framework昇格条件 |
| --- | --- | --- | --- |
| 0: assist | 質問、要約、read-only調査、明確な一手 | 直接agent + 必要なtoolだけ | durable成果物や外部actionが必要になったらTier 1以上 |
| 1: bounded | 一ファイル、小bug、明確なacceptance、低risk | task-specific Skill 1つ + targeted verification | 原因不明、複数工程、複数file、再開が必要ならTier 2 |
| 2: structured | 曖昧なfeature、複数file、brownfield、数十分以上 | 明示的diagnose/spec/plan + work ledger + independent review | 複数repo/team、長時間、並列作業、formal approvalならTier 3 |
| 3: lifecycle | greenfield、cross-repo、multi-agent、複数stakeholder | artifact-driven workflowまたはadaptive lifecycle framework | regulated/high-impact、production write、厳密な職務分離ならTier 4 |
| 4: governed | production、security、finance、legal、個人情報、不可逆action | managed policy、identity、sandbox、approval、auditを持つgoverned lifecycle | 自動昇格はしない。人間がauthorityとevidenceを定義して開始 |

このtierはtask規模だけで決めない。小さな変更でも外部送信や本番secretを扱えばTier 4になり得る。大きなread-only researchは、state管理が必要でもexternal action riskは低い。**complexityとimpactを別軸で評価する。**

## 2. 調査方法と証拠の扱い

本調査は、先に作成した[テーマ地図](./2026-09-ai-agent-operations-theme-map.md)の15テーマを、次の一次情報へ当てたscoping reviewである。

- 現行product documentation、official repository、specification、release note
- provider/開発者本人のengineering articleとfirst-party case study
- original research paper、formal benchmark/evaluation guidance
- NIST、OWASP、OpenTelemetry、DORA等の原典

二次的なまとめ記事、比較affiliate、SNS上の印象は根拠にしない。ただし一次情報にも役割差がある。

| 証拠種別 | このレポートで証明できること | 証明できないこと |
| --- | --- | --- |
| normative spec/policy | 準拠すべきinterface、要求、禁止 | 実装品質、task成功率 |
| current product docs | 現在提供・推奨される操作、architecture | 他方式より優れること |
| design/engineering article | providerが経験から選んだ設計と理由 | 一般化可能な因果効果 |
| research paper/benchmark | 明示条件での実験結果 | 自社環境でのROI、長期保守性 |
| first-party case | その組織・条件で起きた結果 | 平均的な導入効果 |
| methodology README | 作者の設計意図、workflow、導入surface | 効果、利用継続率、独立評価 |

「公式が推している」を「実証された唯一解」とは扱わない。公開日、対象version、後継文書も確認する。たとえばAnthropicの2024年版`Building effective agents`は、simple composable patternsを強調する歴史的資料だが、現在はtooling landscapeが変化したとしてManaged Agentsを参照する注記がある。2026年の現行設計では、session、harness、sandboxを分離し、harness自体がmodel進歩で陳腐化する前提を置く。[Anthropic: Building effective agents](https://www.anthropic.com/engineering/building-effective-agents) / [Anthropic: Managed Agents](https://www.anthropic.com/engineering/managed-agents)

## 3. まず分離すべき5つの対象

AI agent運用の議論が混線する最大の原因は、次を同じ「framework」と呼ぶことにある。

| 対象 | 問い | 代表例 |
| --- | --- | --- |
| Model | どこまで推論・tool選択・error recoveryできるか | GPT、Claude、Gemini等 |
| Harness/runtime | context、agent loop、tool実行、stream、session、sandboxをどう提供するか | Codex harness、Claude Managed Agents、ADK、Agent Framework |
| Capability package | task-specificな知識・手順・connectorをどう発見・配布するか | Skills、plugins、MCP servers |
| Methodology | 要求から検証・運用まで、誰がphase、artifact、gateを所有するか | Superpowers、AI-DLC、BMAD、Spec Kit、OpenSpec |
| Governance | authority、identity、policy、audit、eval、exceptionを誰が管理するか | admin controls、IAM、CI、AgentOps |

OpenAIの現行Codex資料も、再利用部分をagent loop/harnessとし、業務applicationがinterface、business context、rules、tools、approvalを所有する分離を示す。[OpenAI: Codex as a platform](https://developers.openai.com/blog/codex-as-a-platform)

この分離から、次の原則が導ける。

- Skillを増やすことは、methodologyを採用することと同義ではない。
- Subagentを使うことは、multi-agent methodologyを採用することと同義ではない。
- MCP/A2Aへ対応することは、permission、quality、semantic portabilityを保証しない。
- frameworkを導入しても、sandbox、secret、identity、CIは別途必要である。
- 高性能modelへ替えても、durable state、audit、organization ownershipは自動では得られない。

## 4. 個人用の推奨operating model

### 4.1 Globalは原則だけにする

個人のglobal always-on instructionは、次へ絞る。

- language/output preference
- authority boundary: read、edit、commit、push、deploy、external messageを分離
- destructive/high-impact actionのconfirmation rule
- evidenceを伴うcompletion、unverified事項の明示
- project-local instruction、Skill、source of truthを先に探すrouting rule
- 失敗時に隠さず停止・復旧情報を残すrule

特定言語のcoding style、build command、repository architecture、release procedureはproject-localへ置く。diagnosis、research、planning、TDD、review、document generation等の長い手順はSkillへ移す。

### 4.2 Skillは小さく、重要workflowは明示起動にする

個人用では、Skillをtask単位へ分け、descriptionをrouting contractとして扱う。誤発火のcostが高いworkflow、外部action、長時間taskは明示起動をdefaultにする。暗黙起動はread-only、低risk、容易に取り消せる補助へ限定する。

推奨する最小catalogは製品名ではなくjobで定義する。

- diagnose: 原因を特定し、fix authorityがなければ止まる
- research: primary source、claim/source対応、dated report
- plan: scope、source/generated、validation、stop/gate、recovery
- implement: bounded change、repository standard、verification
- tdd: red/green/refactorが必要なfeature/bug
- review: specとrepo standardを独立評価
- verify: acceptance criteriaと実証を照合
- release/external-action: commit、push、publish、message等を明示authorityへ結ぶ
- ledger/handoff: 長時間・並列・中断taskだけで使う

### 4.3 frameworkはtask profileとして選ぶ

常時一つのframeworkへ全taskを通すのではなく、projectまたはtaskごとにprofileを選ぶ。

- `direct`: Tier 0〜1
- `structured`: 明示Skill列 + artifact + ledger、Tier 2
- `spec-driven`: Spec Kit/OpenSpec型、Tier 2〜3
- `methodology`: Superpowers/BMAD型、Tier 3
- `governed-lifecycle`: AI-DLC等を含む強いstate/gate/audit、Tier 3〜4

同じtaskに二つのlifecycle ownerを置かない。global router、Superpowers、AI-DLC、独自orchestratorが同時にphase遷移やmodel routingを指示すると、二重承認、state divergence、停止条件の衝突が起きる。選ばれたowner以外は、utility Skillまたはcontrolとして従属させる。

## 5. 会社用の推奨operating model

会社用は個人設定の一括配布ではない。必要なのは、**federated governance**である。

### 5.1 中央で所有するもの

- approved model/providerとdata handling条件
- identity、authorization、secret、network、sandbox、retention
- high-impact actionとhuman approval policy
- organization-level minimum instructionと禁止事項
- approved Skill/plugin/MCP catalog、provenance、pin、更新・rollback
- 共通trace/eval schema、incident reporting、exception process
- provider/frameworkを替えても残るoutcome metric

### 5.2 Team/projectで所有するもの

- domain vocabulary、architecture、repository conventions
- build/test/review/release command
- project-local Skillとartifact schema
- task archetype別のworkflow profile
- acceptance criteria、eval dataset、known failure mode
- 中央標準への期限付きexceptionとowner

### 5.3 各利用者が所有するもの

- task intentと最終判断
- agentへ委任するauthorityの範囲
- generated outputのreview
- uncertainty、incident、policy conflictのescalation

会社用でframeworkを強くする理由は、agentの能力向上ではなく、複数人の合意、handoff、再現性、separation of duties、audit、教育が必要だからである。一方、全変更へfull SDLCを課すとshadow useと形骸化を招く。risk tierに応じた軽いpathを正式に用意する。

## 6. Task / risk decision matrix

### 6.1 Workflowを選ぶ6つの質問

task開始時に、製品名ではなく次の6問へ答える。

1. **曖昧さ**: 成功条件と必要stepは事前に定義できるか。
2. **範囲**: 一操作、一file、一repository、cross-repo、組織processのどこまでか。
3. **時間と復旧**: 一sessionで終わるか。中断、compaction、handoff、retryがあるか。
4. **影響**: read-only、reversible write、Git publish、production/external/financial/legal actionのどれか。
5. **協働と説明責任**: 一人の判断か、複数stakeholder、職務分離、監査が必要か。
6. **ground truth**: test、schema、live state、承認済みrecord等で結果を検証できるか。

| archetype | 推奨profile | 必須evidence | 避けること |
| --- | --- | --- | --- |
| Q&A、要約、単発read-only | direct | source/観測結果 | ledger、multi-agent、full frameworkの常用 |
| 原因が既知の小修正 | bounded Skill | targeted test、diff | 自動で長いspec一式を作る |
| 原因不明のbug | diagnose → fixを分離 | reproduced symptom、cause evidence、regression test | diagnosis authorityから勝手にfixへ拡張 |
| 複数fileのfeature | structured | acceptance、plan、test、review | chatだけをplan/source of truthにする |
| brownfield横断変更 | structuredまたはspec-driven | codebase evidence、impact map、migration/rollback | greenfield前提のarchitectureを上書き |
| greenfield/複数stakeholder | methodology/lifecycle | intent、spec、decision、task、review gate | 一人のagent判断を合意とみなす |
| long-running research | research + ledger/handoff | dated source、claim/source map、unverified事項 | context summaryだけを永続stateにする |
| release、external message、production write | governed | authority、fresh state、preview、approval、post-write reread | task完了から自動publishへ進む |
| regulated/high-impact process | governed lifecycle | identity、policy、separation、audit、incident recovery | prompt上の「禁止」だけでcontrolしたとする |

### 6.2 Framework選択を二段階にする

まず必要な**運用特性**を決め、その後で候補を選ぶ。

| 必要な特性 | 候補となる型 |
| --- | --- |
| design、plan、TDD、reviewを一貫して習慣化 | Superpowers型のSkill-based methodology |
| constitution、spec、plan、tasks、implementationのtraceability | Spec Kit型 |
| brownfield changeをproposal/spec/design/tasksとして軽量管理 | OpenSpec型 |
| product/architecture/UX/dev/testの複数perspectiveと可変process | BMAD型 |
| stage、approval、audit、resume、adaptive scopeを持つfull lifecycle | AI-DLC型 |
| 自社固有のprocessをapplication UI、business data、approvalへ埋め込む | open/managed harness + application-owned workflow |

比較時はframework名の機能数ではなく、taskに必要なstate、artifact、gate、enforcement、integration、更新責任を満たすかを見る。

## 7. 導入と比較実験

### Phase A: 現状baselineを取る

導入前に、代表taskを最低4種類選ぶ。

- 小さく明確なbug
- 曖昧なbrownfield feature
- 長時間researchまたはmigration
- high-impactなrelease/external actionのdry-run

各taskについて、success、defect/rework、wall time、model call/token、human介入、review時間、停止・復旧、policy violationを記録する。既存の方法で取れないmetricは「未取得」とし、ゼロで埋めない。

### Phase B: Minimal profileを作る

- global always-on kernelを短い原則へ限定
- project instructionをrepository固有情報へ限定
- job別Skillを明示起動可能にする
- permission、sandbox、CI、external action gateを別controlへ置く
- long taskだけledger/checkpointを使う

まずこのprofileでbaseline taskを再実行する。これが以後の比較対照になる。

### Phase C: Structured / framework profileを比較する

同じmodel/effort、同じrepository snapshot、同じtask/acceptance、同じ権限、同じtimeoutで比較する。少なくとも複数回実行し、一回の成功を一般化しない。

比較するのは、たとえば次の3条件である。

1. minimal kernel + task Skill
2. minimal kernel + explicit spec/plan/ledger/review chain
3. 一つのlifecycle-owning framework

model変更とworkflow変更を同時に行わない。multi-agentの有無も別experimentにする。framework導入時は、実際に常時loadされるinstruction、Skill metadata、hook、tool schema、生成artifactを測る。

### Phase D: 個人の採用判断

個人用では、平均速度だけでなく次を重くする。

- agentが何をしているか理解できる
- course correctionとescapeが容易
- 中断後に自力で再開できる
- setup/updateに時間を奪われない
- ceremonyがtask価値を超えない
- 失敗時に元へ戻せる

一つのframeworkを標準にする必要はない。task profileごとに勝者が異なるなら、その差をrouting ruleとして採用する。

### Phase E: 会社のpilotと昇格

会社では最初からauto-mergeやproduction actionを許さない。read-only、draft、PR作成等の限定surfaceから始める。

1. 代表teamとtask archetypeを選ぶ。
2. policy、data、identity、retention、incident ownerを先に決める。
3. shadowまたはhuman-reviewed modeでbaselineと比較する。
4. quality、lead time、review burden、failure/recovery、policy complianceを評価する。
5. 成功条件を満たしたtask classだけauthorityを段階昇格する。
6. model、Skill、framework、policy versionを記録し、変更時にregression evalする。

導入率や生成量だけを成功指標にしない。会社の最終metricは、delivery outcome、品質、保守性、incident、学習、利用者の理解と責任能力である。

## 8. 避けるべき運用

1. **Mega prompt**: すべての手順、domain知識、例外をglobal instructionへ入れる。
2. **Framework everywhere**: 一行修正と規制対象migrationを同じceremonyへ通す。
3. **二重orchestrator**: global router、Superpowers、AI-DLC、独自workflowが同時にphaseを所有する。
4. **Prompt-as-security**: 「secretを読まない」「pushしない」という文だけをpermissionとみなす。
5. **Memory-as-truth**: agent自身のsummary、memory、ledgerをfresh sourceで再確認しない。
6. **Multi-agent by default**: 分割不能なtaskまでworkerを増やし、通信・統合・conflictを増やす。
7. **Official equals proven**: provider docsやmethodology READMEを独立比較実験とみなす。
8. **Install equals adoption**: fileを置いた、commitした、pluginを入れたことをlive activation・正しいrouting・効果検証と同一視する。
9. **Model/workflow confounding**: model、effort、prompt、tools、frameworkを一度に変更して改善要因を失う。
10. **Output-only evaluation**: 最終文だけを見て、無断action、不要tool call、偶然の成功、recovery不能を見落とす。

## 9. Long-horizon stateとhandoff

### 9.1 Chat history、memory、workflow stateを分ける

長時間作業では、次を別の正本として設計する。

| state | 役割 | 必須情報 |
| --- | --- | --- |
| append-only event log | 何が起きたか、監査と障害復旧 | run/session、sequence、actor、tool/action、approval、result/error、artifact/trace reference |
| checkpoint | どこから再開するか | workflow/model/version、current state、pending action/approval、last event、idempotency |
| durable artifact | 何を決め、何を作ったか | intent/spec/plan/decision/code/test/report、owner、version、provenance、acceptance evidence |
| handoff | 次の人/agentが何を再取得すべきか | objective、scope/authority、done/pending、risk、changed artifact、exact next action、verification |
| memory/compaction | contextへ戻すためのcache/projection | source refs、生成時刻、scope、TTL/staleness、supersession |

Anthropic Managed Agentsはsession logをharness外へdurably置き、harness failure時に新しいharnessがevent logから復帰する設計を説明する。OpenAI Agents SDKもapproval interruptionを`RunState`として保存・再開できるが、side effectを伴うrequestの再送は重複実行を起こし得ると警告する。したがってresumeにはevent cursorだけでなく、idempotency keyとexternal receiptが必要である。[Anthropic: Managed Agents](https://www.anthropic.com/engineering/managed-agents) / [OpenAI Agents SDK: Results and RunState](https://openai.github.io/openai-agents-python/results/)

Compactionはcontext windowを続けるための変換であり、監査可能な正本ではない。OpenAIのstandalone compactionも次のcontextに用いるopaqueなcompaction itemを生成する。compaction前に決定、変更、承認、未完了task、検証結果をdurable artifactへflushし、再開時にfresh sourceを読む。[OpenAI: Compaction](https://developers.openai.com/api/docs/guides/compaction)

### 9.2 Handoffをtestする

handoffの成功条件はsummaryを書いたことではない。次のworkerが以下を再取得でき、権限を再評価してから再開できることをtestする。

- repository/data/artifact version
- workflow、model、tool、policy version
- pending approvalとauthority
- last verified evidence
- exact next actionとstop condition
- side-effect receipt、idempotency、rollback state

Memory/context poisoningがあり得るため、agent自身が書いたmemoryはsource reference、timestamp、scope、TTLを持つcacheとする。secret、credential、approval token、現在権限はmemoryへ保存しない。[OWASP: Agentic AI Threats and Mitigations](https://genai.owasp.org/resource/agentic-ai-threats-and-mitigations/)

## 10. Eval、observability、継続改善

### 10.1 三つの評価を混ぜない

| 評価 | 問い | 第一evidence |
| --- | --- | --- |
| outcome | 実環境に目的状態が存在するか | test、schema、diff、database/API reread、artifact digest、human rubric |
| trajectory | 禁止action、無駄なloop、権限逸脱、誤handoffがなかったか | policy rule、tool/action trace、sampled independent review |
| delivery impact | 品質、保守、delivery、人間負荷へ何が起きたか | CI、defect/incident、DORA、business KPI、user feedback |

Anthropicはagentの発言ではなくtrial後のenvironment stateをoutcomeとし、trajectoryを全message/tool callとして分ける。OpenAIはtrace gradingでworkflow-level errorを見つける。Google Vertex AIもfinal responseとtrajectory/tool useを別categoryで扱う。したがって「agentが完了と言った」はどの層のpassにもならない。[Anthropic: Agent evals](https://www.anthropic.com/engineering/demystifying-evals-for-ai-agents) / [OpenAI: Agent evals](https://developers.openai.com/api/docs/guides/agent-evals) / [OpenAI: Trace grading](https://developers.openai.com/api/docs/guides/trace-grading) / [Google: Evaluate agents](https://docs.cloud.google.com/vertex-ai/generative-ai/docs/agent-engine/evaluate)

同じclaimをdeterministic checkで検証できる場合、LLM judgeを第一graderにしない。順序は次にする。

1. deterministic test/schema/state reread
2. versioned rubricでcalibrateしたjudge
3. 独立したhuman review

Judgeは主観品質やtrajectory triageに有用だが、gold setに対するfalse positive/negative、judge model/version/prompt、試行分散を記録する。NIST AI 800-2はmeasurement対象の定義、実行、分析/reportingとuncertaintyの透明化を求めるが、2026-09-01時点ではinitial public draftである。[NIST AI 800-2 draft](https://nvlpubs.nist.gov/nistpubs/ai/NIST.AI.800-2.ipd.pdf)

### 10.2 Production feedbackをevalへ戻す

1. failed action、human override、incident、rollback、sampled traceをfailure taxonomyへ分類する。
2. privacy review後、再現可能な最小caseをversioned eval datasetへ昇格する。
3. model、prompt、Skill、workflow、toolの変更を一要素ずつ同じdatasetで比較する。
4. regression gate通過後にlimited rolloutする。
5. production outcomeを再びdatasetへ戻す。

Traceはそれ自体が新しいsensitive data surfaceになる。prompt/tool input/outputの本文はdefault収集せず、必要ならaccess controlとretentionを分けたstoreへのreferenceにする。OpenTelemetryのGenAI agent semantic conventionsはportable telemetryの有力候補だが、agent spanは基準日時点でDevelopment statusである。[OpenTelemetry GenAI semantic conventions](https://github.com/open-telemetry/semantic-conventions-genai)

## 11. Securityとauthority

### 11.1 Model instructionの外へcontrolを置く

| layer | control |
| --- | --- |
| task | versioned task ID、scope、authority、success/stop conditions |
| content | web/mail/document/tool outputをuntrusted dataとして扱う |
| capability | task専用tool subset、read-only default、rate/cost limit |
| sandbox | filesystem/process isolation、network deny-by-default |
| identity | human、agent/workload、service principalを分離 |
| credential | short-lived、task/tool/resource scoped、vault/brokerからJIT取得 |
| action policy | destination、amount、resource version、schema、TOCTOUを直前確認 |
| approval | destructive、external、privileged、goal-changing actionのdiff/plan確認 |
| audit/recovery | actor、delegation、policy、approval、action digest、receipt、rollback |

CodexはOS sandboxとapproval policyを分け、workspace外writeやnetworkを制御する。Claude Codeもpermission ruleとOS sandboxを別layerとして扱う。どちらも、prompt上の指示だけではなくreachable capabilityを減らす設計である。[OpenAI: Agent approvals & security](https://learn.chatgpt.com/docs/agent-approvals-security) / [Claude Code: Permissions](https://code.claude.com/docs/en/permissions) / [Claude Code: Sandboxing](https://code.claude.com/docs/en/sandboxing)

Human approvalは毎回表示すれば安全になるわけではない。approval fatigueを避け、次へ限定する。

- sandbox/allowlist外への権限昇格
- delete、publish、deploy、purchase、send、transfer
- secret/PII/confidential dataの新しいdestinationへの送信
- goal、scope、budget、owner、risk tierの変更
- production/high-impact systemへのwrite
- independent verification failure後のoverride

承認画面にはcommandだけでなく、目的、対象、差分/amount/destination、data classification、risk、rollback、expiryを表示する。policy service failureやapproval timeoutは中〜高riskでfail closedにする。

会社ではhuman initiator、agent identity/version、session、policy version、approval actor、action digest、external receiptを結ぶ。一般traceはattributionに役立つがcryptographic non-repudiationを保証しない。NISTもagent identity、authorization、auditing、non-repudiationを未解決の標準化課題として扱っている。[NIST: Identity and authority of software agents](https://www.nist.gov/news-events/news/2026/02/new-concept-paper-identity-and-authority-software-agents)

## 12. Interoperabilityの限界

「対応している」を4段階に分ける。

1. **format**: parse、schema、version negotiationが通る。
2. **discovery**: 同じscope/precedenceでSkill、tool、agentが見つかる。
3. **semantics**: 同じinputが同じoperation、side effect、error、idempotencyを意味する。
4. **authority**: delegation、identity、credential scope、approval、audit、retentionがend-to-endで維持される。

| surface | 主に標準化するもの | 標準化しないもの |
| --- | --- | --- |
| AGENTS.md | project instructionのfile位置とnested scope | 解釈、強制、permission、audit |
| Agent Skills | `SKILL.md`形式、directory、progressive disclosure | routing精度、runtime、安全性、side effect意味 |
| MCP | tool/resource接続、request/response、version/auth extension | toolの正しさ、business semantics、safe composition |
| A2A | remote agent discovery、task、message/artifact、stream/push | remote agentの内部state、truthfulness、policy equivalence |

Agent Skillsの`allowed-tools`はexperimentalであり、runtime間でpermission semanticsが同じとは限らない。MCPのtransport sessionはdurable workflow stateではない。A2AのAgentCardは相手agentの能力とsecurity schemeを発見できても、委任chainやoutputの真実性を保証しない。[Agent Skills specification](https://agentskills.io/specification) / [MCP 2026-07-28](https://modelcontextprotocol.io/specification/2026-07-28) / [A2A 1.0](https://a2a-protocol.org/latest/specification) / [NIST AI Agent Standards Initiative](https://www.nist.gov/artificial-intelligence/ai-agent-standards-initiative)

## 13. 経済性

Token単価ではなく、**independently verified success当たりの総費用**で比べる。

```text
total task cost =
  model input/output/cache/reasoning
  + model calls/compaction
  + tool/compute/storage/network
  + human wait/review/approval/training
  + failed attempt/retry/rework/rollback/incident
  + catalog/policy/eval/observability amortization
```

記録するmetricは、model/effort別tokenとcall、tool call、parallelism、p50/p95 first-useful-actionとcompletion、人間の待ち/レビュー、approval、failure/retry、failed change、rollback/recovery、verified outcomeである。

Model/effortは「plannerだから常に最強model」のような固定roleで決めず、代表taskのquality-cost frontierで決める。小model/low effortから始めるtask、balanced modelが必要なtask、high effortが実測上有効なtaskを分ける。Max/pro/multi-agentはhardest quality-first workloadでverified outcomeが改善するときだけ例外採用する。OpenAIの現行model guidanceもeffortを意図的に選び、自taskでquality、latency、costを比較するよう求める。[OpenAI: Current model guidance](https://developers.openai.com/api/docs/guides/latest-model) / [OpenAI: Cost optimization](https://developers.openai.com/api/docs/guides/cost-optimization) / [OpenAI: Latency optimization](https://developers.openai.com/api/docs/guides/latency-optimization)

Parallelizationはwall-clockを短縮しても、total token/call、integration、reviewを増やし得る。時間と総費用を別metricにする。

## 14. 2020〜2026年の言説とmethodの系譜

現在の型は、過去のmethodを全否定して生まれたのではない。初期patternを内側に残しながら、外側へcontext、state、control、evidenceを追加してきた。

| 時期 | 中心的な主張 | 2026年時点の位置づけ |
| --- | --- | --- |
| 2020 | RAGでmodel外knowledgeをretrieveし、generationをgroundする | 有効。ただしknowledge retrievalでありworkflow state、approval、resumeではない。[RAG](https://arxiv.org/abs/2005.11401) |
| 2022〜23 | ReActでreasoningとactionを観測を挟んで反復する | 現行agent loopの内核。権限、durable state、stop conditionは別途必要。[ReAct](https://arxiv.org/abs/2210.03629) |
| 2023 | Self-Refine／Reflexionでfeedback、reflection、episodic memoryを使う | review/evaluator/test-feedbackへ継承。自己feedbackは独立ground truthではない。[Self-Refine](https://papers.nips.cc/paper_files/paper/2023/hash/91edff07232fb1b55a505a9e9f6c0ff3-Abstract-Conference.html) / [Reflexion](https://arxiv.org/abs/2303.11366) |
| 2023 | AutoGPT型の長期自律loop、AutoGen型multi-agent conversation | 歴史的に重要。AutoGPT Classicはdeprecated、AutoGenはMicrosoft Agent Frameworkへのmigration対象。[AutoGPT Classic](https://github.com/Significant-Gravitas/AutoGPT/blob/master/classic/README.md) / [Microsoft migration guide](https://learn.microsoft.com/en-us/agent-framework/migration-guide/from-autogen/) |
| 2023〜24 | Generative Agents、MemGPT等がepisodic/virtual memoryを前面化 | context外stateの発想へ貢献。memory内容のfreshness、authority、監査は別問題。[Generative Agents](https://arxiv.org/abs/2304.03442) / [MemGPT](https://arxiv.org/abs/2310.08560) |
| 2024 | workflowはpredefined path、agentはmodel-driven path。必要なだけ複雑化 | 分類と「simplest sufficient」は有効。ただしAnthropic記事は現行実装としてManaged Agentsを後継参照する。[Building effective agents](https://www.anthropic.com/engineering/building-effective-agents) |
| 2025 | 12-Factor Agents、context engineering、multi-agent実運用、SDD | deterministic software、owned control/state、minimal/JIT context、artifact-firstへ重心移動。[12-Factor Agents](https://github.com/humanlayer/12-factor-agents) / [Context engineering](https://www.anthropic.com/engineering/effective-context-engineering-for-ai-agents) / [Anthropic multi-agent research](https://www.anthropic.com/engineering/multi-agent-research-system) / [Spec Kit](https://github.github.com/spec-kit/) / [OpenSpec](https://github.com/Fission-AI/OpenSpec) |
| 2026 | durable session、交換可能harness、sandbox分離、deterministic workflow＋agent、domain applicationへの埋め込み | 本調査が複数現行一次資料から統合した強いproduction方向。[Managed Agents](https://www.anthropic.com/engineering/managed-agents) / [Google ADK 2.0](https://developers.googleblog.com/why-we-built-adk-20/) / [Codex as a platform](https://developers.openai.com/blog/codex-as-a-platform) |

### 残ったもの、現行production guidanceでは単独defaultとして不足するもの

残ったもの:

- environment observationを挟むtool loop
- clear criteriaと外部signalを使うgenerator–evaluator
- 必要なcontextだけを取得するretrieval
- taskを分解できる場合のparallel/subagent
- versioned intent/spec/plan/task artifact

主要providerとcommunityの現行production guidanceで、それだけをdefaultまたは十分条件にしないもの:

- 無制限な自律loopだけで運用を成立させること
- self-reflectionだけをverificationとみなすこと
- chat historyや長いcontextだけをdurable stateとみなすこと
- agent数だけでmulti-agentの有効性を判断すること
- framework READMEの自己評価だけを効果証拠とみなすこと

これは普及率や効果順位のempirical claimではなく、Managed Agents、Codex harness、ADK 2.0、agent eval等の現行資料からrootが統合した運用上の推論である。[Anthropic: Managed Agents](https://www.anthropic.com/engineering/managed-agents) / [OpenAI: Codex as a platform](https://developers.openai.com/blog/codex-as-a-platform) / [Google ADK 2.0](https://developers.googleblog.com/why-we-built-adk-20/) / [Anthropic: Agent evals](https://www.anthropic.com/engineering/demystifying-evals-for-ai-agents)

Anthropicのmulti-agent research systemはbreadth-firstな独立探索で内部eval改善を報告した一方、multi-agentはchatの約15倍tokenを使い、codingは独立subtaskが少なく向かない場合があると説明する。multi-agentはdefaultではなく、並列性、context isolation、統合可能性、task価値で選ぶ。[Anthropic: Multi-agent research system](https://www.anthropic.com/engineering/multi-agent-research-system)

## 15. Community methodology spectrum

| 型 | lifecycle ownership | state/artifact | 強制力 | 適する用途 | 注意点 |
| --- | --- | --- | --- | --- | --- |
| AI Hero / `mattpocock/skills` | humanがSkillを選ぶ | spec、ticket、commit、review等をSkillごと | 主にinstruction | 個人、既存processへの部分導入 | end-to-end state/release authorityは別途。[repo](https://github.com/mattpocock/skills) / [v1.2.3](https://github.com/mattpocock/skills/releases/tag/v1.2.3) |
| Superpowers | conversation開始からbranch終了まで広く所有 | design、plan、task、test/review、Git worktree | mandatory Skill protocol＋human approval | design/TDD/reviewを一貫して習慣化 | Skill形式でもlifecycle-owning。別routerと併用しない。[repo](https://github.com/obra/superpowers) / [v6.3.0](https://github.com/obra/superpowers/releases/tag/v6.3.0) |
| OpenSpec | change/spec lifecycleを所有 | current spec、delta、proposal、design、tasks、archive | schema/validation/archiveはCLI、guidanceはinstruction | brownfield、living spec、既存workflowとの合成 | implementation/release全体は所有しない。[docs](https://github.com/Fission-AI/OpenSpec/blob/main/docs/getting-started.md) / [v1.11.0](https://github.com/Fission-AI/OpenSpec/releases/tag/v1.11.0) |
| Spec Kit | SDDまたはcustom process | constitution、spec、plan、tasks、checklist | artifact/template中心。hook/CIはextension | organization catalog、large feature、custom process | community componentのreview/pinが必要。[docs](https://github.github.com/spec-kit/) / [v1.0.2](https://github.com/github/spec-kit/releases/tag/v1.0.2) |
| BMAD | discovery/planningからimplementationまで。quick pathあり | PRD、SPEC、architecture、epic/story、sprint、review/retro | guided workflow/document gate | product discovery、教育、role別perspective | harness外writeのdeterministic拒否は別control。[docs](https://docs.bmad-method.org/) / [v6.11.0](https://github.com/bmad-code-org/BMAD-METHOD/releases/tag/v6.11.0) |
| AWS AI-DLC | initializationからoperationのstate machine | intent、state、audit、artifact、gate receipt | TypeScript engine、hooks、approval/evidence guard | high-risk、長期、traceability、複数stakeholder | install/trust/runtime/update/bypass governanceが必要。[repo](https://github.com/awslabs/aidlc-workflows) / [v2.7.0](https://github.com/awslabs/aidlc-workflows/releases/tag/v2.7.0) |
| 12-Factor Agents | applicationが所有 | unified event/thread state | 通常codeで実装する設計原則 | customer-facing agent application | coding SDLC workflowではない。[repo](https://github.com/humanlayer/12-factor-agents) |
| LangGraph / OpenHands | execution runtime/harness | checkpoint/thread/workspace | code、runtime、sandbox | durable execution、custom/remote agent | methodologyではなく別decision。[LangGraph](https://github.com/langchain-ai/langgraph) / [OpenHands](https://docs.openhands.dev/sdk/guides/agent-server/overview) |

### Frameworkは必ずしも固定的full flowではない

Superpowers v6はspike/bounded/architecturalへ分岐し、AI-DLCはbugfix 9/33、PoC 8/33、express 10/33からfeature/enterprise 33/33までscopeを変える。BMADも小さく明確な変更を`bmad-build`へ直行できる。したがってframework比較では、最大工程数ではなく、代表taskで実際に選ばれるpath、常時context、turn数、artifact量、human wait、failure recoveryを測る。[Superpowers v6.3.0](https://github.com/obra/superpowers/releases/tag/v6.3.0) / [AI-DLC scopes](https://github.com/awslabs/aidlc-workflows/blob/main/docs/guide/05-scopes-and-depth.md) / [BMAD workflow map](https://docs.bmad-method.org/reference/workflow-map/)

### 強制力は四段階で読む

1. model instruction
2. artifact schema／CLI validation
3. hook／state machine／policy
4. OS sandbox／IAM／CI／deployment control

たとえばSuperpowersのTDDやapprovalは主にSkill protocol、OpenSpecのartifact dependency/validationはCLI、AI-DLCのplan approval/state transitionはhook/engineでも検査される。しかし、どのmethodologyもIAM、secret、network、production deployを自動的に安全にするわけではない。[Superpowers `using-superpowers`](https://github.com/obra/superpowers/blob/main/skills/using-superpowers/SKILL.md) / [OpenSpec CLI](https://github.com/Fission-AI/OpenSpec/blob/main/docs/cli.md) / [AI-DLC approval guard](https://github.com/awslabs/aidlc-workflows/blob/main/core/hooks/aidlc-plan-approval-guard.ts)

## 16. よくある言説の2026年判定

| 言説 | 判定 |
| --- | --- |
| agentはprompt＋tools＋loop | 最小定義としては有効。production operating modelとしては不足 |
| reflectionを回せば正しくなる | 条件付き改善pattern。外部ground truthと独立verificationが必要 |
| 長いcontextでmemory問題は解消 | 不十分。context pollution、staleness、audit、resumeは残る |
| multi-agentほど強い | 誤り。並列性とcontext isolationが価値を持つtaskだけ |
| Skillを常時読ませるほど確実 | routing漏れは減り得るがcontextを汚す。critical gateは外部controlへ |
| SDDなら実装が正しい | traceabilityは改善するが、誤spec、drift、verification不足は残る |
| frameworkは小taskに必ず重い | adaptive/quick pathがある。実際のpathを測る必要 |
| AutoGPT/AutoGenが現在の標準 | 古い。Classicはdeprecated、AutoGenはmigration対象 |
| installすればprocessが守られる | 誤り。discovery、activation、hook trust、live enforcement、evalを分ける |

一次資料内に、Superpowers、AI-DLC、Spec Kit、OpenSpec、BMAD、AI Heroを同一task、model、repositoryで比較した独立benchmarkは確認できなかった。ここでの位置づけはarchitectureと設計意図の比較であり、効果順位ではない。

## 17. Provider別の2026年現在の型

### 17.1 OpenAI / Codex / Agents SDK

現行surfaceは次の役割分担を取る。

- `AGENTS.md`: 階層化されたalways-on project instruction
- Skills: metadata-first、本文は明示/implicit選択時に読むprogressive disclosure
- Subagents: 独立contextのspecialist。read-heavy parallelismから始める
- Codex harness: conversation、tools、sandbox、approval、turn間継続
- Host application: business context、rule、tool、approval、system of record
- Agents SDK: code-driven／LLM-driven orchestration、Session、trace/eval
- Enterprise: managed configuration、governance、permission mode

OpenAIが提示する現在形は、万能chatへ全業務を押し込むのではなく、再利用可能なagent harnessを業務applicationへ埋め込み、業務側がcontextとconsentを所有する構成である。[Codex as a platform](https://developers.openai.com/blog/codex-as-a-platform) / [Skills](https://learn.chatgpt.com/docs/build-skills) / [Subagents](https://learn.chatgpt.com/docs/agent-configuration/subagents) / [Agents SDK orchestration](https://openai.github.io/openai-agents-python/multi_agent/)

個人のdefaultは短い`AGENTS.md`＋少数のJIT Skill＋sandbox/approval。会社では同じcontext hygieneにmanaged policy、approved Skill/MCP、session/trace、eval datasetを足す。全taskを一つのfull methodologyへ通すことは公式surfaceから導かれない。

### 17.2 Anthropic / Claude Code / Managed Agents

Claude Codeは`CLAUDE.md`をalways-on、Skillsをon-demand、hooksをdeterministic event automation、subagentをisolated contextとして分ける。公式overviewは`CLAUDE.md`を200行未満へ保つことも推奨する。[Claude Code features](https://code.claude.com/docs/en/features-overview) / [Subagents](https://code.claude.com/docs/en/sub-agents)

長時間applicationでは、Managed Agentsがsession、harness、sandboxを分離する。Credentialはsandbox内へ置かずproxy/vaultを使える。Event historyとsandbox filesystem retentionも別物であり、長期artifactはdurable storageへ出す。[Managed Agents](https://www.anthropic.com/engineering/managed-agents) / [Managed Agents events](https://platform.claude.com/docs/en/managed-agents/events-and-streaming)

2024年の`Building effective agents`は分類と「必要なだけ複雑化」という原則には使えるが、current runtimeにはManaged Agentsを優先する。個人ではshort always-on＋Skill＋hooks/sandbox、会社ではmanaged deny、credential isolation、durable session、OTel/evalが追加される。

### 17.3 Google / Gemini CLI / ADK / A2A

Gemini CLIは`GEMINI.md`の階層context、Agent Skillsのprogressive disclosure、tool confirmation、isolated subagent、policy engine、sandboxを分ける。ADKはsimple agentを基本とし、instruction、context、deterministic/non-deterministic controlが複雑になったときgraph/dynamic workflowへ上げる。Sessionとlong-term Memoryも別serviceである。[Gemini CLI context](https://geminicli.com/docs/cli/gemini-md/) / [Gemini Skills](https://geminicli.com/docs/cli/skills/) / [ADK agents](https://adk.dev/agents/) / [ADK workflows](https://adk.dev/agents/workflow-agents/) / [ADK sessions](https://adk.dev/sessions/)

会社用enterprise strict modeはunmanaged Skillを無効化し得るため、個人の自由なSkill directoryをそのまま拡大できない。Approved catalogとsupply-chain管理が必要である。[Gemini enterprise controls](https://geminicli.com/docs/admin/enterprise-controls/)

A2Aはremote agent discovery、task、message/artifact、long-running asyncを標準化するが、agent内部のmemory、tool、harness、business semanticsを標準化しない。[A2A 1.0](https://a2a-protocol.org/latest/)

### 17.4 GitHub Copilot / Microsoft Agent Framework

Copilotはcustom instructions、prompt files、Skills、custom agents、subagents、hooks、MCP、pluginsを別primitiveとして提供する。対応surfaceとmaturityはIDE/cloud/CLIで異なる。Enterprise管理はagent policy/session/audit/approved MCP等を持つが、これをapplication runtimeのdurable stateと混同しない。[Copilot customization](https://docs.github.com/en/copilot/reference/customization-cheat-sheet) / [Enterprise management](https://docs.github.com/en/copilot/concepts/agents/enterprise-management)

Microsoft Agent Frameworkは、function → Agent → Harness Agent → Workflowを段階化する。Open-ended taskはAgent、planning/todo/compaction/files/memory/tool approvalを要する長時間taskはHarness Agent、順序・分岐・multi-agent coordinationを明示する場合はWorkflowである。Skillsはadvertise/load/resource/scriptのprogressive disclosureを持つ。[MAF overview](https://learn.microsoft.com/en-us/agent-framework/overview/) / [Harness Agent](https://learn.microsoft.com/en-us/agent-framework/get-started/harness) / [MAF Skills](https://learn.microsoft.com/en-us/agent-framework/agents/skills)

AutoGenはmaintenance modeで、new userはMAFへ誘導される。過去のAutoGen multi-agent tutorialをMicrosoftのcurrent production recommendationにしない。[AutoGen README](https://github.com/microsoft/autogen/blob/main/README.md)

### 17.5 AWS / Kiro / AI-DLC / AgentCore

AWS内でも三層を分ける必要がある。

- Kiro Skills/Powers: capabilityとMCP/knowledgeの発見・配布
- Kiro Specs / AI-DLC: artifactまたはlifecycle methodology
- AgentCore Runtime / Managed Agent Harness: production runtime/platform

Kiroはsteeringのalways/auto/fileMatch/manual、Skillのprogressive disclosure、Powerのdynamic activation、subagent/custom agent、Quick Spec/Feature Specを持つ。Quick Specはphase gateを省き、high-stakes/complianceではFeature Specへ上げるため、同一製品内で軽量→統制workflowの昇格が明示される。[Kiro steering](https://kiro.dev/docs/steering/) / [Kiro Skills](https://kiro.dev/docs/skills/) / [Quick Spec](https://kiro.dev/docs/specs/quick-spec/)

AI-DLCはadaptive lifecycle distribution、AgentCoreはRuntime、Memory、Gateway、Identity、Policy、Registry、Observability、Evaluations等を組み合わせるplatformである。AgentCore Runtimeは自前loopをhostし、Managed Agent Harnessはmanaged loopを使う選択である。[AI-DLC](https://github.com/awslabs/aidlc-workflows) / [AgentCore](https://docs.aws.amazon.com/bedrock-agentcore/latest/devguide/what-is-bedrock-agentcore.html) / [Harness vs Runtime](https://docs.aws.amazon.com/bedrock-agentcore/latest/devguide/harness-vs-runtime.html)

AI-DLCは会社のhigh-risk、long-running、multi-stakeholder案件で有力な比較候補だが、短期個人taskのdefaultとする独立証拠はない。Adaptive scopeがoverheadをどこまで抑えるかをTier 1/2と同条件で測る必要がある。

## 18. 複数一次資料から見える収束点と対立点

### 収束しているdesign direction

1. always-on instructionとon-demand Skill/contextを分ける。
2. workflowとagentを使い分け、既知のpathはdeterministicにする。
3. subagent/multi-agentはcontext isolation・parallelismが必要なtaskで使う。
4. model contextとdurable session/artifactを分ける。
5. sandbox、permission、identity、policyをprompt外へ置く。
6. final outputだけでなくtrajectoryとenvironment outcomeを評価する。
7. protocol/formatとbusiness semantics/authorityを分ける。
8. 会社ではcatalog、pin、owner、retention、exception、auditを追加する。

これはprovider共同標準や効果のconsensusではない。複数の現行一次資料に現れる設計方向をrootで統合した推論である。

### まだ対立・未収束な点

- open/local harnessとmanaged cloud harnessの境界
- modelが制御するpathとcode/graphが制御するpathの比率
- single-agent、planner/evaluator、multi-agentの費用対効果
- spec/artifactをdefaultにする深さ
- Skillのimplicit routingをどこまで信頼するか
- session/memory/traceのretentionとprivacy
- Agent Skills、MCP、A2A等のsemantic/authority portability
- frameworkの教育価値とceremony/context/update cost

## 19. この調査から提案する標準

### 個人用standard

- Default: Tier 1（minimal kernel + explicit/JIT Skills + interactive approval）
- Tier 2: 複数file、曖昧、長時間、再開、独立reviewが必要なtask
- Tier 3: greenfield、cross-repo、複数stakeholder、full lifecycle
- Tier 4: money、publish、credential、deletion、production等は規模に関係なく昇格
- Framework: task profileとして一つだけ選ぶ
- State: Git/artifact＋必要時ledger/checkpoint。Memoryはcache
- Completion: fresh environment evidence。commit/push/deployは別authority

### 会社用standard

- Default: 個人用Tier 1のcontext hygiene + organization control plane
- Central: provider/model、IAM、sandbox/network、data/retention、catalog/pin、audit/eval schema、exception
- Team: domain Skill、project instruction、acceptance criteria、workflow profile
- User: intent、delegation範囲、review、escalation
- Full framework: 全taskではなくrisk/task tierごと
- Lifecycle owner: 一taskにつき一つ
- Promotion: shadow/read-only/draft/PRから始め、verified outcomeでauthorityを拡大
- Regression: model、Skill、framework、policyのversion変更ごとにeval

### Repository delivery control

Agent内部workflowとは別に、repositoryからdeliveryまでのcontrolを固定する。Worktree/branchはisolation mechanismであり、approvalやverificationの代替ではない。Codex、Claude Code、GitHub coding agent、Superpowers、AI-DLCはいずれもGit/isolated workspaceを利用するsurfaceを持つが、次のauthority分離はhost/teamが所有する。[OpenAI: Git worktrees](https://learn.chatgpt.com/docs/environments/git-worktrees) / [Claude Code best practices](https://code.claude.com/docs/en/best-practices) / [GitHub coding agent](https://docs.github.com/en/copilot/concepts/agents/coding-agent/about-coding-agent) / [Superpowers](https://github.com/obra/superpowers) / [AI-DLC](https://github.com/awslabs/aidlc-workflows)

| step | 推奨control |
| --- | --- |
| workspace allocation | solo/bounded taskはcurrent checkoutも可。parallel writer、long task、remote/cloud agentはtask branch/worktreeを割り当て、base commitを記録する |
| writer ownership | 同じfile/生成物へ同時writerを置かない。component/file ownerと統合ownerを決める |
| change authority | edit、commit、push、PR、merge、deploy、live validationを別authorityとする。前段の完了から後段を推定しない |
| source/generated | 正本source、generator/version、generated pathを宣言する。generated artifactを手編集せず、再生成とcheckをevidenceにする |
| review independence | author/rootの自己申告と、spec/standard/testを照合するreviewerを分ける。高riskではauthorが自分のoverrideを承認しない |
| CI failure/retry | failing SHA、log、owner、last verified state、retry条件をcheckpointする。Intermittent証拠なしに同じrunを反復しない |
| merge order | dependency順を明示し、base更新後にtest/reviewを再実行する。conflict解決は統合ownerが一度だけ行い、workerごとに別解決しない |
| release/live | preview/dry-run、approval、external receipt、post-write reread、rollback testを分ける。Repository/PR完了をlive完了としない |

Recovery packetにはbase/head SHA、dirty/staged/committed/pushed state、CI/review result、generated check、pending authority、exact next actionを含める。

### Distribution acceptance record

Skill、plugin、framework、MCP bundleを「入るか」ではなく、次のrecordが揃うかで受け入れる。OpenAI Plugins、Claude plugin marketplace、Agent Skills、Spec Kit、AI-DLCはいずれも配布/versioning surfaceを持つが、安全なupgrade/rollbackは利用側の運用責任も残る。[OpenAI: Plugins](https://learn.chatgpt.com/docs/plugins) / [Claude plugin marketplaces](https://code.claude.com/docs/en/plugin-marketplaces) / [Agent Skills specification](https://agentskills.io/specification) / [Spec Kit upgrade](https://github.github.com/spec-kit/upgrade.html) / [AI-DLC repository layout](https://github.com/awslabs/aidlc-workflows)

| field | 記録内容 |
| --- | --- |
| identity | package/repository、publisher、license、purpose、method/runtime/distributionの分類 |
| provenance | official source、release/tag、commit/digest、署名/attestationの有無 |
| install scope | project、user、organization/adminのどこへ入り、precedenceがどう変わるか |
| payload | instructions、Skills、agents、hooks、MCP、scripts、binaries、generated files |
| permission surface | filesystem、process、network、secret、external service、approval bypass |
| dependencies | runtime、model、OS、IDE/harness、network、account/plan |
| compatibility | tested harness/model/version matrix、source/generatedの対応 |
| conflict detection | 既存instruction/Skill/hook/state/source of truthと衝突した場合のstop rule |
| update | channel、review window、migration、state/schema変換、regression eval |
| rollback/uninstall | 戻すversion、削除対象、残すartifact/state、cache/config cleanup、recovery test |
| fork drift | local patch、upstream差分、owner、rebase/update期限 |
| ownership | maintainer、security reviewer、business owner、support/EOL、exception expiry |

個人用defaultはproject-local、version/commit pin、payload review、容易なuninstall、明示的なupdateである。会社用はapproved catalog、provenance/license review、permission manifest、stable/canary channel、段階rollout、rollback owner、EOL/exception期限を追加する。Sourceから複数harness向けgenerated distributionを作る場合は、source of truthとpackage/check commandを固定し、生成物だけをforkしない。

Installation、upgrade、uninstallの実行はSection 21の停止線外であり、この表は次工程のacceptance contractである。

## 20. 限界と未解決事項

### この調査で確認できたこと

- current provider architectureと公式推奨surface
- community methodologyの設計、artifact、state、gate、escape、release
- historical methodの継承と明示的supersession
- open standardが保証する範囲と保証しない範囲
- 個人/会社の運用差を設計する材料

### 確認できなかったこと

- framework間の同一task/model/repository条件の独立head-to-head benchmark
- 各frameworkの実install時startup context、token、turn、human wait
- 自分のtask分布と会社固有processでのquality-adjusted cost
- live hook/policy/sandboxの全harness比較
- 会社固有のdata classification、法務、retention、IAM、incident要件
- provider case studyの第三者再現性と長期保守効果

したがって、ここで示したstandardは**一次資料に基づくoperating hypothesis**であり、製品/frameworkの採用決定ではない。次の実証段階はSection 7の比較実験である。

### Source volatility

基準日は2026-09-01。Managed Agents、OpenTelemetry agent conventions、NIST AI 800-2等にはbeta/draft/development要素がある。Provider featureはplan、surface、regionで差があり、adoption時にはcurrent docs、release、account/region、contractを再確認する。

## 21. 調査停止線

本レポートでは、調査、比較軸、個人/会社の推奨operating model、導入実験までを定義した。次はまだ行っていない。

- frameworkのinstallation
- `AGENTS.md` / `CLAUDE.md` / Skill catalogの再設計
- model/framework benchmarkの実行
- company policy、IAM、retentionの決定
- commit、push、activation、live rollout

これらは、対象task、risk tier、個人/会社の採用範囲を選んだ後の別工程である。
