# AIエージェントWorkflow ステップ詳細カタログ

Status: target architecture catalog / Bootstrap control-kernel source implemented / B-H, profiles, objective audit, and operational adoption remain pending

基準日: 2026-09-03（Asia/Tokyo）

## 1. この文書の役割

この文書は、[全面再構築プラン](./ai-agent-workflow-rebuild.md)から分離したLevel 2 GroupとLevel 3 Skillの設計カタログである。恒久的な設計資料として大きくなることは許容するが、実行時に全文を常時読み込ませない。Groupを実装・改訂するときに対象sectionだけを読み、実行時は生成されたGroup manifest、現在のCheckpoint、対象Skill本文だけを使う。

Superpowers、AWS AI-DLC、Spec Kit、OpenSpec、BMAD、AI Heroおよび公開dotfilesは、工程や成果物を拾うための設計資料である。これらをinstall、起動、adapter化、またはlifecycle ownerとして併用しない。新Workflowのdeterministic DAG Orchestratorだけがstate transitionを所有する。

## 2. 全Skillで省略してはいけない契約

AIにAI向け指示を書かせると、主作業は残っても小さな制約や検証が落ちやすい。各Skillは次の項目をfrontmatter、schema、本文、templateのいずれか一つの正本で必ず定義する。

| 項目 | 必須内容 | 抜けた場合の問題 |
| --- | --- | --- |
| Identity | Skill ID、version、owner、対象Group | どの契約を実行したか追跡できない |
| Trigger | 明示起動名、開始条件、使わない条件 | 誤発火、二重lifecycle |
| Objective link | run ID、objective version、小目的ID、寄与関係 | 局所完了が目的達成と誤認される |
| Inputs | path、schema、digest、作成者、state revision、鮮度 | 古い仕様やCheckpointで実行される |
| Authority | 読取範囲、書込範囲、外部操作、承認済み事項 | 権限外変更、worker間衝突 |
| Non-goals | 今回扱わない事項、隣接task | scope creep、ついでのrefactor |
| Method | 順序、分岐、loop、質問方法 | 成果物名だけ同じで品質が揺れる |
| Output | 一つの主成果物、補助成果物、保存path、schema | chatだけで終わり再開不能になる |
| Completion | 機械的check、人間承認、必要証拠 | 「作った」だけで完了になる |
| Failure | blocked条件、retry時に変えるもの、保存する部分成果 | 同じ失敗の反復、成功成果の破棄 |
| Purpose audit | `aligned / uncertain / diverged` と理由 | 目的からの逸脱を蓄積する |
| Command | schema/version、command ID/type、expected HEAD revision/digest、actor role/assignment、authority、input refs、idempotency key | stale command、role越権、重複transitionを許す |
| State transition | parent revision/digest、transaction digest、graph delta、compiler version | 履歴と現在地が分裂する |
| Next advice | Group/Skill、理由、入力、開始条件、代替、clear、workflow/state/checkpoint版、無効条件 | clear後に古い助言を実行する |
| Model advice | 原則Luna max。通常loopを止める具体的停滞時だけArbiter attemptへSol highを助言 | 気分でmodelを切り替える |
| Context Epoch | Epoch ID、Group ID、boundary reason、開始/終了revision、token status | context境界をLevel 4の階層と誤認する |
| Artifact Bundle | canonical artifactsのpath/version/digest、acceptance evidence、approved decisions、unresolved items、invalidated artifacts、next inputs | 下流が会話や本文コピーへ依存する |
| Context budget | target `200K`、通常上限`300K`、絶対上限`500K`、`exact`/`estimated`/`unavailable` | token数を捏造して境界を誤る |
| Package handoff | Task/Review package ID、artifact path/version/digest、authority、write scope、acceptance、stop、freshness、独立Epoch | Controllerへ全文tool outputを戻し、自己reviewやcontext肥大を起こす |
| Review budget | deadlineまたはtimebox、最大review round、Findingごとの最大fix attempt、残量、枯渇時のstop reason | 軽微・重複した指摘で修正loopが終わらない |

### 2.1 完了の共通定義

Skillは次のすべてを満たした時だけ`completed`にできる。

1. 主成果物が指定pathに存在しschemaを満たす。
2. 入力artifactと成果物を相互参照できる。
3. 明記されたacceptance checkが実行され、結果が保存されている。
4. 未解決事項、仮定、証拠不足が隠されていない。
5. objective versionと小目的に対する一致判定がある。
6. validated commandをcommitしHEADを更新するのはDAG Orchestratorだけである。
7. 次の助言が版付きで保存されている、またはWorkflow完了と明記されている。

作業量、文章量、Agentの自己申告だけでは完了にならない。

Context EpochはLevel 4 Skill/Groupではない。Groupが複数Epochにまたがる場合、Epoch終了時にArtifact Bundle兼Checkpointを作り、次Epochはそのbundleの`path`、`version`、`digest`をCAS検証して開始する。compactionや自然言語summaryは補助表示であり、正本にはしない。

token数に関係なく、調査→mutation、設計→実装、実装→独立review、authority/approval変更、canonical version変更ではEpochを閉じる。取得不能なtoken数は`unavailable`、推定値は`estimated`として保存する。

Workerはwork productとFindingのresolution claimを提出できるが、自分のVerdictを発行またはblocking Findingをcloseできない。Reviewerはcandidate Findingとreview Verdictを作るが実装を変更しない。別のfresh Finding Validatorはcandidate Findingの必要性を評価するが実装せず、`required`と認めたFindingだけがfix権限を持つ。domain統合が必要な場合は一つのconvergence Workerへ渡し、その成果をfresh Reviewerが検査する。ControllerとOrchestratorはdomain correctnessを裁定しない。

### 2.2 loopの共通定義

各Skillのloopは小さく閉じる。

```text
入力検証 → 一つの仮説または成果単位 → 検査 → 証拠保存
       ↖ 不合格なら原因を分類し、条件を変えた次試行
```

同じ入力、同じ方法、同じmodelでの同一retryを繰り返さない。再試行時は不足context、task size、仮説、tool、model escalationのいずれを変えたか記録する。成功済みの独立成果物は、別taskの失敗を理由に破棄しない。

実行前に`review_budget`として、deadlineまたはwall-clock timebox、最大review round、Findingごとの最大fix attemptを有限値で固定する。利用者や会社の運用規則に明示値がなければWorkflow profileが有限のdefaultを与え、未設定のままE2へ進まない。残り時間がbounded fixとfresh rereviewを収められない、最大roundへ達した、または同じFindingが最大attemptへ達した場合、新しいworkerを発行しない。DAG Orchestratorは消費済みbudget、成功済みartifact refs、未解決Finding IDs、stop reasonを一つのimmutable transaction、HEAD、Checkpointへ固定し、active leaseを閉じ、未受理dispatch commandを無効化するdurable non-dispatch terminal transitionをcommitする。required Findingが残る場合は完了扱いにせず、E10またはhuman gateへ渡す。`resume`、E1、E2はこのterminalからのdispatchを拒否する。再開できるのは、利用者または権限者がreplacement budgetのversion/valueとexpected HEADを新しく明示承認し、validated reopen commandが新revisionを作る場合だけであり、旧budgetまたはterminal transactionを上書きしない。

candidate Findingはfixへ直結させない。すべてE6の独立評価を通し、再reviewが見つけた新規candidateも同じ入口へ戻す。すでに評価済みの同一fingerprintは元Finding IDへ束ね、canonical requirement、承認済みdecision、対象diffまたは証拠が変わらない限り新しいreview roundやfix attemptを消費しない。

### 2.3 completion scopeの共通定義

`completed` は常にclaimのscopeを持つ。Skill完了は同じGroupの次工程へ進めること、Group完了はそのclear境界を閉じたこと、release scope完了は列挙したcontractを実装したことだけを意味する。いずれもWorkflow全体の目的達成を自動的には意味しない。

Workflow全体を `ready` または `completed` と報告できるのは、digest-bound implementation statusがこのcatalogの60 named Skill contractと23 software profile stepをすべてcoverageへ結び、必須surfaceとevidence付きcompletion gateを満たし、H1の目的監査とH2のoutcome決定が閉じた場合だけである。A6R source completion、A7 handoff、Bootstrap Group close、Nix build成功、preactivation readiness、generation switch readinessは、それぞれ局所claimとしてscope名を省略しない。

status/resumeはcontrol-kernelのlifecycle状態だけを返さず、同じcompletion classifierによる `release_scope`、`release_scope_status`、`overall_status`、`full_workflow_ready`、coverage、missing surface、pending gateを併記する。manifest、正本digest、物理配布が一致しない場合、未検証のproseへfallbackせず停止する。

## 3. 参照した方法論から拾う要素

| 参照元 | 採る要素 | 採らない要素 |
| --- | --- | --- |
| Superpowers | repo探索、質問を一度に一つ、複数案とtrade-off、書面設計の承認、独立検証可能なtask、TDD、spec/quality review、完了前の証拠 | initial instructionによる強制lifecycle、framework runtime、全実装workerの一律直列化 |
| AWS AI-DLC | intentのsource/assumption、reverse engineering、practice discovery、unit依存DAG、walking skeleton、並列batch後の収束、部分成功保持、operation readiness | 33 stage engine、hook、audit runtime、AI-DLC adapter |
| Spec Kit | constitution相当の安定原則、what/whyとtechnical howの分離、cross-artifact analyze、上流所有者へ差し戻す収束loop | CLI/template distributionそのもの |
| OpenSpec | exploreとchange proposalの分離、proposal/spec/design/tasks、提案状態からcanonical truthへのarchive/promotion | OpenSpec CLIを正本にすること |
| BMAD | product brief、Why/Capabilities/Constraints/Non-goals/Success signal、単一writer、implementation readiness、story preparation、status/retrospective、small-task quick path | role persona一式、BMAD runtime |
| AI Hero | 一つの良い習慣を一つのSkillへ、grill、spec、ticket、implement、TDD、review、handoff | catalogの丸ごと導入 |
| 公開dotfiles | 短いrouter、狭いowner、一つの正本からnative投影、managed manifest、unmanaged衝突停止、runtime stateを全面所有しない | 他人のpermission/model/pathをそのままコピー |

一次資料は末尾の[出典](#16-一次資料)にまとめる。

### 3.1 dotfilesから拾う内容と置き場所

人気順をそのまま正解とせず、実装が今回の目的に合う箇所だけを採る。

| dotfiles例 | 拾う内容 | この設計で使う場所 | コピーしないもの |
| --- | --- | --- | --- |
| nicknisi | 短いglobal instructionとon-demand分離 | Skill metadata、短いrouter | 個人path、広いallow list |
| liby | 狭いownerへのrouting、managed keyだけのmerge | 文書階層、config script | YubiKey等の環境固有実装 |
| haacked | 共通sourceからnative renderer、除外list、owned fileだけuninstall | provider projection、installer | model mappingの具体値 |
| TechDufus | managed manifest、unmanaged衝突停止、stale managed cleanup | Bootstrap inventory、配布 | Ansible role全体 |
| wcygan | portable templateとmachine-local trust/stateの分離、copy-if-missing | config ownership | 公開されている危険なpolicy値 |
| joshukraine | directory全体symlinkでruntime stateを巻き込む事故と修復 | `~/.codex`直書き禁止、子path所有 | permission presetの具体値 |
| jackfranklin | managed itemを追加・更新しunmanaged itemを削除しない | MCP/configの部分所有 | credentialやserver設定 |
| meain | Stop/Prompt/PostTool等のeventでsummaryとstatusを更新 | 将来のCheckpoint/status projection | 絶対path、dangerous mode |
| freekmurze | provider非依存Skillだけを明示listで投影 | catalogの限定配布 | ownershipなしの一括置換 |
| jessfraz / joshsymonds | authority、完了、review項目の具体性 | Skill共通契約のchecklist | 約16KB級のalways-on policy、個人権限 |

dotfilesから得る主な知見は、prompt内容よりも所有権、投影、衝突、復旧、runtime stateの境界である。人間UI、通知、hookはpilot後の運用改善候補とし、Workflow correctnessの前提にはしない。

## 4. Group A: Bootstrap

対象はWorkflow基盤の新設・全面置換だけである。通常Runには含めない。この再構築では、最初は本プランを人間とAIが手動で追い、walking skeleton完成後に同じRunを新Workflowへ引き継ぐ。

### A1 `bootstrap-classify-environment`

- 入力: repository root、Git状態、AI関連path候補、Nix設定、runtime path。
- 行うこと: source、generated、managed、unmanaged、app-owned、secret-bearing、ephemeralを分類する。既存AI-DLC/Superpowersの選択状態は「廃止候補」として記録するが、削除しない。
- 成果物: `bootstrap/inventory.yaml`。各pathにowner、生成元、変更状態、保持区分、削除権限、検査方法を持たせる。
- 完了: 未commitの利用者変更とruntime stateを誤って再構築対象にしていない。分類不能は`unknown`として残り、黙って推測していない。
- 停止: owner不明の既存fileへ書く必要がある。
- 参照要素: TechDufus managed manifest、haacked installer ownership、AI-DLC deterministic initialization。

### A2 `bootstrap-isolate-workspace`

- 入力: inventory、target branch/ref、現在の未commit変更。
- 行うこと: 専用worktree/branchが安全か判定する。作る場合は開始commit、path、持ち込む必要があるignored/local input、削除方法を記録する。
- 成果物: `bootstrap/workspace-plan.md` と、承認後に隔離workspace。
- 完了: source作成が`~/.codex`を直接変更せず、既存worktreeの利用者変更とwrite scopeが重ならない。
- 停止: 新branch名、開始ref、ignored file持込に利用者判断が必要。
- 参照要素: Superpowers worktree isolation、dotfilesのsource/runtime分離。

### A3 `bootstrap-design-rollback`

- 入力: inventory、workspace plan。
- 行うこと: 現行生成物の識別、backup対象、復元command、activation前後のrollbackを設計する。
- 成果物: `bootstrap/rollback.md`。
- 完了: 新配布を一度も成功させられなくても元のsource/runtimeへ戻れる。削除対象がowner manifestで限定される。
- 停止: 復元元が存在しない、credentialをbackupへ含める必要がある。

### A4 `bootstrap-initialize-run`

- 入力: 承認済みマスタープラン、暫定目的、workspace plan。
- 行うこと: run ID、objective `v001`候補、events、plan、最初のCheckpointを最小schemaで手動作成する。
- 成果物: 本再構築用Runの物理state。
- 完了: chatを読まないfresh readerが目的、現在地、次のSkill、未承認事項を説明できる。
- 停止: 目的がまだ承認可能な状態まで整理されていない。

### A5 `bootstrap-detach-legacy-owner`

- 入力: inventory、rollback、AI-DLC/Superpowersを不使用とする承認。
- 行うこと: lifecycle selection、router、generated payloadを、research/provenance文書と区別する。変更planとdiffを先に作る。
- 成果物: `bootstrap/legacy-detach-plan.md`。承認後は変更receipt。
- 完了: 実行時に旧frameworkがphaseを所有しない。調査資料の引用は残る。AI-DLC/Superpowersを新runtimeのadapterにしていない。
- 停止: 削除、Home Manager activation、commit/pushなど追加承認が必要。

A5の出口はBootstrap Epoch-01のArtifact Bundle兼Checkpointとする。bundleにはP1-P4の変更receipt、canonical source digests、acceptance evidence、approved decisions、P5/P6の未承認事項、A6のnext inputsを含める。A5完了後はclearを記録し、同じBootstrap Group内でも会話を持ち越さない。

### A6 `bootstrap-build-walking-skeleton`

- 入力: A5 Epoch-01のArtifact Bundle、最小schema、source layout、installer ownership contract。workflow version、state revision、canonical digestの一致を検証する。
- 開始条件: A5→A6が最初のContext Epoch境界として閉じられ、`clear_before_start: true`が保存されている。同じBootstrap Groupであることを理由に前Epochを再利用しない。
- 行うこと: `entry → artifact-producing Group/Skill → close-epoch → close-group → checkpoint → resume/status`の一本だけをend-to-endで作る。state revision、objective protection、bundle digest、CASを先に通す。
- 成果物: 最小helper、Skill本文、Run/Artifact Bundle schema、fixture/test、A6 acceptance receipt。
- 完了: 新規Run、同一Group内Epoch継続、Epoch/Group終了、fresh context resume、Issue/alias検索、目的保護、stale revision/version/digest拒否がfixtureで成功する。token statusと未解決事項がbundleへ保存される。
- 停止: schemaが一意な正本にならない、self-host前にlive config変更が必要。
- 参照要素: AI-DLC walking skeleton、Spec Kit artifact chain、BMAD quick path。

### A6R `bootstrap-migrate-control-kernel`

- 入力: revision 11のA6成果、Run/Artifact Bundle/worker report、採用済みArtifact DAG Control Kernel設計、G1-G8 evidence plan。
- 開始条件: A6成果をmigration sourceとしてfreezeし、A7、P5/P6、live config、stage/commit/pushが未承認のまま保たれている。
- 行うこと: 現行fieldからcontent-addressed object、initial Artifact/Task DAG、immutable transaction、atomic HEADへのmappingとconverterを作る。isolated copied fixtureでold/new reader、projection rebuild、cutover、rollbackをrehearsalする。minimal sliceは`entry → two Task packages → partial success → independent candidate review → fresh Finding validation → required-only fix claim → fresh rereview → finite stop/close → crash recovery → fresh resume/status`とする。
- 成果物: mapping、converter、fixture、command/role guard、Task/Review package、Finding/Verdict、transaction/HEAD、fault-injection receipt、old/new reader receipt、rollback receipt、G1-G8 evidence matrix。
- 完了: stale approval/role/HEAD拒否、Worker self-close拒否、candidate Findingからfixへの直行拒否、重複Finding統合、承認済みdecision尊重、軽微指摘のnon-blocking化、review/time/attempt budget枯渇時の有限停止、fresh resume、context safety、one-off/managed分離、token status、publish前/HEAD前/projection前crash、duplicate command、digest破損が証拠付きで通る。全projectionがHEADから再生成できる。
- 停止: G1-G8の未検証または失敗、converterが完成履歴を再解釈する、rollback不能、personal coreにgraph DB/daemon/company serviceが必要になる、利用者のmigration approvalがない。
- 参照要素: 4案比較のCandidate 3 DAG、Candidate 2 Thin Controller/file-only review、Candidate 4 command/integrity guard、現行三層/Context Epoch。

### A7 `bootstrap-self-host-handoff`

- 入力: A6RのG1-G8 evidence、migration/cutover/rollback receipt、手動Runの最新Checkpoint、利用者のmigration approval。
- 行うこと: 同じrun ID、objective version、state revisionを新helperで読み、次Groupを開始できるか検証する。
- 成果物: self-host handoff receiptとrollback point。
- 完了: 新Workflowが手動stateを再解釈せず読み、current stateを一意に返す。この時点以降のGroup transitionは新Workflowだけが所有する。
- 停止: A6R未完了、G1-G8未検証/失敗、手動stateとschemaが一致しない。無理な自動migrationをしない。

## 5. Group B: 目的理解と承認

このGroupは一つの密な思考連鎖として扱い、目的の最終確定をSub Agentへ分断しない。独立調査だけは補助workerへ渡せる。

### B1 `entry`

- 入力: 利用者が達成したい目的、外部Issue/文書、既知の期限と権限。
- 行うこと: 暫定目的を原文のまま保存し、依頼種別、risk、規模、既存Runとの重複を判定する。実装や解決案へ進まない。
- 成果物: `intent/intake.md`、Run index entry、質問候補。
- 完了: 原文、解釈、未確認仮定が分離され、次に聞く一問がある。
- 停止: 同じIssueのactive Runがあり、resumeか新Runかで意味が変わる。

### B2 `discover-context`

- 入力: intake、repositoryや業務sourceへの読取権限。
- 行うこと: 利用者へ聞く前に、locally discoverableな現状、用語、既存決定、制約、関連成果物を調べる。事実にはsourceを付ける。
- 成果物: `intent/context.md`。
- 完了: confirmed fact、inference、assumption、unknownが分離されている。
- 停止: 必要なsourceへのアクセスがなく、推測で目的が変わり得る。

### B3 `classify-scope`

- 入力: intake、context。
- 行うこと: quick/bounded/architectural、exploration/change、個人/会社、可逆/不可逆を分類し、必要なGroup深度を提案する。
- 成果物: `intent/scope-classification.yaml`。
- 完了: 省略するGroupにも理由があり、riskが高いのにquick pathへ落としていない。
- 参照要素: Superpowers spike/bounded/architectural、AI-DLC adaptive scope、BMAD quick path。

### B4 `grill-purpose`

- 入力: context、scope classification。
- 行うこと: 一度に一問だけ聞き、背景、困りごと、期待する変化、stakeholder、期限、制約、非目的、失敗時の影響を整理する。回答可能な質問を利用者へ投げない。
- 成果物: `intent/grill-log.md` と目的候補。
- 完了: 目的候補を比較するのに必要なcritical unknownが解消された、または未解消理由が明記された。
- 停止: 利用者回答なしでは目的候補がmaterially変わる。
- 参照要素: Superpowers brainstorming、AI Hero grill、AI-DLC intent elicitation。

### B5 `propose-true-purpose`

- 入力: grill log、context。
- 行うこと: 現案を含む2〜3候補を作り、理由、期待変化、失うもの、scope/費用/権限への影響を比較する。AIの推奨を明記する。
- 成果物: `intent/objective-options.md`。
- 完了: 各候補が区別可能で、単なる言い換えになっていない。
- 停止: AIが利用者承認なしに一案を確定しようとしている。

### B6 `assess-feasibility-and-constraints`

- 入力: 目的候補、context。
- 行うこと: 技術、時間、費用、データ、権限、組織、complianceの制約と依存を調べる。調査が独立なら並列化し、一つのconvergence Workerが統合し、fresh Reviewerが検査する。
- 成果物: `intent/feasibility.md`。
- 完了: hard constraint、soft constraint、assumption、open questionが分類される。

### B7 `approve-objective`

- 入力: objective options、feasibility、利用者回答。
- 行うこと: 目的、理由、観測可能な変化、非目的、制約、stakeholder、再度開く条件を一つの候補へまとめ、利用者へ承認を求める。
- 成果物: 承認後だけ`objectives/vNNN.md`とapproval event。
- 完了: 承認者、日時、元候補、変更影響が記録されcurrent pointerが進む。
- 停止: 暗黙の了承、AIの推定、task着手を承認の代わりにしない。

## 6. Group C: 小目的、計測、現在値（Epoch slice C1-C2 / C3-C6）

Group Cは二つのEpoch sliceへ分ける。C1-C2はoutcomeとDAGを閉じ、C3-C6はそのbundleをpath/version/digestで参照する。C1-C2終了時はclearするが、Group C全体はC3-C6が終わるまで継続する。

### C1 `decompose-outcomes`

- 入力: 承認済みobjective version。
- 行うこと: 目的を実装taskではなく、達成状態としての小目的へ分解する。各小目的に必要理由、依存、目的への寄与、除外条件を付ける。
- 成果物: `outcomes/outcome-map.yaml`。
- 完了: すべての小目的を満たしても目的が満たされない穴、または目的に寄与しない小目的がない。
- 停止: 分解がsolutionや作業一覧へ早期固定される。

### C2 `build-outcome-dependency-graph`

- 入力: outcome map。
- 行うこと: prerequisite、並列可能、相互依存、convergence pointをDAGとして表す。
- 成果物: `outcomes/dependencies.yaml`。
- 完了: cycleが検出・解消され、並列batch後の統合ownerが決まる。
- 参照要素: AI-DLC unit dependency DAG、BMAD epics/story map。

#### Epoch C-01出口

C1-C2のArtifact Bundleはoutcome map、dependency DAG、acceptance evidence、approved decisions、unresolved/invalidated items、C3-C6へのnext inputsを持つ。bundle digestが変わった場合、C3以降は古い助言を実行せずC1へ戻す。

#### Epoch C-02（C3-C6）

### C3 `design-measurement`

- 入力: outcome map、利用可能なdata source。
- 行うこと: 各小目的について測れるか、測る価値があるかを判定する。direct metric、proxy、qualitative rubric、合否条件、未計測を選ぶ。
- 成果物: `measurement/measurement-plan.md`。
- 完了: metricが小目的を代理する理由と、gaming/副作用が明記される。測れないものへ無理な数値を付けない。
- 停止: 意味のない数値しか作れず、利用者判断が必要。

### C4 `define-targets`

- 入力: measurement plan、期限・許容範囲。
- 行うこと: 上位の目標値、観測期間、計算法、source、頻度、悪化させないguard metricを定める。
- 成果物: `measurement/targets.yaml`。
- 完了: workerが別々の成功定義を作らず参照できる。

### C5 `capture-baseline`

- 入力: measurement plan、source access、観測条件。
- 行うこと: 変更前の値を同じ計算法で取得し、欠損、期間、sampling、環境差を記録する。
- 成果物: `measurement/baseline.<ext>` とreceipt。
- 完了: 後の比較が同条件で再実行可能。取得不能は0として扱わない。
- 停止: sourceが古い、取得権限がない、計測自体が対象を変える。

### C6 `validate-outcome-system`

- 入力: outcome DAG、targets、baseline。
- 行うこと: 目的→小目的→指標→baseline/targetのtraceabilityと矛盾を検査する。問題は所有する上流成果物へ差し戻す。
- 成果物: `measurement/validation.md`。
- 完了: orphan、重複、矛盾、未所有の修正がない。
- 参照要素: Spec Kit analyze/converge、BMAD implementation readinessの前段。

## 7. Group D: 解決方針、仕様、実行計画（Epoch slice D1-D4 / D5-D7 / D8-D12）

Group Dは三つのEpoch sliceへ分ける。D1-D4は調査とcanonical spec、D5-D7は選択肢・設計・contracts、D8-D12はtask/DAG/verification/readinessを扱う。各sliceの出口でbundleとclearを記録し、下流は上流本文をコピーせず参照する。

### D1 `select-workflow-profile`

- 入力: scope classification、outcome system、対象domain。
- 行うこと: feature、bug fix、improvement、research、decision、業務改善等のprofileと必須/任意Groupを選ぶ。
- 成果物: 版付きWorkflow manifest。
- 完了: 一つのlifecycle ownerだけが選ばれ、AI-DLC/Superpowersは候補にない。

### D2 `reverse-engineer-current-system`

- 入力: objective/outcome、repositoryまたは業務process。
- 行うこと: entrypoint、domain terms、data/control flow、外部contract、テスト、既存制約、過去決定を調べる。独立surfaceは並列調査できる。
- 成果物: `planning/current-system.md`。
- 完了: 変更対象だけでなく隣接contractと検証surfaceが分かる。

### D3 `discover-practices`

- 入力: current system、repository guidance、CI、team規約。
- 行うこと: 実際のcoding/test/review/release規約を複数sourceから調べ、文書と現実の差を記録する。
- 成果物: `planning/practices.md`。
- 完了: downstream Skillが従う一つのglobal constraints listがある。
- 参照要素: AI-DLC practice discovery、公開dotfilesの狭いowner routing。

### D4 `specify-what-and-why`

- 入力: objective、outcomes、current system。
- 行うこと: behavior、利用者scenario、capability、constraint、non-goal、edge case、error behavior、acceptanceを記述し、技術実装案を混ぜない。
- 成果物: canonical `spec.md`。
- 完了: 曖昧語、placeholder、未所有questionがなく、利用者が書面をreviewできる。
- 参照要素: Spec Kit specify、BMAD SPEC kernel、Superpowers written design review。

#### Epoch D-01出口

D1-D4のbundleはworkflow profile、current system/practices、canonical specのpath/version/digest、acceptance evidence、approved decisions、unresolved/invalidated items、D5-D7へのnext inputsを持つ。canonical spec versionが変わればD5を同じEpochで続けない。

#### Epoch D-02（D5-D7）

### D5 `explore-options`

- 入力: spec、current system、constraints。
- 行うこと: 2〜3案、現状維持を比較し、複雑性、可逆性、risk、運用費、migrationを示す。探索はまだ実装承認ではない。
- 成果物: `planning/options.md`。
- 完了: 推奨案と反証条件がある。
- 参照要素: Superpowers approach comparison、OpenSpec explore。

### D6 `design-solution`

- 入力: 承認されたoption、spec。
- 行うこと: architecture、component責務、interface、data/control flow、error handling、compatibility、migration、observability、security、test seamを定義する。
- 成果物: `design.md`。
- 完了: downstreamが上流仕様を再解釈せず実装でき、設計判断と未決が分離される。

### D7 `design-contracts`

- 入力: design、既存external contract。
- 行うこと: consumes/produces、schema、version、failure、idempotency、backward compatibilityを具体化する。
- 成果物: contract/schema/test fixtures。
- 完了: 並列task間の境界が事前に固定され、同じ可変fileを共有しない。

#### Epoch D-02出口

D5-D7のbundleは選択理由、design、contracts/schema、互換性、acceptance evidence、approved decisions、unresolved/invalidated items、D8-D12へのnext inputsを持つ。authorityまたはapprovalが変わった場合はD8へそのまま進まずEpochを閉じる。

#### Epoch D-03（D8-D12）

### D8 `decompose-tasks`

- 入力: spec、design、contracts、outcome targets。
- 行うこと: reviewerが一つを承認し隣を却下できる最小単位へ分ける。各taskにexact files、interface、入力、出力、task固有合格条件、上位小目的への寄与を付ける。
- 成果物: `planning/tasks.yaml`。
- 完了: 各taskが独立検証可能で、単なる「componentを作る」のような曖昧項目がない。
- 参照要素: Superpowers writing plans、AI Hero tickets、BMAD story preparation。

### D9 `sequence-and-parallelize`

- 入力: tasks、contracts、outcome DAG。
- 行うこと: task依存DAG、non-overlapping write set、parallel batch、batch convergence、密結合taskを定める。
- 成果物: `planning/execution-dag.yaml`。
- 完了: siblingだけが並列化され、domain収束が必要な箇所にはconvergence Workerとfresh Reviewerが定義される。
- 停止: 判断の因果鎖やwrite scopeを安全に分けられない場合は直列化する。

### D10 `prepare-worker-briefs`

- 入力: task、spec/design参照、global constraints。
- 行うこと: workerに全planを読ませず、目的参照、task、interface、write scope、check、stop、report pathだけを切り出す。
- 成果物: `briefs/<task-id>.md`。
- 完了: fresh workerが追加探索なしに開始できるか、または必要な探索範囲が限定される。

### D11 `plan-verification-and-recovery`

- 入力: tasks、risk、delivery surface。
- 行うこと: test level、independent review、Finding validation、E2E、dry-run、rollback、post-check、activation/commit/push gateを定義する。各taskにdeadlineまたはwall-clock timebox、最大review round、Findingごとの最大fix attemptを有限値で割り当てる。
- 成果物: `planning/verification-plan.md`、`recovery-plan.md`。
- 完了: 失敗時にどこまで戻すか、何を保持するか、誰の承認が必要か、どの予算枯渇で新規workerを止めるかが明記される。

### D12 `implementation-readiness-review`

- 入力: spec、design、contracts、tasks、DAG、verification/recovery。
- 行うこと: cross-artifactの矛盾、placeholder、未解決critical decision、目的traceability、権限を検査する。修正は所有する上流Skillへ戻す。
- 成果物: `planning/readiness-review.md`。
- 完了: `ready`、`ready_with_accepted_risks`、`not_ready`のいずれかと証拠がある。人間承認が必要なprofileでは書面承認後にだけ実行へ進む。

#### Epoch D-03出口

D8-D12のbundleはtask list、execution DAG、worker briefs、verification/recovery、readiness判定、acceptance evidence、approved decisions、unresolved/invalidated itemsを持つ。E1が参照するnext inputsはすべてpath/version/digestで固定する。

## 8. Software profile固有ステップ

共通Groupをコピーせず、次の差分をWorkflow manifestへ追加する。

### 8.1 機能追加

| Step | 必ず扱う内容 | 主成果物・完了証拠 |
| --- | --- | --- |
| F1 user behavior | 誰が、何をきっかけに、何ができるようになるか | scenarioとobservable outcome |
| F2 acceptance examples | happy path、boundary、permission、error、cancel/retry | example table、未決なし |
| F3 UX/API contract | 入出力、state transition、compatibility、accessibility | contractとfixture |
| F4 architecture fit | 既存moduleとの責務、new seam、migration | design approval |
| F5 vertical slice | end-to-end最小経路、仮実装を残す条件 | walking skeleton test |
| F6 behavior TDD | 一度に一behaviorをred→green→refactor | failing-before/passing-after evidence |
| F7 integration | dependency、error、migration、observability | integration tests |
| F8 acceptance/E2E | 利用者scenarioを同等環境で通す | 必須E2E receipt |

### 8.2 Bug fix

| Step | 必ず扱う内容 | 主成果物・完了証拠 |
| --- | --- | --- |
| BGF1 symptom | 期待値、実値、環境、頻度、impact、最初に観測した版 | symptom record |
| BGF2 reproduce | 最小再現、non-repro条件、flaky判定 | 修正前に失敗するcommand/test |
| BGF3 evidence | log、trace、state、recent change、境界値 | evidence inventory |
| BGF4 hypotheses | 反証可能な候補、優先順、各検査 | hypothesis ledger |
| BGF5 root cause | 因果鎖、なぜ既存guard/testが防げなかったか | root-cause explanationと証拠 |
| BGF6 fix options | 最小修正、回避策、広い修正、risk | approved fix choice |
| BGF7 regression | 原因を捉え修正前fail/修正後passするtest | regression test |
| BGF8 verify impact | 元症状、隣接contract、性能/安全、実環境相当 | verification receipt |

根本原因の一つの因果鎖は分割しない。独立したlog調査や仮説検査は並列化できるが、一つのconvergence Workerが原因説明へ収束させ、fresh Reviewerが検査する。

### 8.3 機能・業務改善

| Step | 必ず扱う内容 | 主成果物・完了証拠 |
| --- | --- | --- |
| I1 baseline | 同条件の現在値、欠損、ばらつき | baseline receipt |
| I2 hypothesis | 何を変えると、なぜどの指標が動くか | falsifiable hypothesis |
| I3 guardrails | 悪化させない指標、費用、品質、利用者影響 | guard metric |
| I4 change design | 最小変更、比較可能性、rollback | approved change |
| I5 execute | 変更とそのreceipt | scoped diff/action log |
| I6 compare | 同じmethod、期間、environmentで前後比較 | result comparison |
| I7 decide | adopt、iterate、rollback、insufficient evidence | decision candidateまたは承認済みdecision |

数値化できない改善はqualitative rubricや合否条件を使える。取得不能値を0にせず、比較不能なら結論を保留する。

### 8.4 後から追加するprofile

性能、refactoring、security、migration/releaseはpilot後に追加する。追加時も共通Groupをforkせず、固有のrisk/証拠だけを定義する。

- 性能: representative workload、warm-up、variance、resource、同条件benchmark。
- Refactoring: 外部動作不変、change frictionのbaseline、構造改善の検証。
- Security: threat model、attack precondition、権限境界、悪用不能証拠、秘密の非出力。
- Migration/release: compatibility matrix、dry-run、backup、rollback test、post-deploy readback。

## 9. Group E: 実行とSub Agent収束

EはArtifact依存からcompileされたready frontierのbatchごとにContext Epochを閉じる。各Worker/Reviewer/convergence/Arbiter attemptはimmutable packageとfresh Epochを持ち、Thin Controllerへ全文tool outputを返さない。DAG Orchestratorだけがvalidated commandでHEADを進める。

### E1 `execution-preflight`

- 入力: readiness approval、Artifact/Task DAG、current HEAD、workspace、Git状態。
- 行うこと: expected HEAD revision/digest、objective/workflow/graph version、write lease、依存artifact、authority、open blocking Finding、context budget、有限のreview/time/attempt budget、context status、rollback、durable non-dispatch terminalの有無を再確認する。
- 成果物: preflight receipt。
- 完了: plan作成後のdriftがなく、あれば再計画済み。review budgetが未設定、不足、またはbudget terminalが新しい明示budget approvalとexpected HEADへbindされたreopen commandで解除されていない場合はdispatchしない。

### E2 `dispatch-task`

- 入力: ready attemptと依存artifact。
- 行うこと: Luna maxを既定に、Task package、独立Epoch ID、input refs、authority、write scope、acceptance、stop、freshness、output pathを固定してWorkerへ渡す。密結合taskは一つのpackageで同じWorkerが連続して扱う。current HEADがdurable non-dispatch terminalなら、validated reopen commandなしにpackageを発行しない。
- 成果物: immutable Task packageとdispatch receipt。
- 完了: workerが`DONE / DONE_WITH_CONCERNS / NEEDS_CONTEXT / BLOCKED`のいずれかで返せる。

### E3 `execute-small-loop`

- 入力: Task package。
- 行うこと: 一つのbehavior/仮説/成果単位を実装・検査し、次の小loopへ進む。仕様が固い場合はE2E通過までを一workerの遠めのゴールにできる。
- 成果物: scoped change/artifactとtest evidence。
- 完了: packageのtask固有合格条件を満たし、変更範囲、result object、evidence、未解決事項が所定pathへ保存される。WorkerはHEADやVerdictを更新しない。
- 停止: spec変更、write scope越境、目的変更、外部critical actionが必要。

### E4 `review-task-spec`

- 入力: fresh Review package、spec、小目的、candidate/diff、acceptance evidence。
- 行うこと: Workerと独立したReviewerが、taskが上流specと小目的を満たすか確認する。
- 成果物: stable candidate ID/fingerprint、Background、As-Is、canonical To-Be、Gap、requirement ref、evidence、severity候補、blocking候補、owner候補を持つcandidate Finding setとspec review Verdict attempt。
- 完了: candidate Findingごとにsource requirementと証拠があり、Reviewerのinput refsとactor roleを検証できる。reviewer自身はfix authorityまたは最終dispositionを発行しない。

### E5 `review-task-quality`

- 入力: fresh Review package、practices、design、artifact/diff、test。
- 行うこと: correctness、maintainability、test quality、security、scopeを独立確認する。
- 成果物: E4と同じ構造を持つcandidate Finding setとquality review Verdict attempt。
- 完了: spec reviewと混ぜず、blocking/non-blocking候補を分類する。単なる好みや改善案を必須要件として扱わない。

### E6 `validate-review-findings`

- 入力: E4/E5のcandidate Finding set、canonical spec/design/non-goals、承認済みdecision/accepted risk、過去Finding index、対象diff、review budget。
- 行うこと: 元Reviewerおよび実装Workerと異なるfresh Finding Validatorが、各candidateについてBackground、As-Is、canonical To-Be、Gap、evidenceを照合し、`required / defer / reject / needs-user`へ分類する。exactで事前承認済みの機械predicateは同じ第二段階をdeterministicに実行できるが、predicate ID/version/digest、pre-approval/authority ref、入力Review packageとcandidate digest、実行結果をimmutable receiptへbindする。predicateは解釈、適用資格の自己申告、fix authority発行を行わず、DAG Orchestratorがreceipt、approval、expected HEADを検証する。解釈を要するcandidateは独立Validatorを省略しない。
- 判定規則: 現taskのmandatory requirement/acceptance、correctness、security、integrity、regressionを満たすため今直す必要があるものだけを`required`とする。妥当だが現taskの受入条件や観測結果を変えない軽微な改善は`defer`、根拠のないstyle preference、scope外、既解決、superseded、重複は`reject`とする。理由付きで承認済みの選択は、新しい証拠がmandatory requirement、安全性、互換性との衝突を示さない限り再修正しない。spec/scope/risk/budgetの選択を変えるものは`needs-user`とする。
- 成果物: validation attempt ID/Epoch、元Review package ID/digestとReviewer actor/Epoch、Validator actor/Epochまたはpredicate ID/version/digest、pre-approval/authority ref、predicate execution receipt、independence/provenance check、candidate ID、canonical Finding ID、fingerprint、disposition、reason、materiality、requirement/decision ref、`duplicate_of`、許可するfix scope、budget残量を持つFinding validation setとadmissibility Verdict。
- 完了: 全candidateが一度だけ分類され、同一fingerprintは元Finding IDへ統合される。`required`だけがopen Findingとfix authorityの候補になり、`defer/reject`は完了をblockせず、`needs-user`は該当枝を停止する。Validatorは実装せず、required Findingの修正後closeも行わない。

### E7 `fix-and-rereview`

- 入力: E6で`required`と認められたopen Finding、元Task package、review/time/attempt budget。
- 行うこと: 残りbudget内で該当枝だけへfix Task packageを作る。Workerはresolution claimと証拠を出し、別のfresh Review attemptまたは事前承認された機械predicateが同じFinding IDと必要な回帰範囲だけを検査する。rereview中の新規candidateはfixへ直行させずE6へ戻す。
- 成果物: fix result、resolution claim、fresh Verdict attempt。
- 完了: blocking FindingがReviewerにより`resolved/superseded`へ遷移、またはaccepted-risk/human/Arbiter待ちになる。Worker self-closeは拒否される。budget枯渇時は新規fixを発行せず、部分成功、未解決Finding、stop reasonをdurable non-dispatch terminal transitionへ保存する。

### E8 `converge-parallel-batch`

- 入力: sibling result objects、Findings/Verdicts、共有contract。
- 行うこと: 機械的に結合できる場合はDAG Orchestratorが次frontierをcompileする。domain判断が必要なら、一つのconvergence Workerへpackageを渡して矛盾、重複、gap、目的ずれ、integration riskを統合し、その成果をfresh Reviewerへ渡す。
- 成果物: convergence result、fresh review、batch Artifact Bundle。
- 完了: 各部分の成功を保持しつつ、次batchが依存できる一つのreview済みcanonical resultがある。Controller/Orchestratorは統合判断を作らない。
- 停止: 暗黙知の分断を回収できず、密なtaskとしてやり直す必要がある。

E8のbatch終了時にEpochを閉じ、HEADに束縛されたbatch Artifact Bundleを作る。bundleは各result/review/validation objectのpath/version/digest、Finding、検証、未解決事項、budget消費、次frontierを含み、Controllerは全文ログを取り込まない。

### E9 `verify-whole-change`

- 入力: 全batch、spec、verification plan。
- 行うこと: 実装workerとは別のfresh context/独立reviewerまたはdeterministic checkで、task単位でなく全体のintegration、E2E、regression、objective contributionを確認する。新しいcandidate FindingはE6へ渡し、直接fixしない。E9自身を新Epochとして閉じる。
- 成果物: whole-change verification。
- 完了: 実行command、期待、実結果、環境、未検証riskが保存される。

### E10 `arbitrate-exception`

- 入力: valid review/validation conflict、同一Finding反復、review/time/attempt budget枯渇、spec defect、failed decomposition、root-cause/whole-design conflictのいずれかと、関連artifact refs。
- 行うこと: 通常loopを止め、一つのArbiter packageを作る。既定はLuna maxとし、通常モデルで実際に解決不能な問題だけこのattemptをSol highへ上げる。Arbiterは原因分類、選択肢、推奨、必要なhuman decisionを返す。
- 成果物: Arbiter recommendationまたはhuman approval request。
- 完了: 次の機械的transition、上流差し戻し、human gate、有限停止のいずれかが証拠付きで決まる。budget terminalから新attemptへ戻すtransitionは発行せず、利用者または権限者によるreplacement budgetとexpected HEADの新しい明示承認を要求する。
- 停止: objective、scope、authority、material riskを承認なしに変える必要がある。Arbiterはprotected approvalを発行しない。

## 10. 共通closure protocol（旧Group F: Group終了、clear、resume）

FはLevel 2 Groupではない。以下のF1-F8は全Groupが共有するclosure protocolのSkill群であり、Workflowごとにコピーしない。`close-epoch`は同一Group内の文脈境界、`close-group`はLevel 2 Group終了のclear境界を担当する。

すべての出口はArtifact Bundle兼Checkpointとして、canonical artifacts、digest、acceptance evidence、approved decisions、unresolved items、invalidated artifacts、next inputsを保存する。token budgetはtarget `200K`、通常上限`300K`、絶対上限`500K`とし、exact/estimated/unavailableを明示する。

### F1 `audit-group-purpose`

- 入力: current objective、小目的、Group成果。
- 行うこと: `aligned / uncertain / diverged`を判定し、完成量ではなく目的への寄与を見る。
- 成果物: alignment report。
- 完了: 判定、根拠、目的version、影響する小目的が記録される。
- 停止: divergedの場合、目的を自動変更せず戻す・再提案・停止を提示する。

`close-epoch`の最初の検査としても使う。EpochはLevel 4ではなくruntime metadataであり、同じGroupに複数存在できる。

### F2 `collect-group-artifacts`

- 入力: transaction/event projection、result/review objects、artifact refs、Findings/Verdicts。
- 行うこと: 成果、失敗、未検証、部分成功、外部receiptを収集し、canonical artifactのpath/version/digestを固定したArtifact Bundleを作る。
- 成果物: Artifact Bundle兼Checkpoint manifest。
- 完了: Groupが生成・参照した全成果物、証拠、未検証、部分成功に所在とdigestがある。

### F3 `extract-decision-candidates`

- 入力: Group内events/reports/artifacts。
- 行うこと: 将来の設計・運用に影響する判断だけを候補化し、観察/仮定/提案/承認済みを分類する。
- 成果物: `decision-candidates/*.md`。
- 完了: 未承認提案を決定と書かない。

### F4 `replan-future`

- 入力: current plan、Group結果、依存DAG。
- 行うこと: 完了履歴を変えず、新graph revisionでfuture frontierだけを置き換えるcommandを作り、理由とimpactをtransactionへ含める。
- 成果物: 新HEAD revisionのfuture plan projection。
- 完了: 完了履歴と承認済み目的が不変で、future差分と変更理由が検証される。
- 停止: 目的、scope、risk、予算、ownerを変える場合は利用者承認へ戻す。

下流の不足はこのSkillで補完せず、正本を所有する上流Skillへ差し戻す。future planはreference path/version/digestだけを更新する。

### F5 `advise-next-group`

- 入力: future plan、未解決、開始条件。
- 行うこと: 次Group、Skill列、必要入力、代替、モデル、clear有無、workflow/state/checkpoint版、無効条件を提案する。
- 成果物: next advice。
- 完了: 古いstateでは実行されないbindingがある。

### F6 `write-checkpoint`

- 入力: F1〜F5。
- 行うこと: fresh contextに必要な最小情報だけをArtifact Bundle兼Checkpoint objectへまとめ、close commandをDAG Orchestratorへ渡し、commit後のHEADからstatus/event projectionを再生成する。summaryやcompactionは正本にしない。
- 成果物: immutable Artifact Bundle兼Checkpoint、transaction receipt、status/event projection。
- 完了: 元chatなしのreaderが次の一手と停止理由を答えられる。

### F7 `clear-boundary`

- 入力: validated checkpoint。
- 行うこと: EpochまたはGroupを閉じるversion付きcommandをrole/authority/expected HEADとともに検証し、atomic HEADを確定してから、利用者へclearと次Skillの正確な起動方法を案内する。自動実行しない。
- 成果物: clear guidanceと、Checkpointへ保存された再開command。
- 完了: Group内の未保存contextがなく、次GroupはCheckpointから開始できる。

### F8 `resume`

- 入力: Issue/URL/alias/run ID、HEADから生成された最新index。
- 行うこと: Runを検索し、HEAD parent/digest chain、objective version、workflow/graph version、state revision、Context Epoch、Checkpoint、Artifact Bundle digestの鮮度を検証する。projectionがstaleならHEADから再生成し、canonical inputがstale/corruptなら拒否する。
- 成果物: resume briefまたは`not found/stale/ambiguous`。
- 完了: 「issue xxはどこまで？」へ、目的、完了、現在地、未検証、次Skillを物理stateだけで答える。

## 11. Group G: 判断の集約と永続化

`consolidate-decisions`は有効なので採用する。理由は、Groupごとのlocal stateが再開には有用でも、長期設計の正本としては細かすぎ、後から重要判断が埋もれるためである。ただし、承認機構なしでは危険なので次の分離を守る。

### G1 `scan-decision-evidence`

- 入力: 対象Run/期間/Group、events、checkpoints、reports、artifacts、既存ADR。
- 行うこと: 判断を示すsourceを列挙し、引用ではなくpath/event IDで参照する。
- 成果物: source inventory。
- 完了: 対象scope内の判断sourceを検索した範囲と、読めなかった範囲が明記される。

### G2 `classify-decision-status`

- 入力: source inventory。
- 行うこと: observation、assumption、proposal、temporary ruling、approved decision、superseding candidateへ分類する。
- 成果物: classification table。
- 完了: 全candidateにstatus、根拠event、権限者、確信度がある。
- 停止: 承認者やauthorityが不明なものをapprovedにしない。

### G3 `deduplicate-and-link`

- 入力: classification、既存ADR/設計文書。
- 行うこと: 同一判断、矛盾、時間順、supersedes/superseded-byを整理する。
- 成果物: decision graph。
- 完了: 各candidateが新規、重複、競合、supersedingのいずれかに分類される。

### G4 `draft-durable-records`

- 入力: decision graph、対象repositoryの規約。
- 行うこと: context、decision、drivers、options、rationale/evidence、rejected alternatives、consequences、owner、revisit trigger、source refsを持つ候補を作る。
- 成果物: preview用decision candidates。
- 完了: 一つのrecordに複数の独立決定を詰め込まず、現在規則と履歴を区別する。

### G5 `approve-promotion`

- 入力: candidates、昇格先、差分。
- 行うこと: 利用者または権限者が採用、修正、保留、却下を決める。
- 成果物: approval event。
- 完了: 各candidateの採用、修正、保留、却下と権限者が記録される。
- 停止: AIによる自動承認は禁止。

### G6 `promote-and-backlink`

- 入力: approved candidate。
- 行うこと: `docs/decisions/`、ADR、設計正本等へ書く。元eventは変更せず、新しいpromotion transactionへ元event/candidate refと`promoted_to`を保存してbacklink projectionを再生成する。古いADRは改変せず新recordでsupersedeする。
- 成果物: durable record、backlink、validation。
- 完了: repository規約と構文checkを通り、local sourceからもdurable recordからも相互追跡できる。

## 12. Group H: Workflow完了と目的監査

### H1 `audit-objective`

- 入力: 承認済みobjective、全小目的、targets、成果、未検証。
- 行うこと: delivery完了、trajectory改善、目的状態の実現を別々に評価する。
- 成果物: final objective audit。
- 完了: 全Skill完了を目的達成とみなさず、各claimにevidenceがある。

### H2 `decide-run-outcome`

- 入力: objective audit、risk、利用者判断。
- 行うこと: achieved、partially achieved、not achieved、superseded、abandonedを決め、理由と残taskを記録する。
- 成果物: run outcome event。
- 完了: outcome、目的監査への参照、未達理由、残task、承認者が記録される。

### H3 `archive-or-continue`

- 入力: outcome、decision candidates、retention policy。
- 行うこと: 判断集約、local artifact retention、cleanup候補、次Runへのhandoffを作る。
- 成果物: archive manifestまたは新Run link。
- 完了: retention、cleanup候補、判断候補、rollback artifact、次Runの所在が検証される。
- 停止: 未昇格の重要decisionやrollbackに必要なartifactが残る。

## 13. 会社用へ追加するGroup

個人pilotで共通基盤を検証した後に追加する。promptだけへ埋めずdeterministic policyと外部systemへ分離する。

| Group | 追加する内容 | 完了証拠 |
| --- | --- | --- |
| classify-data | data class、retention、region、secret/PII | policy decisionとscan |
| resolve-identity | human/agent/service identity、delegation、expiry | identity receipt |
| authorize-tools | filesystem、network、external action、production | effective policy readback |
| approve-catalog | Skill/framework source、version、license、review、owner | approved manifest |
| evaluate-change | golden task、quality、cost、latency、failure、rollback | comparative eval |
| release-workflow | staged rollout、canary、rollback、communication | release receipt |
| audit-operation | event completeness、bypass、incident、retention | audit report |

## 14. Skill本文をAIに生成させる時のチェックリスト

生成したSkillを採用する前に、別のreview passで次を確認する。

- descriptionが一文でtriggerと非triggerを区別できる。
- 一つの主成果物に絞られ、隣接Skillの責務をコピーしていない。
- input pathだけでなくversion、revision、freshnessがある。
- objective本文を複製せずapproved versionを参照する。
- 利用者へ聞く前にlocal contextから解ける項目を明記する。
- 質問は一度に一問で、回答がmaterially変える判断に限定される。
- exact write scopeと禁止範囲がある。
- happy pathだけでなくblocked、needs decision、partial successがある。
- 「確認する」ではなくcommand、schema、reviewer、expected resultがある。
- retry時に何を変えるかがある。
- Sub Agentを使う条件と、使わない密結合条件がある。
- workerへ全planを読ませず、必要なbriefだけを渡す。
- approvalとAIの提案が分離されている。
- next adviceに版bindingと無効条件がある。
- clear前にCheckpointだけでfresh contextが再開できる。
- 重要判断のcandidate化と永続化の承認点がある。
- existing file、unmanaged symlink、user change、secretを保護する。
- modelはLuna maxを既定とし、Sol highは実際の停滞根拠を持つArbiter attemptだけに限定される。
- 小さいSkill本文で足りない詳細はreferenceへ分離し、常時metadataへ詰め込まない。

## 15. 実装優先順

1. Bootstrap A1〜A4を手動実行し、現状と再開点を保護する。
2. A5のlegacy owner切離し差分とrollbackを提示する。承認待ちでも、隔離worktree内のsource作成は継続できるがlive activationとself-hostは行わない。
3. A5 EpochをArtifact Bundle兼Checkpointへ閉じ、A5→A6を最初のclear境界としてmigrationを記録する。
4. A6の既存walking skeleton成果をmigration sourceとしてfreezeする。
5. A6Rでimmutable objects/transactions/HEAD、command/role guard、Task/Review/Evaluation package、stable candidate Finding、独立Finding validation、required-only fix/rereview、有限停止、crash recovery、fresh resumeのminimal sliceを作る。isolated fixtureでconverter、old/new reader、cutover、rollbackとG1-G8を検証する。
6. G1-G8 evidenceと利用者のmigration approval後だけ、A7でこの再構築Runを自己hostへ引き継ぐ。
7. 目的Group Bと小目的・計測Group Cを作る。CはC1-C2とC3-C6のEpoch sliceへ分ける。
8. DのplanningをD1-D4/D5-D7/D8-D12のEpoch sliceへ分け、共通closure protocolを使う。
9. Eのready frontier batchごとにEpochを閉じ、E6を独立Finding validation、E9をfresh context/独立whole-change review、E10を例外Arbiter routeとして実装する。
10. Gの判断集約をpreview-onlyで作り、承認後promotionを追加する。
11. feature、bug fix、improvementの順にsoftware profileをpilotする。
12. 設定installerと会社用adapterをpersonal coreから分離し、実測に応じて追加する。

この順序では、AI-DLC/Superpowersを一時的な実装基盤にも使わない。旧選択状態の削除や無効化は、Bootstrap inventoryとrollbackを作った後の独立した承認付き変更とする。

## 16. 一次資料

- Superpowers: [Skills一覧](https://github.com/obra/superpowers/tree/main/skills), [brainstorming](https://github.com/obra/superpowers/blob/main/skills/brainstorming/SKILL.md), [writing-plans](https://github.com/obra/superpowers/blob/main/skills/writing-plans/SKILL.md), [subagent-driven-development](https://github.com/obra/superpowers/blob/main/skills/subagent-driven-development/SKILL.md)
- AWS AI-DLC: [Phases and stages](https://awslabs.github.io/aidlc-workflows/guide/04-phases-and-stages/), [repository](https://github.com/awslabs/aidlc-workflows)
- Spec Kit: [Documentation](https://github.github.com/spec-kit/), [Agentic SDD](https://github.github.com/spec-kit/reference/agentic-sdd.html)
- OpenSpec: [Repository](https://github.com/Fission-AI/OpenSpec), [Overview](https://github.com/Fission-AI/OpenSpec/blob/main/docs/overview.md)
- BMAD: [Workflow map](https://docs.bmad-method.org/reference/workflow-map/), [Planning inside an organization](https://docs.bmad-method.org/plan/plan-inside-an-organization/)
- AI Hero: [Skills](https://www.aihero.dev/skills), [TDD Skill](https://www.aihero.dev/skills-tdd)
- Dotfiles: [nicknisi global instructions](https://github.com/nicknisi/dotfiles/blob/4f9d3afe824a597eef9a093612fb9dbcbce2b62c/home/.claude/CLAUDE.md), [liby routing](https://github.com/liby/dotfiles/blob/0346449a94932aa21fbce31df1f4875d9c53ffd4/AGENTS.md), [haacked AI architecture](https://github.com/haacked/dotfiles/blob/895d4b06a963c72c7ed269a4aa40d97bf04463b2/ai/README.md), [TechDufus Codex role](https://github.com/TechDufus/dotfiles/blob/56a0085acb2bc4f835ce6f20aa7a1fe5a80ecc16/roles/codex/tasks/main.yml), [wcygan portable config](https://github.com/wcygan/dotfiles/blob/e2c9eb73afa27b06594bb24912a07728eea6b936/config/codex/config.toml), [joshukraine runtime-state incident](https://github.com/joshukraine/dotfiles/blob/157799742c2d0f455feac89c485991a68a1e6605/README.md), [jackfranklin MCP source](https://github.com/jackfranklin/dotfiles/blob/a16ec499e98d0fbae9f595285bba4988f1b6362a/claude/mcp.json), [meain lifecycle hooks](https://github.com/meain/dotfiles/blob/1bb42a69a70569f01128b0adb573d6b17e68e06c/claude/.claude/settings.json), [freekmurze Skill projection](https://github.com/freekmurze/dotfiles/blob/09219e07aab14ea34375e85fd9597b63ca88f239/bin/link-agent-skills)
- Local research synthesis: [AIエージェント運用の型](../research/2026-09-ai-agent-operations-research.md), [AIエージェントdotfiles調査](../research/2026-09-ai-agent-dotfiles-landscape.md)

これらのsourceは設計のprovenanceであり、採用済みdistributionの一覧ではない。

## 17. 2026-09-02 trajectory correction

一次資料 [Clear境界に関するWorkflow実例調査](../research/clear-boundary-workflow-samples-2026-09-02.md)を比較した結果、以下をこのカタログの拘束条件とする。

- Workflow / Group / Skillの三レイヤーは維持する。Context EpochはLevel 4ではなく実行時管理情報で、Groupは複数Epochにまたがれる。
- Group間handoffは上流本文のコピーではなく、version付きArtifact Bundleの`path`/`version`/`digest`参照で行う。不足は所有する上流Skillへ戻す。
- Epoch/Group出口はcanonical artifacts、digest、acceptance evidence、approved decisions、unresolved items、invalidated artifacts、next inputsを持つbundle兼Checkpointとする。
- context budgetはtarget `200K`、通常上限`300K`、絶対上限`500K`。token値はexact/estimated/unavailableのいずれかで、推測値をexactにしない。
- token量に関係なく、調査→mutation、設計→実装、実装→独立review、authority/approval変更、canonical version変更でEpochを閉じる。
- A5→A6を最初のContext Epoch境界とし、A6は`clear_before_start: true`で開始する。
- A6成果はmigration sourceとして保持し、A6RでArtifact DAG Control Kernelへ変換・検証する。G1-G8とmigration approvalなしにA7へ進まない。
- CはC1-C2/C3-C6、DはD1-D4/D5-D7/D8-D12へ分ける。EはDAG batchごとにEpochを閉じ、E4/E5のcandidate review、E6の独立Finding validation、E9のfresh context/独立whole-change reviewを分離する。
- FはLevel 2 Groupではなく共通closure protocolとし、Sub Agentはtaskごとに独立Epochとpackageを持つ。Controllerへはresult/review object、artifact path/version/digest、検証、未解決事項だけを返す。
- compactionと自然言語summaryは正本ではない。resumeはphysical state、version、revision、digest、authority、未検証事項だけを根拠にする。
- canonical stateはimmutable objects、DAG transaction chain、atomic HEADの一系統とし、run/plan/status/event/CheckpointはHEADから再生成可能なprojectionにする。
- Controller/Orchestratorはdomain統合をせず、密な統合はconvergence Worker、blocking closeはfresh Reviewer、例外はArbiter/human gateへ渡す。

既存のGroup/Skill履歴と目的candidate `v001`は変更しない。上記は境界と契約の追補であり、既存成果物の本文を再解釈しない。

## 18. 2026-09-03 Finding validationと有限収束の決定

利用者承認により、reviewは「candidate Findingを作る段階」と「そのFindingが現在の仕事で本当に修正必須かを評価する段階」の二段階を通常経路とする。元Reviewerとfresh Finding Validatorを分離し、`required`だけがfix authorityへ進む。`defer/reject/needs-user`を明示し、重複、承認済みの理由ある選択、根拠のない好み、受入条件へ影響しない軽微事項から修正loopを開始しない。

仕事用Workflowを含む全profileは有限のreview budgetを持つ。deadlineまたはtimebox、最大review round、Findingごとの最大fix attemptをdispatch前に固定し、枯渇後はworkerを追加発行せず、成功済み成果、未解決Finding、stop reasonを返す。budgetの自動延長や、同一fingerprintを新Findingとして再発行することは禁止する。

## 19. 2026-09-05 Workflow Execution V2共通contract

このsectionは[全面再構築プラン section 21](./ai-agent-workflow-rebuild.md#21-2026-09-05-workflow-execution-v2-canonical-policy)のV2
execution policyを全60 named Skill contractと23 profile stepへ適用する。既存A1〜H3の60 contractと
feature 8、bug-fix 8、improvement 7の23 stepは増減・改名しない。二軸review、Validator、regression
shard、receipt aggregation、finalizationはordinary DAG Taskまたはdeep Moduleであり、新しいnamed Skillでは
ない。現在配布済みと主張できるSkillはA6/A6R/A7の3件だけで、全体は`partial`のままである。

### 19.1 全packageに追加するV2 execution contract

- loop levelを`artifact | section | workflow`として明記し、三層とも Plan、Implement、Review、Finding
  validation、Feedbackを持つ。Inner Artifactはfrozen candidateを作り、`RegressionFrontier`が宣言済み
  regression setをdisjoint shardへ分割する。Middle Sectionはaccepted Artifact refだけをConvergence
  Workerがfrozen Section candidateへ統合する。Outer Workflowはaccepted Section bundleを統合し、60/23、
  objective audit、outcome gateをcompletion classifierで閉じる。
- regression shardはexact canonical execution-package closure digest、resource/conflict claim、isolated cwd、
  temporary/output namespaceへbindする。ready frontierはpairwise-compatibleなshardだけをdispatchし、
  unknown unsafe claimを直列化または拒否する。`ReceiptAggregator`はdisjoint membership、coverage、exit、
  receipt completenessを検証するだけで、testを再実行しない。
- 同じfrozen candidateとaggregate receiptを、spec/architecture/safety軸とstandards/integration/operability軸の
  二つのfresh ordinary review Taskが別actor/Epoch/packageで検査する。両terminal reportのexact closure join後、
  一つのfresh `FindingValidator`が `required | duplicate | invalid | deliberate-design | downstream-only | too-minor |
  test-evidence-debt | needs-user` をadvisory出力する。Validatorはfix、counter、transition authorityを持たない。
- DAG Orchestratorだけがrole、authority、lease、expected HEAD、budgetを検証し、canonical round/attemptを導出し、
  fix/refusal/HEAD transitionを発行する。Workerはcloseせず、Reviewerは実装せず、Orchestratorはdomain correctnessを
  裁定しない。candidate、receipt、review、disposition、repair、close setを別immutable nodeにする。
- `EvidenceFinalizer`は全declared branchがknown/complete/acceptedで、join/refがintegral、open `required`、
  `needs-user`、incomplete、unknownがない時だけaccepted refを機械的にclose setへ組み立てる。test、Finding、
  product fix、budget stopの再解釈、reopenは行わない。

### 19.2 closure、persistent receipt、timing

execution-package closureにはcandidate/test/fixture/schema/config/lock bytes、executable/toolchain、command/args、
cwd、allowed environment、isolation/resource、supervision、external-input snapshotを含める。欠損または変更は
新packageとし旧receiptをreuseしない。stable upstream authorityからconsumerへだけedgeを張り、mutable
downstream byteをupstream close setへ逆bindしない。同じcomplete payload/idempotency keyは既存receiptを返し、
changed payloadはconflictとする。

`PersistentReceiptRunner`はspawn前にpackage/closure/shard/commandへbindしたatomic `started` receiptと、nonce、
boot、lease、process-birth identityを持つfenceをpublishする。timeout、heartbeat、grace、signal、whole process
group kill、terminal-publication allowanceをpackageへ固定する。stdout/stderrはowner-only binary storageへbounded
captureし、`complete | truncated | corrupt`、bytes、digest、sensitivity、安全なrefを記録する。secret raw bytesは
report/canonical/portable sourceへ入れない。terminalはreuse、live startedはwait/recover、証明済みorphanだけを
exactly-once terminalizeし、missing/corrupt/mismatch/ambiguityはduplicate launchせずfail closedとする。

package/reportはUTC RFC3339 start/endとmonotonic durationを持ち、queue、execution、preflight、design、edit、
focused test、regression、tool/output wait、review、report、helper、parent assimilationを分離する。`new-run |
reused-receipt`、closure digest、loop level、outcome、retry class、capture state/bytes、sensitivityも記録する。
retry counterは`product_fix_attempt`、`test_fixture_correction`、`command_or_capture_retry`、
`package_or_report_correction`、`review_round`を混ぜない。

### 19.3 finite stop、ownership、cutover

full timeout/grace/terminal allowanceが残budgetへ収まらない、overrun、round/attempt exhaustionでは
`stopped_budget` immutable non-dispatch terminalへ進み、leaseと未受理authorityを閉じる。再開はprior terminal
digest、expected HEAD、新lease、新budgetへbindしたversioned authorizationだけを受理する。stopをpass/finalize
してはならない。

portable source、generated native projection、managed destination、unmanaged user state、app-owned runtime、secret、
cacheを別owner classにする。`~/.codex`/`~/.claude` rootをapp-owned directoryとして保持し、owned child/keyだけを
preview付きで管理する。unmanaged conflictは拒否し、trust/session/UI/cache/marketplace/unowned MCPを保持する。
source、projection、packaging、Nix、rebuild/switch、migration、activation、Git、live validationは別gateである。

すでに発行済みのS1-Cとそのreplacement/descendantは旧immutable lineageを継承し、再packageしない。最初の
未発行frontierはV2を使う。parent Orchestratorはexpected-HEAD CASで旧/V2 digest、issuance watermark、全old
outstanding IDをatomic freezeし、watermark以下を旧、後続new rootをV2として選ぶ。old IDは旧terminal collection後
だけ除去する。rollbackはfuture rootの選択だけを変え、既発行lineage/reportを変更しない。このcatalog applyは
Run、Nix、migration、activation、Git、external stateを変更しない。
