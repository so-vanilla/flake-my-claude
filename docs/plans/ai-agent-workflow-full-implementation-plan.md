# AIエージェントWorkflow 全体実装計画

Status: reviewed / accepted for S0 source planning

Plan ID: `ai-agent-workflow-full-implementation/v1`

基準日: 2026-09-04（Asia/Tokyo）

## 1. 目的と今回の到達点

この計画は、[全面再構築プラン](./ai-agent-workflow-rebuild.md)と
[ステップ詳細カタログ](./ai-agent-workflow-step-catalog.md)を、現在の
Bootstrap control-kernelから全体実装へ進めるための実行計画である。

親ゴールは次のすべてであり、A6/A6R/A7の3 contractではない。

- 60 named Skill contractを実装artifactとacceptance evidenceへ結ぶ。
- feature 8、bug-fix 8、improvement 7の23 software profile stepを実装する。
- `catalog.yaml`、Group manifest、Workflow/profile manifest等の必須surfaceを作る。
- objective approval、二段階review、共通closure、判断集約、H1/H2/H3を含む
  completion gateを実装する。
- 個人用core、model/config/distribution source、会社用拡張sourceを統合する。
- 局所完了と全体完了を同じclassifierで区別し、物理artifactから再開できるようにする。

今回まず閉じる成果は、この全体計画とその独立reviewである。product sourceの
実装は、計画のreview・Finding validation・required-only feedback・fresh rereviewが
完了してから開始する。

## 2. 順序と権限境界

### 2.1 source作成を先に完了する

S0〜S8の全source sectionとsource-level integrationを先に完了する。この間に行う
検証は、unit/integration test、schema validation、static check、disposable fixture、
fault injection、source projection parityに限定する。

全source統合が閉じるまで、次を開始しない。

- `nix build`、buildを伴う`nix flake check`
- `darwin-rebuild build/check/switch`
- `home-manager build/switch`
- active generationまたはapp-owned configを使うfresh runtime検証
- generation変更を伴うmigration、cutover、rollback

S8完了後のR0で、generationを変更しないrebuild/build検証を別に計画・実行する。
実際のswitch/activationはR0とも別の明示承認単位とする。Git stage、commit、push、
branch/worktree操作も独立した権限であり、この計画から推論しない。

### 2.2 現在許可された範囲

利用者は、全体のsource作成をgeneration rebuild検証より先に進め、Sub Agentの
並列実行と最小contextを原則にするよう指示した。この権限は、S0〜S8のrepository
source、tests、docs、ignored work ledger/reportへ適用する。次の場合は継続権限を
section間で再利用できず、該当枝だけを停止する。

- objective、scope、material risk、予算、ownerを変える。
- source作成からactivation、credential、external mutation、Git操作へ越える。
- write scopeが既存の利用者変更または別workerと衝突する。
- accepted plan/catalog/objective/HEADのversionまたはdigestが変わる。
- `needs-user`または有限budget terminalが発生する。

clearはfresh-context境界であり、常に新しい利用者承認を意味しない。上記が不変で、
事前承認scope内なら、fresh Orchestratorはcheckpointを検証して次sectionを開ける。

### 2.3 objectiveの扱い

現在の`objectives/v001.md`は`candidate`で、approved pointerはnullである。これを
Bootstrap完了や本計画への同意から自動承認しない。generic sourceの作成は進められるが、
current Runのobjective-dependent execution、pilot、H1/H2、および全体完了claimには
B7相当の明示的なobjective approval receiptを要求する。

各Section/Task packageはobjective execution classを次のどちらかに固定する。

- `candidate-generic`: candidate objectiveとnull pointerでも、事前承認済みのgeneric
  source、schema、fixtureだけを作成できる。
- `approved-objective-dependent`: current Runの目的依存Task、pilot、H1/H2/H3、
  `full_workflow_ready` claimを扱い、B7 receiptとadvanced current pointerを要求する。

class、receipt、pointerの不整合はdurable rejection receiptを残し、HEADとready frontierを
変えない。fixture内のB7 positive pathをcurrent Runの`objective-approved`へ流用しない。

## 3. 数え方と正本

60/23はdirectory数ではなくcontract coverageで数える。

| namespace | IDs | count |
| --- | --- | ---: |
| Bootstrap | A1〜A7（A6Rを含む） | 8 |
| Objective | B1〜B7 | 7 |
| Outcome/measurement | C1〜C6 | 6 |
| Specification/planning | D1〜D12 | 12 |
| Execution/review | E1〜E10 | 10 |
| Shared closure | F1〜F8 | 8 |
| Decision promotion | G1〜G6 | 6 |
| Outcome/retention | H1〜H3 | 3 |
| Named contracts total |  | 60 |
| Feature profile | feature F1〜F8 | 8 |
| Bug-fix profile | BGF1〜BGF8 | 8 |
| Improvement profile | I1〜I7 | 7 |
| Profile steps total |  | 23 |

Group Fとfeature profile Fの衝突を避けるため、canonical selectorは少なくとも
`group.F.F1`と`profile.feature.F1`のようにnamespace-qualifiedにする。短いIDは
文書表示aliasに限定する。

一つのcontractは次の組が揃った時だけcoverageを得る。

1. catalog内のqualified IDとversion
2. `implementation_kind`
3. canonical sourceまたはcomposition mapping
4. owner moduleとinterface
5. acceptance test selector
6. artifact digestを持つacceptance evidence
7. Group/Workflow/profileからの到達可能性

60/23とは別に、`additional-required-surfaces/v1` inventoryをS0で定義する。少なくとも
model policy、config installer/guardrail、owner/distribution manifest、provider
projection、会社用7 Groupをstable IDごとに列挙し、canonical source、owner、acceptance
selector、evidence digest、completion stateを持たせる。S8 classifierは一件でも欠損・
未受理ならfail-closedとし、既存の3 pathや60/23だけで代替しない。

`implementation_kind`は、独立Skill、shared protocol operation、profile step/template、
composed/on-demand operationを許す。したがって60個の物理Skill directoryを要求しないが、
共有実装を理由にcontract固有の入力、出力、完了、証拠を省略しない。

## 4. 三重ループの共通contract

一つのまとまった実装sectionを、外側のSection loop、各成果物のArtifact loop、
section全体を結合するIntegration loopで進める。三ループはいずれも同じ五段階を持つ。

```text
PLAN -> IMPLEMENT -> CANDIDATE REVIEW
     -> FRESH FINDING VALIDATION + DEDUP/MATERIALITY
     -> FEEDBACK -> FRESH REREVIEW -> ACCEPT | BOUNDED STOP
```

### 4.1 各段階

| stage | owner | 必須入力 | durable output | advance guard |
| --- | --- | --- | --- | --- |
| Plan | planner + Orchestrator | objective/section/contract ref、accepted upstream、authority、budget | versioned plan/package | dependency、write scope、acceptance、stopが具体的 |
| Implement | WorkerまたはConvergence Worker | digest-bound Task package | candidate artifact/result、checks、未解決事項 | assignment、lease、HEAD、scopeがfresh |
| Candidate review | fresh Reviewer | Review package、対象diff/result、canonical requirement | candidate Findings、review Verdict | Workerと別actor/Epoch、実装変更なし |
| Validate/deduplicate | fresh Finding Validator | Evaluation package、全candidate、decision refs | disposition set、admissibility Verdict | 全candidateを一度だけ分類し、実装/closeなし |
| Feedback | Orchestrator + narrow Worker + fresh Reviewer | `required` Findingだけ | resolution claim、fresh rereview、replan/stop receipt | requiredがclose/supersede、または有限停止 |

Reviewerのcandidateは修正命令ではない。Validatorは各candidateを次に分類する。

- `required`: current mandatory acceptance、correctness、security、integrity、regressionを
  満たすため、現在直す必要がある。
- `defer`: 妥当だがcurrent scopeの結果を変えない改善。future planへ記録する。
- `reject`: 根拠不足、style preference、scope外、既解決、superseded、重複。
- `needs-user`: objective、scope、risk、budget、承認済みchoiceを変える。

S4の`finding-identity/v1`はalgorithm version、正規化したrequirement/decision refs、
affected artifact/result digestまたは明示的なintegration-scope digest、canonical gap
code、severity policyを持つ。完全に同じidentityだけをcanonical Findingへ
`duplicate_of`で統合し、review roundもfix attemptも消費しない。proseやevidence pathは
identityに含めず、artifact-localとsection-integrationはscope digestが違えば統合しない。

`rereview-closure/v1`は`pass`、`same_finding_unresolved`、`new_candidate`を区別する。
同一identityの残存はattemptを一回だけ消費し、新candidateはfix前にFinding validationへ
戻す。上限到達またはreview conflictはE10互換のArbiter/human routeを持つimmutable
terminalにし、replacement budgetとexpected HEADへbindした新approvalなしにreopenしない。

### 4.2 Artifact loop

Artifact loopは、一つのReviewerが独立にaccept/rejectできる成果物を扱う。

```text
READY -> ARTIFACT_PLAN -> WORKER_CANDIDATE -> ARTIFACT_REVIEW
      -> FINDING_VALIDATION
      -> ACCEPTED
      -> REQUIRED_FIX -> RESOLUTION_CLAIM -> FRESH_REREVIEW -> VALIDATION
      -> UPSTREAM_REPLAN | NEEDS_USER | TERMINAL_NON_DISPATCH
```

開始条件は、全`requires`がaccepted、write scopeがin-flight siblingと非重複、
sectionとartifactのreview budgetが残り、fixとfresh rereview分のreserveがあること。
WorkerはFindingをcloseできず、同じWorker/Reviewer/Epochでclosure reviewを行わない。

### 4.3 Integration loop

Integration loopは、accepted siblingと明示されたfailed/blocked branchを入力にする。

```text
COLLECT -> MECHANICAL COMPILE
        -> DOMAIN CONVERGENCE WHEN NEEDED
        -> FRESH INTEGRATION REVIEW
        -> FINDING VALIDATION
        -> REQUIRED-ONLY FEEDBACK + FRESH REREVIEW
        -> SECTION INTEGRATION ACCEPTED | BOUNDED STOP
```

機械的に結合できる場合、Orchestratorは次frontierをcompileするだけでdomain判断をしない。
矛盾、semantic gap、objective drift、cross-artifact riskがある場合だけ、一つの
Convergence Workerが統合し、別のfresh Reviewerが検査する。あるsiblingの失敗で、
受理済みの他siblingを破棄しない。

ただし受理状態は消費したinput digestへbindする。required fixまたはupstream replanで
digestが変わったらreverse `requires` closureを計算し、影響したartifactだけを
`invalidated`としてready/integration inputから外す。historical result/evidence objectと
影響外siblingは保持するが、影響artifactはreplacementとfresh review/validationなしに
current acceptedへ戻さない。

### 4.4 外側のSection loop

```text
VERIFY INPUT -> FIX SECTION PLAN -> RUN ARTIFACT WAVES
             -> RUN INTEGRATION LOOP -> SECTION REVIEW
             -> FINDING VALIDATION -> FEEDBACK/REREVIEW
             -> CLOSE + CHECKPOINT -> CLEAR -> OPEN NEXT SECTION
```

Sectionを閉じるには次をすべて満たす。

1. 計画された全artifactがaccepted、または明示的なterminal/human routeを持つ。
2. section integrationとsection-level acceptanceがfresh review済みである。
3. 未解決の`required` Findingがない。
4. objective alignmentが`aligned`、または承認済みの例外へbindされている。
5. 60/23 coverage delta、必須surface、test/evidenceがclassifierへ反映されている。
6. Bundle/Checkpointがaccepted、unresolved、invalidated、budget、next inputsを持つ。
7. 次sectionのschedule node、開始条件、最初のready frontierがdigest-boundである。

Section完了は局所claimであり、Workflow全体の完了ではない。

## 5. 並列実行と最小context

### 5.1 ready frontier

Sub Agentは依存DAG上のready frontierだけで並列化する。次のいずれかがあるtaskは
同時実行せず、一つへ結合するか順序付ける。

- 同じfileまたは同じcanonical interfaceへのwrite
- 未確定の共有schema/interface
- 一方のdomain判断が他方のacceptanceを変更する関係
- fix+rereview分を含むreview budget不足
- worker/report slot不足

人工的な総worker上限やSectionごとの固定同時実行数は設けない。ready frontier上で
dispatch可能な全Taskを並列候補にする。実効同時実行数は、実際のharness capacity、
依存関係、非重複write scope、review/validation capacity、残budgetによってだけ決まる。

### 5.2 package内容

Workerへ会話履歴や全planを渡さない。Task packageは次に限定する。

- work/ticket/section/artifact ID
- parent objective、plan、catalog、upstream artifactのpath/version/digest
- exact interface、write scope、non-goals
- acceptance tests/checksとevidence path
- owner role/assignment、expected HEAD、authority ref
- `workspace-provenance/v1`: assignment専用workspace/staging identity、または
  workspace identity + normalized owned-path baseline digest + expected HEAD
- finite deadline/review round/fix attempt budgetと残量
- stop reason一覧、unique report/result path

Review packageは対象candidate/diff/evidenceと関連requirement/decisionだけを持つ。
Evaluation packageはReview package digest、candidate IDs/fingerprints、materiality規則、
fresh Validator identityだけを持つ。Reviewer会話、Workerの自己評価、全文tool logは渡さない。

Task/Section packageは前節のobjective execution classと必要receipt/pointerも持ち、
不一致時のnon-mutating rejectionをacceptanceへ含める。

### 5.3 Context Epoch

次では必ずContext Epochを閉じ、物理packageからfresh actorを開始する。

- planからimplementation
- implementationからcandidate review
- reviewからFinding validation
- required fixからfresh rereview
- artifact waveからintegration
- section acceptanceからsection close
- section closeから次section open
- authority、objective、canonical versionの変更

token数は取得不能なら`unavailable`、推定なら`estimated`とする。200Kはcheckpoint検討、
300K超見込みはfresh split、500Kは継続拒否の目安であり、取得不能値を捏造しない。

### 5.4 worker lossとintegrity recovery

`worker-loss-recovery/v1`はlease/assignment/attempt ID、last accepted HEAD、write scope、
result/report digestまたはabsent declaration、workspace diffの隔離/破棄/引継ぎ判断、
消費budget、late-result rejection、replacement input/output refsをimmutableに記録する。
旧assignmentのlate resultは受理せず、新assignmentはunique result pathとfresh expected
HEADへbindする。影響外のaccepted siblingと残budgetは保持する。

workspace diffを引き継げるのは、assignment/attempt、authorized write scope、expected
HEAD、`workspace-provenance/v1`の全てに一致し、assignment-ownedと検証できる場合だけで
ある。帰属不能、dispatch前から存在、scope外の差分はsource bytesを変更せずquarantine
referenceとhuman/authority routeを記録し、重なるscopeのreplacement dispatchをblockする。
discardはassignment専用workspace/stagingと検証できる場合、または別の明示authorityが
ある場合だけ許す。

`integrity-recovery/v1`はfailing object/digest、許可されたprojection rebuild、staging
quarantine、orphan record、HEAD/source不変条件、検証済みrecovery transaction、新しい
expected-HEAD-bound authorityを持つ。sourceを上書きせず、このpackageの検証前にdispatchを
再開しない。S0とS4は両packageをfault injectionで検査する。

## 6. 有限budgetと停止

各Section planはdispatch前に、ISO deadlineまたはwall-clock timebox、最大review round、
Findingごとの最大fix attemptを有限値で固定する。並列policyは
`uncapped-ready-frontier`とし、同時worker数の数値上限は設定しない。既定budgetを使う場合も
profile/versionを明記し、未設定のまま実装を開始しない。

Findingごとの最大fix attemptの全体既定値は5回とする。Section planがより小さい値を
明示した場合はその値を優先するが、上限到達後の継続は利用者がreplacement budgetを
明示した場合に限る。上限変更は同一Finding ID、既消費回数、過去の失敗証拠を保持し、
新しいFindingへ付け替えてbudgetを初期化してはならない。

残りbudgetがbounded fixとfresh rereviewを収められない場合、新しいWorkerを出さない。
成功artifact、未解決Finding、消費budget、lease、stop reasonをimmutable terminalへ保存する。

| reason | state | resume condition |
| --- | --- | --- |
| context split | `paused_clear_required` | checkpoint検証後にfresh Epoch |
| stale/corrupt input | `blocked_stale_input` | upstream所有者の新artifactとCAS |
| missing/expired authority | `blocked_missing_authority` | expected HEADへbindした新approval |
| failed integration | `blocked_integration` | narrow fix + fresh integration review、またはhuman decision |
| unavailable worker | `paused_worker_unavailable` | recovery packageで旧outputを隔離し、lease release後にfresh package/result pathを新assignmentへbind |
| budget exhaustion / same Finding反復 | `terminal_non_dispatch` | E10 route、replacement budget、expected HEADの明示approval |
| objective/scope/risk change | `needs_user` | 利用者のversioned decision |
| integrity failure | `blocked_integrity` | source/HEADを変えず隔離・再生成し、検証済みrecovery transactionと新authority |

## 7. 全体の実装section

各Sectionは前節のSection loopを一回実行する。表のartifactは想定される主成果物であり、
Section planがfile名を精密化しても、coverage、owner、interface、acceptanceを削れない。

| Section | scope | 主な成果物 | 依存 | source-level exit |
| --- | --- | --- | --- | --- |
| S0 Goal/schedule control plane | 全60/23のregistry、追加surface、completion、次section遷移 | `catalog.yaml`、catalog/schedule schemas、plan coverage manifest、`additional-required-surfaces/v1`、qualified selector/alias、`section-control-plane/v1`、status/resume classifier | accepted plan | closed Bootstrap fixtureから`source-transition-fixture-passed`を得て、60/23 scheduleと最初のready frontierを再生成する。actual A7は実行しない |
| S1 Bootstrap and shared lifecycle | A1〜A7、F1〜F8 | A1〜A5 contract source、既存A6/A6R/A7 mapping、Bootstrap Group manifest、close/checkpoint/clear/resume shared modules、evidence。S0 transition contractのconsumer | S0 | 8+8 contractsがmappingされ、S0 transitionを再定義せず、A履歴を改変しないmulti-Group fixtureとcrash recoveryが通る |
| S2 Objective and outcome system | B1〜B7、C1〜C6 | B/C Group manifests、objective candidate/approval/version modules、outcome DAG、measurement/target/baseline schemas/templates、C-01/C-02 bundles | S0/S1 | 13 contracts、fixture B7 positive/negative path、live approvalとの非流用、測定不能=`unavailable`、二Epoch resumeが通る |
| S3 Specification and planning | D1〜D12 | D Group manifest、profile selection、current-system/practice discovery、spec/design/contract artifacts、task DAG、worker briefs、verification/recovery/readiness package | S2 | 12 contracts、D-01/D-02/D-03 handoff、upstream return、readiness rejectionが通る |
| S4 Execution, review, convergence | E1〜E10 | dispatch engine、Task/Review/Evaluation/Finding/Verdict modules、`finding-identity/v1`、`rereview-closure/v1`、review budget、dedup/materiality、reverse dependency invalidation、worker-loss/integrity recovery、Convergence Worker、whole-change verification、Arbiter route。S0 transition contractのconsumer | S0/S1/S3 | 10 contractsと三重loopが全fixtureで有限収束し、影響外siblingを保持し、影響済みaccepted stateとlate resultを再利用しない |
| S5 Software profiles and composition | feature 8、bug-fix 8、improvement 7 | 3 profile manifests/templates、profile固有artifact/evidence、B/C/D/E/F/G/H composition、representative fixture | S2〜S4 | 23 qualified profile stepsが到達可能で、共通Group本文のcopyがなく、3 profileの証拠が異なる |
| S6 Decision and outcome lifecycle | G1〜G6、H1〜H3 | decision inventory/classification/graph、candidate record、approval/promotion/backlink、objective audit、run outcome、archive/continue | S1〜S5 | 9 contracts、unapproved promotion拒否、deliveryとobjective達成の分離、retention/rollback保持が通る |
| S7 Model, config, distribution, company extension | model routing、installer/config ownership、provider projection、会社用7追加Group | model-policy source、effective-model receipt、managed config merge/backup/rollback、owner/distribution manifests、company data/identity/tool/catalog/eval/release/audit policy modules、追加surface evidence | S0〜S6 | inventoryの各stable IDへsource/test/evidenceをbindし、fixture/static checkでunknown config保持、secret非保存、single lifecycle owner、company/personal store分離が通る |
| S8 Source-wide integration and completion audit | 全60/23 + required surfaces/gates | compiled source projections、coverage/evidence index、3 profile E2E fixtures、cold resume fixture、source release-readiness report、H1/H2/H3 evidence skeleton | S0〜S7 | named 60/60、profile 23/23、`additional-required-surfaces/v1`全件accepted、source-level required Finding 0。Nix/generation rebuildは未実行と明記する |

### 7.1 dependency waves

```text
Reviewed plan
  -> S0
  -> S1
  -> S2
  -> S3
  -> S4
  -> S5
  -> S6
  -> S7
  -> S8 source-wide integration
  -> R0 non-switching generation rebuild/build verification
  -> R1 explicit activation decision
  -> R2 explicit Git publication decision
```

Section内では非重複artifactを並列化する。S7の会社用policy sourceはpersonal coreの
interfaceがS6までに固定されてから開始し、運用接続や実credentialは扱わない。

S0の最初のdesign artifactである`section-control-plane/v1`は、section/group/workflow
mapping、object/transaction/node/edge type、`open_group`/`open_section` payload、expected
HEAD、authority、idempotency、closed Bootstrap checkpoint adapter、唯一の
reducer/compiler/status/resume owner、rejection/rollback semanticsを固定する。S1/S4は
このpublic contractだけを消費し、transition surfaceを再定義しない。

S0が扱うのはsource simulation claimである。A6R、G1〜G8、migration approvalを要する
actual A7 self-host handoffは`A7-handoff-complete`という別のoperational gate/claimであり、
S0/S1 source acceptanceのpreconditionにも代替証拠にもならない。

## 8. Sectionごとの計画artifact

各Section開始前に`section-plan/v1`を作る。少なくとも次を含む。

- section ID/versionとcoverage delta
- parent objective、plan、catalog、implementation-status refs
- objective execution class、required approval receipt/pointer、rejection semantics
- artifact DAGとqualified contract/profile IDs
- `additional-required-surfaces/v1`のsection delta
- 各artifactのowner module、interface、write scope、Task package
- consumed input digestsとreverse `requires` invalidation rule
- ready frontier waveとintegration seam
- `uncapped-ready-frontier`と実効並列を制約するdependency/write-scope/capacity条件
- candidate review、Finding validation、feedback/rereview assignment
- section、artifact、Findingのfinite budget
- unit/integration/schema/static/fault-injection checks
- recovery、rollback、stop reason、next-section preconditions
- worker-loss/integrity recovery package、workspace provenance/baseline、diff attribution、
  non-destructive quarantine、blocked replacement scope、late-result rule
- source-only boundaryと未許可操作

Section完了時は`section-result/v1`、`section-review/v1`、
`finding-validation-set/v1`、`section-bundle/v1`、`checkpoint/v1`を保存し、
implementation-statusを受理済みevidenceから再計算する。

## 9. 全体完了判定

次を別claimとして表示する。

- artifact accepted
- section source complete
- source-wide integration complete
- non-switching rebuild verification complete
- activation complete
- full Workflow ready
- current Run objective achieved

S8 source完了時点でも、R0、activation、pilotの実観測、objective auditが残る場合、
`full_workflow_ready`をtrueにしない。逆に、R0のbuild成功だけで60/23 coverageを補わない。

全Workflowの`ready`には、少なくとも次を要求する。

1. plan/catalog digest一致
2. named contract 60/60、profile step 23/23
3. `additional-required-surfaces/v1`全件とprovider projections
4. 全required Findingのfresh closure
5. evidence付きcompletion gate
6. approved objectiveとH1 objective audit
7. H2 run outcomeとH3 archive/continue
8. generation/rebuild/runtimeの要求された検証結果
9. 明示されたoperational adoption/activation decision

## 10. この計画自体のreview loop

本計画も同じ五段階で受理する。

1. Plan: coverage、loop、authority/recoveryの独立proposalを作る。
2. Implement: rootが本計画へ統合する。
3. Review: spec/coverage、operability/loop、context/authorityをfresh Reviewerが並列検査する。
4. Validate/deduplicate: 別のfresh Validatorが全candidateを一度だけ分類・統合する。
5. Feedback: rootが`required`だけを反映し、fresh rereviewでclosureする。

plan reviewの受入条件は次のとおり。

- 60/23と追加必須surfaceが一つの実装順へ割り当てられている。
- 三重loopの全段階、owner、package、budget、停止・再開が定義されている。
- Sub Agentが必要最小限のcontextだけを受け取る。
- closed Bootstrapからnext sectionへ進めない回帰をS0のsource fixtureが直接扱い、
  actual A7 handoffとは別claimにする。
- source作成とgeneration rebuild検証、activation、Gitが分離されている。
- required candidateが0になるか、有限停止理由とexact next actionが残る。

## 11. 最初の実装action

本計画のfresh rereviewがpassした後、S0の`section-plan/v1`を作る。S0の最初の
acceptance testは、現在の物理状態を再現する次のred caseである。

```text
Bootstrap group = closed
next_group = contracts-and-schema
status = paused_after_group
ready = []
next Groupをopenするvalidated command = absent
```

`candidate-generic` classと、validなcandidate objective/plan/catalog/closed Bootstrap
checkpoint/source authority/expected HEADを持つfresh `open_group`/`open_section` source
commandだけが、fixture上で次Groupと最初のEpochをopenし、予定された最初のTaskをreadyへ
載せる。stale、forged、scope不足ではrejection receiptを残し、HEADもready frontierも
変えない。

これは`source-transition-fixture-passed`を検査するsimulationであり、actual A7を実行したり
`A7-handoff-complete`を主張したりしない。actual A7 commandはA6R、G1〜G8、migration
approvalが不足する限り拒否され、S0のsource acceptanceを妨げない。

## 12. Workflow Execution V2 canonical implementation rule

2026-09-05以後の実装は、[全面再構築プラン section 21](./ai-agent-workflow-rebuild.md#21-2026-09-05-workflow-execution-v2-canonical-policy)と
[step catalog section 19](./ai-agent-workflow-step-catalog.md#19-2026-09-05-workflow-execution-v2共通contract)をcanonical
execution contractとする。本sectionはsection 4の旧execution mechanicsをV2で置き換えるが、section 3の60/23
inventory、section 7のS0〜S8 scope/dependency、特に既発行のhistorical S1 section planを書き換えない。

### 12.1 実装する三重loopとdeep Module

三重loopは **Inner Artifact / Middle Section / Outer Workflow** であり、いずれもPlan、Implement、Review、
Finding validation、Feedbackを持つ。Inner WorkerはRED、最小fix、focused GREEN、purity後にcandidateをfreezeする。
`RegressionFrontier`は完全closure digestとresource claimへbindしたdisjoint shardを作り、compatible ready frontierだけを
isolated namespaceで実行する。`ReceiptAggregator`はcandidate-bound receiptのdisjointness/coverage/completenessを
test再実行なしにjoinする。Middleはaccepted Artifact refs、Outerはaccepted Section bundlesだけを統合してそれぞれ
frozen candidateを作る。

各loopのreviewは、同じfrozen candidate/aggregate receiptを入力にした二つのfresh ordinary DAG Taskと、そのexact
joinを分類する一つのfresh advisory `FindingValidator`で行う。review軸はarchitecture/safetyと
integration/operabilityで、actor、Context Epoch、packageを分離する。paired-review Skill/Module/schemaは作らない。
fix/refusal/canonical counter/HEAD transitionはDAG Orchestratorだけが発行し、Validatorは助言、Reviewerはcandidate
Finding、Workerはwork product/resolution claimだけを提出する。

実装surfaceは次のdeep Module interfaceを優先し、private helperごとにSkill/portを増やさない。

- `ArtifactCandidateBuilder`: validated Task packageからfrozen candidateとfocused receiptを作る。
- `RegressionFrontier`: regression inventoryをdisjoint shardへ分け、resource conflict/unknownをdeterministically
  admit、serialize、またはrefuseする。
- `PersistentReceiptRunner`: closure-bound commandをfenced supervisionし、atomic receiptとbounded binary output refを残す。
- `ReceiptAggregator`: exact closure、membership、coverage、exit、completenessだけをjoinする。
- `FindingValidator`: 二つのreviewからadvisory disposition、materiality、提案scope、観測budgetを出す。
- `DAGOrchestrator`: expected HEAD、lease、authority、budget、canonical counter、dispatch/stop/CASだけを所有する。
- `EvidenceFinalizer`: 全branchのknown/complete/accepted predicateを満たすaccepted refだけを機械的に整列する。
- `DistributionPlanner` / `NativeProjectionAdapter`: ownership付きpreviewとprovider-native staged projectionを作るが、
  destination mutation/app-owned state installationを所有しない。

### 12.2 closure、supervision、finite convergence

canonical execution-package closureはcandidate/test/fixture/schema/config/lock、executable/toolchain、command/args、cwd、
allowed environment、isolation/resource、supervision、explicit external snapshotを含む。一要素でも変われば新packageで
あり、旧receiptはreuse不可とする。dependency/authorityはstable ownerからconsumerへの一方向だけとし、mutable
downstream test bytesをfixed upstream close setへbindしない。candidate、receipt、review、Validator disposition、repair、
close setは別immutable nodeで、attemptをappendして履歴を保持する。

runnerはspawn前のatomic `started` receipt、nonce/boot/lease/process-birth fence、timeout/heartbeat/grace/signal/process-group
kill/terminal-publication allowanceを実装する。binary stdout/stderrをowner-only bounded storageへcaptureし、complete/
truncated/corrupt、bytes、digest、sensitivity、safe refをterminal化する。disconnect後はterminal reuse、live wait/recover、
証明済みorphanのexactly-once terminalizationだけを許し、ambiguous stateではreplacementを拒否する。four crash pointの
fault injectionで同時live commandが高々一つと証明する。

残budgetがfull supervision allowanceを収めない、overrun、review-round/finding-attempt exhaustionではcanonical
`stopped_budget`へ遷移し、leaseとunaccepted dispatchを閉じる。reopenはprior terminal digest、expected HEAD、新lease、
replacement budgetへbindしたversioned authorityだけを受理する。`EvidenceFinalizer`はopen `required`/`needs-user`、
incomplete/unknown branch、integrity不成立、`stopped_budget`をfinalizeしない。

`product_fix_attempt`（最大5）、`test_fixture_correction`、`command_or_capture_retry`、
`package_or_report_correction`、`review_round`を独立に記録する。package/report timingはUTC RFC3339とmonotonicでqueue、
execution、preflight、design、edit、focused/regression test、tool wait、review、report、helper、parent assimilationを分離し、
new-run/reused-receipt、closure digest、loop、outcome、capture state/bytes/sensitivityを持つ。診断SLOはdispatch 2分、
candidate 15分、shard 5分、review wave 10分、Validator 5分、parent finalization 3分、no-Finding inner loop 35分とする。

### 12.3 distribution、cutover、現在値

S7 distributionはportable source、generated projection、managed destination、unmanaged state、app-owned runtime、secret、
cacheを別owner classとして実装する。provider rootはreal app-owned directoryのまま、owned child/keyだけをpreview、
conflict refusal、backup、doctor、uninstall、rollback付きで扱う。generated native projectionは手編集せず、unsupportedな
provider semanticsを保存したと主張しない。live distribution transactionとincremental/Merkle aggregationは今回の
canonical applyに含めず、後者はfan-in/bytes/parent assimilationの測定後に別authorityで判断する。

すでに発行済みのS1-Cは旧immutable lineageのまま継続し、historical S1 plan、package、reportを再packageまたは
書き換えない。最初の未発行frontierはV2を使う。parent-only expected-HEAD CAS cutoverは旧/V2 contract digest、単調
増加issuance watermark、全outstanding old-package IDを一transactionでfreezeする。watermark以下のrootと全
replacement/descendantは旧contractを継承し、後続new rootだけがV2を使う。old IDは旧terminal collection後だけ
outstanding setから除去し、rollbackはfuture root selectionだけを変える。

このcanonical document applyはimplementation coverageを増やさない。60 named contract、23 profile step、S0〜S8順序、
既存A6/A6R/A7の3 distributed Skill claimを保持し、`overall_status: partial`、`full_workflow_ready: false`のままとする。
Run/HEAD、Nix/build/rebuild、migration/cutover実行、activation、Git、external stateは別authorityである。
