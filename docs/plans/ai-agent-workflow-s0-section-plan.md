---
schema: section-plan/v1
section_id: S0
version: v1
status: accepted-for-source-implementation
objective_execution_class: candidate-generic
parallel_policy: uncapped-ready-frontier
---

# S0 Goal/schedule control plane section plan

基準日: 2026-09-04（Asia/Tokyo）

## 1. 目的とexit

S0は、全体計画を実行可能なsource control planeへ変換する。次を一つのSection loopで
実装・統合・reviewし、open required Findingが0になった時だけ閉じる。

1. 60 named contractと23 profile stepのqualified target registry
2. 11件の`additional-required-surfaces/v1`
3. target/source-present/acceptedを分離するfail-closed coverage classifier
4. 既存`ControlKernel`を唯一の状態ownerとする`section-control-plane/v1`
5. closed Bootstrap fixtureから次section/group/Epoch/frontierを開くsource simulation
6. status/resumeによるschedule、coverage、ready frontierの再生成

exit claimは`source-transition-fixture-passed`である。actual A7、migration、activation、
full Workflow readyはclaimしない。named contractのaccepted countはS0完了後も、既存の
A6/A6R/A7を除きsource/evidenceが実装されるまで増やさない。

## 2. digest-bound inputs

| input | path | SHA-256 |
| --- | --- | --- |
| accepted overall plan | `docs/plans/ai-agent-workflow-full-implementation-plan.md` | `6c1b2ed01a4151ead414c24bd1d2eab78f596623039f038a3f2c17c212ebb401` |
| rebuild design | `docs/plans/ai-agent-workflow-rebuild.md` | `61e43ff9c56e459a26d8419cd957eb5767623f10ea173820d018f8863a16f0be` |
| contract/profile catalog source | `docs/plans/ai-agent-workflow-step-catalog.md` | `8418e7d32291860faeb7e905b048a7ff7a03d0768b258ac9e4a75f4e7d498b6b` |
| current implementation status | `agent-workflows/manifests/implementation-status.json` | `7ab62092b7031f58fc5735e8ae33e369d87208df9f53257576eb4dac4032196e` |
| candidate objective | `.local/agent/runs/2026-09-01-ai-agent-workflow-rebuild/objectives/v001.md` | `38b66e58c1a24419159d1e10f4e659cdcc46dbbdfc5cbbae1e321e3325094752` |
| closed Bootstrap example | `.local/agent/runtime/2026-09-01-ai-agent-workflow-rebuild-a7/new-kernel-002/.local/agent/runs/2026-09-01-ai-agent-workflow-rebuild/control-kernel/projections/status.json` | `f52a92e1894edc6309f671a531cb04b005bafca77e2b0c1d7a4e14fcd5202561` |
| source authority | work ledger checkpoint 11 | `b72ef8048ecadd7b661e49612b09e520fadf607e9b26ae7798fc97956e220ea0` |

candidate objectiveとclosed Bootstrap exampleはfixture設計のread-only inputであり、
current Runを変更するauthorityではない。Task dispatch時は本plan自体のdigest、owned-path
baseline、expected HEAD、assignmentを追加でbindする。

## 3. authorityとnon-goals

許可するwriteは`agent-workflows/`配下のS0 source/schema/manifest/test/fixture/evidence、
本S0 plan、ignored ledger/reportだけである。次は扱わない。

- S1〜S8のcontract/profile implementation
- Nix build/check、darwin/home-manager rebuild/switch
- active runtime、manual/new-kernel Run state、runtime pointer
- actual A7、migration/cutover/activation/rollback operation
- credential、external mutation、Git stage/commit/push
- 既存利用者diffまたは他assignmentへ帰属するbytesの上書き・破棄・引継ぎ

`candidate-generic`はsource/schema/fixtureにだけ使う。B7 fixtureのpositive pathをlive
objective approvalへ昇格せず、objective-dependent Task、pilot、H1/H2/H3、full-readyは
durable rejectionかfalse claimのままにする。

## 4. public seamとowner

TDDで確認済みの公開seamは`SectionControlPlaneV1`である。testとconsumerは次だけを知る。

```text
compile_registry(inputs) -> CoverageProjectionV1
open_section(kernel, command) -> CommandReceiptV1
open_group(kernel, command) -> CommandReceiptV1
status(kernel, expected_head) -> SectionStatusV1
resume(kernel, expected_head) -> SectionStatusV1
```

`SectionControlPlaneV1`はcatalog-qualified selector解決、section/group/workflow mapping、
closed-Bootstrap adapter、schedule/first-frontier compilation、claim分離をownerする。
immutable object、transaction、CAS、authority、idempotency、reducer、HEAD、projection publish、
recoveryの唯一のownerは既存`ControlKernel`のままとする。第二のstore/state/reducerを作らない。

`open_section`はsectionと最初のgroup/Epoch/frontierを一transactionで開く。
`open_group`はaccepted section内の後続groupと最初のEpoch/frontierを一transactionで開く。
どちらもhalf-open section/groupを残さない。S1/S4はこのpublic seamのconsumerであり、
command grammar、schedule compiler、status/resume reducerを再定義しない。

`catalog.yaml`はstandard-library-only制約を維持するため、JSONとしても妥当なYAML 1.2
documentをcanonical serializationとし、runtimeは`json` parserで読む。表示aliasは保持できるが、
machine selector、evidence、status queryはqualified IDだけを受理する。

## 5. artifact DAG

| artifact | owner | dependencies | owned write scope | acceptance |
| --- | --- | --- | --- | --- |
| S0-P section plan | root | overall plan | this file、ledger | plan/schema/seam/DAG/budget/stop fixed |
| S0-A registry vertical slice | registry Worker | S0-P | `catalog.yaml`、new catalog/coverage/additional-surface modules/schemas/manifests、`test_catalog_coverage.py`、S0 coverage fixtures | 60/23/11、qualified selector、three-map classifier、fail-closed tests |
| S0-B control-plane contract slice | contract Worker | S0-P | `section_control_plane.py`、new section schemas、`test_section_control_plane_contract.py`、contract fixtures | deterministic compile/public result、no second state owner |
| S0-C kernel transition slice | kernel Worker | accepted S0-B | `control_kernel.py`、`dag-command-v1.schema.json`、必要最小限のstate/object schema、`test_section_control_plane_kernel.py` | atomic open commands、CAS/authority/idempotency/rejection/recovery |
| S0-D coverage/status slice | status Worker | accepted S0-A | `implementation_status.py`、`implementation-status-v1.schema.json`、`test_s0_coverage_status.py` | compatibility維持、planned/present/accepted分離、claim false-positiveなし |
| S0-E integration/evidence | rootまたはsingle convergence Worker | accepted S0-A〜D | new `test_s0_integration.py`、S0 integration fixtures、section evidence/result、必要なREADME/manifest wiring | source transition、cold resume、146 baseline regression、section claim |
| S0-R candidate reviews | fresh Reviewers | S0-E candidate | unique ignored reports only | spec/coverage、operability/kernel、authority/recovery candidates |
| S0-V Finding validation | fresh Validator | all S0-R reports | unique ignored report only | every candidate once、dedup/materiality、required-only scope |
| S0-F feedback/rereview | root + narrow Worker + fresh Reviewer | S0-V | permitted required scopes only | open required 0 or bounded terminal |

S0-AとS0-Bは最初のready frontierとして並列化する。S0-CとS0-DはそれぞれB/Aの
artifact acceptance後に並列化できる。`control_kernel.py`と既存dag schemaはS0-Cだけ、
`implementation_status.py`と既存status schemaはS0-Dだけがwriteする。S0-E前に両者を
同じworkerへ同時所有させない。

## 6. Artifact loop

各S0-A〜Dは次を一回以上実行する。

```text
artifact plan/package
  -> one red public-seam test
  -> minimum green implementation
  -> focused test
  -> candidate result
  -> fresh artifact review
  -> fresh Finding validation/dedup/materiality
  -> required-only fix + fresh rereview
  -> accepted | bounded stop
```

WorkerはFindingをcloseしない。reviewer candidateは修正authorityではない。同一Findingは
`finding-identity/v1`で束ね、文章差だけでreview/fix budgetを再消費しない。

## 7. ordered TDD slices

### Slice 1: registry compiler

Red: canonical inputを60/23/11のqualified registryへcompileできず、bare `F1`が
group/profile間で曖昧なまま通る。

Green: deterministic `catalog/v1`、`additional-required-surfaces/v1`、
`plan-coverage/v1`を返し、named 60、profile 23、additional 11を区別する。target、
source-present、acceptedは別mapとし、欠損source/evidenceはacceptedにならない。

### Slice 2: section contract compiler

Red: closed Bootstrap、accepted plan/catalog、next groupを入力しても、versioned
section command/statusをcompileできない。

Green: deterministic `section-control-plane/v1`と`section-plan/v1`を返し、section、
group、workflow、first Epoch/Task、input digests、execution class、authority scope、
expected HEAD、idempotencyを一つのpublic commandへbindする。

### Slice 3: atomic kernel transition

Red: `paused_after_group`、closed Bootstrap、`ready=[]`からvalidated open commandがなく、
next group/frontierを開けない。

Green: valid `candidate-generic` commandだけが一transactionでsection/group/Epochを開き、
予定されたfirst Taskだけをreadyにする。stale HEAD、unknown section/group、wrong role/scope、
forged digest、objective-class mismatchはHEAD/ready不変でrejectする。

### Slice 4: idempotency/status/resume

Red: exact retryが二重advanceする、同じkeyの別payloadが通る、projection欠損/偽造で
schedule/readyが変わる。

Green: exact retryはcanonical receiptを返し、conflictはrejectする。fresh status/resumeは
accepted HEADから60/23/11 schedule、section status、ready frontierを再生成する。

### Slice 5: coverage/status compatibility

Red: target/source/acceptedが混在し、S0 registryの存在だけで60/23やfull-readyが増える。

Green: current A6/A6R/A7 release evaluationを維持しつつ、planned 60/23/11、present、
accepted、missing/invalidを別表示する。S7 surface、B7、pilot、R0/activation等のevidenceが
なければsource-wide/full-readyはfalseである。

### Slice 6: minimal recovery and claim separation

Red: lost assignment、pre-HEAD orphan、projection欠損がsectionを自動openする、または
source transitionがactual A7 completeをclaimする。

Green: recoveryはquarantine/rebuild/orphan receiptだけを作り、HEAD/frontier/source bytesを
勝手に変えない。帰属不能diffはreplacement scopeをblockする。source fixtureは
`source-transition-fixture-passed`だけをtrueにし、`A7-handoff-complete`はfalse/absentにする。

## 8. acceptance matrix

| case | required observation |
| --- | --- |
| counts | named=60、profile=23、additional=11をcanonical sourceから導出 |
| namespace | `group.F.F1`と`profile.feature.F1`は別、bare ambiguous alias reject |
| missing source/evidence | `missing_or_invalid`または`present_unaccepted`; acceptedへ昇格しない |
| valid open section | one transaction、next group/Epoch、only scheduled first ready Task |
| valid open group | accepted section childだけをone transactionでopen |
| stale/unknown/forged/wrong authority | typed rejection、HEAD/ready/status不変 |
| idempotent retry | original receipt、revision不変; changed payloadはconflict |
| candidate-generic | generic sourceだけ許可; pilot/H1/H2/H3/full-ready/actual A7 reject |
| resume | projectionを削除/偽造してもaccepted HEADから同一schedule/frontierを再生成 |
| worker loss/integrity | late result reject、unowned diff bytes不変、quarantine/block、valid recovery only |
| claims | S0 source claimとactual A7/activation/full-readyが別 |

## 9. Integration loop

S0-EはA〜Dのresult/evidence digestだけを入力にする。upstream digestが変わった場合、
reverse `requires` closure上のaccepted stateだけをinvalidateし、historical evidenceと
影響外siblingを保持する。

統合順:

1. schema registryをcompileし、全documentをvalidationする。
2. disposable closed-Bootstrap fixtureでopen/status/resume/recoveryを実行する。
3. existing implementation-status compatibilityとclaim分離を検査する。
4. focused S0 testsを通す。
5. Nixを使わずPython full suiteを通し、baseline 146 testsを下回らない。
6. section result/evidence/statusをaccepted artifact digestから生成する。
7. fresh integration/section reviewとFinding validationを行う。

## 10. parallelism、budget、stop

parallel policyは`uncapped-ready-frontier`。固定worker数は持たず、実効並列度はharness
capacity、dependency、non-overlapping write scope、review/validation capacity、残budgetで
決める。現在のharness実容量ではroot以外に最大3 active Sub Agentだが、計画上の上限にはしない。

- section timebox: `PT8H`
- maximum artifact review rounds: 2
- maximum integration/section review rounds: 2
- maximum fix attempts per canonical Finding: 5
- feedback前にfix + fresh rereviewの一往復分をreserveする

同一Finding反復、review conflict、budget不足は新IDで回避せず、E10-compatible
`terminal_non_dispatch`またはhuman/authority routeへ送る。

`S0-C-F001`は2回目で一度`terminal_non_dispatch`へ到達したが、利用者が上限を5回へ
明示的に増やしたreplacement budgetにより同じFinding IDと既消費2回を保持して再開する。
この変更はreview roundを増やさず、受理済みsiblingをinvalidateしない。

停止条件:

- accepted input digestまたは本plan digestが変わる
- owned pathにbaseline後の利用者/別worker変更がある
- worker assignment/provenance/expected HEADが一致しない
- source-onlyからNix、current Run、migration、activation、Gitへ越える
- actual A7/B7 approvalをS0 source acceptanceの前提または代替証拠にする必要が生じる
- required artifactを正本から一意にcompileできない
- `needs-user`またはfinite budget terminal

## 11. recovery

各Task packageは`workspace-provenance/v1`としてassignment専用workspace/staging identity、
またはworkspace identity + owned-path baseline digest + expected HEADを持つ。lost workerの
diffはassignment-owned/in-scopeと証明できる場合だけtransferする。帰属不能、既存、scope外は
bytes不変でquarantine referenceとhuman routeを残し、重なるreplacementをblockする。

accepted resultは消さない。corrupt projectionはHEADから再生成し、corrupt object/parent chainは
`blocked_integrity`で止める。sourceを推測で上書きせず、verified recovery transactionと
fresh expected-HEAD authority後だけresumeする。

## 12. Section close artifacts

S0 close時に次を物理化する。

- `section-result/v1`
- `section-review/v1`
- `finding-validation-set/v1`
- `section-bundle/v1`
- `checkpoint/v1`
- S0 acceptance evidenceとtest command/exit status
- accepted/invalidated/unresolved/budget/next-section input refs

`implementation-status`はaccepted evidenceから再計算する。S0 source completeと、named
contracts 3/60、profiles 0/23、full Workflow ready=falseを同時に表示できなければ閉じない。

## 13. exact first action

S0-A registry vertical sliceとS0-B control-plane contract sliceを、別owned write scopeへ
history-free Terra Workerとして並列dispatchする。それぞれ最初にpublic-seam red testを
追加し、redを確認してからminimum greenへ進む。
