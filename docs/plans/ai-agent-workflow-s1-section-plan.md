---
schema: section-plan/v1
section_id: S1
version: v1
status: accepted
objective_execution_class: candidate-generic
parallel_policy: uncapped-ready-frontier
---

# S1 Bootstrap and shared lifecycle section plan

基準日: 2026-09-04（Asia/Tokyo）

## 1. 目的とexit

S1は、S0が固定したqualified registryと`section-control-plane/v1`を使い、Bootstrapと
共有closure protocolをsourceとして完成させる。S1自身はschedule、reducer、HEAD、CAS、
status/resumeの新しいownerを作らない。

対象は16 named contractである。

- Group A: `group.A.A1`〜`group.A.A7`（`A6R`を含む）8件
- shared closure: `group.F.F1`〜`group.F.F8` 8件

新規実装はA1〜A5の5 Skillと、F1〜F8を提供する一つのshared protocol moduleである。
A6/A6R/A7のSkill source、selector、acceptance evidenceはread-only dependencyとして保持する。
F1〜F8はregistry上の`shared-protocol-operation`であり、8個の薄いSkillへ複製しない。

source-level exitは次の全てである。

1. 16 contractがqualified source、公開interface、実行可能selector、digest-bound receiptへ結合される。
2. Bootstrap Group manifestとshared closure manifestがstrict schemaに適合する。
3. S0 accepted resultを親にしたdisposable multi-Epoch/multi-Group fixtureが通る。
4. A履歴を変更せず、crash/orphan/projection lossからcold resumeできる。
5. S1のartifact/integration/section reviewとfresh Finding validation後、open requiredが0である。
6. `implementation-status`がnamed 16/60、profile 0/23を示し、actual A7、migration、
   activation、full Workflow readyはfalseのままである。

## 2. digest-bound inputsとworkspace provenance

| input | path | SHA-256 |
| --- | --- | --- |
| accepted overall plan | `docs/plans/ai-agent-workflow-full-implementation-plan.md` | `6c1b2ed01a4151ead414c24bd1d2eab78f596623039f038a3f2c17c212ebb401` |
| rebuild design | `docs/plans/ai-agent-workflow-rebuild.md` | `61e43ff9c56e459a26d8419cd957eb5767623f10ea173820d018f8863a16f0be` |
| contract/profile catalog source | `docs/plans/ai-agent-workflow-step-catalog.md` | `8418e7d32291860faeb7e905b048a7ff7a03d0768b258ac9e4a75f4e7d498b6b` |
| accepted S0 plan | `docs/plans/ai-agent-workflow-s0-section-plan.md` | `ff10c21f8e6ef31c25d30226ca8c633d7d0826a92a650129cf959b7f86df35ee` |
| current implementation manifest | `agent-workflows/manifests/implementation-status.json` | `7ab62092b7031f58fc5735e8ae33e369d87208df9f53257576eb4dac4032196e` |
| fixed S0 source authority | `agent-workflows/manifests/s0-source-transition-authority.json` | `673114ec3e1ea69fcd9580805b7f872895062253a0a1ccc352f40f281ccc3070` |
| accepted S0 close index | `agent-workflows/evidence/sections/S0/index.json` | `74b10d757ef188ae5d11cbc1ac476038a148702d184e5cef5389ea8a7ab1dde4` |
| accepted S0 section result | `agent-workflows/evidence/sections/S0/section-result.json` | `4b0010abf9f7d78c4adbe8b8e2ed98d642688b685fe99552e926401eac4490ce` |
| accepted S0 bundle | `agent-workflows/evidence/sections/S0/bundle.json` | `0d2e2b0d9ddccf0779769e3208622a8e7aae51e73fd2b392c35b5a75e1932bf3` |
| accepted S0 checkpoint | `agent-workflows/evidence/sections/S0/checkpoint.json` | `6425cf29163434fc0282d15315ba684d97790c77918f4869636a66a76139f8de` |
| candidate objective | `.local/agent/runs/2026-09-01-ai-agent-workflow-rebuild/objectives/v001.md` | `38b66e58c1a24419159d1e10f4e659cdcc46dbbdfc5cbbae1e321e3325094752` |
| accepted S0→S1 compatibility plan | `docs/plans/ai-agent-workflow-s0-s1-transition-compatibility-plan.md` | `1e7673dfc6ac3d3d8912c6920320128d2ec2962f14c84957a7090e078e8a3c7b` |
| accepted compatibility manifest | `agent-workflows/manifests/section-transition-compatibility.json` | `16ba74bdf1795b3cf27bd47a2c5e49bfcda0e46c95353e2878741a0ad2e38880` |
| accepted compatibility evidence | `agent-workflows/evidence/compatibility/S0-S1-transition.json` | `ad7fb31ba24a1b46b06c52414ce28a9ae3a078bf37b1a71082f9ac03b41a8366` |
| fixed compatibility authority | `agent-workflows/manifests/section-transition-compatibility-authority.json` | `ffbb39d13310fcc266540528bb28a315e44b57fa5376b9d4b50536c548329ae8` |
| compatibility authority schema | `agent-workflows/schemas/section-transition-compatibility-authority-v1.schema.json` | `335079c2b5e1b02e1e98f1e1dd2a2ecb578da82d33c5d475724c75378ab995a0` |
| compatibility artifact schema | `agent-workflows/schemas/section-transition-compatibility-v1.schema.json` | `5f63984646f407037592684d43618bf973e9faf3cc29cdd41c09c3b3efa2e6e9` |
| fixed compatibility verifier | `agent-workflows/src/ai_agent_workflow/section_transition_evidence.py` | `2c1a1eab25976bbd6278c9301bc297d65be2e4a59e50290a5a0380733c2655d7` |
| compatibility boundary test | `agent-workflows/tests/test_section_transition_compatibility.py` | `59b2d0b12311b0ea032829316cefc4ed29ddf7cbc3e96c350796f9c19ee8f71b` |
| accepted section-control compiler | `agent-workflows/src/ai_agent_workflow/section_control_plane.py` | `582c096ae14129fa04a9d75202a733a76f3ccc8c6268a48432112bea0017163b` |
| accepted transition Kernel | `agent-workflows/src/ai_agent_workflow/control_kernel.py` | `f481f0c62b881f0ad7c6524324a68a31462d283f215030b5bbddf3f45c254c8c` |
| compatibility acceptance validation | `.local/agent/reports/ai-agent-workflow-full-implementation/s1-c-upstream-closure-validation-001.md` | `6b5242e71a401605e24d7591c144f145d85aa56100b9e7617bce1ad0bcf4be35` |

workspace baselineはGit commit
`e5b87e08d0730e0f2a2c99c9a4883a172c0078e7`と、S1 package dispatch直前にrootが取得する
owned-path digest mapの組である。既存のstaged deletion、`flake.nix`/`flake.lock`変更、
untracked `agent-workflows/`と`docs/`は利用者または先行成果として保護する。`git status`の
一括clean、既存fileのwhole-file置換、帰属不能diffのadoptを行わない。

入力digestの一つでも変わった場合、未開始Taskは再packageし、実行中Taskは結果を受理せず
`blocked_integrity`へ送る。accepted S0 evidenceは変更せず、S1 close evidenceから参照する。

## 3. authorityとsource-only境界

許可するwriteは次に限定する。

- 本S1 plan、ignored ledger/report
- `agent-workflows/`配下のA1〜A5 Skill source
- S1-owned Bootstrap/shared closure source、schema、manifest、test、fixture、contract evidence
- S1 integration/status projectionと`evidence/sections/S1/`

次はS1の権限外である。

- `control_kernel.py`、`section_control_plane.py`、S0 schema/manifest/evidenceの再定義または変更
- 既存A6/A6R/A7 Skill、selector、contract evidenceの変更
- current/manual/new-kernel Run、runtime pointer、actual A7 handoff、migration/cutover/rollback operation
- Nix build/check/rebuild/switch、Home Manager、Codex app-owned config、credential、external mutation
- S2〜S8 implementation、Git stage/commit/push、既存差分の削除

S1 fixtureのexecution classは`candidate-generic`であり、実環境のworkspace作成、legacy detach、
Run初期化、clear、handoffを実行しない。Skillはplan/resultを生成するcontractを持てるが、
destructive/external operation直前にはその操作固有のhuman authorityを要求する。

parent objectiveは上表の`v001` candidateであり、S1 sourceのexecution classは
`candidate-generic`に固定する。source作成のhuman receiptは利用者の
`user-2026-09-04-full-source-before-rebuild`決定をledgerへ保存したものとする。このreceiptは
actual workspace作成、detach、A7、migration、activation、Gitを承認しない。objective approval pointerが
存在しない間、B7相当またはobjective-dependent実行はtyped refusalにする。

`additional-required-surfaces/v1`のS1 deltaは0件である。S0が登録した11 stable surfaceはいずれも
S7 ownerで、S1はacceptedへ昇格させない。一方、full readinessのrequired path
`agent-workflows/groups`はS1の二manifestで`present`になるが、全Group manifestが揃うまで
`group-manifests-implemented` completion gateはpendingのままにする。

## 4. canonical 16-contract map

この表がS1 contract bindingの唯一の正本である。新規sourceのdigestはacceptance時にreceiptへ
実byte digestを必須記録し、null/placeholderを受理しない。`input -> output`はfresh physical refを表し、
会話summaryを入力にしない。

| qualified ID / kind | canonical source | owner / public interface | fresh input -> output | completion / stop and authority | executable selector / evidence |
| --- | --- | --- | --- | --- | --- |
| `group.A.A1` v1 / skill | `agent-workflows/skills/bootstrap-classify-environment/SKILL.md` | `agent-workflows.group.A` / `BootstrapContractsV1.classify_environment` | repo root、Git state、AI/Nix/runtime paths -> `bootstrap-artifact/v1` inventory | 全pathにclass/owner/source/retention/inspection、unknown保持 / unknown ownerへのwriteでstop | `agent-workflows/tests/test_bootstrap_contracts.py::A1_EnvironmentInventoryTests` / `agent-workflows/evidence/contracts/A1.json` |
| `group.A.A2` v1 / skill | `agent-workflows/skills/bootstrap-isolate-workspace/SKILL.md` | same / `plan_workspace` | A1 inventory、ref、dirty state -> workspace plan | diff/write scope非重複、ref/local input/rollback明記 / branch/ref/import判断またはoverlapでstop | `agent-workflows/tests/test_bootstrap_contracts.py::A2_WorkspaceIsolationTests` / `agent-workflows/evidence/contracts/A2.json` |
| `group.A.A3` v1 / skill | `agent-workflows/skills/bootstrap-design-rollback/SKILL.md` | same / `design_rollback` | inventory、workspace plan -> rollback plan | owner限定restoreとactivation前後recovery / restore source欠損、secret backupでstop | `agent-workflows/tests/test_bootstrap_contracts.py::A3_RollbackDesignTests` / `agent-workflows/evidence/contracts/A3.json` |
| `group.A.A4` v1 / skill | `agent-workflows/skills/bootstrap-initialize-run/SKILL.md` | same / `initialize_run_plan` | approved plan、candidate objective、workspace plan -> initial Run plan/event/checkpoint artifacts | fresh readerがobjective/current/next/unapprovedを説明 / objectiveがapproval提案不能でstop | `agent-workflows/tests/test_bootstrap_contracts.py::A4_RunInitializationTests` / `agent-workflows/evidence/contracts/A4.json` |
| `group.A.A5` v1 / skill | `agent-workflows/skills/bootstrap-detach-legacy-owner/SKILL.md` | same / `plan_legacy_detach` | inventory、rollback、non-use receipt -> detach plan/A6 input bundle | runtime owner分離、research保持、P1-P4/A6 refs結合 / approval、delete、activation、Gitでstop | `agent-workflows/tests/test_bootstrap_contracts.py::A5_LegacyDetachTests` / `agent-workflows/evidence/contracts/A5.json` |
| `group.A.A6` v1 / skill | `agent-workflows/skills/bootstrap-build-walking-skeleton/SKILL.md` (`5746e91d41ac3297cbb30bf88fe7c9846aa278c798763dba13167826c47bb133`) | `agent-workflows.group.A` / existing `core` interface | closed A5 bundle/checkpoint -> A6 receipt | existing fresh-process completion/stopを保持 | `agent-workflows/tests/test_walking_skeleton.py::A6_WalkingSkeletonTests` / `agent-workflows/evidence/contracts/A6.json` (`7366d75dea974847f82fb1a4d9dd23761a56bea78a2b7b11a38dea7c3bd6cbc2`) |
| `group.A.A6R` v1 / skill | `agent-workflows/skills/bootstrap-migrate-control-kernel/SKILL.md` (`6301f4adeff6cbdc4d49e25bcce0e647fd8f05ce3adc278eaf86decde96fed59`) | same / existing `migration` + `ControlKernel` | attested copied A6 set + authority -> source-only migration evidence | G1-G8/source replay、operational adoptionなし / gate失敗でstop | `agent-workflows/tests/test_a6r_evidence.py::A6R_EvidenceTests` / `agent-workflows/evidence/contracts/A6R.json` (`1eb28fc0dffed9e3eb8f764853a6096f5910129d185ebf66f42fdc576c31cf5e`) |
| `group.A.A7` v1 / skill | `agent-workflows/skills/bootstrap-self-host-handoff/SKILL.md` (`e2b061ee152163b2a3cd5659d240aaa3e28afe3d1af8bedede70a8257722c2fa`) | same / existing `SelfHostHandoff` | manual snapshot、G1-G8、approval、HEAD -> handoff package/status | source contractのみ、actual claimは別 / identity/gate/approval不一致でstop | `agent-workflows/tests/test_a7_self_host_handoff.py::A7_SelfHostHandoffTests` / `agent-workflows/evidence/contracts/A7.json` (`0cb555c81965fefafa07acbd95c049c8d6b8df972d5f5b2d7ce3041f3c59aa67`) |
| `group.F.F1` v1 / shared-protocol-operation | `agent-workflows/src/ai_agent_workflow/closure_protocol.py` | `agent-workflows.group.F` / `SharedClosureProtocolV1.audit_group_purpose` | objective/subobjective、Group results -> alignment result | evidence-bound aligned/uncertain/diverged / divergentまたはmissing objectiveでreplan/user stop | `agent-workflows/tests/test_shared_closure_protocol.py::SharedClosureProtocolTests.test_f1_audit_group_purpose` / `agent-workflows/evidence/contracts/F1.json` |
| `group.F.F2` v1 / shared-protocol-operation | same source | same / `collect_group_artifacts` | projections/results/reviews/Findings -> bundle inventory | canonical/partial/unverified全件をdigest分類 / missing required artifactでstop | `agent-workflows/tests/test_shared_closure_protocol.py::SharedClosureProtocolTests.test_f2_collect_group_artifacts` / `agent-workflows/evidence/contracts/F2.json` |
| `group.F.F3` v1 / shared-protocol-operation | same source | same / `extract_decision_candidates` | events/reports/artifacts -> decision candidates | proposal/observation/assumptionを未承認保持 / auto-promotion要求でstop | `agent-workflows/tests/test_shared_closure_protocol.py::SharedClosureProtocolTests.test_f3_extract_decision_candidates` / `agent-workflows/evidence/contracts/F3.json` |
| `group.F.F4` v1 / shared-protocol-operation | same source | same / `replan_future` | accepted plan/history + Group result + DAG -> future plan revision | future frontierだけ変更 / objective/scope/risk/budget/owner changeはauthority route | `agent-workflows/tests/test_shared_closure_protocol.py::SharedClosureProtocolTests.test_f4_replan_future` / `agent-workflows/evidence/contracts/F4.json` |
| `group.F.F5` v1 / shared-protocol-operation | same source | same / `advise_next_group` | future plan、unresolved、preconditions -> next advice | workflow/state/checkpoint/input/clearへbind / unmet prerequisiteでstop | `agent-workflows/tests/test_shared_closure_protocol.py::SharedClosureProtocolTests.test_f5_advise_next_group` / `agent-workflows/evidence/contracts/F5.json` |
| `group.F.F6` v1 / shared-protocol-operation | same source | same / `write_checkpoint` | F1-F5 refs、authority、HEAD -> bundle/checkpoint command/result | fresh readerがnext/stopを復元 / incomplete refs、stale HEADでstop | `agent-workflows/tests/test_shared_closure_protocol.py::SharedClosureProtocolTests.test_f6_write_checkpoint` / `agent-workflows/evidence/contracts/F6.json` |
| `group.F.F7` v1 / shared-protocol-operation | same source | same / `clear_boundary` | accepted close receipt、authority、HEAD -> clear guidance/receipt | HEAD commit後、unsaved contextなし、自動実行なし / pre-close、stale receiptでstop | `agent-workflows/tests/test_shared_closure_protocol.py::SharedClosureProtocolTests.test_f7_clear_boundary` / `agent-workflows/evidence/contracts/F7.json` |
| `group.F.F8` v1 / shared-protocol-operation | same source | same / `resume` | identifier、expected HEAD -> resume brief/refusal | parent/digest/objective/version/revision検証、projection再生成 / corrupt canonical chainでstop | `agent-workflows/tests/test_shared_closure_protocol.py::SharedClosureProtocolTests.test_f8_resume` / `agent-workflows/evidence/contracts/F8.json` |

A1〜A5 Skillは既存Bootstrap Skillと同じ`disable-model-invocation: true`で、明示起動のみ。
frontmatterはname、discriminating description、上表のacceptance-testを持つ。本文はpurpose、fresh input、
authority、ordered steps、checkable completion、stop、handoffだけを保持し、schema/Kernel/F protocolを
複製しない。F machine selectorはqualified IDだけで、bare `F1`〜`F8`は表示aliasに限定する。

## 5. public seamとsingle owner

S1が追加する公開seamは二つだけである。

```text
BootstrapContractsV1
  compile(operation, qualified_id, inputs, authority, expected_head)
    -> bootstrap-artifact/v1 | typed refusal

SharedClosureProtocolV1
  compile(operation, qualified_id, inputs, authority, expected_head)
    -> closure-operation-result/v1 | dag-command/v1 | typed refusal
```

`bootstrap_contracts.py`はA1〜A5のpure validation/compilation owner、
`closure_protocol.py`はF1〜F8のpure validation/compilation ownerである。
`bootstrap_lifecycle.py`は両者のresultを既存Kernel commandへ変換するthin consumerとし、
`ControlKernel.apply`、S0 `open_section/open_group/status/resume`へ一回だけ委譲する。

state、immutable object、transaction、CAS、authority、idempotency、HEAD、projection、orphan recoveryの
唯一ownerは`ControlKernel`である。section/group mapping、schedule、open command、status/resume classifierの
唯一ownerはS0 `SectionControlPlaneV1`である。S1 moduleはfile-backed state、独自HEAD/pointer、
別reducer、別schedule compilerを持たない。

`core.WorkflowStore`はA6 historical compatibility readerであり、S1 current state ownerへ昇格しない。
`migration.py`と`a7.py`はcopied-source rehearsal/operational handoff用で、S1 source fixtureのshortcutにしない。

### 5.1 S0 upstream compatibility gate

`S1-LIFE-001`のownerはS1ではなくS0/Kernelである。S1の全implementation dispatch前に
`docs/plans/ai-agent-workflow-s0-s1-transition-compatibility-plan.md`を独立plan/review/validation/
implementation loopで閉じ、次を得た。

- manifest: `agent-workflows/manifests/section-transition-compatibility.json`
- evidence: `agent-workflows/evidence/compatibility/S0-S1-transition.json`
- schema: `section-transition-compatibility-v1`
- accepted `section_control_plane.py`と`control_kernel.py` digest
- S0 unchanged、valid S0→S1、strict successor、CAS/idempotency、crash/orphan/cold-resume receipt

受理済みreceiptは7-path aggregate
`58dd232791977ec3de47ee4b6b2b51c567f48c7c08c9661ec40950f772d5d921`、fixed authority
`ffbb39d13310fcc266540528bb28a315e44b57fa5376b9d4b50536c548329ae8`で、§2のactual digestに
binding済みである。4 canonical Findingはfresh closure review/validationにより各2/5で
closed、open required/needs-user/test-evidence-debt/duplicate-aliasは各0と確定し、本planは
`accepted`である。S1-A/Bのみdigest-bound implementation packageでdispatchできる。互換artifactの
fixed verifier replayが失敗した場合はS1も`terminal_non_dispatch`で停止し、private simulatorや
第二ownerへ迂回しない。

## 6. schemas、manifests、source paths

S1で新設するcanonical pathsを次に固定する。

- `agent-workflows/groups/bootstrap.json`
- `agent-workflows/groups/shared-closure.json`
- `agent-workflows/schemas/group-manifest-v1.schema.json`
- `agent-workflows/schemas/bootstrap-artifact-v1.schema.json`
- `agent-workflows/schemas/shared-closure-manifest-v1.schema.json`
- `agent-workflows/schemas/closure-operation-result-v1.schema.json`
- `agent-workflows/schemas/next-group-advice-v1.schema.json`
- `agent-workflows/schemas/s1-source-authority-v1.schema.json`
- `agent-workflows/src/ai_agent_workflow/bootstrap_contracts.py`
- `agent-workflows/src/ai_agent_workflow/closure_protocol.py`
- `agent-workflows/src/ai_agent_workflow/bootstrap_lifecycle.py`
- `agent-workflows/src/ai_agent_workflow/s1_evidence.py`
- `agent-workflows/src/ai_agent_workflow/implementation_status.py`（S1-D compatibility extension only）
- `agent-workflows/manifests/s1-source-authority.json`
- `agent-workflows/manifests/implementation-status.json`（S1-D entries only）
- `agent-workflows/schemas/implementation-status-v1.schema.json`（shared-operation branch only）
- `agent-workflows/tests/test_bootstrap_contracts.py`
- `agent-workflows/tests/test_shared_closure_protocol.py`
- `agent-workflows/tests/test_s1_lifecycle_integration.py`
- `agent-workflows/tests/test_s1_evidence.py`
- `agent-workflows/tests/test_implementation_status.py`（S1-D cases only）
- `agent-workflows/tests/fixtures/s1/`
- `docs/ai-agent-workflow-usage.html`（renderer output only）

既存`artifact-bundle/v1`、`checkpoint/v1`、`dag-command/v1`、`section-result/v1`、
`section-review/v1`、`finding-validation-set/v1`、`section-bundle/v1`、
`qualified-contract-acceptance/v1`を再利用する。新schemaは上記で表せないS1 documentだけに限定し、
Kernel state/command schemaをforkしない。全schemaはnested `additionalProperties: false`を原則にする。

S1-Dは既存completion classifierをsingle ownerのまま次の形へ互換拡張する。

- `implementation-status-v1.schema.json`へ`implemented_shared_operations`を追加し、既存
  `implemented_contracts`（skill）と`implemented_profile_steps`をそのまま保持する。
- 各shared entryは`id=group.F.Fn`、`implementation_kind=shared-protocol-operation`、artifact path
  `agent-workflows/src/ai_agent_workflow/closure_protocol.py`、qualified selector、exactly one
  `qualified-contract-acceptance/v1` receiptを持つ。
- `implementation_status.py`の同じevaluatorが、S0 registryのqualified targetとname/kind/interface、
  module digest、receipt contract ID、per-operation executable test selector、evidence digest/statusを照合する。
- A1〜A7/A6Rは既存`implemented_contracts`のbare ID compatibilityを維持し、内部で
  `group.A.<ID>`へ一意にprojectする。A6/A6R/A7のsource/evidence bytesは変更しない。
- accepted named countはA skill qualified setとF shared-operation qualified setのunionから導出し、
  重複、bare F alias、profile F1〜F8とのcross-bindingを拒否する。

direct/CLI/HTML、S0 regression、forged/cross-bound receipt、wrong module digest、non-executable selector、
duplicate qualified IDをtestし、S1以外のclaimを変えない。

## 7. artifact DAGとowned write scope

| artifact | owner | dependencies | exclusive write scope | acceptance |
| --- | --- | --- | --- | --- |
| S1-P section plan | root | accepted S0 | this file、ledger | plan review + validation、open required 0 |
| S1-U upstream compatibility | S0/Kernel Compatibility Workers | reviewed compatibility plan | S0-owner paths in compatibility plan only | accepted manifest/evidence/source digests; S1 dispatch gate |
| S1-A Bootstrap contracts | Bootstrap Worker | accepted S1-P + S1-U | A1〜A5 Skill dirs、`bootstrap_contracts.py`、bootstrap artifact schema、`test_bootstrap_contracts.py`、A fixtures | five Skills/operations、strict authority/stop、focused tests |
| S1-B shared closure | Closure Worker | accepted S1-P + S1-U | `closure_protocol.py`、shared closure manifest/schema/result/advice schemas、`test_shared_closure_protocol.py`、F fixtures | F1〜F8 selectors/receipts、future-only replan、checkpoint/clear/resume compile |
| S1-C composition/lifecycle | Lifecycle Worker | accepted S1-A/B | two Group manifests、group schema、`bootstrap_lifecycle.py`、`test_s1_lifecycle_integration.py`、composition fixtures | 16 exact mapping、S0 consumer、multi-Epoch/Group/crash/cold resume |
| S1-D integration/coverage candidate | Integration Worker | accepted S1-C | new A1〜A5/F1〜F8 contract evidence、`implementation_status.py`、status schema/manifest/tests/HTML projection、integration fixtures | shared-operation compatibility、16/60、0/23、false claims preserved; final accepted/indexはまだ作らない |
| S1-R1 artifact/integration reviews | fresh Reviewers | candidate S1-A〜D | unique ignored reports | spec/Skill、lifecycle/authority、evidence/status candidates |
| S1-V1 validation | fresh Validator | all S1-R1 reports | unique ignored report | each candidate once、dedup/materiality、required-only owner |
| S1-F1 feedback/rereview | narrow Workers + fresh Reviewers | validated Finding | exact Finding-owned paths only | closed or attempt 5/5 terminal |
| S1-E final evidence | Evidence Finalizer + separate Anchor Worker | S1-A〜D accepted、V1 open required 0 | `evidence/sections/S1/`、`s1_evidence.py`、S1 authority schema/manifest/test | physical close set、terminal history、fixed external authority |
| S1-R2 final truth/DAG reviews | fresh Reviewers | S1-E candidate | unique ignored reports | physical truth、complete graph、external anchor、status parity candidates |
| S1-V2 final validation/feedback | fresh Validator + narrow Workers | S1-R2 reports | reports + exact required paths only | open required 0 after fresh closure |

S1-AとS1-Bは最初のready frontierで並列実装する。S1-Cは両者accepted後、S1-DはC accepted後に
開始する。S1-EはA〜Dのreview/validation/feedbackがterminalかつopen required 0になってからだけ
final close setを物理化する。同じschema、manifest、test、receipt pathを複数workerへ渡さない。
`implementation_status.py`変更が必要ならS1-Dだけが最小scopeを持つが、既存S0 claim/seamを
壊す変更はupstream Findingとして停止し、無断修正しない。

## 8. Artifact loopとTDD slices

各S1-A〜Dは次を実行する。

```text
digest-bound package
  -> public behavior red test
  -> minimum green source
  -> focused regression
  -> candidate review
  -> fresh Finding validation/dedup/materiality
  -> required-only feedback
  -> fresh closure review
  -> accepted | bounded terminal
```

Workerは自分のFindingをcloseしない。Reviewerは修正を指示できるcandidateを出すが、fix authorityを
持たない。Validatorは妥当性、既存decision、重複、materialityを判定し、requiredだけをfixへ送る。
表記差、既知decision、意図したtrade-off、証拠付きdefer、軽微な好みはblocking Findingにしない。

### Slice A: Bootstrap contracts

Red: unknown ownerを安全classへ推測する、foreign diffとscopeが重なるworkspace planを通す、
secretをrollbackへ入れる、未承認objectiveをapprovedとしてRunへ入れる、non-use authorityなしで
legacy detachを実行可能とする。

Green: A1〜A5はstrict artifact/refusalを返し、unknown、approval、owned path、restore source、
objective statusを物理refへbindする。candidate-generic fixtureは外部operationを実行しない。

### Slice B: shared closure

Red: alignment推測、artifact omission、proposalのdecision昇格、accepted history rewrite、stale advice、
incomplete checkpoint、pre-close clear、corrupt resumeが通る。

Green: F1〜F8の各selector/result/receiptを分け、F4はfuture frontierのみ変更する。F6/F7は
Kernel commandをcompileするだけで、F8はS0 resume/Kernel HEAD chainだけを読む。

### Slice C: composition and lifecycle

Red: Group manifestがunknown/duplicate contract、bare F alias、A6 receiptのcross-binding、S0 parentなしの
open、同一key別payload、open Epochを残したGroup close、clear前のnext Groupを受理する。

Green: accepted S0 parentからS1を一transactionで開き、Bootstrap内の複数Epochをclose/openし、
別Groupを開閉する。exact retryはcanonical receipt、conflict/stale HEADはno mutationでrejectする。

### Slice D: integration coverage and status candidate

Red: 16というcount literal、sourceだけ、他contract receipt、non-executable selector、S0 evidence、
A7 source acceptanceのいずれかでS1/full-readyを偽陽性にする。

Green: 13新規contractはsource+selector+receipt、既存3件は元digestのままexactly onceで数え、
named 16/60、profile 0/23を導出する。full-ready、actual A7、migration、activationはfalseである。
このsliceはcontract receiptsとstatus/integration evidenceまでを作り、まだ存在しないfinal review/
validationをplaceholderで埋めたり、S1 accepted/indexを先に発行したりしない。

## 9. Integration loopとfault matrix

S1-C/Dはaccepted S1-A/Bのartifact/result digestだけを入力にする。変更されたinputのreverse
`requires` closureだけをinvalidateし、影響外accepted siblingとA6/A6R/A7 historyを保持する。

positive fixtureは次を最低限含む。

```text
accepted S0 section receipt
  -> open S1 / Bootstrap first Epoch
  -> A1..A5 source artifacts + F1..F6 checkpoint
  -> F7 clear
  -> next Bootstrap Epoch with retained A history
  -> A6/A6R/A7 source mappings
  -> close Bootstrap
  -> open and close a second synthetic Group
  -> delete projection
  -> fresh-process F8 resume from immutable HEAD chain
```

actual A7 handoffは実行しない。synthetic second Groupはlifecycle isolationを検証するfixtureであり、
S2 objective approvalやB contract acceptanceを捏造しない。

| fault | required observation |
| --- | --- |
| wrong/missing S0 parent or digest | typed reject、HEAD/ready/history不変 |
| stale HEAD、wrong role/scope、expired authority | no object/transaction/projection mutation |
| forged source/evidence/selector、cross-bound receipt | relevant contract remains unaccepted |
| exact retry / same key changed payload | original receipt/revision / typed conflict |
| accepted A history or old checkpoint mutation | integrity block; future replan cannot rewrite it |
| open Epoch、lease/running Task、blocking Findingでclose | close reject、clear/adviceなし |
| crash before publish | staging quarantine、HEAD不変 |
| publish後HEAD前 | orphan report、automatic adopt/openなし |
| HEAD後projection前、projection delete/forge | HEADから同一projectionをrebuild |
| worker loss/late result/overlapping replacement | late result quarantine、replacement block until fresh authority |
| corrupt object/parent chain | `blocked_integrity`; projection rebuildで隠さない |
| source A7 receipt without operational gates | source coverageだけ、actual handoff false |

focused A/F/C/D tests、全schema JSON parse、implementation-status direct/CLI/HTML projection、
fresh-process fixture、Python full discovery、`git diff --check`を実行する。Nix commandは実行しない。

統合candidateの二段reviewとrequired-only feedbackが閉じた後だけS1-Eがfinal close setを作る。
その物理成果物は別のfresh truth/DAG reviewとfresh Validatorを通す。final reviewでrequired gapが
見つかれば同一Findingの残attempt内で修正し、再物理化・fresh closureを行う。これにより未実施の
review/validationを自己申告したplaceholder closeや、self-consistentな全再digestを受理しない。

## 10. plan review、Finding validation、finite convergence

本planは実装前に最低二つのfresh reviewを並列で受ける。

1. contracts/Skill/coverage review: 16 IDs、5 Skill+8 operations+3 retained、source/selector/evidence、
   progressive disclosure、catalogとの整合
2. lifecycle/authority/evidence review: S0 consumer、single owner、A history、crash/cold resume、
   status/claim、owned paths、source-only境界

両reviewのcandidateを別のfresh Validatorが一度だけ分類・fingerprint dedupする。requiredだけを
本planへfeedbackし、fresh closure reviewでopen required 0を確認してからS1-A/Bをdispatchする。

実装中もartifact reviewとintegration/section reviewを最大2 roundsずつ行う。canonical Findingごとの
fix attempts上限は5。attempt履歴をresetせず、同一root causeを新IDに分割しない。attempt 5で未解決、
needs-user、authority欠損、deadline/budget不足は`terminal_non_dispatch`またはhuman routeへ送る。

parallel policyは`uncapped-ready-frontier`であり、人工的なworker数上限を持たない。実効並列度は
dependency、exclusive write scope、harness/review capacity、fresh review capacity、残budgetだけで決める。

## 11. Section close artifacts

S1 close時に次を物理化する。

- `agent-workflows/evidence/contracts/A1.json`〜`A5.json`
- `agent-workflows/evidence/contracts/F1.json`〜`F8.json`
- A6/A6R/A7の既存contract evidenceへの不変参照
- `evidence/sections/S1/`のartifact/result、plan、catalog、transaction、review、validation、
  `section-result/v1`、`section-bundle/v1`、`checkpoint/v1`、accepted/index、S2 next-input
- acceptance test command、exit status、test count、source/selector/evidence digests
- accepted/invalidated/unresolved、Finding attempt/budget、claims vector、S0 parent refs

S1 close graphはcaller-root contained、exact path、distinct node/edge identity、acyclic、reachable、
fully materializedでなければならない。固定S1 authority protocolは次の二段publishで構成する。

1. V1 open required 0を確認したroot Finalizerだけが、close graph外のregular file
   `agent-workflows/manifests/s1-source-authority.json`を発行する。strict schemaはplan/catalog、S0 parent
   index/result/bundle/checkpoint、S0→S1 compatibility receipt/source digests、16 contract source/evidence、
   canonical Finding/attempt history、expected HEAD/checkpoint/next-S2 input、test command/count/exit、
   false claim vector、authority version/issuerを全てexactly bindする。
2. 別assignmentのAnchor Workerがmanifest bytesを再検証して、そのSHA-256をclose graph外のverifier
   `agent-workflows/src/ai_agent_workflow/s1_evidence.py`内の固定constantへpinする。index/bundleから
   authority path/digestを指定できない。caller-root regular-file/no-symlink containmentを検査する。

issuerは`codex-root`、versionは`S1-source-authority/v1`。rotationはaccepted plan revision、理由、旧/新
digest、fresh truth/DAG reviewとValidatorを要し、旧S1 acceptanceをinvalidateする。同時再digestした
authority/index/plan/catalog/S0 parent/A receipts/F receipt/Finding history/checkpoint/next input/test receiptを
個別に改変するpersistent negative matrixを持つ。matching disposable authorityなら内部graphが通り、
checked-in fixed authorityでは各置換が拒否されることを確認する。

`implementation-status`とHTMLはaccepted evidenceからrendererで再計算する。次を同時表示できない場合は
閉じない。

- S0 registry/transition claim: true
- S1 Bootstrap/shared lifecycle source complete: true
- named contracts: 16/60
- profile steps: 0/23
- actual A7 handoff、migration、activation、source-wide integration、full Workflow ready: false

## 12. recovery、rollback、stop

各Task packageはworkspace identity、Git baseline、owned-path baseline digests、accepted input digests、
expected HEAD、actor/assignment、write scope、report path、test command、review/fix budgetを持つ。
lost worker diffはassignment-owned bytesと証明できる場合だけtransferする。帰属不能またはscope外bytesは
変更せずquarantine referenceを残し、重なるreplacementを止める。

停止条件:

- §2のdigest、expected HEAD、S0 accepted claim、owned-path baselineが変わる
- S0/Kernel-owned surfaceの変更なしにS1を実装できない
- A6/A6R/A7の既存source/evidenceを変更する必要が生じる
- current Run、actual A7、migration、activation、Nix、credential、Git authorityが必要になる
- objective-dependent B7/pilot/H1〜H3 evidenceをS1 acceptanceへ流用する必要が生じる
- required artifact/selector/receiptを正本から一意にcompileできない
- open required、needs-user、integrity block、attempt 5/5 terminalが残る

projectionだけが壊れた場合はHEADから再生成する。immutable object/parent chainが壊れた場合は停止する。
rollbackはS1の新規source bytesをowned-path単位で隔離できることまでとし、既存S0/A6/A6R/A7 bytesや
利用者diffを巻き戻さない。

## 13. validated plan feedback trace

| canonical Finding | disposition / attempt | applied feedback | closure prerequisite |
| --- | --- | --- | --- |
| `S1-CSR-001` | pass/closed 2/5 | §4をcanonical 16-row mapへ置換し、exact source/owner/interface/input/output/completion/stop/selector/evidenceを固定 | contracts closure review pass |
| `S1-LIFE-001` | pass/closed 2/5 | §5.1/S1-Uでaccepted compatibility aggregate/authority/manifest/evidence/schema/source/validationを§2へbind | lifecycle closure review + fixed replay pass |
| `S1-LIFE-002` | pass/closed 2/5 | §6/S1-Dへsingle classifierの`implemented_shared_operations`表現とcompatibility/negative testsを固定 | lifecycle/status closure review pass |
| `S1-LIFE-003` | pass/closed 2/5 | §6/§11へS1 authority path/schema/verifier、two-step issuer/pin/rotation、full-redigest matrixを固定 | authority closure review pass |

review candidate 4件はfresh Validatorによりdistinct required 4、defer/reject/needs-user 0と判定された。
本feedbackは4件のminimum sufficient correctionとaccepted upstream actual digestsだけを反映し、
新しい実装scopeやFindingを追加しない。fresh validation report
`.local/agent/reports/ai-agent-workflow-full-implementation/s1-plan-closure-validation-001.md`
(`9f797b7c065cd0a7bffa6abc9acd4038dbcffae4866181182457aa1192b413fa`)が4件の2/5 closure、
open required/needs-user/test-evidence-debt/duplicate-aliasの各0、review round 2/2 terminalを確定した。

## 14. exact first action

accepted plan digest、accepted compatibility receipt、Git HEAD、exact owned-path baselineをbindした
S1-A Bootstrap contractsとS1-B shared closureのimplementation packageを発行し、別owned write
scopeで並列TDD実装する。S1-Cは両artifactのfresh review/validation後までdispatchしない。
