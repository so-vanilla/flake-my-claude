---
schema: section-plan/v1
section_id: S2
version: v2-replan
status: accepted
objective_execution_class: candidate-generic
execution_contract: workflow-execution/v2
parallel_policy: uncapped-ready-frontier
review_round_budget: 2
finding_attempt_budget: 5
section_budget_profile: s2-source-section-budget/v1
section_wall_clock_minutes: 480
section_minimum_closure_reserve_minutes: 45
---

# S2 Objective and outcome system section plan

基準日: 2026-09-05（Asia/Tokyo）

## 1. 目的とsource-level exit

S2はaccepted S0/S1のSection transition、HEAD/CAS、shared F1〜F8を再利用し、目的理解・
明示承認・outcome分解・計測設計をsourceとして完成させる。対象は次の13 named Skillである。

- Group B: `group.B.B1`〜`group.B.B7` 7件
- Group C: `group.C.C1`〜`group.C.C6` 6件

13個のSkillはcatalog entrypointとして物理化するが、domain semanticsを13個のruntime moduleへ
分裂させない。公開domain ownerは`ObjectiveSystemV1`と`OutcomeSystemV1`、物理証拠ownerは
`S2EvidenceCompiler`とする。S0 Control Kernelだけがstate/HEADを更新し、S1
`SharedClosureProtocolV1`だけがcheckpoint/clear/resume commandをcompileする。

source-level exitは次の全てである。

1. B1〜B7/C1〜C6がqualified Skill source、deep Module interface、strict schema、実行可能な
   acceptance selector、digest-bound evidenceへexactly onceで結合される。
2. B7 positive/negative fixtureが、明示human receiptによるatomic approvalと、implicit/AI/
   task-start/fixture-to-live流用拒否を同じpublic seamで実証する。
3. C1〜C2のC-01 bundleとC3〜C6のC-02 bundleが別Context Epochとして閉じ、C-01 byte変更で
   current C-02だけがinvalidateされ、historyを保持したfresh resumeが通る。
4. baselineの取得不能、stale、access denied、side effect、incomparableは`unavailable`であり、
   0、pass、fail、percentage changeへ変換されない。
5. V2 Inner/Middle/Outer loopの二軸reviewと単一Validator後、open required/new material root/
   needs-user/integrity errorが0である。
6. `implementation-status`はnamed 29/60、profile 0/23を物理evidenceから導出し、current Runの
   `objective-approved`、actual A7、migration、activation、source-wide/full readyをfalse/pendingに保つ。

S2のsource acceptanceはlive objective approvalではない。current Runの
`objectives/v001.md`、approved pointer `null`、既存Run stateは変更しない。

## 2. digest-bound inputsとplanning provenance

| input | path | SHA-256 |
| --- | --- | --- |
| canonical overall plan | `docs/plans/ai-agent-workflow-full-implementation-plan.md` | `c8e10469fcf310e913f7520990b833e259daf5d0d3144defdcd4ba31d5b46524` |
| rebuild design | `docs/plans/ai-agent-workflow-rebuild.md` | `f4e71ef2c587d12df19dd13affdb929ac8995269448a2218656927694ed966b3` |
| 60/23 step catalog | `docs/plans/ai-agent-workflow-step-catalog.md` | `4ae2f11b89eedabc4c4a5cf71f96d4041c94ed58a48f6c1e75172ae648fe7c0d` |
| accepted S1 plan | `docs/plans/ai-agent-workflow-s1-section-plan.md` | `66613b6aeb3739d62f34c20b3732191dd1cd60aaa8ee49b15f3e845eb5d73dbc` |
| parent S1 acceptance | `.local/agent/workplans/ai-agent-workflow-full-implementation/s1-acceptance-001.json` | `14de27404218b2a5b58a0e8b66dbb4ba3c710b1f5fa7d1630f13e49371eb3cdf` |
| S1 fixed source authority（successor rotation前の観測値） | `agent-workflows/manifests/s1-source-authority.json` | `ec4618cb5aab0384c49b07c827b768dd78a8dc90995fc90ffd855b9cf017879f` |
| S1 close index | `agent-workflows/evidence/sections/S1/index.json` | `0dc2ec887d8df553791abad69b6fc84186d114f01e7d13886bce015b732988c6` |
| S1 section bundle | `agent-workflows/evidence/sections/S1/bundle.json` | `a889f5d8e3313b4f83bb2c925f265019ed1edf48f3bf8c909b95da4318956ddb` |
| S1 checkpoint | `agent-workflows/evidence/sections/S1/checkpoint.json` | `b38d1c83652018d5f7d7a07ded040890e00e596367ee4d02c1438dec25262468` |
| S1 next input | `agent-workflows/evidence/sections/S1/next-S2.json` | `4846db980aa630bb45461c00f6d6f611038bd9e91a07d6d8d6b075aea6142a45` |
| current status | `agent-workflows/manifests/implementation-status.json` | `cf788babd63a8554065360ea69fce2eb64d6743b690d124b42b5efc00597539c` |
| V2 cutover record | `.local/agent/workplans/ai-agent-workflow-full-implementation/workflow-v2-cutover-001.json` | `cf6e81fe465ebf6abd834159e6295b953f61b266b7ee78f6620e05ee8ff6949d` |
| candidate objective | `.local/agent/runs/2026-09-01-ai-agent-workflow-rebuild/objectives/v001.md` | `38b66e58c1a24419159d1e10f4e659cdcc46dbbdfc5cbbae1e321e3325094752` |
| current Run observation | `.local/agent/runs/2026-09-01-ai-agent-workflow-rebuild/run.yaml` | `db12a4ee47d89b2e7d4c2b6056abef00adf3d524d40cfd78075db1e78a39dbeb` |
| B mapper | `.local/agent/reports/ai-agent-workflow-full-implementation/s2-plan-objective-001.md` | `1cc66103604da1871ad7ed09a8918717596c02752aab29ce586d4d80a5c0de05` |
| C mapper | `.local/agent/reports/ai-agent-workflow-full-implementation/s2-plan-outcomes-001.md` | `d1a2e261c858b6cafe74bd4d9dfc3821ae41604ccdb223c4ff2000b35d50d9d4` |
| integration mapper | `.local/agent/reports/ai-agent-workflow-full-implementation/s2-plan-integration-evidence-001.md` | `bd761e03cdb9bd5e55c81702ac1cd1a8aeb2406fff3dee37519e85f2323479c9` |
| common-schema closure mapper | `.local/agent/reports/ai-agent-workflow-full-implementation/s2-authority-replan-closure-map-001.md` | `541949d223efc24de296043acb29d1d00a1d60b7354d170dc01a92d148827c2b` |
| successor-order mapper | `.local/agent/reports/ai-agent-workflow-full-implementation/s2-authority-replan-order-map-001.md` | `112e4e94f507b6656e484c13b6ee49f6e18240d66b975b881bb3d9c17989b83a` |
| pre-replan S2 terminal | `.local/agent/workplans/ai-agent-workflow-full-implementation/s2-fast-mode-section-terminal-001.json` | `ab69f0321205399c37444b237ce221f06c30b4190d08744364047a581dd5697b` |
| corrected retained S2-U product candidate | `.local/agent/workplans/ai-agent-workflow-full-implementation/s2-u-product-fast-candidate-001.json` | `25e85d85cef29822d2152164a65d38fbd41e4e4e5092189dff7723cead374818` |
| Finding007 correction report | `.local/agent/reports/ai-agent-workflow-full-implementation/s2-u-product-fast-fix-007-001.md` | `2a7f4d08c2d079bdbe6d440109870b5b802c6c6626825ea4f67f97c356af23c3` |
| pre-terminal product budget | `.local/agent/workplans/ai-agent-workflow-full-implementation/s2-u-product-replacement-budget-003.json` | `cc258217a24ee44441b53a13c8fc5a0634a3ad7305a138add490557f0e29bcf1` |
| replan Finding Validator | `.local/agent/reports/ai-agent-workflow-full-implementation/s2-authority-replan-validator-001.md` | `27b030f7aff6a51c70f361fa01231079b3b92c9482b7540ab3ed8743e05a34b3` |
| round2 architecture closure review | `.local/agent/reports/ai-agent-workflow-full-implementation/s2-authority-replan-rereview-architecture-001.md` | `0ede0ea0957b4afbfe21a2abed7b6ac7ea40e86e0f1f3339382686d6d4c98336` |
| round2 operability closure review | `.local/agent/reports/ai-agent-workflow-full-implementation/s2-authority-replan-rereview-operability-001.md` | `99b2bcc88d6798f422421c051338bbed9079a63103138033a66f06616e707f8a` |
| round2 Finding Validator | `.local/agent/reports/ai-agent-workflow-full-implementation/s2-authority-replan-validator-002.md` | `fd084e20437003add5642809436d5c6bafa5bc7c27a76e5384dbe81e81541615` |

workspace baselineはGit commit
`e5b87e08d0730e0f2a2c99c9a4883a172c0078e7`と、各package発行時のowned-path digest mapの
組である。既存のstaged deletion、`flake.nix`/`flake.lock`変更、untracked
`agent-workflows/`/`docs/`は保護する。入力digestまたはHEADが変わったら未開始packageを破棄し、
実行中resultは`blocked_stale_input`として受理しない。

## 3. authority、execution class、非対象

許可するwriteは、本plan、ignored ledger/report、およびdigest-bound packageが列挙したS2-owned
Skill/module/schema/template/manifest/test/fixture/evidence/status/HTML projectionだけである。

S2 source Taskは`candidate-generic`である。repository-local disposable fixtureにだけ、
`namespace=fixture:<id>`、`approval_scope=fixture-only`のsynthetic human receiptを使用できる。
`approved-objective-dependent`へ変換するAPI、fixture receiptをlive RunへcopyするAPI、ambient
conversationをapprovalにするAPIは作らない。

次は権限外である。

- current Run/objective candidate/approved pointerのwrite、real B7回答、real baseline/source access
- S0/S1 accepted evidence/historyの書換え、別HEAD/CAS/state owner、F1〜F8のfork
- S3〜S8 implementation、pilot、H1/H2/H3、company/live connector、credential/secret
- Nix/build/rebuild/switch、Home Manager、dotfiles/app-owned state、migration、activation
- Git branch/stage/commit/push、既存差分のcleanup、external mutation

`not_measured`はobjective/profile-versionにbindした明示decisionがある場合だけsource contract上の
有効strategyとする。その場合もcomparisonは`unavailable`であり、成功値を合成しない。S2 domain
moduleはcollectorを実行せず、physical observation receiptだけを入力にする。collector registry、
credential、network accessは後続profile/distribution authorityの対象である。

owner/evaluatorはfree proseでなく、strict `owner_ref`（kind、stable id、role、path、digest）を使う。
fixture ownerはfixture namespaceに固定し、live ownerへ昇格しない。

## 4. 既存Kernel gapとS2-U compatibility gate

as-isの`dag-command-v1`/`ControlKernel`は`entry`で最初の`objective_ref`を置けるが、B7に必要な
`approve_objective` command、approval object、candidate/version/event/current-pointerのatomic
transitionを持たない。pure Moduleがpointer candidateを返すだけでは実Workflowのcurrent ownerが
二重化するため、これをS2実装後へ隠さない。

S2-A/B/C dispatch前に、別のplan/review/validation loopでS2-Uを閉じる。S2-Uは一つの曖昧な
packageにせず、次の7 frontierをこの順で扱う。

1. **S2-U discovery（report-only）**: reverse `requires` closureをread-onlyで計算する。packageの唯一の
   write pathは
   `.local/agent/reports/ai-agent-workflow-full-implementation/s2-u-reverse-closure-discovery-001.md`
   とし、review/Validatorはそれぞれpackageで列挙した自身のreport一つだけを書ける。
2. **S2-U product**: discoveryの受理後、次の7 pathだけを一つのproduct candidateとして変更できる。
   - `agent-workflows/src/ai_agent_workflow/control_kernel.py`
   - `agent-workflows/schemas/dag-command-v1.schema.json`
   - `agent-workflows/schemas/dag-state-v1.schema.json`
   - `agent-workflows/schemas/objective-approval-v1.schema.json`
   - `agent-workflows/tests/test_control_kernel.py`
   - `agent-workflows/tests/test_objective_transition_compatibility.py`
   - `agent-workflows/evidence/compatibility/S2-objective-transition.json`
3. **S2-U retained-product recovery（今回のrecoveryだけ）**: 旧product terminal、全canonical Findingの
   attempt履歴、Finding007 correction、retained 7-path candidate、pre-terminal budget/lease、fresh expected
   HEADを一つのcontinuation envelopeへbindする。同じcandidate digestへfresh二軸product reviewと単一
   Validatorを行い、rootがaccepted-product receiptを発行できた時だけ次へ進む。Finding007 focused fixは
   acceptanceではない。open requiredが残る、digestが変わる、counterを継承できない場合はrotationを
   dispatchせずterminalizeする。通常のclean executionでS2-U productが既にacceptedなら本gateは不要である。
4. **S2-U successor mapping（report-only）**: productが必要とする既存S0/S1 compatibility rotationと、
   S2 close artifactsに必要なcommon-schema admissionを同じpre-body境界としてmappingする。今回の
   recoveryではhelper-validなclosure/order mapが、旧14-path rotationを`12 unchanged revalidate +
   2 reissue`とし、次のcombined rotationのwrite scopeをexact 6 pathsへ固定した。
5. **S2-U combined successor rotation**: 一つのowner/packageが、内部では次の非循環順で実行する。
   - P1: `section-review-v1.schema.json`、`section-bundle-v1.schema.json`、
     `section-accepted-result-v1.schema.json`へS2だけを追加する。
   - P2: P1のfinal digestを入力に`s1-source-authority.json`を再発行し、次に
     `s1_evidence.py`のfixed anchor、最後に`test_s1_evidence.py`を再発行する。
   manifestはverifierをtrust inputに含めず、verifierだけがmanifestを固定してdigest cycleを避ける。
   packageはこの6 paths以外を書かず、S0/S1 historical evidence、旧rotation receipt/report、
   既存S2 body bytesを書換えない。fresh二軸reviewと単一ValidatorがS0/S1 replay、exact S2 admission、
   open required 0を確認して初めてroot acceptanceとする。
6. **retained-body closure mapping（report-only）**: accepted successor receipt後、mapperは
   `.local/agent/reports/ai-agent-workflow-full-implementation/s2-retained-body-closure-map-001.md`だけを書き、
   report内のcanonical `retained-body-rebind-map/v1`をfreezeする。mapはeligibleな全retained candidate/receipt
   ID、source-byte path/digest、declared plan/authority input path/digest、affected/reusable disposition、通常の
   downstream edge、保持必須byteを列挙する。immutable input list、expected HEAD、accepted successor receiptを
   bindし、fresh二軸reviewと単一Validatorで受理する。unlisted/ambiguous receiptは`blocked_integrity`とする。
7. **retained-body rebind（read-only product）**: accepted map digestを入力に、既存S2 bodyのbyteを保持したまま、
   plan/authority inputが変わったreceipt closureだけを再検証しreplacement receiptへbindする。unchanged
   sibling receiptは再利用し、source-wide rebuildやbodyの無条件再実装は行わない。

`objective-approval-v1.schema.json`のwrite ownerはS2-U productだけであり、S2-0以降はread-onlyで
消費する。discovery、product、product recovery、successor mapping、combined rotation、retained-body mapping、
rebindのwrite scopeを合併せず、前frontierの受理前に次frontierをdispatchしない。

旧14-path rotation receiptはimmutableな実行履歴であり、現authorityとして再利用しない。その12 unchanged
pathsはread-onlyに再検証し、`s1-source-authority.json`と`s1_evidence.py`だけをP2でsupersedeする。
本replanは`rotation_wave_id=s2-u-successor-rotation-001`という一つの新しいsuccessor waveを許可する。
失敗後のcorrective retryは同じwave id、prior terminal/replan digest、replacement receiptをbindして
`corrective_rebind`と記録し、別のordinary rotation waveとしてbudgetをresetしない。

追加するpublic operationは一つだけである。

```text
ControlKernel.apply(approve_objective command)
  -> one transaction containing immutable candidate ref,
     approved objective version, approval event, and new current objective_ref
  -> CAS commit all-or-none | typed no-mutation refusal
```

commandはprior objective digest/version、proposal digest、human receipt、actor、run/namespace/scope、
expected HEAD、idempotency keyをbindする。protected objective changeを宣言し、absent/implicit/AI/
task-start/wrong candidate/wrong namespace/stale HEAD/changed-payload retryを拒否する。旧approved versionは
immutable historyに残す。crash before publish、publish後HEAD前、HEAD後projection前を既存Kernel recovery
規則で処理する。

Kernel/schema変更によりS0/S1 fixed verifierが参照するcurrent byte closureが変わる場合、reverse closureを
先に計算し、current compatibility evidence/authority、common schema、S1 authorityを一方向にrotationする。
historical evidence/package/reportは書換えない。fresh二軸reviewと単一Validatorがfixed replay、S0/S1 regression、
S2 exact admission、open required 0を確認するまでbody consumerをadmitしない。影響closureを正確に列挙できない
場合は`blocked_integrity`で停止し、private reducerやS2専用schemaへ迂回しない。

## 5. canonical 13-contract map

| qualified ID / Skill | deep Module selector | physical input -> candidate output | completion / typed stop | acceptance selector |
| --- | --- | --- | --- | --- |
| `group.B.B1` / `entry` | `ObjectiveSystemV1.capture_intake` | raw request、refs、deadline、Run-index snapshot -> intake | 原文/解釈/assumption/unknown分離 / active duplicateでresume/newがmaterialなら`needs_user_duplicate_run` | `B1_EntryTests` |
| `group.B.B2` / `discover-context` | `ObjectiveSystemV1.discover_context` | intake、read-only source snapshots -> sourced context | fact/inference/assumption/unknown exact / material source authority欠損で`blocked_missing_authority` | `B2_DiscoverContextTests` |
| `group.B.B3` / `classify-scope` | `ObjectiveSystemV1.classify_scope` | intake/context -> scope classification | depth/operation/personal-company/reversibilityと省略理由 / risky quick、unknown ownerでstop | `B3_ClassifyScopeTests` |
| `group.B.B4` / `grill-purpose` | `ObjectiveSystemV1.continue_inquiry` | context/scope/prior answers -> append-only inquiry + next one question | discoverableでないmaterial unknownだけ一問 / humanなしで目的が変わる時`needs_user_purpose` | `B4_GrillPurposeTests` |
| `group.B.B5` / `propose-true-purpose` | `ObjectiveSystemV1.propose_options` | inquiry/context -> 2〜3 options + recommendation | 区別可能なtrade-off/scope/cost/authority / AIによる自動選択で`blocked_implicit_approval` | `B5_ProposePurposeTests` |
| `group.B.B6` / `assess-feasibility-and-constraints` | `ObjectiveSystemV1.assess_feasibility` | options、constraint snapshots -> hard/soft/assumption/open | delegated evidenceを一ownerが収束 / hard constraint、missing authorityでtyped stop | `B6_FeasibilityTests` |
| `group.B.B7` / `approve-objective` | `ObjectiveSystemV1.prepare_approval` + S2-U adapter | selected option、feasibility、explicit receipt、prior pointer/HEAD -> approval command/result | atomic version/event/pointer、actor/time/impact/reopen条件 / absent/implicit/fixture-live/staleでno mutation | `B7_ApproveObjectiveTests` |
| `group.C.C1` / `decompose-outcomes` | `OutcomeSystemV1.decompose_outcomes` | approved objective ref -> outcome map | achieved state/why/contribution/exclusion/owner、objective coverage / task/solution node、holeでstop | `C1_DecomposeOutcomesTests` |
| `group.C.C2` / `build-outcome-dependency-graph` | `OutcomeSystemV1.build_dependency_graph` | outcome map -> prerequisite DAG/batches/convergence | acyclic、all nodes/owners/joins reachable / cycle/orphan/unknown endpoint/ownerless joinでstop | `C2_OutcomeDAGTests` |
| `group.C.C3` / `design-measurement` | `OutcomeSystemV1.design_measurement` | C-01 bundle、source capabilities -> measurement plan | direct/proxy/rubric/not_measured、rationale/gaming/guard / meaningless measureで`needs_user` | `C3_MeasurementDesignTests` |
| `group.C.C4` / `define-targets` | `OutcomeSystemV1.define_targets` | plan、deadline/window -> target set | unit/formula/source/frequency/window/guard exact / contradictory or divergent local targetでstop | `C4_TargetDefinitionTests` |
| `group.C.C5` / `capture-baseline` | `OutcomeSystemV1.record_baseline` | target、observation receipt -> baseline | available 0とunavailable null/reasonを区別 / stale/access/side-effect/incomparableを値へ変換せずstop/retain | `C5_BaselineTests` |
| `group.C.C6` / `validate-outcome-system` | `OutcomeSystemV1.validate_system` | objective/DAG/plan/target/baseline -> trace validation | orphan/duplicate/mismatch/gaming/ownerless return 0 / ownerへtyped upstream return | `C6_OutcomeValidationTests` |

各Skillは`skills/<catalog-name>/SKILL.md`に置き、name、discriminating description、
`disable-model-invocation: true`、exactly one `acceptance-test`を持つ。本文はpurpose、physical input、
authority、ordered method、checkable completion、failure/stop、handoffに限定し、deep Module/schema/
Kernelを複製しない。

## 6. public seamとcanonical machine data

公開seamは次の三つである。

```text
ObjectiveSystemV1.compile(qualified_id, inputs, authority, expected_head)
  -> objective-system-artifact/v1 | objective-approval-command/v1 | typed refusal

OutcomeSystemV1.compile(qualified_id, inputs, authority, expected_head)
  -> outcome-system-artifact/v1 | outcome-epoch-bundle/v1 | typed refusal

S2EvidenceCompiler.compile(root, accepted_refs, receipts, expected_counts)
  -> frozen S2 evidence/status candidate | integrity refusal
```

strict JSON machine documentがcanonicalであり、catalogが要求するMarkdown/YAML artifactはそこからの
deterministic projectionとする。projectionを独立に編集してstateへ戻さない。templateは表示shapeであり、
canonical field ownerではない。

`ObjectiveSystemV1`はB1〜B7のsemantic convergenceとapproval command construction、
`OutcomeSystemV1`はC1〜C6とC-01/C-02 semantic validation、`S2EvidenceCompiler`はphysical byte/
selector/evidence/status graphだけを所有する。S2-U adapterはvalidated B7 commandを既存Kernelへ一回だけ
渡し、独自store/pointer/reducerを持たない。

## 7. schemas、manifests、templates、source paths

S2-owned canonical pathsを次に固定する。

- `agent-workflows/src/ai_agent_workflow/objective_system.py`
- `agent-workflows/src/ai_agent_workflow/outcome_system.py`
- `agent-workflows/src/ai_agent_workflow/s2_lifecycle.py`
- `agent-workflows/src/ai_agent_workflow/s2_evidence.py`
- `agent-workflows/schemas/objective-system-v1.schema.json`
- `agent-workflows/schemas/objective-approval-v1.schema.json`（S2-U product所有、S2-0以降read-only）
- `agent-workflows/schemas/outcome-system-v1.schema.json`
- `agent-workflows/schemas/outcome-epoch-bundle-v1.schema.json`
- `agent-workflows/schemas/measurement-observation-v1.schema.json`
- `agent-workflows/schemas/group-b-manifest-v1.schema.json`
- `agent-workflows/schemas/group-c-manifest-v1.schema.json`
- `agent-workflows/schemas/s2-source-authority-v1.schema.json`
- `agent-workflows/groups/objective.json`
- `agent-workflows/groups/outcomes.json`
- `agent-workflows/templates/intent/`、`templates/objectives/`、`templates/outcomes/`、`templates/measurement/`
- B1〜B7/C1〜C6の13 Skill directoriesと`evidence/contracts/B1.json`〜`C6.json`
- `agent-workflows/tests/test_objective_system.py`
- `agent-workflows/tests/test_outcome_system.py`
- `agent-workflows/tests/test_s2_lifecycle_integration.py`
- `agent-workflows/tests/test_s2_evidence.py`
- `agent-workflows/tests/fixtures/s2/`
- `agent-workflows/evidence/sections/S2/`
- `agent-workflows/manifests/s2-source-authority.json`
- S2-Dに限定した`implementation_status.py`、status schema/manifest/test/HTML projection

既存S1 `group-manifest-v1`はGroup A専用であるため、緩和せずB/Cのstrict schemaを新設する。
Section common schema 3件はS2-0より前のaccepted combined rotationからread-onlyで消費する。
`section-review-v1`と`section-bundle-v1`のclosed enumはexactly `S0|S1|S2`、
`section-accepted-result-v1`は既存S0/S1 branchを保持してexplicit S2 branchと`s2SliceRefs`だけを追加する。
regex-open namespace、S3先取り、S2専用の代替common schemaを許さない。
全nested objectは原則`additionalProperties: false`、duplicate raw key/path/digest/selectorを拒否する。

## 8. artifact DAG、ready waves、write scope

| artifact | owner | requires | exclusive write scope | acceptance gate |
| --- | --- | --- | --- | --- |
| S2-P v2 replan | root | accepted S1 + terminal Finding + two successor maps | 本plan、ledger | 同一digestのplan二軸review + Validator、open required 0 |
| S2-U discovery | reverse-closure mapper | accepted predecessor S2-P + valid section budget | §4のexact report一つだけ | accepted historical input; exact Kernel reverse closureをfreeze |
| S2-U product | Kernel compatibility owner | accepted discovery | §4のexact 7 product paths | retained corrected candidate; atomic B7 command、focused artifact gate; acceptanceは別gate |
| S2-U product recovery | product recovery reviewers + root | v2 replan + §2 terminal/candidate/fix/prior budget | continuation envelope、review/Validator report、root acceptance receipt only; product read-only | inherited attempts/counters、fresh二軸review + Validator、accepted-product receiptまたはterminal |
| S2-U successor mapping | two read-only mappers | accepted-product receipt + terminal Finding + current S1/common-schema bytes | §2のexact report二つだけ | source closure6、old rotation12 revalidate +2 reissue、logical order fixed |
| S2-U combined successor rotation | single authority owner | accepted S2-P v2 + accepted-product receipt + successor maps | §4のexact 6 source/test paths only | same-wave P1→P2 issuance、S0/S1 replay、exact S2 admission、二軸review + Validator |
| S2 retained-body closure map | fresh receipt-closure mapper | accepted combined successor rotation + frozen retained body | §4のexact ignored report一つだけ | canonical map complete/unambiguous、二軸review + Validator |
| S2 retained-body rebind | rebind verifier | accepted combined successor rotation + accepted retained-body map | replacement private receipt/report only; product read-only | retained bytes一致、affected receipt closureだけreplacement、unaffected receipt reuse |
| S2-0 contract foundation | contract worker | accepted combined successor rotation + applicable retained-body rebind | §7のB/C/outcome/observation/group schemas（`objective-approval-v1`を除く）のみ | interface/schema examples、strict negatives、13 selector map freeze |
| S2-A objective | Objective Worker | accepted S2-0 | `objective_system.py`、`s2_lifecycle.py`、B Skill/template/test/fixture subtree | B1〜B7 focused GREEN、fixture/live separation、candidate freeze |
| S2-B outcome | Outcome Worker | accepted S2-0 | `outcome_system.py`、C Skill/template/test/fixture subtree | C1〜C6 focused GREEN、C-01/C-02、unavailable、candidate freeze |
| S2-C evidence | Evidence Worker | accepted S2-0 | `s2_evidence.py`、authority schema/evidence test fixtures | physical qualifier/finalizer interface candidate、no final acceptance |
| S2-D integration/status | Convergence Worker | accepted S2-A/B/C | B/C manifests、13 contract evidence、integration/status/schema/test/HTML candidate | B→C-01→C-02、29/60、0/23、false live claims、frozen candidate |
| S2-E final evidence | Finalizer + separate Anchor | accepted S2-D closure | `evidence/sections/S2/`、S2 authority manifest、fixed verifier constant | complete graph、fixed external authority、next-S3; self-acceptなし |

通常実行ではS2-A/B/CをS2-0 accepted後に別write scopeで並列化する。今回のrecoveryでは既に作成済みの
S2-0/A/B/C/D候補bytesを再dispatchせず、successor rotation後にmapper-frozen closureだけをrebindする。
未開始のS2-D status以降はrebind acceptanceまで開始しない。S2-Dは三者のaccepted refsだけを結合する。
S2-EはDまでのrequiredが0でなければ発行しない。共通manifest/status/HTML/close graphを
複数workerへ渡さない。人工的な並列上限は置かず、dependency、write conflict、available capacity、
fresh reviewer capacity、残budgetだけでadmit/serialize/refuseする。

plan digest変更は未受理dispatchをinvalidateするが、既にfreezeしたproduct bytes自体は保持し、declared
plan/authority inputを持つreceiptだけをmapper-defined rebind対象にする。common schemaまたはreplacement S1
authority変更はそのfrozen S2 receipt/evidence closureだけをinvalidateし、S0/S1 historical resultは外さない。
通常のS2-U/0変更はA〜E、A変更はB evidenceとD/E、BのC-01変更はcurrent C-02/C3〜C6/D/E、C変更は
evidence/D/Eを外す。rebind出力が変わった場合はその通常downstream `requires` closureを外す。影響外accepted
siblingと全historical result/receiptを保持し、replacementのfresh review/validationなしにcurrent acceptedへ戻さない。

## 9. C-01/C-02とB approval lifecycle

B1〜B6は一つの目的収束chainであり、B2のread-only調査だけを独立workerへ分けられる。B7は
明示approvalというauthority changeなのでfresh Epochで行うが、目的選択ownerは変えない。

```text
accepted S1 + candidate objective observation
  -> B1 -> B2 discovery join -> B3 -> B4 -> B5 -> B6
  -> candidate objective bundle/checkpoint -> clear
  -> fixture B7 approval command -> S2-U atomic disposable transaction
  -> approved fixture objective ref
  -> C1 -> C2 -> C-01 review/validation/bundle/checkpoint -> clear
  -> fresh C-02: C3 -> C4 -> C5 -> C6
  -> C-02 review/validation/bundle/checkpoint -> Group C close
```

C-01はapproved objective、approval event/current pointer、outcome map、DAG、C1/C2 review/validation、
accepted/unresolved/invalidated、next C3〜C6、context budget、HEADをbindする。C-02はexact C-01 refと
measurement plan、targets、baseline receipt、validation、next S3をbindする。両境界でfresh processが
HEAD/bundleだけからfrontierを復元し、chat/projection/stale adviceを入力にしない。

fixture B7 transactionはisolated repo-local Kernel stateだけに適用する。current Run pointerはnullのまま、
status completion gate `objective-approved`もpendingである。`next-S3.json`はsource plan inputを作れるが、
real `approved-objective-dependent` S3 execution packageはreal B7/current pointerなしにdispatchしない。

## 10. Artifact/Section/Workflow V2 loops

各S2-0/A/B/C/D/E candidateは、package closureにsource/test/fixture/schema/config/lock/toolchain/
command/args/cwd/environment/isolation/resource/supervision/external snapshotをbindする。WorkerはRED、
minimum GREEN、focused test、purity後だけcandidateをfreezeする。

freeze後、`RegressionFrontier`がdisjoint shardを作り、`PersistentReceiptRunner`がbounded capture付きで
実行し、`ReceiptAggregator`が再実行なしにjoinする。同じcandidate/aggregateへbindしたfresh
architecture/safety Reviewerとfresh operability/time/dotfiles Reviewerを並列発行し、別fresh
Finding Validatorが全candidateをexactly onceでrequired/defer/reject/needs-userへ分類する。

Orchestratorはrequiredだけをnarrow fixへ送り、同じcanonical Finding IDで最大5 product attempts、
変更candidateに対するfresh二軸closure reviewを行う。duplicate、既知decision、deliberate design、
downstream-only、too-minor、test-evidence debt、report correctionはproduct attemptを消費しない。
ordinary review roundは最大2で、新candidateはfixせずValidator入口へ戻す。

Middle Section loopはaccepted A/B/Cを一つのS2-D Convergence Workerで統合し、同じ二軸review +
Validator + required-only feedbackを行う。S2-E close graphも別candidateとして二軸review + Validatorを
通す。Outer loopはaccepted S0/S1/S2 bundleだけを結合し、S1→S2 transition、coverage、next S3を
incremental reviewする。S3〜S8未完のためWorkflow EvidenceFinalizerはclose setを出さない。

## 11. TDD slicesとfault matrix

### Objective RED/GREEN

REDはraw/interpretation混同、sourceなしfact、high-risk quick path、複数質問、同義option、AI/implicit/
task-start approval、fixture-to-live、stale pointer、partial event/pointer commitを通す。GREENはB1〜B7が
typed artifact/refusalを返し、candidate/version/event/pointerの原子性とimmutable historyを保つ。

### Outcome RED/GREEN

REDはtask-shaped outcome、coverage hole、cycle/orphan/unknown edge/ownerless convergence、proxy guard欠損、
rubric anchor欠損、worker-local target、stale/access denied/side-effectを0へ変換、condition mismatch比較、
guard悪化時passを通す。GREENはC1〜C6 traceとC-01/C-02 invalidationをstrictに保つ。

### Integration/evidence RED/GREEN

REDは13 count literal、missing Skill/source/selector/evidence/reachability、bare alias/cross-bound receipt、
S1 next input置換、B7 fixtureでlive gate advance、profile増加、full true、S2 common schemaのS3許可、
self-consistent全再digestを通す。GREENはexact 13 acceptance、29/60、0/23、partial、全live false/pendingを
固定し、external fixed authorityによるreplacement matrixを通す。

fault injectionはcrash before object publish、publish後HEAD前、HEAD後projection前、worker loss、old lease
late result、ambiguous result、same idempotency key changed payload、C-01 drift後old C-02 resume、corrupt
receipt/object/parent chain、projection delete/forge、owned scope overlapを含む。no mutation、orphan quarantine、
HEAD-derived rebuild、accepted sibling retentionを観測する。

## 12. regression frontier、receipts、time budget

rootはS2-U discovery packageを含む最初のS2 packageより先に、versioned
`s2-section-budget/v1` envelopeを発行する。envelopeは少なくとも`issued_at`、発行時点から480分後の
finite `deadline_at`、monotonic基準、remaining source、review round最大2、Finding fix最大5、
consumed counters、minimum closure reserve 45分、expected HEADとplan digestをbindする。全S2 packageは
このbudget envelopeのpath/digest/versionを入力に持つ。

45分のreserveは、適用可能なproduct fix 15分、affected shard 5分、fresh二軸rereview 10分、
単一Validator 5分、parent finalization 3分、runner timeout/grace/terminal記録 7分を確保する。
dispatch前にremainingが当該workのSLOと45分reserveを満たさない場合、新規workを始めず
`terminal_non_dispatch/stopped_budget`にする。missing、stale、expired、reserve-insufficientなenvelopeも
同じくfail closedとする。再開には別途authorizeされたreplacement budget version、prior terminal digest、
new expected HEADが必要であり、calendar dateをsourceへhardcodeしてbudgetを延長しない。

combined successor rotationとretained-body rebindは、accepted v2 replan digest、expected HEAD、同じ
versioned budget/lease、`rotation_wave_id=s2-u-successor-rotation-001`をbindする。旧14-path receiptは履歴入力、
新receiptは12 unchanged digestの再検証と2 reissued pathのsupersessionを明示する。変更していない
candidate digestのtest receiptは再利用し、同じsuiteをgateごとに再実行しない。

terminal後のreplacement budget/version/leaseは**continuationだけ**である。prior terminal、同じ
`rotation_wave_id`またはproduct `recovery_chain_id`、全canonical Findingのattempt、消費済みreview round、
fix dispatch、runner/review/Validator時間を引継ぎ、global capのremainingを増やさない。新しいversion/leaseは
fresh expected HEADと失効時刻を持てるが、ordinary root、Finding identity、attempt/review counterをresetしない。
corrective candidate/review/Validatorは継承counterを消費し、cap超過時は自動replacementを発行せずterminalizeする。

S2-D frozen candidateで次の6 shardを一回だけ実行する。

| shard | membership |
| --- | --- |
| `schema-static` | S2 schemas/manifests/templates/evidence parse、Python compile/import/static invariants |
| `objective-focused` | B1〜B7、S2-U atomic transition、fixture/live negatives |
| `outcome-focused` | C1〜C6、C-01/C-02、unavailable/gaming/trace negatives |
| `integration-fault` | B→C cold resume、crash/recovery/late result、status delta |
| `affected-regression` | changed Kernel/S0/S1 consumer/status/evidence exact affected selectors |
| `remaining-source` | source-wide inventoryから上記を除いた補集合 |

member重複0、coverage complete、resource conflictなしまたは明示serialize/refuse、capture complete/integral、
exit acceptedをaggregateが証明する。candidate/closureが不変なreceiptは`reused-receipt`、変更shardだけ
`new-run`にする。broad suiteを各gateで再実行しない。

package/reportはUTC RFC3339とmonotonicでqueue、preflight、design、edit、focused/regression test、tool wait、
review、report、helper、parent assimilationを分ける。診断SLOはdispatch 2分、candidate 15分、shard 5分、
review wave 10分、Validator 5分、parent finalization 3分、no-Finding inner loop 35分。超過は記録し、
deadline後もworkerを無期限に走らせない。

## 13. plan/review/finding finite convergence

本v2 replan candidateは同じbyte digestを入力に、次のfresh reviewを並列で受ける。

1. architecture/safety: S2-U necessity、single state owner、approval atomicity、13 contract/interface、
   C-01/C-02 invalidation、authority rotation、schema/graph integrity。
2. operability/time/dotfiles: artifact wave、write scope、regression reuse、finite budget、fixture/live、
   unavailable、status/HTML、Run/Nix/dotfiles/Git boundary。

単一fresh Validatorだけがcandidateをdedup/materiality判定する。requiredだけを本planへ一度にfeedbackし、
fresh round-2二軸reviewでcloseする。review最大2、Findingごとのproduct fix最大5。重複、理由付き選択、
軽微な表記、downstream-onlyはloopを延長しない。round2/2、attempt5/5、needs-user、authority欠損、
残budget不足はimmutable terminalを作り、prior terminal digest、new expected HEAD/lease/versioned budgetなしに
reopenしない。

replan acceptanceはsource rotation acceptanceではない。plan通過後にexact 6-path source candidateをfreezeし、
その同一digestへfresh二軸reviewと単一Validatorを別途一回適用する。corrective restartは同じwave idを使い、
review rootやbudgetを新規作成してround/attemptをresetしない。

round1 Validatorのcanonical plan Findingsは次の3件で、round2はこれらのclosureとnew material root 0だけを扱う。

- `s2-replan/product-acceptance-gate-missing`
- `s2-replan/retained-body-rebind-map-unowned`
- `s2-replan/same-wave-continuation-counters-unbound`

`s2-replan/future-body-write-partition`はcurrent recoveryのsource frontier外としてdeferする。将来body candidateを
実際にadmitする時、既存frontier mapとcandidateのleaf allocation/collision checkを必須にするが、今回の
retained bytesを再openしない。

round2 closureはfeedback candidate `sha256:36d11228a7d3fee2436376c742e3d2e8d09e825d09a91137907ec0f8c8209829`
に対し、両軸でcanonical 3/3 closed、新規material root 0、単一Validatorでrequired-open/needs-user各0、defer1と
確認した。これは本planだけのacceptanceであり、product、rotation、bodyのacceptanceではない。

## 14. final evidence、status、recovery、stop

S2-Eはartifact/result、focused/final regression receipts、二軸reviews、Validator dispositions、
Finding attempts、B/C manifests、13 contract evidence、C-01/C-02、section result/review/validation/bundle/
checkpoint/accepted/index/next-S3、source authorityをcanonical orderで物理化する。

Finalizerは全branch known/complete/accepted、required/needs-user/incomplete/unknown/integrity/stopped-budget 0の
時だけclose graphを作る。別Anchorがclose graph外のroot-issued S2 authority manifest digestをfixed verifier
constantへpinする。Finalizer/Anchorはtest、domain判断、Finding、fix、acceptance、HEAD updateを行わない。

statusは次を同時表示する。

- S0 accepted: true、S1 source accepted: true、S2 source accepted: true
- named contracts: 29/60、profile steps: 0/23、overall: partial
- current Run objective-approved: false/pending
- actual A7、migration、activation、source-wide integration、full Workflow ready: false

停止理由は`blocked_stale_input`、`blocked_missing_authority`、`blocked_integration`、
`paused_worker_unavailable`、`blocked_integrity`、`blocked_boundary`、`needs_user`、
`terminal_non_dispatch/stopped_budget`のいずれかに型付けする。停止時もaccepted sibling、historical bytes、
open Finding、consumed counters、closed lease、invalidated dispatch、exact next actionを保存する。

本replan acceptance、accepted-product receipt、combined successor rotation、accepted retained-body map/rebindが
揃うまで、S2-D status、S2 candidate freeze/Section review、S2-E、S3〜S8を開始しない。rotation後にretained
12 pathsのdigest差分またはmapper外の
reverse consumerが見つかった場合は`blocked_integrity`で停止し、二つ目のordinary rotationを開始しない。

rollbackはS2-owned source byte/current source candidateを隔離またはprior accepted refへ戻す計画までで、
S0/S1 history、current Run objective、Nix generation、dotfiles、Gitを巻き戻さない。projectionだけの破損は
HEADから再生成し、immutable object/parent chain破損は`blocked_integrity`で停止する。

## 15. planning decisionsとexact first action

3 mapperの未解決点は次のように収束した。

1. strict JSONをmachine truth、Markdown/YAMLをdeterministic projectionとする。
2. `not_measured`はversion-bound explicit decisionがある時だけ有効だがcomparison/success値は作らない。
3. owner/evaluatorはphysical digest付きstrict `owner_ref`にする。
4. S2 moduleはcollectorを持たず、candidate-generic fixture observationだけを受ける。
5. B7 pointerはpure Moduleだけにせず、既存Kernelを唯一ownerのまま拡張するS2-U gateで閉じる。

本v2 replanのround2二軸reviewとValidatorがcanonical3 closed、new material root 0、open required 0を確認した後、
rootは最初にFinding007修正済みretained product candidateのacceptance回収gateを発行する。同じdigestのfresh二軸
product review、単一Validator、root accepted-product receiptが揃った後にだけ、§4のexact 6-path combined successor
rotationを発行する。そのsource candidateをfresh二軸reviewと単一Validatorで受理した後、exact ignored report
一つのretained-body closure mapper、同mapのreview/Validator、mapper-defined rebind/revalidationを順に実行する。
そこまで通過して初めて未開始のS2-D status以降を再開する。現在のS2-U productとS2 body candidateは保持し、
S2-0/A/B/Cを無条件に再実装しない。current Run、live approval、Nix、dotfiles、migration、activation、Gitは開かない。
