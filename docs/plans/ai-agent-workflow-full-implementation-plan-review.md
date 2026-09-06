# AIエージェントWorkflow 全体実装計画レビュー

Status: complete / pass

Review target: `ai-agent-workflow-full-implementation/v1`

基準日: 2026-09-04（Asia/Tokyo）

## 1. レビュー方式

計画作成者と別のhistory-free Sub Agentが、次の三観点を並列にcandidate reviewした。

- spec/coverage: 利用者意図、60 named contract、23 profile step、追加surface
- operability/loop: 三重loop、DAG、停止性、回復、実装可能性
- context/authority: 最小package、objective/approval、A7境界、worker loss/integrity

reviewerのcandidateは修正命令にしなかった。第四のfresh Finding Validatorが全候補を
一度ずつ`required | defer | reject | needs-user`へ分類し、requirement、scope、gapに基づく
fingerprintで重複とmaterialityを検証した。

## 2. 候補と判定

候補は12件だった。判定はrequired 9候補、defer 1候補、reject 2候補、needs-user 0件。
重複統合後はrequired 7 canonical Finding、defer 1件である。

| candidate | disposition | canonical | 判定要旨 |
| --- | --- | --- | --- |
| PLAN-SPEC-001 | required | PF-001 | S2 fixture B7とlive objective approvalを分離する |
| PLAN-SPEC-002 | required | PF-002 | S7追加surfaceをfail-closed inventoryへ接続する |
| PLAN-SPEC-003 | defer | PF-008 | post-S8 runtime/pilot計画は別authorityで作る |
| PLAN-SPEC-004 | reject | — | S7は既にfixture/static sourceへ限定されている |
| PLAN-OPS-001 | required | PF-003 | S0 source simulationとactual A7を別claimにする |
| PLAN-OPS-002 | required | PF-004 | section transitionのpublic seam/ownerをS0に固定する |
| PLAN-OPS-003 | required | PF-005 | Finding identityをscope/algorithm込みで固定する |
| PLAN-OPS-004 | required | PF-006 | upstream変更時のaffected sibling invalidationを定義する |
| PLAN-OPS-005 | required | PF-005 | same Finding rereviewの有限closureをidentityへ統合する |
| PLAN-CTX-001 | required | PF-001 | candidate-genericとapproved-dependent admissionを分離する |
| PLAN-CTX-002 | reject | — | actual A7 receiptをS0 admissionへ要求すると循環を再導入する |
| PLAN-CTX-003 | required | PF-007 | worker loss/integrity recovery provenanceを固定する |

## 3. required-only feedback

計画へ反映した変更は次の7件に限定した。

1. `PF-001`: `candidate-generic` / `approved-objective-dependent` execution class、
   approval receipt/pointer、non-mutating rejectionを追加し、fixture B7をlive gateへ流用しない。
2. `PF-002`: `additional-required-surfaces/v1`を追加し、model/config/distribution/provider/
   company 7 Groupをstable ID、owner、test、evidence、stateでfail-closedにする。
3. `PF-003`: S0 exitを`source-transition-fixture-passed`へ変更し、actual A7の
   `A7-handoff-complete`と分離する。
4. `PF-004`: S0の`section-control-plane/v1`へmapping、type、command/CAS/authority/
   idempotency、Bootstrap adapter、owner、rejection/rollbackを集約する。
5. `PF-005`: `finding-identity/v1`と`rereview-closure/v1`を追加し、exact dedup、
   same-Finding attempt、new candidate再検証、E10 terminalを定義する。
6. `PF-006`: accepted stateをinput digestへbindし、reverse `requires` closureで影響分だけを
   invalidatedにする。historical evidenceと影響外siblingは保持する。
7. `PF-007`: worker-loss/integrity recovery package、late result拒否、quarantine、
   replacement provenance、reopen authorityを追加する。

### 3.1 fresh rereviewで追加されたrequired feedback

最初のfresh rereviewでは、spec/coverage側はPF-001〜PF-004 pass、新candidate 0、
operability/authority側はPF-003〜PF-007 passだった。後者が新candidate `RROPS-001`を
提出したため、直接修正せず別のfresh Finding Validatorで検証した。

validatorはRROPS-001をrequired、PF-007とは別gap、needs-userではないと判定した。
そこで次のminimum editだけを反映した。

- Task packageへ`workspace-provenance/v1`を追加し、assignment専用workspace/staging、
  またはworkspace identity + owned-path baseline digest + expected HEADへbindする。
- transferはassignment/attempt、write scope、provenance、expected HEADに一致する差分だけに
  限る。帰属不能・既存・scope外差分はbytes不変でquarantineし、human/authority routeと
  blocked replacement scopeを残す。
- discardはassignment専用workspace/stagingと検証できる場合、または別の明示authorityが
  ある場合だけ許す。

この追加feedbackもfresh closure rereviewがpassするまでclosedと扱わない。

product source、Run state、runtime pointer、Nix、activation、Gitは変更していない。

## 4. defer / reject

`PF-008`はS8後に必須となるpost-source operational planとしてdeferした。R0 non-switching
rebuild、activation、fresh runtime/model/config observation、personal pilots、H1/H2/H3、
operational adoptionを、その時点の別authorityとevidenceへ割り当てる。これがない限り
`full_workflow_ready=false`を維持する。

PLAN-SPEC-004は、company sourceがfixture/static checkへ限定され、運用接続とcredentialを
除外済みなのでrejectした。PLAN-CTX-002はactual A7 provenanceをS0 generic source
admissionへ必須化してsource-firstの循環を戻すためrejectした。

## 5. fresh rereviewの受入条件

- PF-001〜PF-007のminimum plan editが計画に存在し、相互矛盾しない。
- 60/23と追加surfaceがS0〜S8に一意に割り当てられる。
- source simulation、actual A7、R0以後のoperational actionが別claim/authorityである。
- three loopsがexact Finding identity、required-only feedback、有限terminalを持つ。
- upstream change、worker loss、integrity failureからstale resultを再利用しない。
- 新しいmaterial candidateがあれば直接修正せず、再度fresh validationへ戻す。

fresh rereviewがpassするまで、計画statusはacceptedにしない。

## 6. 最終判定

- spec/coverage rereview: PF-001〜PF-004 pass、新candidate 0。
- operability/authority rereview: PF-003〜PF-007 pass、RROPS-001をcandidateとして提出。
- RROPS-001 validation: required、PF-007とは別gap、needs-userではない。
- RROPS-001 required-only feedback後のclosure rereview: pass、新candidate 0。
- open required Finding: 0。
- deferred: PF-008のみ。S8後、R0実行前のpost-source operational plan作成gate。

したがって本計画は、S0の`section-plan/v1`作成へ進める全体source実装計画としてpassとする。
これはS0〜S8のsource実装、R0、runtime/pilot、activation、Git、full Workflow readyの
完了を意味しない。

### 6.1 review後の利用者決定: 固定並列上限なし

利用者の明示決定により、基本並列には人工的な総worker上限もSectionごとの固定同時実行数も
設定しない。ready frontier上のdispatch可能Taskを全て並列候補とし、実効並列度は
dependency、write-scope衝突、harness capacity、review/validation capacity、残budgetだけで
制約する。deadline、review round、Findingごとのfix attemptという有限収束budgetは維持する。

この変更は既存§5.1の原則へ有限budget節を整合させたものであり、source実装、activation、
Gitその他の権限は追加しない。

## 7. 物理レビュー証拠

local review report:

- `.local/agent/reports/ai-agent-workflow-full-implementation/plan-review-spec.md`
- `.local/agent/reports/ai-agent-workflow-full-implementation/plan-review-operability.md`
- `.local/agent/reports/ai-agent-workflow-full-implementation/plan-review-context-authority.md`
- `.local/agent/reports/ai-agent-workflow-full-implementation/plan-finding-validation.md`
- `.local/agent/reports/ai-agent-workflow-full-implementation/plan-rereview-spec.md`
- `.local/agent/reports/ai-agent-workflow-full-implementation/plan-rereview-operability.md`
- `.local/agent/reports/ai-agent-workflow-full-implementation/rrops-finding-validation.md`
- `.local/agent/reports/ai-agent-workflow-full-implementation/rrops-closure-rereview.md`

上記reportはrepository-local private evidenceであり、tracked計画本文の正本を置換しない。
