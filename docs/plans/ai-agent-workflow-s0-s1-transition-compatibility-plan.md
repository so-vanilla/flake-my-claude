---
schema: compatibility-plan/v1
compatibility_id: S0-S1-TRANSITION
version: v1
status: feedback-applied-closure-pending
finding_id: S1-LIFE-001
parallel_policy: uncapped-ready-frontier
---

# S0→S1 section transition compatibility plan

基準日: 2026-09-04（Asia/Tokyo）

## 1. 目的と限定scope

現行`SectionControlPlaneV1`と`ControlKernel`はS0だけを開ける。S1をその公開seamのconsumerに
するため、S0の既存挙動を保持したまま、accepted sectionから直後のsectionを開く一般化を
唯一ownerへ追加する。本planはvalidated Finding `S1-LIFE-001` attempt 1/5の上流依存を閉じる。

成果は`section-transition-compatibility/v1` receiptである。S1 planはreceiptと公開source digestを
入力へbindし、受理前はS1-A/Bをdispatchしない。

許可する変更:

- `agent-workflows/src/ai_agent_workflow/section_control_plane.py`
- `agent-workflows/src/ai_agent_workflow/control_kernel.py`
- 必要と独立reviewで確認された既存section/dag schemaの最小互換変更
- section transition focused tests/fixtures
- compatibility schema/manifest/evidence
- 本plan、ignored report/ledger

禁止する変更:

- S1のA/F source、status classifier、S1 final authority
- S0 accepted close evidence/authorityの意味、A6/A6R/A7 source/evidence
- current/manual/new-kernel Run、actual migration/activation、Nix、Git、credential
- 第二のstate/store/reducer/HEAD/schedule compiler

## 2. digest-bound baseline

| input | SHA-256 |
| --- | --- |
| overall plan | `6c1b2ed01a4151ead414c24bd1d2eab78f596623039f038a3f2c17c212ebb401` |
| accepted S0 plan | `ff10c21f8e6ef31c25d30226ca8c633d7d0826a92a650129cf959b7f86df35ee` |
| step catalog | `8418e7d32291860faeb7e905b048a7ff7a03d0768b258ac9e4a75f4e7d498b6b` |
| `section_control_plane.py` | `6206996fb9863d5beb95b6340fc5d922079c445057dd22dd61ad4e9b0628433d` |
| `control_kernel.py` | `9d8b2bf47902b8d17829b19c7b3c2502592534ba3f2d52c225c7aa5ac3cd90d5` |
| `section-plan-v1.schema.json` | `3640c800b5adcbdac3485d4dd5a1eff92f3b3318502e0b3ecdd90233425e7a97` |
| `dag-command-v1.schema.json` | `752581ea85502352f8241e6f3d297d0448879398d095f8be81121a9ece2c019c` |
| `dag-state-v1.schema.json` | `6242dca57926aaecb266f35f27b2d3fa308522f17fedf3320d981d1bd29568d8` |
| `section-status-v1.schema.json` | `4f85ae1716c28eb4f4ad9ebc3e543b9f49f1287f915f3cc5a2583f478136ff3a` |
| current S0 close index | `74b10d757ef188ae5d11cbc1ac476038a148702d184e5cef5389ea8a7ab1dde4` |

Task packageは上記とowned-path baseline、Git baseline
`e5b87e08d0730e0f2a2c99c9a4883a172c0078e7`をbindする。差分があれば再packageする。
U-A/U-B/U-C、各Reviewer/Validator、narrow fixの全packageは次を必須とし、package全体を
canonical JSONでdigest-bindする。

- work/ticket/section/artifact、assignment、attempt、Context Epoch ID
- workspace identityと、assignment専用workspace/staging identityまたはnormalized owned-path
  baseline digest + expected HEADから成る`workspace-provenance/v1`
- exact write/read scope、authority ref、expected HEAD、上表を含むinput refs/digests
- assignmentごとに一意なresult/report path
- role別wall-clock timebox、review round/Finding fix budgetの最大・消費・残量
- stop reason一覧、late-result disposition、replacement input/output refs

timeboxはU-A/U-B各90分、U-C 120分、Reviewer各45分、Validator 30分、narrow fix 60分とする。
期限切れpackageのresultは受理せず、fresh expected HEAD、authority、provenance、unique outputへ
再bindしたreplacementだけをdispatchする。

## 3. public contract

### 3.1 初回S0 openを保存する

S0 initial pathは現在とbyte-compatibleな入力を受理する。

- predecessor: closed Bootstrap
- target: `S0`
- lifecycle prerequisite: `closed-bootstrap`
- state: `paused_after_group`、ready empty、clear true
- result: S0 first Group/Epoch/frontierをone transactionでopen

S0 positive/negative/status/replay testsを変更後も全て通す。

### 3.2 accepted section successorを開く

S1以後の`open_section`は次だけを受理する。

- current HEADは`paused_after_group`、ready/lease/running taskなし、clear required/true
- `metadata.section_control.transition.state == accepted`
- canonical `accepted_section_receipt`がcurrent HEADのobject catalogと一致
- prerequisite kindは`accepted-section`で、receiptのsection IDはcurrent sectionと一致
- target section IDはcurrent sectionの数値上の直後（`S0 -> S1`、`S1 -> S2`）
- plan/catalog/checkpoint/bundle/closed-group/accepted-section receiptをphysical attestationで検証
- target first Group/Epoch/frontierはcatalogと一致
- authority、expected HEAD、scope、protected fields、idempotency keyがcommandと一致

任意section skip、同一section reopen、過去receipt replay、command textだけのdigest、別Run receipt、
uncatalogued objectはno-mutation rejectする。

### 3.3 state/history semantics

`open_section`はtarget sectionの`groups`を空から作り、前sectionのGroup mapを混在させない。
直前のaccepted section control stateは次のclosed itemとして`metadata.section_history`へ追加する。
履歴はHEADのparent chainと各transactionのobject catalogからだけ再構築し、mutable projectionを
authorityにしない。

```json
{
  "position": 0,
  "section_id": "S0",
  "accepted_receipt_ref": {
    "digest": "sha256:<64-hex>",
    "object_type": "artifact",
    "path": "objects/<64-hex>.json"
  },
  "bundle_ref": {
    "digest": "sha256:<64-hex>",
    "object_type": "artifact-bundle",
    "path": "objects/<64-hex>.json"
  },
  "checkpoint_ref": {
    "digest": "sha256:<64-hex>",
    "object_type": "checkpoint",
    "path": "objects/<64-hex>.json"
  }
}
```

`position`はS0を0とする連続整数、`section_id`は`S{position}`、各refはcanonical object
bytesとaccepted receipt payload内のbundle/checkpoint refsに一致しなければならない。同じsection
ID/position/refの重複、欠番、順序逆転、同一accepted receiptの再利用を拒否する。

`open_group`は同一current section内だけを進め、現行accepted-section prerequisite semanticsを保持する。
section close receiptは既存`section-acceptance-receipt/v1`を使い、payloadの実section IDを検証する。

`status`/`resume`はcurrent sectionとordered section history、current Group/Epoch/frontier、accepted parent
receiptをHEAD chainから再生成する。公開`section-status/v1`へoptional `section_history`配列を追加し、
S1以後は上記closed itemを1件以上必須にする。既存S0 documentはfield省略を有効なまま保持し、adapterは
省略を空配列として扱う。S0で非空、S1以後で省略/空、またはcurrent `section_id`と履歴末尾が直前関係で
ないdocumentは拒否する。projectionの欠損・偽造はHEADから再生成し、object/parent corruptionは
`blocked_integrity`で止める。

## 4. implementation ownership

| slice | owner | exclusive paths | acceptance |
| --- | --- | --- | --- |
| U-A compiler/schema | Compatibility Worker | `section_control_plane.py`、`section-status-v1.schema.json`、必要なsection schema、focused contract tests | S0 unchanged + S1 successor command compile/reject + optional history schema/adapter exact validation |
| U-B kernel/history | Kernel Compatibility Worker | `control_kernel.py`、必要なdag-state schema、focused kernel tests | one owner、accepted parent、HEAD-derived history、CAS/idempotency、no mutation |
| U-C convergence/evidence | Convergence Worker | `section_transition_evidence.py`、integration fixture/test、compat authority/schema/manifest/evidence | S0→S1 positive、fault/redigest matrix、full suite、fixed-authority receipt |
| U-R/V/F | fresh Reviewers/Validator/narrow fix | unique reports / validated exact path | open required 0またはattempt 5/5 terminal |

U-AとU-Bは同じcommand contractを触るため並列writeしない。まずU-Aをaccepted candidateにし、
そのdigestをU-B packageへ渡す。U-Cは両者accepted後だけ開始する。

## 5. ordered TDD

1. 既存testへS1 successor commandのredを追加する。S0 initial openはgreen baselineを維持する。
2. compilerを一般化し、predecessor receipt、strict successor、catalog frontierをcommandへbindする。
3. Kernel redでS1を現在拒否すること、forged/stale/skip/replayがno mutationであることを確認する。
4. Kernelを一般化し、parent receiptのphysical/current-HEAD bindingとsection history reset/preserveを通す。
5. `section-status/v1`のS0 field省略/空とS1 ordered historyを通し、duplicate、gap、reverse、
   receipt substitution、parent/object corruptionを拒否する。
6. disposable RunでS0 open/accept/close→S1 open、exact retry、cold resumeを通す。
7. crash before publish、publish-before-HEAD、HEAD-before-projection、orphan、projection forgeを検査する。
8. compatibility receiptへsource/test/schema/input digestとtest command/count/exit statusを記録する。
9. caller-root replayと、§8の固定authorityを保ったfull-redigest negative matrixを永続testにする。

新しいtest method/fileの追加は許可する。S0のhistorical acceptance receiptに記録された209件は
過去の受理証拠として書き換えず、compatibility receiptへ新しい全suite countを記録する。

## 6. fault/acceptance matrix

| case | required result |
| --- | --- |
| S0 initial | current accepted behavior and all S0 tests pass |
| valid S0→S1 | one transaction、S1 first frontier only ready、S0 accepted history retained |
| valid S1→S2 representative | same generic successor rule; S2 product sourceは実装しない |
| skip/reopen/backward | typed reject、HEAD/history/ready unchanged |
| wrong/missing parent receipt | reject before transaction/object/projection write |
| cross-Run/section/group receipt | reject and preserve current accepted section |
| stale HEAD / changed idempotency payload | CAS reject / duplicate conflict |
| exact retry | canonical original receipt、revision unchanged |
| open Group vs open Section | Group preserves current section; Section archives then resets Group map |
| crash/orphan/projection | quarantine/report/rebuild only; no orphan adoption or automatic open |
| cold resume | fresh process derives identical current/history/frontier from HEAD |
| status history | S0 omission/empty compatible; S1 exact ordered history; duplicate/gap/reverse/substitution/corruption reject or integrity block |
| package mismatch | assignment/attempt/scope/provenance/expected HEAD mismatchはtransfer不可、source不変 |
| lost/late worker | late result quarantine、overlap replacement block、fresh packageだけ再dispatch |
| source boundary | actual A7/migration/activation/full-ready remain false |

## 7. review、validity、budget

U-A/U-B/U-C candidateはfresh Reviewerがspecとintegrityを検査する。候補はfresh Validatorが
required/defer/reject/needs-userへ分類し、fingerprintで重複を統合する。requiredだけをnarrow fixへ送り、
fresh closure reviewを行う。canonical Findingごとのfix上限は5、review roundsは最大2。
文章の好み、既知decision、意図した互換性、軽微な非機能差はblockingにしない。

現在のvalidated correctionは`S1-LIFE-001`、`S0S1-AUTH-001`、`S0S1-RECOVERY-001`が各attempt
1/5、残り4である。transition-status候補は`S1-LIFE-001`へ統合済みで別attemptを消費しない。
closure failureごとに同じcanonical IDのattemptだけを消費する。レビューから実証された新しい別root
causeが出た場合だけ別canonical IDを使う。

## 8. evidenceとhandoff

accepted compatibility artifactは次を物理化する。

- `agent-workflows/manifests/section-transition-compatibility.json`
- `agent-workflows/schemas/section-transition-compatibility-v1.schema.json`
- `agent-workflows/evidence/compatibility/S0-S1-transition.json`
- final source/schema/test digests、S0 parent index digest、test command/count/exit status
- review/validation/closure report digestsまたはcontent-bound records
- public interface version、supported predecessor/target rule、false claim vector

compatibility close setの外部trust rootは次へ固定する。

- authority: `agent-workflows/manifests/section-transition-compatibility-authority.json`
- authority schema: `agent-workflows/schemas/section-transition-compatibility-authority-v1.schema.json`
- caller-root-only verifier: `agent-workflows/src/ai_agent_workflow/section_transition_evidence.py`
- verifier定数: 上記authorityの固定pathとreview後の固定SHA-256。manifest/evidence/commandはauthority
  path/digestを指定できない

authorityはstrict schemaでissuer=`codex-root`、compatibility ID/version、accepted S0 close indexと
canonical parent receipt、expected HEAD、compiler/kernel/status/schema digests、compatibility test receipt、
false claim vector、canonical Finding/review/validation historyをbindする。regular file、no symlink、
caller root内normalized pathだけを受理する。authority bytesまたは固定digestのrotationは旧compatibility
receiptを即invalidにし、全source/evidenceを再検証したfresh review/validation/authorityでのみ再発行する。

永続full-redigest matrixはauthority、compat manifest/evidence、S0 close index/parent receipt、
compiler/kernel/status/schema、test receipt、claim、expected HEAD、review/validation historyを1種類ずつ
coherently置換し、close set側の全参照を再digestする。置換authorityへ一致する一時verifierを対照として
acceptさせても、checked-in fixed verifierは全branchをrejectしなければならない。

S1 planはmanifest/evidence/source digestsを入力表へ追加し、fresh processでreceiptを再検証する。
receipt生成前またはopen requiredが残る間はS1-A/Bをdispatchしない。

## 9. stop/recovery

次で停止する。

- baseline digest/owned path/expected HEADが変わる
- current S0 behaviorを互換維持できない
- generalizationに第二store/reducer/schedule ownerが必要
- A6/A6R/A7、S1 product source、current Run、Nix、migration、activation、Gitへscopeが広がる
- accepted parent receiptをphysical/current HEADへbindできない
- required Finding、needs-user、attempt 5/5、integrity blockが残る

lost worker diffをtransferできるのはassignment/attempt/Context Epoch、authorized write scope、
expected HEAD、authority ref、`workspace-provenance/v1`、owned baselineがすべて一致し、assignment-ownedと
証明できる場合だけである。期限切れ・旧assignmentのlate resultはunique quarantine refへ記録して
受理しない。帰属不能、dispatch前から存在、scope外の差分はsource bytes不変でquarantineし、重なる
scopeのreplacement dispatchをblockする。replacementは旧lease terminal化、消費/残budgetとstop reason、
late-result dispositionを`worker-loss-recovery/v1`へ保存し、fresh expected HEAD/authority/provenanceと
unique result pathを得た後だけ許す。失敗時はsource bytesを推測で巻き戻さず、accepted S0の現行実装と
証拠を基準に差分を分離する。

## 10. exact next action

本feedback版をfresh closure Reviewerへdispatchし、`S1-LIFE-001`、`S0S1-AUTH-001`、
`S0S1-RECOVERY-001`のrequired correctionだけが具体化され、相互矛盾や新しいscope拡張がないことを
検査する。fresh closure Validatorでopen required 0を確認した後だけU-AからTDD実装する。
