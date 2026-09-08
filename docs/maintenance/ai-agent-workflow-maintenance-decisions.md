# AI Agent Workflow 保守判断の基準

最終更新: 2026-09-08（Asia/Tokyo）

この文書は、AI Agent Workflow再構築で確定した設計判断、完成判定の境界、
今後の保守時に維持すべき不変条件をまとめる。詳細contractの正本は
`docs/plans/ai-agent-workflow-full-implementation-plan.md` と
`docs/plans/ai-agent-workflow-step-catalog.md`、Inceptionの現行運用は
`docs/plans/inception-single-skill-operation.md` と
`agent-workflows/skills/entry/references/inception-single-skill.md` である。
完成数は文書へ手書きせず、`agent-workflow-implementation-status` の判定を使う。

## 1. この完成判定が意味すること

2026-09-08時点で、sourceとして計画した次の面は揃っている。

- named contract 60件
- software Workflow profile step 23件
- additional required surface 11件
- Skill、Group/Profile manifest、schema、Control Kernel、runtime adapter、
  project-local installer/wrapper、evidence、説明書、試験fixture
- source-wide integrationとsource releaseの機械的判定

最終candidateに対する全体回帰は59 test file、638 testで全件成功した。
この後の変更がsource/runtime/schema/evidenceへ及ぶ場合、この結果を無条件に再利用しない。

完成状態は `source-complete-operationally-pending` である。これは次を完了扱いしない。

- Nix generation build、`nix build`、`darwin-rebuild`等の実環境検証
- actual A7 handoff、既存Runのmigration、pointer cutover
- Home Manager / nix-darwin activation
- 実利用者による全Inception Skillの判断品質とhostのclear操作
- personal profileの実案件pilot、live runtimeの継続運用
- H1 objective audit、H2 Run outcome、H3 archive/continue
- full Workflowのoperational adoption

したがって、保守者は「sourceが完成した」と「本番導入が完成した」を同義にしない。

## 2. Workflowの責務分割

標準的な流れは次のとおり。

1. Group AがBootstrap、所有境界、隔離、rollback、Control Kernelへの移行を扱う。
2. Group Bが依頼から真の目的と明示的な目的承認を作る。
3. Group Cが成果、依存、測定、目標、baselineを固定する。
4. Group Dが現状、仕様、選択肢、設計、契約、task分割、並列順、検証と復旧を固定する。
5. Group EがTaskを実行し、成果を検証し、必要な修正だけを有限に行う。
6. Group Gがdecision evidenceを分類、重複整理し、人間が選んだものだけを昇格する。
7. Group Hが目的達成監査、人間によるRun outcome、archiveまたは継続を扱う。
8. F1〜F7の共通closureは各Group境界でbundle、checkpoint、停止、再開を扱う。

Groupは責務境界、Context Epochはfresh-context境界であり、同じ概念ではない。
clearや新しい会話で、承認、期限、review round、fix attemptを初期化しない。

## 3. Inceptionは1会話1 Skill

Group B/C/Dの現行運用は、各named Skillを一つずつ人が明示して呼ぶ方式である。
一つのSkillが成果物と短いhandoffを保存したら停止し、人がclearまたは履歴を継承しない
新しい会話へ移り、次のSkillを明示する。

維持する条件:

- Skillは次のSkillを自動起動せず、残りのInceptionを子Workerへ丸投げしない。
- 質問への回答待ちは同じSkillを継続する。未解決のまま次へ送らない。
- 保存済みの物理path、version、digestから再開する。会話要約を正本にしない。
- 保存失敗、blocked、needs-userを成功としない。停止理由をhandoffへ残す。
- Skill終了はGroup closureではない。Group全体のclosureは境界で一度だけ行う。
- B7の人間承認は、B1〜B6を再検証して実Runへ採用する。AIが承認を生成しない。
- D closure後にclearし、人が `execution-preflight` を明示してGroup Eを始める。
- helperやruntime adapterはモデルを起動せず、hostのclearを実行または証明しない。

`init/save/resume` はpre-Run候補の保存器であり、compiler合格や目的承認ではない。
`runtime adopt/status/step/close/advance` は保存物を実compilerとKernelへ接続する別経路である。

## 4. 親子関係と二段階レビュー

親OrchestratorはDAG、authority、期限、write scope、ready frontier、統合、Kernelの
CAS/HEAD/checkpointを所有する。domain correctnessを自分で捏造せず、次の役割を分離する。

- Worker: 狭いTask package内で実装し、自己確認とresolution claimを返す。
- Reviewer 1: architecture / specification / safetyを検査する。
- Reviewer 2: integration / quality / operability / time / dotfilesを検査する。
- Finding Validator: 二つのreview候補を一度結合し、重複、materiality、既存decisionを評価する。
- Repair Worker: Validatorが `required` としたFindingの許可scopeだけを修正する。
- Fresh rereviewer: 修正後candidateを新しいcontextで再検査する。

Reviewerの指摘は、そのまま修正命令ではない。Validatorは `required`、`defer`、
`reject`、`needs-user`、重複のいずれかへ分類する。理由のある設計選択、style preference、
現在の受入を壊さない軽微な問題は、Workflowを終わらせないblocking Findingにしない。
Workerは自分のFindingを閉じず、ReviewerとValidatorは実装を変更しない。

## 5. 有限収束と並列実行

同一root causeはcanonical identityへ束ね、別Finding ID、別subloop、clear後の別rootとして
再発行しない。重複はreview roundやfix attemptを消費しない。Findingごとのproduct fix
attemptの既定上限は5回で、より小さいSection固有上限は許される。上限到達後は、過去の
失敗証拠を保持したterminal/replan/人間判断へ進み、暗黙に予算を足さない。

並列数に人工的な固定上限は設けない。ただし並列化できるのは、依存がacceptedで、
入力digestが確定し、write scopeとresource claimが競合しないready taskだけである。
実行環境の実効capacity、期限、fresh reviewを完了できる残予算は常に制約となる。
未確定のdependent taskを投機実行しない。

## 6. Repair candidateのidentityはGitで固定する

Group Eのpost-E6 repairでは、会話中に列挙した任意のbytes集合ではなくGit commitを
candidate identityに使う。

- repair開始時にbaseline commit/treeを記録する。
- Workerの修正は別commitとして固定する。
- candidate commitがbaselineの子孫であることを確認する。
- `git diff <baseline>..<candidate>` の変更pathが、許可されたfix scopeと完全一致することを確認する。
- artifact bytesはcandidate commitから読み、未コミットworking treeの見た目を証拠にしない。
- 最終配布時にlocal squashすることは許される。ただし進行中Runのreceiptが参照する
  commit identityを書き換えた場合、そのreceiptを新しいcommitへ自動転用しない。

この規則は通常の開発者操作すべてに細かな中間commitを強制するものではなく、
repair authorityと検証対象を一致させる実行時contractである。

## 7. production-only finalization gate

rehearsal/mock Runは、自分の隔離Runを閉じて試験結果を残せる。しかし、その最終recordを
本番Runの完了証拠へそのまま昇格できない。productionへ採用しようとする時だけ、
`assert_production_adoptable` 相当の境界で次を確認する。

- 採用先が `real` modeのruntime identityである。
- final recordが、その同じruntime identityへ結び付いている。
- execution claimがproduction-authoritativeである。

これはsource修正のたびに先にrebuildするためのgateではない。rehearsalで設計とsourceを
修正し、source候補を検証してから、別権限でrebuild・real Run・activationへ進む。
mock回答や隔離試験を本番承認へ転用しないための最後の境界である。

## 8. Project-local配布

global installationだけに依存せず、対象projectへ一つの整合したsnapshotをcreate-onlyで
配置できるようにする。installerは
`agent-workflows/manifests/project-local-inception-release.json` を読み、Skill、runtime、
schema、Group、catalog、profile bindingのbytesを同時に固定する。

wrapperは毎回manifestとmanaged inventoryを確認し、source drift、余分なmanaged file、
symlink、既存fileとの衝突を拒否する。project-local snapshotのSkillと別revisionのglobal
runtimeを混ぜない。`.agent-workflow/` は対象projectの明示的なruntime領域であり、
current Run/objectiveやlive Codex設定をinstallerが勝手に変更しない。

## 9. 実行と安全境界

macOS task processはsandboxed childではなくtrusted parentをstate writerとする。
childにはpositive read allow-listと狭いwrite rootだけを渡し、broker state、credential、
dotfiles、外部stateへ権限を広げない。callerが自己申告した「安全」をreceiptとして
受理せず、親がcommand/process/sandbox/terminal identityを結合する。

同一login UIDの悪意ある別processによるproject ancestorのrenameと、publication syscall直後の
trusted parentへのSIGKILLは、現行practical threat profileのaccepted residualである。
これらを防げると文書やUIで主張しない。より強い保証が必要なら、別のOS isolationまたは
broker設計を新しいdecisionとして扱う。

## 10. 保守手順

変更前:

1. 変更対象contract、profile step、surfaceと、そのownerをcatalogから特定する。
2. current source status、Git baseline、関係manifest、receipt、未完了operational gateを読む。
3. source、generated/evidence、Nix、migration、activation、Git publicationを別の権限単位として扱う。

変更時:

1. canonical source、schema、runtime、focused testを同じcandidateで整合させる。
2. 過去のaccepted evidenceを上書きして新しい実行証拠に見せず、必要ならcurrent compatibility
   extensionまたは新versionを作る。
3. digestを持つconsumerを検索し、変更したsourceから外側へ順に更新する。
4. project-local配布対象を変えた場合はrelease manifestを再生成して `--check` する。
5. frozen candidateに対してfocused test、source classifier、全体回帰、`git diff --check`を行う。

完了報告時:

- source、build、activation、live validation、Git publicationを別々に報告する。
- test件数は異なるsuiteを合算せず、対象candidateと実行commandを示す。
- defer、accepted residual、未検証gateを消さない。
- current countsはmanifest classifierから読み、古いREADMEやHTMLの数字をコピーしない。

## 11. この作成セッションだけの方針

次は完成までの実装運用を短縮するためのtemporary policyであり、製品Workflowの既定動作ではない。

- S2〜S8のSection-wide fast mode
- Section 3時間、全体5時間を目安にしたwall-clock運用
- individual Artifact reviewの一部省略とSection単位の一括review
- stepごとの開始・終了時刻表示
- 「小さい問題は後で直す」という今回candidateに対する優先順位
- executor capacityまでの大量並列発行という、この作成セッションの運用判断

一方、二軸review、単一Validator、required-only repair、重複排除、有限budget、
1会話1 Inception Skill、物理handoff、Git candidate identity、production adoption境界は
実装されたdurable contractである。

## 12. 関連文書

- `docs/ai-agent-workflow-manual.html`: 利用者向けの呼出し順と運用説明
- `docs/ai-agent-workflow-usage.html`: 全体の用途、構成、completion projection
- `docs/a6r-codebase-overview.html`: Control KernelとA6Rのコードベース概要
- `docs/reports/inception-helper-trial-2026-09-06.md`: pre-Run helperの隔離試験
- `docs/reports/inception-runtime-trial-2026-09-06.md`: B〜E接続の隔離実Run試験
- `agent-workflows/README.md`: CLI、project-local installer、status commandの入口
