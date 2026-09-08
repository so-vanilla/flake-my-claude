# AIエージェント運用基盤の全面再構築プラン

Status: target architecture selected / Bootstrap control-kernel source and preactivation verified / full Workflow implementation, operational adoption, activation, commit, and push remain separately gated

基準日: 2026-09-03（Asia/Tokyo）

## 1. 目的

個人用および会社用のAIエージェント運用を、既存のAI関連設定を前提にせず再設計する。

最終的に目指す状態は次のとおりである。

- AIが最終目的を見失わず、定性的な小目的と定量目標へ分解して作業できる。
- 承認済みの目的を第一級かつ保護された成果物として扱い、計画や個別Skillから暗黙に変更できない。
- 長い作業でも、会話のコンテキストではなく物理ファイルから現在地を復元できる。
- Workflow全体、clear境界となるGroup、個々のSkillを分離する。
- 必要なSkillだけを使い、常時読み込まれる情報を小さく保つ。
- 疎に分割できる作業ではSub Agentを最大限並列化し、密な判断は一つの文脈で統合する。
- 個人用から会社用へ拡張するとき、権限、監査、承認、配布、更新を追加できる。
- Nixは静的で再現可能な部分を管理し、Codex自身が更新する状態をimmutableにしない。
- `.local/agent/` に蓄積した判断候補を、利用者承認を経てADR等の永続文書へまとめられる。

この文書は再構築の設計と実装順を定義する。既存AI関連ファイルの削除、置換、Home Manager activation、Git操作は別途承認を必要とする。

## 2. 用語

| 用語 | このプランでの意味 |
| --- | --- |
| 目的 | 最終的に実現したい状態と、その理由 |
| 小目的 | 目的を構成する、定性的に表現された到達状態 |
| 定量目標 | 小目的を達成したと判断するための数値、比率、件数、時間、または合否条件 |
| 計測方法 | 現在値と結果を取得する計算方法、データ源、観測期間、頻度 |
| Workflow | 目的達成までのGroup、分岐、承認、完了条件を持つ全体経路 |
| Group | 同じコンテキストで連続して扱うSkillのまとまり。終了がclear境界になる |
| Skill | 一つの明確な成果物を作り、明確な完了条件を持つ最小の作業手順 |
| Context Epoch | 実行時の文脈を閉じて再開可能な成果物へ固定する単位。Level 4ではなく、同じGroupに複数Epochを持てる |
| Artifact Bundle | EpochまたはGroup出口で固定するcanonical artifact、digest、acceptance evidence、approved decisions、unresolved items、invalidated artifacts、next inputsの束 |
| Clear boundary | 未保存文脈を正本へ閉じ、次のEpochまたはGroupを物理stateから開始する境界 |
| Run | ある目的についてWorkflowを一回実行する記録単位 |
| Checkpoint | Group終了時に保存する、再開に必要な状態と証拠 |
| Bootstrap Group | このWorkflow自身を作る最初のLevel 2 Group。既存frameworkを実行せず、このプランを手動の正本として最小基盤を作る |
| Artifact/Task DAG | Artifactの依存関係とTask attemptを結び、ready、blocked、並列実行、無効化を導出する実行時graph。三つのsemantic layerを置き換えない |
| Thin Controller | 利用者との対話と承認receiptだけを扱う小さい窓口。domain実装、review裁定、状態遷移は行わない |
| DAG Orchestrator | version付きcommandを機械検証し、Runのcanonical HEADを更新できる唯一のstate transition owner |
| Candidate Finding | Reviewerが発行するstable ID/fingerprint付きの指摘候補。独立評価前はfix authorityを持たない |
| Finding | fresh Finding Validatorが`required`と認めた指摘。Workerは修正証拠を出せるが、自分ではcloseできない |
| Finding validation | 元Reviewerと異なるfresh actorがcandidateを`required / defer / reject / needs-user`へ分類する、reviewとは別の第二段階 |
| HEAD | immutable transaction chainの現在revision/digestを指す、Runごとに一つのatomic pointer |
| Decision candidate | 観察、提案、採否、理由等から抽出した正式記録の候補。承認前は決定ではない |

「目的・小目的・定量目標」は何を達成するかを表す。「Workflow・Group・Skill」はAIがどう作業するかを表す。両者を同じ階層として扱わない。

## 3. 決定済みの方針

### 3.1 三つの実行レイヤー

1. Level 1: Workflow全体
2. Level 2: clear境界となるGroup
3. Level 3: 各Skill

clearは原則としてLevel 2 Groupの終了後に行う。Level 3 Skillのたびにはclearしない。密接につながるSkillを同じGroupに置き、そのGroupの成果物とCheckpointを保存してからclearする。

Level 1からLevel 3までの階層そのものがclear境界なのではない。clearを発生させる場所はLevel 2の終了点である。Level 1の終了はRun全体の完了、Level 3の終了は同じGroup内の次工程への移行を意味する。

Context Epochはこの三層へ第四の実行階層を追加しない。EpochはRunの実行時管理情報であり、一つのGroupが複数Epochにまたがることを許す。同じGroup内で文脈を閉じる場合も、Groupの意味・責務・承認境界は変えない。Epochを閉じる条件はtoken数だけではなく、次の意味上の境界である。

- 調査からmutationへ移る時
- 設計から実装へ移る時
- 実装から独立reviewへ移る時
- authorityまたはapprovalが変わる時
- canonical artifactのversionが変わる時
- token budgetの予測が通常上限へ近づき、次の成果単位を安全に保持できない時

目安としてcontext budgetはtarget `200K`、通常上限 `300K`、運用上の絶対上限 `500K`とする。実測値を取得できない場合は`token_status: exact | estimated | unavailable`を保存し、推定値をexactとして扱わない。Epoch終了時にはGroupを完了扱いにせず、次Epochの入力へbundleを渡せる。

### 3.2 モデル方針

- 通常の簡易タスクから一般タスクまで、rootとSub Agentの既定を `gpt-5.6-luna` / `max` とする。
- 原因究明や設計矛盾の解消など、通常モデルで実際に問題解決が停滞した場合だけ、例外的なArbiter taskとして `gpt-5.6-sol` / `high` を使う。通常のGroupやSkill全体をSolへ切り替えない。
- Solへの切替は該当Arbiter attemptだけに限定し、完了後はLunaへ戻す。
- モデル変更だけで目的、権限、計画、完了条件を変更しない。
- モデル変更は助言または明示された実行設定として扱い、永続設定を裏で書き換えない。

OpenAIのAPIモデル資料では `gpt-5.6-luna` と `gpt-5.6-sol` の両方が `max` を含むreasoning effortを扱う。一方、2026-09-01時点のCodex `config.toml` リファレンスは `model_reasoning_effort` の値として `minimal | low | medium | high | xhigh` のみを列挙している。この差があるため、`gpt-5.6-luna` / `max` は方針として採用するが、Nixや配布物へ固定する前に実際のCodexで新規thread、再起動、resumeを試験する。

Sources: [GPT-5.6 model guidance](https://developers.openai.com/api/docs/guides/latest-model), [GPT-5.6 Luna](https://developers.openai.com/api/docs/models/gpt-5.6-luna), [Codex configuration reference](https://learn.chatgpt.com/docs/config-file/config-reference)

### 3.3 目的の確定

Entryでは利用者の入力を暫定目的として受け取る。その後、`grill-me`相当のGroupで背景、制約、期待する変化、真の目的の候補を整理する。

目的はこのGroupの最後に利用者が承認して確定する。AIは次を行えるが、承認なしに目的を確定または変更しない。

- 現在の目的の問題点を指摘する。
- 真の目的の候補を複数提示する。
- 候補ごとの理由、利点、失うものを示す。
- 目的を変更した場合の既存計画への影響を示す。

目的確定後は `objectives/vNNN.md` に承認者、承認日時、非目的、制約を保存し、`run.yaml`のcurrent pointerをその版へ進める。目的を再度変更するときは、新しい版と影響分析に対する利用者承認を必要とする。

### 3.4 目的から目標への分解

目的を、まず定性的な小目的へ分解する。その後、定量目標を二段階で具体化する。

1. 並列化前: 小目的ごとの成果指標と目標値を、全workerが共有する上位基準として定める。未確定の場合は暫定値または「測定設計中」とする。
2. タスク分割後: 各タスクが上位の小目的へどう寄与するかを示し、タスク固有の合格条件へ具体化する。

すべての定量目標を分割後のworkerへ委ねない。そうするとworkerごとに別の成功定義が作られ、すべてのタスクが完了しても小目的を達成しない可能性がある。

定量目標には原則として次を含める。

- 現在値
- 目標値または許容範囲
- 期限または観測期間
- 計算方法
- データ源
- 計測頻度
- 小目的との関係

妥当な数値を作れない場合は捏造せず、「未定義」または「測定設計中」と記録し、計測方法を設計するGroupへ進む。

### 3.5 判断記録

このプランで承認済みまたは実装前提とする重要判断をまとめる。別のledgerは作らず、このプランを再構築設計の正本とする。実装後に複数repositoryや会社標準へ影響するものは、必要に応じてADRへ昇格する。

| Status | 判断 | 理由 | 採らない案 | 見直す条件 |
| --- | --- | --- | --- | --- |
| accepted | Workflow、Group、Skillの三レイヤーにする | 全体管理、context分離、最小成果物を別々に扱うため | 一つの巨大Skill、各Skillを独立Workflow化 | pilotでGroup境界が復旧不能または過剰になる |
| accepted | clearはGroup終了時に行う | 密なSkill間の文脈を保ちつつ、次段階を小さいcontextで始めるため | Skillごとのclear、Workflow完了までclearしない | resume成功率またはhuman waitが悪化する |
| accepted | 真の目的の変更は利用者承認を必要とする | 目的変更は全成果と権限へ影響するcritical decisionだから | AIによる自動確定 | 利用者が明示的に別の委任範囲を定義する |
| accepted | Group終了時に次Groupを整理し、future planだけを更新する | 履歴を保ちながら現実に合わせて再計画するため | Skillが全体stateを自由に書き換える | state machineの実装でより強い整合性方式が確認される |
| accepted | 上位の定量目標を分割前に共有し、task合格条件を分割後に具体化する | worker間で成功定義が分裂することを防ぐため | すべての数値化をworkerへ委ねる | 目的自体が探索的で、測定設計が独立した先行taskになる |
| accepted | `~/.codex/config.toml` をNix所有外に置く | Codexがtrust等を追記できるwritable stateを守るため | `programs.codex.settings` やwhole-file symlink | Codexがportable configとapp-owned stateを公式に別fileへ分離する |
| accepted | 明示スクリプトによるconfig配置と限定mergeを許可する | 再現性とwritable runtime stateを両立するため | 完全手作業、activation時の自動上書き | merge安全性を証明できない、または公式設定commandが代替する |
| accepted | Luna maxを通常、Sol highを例外とする | 利用者の費用と推論量の方針に合うため | taskごとの頻繁なmodel変更、Sol常用 | Codex対応、品質、時間、費用の実測が許容範囲を外れる |
| accepted | AI-DLCとSuperpowersを今後のruntime/lifecycle ownerにしない | 小さいon-demand Skillと一つの独自Workflowへ統一するため | AI-DLC/Superpowersの導入、adapter、併用 | 利用者が別の採用調査を明示的に開始する |
| accepted | AI-DLC、Superpowers、Spec Kit、OpenSpec、BMAD等は設計資料としてのみ参照する | 実績ある工程やartifact contractを拾いつつ二重orchestratorを避けるため | 参照資料も削除する、frameworkをそのまま実行する | provenanceを維持できない、または一次資料が失効する |
| accepted | この再構築自体を最初のBootstrap Groupとして実行する | 新基盤を作るために旧AI-DLCへ依存する循環を避けるため | AI-DLCで新Workflowを作る、無記録の手作業 | 最小基盤が自分自身のRunを安全に扱えない |
| accepted | 目的は版付きで保存し、参照と変更権限を計画から分離する | 目的を第一級かつ保護された状態にするため | `plan.yaml` 内へ目的本文を埋め込む | schema上で同等以上の保護が証明される |
| accepted | `.local` の判断候補を承認付きSkillで永続文書へ昇格する | 会話や中間報告に埋もれた理由を残し、誤った自動確定を防ぐため | 全eventをADR化する、AIが自動的に決定する | 候補抽出の費用が価値を継続的に上回る |
| accepted | Context EpochはLevel 4ではなく実行時管理情報とし、同じGroupに複数Epochを許す | Groupの意味を保ったままcontextを閉じ、token量だけに依存しない境界を持つため | EpochをLevel 4の別Workflow階層にする | Group内handoffの復旧率または運用負荷が許容範囲を外れる |
| accepted | Epoch/Group出口をArtifact Bundle兼Checkpointにする | downstreamが会話や上流本文のコピーに依存せず、versionとdigestだけで再開できるため | 自然言語summary、compaction、tool outputを正本にする | bundleの完全性検証が維持できない |
| accepted | Group間はversion付きartifact referenceだけを渡し、不足は所有する上流へ戻す | 正本の重複と下流の再解釈を防ぐため | 上流spec/objectiveの本文をbriefへ複製する | 明示的な互換変換が必要な場合に別versioned artifactが承認される |
| accepted as target design | Artifact DAG Control Kernelを実装対象にする | 4案の独立採点後、統合案が全reviewerで最上位案の中央値を上回り、部分成功、独立review、復旧を一つの小さいkernelへ統合できたため | 現行案の継続、Thin Controller単体、DAG単体、full typed state graph | G1-G8の実測で不成立、または複雑性が効果を上回る |
| accepted | canonical Run stateをimmutable objects、immutable DAG transaction chain、atomic HEADの一系統にする | crash位置を区別し、projectionやevent viewを再生成可能にして二重正本を避けるため | `run.yaml`と`events.jsonl`を別々の可変正本にする、graph DBを正本にする | fault injectionでsilent advanceまたは復旧不能が見つかる |
| accepted | Controller、Orchestrator、Worker、Reviewer、Arbiterのcapabilityを分離する | user-facing contextを小さくし、Workerの自己採点とControllerの黙示裁定を防ぐため | rootが作業、統合、review close、state updateを兼任する | 実agentで権限分離を強制できない |
| accepted | personal coreはfile-backed single writerとし、会社機能はadapterに分離する | 個人運用へidentity/policy/audit serviceの複雑性を持ち込まないため | 最初から会社用graph serviceを必須にする | 複数writerや統制要件をfile-backed adapterで満たせないことが実測される |

### 3.6 文書階層と実行時context

恒久的な設計文書は必要な大きさを許容する。ただし、実行時に全文を常時読み込まない。

| 文書 | 正本となる内容 | 読み込む場面 |
| --- | --- | --- |
| このマスタープラン | 目的、設計原則、所有権、段階、停止条件 | Bootstrap、設計変更、全体監査 |
| [ステップ詳細カタログ](./ai-agent-workflow-step-catalog.md) | 各Group/Skill候補の入力、成果物、完了条件、失敗時処理、出典 | Group定義の作成・改訂時に対象部分だけ |
| Workflow/Group manifest | 実行順、分岐、依存関係、承認点 | 対象Runと現在Groupの開始時 |
| Skill本文 | 一つの成果物を作る手順 | Skillが明示起動またはGroup内で選択された時 |
| RunのCheckpoint/brief | 現在の目的参照、入力、状態、次の行動 | resume、worker dispatch、clear後 |
| Context Epoch record | Epoch ID、Group ID、boundary reason、token status、入力bundle | Epoch開始、予測上限接近、意味境界の判定時 |
| Artifact Bundle | canonical artifactのpath/version/digest、acceptance evidence、approved decisions、unresolved、invalidated、next inputs | Epoch/Group出口とresume検証時 |

下流文書は上流の目的や仕様をコピーして再解釈せず、`path`、`version`、`digest`で参照する。上流の不足を見つけた場合は、下流で補完して正本を分裂させず、所有する上流Skillへ戻して修正する。要約やcompactionは補助表示にとどめ、正本は物理的なimmutable objects、transaction chain、HEADである。Artifact BundleはHEADに束縛された再開用projectionとして扱う。

### 3.7 採用する実行architecture

Workflow / Group / Skillは、人間が目的と工程を理解する三つのsemantic layerとして維持する。Artifact/Task DAGは第四の業務階層ではなく、Artifact依存からready frontier、並列batch、blocking、無効化、fix/review attemptを導出するruntime表現である。Context Epochも引き続きruntime metadataである。

canonical Run stateは、content-addressedなimmutable object、parent revision/digestを持つimmutable DAG transaction、そのcurrent transactionを指すatomic `HEAD`だけで構成する。`run.yaml`、`plan.yaml`、`status.md`、ready frontier、event view、Artifact Bundle、Checkpoint、global indexはprojectionとし、HEADから再生成できなければならない。transaction chainとは別に更新可能なevent journalを正本にしない。

状態遷移はDAG Orchestratorだけが行う。Thin Controllerはallowlistされたcontrol projectionを読み、利用者入力をexact request digestへ結び付けたreceiptを出す。WorkerはTask packageからwork productを作り、Reviewerは独立したReview packageからFinding/Verdictを作る。WorkerはFindingのresolution claimを提出できるがcloseできない。密な統合作業は一つのconvergence Workerへ渡し、その結果をfresh Reviewerが検査する。Orchestratorはdomain上の正しさを裁定しない。

実行edgeは初期実装では`requires`、`produces`、`authorizes`、`verdict-for`、`converges`に限定する。review/fix loopはcycleを作らず新しいattemptを追加する。未知のnode/edge type、graph database、daemon、full typed historical graph、会社用central serviceは、繰り返し必要性が観測されるまでpersonal coreへ入れない。

この選択はtarget architectureの採用であり、operational adoptionではない。比較、採点、pull元、未採用要素、残るgateの正本はRun内の`architecture-selection/final-selection.md`とする。A6の既存成果を完成品とみなさず、A6Rでconverter、transaction/HEAD、role guard、package、Finding、crash recoveryを検証してからA7へ進む。

## 4. 現在のCodexに合わせたSkill設計

### 4.1 Level 1 metadataの現実

当初の「未使用SkillのLevel 1 metadataは一切読まれない」という前提は、現在のCodexにはそのまま当てはまらない。

Codexは最初に各Skillのname、description、pathを一覧として持ち、選択後に完全な `SKILL.md` を読む。初期一覧はcontext windowの最大2%、context windowが不明な場合は8,000文字までというbudgetを持つ。Skill数が多い場合はdescriptionの短縮またはSkillの省略が起こり得る。

また、`agents/openai.yaml` の `policy.allow_implicit_invocation: false` により暗黙起動は無効化できるが、初期一覧からmetadataが完全に消えることを意味しない。

Source: [OpenAI: Build skills](https://learn.chatgpt.com/docs/build-skills)

### 4.2 採用する対策

- 常時見えるdescriptionは一文にし、用途と発火条件だけを書く。
- 重要なWorkflow Skillは `allow_implicit_invocation: false` とする。
- 利用者は、Checkpointに記録された正確なSkill名を明示的に呼ぶ。
- Skill本文からしか必要にならない説明、例、テンプレートはreferencesやassetsへ分離する。
- 全Workflowの全Skillを常にglobalへ配らず、個人共通、会社共通、project固有を分ける。
- Skill catalogが初期budgetへ近づいたら、Workflow単位のproject-local配布またはinactive Skillの非配置を行う。
- catalog budget、description短縮、Skill省略をfresh sessionで測定する。

Skillを非常に多数配置して初期contextを完全にゼロにすることは、現在のCodexのSkill discoveryとは両立しない。絶対にゼロへ近づける必要が生じた場合は、公開Skillを `entry`、`status`、`resume`、`run-group` 程度に絞り、個々のstepをSkillではなく明示的に読み込む内部手順へ変える。この代替は、実測でcatalog budgetが問題になった場合だけ採用する。

## 5. Group終了と次のGroupの決定

各Level 2 Groupの最後に、共通の `close-group` 処理を置く。Groupが大きい場合は、同じGroup内の`close-epoch`で文脈だけを閉じて継続する。両方の処理は次を行う。

1. Group内の成果物と検証結果を収集する。
2. 現在の目的、小目的、定量目標との一致を確認する。
3. 未解決事項と失敗を記録する。
4. canonical artifactの`path`、`version`、`digest`を束ねるArtifact Bundleを作る。
5. acceptance evidence、approved decisions、unresolved items、invalidated artifacts、next inputsをbundleへ保存する。
6. context budgetのtarget、通常上限、絶対上限と`exact | estimated | unavailable`のtoken statusを保存する。
7. version付き`close-epoch`または`close-group` commandをDAG Orchestratorへ渡し、immutable transactionとatomic HEADをcommitする。eventはそのprojectionとして生成する。
8. HEADに束縛されたCheckpointを作成する。CheckpointはArtifact Bundleを兼ね、fresh contextから復旧可能にする。
9. Group終了時だけ次のLevel 2 Groupを提案し、同一GroupのEpoch終了時は次Epochを提案する。
10. 次のSkill、必要なversioned inputs、開始条件、clear要否を整理する。
11. Workflow側の予定と依存関係に照らして提案を検証する。
12. 将来計画部分だけを更新する。
13. clearの実施と、利用者による次Groupまたは次Epochの明示起動を案内する。

完了済み履歴と承認済み目的は上書きしない。予定変更は新transactionでfuture graphだけを置き換え、理由をevent projectionへ出す。`plan.yaml`を直接更新しない。

次のGroupを一つに決められない場合は、候補、理由、必要な判断を利用者へ提示する。目的、scope、risk、予算、外部権限を変える分岐は自動確定しない。

Epoch終了はtoken数の機械的閾値だけで発生させない。調査→mutation、設計→実装、実装→独立review、authority/approval変更、canonical version変更では、予算に余裕があってもEpochを閉じる。Group終了は必ずclear境界とし、同じGroupを次Epochで続ける場合も前Epochのbundleをversion/digestで参照する。

## 6. 各Skillの完了契約

各Skillは一つの成果物を作り、最低限次を返す。

```yaml
result:
  command:
    schema: dag-command/v1
    command_id: ""
    actor_role: worker | reviewer | arbiter | controller | orchestrator
    expected_head_revision: 0
    expected_head_digest: ""
    authority_ref: ""
    idempotency_key: ""
  workflow_version: ""
  context_epoch:
    id: ""
    group_id: ""
    boundary_reason: ""
    token_status: exact | estimated | unavailable
    token_count: null
  input_state_revision: 0
  produced_state_revision: 0
  produced_head_digest: ""
  checkpoint_id: ""
  outcome: completed | blocked | needs_decision
  artifact_paths: []
  artifact_digests: []
  artifact_bundle:
    path: ""
    version: ""
    digest: ""
    canonical_artifacts: []
    acceptance_evidence: []
    approved_decisions: []
    unresolved_items: []
    invalidated_artifacts: []
    next_inputs: []
  acceptance_evidence: []
  purpose_alignment:
    status: aligned | uncertain | diverged
    explanation: ""
  next_skill_advice:
    target_group: ""
    recommended: ""
    reason: ""
    required_inputs: []
    expected_workflow_version: ""
    expected_state_revision: 0
    expected_checkpoint_id: ""
    start_conditions: []
    invalidation_conditions: []
    alternatives: []
    clear_before_start: false
  model_advice:
    model: gpt-5.6-luna | gpt-5.6-sol
    effort: max | high
    reason: ""
```

次Skillの助言は「現在のworkflow version、HEAD revision/digest、Checkpointを前提とした提案」であり、実行命令ではない。`resume` またはDAG Orchestratorは開始前にこれらと入力artifactの存在・digest・鮮度を検査し、ずれていれば古い助言を実行せず再計画する。

Skillは次Skillを自動実行しない。ただしこれは「初期metadataを読まないから」ではなく、次の理由による。

- Group内ではオーケストレータが助言を検証して継続できる。
- Group境界ではclear後に利用者が正確なSkill名を明示起動できる。
- 予定外の分岐や目的変更に承認を挟める。
- 再開時に会話ではなくCheckpointを根拠にできる。

## 7. 物理ファイルによる状態管理

### 7.1 推奨配置

```text
.local/agent/
├── run-index.jsonl
└── runs/
    └── <run-id>/
        ├── HEAD
        ├── objects/
        ├── transactions/
        ├── inbox/
        │   ├── commands/
        │   └── results/
        ├── packages/
        │   ├── tasks/
        │   └── reviews/
        ├── run.yaml
        ├── objectives/
        │   ├── v001.md
        │   └── v002.md
        ├── plan.yaml
        ├── status.md
        ├── events.jsonl
        ├── checkpoints/
        │   └── <sequence>-<group-id>.md
        ├── epochs/
        │   └── <epoch-id>/
        │       ├── context.yaml
        │       └── artifact-bundle.yaml
        ├── artifact-bundles/
        ├── artifacts/
        ├── decision-candidates/
        └── reports/
```

`.local/agent/` はrepository-localで、公開しない作業状態として扱う。正式な仕様、ADR、コード、テスト、利用者向け文書はrepositoryの通常の公開場所へ置き、`.local/agent/` から参照する。

### 7.2 ファイルの役割

| ファイル | 役割 | 更新方法 |
| --- | --- | --- |
| `HEAD` | current transactionのrevision/digest | DAG Orchestratorだけがlock、CAS、fsync、atomic renameで置換 |
| `objects/` | accepted artifact、package、evidence、Finding、Verdict、approval等のcontent-addressed object | immutable。digest一致後だけpublish |
| `transactions/` | parent、command、actor/role、authority、graph delta、output、compiler versionを持つ履歴 | immutable。DAG Orchestratorだけが追加 |
| `inbox/commands/` | Controller/Worker/Reviewer/Arbiterが提出する未受理command | accepted stateではない。Orchestratorが受理またはquarantineし、受理内容をobject/transactionへ固定 |
| `inbox/results/` | Worker/Reviewer等の未受理resultと外部receipt | untrusted staging。検証前はready/completeを変えない |
| `packages/tasks/` | Workerへ渡すTask packageのobject-backed projection | HEADから再生成。canonical packageは`objects/`にある |
| `packages/reviews/` | fresh Reviewerへ渡すReview packageのobject-backed projection | HEADから再生成。canonical packageは`objects/`にある |
| `run-index.jsonl` | Issue番号、目的、run ID、現在状態からRunを探す索引projection | HEADから再生成 |
| `run.yaml` | Run ID、Workflow、workflow version、現行objective version、外部Issue、owner、現在Groupのprojection | HEADから再生成 |
| `objectives/vNNN.md` | 承認済み目的objectの人間向けprojection | HEADから再生成。canonical objectiveは`objects/`にあり、旧版を保持 |
| `plan.yaml` | Group/Task/Artifact graphとfuture frontierのprojection | HEADから再生成。直接更新しない |
| `status.md` | 人間が一目で読む現在地 | HEADから生成し直せるprojection |
| `events.jsonl` | transactionから生成した、人間・監査向けevent view | HEADから再生成。独立してappendしない |
| `checkpoints/*.md` | clear後に再開するための引継ぎprojection | HEADから再生成。対応するCheckpoint objectはimmutable |
| `epochs/<epoch-id>/context.yaml` | 実行時Epoch、境界理由、token status、入力bundleのprojection | validated open/close commandから再生成 |
| `artifact-bundles/*` | Epoch/Group出口のartifact参照、digest、acceptance、decision、未解決、無効化、次入力のprojection | HEADから再生成。対応するBundle objectはimmutable |
| `artifacts/` | Run外のcode/spec/docs/data等domain artifactへの参照またはlocal work product | Run stateの正本ではない。accepted digest/refだけを`objects/`とtransactionへ固定 |
| `decision-candidates/` | 判断候補objectの人間向けprojection | HEADから再生成。status変更は新command/object/transactionで表す |
| `reports/` | Sub Agent resultの読みやすいprojectionまたは未受理inboxへの導線 | accepted resultはHEADから再生成。raw report自体を正本にしない |

`status.md`を含むprojectionは正本ではない。壊れた場合はHEAD、transaction chain、objectsから再生成できなければならない。

Run配下のpathは必ず次の三つのどれかにschemaで分類する。(a) `objects/transactions/HEAD`から再生成するobject-backed projection、(b) Orchestrator未受理でstateを変えないinbox/staging、(c) Run外domain artifactまたはその参照。accepted Run stateは`objects/`、`transactions/`、`HEAD`だけであり、直接pathへ成果物を書いたことを完了や受理とみなさない。

### 7.3 書込権限、revision、競合

- Runの可変pointerであるHEADには単調増加する `state_revision` とtransaction digestを付ける。
- HEADを進められるのはdeterministic DAG Orchestratorだけとする。Controller、Worker、Reviewer、Arbiterはversion付きcommand/objectを所定のscopeへ提出するが、state transitionを実行しない。
- Sub Agentは割り当てられた`reports/<worker-id>/`または明示された非重複write scopeだけへ書き、Run stateを直接変更しない。
- state更新は、per-Run lock、expected HEADのcompare-and-swap、command/role/authority/schema/digest/DAG/Finding/lease/budget検証、object/transactionのstageとfsync、immutable publish、HEADのfsyncとatomic rename、projection再生成の順で行う。
- revision不一致なら上書きせず、最新stateを再読込して再評価する。
- workflow versionまたはcanonical source digestが変わる場合は、旧versionを上書きせずmigration command、新Bundle object、新transactionをcommitする。event/Bundle viewはHEADから再生成する。下流は`path`、`version`、`digest`だけで上流を参照する。
- transactionには一意なcommand ID、親revision/digest、actor role、authority、入力artifact参照、idempotency keyを保存し、同じcommandの再送には既存receiptを返す。
- publish前crashはstagingをquarantineし、transaction publish後/HEAD前crashはorphanをcurrentにせず、HEAD後/projection前crashはHEADからprojectionを再構築する。parent/object/compiler/graph-root不一致は`blocked_integrity`で停止し、履歴を自動修復しない。
- Epoch終了とGroup終了はそれぞれArtifact Bundle兼Checkpointを残す。bundleの必須欄はcanonical artifacts、acceptance evidence、approved decisions、unresolved items、invalidated artifacts、next inputsである。
- token数を取得できない場合は`unavailable`、推定の場合は`estimated`とし、target `200K`、通常上限`300K`、絶対上限`500K`を記録する。数値を補間してexactとは書かない。
- 目的本文はplanへ埋め込まず、承認済みobjective versionを参照する。目的変更は新しい版、影響分析、利用者承認の後にだけcurrent pointerを進める。

### 7.4 「Issue xxはどこまで進んだか」への対応

`run-index.jsonl` と `run.yaml` に次を保存する。

- `external_refs`: GitHub Issue、社内チケット、文書URL等
- `aliases`: 人間が使う短い呼び名
- `objective_summary`
- `current_group`
- `last_checkpoint`
- `next_recommended_skill`
- `workflow_version`
- `state_revision`
- `objective_version`
- `updated_at`

`status`または`resume` Skillは、`issue xx`、URL、alias、目的の一部からRunを検索し、次を回答する。

- 最終目的
- 現在の小目的と定量目標
- 完了したGroupと成果物
- 未検証事項
- 現在の停止理由
- 次に明示起動するSkill
- 必要なモデル

会話履歴やmodel memoryだけを根拠に「やっていたはず」と推測しない。該当Runがない場合は見つからないと回答する。

### 7.5 判断候補を永続文書へまとめるSkill

`consolidate-decisions` Skillを用意する。このSkillは、`.local/agent/` の中間記録をすべて正式決定とみなすのではなく、次の分類を行う。

- 観察事実
- 未確認の仮定
- 採用前の提案
- task内だけの一時的な裁定
- 利用者または権限者が承認した決定
- 既存決定を置き換える決定候補

永続化候補には最低限、context、決定文、理由と証拠、比較した選択肢、採らなかった理由、影響、owner、見直す条件、元event/artifact、昇格先を含める。Skillは重複、矛盾、supersessionを整理して候補文書を作るが、承認なしにADR、設計正本、会社標準へ書き込まない。

利用者承認後、対象repositoryの規約に従って `docs/decisions/`、ADR、設計文書等へ昇格する。過去eventは変更せず、新しいpromotion transactionへ元event/candidate refと`promoted_to`を保存し、backlink projectionを再生成する。過去のlocal履歴は削除しない。実行候補時点は次である。

- Group終了時に重要判断が検出された時
- 設計・実装・pilot等の大きな段階を閉じる時
- `.local/agent/` を整理・archiveする前
- 利用者が「この作業の判断をまとめて」と明示した時

これにより、即時に一件の決定を記録するSkillと、後から散在した判断をまとめるSkillを分ける。後者は忘れ物の回収であり、承認済みでない提案の自動確定ではない。

## 8. Sub Agentとオーケストレータ

### 8.1 並列化の判断

次のすべてを満たす作業は、原則として並列化する。

- worker間で同じ可変ファイルを書かない。
- 入力と成果物を先に定義できる。
- workerの結果を個別に評価できる。
- 一方の作業中に、他方との頻繁な相談が不要である。
- 矛盾した場合にconvergence Workerとfresh Reviewerが比較できる共通基準がある。

満たさない場合は、一つのAgentで連続実行するか、依存順の直列pipelineにする。

### 8.2 判断の分断を回収するループ

Artifact DAGと収束loopにより、並列化による判断の分断を回収する。DAG Orchestratorは遷移条件を機械判定するだけで、domain判断を統合しない。標準形を次とする。

1. approved objective、小目的、定量目標、入力、出力、authorityからversion付きTask packageをcompileする。
2. 独立Workerがtaskごとのfresh Epochで案、証拠、実装、テストを作る。
3. Workerは全文tool outputをControllerへ返さず、result object、artifact path/version/digest、検証結果、未解決事項を提出する。
4. 独立ReviewerがReview packageからcandidate Findingとreview Verdictを作る。Reviewerの指摘だけではfixを発行しない。
5. 元Reviewerおよび実装Workerと異なるfresh Finding Validatorが、各candidateのBackground、As-Is、canonical To-Be、Gap、evidence、過去Finding、承認済みdecisionを照合し、`required / defer / reject / needs-user`へ分類する。事前承認済みのexact predicateを使う場合も、predicate ID/version/digest、approval、入力Review package/candidate digest、実行receiptをimmutableにbindし、predicate自身による適用資格の自己申告やfix authority発行を許さない。
6. `required`のblocking Findingだけに該当枝のfix attemptを作り、fresh Reviewerが同じFinding IDを再検査する。成功したsiblingは保持する。rereview中の新規candidateも第二段階へ戻し、直接fixしない。
7. domain上の統合が必要なら、一つのconvergence Workerが入力packageを読み、一つの統合成果を作る。
8. 別のfresh Reviewerまたは事前承認されたdeterministic checkが統合結果を検証する。
9. DAG Orchestratorはvalidated review、Finding validation、open required Finding、有限budget、authority、leaseから次transitionだけを導出する。

重複、既解決、superseded、scope外、根拠のないstyle preferenceは`reject`とする。理由と承認記録がある意図的な選択は、新しい証拠がmandatory requirement、安全性、互換性との衝突を示さない限り再修正しない。妥当でも現在の受入条件や観測結果を変えない軽微な改善は`defer`とし、現在の完了をblockしない。spec、scope、risk、budgetの選択を変えるcandidateは`needs-user`とする。

各taskはdispatch前にdeadlineまたはwall-clock timebox、最大review round、Findingごとの最大fix attemptを有限値で持つ。残り時間でbounded fixとrereviewを完了できない、最大roundへ達した、またはFinding別attemptを使い切った場合、新しいworkerを発行しない。DAG Orchestratorは消費済みbudget、成功済み成果、未解決Finding IDs、stop reasonをimmutable transaction/HEAD/Checkpointへ固定し、leaseと未受理dispatchを閉じるdurable non-dispatch terminal transitionをcommitする。fresh `resume`もそこから再dispatchできず、利用者または権限者がreplacement budgetとexpected HEADを新しく明示承認したvalidated commandだけが新revisionで再開できる。required Findingが残る場合は完了を偽らず、Arbiter/human gateへ渡す。旧budgetやterminal履歴を上書きせず、予算を自動延長しない。

ただし、このループは共有された暗黙知や連続した因果推論を完全には復元しない。また、分割、説明、比較、再質問の調整費用が増える。次は原則として一つのAgentまたは一つのGroupで扱う。

- 目的の最終確定
- 相互依存が強い仕様判断
- 一つの因果鎖を追う根本原因分析
- 全体設計の最終統合。ただし実行主体はController/Orchestratorではなく一つのconvergence Workerとする
- worker間の矛盾解消

「並列化可能な部分を最大化し、密な部分は分割しない」を標準とする。

### 8.3 workerの目標

承認済み上位artifactが小目的と定量目標を所有する。各Workerはそれを変更せず、寄与関係とtask固有の合格条件に従う。

仕様が十分に固まった実装では、一つのworkerへE2Eテスト通過など少し遠いゴールを与えてよい。その場合も、変更可能範囲、停止条件、成果物、検証方法を明示する。

Sub Agentはtaskごとに独立したContext Epochとimmutable packageを持つ。HEAD、objective、plan、Checkpointを直接更新せず、Artifact Bundleへの参照を含むresult/review objectだけを返す。workerの全文ログ、自然言語summary、compaction結果は正本ではない。

## 9. 共通GroupとWorkflowの組み立て

### 9.1 コピーを避ける構造

共通処理をWorkflowごとにコピーしない。Group定義を一か所に置き、Workflow manifestがGroup IDと必要なparameterを参照する。

```text
agent-workflows/
├── catalog.yaml
├── groups/
│   ├── bootstrap/
│   ├── understand-intent/
│   ├── grill-purpose/
│   ├── decompose-outcomes/
│   ├── design-measurement/
│   ├── capture-baseline/
│   ├── plan-execution/
│   ├── execute-change/
│   ├── verify-outcome/
│   ├── review-independently/
│   ├── consolidate-decisions/
│   ├── close-group/
│   └── audit-objective/
├── skills/
└── workflows/
    ├── feature-addition.yaml
    ├── bug-fix.yaml
    ├── feature-improvement.yaml
    ├── performance-improvement.yaml
    ├── refactoring.yaml
    ├── research.yaml
    ├── decision-making.yaml
    └── work-improvement.yaml
```

Group定義は入力、成果物、完了条件、許可する次Groupを持つ。Workflow manifestは順序、分岐、必須/任意、riskによる追加Groupだけを定義する。Workflow固有の差はparameterまたは専用Groupで表し、共通Group本文をforkしない。

各GroupとSkillに何を書くかの詳細、完了条件、停止条件、参照元は[ステップ詳細カタログ](./ai-agent-workflow-step-catalog.md)を正本とする。カタログはSuperpowers、AI-DLC、Spec Kit、OpenSpec、BMAD、AI Heroおよびdotfilesの一次資料から構成要素を拾うが、それらのframework自体を実行または配布しない。

Group間のhandoffは本文コピーではなく、version付きArtifact Bundleの参照で行う。下流は`path`、`version`、`digest`、`state_revision`、`workflow_version`を検証してから開始し、不足や矛盾を見つけた場合は補助文書を作らず、正本を所有する上流Skillへ戻す。

### 9.2 共通の開始部分

```text
bootstrap（この再構築Runだけ。最初は手動、最小基盤完成後に自己hostへ移行）
  → understand-intent
  → grill-purpose
  → 利用者による目的承認
  → decompose-outcomes
  → design-measurement
  → capture-baseline（必要な場合）
  → plan-execution
```

通常の新規Runは `understand-intent` から始める。`bootstrap` はWorkflow基盤自体の新設・全面置換時だけ使う。Bootstrapはworktree等の隔離、現状とrollbackの記録、最小stateの初期化、旧lifecycle ownerの停止、walking skeletonの検証、自己hostへの引継ぎを成果物とする。

BootstrapではA5からA6への遷移を最初のContext Epoch境界とする。A5のArtifact Bundle兼Checkpointを閉じ、clearを記録してからA6を新Epochで開始する。A6の`clear_before_start`は`true`であり、同じBootstrap Groupに属することを理由に前Epochの会話を持ち越さない。

`grill-purpose` の後に別の目的探索Groupは追加しない。代わりに次を目的承認recordへ追加する。

- 非目的
- 制約
- 前提
- 成功したときに観測できる変化
- 目的を再度開く条件

### 9.3 ソフトウェアWorkflow

| Workflow | 固有の中核Group | 最終的な主な証拠 |
| --- | --- | --- |
| 機能追加 | 利用者行動の定義、仕様、設計、実装 | 必須E2E、受入条件、実環境または同等環境の結果 |
| Bug fix | 再現、影響、根本原因、修正方針、回帰防止 | 修正前に失敗する再現、修正後の成功、回帰テスト |
| 機能改善 | 現在値、改善仮説、変更、前後比較 | 基準値と変更後の差、悪化していない指標 |
| 性能改善 | workload定義、benchmark、bottleneck、変更 | 同条件benchmark、資源使用量、誤差範囲 |
| Refactoring | 外部契約、変更容易性の問題、構造変更 | 外部動作不変、テスト、複雑性や変更範囲の改善 |
| Security対応 | threat、影響範囲、修正、悪用不能確認 | 再現可能な防御検証、回帰、権限確認 |
| 移行・Release | compatibility、移行、rollback、post-check | dry-run、receipt、再読込、rollback test |

### 9.4 仕事のWorkflow

仕事の目的設計、計測、比較は共通Groupとして作り、ソフトウェアWorkflowからも流用する。

| Workflow | 使用する主な共通Group |
| --- | --- |
| 目的を深める | understand-intent、grill-purpose |
| 真の目的を提案する | grill-purpose、利用者承認 |
| 小目的へ分解する | decompose-outcomes |
| 定量目標を作る | design-measurement |
| 数値化を補助する | design-measurement、proxyの妥当性確認 |
| 現在値を取る | capture-baseline |
| 経過を確認する | collect-current、compare-to-target |
| 前後を比較する | capture-baseline、compare-results |
| 業務を改善する | baseline、hypothesis、change、compare、decision |
| 複数案から決める | criteria、alternatives、evidence、decision record |

データ源や計算方法だけをdomain別adapterとして差し替える。たとえばソフトウェアではbenchmarkやtest、営業ではCRM、業務時間ではtime logを使うが、「現在値を取り、同じ方法で比較する」というGroupの契約は共有する。

## 10. 目的との一致確認

目的は物理ファイルに保存し、次の頻度で確認する。

- 各Skill完了時: 軽量な `aligned | uncertain | diverged` 判定と短い理由
- 各Group完了時: 成果物、未解決事項、次Groupが目的と小目的に合致するかを検証
- 計画変更時: 変更後も定量目標へ寄与するかを検証
- Workflow完了時: 全Skill完了ではなく、目的状態が存在するかを証拠で評価

`diverged` の場合は自動的に目的を変更しない。現在の計画へ戻す、目的変更を提案する、停止する、の選択肢を利用者へ提示する。

## 11. NixとCodex設定の所有権

### 11.1 Nixで管理するもの

- Codex packageと必要なruntime
- 静的なSkill sourceと配布先
- agent定義、hook script、rule等の静的source
- Workflow catalog、schema、validator、doctor
- 外部Skill/frameworkのsource commit、license、manifest
- 生成物の再現と検証command

### 11.2 Nixで管理しないもの

- `~/.codex/config.toml` 全体
- `projects.<path>.trust_level`
- UI state、notice、model migration acknowledgement
- plugin cache、session、database、marketplace state
- machine-local credential、token、approval record
- Codex自身が更新するapp-owned state

ここでいう「Nixで管理しない」は、設定を一切自動化しないという意味ではない。利用者が必要なときに明示実行するスクリプトによる初回配置と限定的な更新は許可する。

Codexの公式資料ではuser-level設定は `~/.codex/config.toml` に置かれ、project trustは `projects.<path>.trust_level` として扱われる。project-local `.codex/config.toml` はtrusted projectでのみ読み込まれる。[Codex config basics](https://learn.chatgpt.com/docs/config-file/config-basic), [Codex configuration reference](https://learn.chatgpt.com/docs/config-file/config-reference)

### 11.3 必須のNix guardrail

次を自動検証する。

- `programs.codex.settings` が存在しない。
- `home.file.".codex/config.toml"` が存在しない。
- `xdg.configFile` 等、別経路から `config.toml` を生成していない。
- `~/.codex` directory全体をsymlinkしていない。
- `~/.codex/config.toml` が通常ファイルで、利用者が書込可能である。
- `~/.codex/config.toml` の実体が `/nix/store` 配下ではない。
- Home Manager activationが `config.toml` を置換しない。trustの再確認が必要になること自体は受入可能とする。

このrepositoryの現在の `flake.nix` では、既存のstaged changeが `programs.codex.settings` を削除し、`enable`、`package`、`skills` を残している。本プランはその変更を上書きせず、再構築時の必須条件として固定する。

### 11.4 モデル設定の配置

モデル運用方針はrepository内のportable manifestに記録する。必要な場合は、明示実行する設定スクリプトがmanifestから `~/.codex/config.toml` を初回配置または限定更新できる。ただし、Home Manager activationや通常のshell起動に連動させない。

推奨形は次のとおりである。

- portable manifest: 既定 `gpt-5.6-luna` / `max`、例外 `gpt-5.6-sol` / `high`、切替条件を記録
- mutable user config: 利用者、Codex app、または明示実行された設定スクリプトが既定値を設定
- Workflow/Group: 必要時に明示的な一時overrideを助言または指定
- doctor: manifestと実効設定を比較し、差分を報告するだけで自動修正しない

設定スクリプトは次の契約を満たす。

- `plan` または `--dry-run` で変更対象キーと差分を先に表示する。
- ファイルが存在しない場合は、最小のportable keyだけで通常ファイルを作成する。
- ファイルが存在する場合は、管理対象キーだけを構造的にmergeする。
- 通常は `projects.<path>.trust_level`、hook trust、notice、UI state、plugin state、未知のkeyを保持する。適用中に別processの変更を検出した場合は複雑な自動mergeをせず停止し、再読込またはtrust再確認を案内してよい。
- 更新前にtimestamp付きbackupを作り、rollback方法を表示する。
- 一時ファイルへ書き、構文検証後にatomic renameする。
- permissionを0600にし、ownerが現在利用者であることを確認する。
- symlinkまたは実体が `/nix/store` にある場合は書き込まず停止する。
- secret、credential、machine固有pathをportable manifestから配置しない。
- 実行後にCodexで設定読込を確認する。project trustの再確認が必要でも許容するが、managed key以外の意図しないwhole-file置換がないことを確認する。
- `apply` は利用者の明示操作とし、Home Manager activationや自動updateから呼ばない。

Workflow基盤のsource、Skill、schema、installerはまずrepository内の専用worktreeまたはbranchで作成・検証し、`~/.codex` へ直接実装しない。配布は所有manifestを持つ明示的なinstallerから行い、次を守る。

- 既存のunmanaged file/symlinkと衝突したら書込前に停止する。
- 自分が以前配置したと証明できるfileだけを置換・削除する。
- staleなmanaged fileの整理は、新しい配布物が完全に生成・検証された後に行う。
- dry-run、backup、rollback、構文検証、fresh-session discoveryを一連の受入試験にする。
- 作成途中で現行環境を壊さないよう、旧環境の無効化と新環境の有効化を別の承認点にする。

受入試験で `max` がCodex user configとして受理されない場合は、勝手に `xhigh` 等へ置換しない。次のいずれかを利用者が選ぶまで停止する。

1. Codex側の対応を待つ。
2. appまたはthread単位の明示選択でLuna maxを使う。
3. 暫定値を別途承認する。

## 12. 全面再構築の実施段階

### Phase 0 / 最初のLevel 2: Bootstrap Group

- このマスタープランと詳細カタログを、旧frameworkの代わりとなる手動の実行正本にする。
- 必要なら専用worktree/branchを作り、現在のworktreeと利用者の未commit変更から隔離する。
- 現在のGit状態、Nix generation、AI関連source、generated file、runtime stateを列挙する。
- 既存の未commit変更と利用者のファイルを区別する。
- 保持、再構築、廃止、app-ownedの四分類を作る。
- rollback先と復元commandを記録する。
- 最小のRun ID、目的version、event、Checkpointを手動で作り、この再構築自体の現在地を残す。
- AI-DLC/Superpowersをlifecycle ownerとして選ぶ設定があれば、削除・無効化対象として列挙する。実際の変更は差分とrollbackを示した承認後に行う。
- 削除対象を確定するが、承認前には削除しない。
- A5のlegacy detachを最初のEpochで閉じ、canonical artifacts、digest、acceptance evidence、approved decisions、unresolved items、invalidated artifacts、next inputsをArtifact Bundle兼Checkpointへ保存する。
- A5→A6はclear境界とし、A6開始条件の`clear_before_start`を`true`にする。同じBootstrap Group内でもA6は新Epochから開始する。

完了条件: 全対象にownerと扱いが付き、隔離、rollback、最小Checkpointが再現でき、`~/.codex/config.toml` が削除・置換対象に含まれていない。A5 Epochのbundleが検証され、A6開始前にclearできる。

### A6R / target architectureへのtrajectory correction

A6で既に作った成果物とrevision 11はmigration sourceとして保持し、削除、上書き、完成扱いをしない。A7の前にA6Rを挿入し、現行Run/Artifact Bundle/worker reportからimmutable object、initial DAG、transaction、HEADへのfield mappingとconverterを作る。

隔離したfixtureでconverter、old/new reader、projection rebuild、cutover、rollbackをrehearsalする。cutoverはpointerだけを変更し、旧readerが読める状態とsource digestを保持する。A6Rの証拠、G1-G8、利用者のmigration approvalが揃うまでA7 self-hostへ進まない。

### Phase 1: 契約とschema

- 用語、三レイヤー、Artifact/Task DAG、状態遷移、role、承認点をschemaとして定義する。
- immutable object、transaction、HEAD、versioned command、Task/Review/Evaluation package、candidate Finding/Finding validation/Verdict、objective、Context Epoch、Artifact Bundle、checkpoint、Skill resultのschemaを作る。
- 完了済み履歴が書き換えられないことをtestする。
- objective versionの保護、DAG OrchestratorだけのHEAD更新、state revision/digest競合、role mismatch、Worker self-close拒否をtestする。
- Group間のversion/path/digest参照と、下流から上流への不足差し戻しをtestする。
- 次Skillと次Groupの助言形式を固定する。

完了条件: サンプルRunをschema validationでき、transaction/HEADから全projectionを再生成し、Epoch/Group出口のArtifact Bundleとfresh readerが現在地を再現できる。

### Phase 2: 最小の共通基盤

- `entry`、二つのTask package、独立candidate Review、fresh Finding validation、required blocking Findingだけのfix、fresh rereview、有限停止、`close-epoch`、`close-group`、`resume/status`を最小一本として作る。成功したsiblingを保持する。`audit-objective`と`consolidate-decisions`はこの骨格の外へ横展開しない。
- immutable objects、transactions、atomic HEAD、command/role guardとprojectionを操作するdeterministic DAG Orchestratorを作る。
- Issue番号、URL、aliasからRunを検索できるようにする。
- staleなHEAD、workflow/graph version、canonical digest、authority、role、Finding closeを拒否する。
- publish前、HEAD前、projection前、duplicate、parent/object digest破損をfault injectionし、silent advanceしないことを示す。
- Level 1 metadata量とSkill discoveryを測る。token数は取得不能なら`unavailable`で記録する。

完了条件: 新規Run作成、部分成功、二段階review、required-only fix/rereview、重複統合、承認済みchoiceの保持、軽微指摘のnon-blocking化、time/review/attempt budget枯渇時の有限停止、Epoch/Group終了、crash recovery、clear後fresh-context resume、Issue/alias検索、stale/role/authority拒否が一連で通る。A6R migration rehearsalとG1-G8 evidenceが承認されて初めて、本再構築Runを新kernelへ引き継げる。

### Phase 3: 目的と計測の共通Group

- Epoch C-01（C1-C2）: `decompose-outcomes` → `build-outcome-dependency-graph`。outcome mapとDAGをArtifact Bundleへ閉じ、C-02へversion/digestで渡す。
- Epoch C-02（C3-C6）: `design-measurement` → `define-targets` → `capture-baseline` → `validate-outcome-system`。C-01 bundleを本文コピーせず参照する。
- C-01/C-02の境界ではclearを記録する。baseline取得不能は`unavailable`として保存し、0や推測値にしない。

完了条件: C1-C2とC3-C6が別Epochとして再開でき、数値化できる例と無理に数値化すべきでない例を両方正しく処理できる。

### Phase 4: ソフトウェアWorkflow

Dは一つの巨大な実装文脈にしない。次の三Epochへ分け、各出口をversion付きArtifact Bundleにする。

- Epoch D-01（D1-D4）: profile選択、現行system/practice調査、what/whyのcanonical spec。
- Epoch D-02（D5-D7）: option比較、solution design、contracts/schema。
- Epoch D-03（D8-D12）: task分解、実行DAG、worker brief、verification/recovery、readiness review。
- D-01→D-02→D-03では、調査→設計、canonical spec version変更、authority変更を理由にclearする。

まず次の三つを実装し、残りは実測後に追加する。

1. 機能追加
2. Bug fix
3. 機能改善

完了条件: 同じ共通Groupを参照し、コピーされたGroup本文がなく、それぞれの固有経路と証拠が異なる。

### Phase 5: Sub Agent orchestration

- 並列化判定を実装する。
- 各taskへ独立Context Epochを割り当て、taskが同じGroupに属していてもroot文脈を共有しない。
- 非重複write scopeを検証する。
- Task/Review/Evaluation packageとresult/candidate Finding/Finding validation/Verdictを一意なpathへ保存し、Controllerへ全文tool outputを戻さない。
- candidate reviewとfresh Finding validationを分離し、`required`だけにfix authorityを発行する。重複、承認済みの理由ある選択、軽微事項、scope外を直接fixへ流さない。
- successful sibling保持、該当枝だけのfix、stable Finding、fresh rereviewを実装する。rereviewの新規candidateもFinding validationへ戻す。
- 密なdomain統合は一つのconvergence Workerへ委任し、別のfresh Reviewerで検証する。Controller/Orchestratorはdomain統合やFinding closeをしない。
- review/validation conflict、同一Finding反復、time/review/attempt budget枯渇、spec defectでは通常loopを止めてArbiter packageを作る。予算を自動延長しない。Sol highはこの例外attemptだけに使う。
- 密結合taskを無理に分割しないfallbackを持つ。

完了条件: 独立調査の並列例、片枝だけが失敗する例、重複・理由付きchoice・軽微指摘をfixせず収束する例、時間またはattempt枯渇で新規workerを止める例、密な根本原因分析を一つのWorkerへ保つ例、review/validation conflictをArbiter/humanへ上げる例が期待どおりにrouteされる。

Eの各parallel DAG batchの終了でEpochを閉じ、batch bundleを次batchへ渡す。E6 `validate-review-findings`はcandidate reviewと別attempt、E9 `verify-whole-change`は別Epochのfresh contextで、実装workerとは独立したreviewerまたはdeterministic checkが行う。

### Phase 6: モデル運用とescalation

- Luna maxのdefault pathをfresh threadで検証する。
- LunaからSol highへ上げる根拠と記録形式を定義する。
- 対象Arbiter attemptだけをSolで処理し、その後Lunaへ戻ることを検証する。
- model configを変更せず助言だけを行うpathも検証する。
- 明示実行する設定スクリプトのdry-run、限定merge、backup、rollbackを検証する。trust再確認が必要になった場合は失敗扱いにせず、意図しないwhole-file置換がないことを確認する。

完了条件: 実効model/effortが観測でき、未対応値を静かに置換せず停止する。

### 共通closure protocol（旧Group Fの再構成）

FはLevel 2 Groupではない。`close-epoch`、`close-group`、`checkpoint`、`resume`、`status`を、すべてのGroupが共有するclosure protocolとして実装する。

- `close-epoch`: 同一Group内のEpochをArtifact Bundleへ閉じ、次Epochの入力をversion/path/digestで固定する。
- `close-group`: Groupの全Epochを収束させ、clearを必須化し、次Groupの開始条件を保存する。
- `checkpoint`: bundleとevent、authority、未検証事項、invalidated artifact、next inputsを一つの再開点へ束ねる。
- `resume/status`: Issue、URL、alias、run IDから物理stateを検索し、workflow version、state revision、objective version、bundle digestをCAS検証する。自然言語summaryだけでは再開しない。

これにより、Fの共通処理はLevel 2の意味を持たず、Groupごとの実装へコピーされない。

### Phase 7: 個人用pilot

- 実際の小規模Bug、機能追加、改善、非software業務で試す。
- 成功率、手戻り、context量、人間の待ち時間、resume成功率を測る。
- Skill数とGroup sizeを調整する。

完了条件: Chat履歴なしのfresh sessionがCheckpointだけで正しい次工程を開始できる。

### Phase 8: 会社用拡張

- approved catalog、owner、version pin、license、update、rollbackを追加する。
- data classification、retention、identity、sandbox、network、外部action approvalを追加する。
- team/project固有Workflowを中央の共通Groupから組み立てる。
- auditとevalを個人用stateから分離した会社用storeへ接続する。

完了条件: 中央、team、利用者の所有範囲が明確で、一つのtaskを二つのlifecycle ownerが同時に進行しない。

## 13. 検証計画

### 13.1 会話との一致

このプランを次の項目で再読する。

- 目的、小目的、定量目標が別概念になっている。
- 目的から小目的へ上位で分解し、定量目標は上位基準とtask固有基準の二段階になっている。
- Workflow、Group、Skillの三レイヤーになっている。
- この基盤自体の最初のLevel 2がBootstrap Groupで、旧frameworkなしに自己hostへ移行する。
- clearはGroup境界で行う。
- grill-purpose後に利用者が目的を確定する。
- 各Skillが成果物、完了条件、次Skillの助言を持つ。
- Group終了時に次GroupとそのSkillを整理する。
- 物理ファイルからIssue単位の現在地を取得できる。
- 疎な作業を最大限並列化し、密な部分を分割しない。
- Orchestratorは機械遷移だけを行い、domain統合はconvergence Worker、close判定はfresh Reviewerが行う。
- candidate reviewとfresh Finding validationが分離され、reviewerの指摘だけでfixを開始しない。
- 重複、承認済みの理由ある選択、軽微な改善、時間・review・attempt budget枯渇で修正loopが有限に停止する。
- Luna maxを通常、Sol highを例外とする。
- 測れないものを無理に数値化しない。
- 目的変更とcritical actionは利用者承認を必要とする。
- 目的が版付き第一級artifactとしてplanから保護される。
- Run stateがsingle writerとrevisionで競合から保護される。
- AI-DLC/Superpowersは設計資料に限定され、runtime候補に残っていない。
- local判断を承認付きで永続decisionへまとめる経路がある。
- 既存の採用済み項目を落としていない。

### 13.2 既存ベストプラクティスとの一致

| 調査から得た方向 | このプランでの対応 |
| --- | --- |
| always-onとon-demandを分離 | 短いSkill metadata、本文とreferenceの遅延読込、限定配布 |
| 一つのtaskに一つのlifecycle owner | deterministic DAG OrchestratorだけがHEADを進める |
| 上流artifactを一つの正本にする | 版付きobjective/specを参照し、不足は所有Skillへ差し戻す |
| chatとdurable stateを分離 | immutable object/transaction/HEADと再生成可能projectionを物理ファイル化 |
| subagentはcontext isolationとparallelismに使う | package境界、独立Worker/Reviewer、convergence Worker |
| final outputだけで完了判定しない | acceptance evidence、目的監査、環境状態の確認 |
| permissionとapprovalをprompt外へ置く | Nix、sandbox、policy、deterministic validationを別layer化 |
| app-owned stateをdotfilesで全面所有しない | `~/.codex/config.toml` とtrustをmutable localに保持 |
| 共通sourceからnative surfaceへ投影 | 共通GroupとSkill source、Workflow manifest、provider別projection |
| 配布物をpin、review、rollback可能にする | manifest、source commit、doctor、段階導入 |
| proposed stateとcanonical truthを分離 | local decision candidateを承認後だけADR等へ昇格 |

根拠となる調査: [AIエージェント運用の型](../research/2026-09-ai-agent-operations-research.md), [AIエージェントdotfiles調査](../research/2026-09-ai-agent-dotfiles-landscape.md)

### 13.3 実装後の受入試験

- fresh sessionで未使用Skill本文が読み込まれていない。
- 初期Skill一覧の文字数、短縮、欠落を記録できる。
- implicit invocation無効のSkillが明示起動なしに開始されない。
- Group内ではSkill間の必要な文脈が保持される。
- Group終了後にclearし、Checkpointだけで次Groupを開始できる。
- Context EpochはLevel 4として扱われず、一つのGroup内で複数Epochを継続できる。
- Epoch/Group出口のArtifact Bundleにcanonical artifacts、digest、acceptance evidence、approved decisions、unresolved items、invalidated artifacts、next inputsが揃う。
- A5→A6で`clear_before_start: true`が保存され、fresh contextがA5会話なしにA6を開始できる。
- context budgetのtarget `200K`、通常上限`300K`、絶対上限`500K`と、exact/estimated/unavailableのtoken statusが正しく保存される。
- token量に関係なく、調査→mutation、設計→実装、実装→独立review、authority/approval変更、canonical version変更でEpochが閉じる。
- 目的変更が承認なしに適用されない。
- objectiveの旧版が保持され、planから本文を上書きできない。
- staleなworkflow version、state revision、Checkpointを参照する次Skill助言が拒否される。
- event logの過去行と完了済みCheckpointを更新できない。
- planのfuture sectionだけが再計画される。
- 複数workerが同じwrite targetを所有できない。
- workerの矛盾がconvergence Workerの統合前に検出される。
- 各Sub Agent taskが独立Epoch/packageになり、Controllerへ全文tool outputではなくresult/review object、artifact path/version/digest、検証、未解決事項だけが戻る。
- Sub Agentがroot所有stateを直接更新できない。
- Controller、Worker、Reviewer、ArbiterがHEADを更新できず、DAG Orchestratorだけがvalidated commandで進められる。
- Workerがrequired blocking Findingを自己closeできず、fresh Reviewerだけが同じFinding IDをcloseできる。
- candidate Findingが独立Validatorにより`required / defer / reject / needs-user`へ分類され、`required`以外はfix authorityを得ない。
- 同一fingerprintの重複Findingが元IDへ統合され、承認済みchoiceまたは軽微なnon-blocking事項から新しいfix loopが始まらない。
- deadline/timebox、最大review round、Findingごとの最大fix attemptが有限で、枯渇後に新規workerを発行せずstop reasonを返す。
- 成功したparallel siblingを保持したまま、失敗した枝だけをfix/reviewできる。
- transaction publish前、HEAD前、projection前のcrash、duplicate command、parent/object digest破損でsilent advanceしない。
- `run.yaml`、`plan.yaml`、`status.md`、event view、CheckpointをHEADから再生成できる。
- Issue番号から正しいRunと次Skillが見つかる。
- 判断候補が承認なしにADR等へ昇格せず、承認後はsource eventと相互参照できる。
- `~/.codex/config.toml` が通常のwritable fileである。
- Home Manager activationが `config.toml` を置換しない。必要ならtrustを再確認できる。
- `programs.codex.settings` および別経路のconfig生成が存在しない。
- 明示実行する設定スクリプトが通常は未知のkeyと既存trustを保持し、managed keyだけを変更する。競合時は破壊的mergeをせず停止する。
- 設定スクリプトのdry-run、backup、atomic write、0600、rollbackが機能する。
- Luna maxとSol highの実効model/effortが確認できる。
- 代表taskで現在方式より成功率、手戻り、resumeの少なくとも一つが改善し、重大な悪化がない。
- C1-C2/C3-C6、D1-D4/D5-D7/D8-D12の分割と、EのDAG batchごとのEpoch closeが記録される。
- E9 whole-change verificationが実装workerと別のfresh context/独立reviewで実行される。

## 14. 停止条件と承認点

次の場合は自動的に進めず、利用者へ判断を求める。

- 真の目的、scope、risk、予算、ownerを変更する必要がある。
- 妥当な定量目標または計測方法を定義できない。
- 次のGroupが複数あり、選択で成果または費用が大きく変わる。
- Sol highへのescalationが必要である。
- 既存AI関連設定の削除、上書き、移行が必要である。
- A6R converter、cutover、rollbackまたはA7 self-hostを実行する必要がある。
- G1-G8のどれかが未検証または失敗で、target architectureをoperational adoptionしようとしている。
- Home Manager activation、commit、push、外部操作が必要である。
- `max` がCodex runtimeで受理されず、代替値の選択が必要である。
- app-owned stateとNix-owned sourceの所有権が衝突する。
- 並列workerの結果が統合できず、目的判断をやり直す必要がある。
- Context EpochまたはArtifact Bundleの`workflow_version`、`state_revision`、canonical digestが開始時と一致しない。
- Bundleの必須欄（canonical artifacts、acceptance evidence、approved decisions、unresolved、invalidated、next inputs）が欠けている。
- token数をexactとして記録する根拠がなく、`estimated`または`unavailable`への降格を拒む必要がある。

## 15. このプランの未決事項

実装前に次を小さなprototypeまたはschema比較で決定する。

- repository内の正式なsource directory名
- `run.yaml` と `plan.yaml` の詳細schema
- `run-index.jsonl` のcompactionと修復方法
- deterministic helperをshellとTypeScriptのどちらで実装するか。AI-DLC adapterは候補に含めない
- GroupとSkillの適切な平均サイズ
- Skillをglobal、user、repositoryのどこへ配るかの最終基準
- clearを利用者操作として案内するだけか、Codex appの機能と連携するか
- 会社用audit store、retention、data classification
- 既存の個別Skillのうち、新Workflowのutilityとして再利用、書き直し、廃止するもの。AI-DLC/Superpowersはruntime候補に戻さず、調査資料の出典としてだけ残す

## 16. 設計レビュー結果

### 16.1 今回のレビューで修正した項目

最初のプランをそのまま`pass`とせず、会話と追加調査から次を修正した。

1. AI-DLC/Superpowersを「残すか未決」から、設計資料のみ・runtime/lifecycle不採用へ変更した。
2. 最初の実施単位をread-only inventory単体から、隔離、rollback、最小Run、walking skeleton、self-host handoffを含むBootstrap Groupへ変更した。
3. `objective.md`一枚を、版付き`objectives/vNNN.md`とcurrent pointerへ変更し、目的をplanの書換えから保護した。
4. state revision、single writer、worker output限定、compare-and-swap、atomic updateを追加し、今回の比較後にsingle writerをDAG Orchestrator、canonical pointerをHEADとして具体化した。
5. 次Skill助言へworkflow version、state revision、Checkpoint、入力digest、無効条件を追加した。
6. `.local`の判断を後からADR等へまとめる`consolidate-decisions`を、承認前previewと承認後promotionに分割して追加した。
7. source作成をrepository worktree/branchで行い、所有manifest付きinstallerから配布する置換手順を追加した。
8. trust保持を過度に複雑化せず、whole-file置換を防ぎ、競合時は停止または再確認を許容する方針へ緩和した。
9. 680行規模のマスタープランへ全手順を詰め込まず、詳細を[ステップ詳細カタログ](./ai-agent-workflow-step-catalog.md)へ分離した。
10. 2026-09-02のclear境界調査に基づき、Context EpochをLevel 4ではなく実行時管理情報として追加し、Artifact Bundle兼Checkpoint、token budgetのstatus、semantic boundaryを定義した。
11. CをC1-C2/C3-C6、DをD1-D4/D5-D7/D8-D12へ分け、EのDAG batchごとのEpoch closeと現在のE9に当たるfresh independent reviewを追加した。
12. 旧Group FをLevel 2 Groupではなく、すべてのGroupが使う`close-epoch`/`close-group`/`checkpoint`/`resume`/`status` protocolへ再構成した。
13. A5→A6を最初のclear境界とし、A6の`clear_before_start`を`true`にした。Sub Agentはtaskごとに独立Epochを持つ。
14. 現行案、Thin Controller、Artifact/Task DAG、Event-sourced typed state graphの4案を3名が独立採点し、最上位DAG案をbaseに必要機構をpullした。
15. 統合案はoverall中央値67.0で初期最上位63.5を全reviewerで上回ったためtarget architectureに採用した。ただしG1-G8未検証、Final-P/C 70未満なのでoperational adoptionは止めた。
16. canonical stateをimmutable objects、DAG transaction chain、atomic HEADの一系統にし、run/plan/status/event/Checkpointを再生成可能なprojectionへ変更した。
17. user-facing Thin Controller、deterministic DAG Orchestrator、Worker、Reviewer、Arbiterを分離し、domain統合をconvergence Worker、blocking closeをfresh Reviewerへ移した。
18. A6の既存成果をmigration sourceとして保持し、A7前にconverter、fault injection、old/new reader、rollback、G1-G8を扱うA6Rを追加した。
19. reviewをcandidate Finding生成とfresh Finding validationの二段階にし、`required`だけをfixへ流す。重複、承認済みchoice、軽微事項、有限budgetを収束条件へ追加した。

### 16.2 会話との一致

Result: pass

Section 13.1の全項目を本文へ対応付けた。特に今回再確認された次を反映している。

- 次のLevel 2 Groupは `close-group` が整理し、完了履歴ではなく将来計画だけを変更する。
- 各Skillの次Skill助言をCheckpointと `status.md` に残す。
- Issue番号やaliasからRunを検索し、前回の現在地を質問できる。
- 目的は `grill-purpose` の終了時に利用者承認で確定する。
- 定量目標は上位で共有基準を置き、分割後にtask固有の合格条件へ落とす。
- 現在値と比較のGroupはsoftwareと仕事のWorkflowで共有する。
- `config.toml` はNixの所有外に置くが、明示実行するスクリプトで安全に配置またはmergeできる。
- AI-DLC/Superpowersを今後のruntimeにせず、この作業をBootstrap Groupとして手動開始しself-hostへ引き継ぐ。
- 目的、revision、次Skill助言、判断の永続化、worktree配布の追加要件が反映されている。

### 16.3 ベストプラクティスとの比較

Result: pass with deliberate deviations

Section 13.2の比較では、未対策の大きな乖離は見つからなかった。次は一般的なprovider guidanceから意図的に強めているため、個人pilotで検証する。

[ステップ詳細カタログ](./ai-agent-workflow-step-catalog.md)は8 Group、60 Skill候補を持つ。機械的な構造検査で、各候補に入力、成果物、完了条件があることを確認する。Superpowers、AI-DLC、Spec Kit、OpenSpec、BMAD、dotfilesの要素crosswalkと、記載した外部一次資料・local文書へのlink検査はpassした。

| 意図的な差 | 理由 | 必須の検証 |
| --- | --- | --- |
| ほぼ全taskでLuna max | 利用者が推論量を優先し、安価なLunaを通常運用に選んだ | Lunaのeffort別品質、時間、token、失敗率を代表taskで比較する |
| Groupごとのclearと明示再開 | contextを小さくし、利用者が現在地を確認できるようにする | human waitと操作負担、resume成功率を測る |
| 疎な作業の積極的なSub Agent化 | context isolationと分割統治を優先する | 調整turn、矛盾、統合漏れが単一Agentより悪化しないか測る |
| stepごとの細かなSkill | 成果物と完了条件を明確にする | 初期metadata量、Skill選択ミス、人間の認知負荷を測る |

これらは標準から外れるため却下すべき項目ではなく、効果がtask分布に依存しており、無条件の優位性が確認されていない項目である。pilotで悪化が観測された場合は、利用者の中心目的を保ったままGroup size、Skill公開範囲、parallelismを調整する。

### 16.4 現在のrepositoryとruntime

Result: partially verified

- `~/.codex/config.toml` は現在、symlinkではない通常ファイルで、ownerは現在利用者、permissionは0600である。
- 現在のmutable configは `gpt-5.6-sol` / `xhigh` であり、今回決めたLuna max方針への切替はまだ行っていない。
- `flake.nix` の既存staged changeは `programs.codex.settings` を削除している。本プランはそのdiffへ触れていない。
- repositoryのcurrent working treeでは、`programs.codex.settings` blockは検出されなかった。
- 元worktreeの`.local/agent/workflow-selection.json`はAI-DLCを選択したまま残している。rebuild worktreeには旧`.local`を持ち込んでいないため実行時選択は存在しない。Bootstrap A5では元環境と将来の配布先をinventoryし、差分、rollback、承認を経て切り離す。
- Home Manager activation後も`config.toml`がNix所有へ戻らないことは、この作成時検証では実行していない。
- Codex runtimeがuser configの `max` を受理することは、この作成時検証では実行していない。

### 16.5 残るrisk

- Skill数が増え、初期metadata budgetまたは人間の選択負担を圧迫する可能性がある。
- Groupを小さくしすぎると、clearと引継ぎの費用が推論上の利益を上回る可能性がある。
- convergence Workerまたはfresh Reviewerのpackage設計が弱い場合、並列workerの局所最適を見落とす可能性がある。
- Finding Validatorのpackageまたはmateriality基準が弱い場合、必要な修正をrejectする、または軽微な指摘をrequiredにして時間を浪費する可能性がある。
- 定量目標が形式だけになり、真の目的を代理しない数字へ最適化する可能性がある。
- object/transaction/HEADとYAML、JSONL、Markdown projectionのcompilerに不具合があると誤表示する可能性があるため、rebuildとfault injectionが必要である。
- file-backed transaction/HEADのfsync、lock、atomic rename、digest、quarantineは設計済みだが未実装である。
- command role、fresh Reviewer、secret exclusion、company policyはschemaだけでは強制できないため、実process evidenceが必要である。
- 設定スクリプトのmerge parserが未知のTOML構造を壊さないことを実装時にtestする必要がある。

## 17. 次の実行候補

次の実装作業はA6R `bootstrap-migrate-control-kernel`である。A6の既存成果とrevision 11をmigration sourceとしてfreezeし、現行Run/Artifact Bundle/worker reportからimmutable objects、initial DAG、transaction、HEADへのmappingを作る。

A6Rのminimal sliceは`entry → two Task packages → partial success → independent candidate review → fresh Finding validation → required-only fix claim → fresh rereview → finite stop/close Epoch/Group → crash-safe projection rebuild → fresh resume/status`である。重複、承認済みchoice、軽微な改善、review/time/attempt budget枯渇をfixtureへ含め、converter、old/new reader、cutover、rollback、G1-G8を検証する。証拠と利用者のmigration approvalが揃うまでA7 self-hostへ進まない。

元worktreeや将来の配布先に残るAI-DLC/Superpowersの選択状態を実際に削除・無効化する変更、Home Manager/nix-darwin activation、Git stage/commit/pushは別承認のままである。rebuild worktreeには旧`.local`を持ち込まない。Context Epochやcompactionのsummaryは正本にせず、物理state、version、revision、digest、authority、未検証事項から再開する。

モデルは `gpt-5.6-luna` / `max` を推奨する。実際に解けない停滞が確認され、通常loopを止めたArbiter attemptだけ `gpt-5.6-sol` / `high` を助言し、設定自体は変更しない。

## 18. 2026-09-02 trajectory correction record

一次資料 [Clear境界に関するWorkflow実例調査](../research/clear-boundary-workflow-samples-2026-09-02.md)との比較により、次をこの版の拘束条件として追加した。

1. Workflow / Group / Skillの三レイヤーを維持し、Context EpochはLevel 4にせず実行時管理情報とする。一つのGroupは複数Epochにまたがる。
2. Group間はversion付きArtifact Bundleを`path`/`version`/`digest`で参照し、上流本文をコピーしない。不足は所有する上流へ戻す。
3. Epoch/Group出口はcanonical artifacts、digest、acceptance evidence、approved decisions、unresolved items、invalidated artifacts、next inputsを持つbundle兼Checkpointとする。
4. context budgetはtarget `200K`、通常上限`300K`、絶対上限`500K`。正確なtoken値が取れない場合は`exact`/`estimated`/`unavailable`を記録し、数値を捏造しない。
5. token量に関係なくsemantic boundaryでEpochを閉じる。A5→A6を最初の境界とし、A6 `clear_before_start`をtrueにする。
6. C/Dを指定された三つずつのEpoch sliceへ分け、EはDAG batchごとに閉じ、E4/E5のcandidate review、E6のfresh Finding validation、E9のfresh context/独立whole-change reviewを分離する。
7. FはLevel 2 Groupではなく共通closure protocolへ再構成し、Sub Agentはtaskごとに独立Epochを持つ。
8. compactionと自然言語summaryは正本にせず、physical state、version、revision、digest、authority、未検証事項からresumeする。
9. 4案の比較後、Artifact DAG Control Kernelをtarget architectureに選び、canonical stateをimmutable objects/transactions/HEADへ一本化した。
10. Controller/Orchestratorからdomain統合とreview closeを外し、convergence Worker、fresh Reviewer、例外Arbiter/human gateへ分離した。
11. A6とA7の間にA6Rを追加し、G1-G8とmigration rehearsalをoperational adoptionの必須条件にした。

既存の承認済み判断、完了済み履歴、目的candidate `v001`は変更していない。このrecordは正本の追補であり、既存の履歴を再解釈するものではない。

## 19. 2026-09-03 Finding validationと有限収束の決定

利用者承認により、reviewを二段階にする。第一段階のReviewerはcandidate Findingを発行し、第二段階のfresh Finding Validatorが正本、承認済みdecision、過去Finding、対象diff、証拠を照合して`required / defer / reject / needs-user`を決める。`required`だけがfix authorityを得る。

この分離はreview品質を再帰的にreviewし続けるためではなく、修正loopへのadmission controlである。同一fingerprintの重複、理由付きで承認済みの選択、根拠のない好み、現在の受入条件へ影響しない軽微な改善をfix loopへ入れない。全taskは有限のdeadlineまたはtimebox、最大review round、Findingごとの最大fix attemptを持つ。枯渇時はcanonical stateをdurable non-dispatch terminalへ進め、workerを発行せず、成功済み成果、未解決Finding、stop reasonを利用者へ返す。新しい明示budget approvalなしにresumeから再開しない。

## 20. 2026-09-04 completion claim境界

A6、A6R、A7の完了はBootstrap control-kernelというrelease scopeの完了であり、この全面再構築の完了ではない。Skill、Epoch、Group、release scope、Workflow全体の完了を別のclaimとして扱い、下位claimから上位claimを推論しない。

source実装の現在値は `agent-workflows/manifests/implementation-status.json` を正本planとstep catalogのdigestへ束縛し、共通のcompletion classifierで評価する。classifierはcatalog内の60 named Skill contractと23 software profile step、必須surface、完了gate、証拠refを検証する。`full_workflow_ready` は入力値として信用せず、すべてを満たした時だけ導出する。manifest欠落、digest不一致、coverage不足、evidenceなしのpassed gate、または物理Skill配布との差異ではfail closedとする。

`release_scope_status: implemented` と `overall_status: partial` は同時に成立する。現時点のrelease scopeは `bootstrap-control-kernel`、source coverageはA6/A6R/A7の3 contract、全体状態は `partial`、`full_workflow_ready` は `false` である。B〜H、共通closure、software profile、pilot、目的監査、operational adoptionの残作業を完了扱いにしない。

「Rebuild直前」は単独でcompletion claimに使わない。Nix/Home Managerの切替境界を指す場合は `generation rebuild / generation switch直前`、全面計画を指す場合は `Workflow rebuild` とscopeを明記する。既存Checkpointは履歴証拠として書き換えず、以後のstatus、resume、release check、説明資料が共通classifierの判定を表示する。

## 21. 2026-09-05 Workflow Execution V2 canonical policy

このsectionは、proposal `workflow-execution-v2-proposal-001` のdigest
`sha256:398fe058cd540f74cee38017f2de7c3bb13fd6bc641fd73df245e71569c86d96` と、
独立二軸reviewをjoinしたfresh Validatorの`pass-to-apply`を受けて採用したcanonical
execution policyである。既存sectionの意味を消さず、実行loop、review、budget、receipt、
distribution、cutoverについて矛盾する旧mechanicsだけをV2で置き換える。Workflow / Group /
Skillの三レイヤー、Context Epoch、60 named Skill contract、23 software profile step、完了済み
履歴、承認境界は維持する。

### 21.1 三重loop

三つのloopはすべて `Plan -> Implement -> Review -> Finding validation -> Feedback` の五段階を
持つが、scopeを混同しない。

1. **Inner Artifact loop**: 一つの独立検証可能なArtifact packageを計画し、一つのWorkerが
   RED、最小product fix、focused GREEN、purity checkを行ってcandidate digestをfreezeする。
   `RegressionFrontier`はそのfrozen candidateの宣言済みregression setを重複のないdisjoint
   shardへ分割する。各shardは完全なexecution-package closure digest、resource/conflict claim、
   isolated cwd、temporary/output namespaceへbindされ、ready frontierはpairwise-compatibleなもの
   だけをdispatchする。unknown unsafe claimは直列化または拒否し、固定worker数は設けない。
   `ReceiptAggregator`はcandidate、closure、membership、disjointness、coverage、exit、receipt
   completenessを検証して結合するだけで、testを再実行しない。続いて同じfrozen candidateと
   aggregate receiptを、(a) spec/architecture/safety、(b) standards/integration/operability の
   **二つのfreshなordinary DAG Task**が別actor・別Context Epoch・別packageとして独立reviewする。
   paired-review wrapper Moduleまたは新Skillは作らない。通常joinは両terminal reportのexact input
   closure一致を要求し、その後一つのfresh `FindingValidator`がadvisory dispositionだけを出す。
2. **Middle Section loop**: Section Artifact DAGとready frontierを計画し、非重複Artifact loopを
   並列実行する。overlapまたはdense convergenceは一ownerへまとめる。Convergence Workerは
   accepted Artifact refだけを統合してfrozen Section candidateとfocused integration receiptを
   作る。二つのfresh ordinary Section review Taskと一つのadvisory Validatorを通し、変更または
   invalidated seamだけを修正・再reviewする。
3. **Outer Workflow loop**: objective、scope、Sections、dependency、authority、external gate、
   completion coverageを計画し、accepted Section bundleをfrozen Workflow candidateへ統合する。
   二つのfresh whole-Workflow review Taskと一つのadvisory Validatorを通す。canonical completion
   classifierが60/60 named contract、23/23 profile step、objective audit、outcome gateを証明した時
   だけWorkflowを完了できる。source完了からrebuild、activation、migration、Git、live完了を
   推論しない。

Finding Validatorはcandidateを `required`、`duplicate`、`invalid`、`deliberate-design`、
`downstream-only`、`too-minor`、`test-evidence-debt`、`needs-user` に分類し、materiality、提案scope、
観測budgetを報告するadvisory actorである。fix authorityを発行せずcanonical counterを進めない。
review Taskも実装しない。DAG OrchestratorだけがValidator reportのrole、authority、lease、expected
HEAD、残budgetを検証し、immutable historyからround/attemptを導出してbounded fixを発行または
拒否する。WorkerはFindingをcloseせず、Orchestratorはdomain correctnessを黙示裁定しない。

### 21.2 immutable execution closureとone-way authority

一つのcanonical execution-package closure digestは、candidate/test bytes、fixtures、schema、config、
lock、executable/toolchain identity、command/arguments、cwd、許可environment、isolation/resource claim、
supervision policy、明示的external-input snapshotをすべて含む。behaviorへ影響するidentityが欠ける、
または一要素でも変わる場合は新packageとし、旧receiptをreuseしない。一つのlogical commandは
closure digest由来のidempotency keyを持ち、同じcomplete payloadは既存receiptを返し、同じkeyの
異なるpayloadはconflictとする。

authority edgeはstable owned inputからconsumerへの一方向だけである。fixed upstream authorityは
mutable downstream outputの現在bytesをbindまたは再読込しない。とくにS1-C compatibility authority
から後続lifecycle consumerへは一方向に依存し、後続test byteをupstream close setへ含めない。
candidate、aggregate receipt、二つのreview report、Validator disposition、repair claim、close setは
別々のimmutable nodeであり、後段が前段を書き換えない。fix/reviewはmutable cycleではなくattemptを
appendする。canonical transaction、counter、dispatch grant、HEAD更新はparent DAG Orchestratorだけが
行い、Worker/Reviewer/Validatorはcontent-addressed objectと割当済みreport/receiptだけを提出する。

### 21.3 persistent receiptとdisconnect recovery

`PersistentReceiptRunner`はchildをrunnableにする前に、package ID、closure digest、shard ID、command
identityへbindしたatomic `started` receiptとstable supervisor fenceをpublishする。fenceはnonce、boot
identity、supervisor lease、process-birth identityを持つ。command packageはtimeout、heartbeat、expiry、
grace、ordered signal、process-group containment/kill policy、terminal-publication allowanceを固定する。

stdout/stderrはbinary bytesのままowner-only atomic storageへbyte limit付きで保存し、streamごとに
`complete | truncated | corrupt`、byte count、digest、sensitivity、safe referenceをterminal記録する。
secret-bearing raw bytesをreport、canonical state、portable source、generated projectionへ入れない。
timeout、death、heartbeat lossではfenced process groupをgrace後にkillし、escaped child/PID reuseを
確認して `interrupted | timed_out` をexactly onceでpublishする。

disconnect後は、terminal receiptならreuseし、live `started`ならwait/recoverしてduplicate launchを
しない。launch absenceとdead process groupを証明できるorphanだけを一度terminalizeし、full supervision
allowanceを収める新しいversioned replacementを検討できる。receipt、child absence、process-group death、
output integrityがmissing/corrupt/mismatched/ambiguousならfail closedとする。before-spawn、
after-spawn/pre-identity、capture、terminal publicationのfault injectionでlive executionが高々一つで
あることを証明する。

### 21.4 finite stopとmechanical finalization

dispatch前にtimeout、grace、terminal publication allowanceまで残budgetへ収まることを証明する。
時間不足、overrun、review-round exhaustion、Finding別attempt exhaustionはcanonical
`stopped_budget`というimmutable non-dispatch terminalへ進め、leaseと全unaccepted dispatch authorityを
閉じる。required、needs-user、incomplete、unknownをpassへ変換しない。再開にはprior terminal digest、
expected HEAD、新lease、新budgetへbindしたversioned authorizationが必要である。

`product_fix_attempt`、`test_fixture_correction`、`command_or_capture_retry`、
`package_or_report_correction`、`review_round`は別counterにする。最大5回を消費するのは、Orchestratorが
validated `required` dispositionからproduct-fix transitionを発行した時だけである。review roundは
accepted candidate bytesまたはauthoritative evidenceがvalidated feedback後に変わった時だけ増える。

`EvidenceFinalizer`はaccepted immutable refをcanonical orderで機械的に組み立てるだけである。全declared
branchに既知・complete・accepted terminalが一つあり、join/refがintegralで、`required`、`needs-user`、
incomplete、unknownがない場合だけclose-set candidateを出す。product code変更、Finding発見、test実行、
budget stop再解釈、reopen authorityを持たない。`stopped_budget`をfinalizeできない。

### 21.5 timingと診断SLO

すべてのpackage/reportはUTC RFC3339 start/endとmonotonic durationを持ち、compatible-admission queue、
execution、preflight、reasoning/design、edit、focused test、regression command、tool/output wait、review、
report write、helper validation、parent assimilationを分離する。各resultは`new-run | reused-receipt`、exact
closure digest、loop level、outcome、retry class、bounded capture state/bytes、sensitivity classを持つ。

初期診断SLOは package creation+dispatch 2分、candidate Worker 15分、regression shard 5分、parallel
review wave 10分、Validator 5分、parent aggregation/finalization/checkpoint 3分、no-Finding inner loop 35分
以内とする。これらは測定targetであってacceptance shortcutではない。aggregate fan-in count、bytes、parent
assimilation timeを先に計測し、incremental/Merkle aggregateは測定根拠と別authorityが得られるまで
`test-evidence-debt`のままにする。

### 21.6 dotfilesとdistribution ownership

portable source、generated native projection、managed destination、unmanaged user state、app-owned runtime
state、secret-bearing state、ephemeral cacheを別inventory classにする。一つのportable sourceからprovider
native projectionを生成し、generated projectionは手編集しない。unsupportedなpermission、hook、subagent
semanticsをsyntax変換で保存したと主張しない。`~/.codex`、`~/.claude`等のrootはreal app-owned directory
として保持し、明示したchild pathまたはowned keyだけを管理する。

apply前にdestination、conflict、replacement、permission、exclusionをpreviewし、unmanaged conflictは拒否
する。app-written trust、session、UI state、cache、marketplace data、unowned MCP entryを保持する。ownership
manifest、source provenance/version/digest、native compatibility、doctor、backup、uninstall、rollbackを配布
evidenceとし、削除はowned file/keyだけに限定する。absolute user path、credential、trusted-project list、
employer policy、personal shell/editor/UI preferenceをportable coreへ入れない。source edit、packaging/check、
Nix build、rebuild/switch、migration、activation、commit、push、live validationは別authority gateである。

### 21.7 issuance-watermark cutover

すでに発行済みのS1-Cは旧immutable lineageのまま完了させ、V2へ再package、割込み、履歴書換えをしない。
**最初の未発行frontierはV2を使う。** contract selectionを有効化する時、parent DAG Orchestratorは一つの
expected-HEAD CAS transactionで旧/V2 contract digest、単調増加issuance watermark、全outstanding
old-contract package IDをfreezeする。watermark以下のroot sequenceと、そのreplacement/descendantは旧contractを
継承し、それより後のnew rootだけがV2を使う。old outstanding setからの除去は旧contractでのterminal
collection後だけ許す。issuanceとcontract選択はatomicで、mixed-version joinは明示的なshape/lineage
compatibilityを証明できなければ拒否する。Adapterはversionを記録できるがold outcomeを再解釈しない。

rollbackもexpected-HEAD transactionでfuture new rootのfrozen contractを選び直すだけで、発行済みroot、
replacement、immutable reportを変更しない。この文書変更はcanonical source policyの採用であり、現在Runの
HEAD/CAS、Nix、migration、activation、Git、external stateを変更しない。

### 21.8 現在のcompletion claim

V2はexecution policyの正本化であり、実装coverageの追加ではない。60 named Skill contractと23 software
profile stepのinventory、既存の物理Skill配布claim、`bootstrap-control-kernel`のA6/A6R/A7だけという
implemented coverageを保持する。`overall_status`は`partial`、`full_workflow_ready`は`false`のままである。
general live distribution transactionは`downstream-only`として、incremental/Merkle aggregationは測定前の
`test-evidence-debt`として、このapplyから除外する。
