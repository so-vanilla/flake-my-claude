# InceptionのSkill単位運用

決定日: 2026-09-06。利用者の追加指示「Inceptionフェーズにあたる部分は1コずつskillを呼んでclearする」に基づく現行運用。

この決定は、元の再構築計画の「Skillのたびにはclearしない」「同一Group内は連続実行」という規定を、Group B/C/Dについて上書きする。既存の受入時点の計画・証拠は履歴として保持する。

対象は、Bの目的探索・承認、Cの成果分解・測定、Dの仕様・設計・タスク分解・実装準備確認。25個のnamed Skillをそれぞれ明示起動し、成果物と短いhandoffを保存したら終了する。利用者がclearまたは履歴を引き継がない新規会話を行い、提示された次のSkill呼び出し文を貼り付ける。

各Skillは次のSkillを呼ばず、その代行workerも発行しない。同一Skill内の質問への回答、調査、検証はそのSkillの成果物を作るために継続できる。profile stepは既存Workflowで定められたhost Skill内の作業として扱う。Skill数を減らす変更や、clearごとの再承認・予算resetではない。

handoffには依頼原文、目的・承認、制約、成果物、検証結果、未解決事項、次Skillと開始条件を残す。入力は物理path/version/digestで固定し、fresh contextは保存ファイルから復元する。保存不能時はclearを案内しない。承認待ち・blockedは完了とせず同じSkillへ、要修正は所有する上流Skillへ戻す。

Groupの成果まとめ・closureは本来のGroup終了時だけ行う。途中のSkill終了はGroup完了を意味しない。D12とD closureが通過した後、clearし、利用者が`execution-preflight`を起動すると既存のGroup E orchestrationへ移る。A、E以降、二段階review、requiredだけの修正と有限収束の規則は従来どおり。

配布される詳細手順の正本は[Inception single-Skill protocol](../../agent-workflows/skills/entry/references/inception-single-skill.md)。B/C/Dの全Skillから参照する。hostのclearは呼出し側が行う。既存read-only CLIは維持し、下記の明示的なruntime操作を別経路として提供する。

Project-local操作では、installerが配置したmanifestの`wrapper`を
project rootに対して解決し、次のように物理absolute pathを固定する。
以降の操作はすべてこの値を使う。`PATH`上の同名command、相対path、
別snapshotのwrapperは代用できない。

```sh
PROJECT_ROOT="$(cd -- "$(git rev-parse --show-toplevel)" && pwd -P)"
INCEPTION_WRAPPER="$PROJECT_ROOT/.agent-workflow/bin/agent-workflow-inception"
```

## 2026-09-06 新規入口の修正・試験用補助

サンプル試験で、新規依頼に存在しないRun HEADを入口が要求する問題と、
保存・digest確認をAIが毎回組み立てる負担を確認した。追加指示に基づき、
`"$INCEPTION_WRAPPER" init/save/resume` を別CLIとして用意する。
これはpre-Runの依頼と候補の保存・再開だけを所有し、既存のread-only
`agent-workflow`、Kernel、current Run/objective pointerは変更しない。

`recorded` は候補の保存成功であり、既存B/C/D compilerの合格やGroup closure
ではない。通常経路はB7で実承認・closure接続待ちとして止める。
利用者が許可した隔離試験だけは `rehearsal` とし、Grilling・承認の回答を
`source: mock` で保存して25 Skill分の候補引継ぎを試せる。これは正常運用での
Group境界の省略許可ではない。D12からGroup Eを起動せず、サンプル実装は
利用者の隔離試験の権限を別途根拠にする。モック経路の結果を本番承認へ転用しない。

既存B compilerについては、Kernelと同じrevision=0/digest=nullの組合せを
認め、revision>0/nullやrevision=0/非nullは拒否する。Grillingは一度に一問を
維持しつつ、物理根拠と解消理由がある場合は質問を捏造せず `inquiry-resolved`
で候補を返す。旧source受入証拠は実務完走の証拠と区別する。

## Project-local runtimeへの接続

`"$INCEPTION_WRAPPER" runtime adopt/status/step/close/advance` は、保存した
入力を既存B/C/D compilerとControlKernelへ接続する。`init/save/resume`の
候補保存とは別で、`adopt`は物理承認receiptを検証しB7をcompileして実際の
objective approval transactionを適用する。AIモデルの起動やclearは行わない。

`adopt --inputs`のJSONには`intake_ref`、`candidate_ref`、`proposal_ref`、
`actor_ref`、`receipt_ref`、`mode`、`budget_seconds`を渡す。refは実際の
project内ファイルの絶対path/version/digest。`approval_context`は承認対象の
結び付けだけを返し、利用者の決定を生成しない。実際に供給されたreceiptへ
決定・時刻・receipt IDを保存する。モック応答は明示されたrehearsalに限る。

必須の`preapproval_steps`はB1〜B6の順序付き`[qualified_id, inputs]`配列。
各Skillで保存済みの入力を再compileし実Runへ記録するためのもので、複数Skillを
同じ会話で自動起動する許可ではない。省略・不足・順序違いはRun登録前に拒否する。

新しいプロセスで`"$INCEPTION_WRAPPER" runtime status`を読み、現在の`next_id`だけを
`"$INCEPTION_WRAPPER" runtime step --qualified-id … --inputs … --actor …`で処理する。`--actor`は目的承認と
同じactorの物理ファイル。C6は現行C1〜C5、Dは直前の成果物、D12はD1〜D11を
参照する。D6の選択承認receiptは現行D5、目的、actorへ結び付ける。

Group終端では、実際に行った整合性確認のauditを保存し、
`"$INCEPTION_WRAPPER" runtime close --inputs "$PROJECT_ROOT/group-audit.json"`を呼ぶ。auditはGroup ID、
目的digest、順序付きの全Skill成果物ref、alignment、reviewer、rationaleを持つ。
F1〜F7が既存protocolで実行され、KernelのEpoch/Groupを閉じる。
途中のSkillごとには実行しない。closureは複数transactionから成り、中断時は
部分的に進んでいる可能性がある。HEAD・receiptを確認し、F6受理後なら同一auditと
証拠の`"$INCEPTION_WRAPPER" runtime close`再試行でF7だけを再開する。F7受理後は同じreportを返す。
F1〜F5途中の中断は引き続きfail-closedであり、再試行の成功を仮定しない。

closure完了後に停止・clearし、新しい会話でstatusと物理成果物を確認して
`"$INCEPTION_WRAPPER" runtime advance`を明示実行する。B→C→D→Eの境界はこの操作で開く。予算と承認は
保存済みRunから復元し、clearで初期化しない。Eのタスク実行は既存Kernel APIを
使用する。自動モデルrunner、全工程の本番benchmark、実運用での効果は未証明であり、
サンプルの実行結果も実際の記録を確認するまでは成功としない。
