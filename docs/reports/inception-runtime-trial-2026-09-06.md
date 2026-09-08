# Inception 実Run接続の修正・隔離試験（2026-09-06）

この報告は同日の `inception-helper-trial-2026-09-06.md` に続くもの。
前回の draft replay と独立実装を、通常Workflowの完走証拠として扱わない。

## 結果

隔離した今回の業務ケースは、実Run登録から **17分26秒** で Group E を閉じた。
2026-09-06 **16:32:23〜16:49:48 JST**（UTC 07:32:23〜07:49:48）。
設定した20分枠内。開始後の期限resetなし。次はHへのhandoffであり、
H1〜H3の最終判定や本番Workflow全体の完走を主張する結果ではない。

- B1〜B7、C1〜C6、D1〜D12の実compiler記録とB/C/Dの実closureを保存。
- 計画の依存順で3つのfresh Workerをdispatch・claim・実装・結果受入。
- 外部受入 **24件**、プロジェクト内unit test **6件**、READMEの実コマンド成功。
- 同じ凍結candidateをfreshな2軸でreviewし、別の単一Validatorで評価。
  `QUALITY-001` は軽微としてdefer、required 0件。修正ループを追加しなかった。
- 実Kernelのreview/evaluationとF1〜F7を通し、revision **78** でE closed。
  cold resumeは `paused_after_group`、次GroupはH。3 Taskはすべて `succeeded`。

| 区間 | 実測 |
| --- | --- |
| core Worker | 16:36:09〜16:38:25 JST、136秒、自己検証12項目 |
| CLI Worker | 16:40:15〜16:41:24 JST、69秒 |
| tests/README Worker | 16:42:47〜16:44:17 JST、90秒 |
| Cの各compiler・記録・次状態確認 | 約0.37〜0.51秒 |
| Dの各compiler・記録・次状態確認 | 約0.93〜1.54秒 |
| B/C/Dのclosure | 約1.0／2.6／6.6秒 |

17分26秒には親の確認・dispatch待ち・review・終了処理も含む。
元の固定計画入力の準備、Workflow source修正の全時間、25 Skillすべてのfresh AI判断は
この実測に含めない。今回の小規模ケースから一般業務の所要時間や品質を保証しない。

証拠はサンプルの `.local/agent/support-report-trial/` に保存した
`verification.json`、`architecture-review.json`、`quality-review.json`、
`validation.json` と、`.local/agent/runs/support-report/control-kernel/`。
完了bundleは `sha256:26adbeaa2c33e7f375f661fbe487479cff0af397420990f0c8ca576e8630aa52`。
READMEとサンプルsourceも同じprojectに保持している。

既知の軽微な問題は、受理される並べ替えヘッダーに不正引用符があると、エラーの列名を
固定順で誤表示すること。exit 2・stdout空・不正入力拒否は維持される。
Validatorは目的への影響と有限時間方針から後回しとし、Findingを消さずに保存した。

## 今回の修正

- `runtime_approval.py`: B1〜B6の物理入力を必須とし、実compilerで検証・記録。
  B7は明示receiptから実際のKernel承認transactionを作り、version/history/eventを保持。
  real/human と rehearsal/mock は相互転用しない。期限の再設定、別入力による再採用、
  project外へ出るパスは拒否する。
- `inception_runtime.py`: C/Dを1 Skill分ずつ実compileし、現在のobjective・前段出力・
  物理bytes・HEADへ結合。D6は実際に選択された現D5のoptionを確認。
  `status`と`advance`は新processから再開できる。AI起動やhostのclearを代行しない。
- `runtime_closure.py`: 明示的なGroup auditと実artifact一覧からF1〜F7を実行し、
  Kernelのbundle/checkpointを作る。digestの一致だけで目的適合を推定しない。
  F6成功後の中断は実受理済みtransactionを確認してF7のみ再開し、F7後の再試行は
  同じreportを再取得する。別の証拠・判断に差し替えて再試行することはできない。
- KernelのD→E移行: 閉じたDのD8/D10/D12から、計画されたTask・files・report pathだけに
  着手権限を渡す。無関係なTask、範囲外write、D以前の着手は拒否する。
- Skill案内は `skill-creator` に従い、draft保存・実承認・次Skillの手動呼出しを区別した。

## 試験の区別

`runtime_trial.py` / `runtime_plan.py` は固定業務ケースのcompiler/adapter試験ドライバー。
25回の実compiler操作ではあるが、25回のfresh AI Skill判断を測ったものではない。
質問・目的選択・設計optionの人間応答のみmockを使用する。
実装Workerの作業、テスト実行、レビュー、Kernelのtransactionは実処理として別途記録する。

最初の試験 `/private/tmp/inception-runtime-trial.im2Dnx/project` はB/C/Dを終了したが、
Eの着手時にRun権限がentryのままという接続不備で停止した。成功扱いにせず保持。
privateなstate編集や期限resetはしていない。修正後の独立試験は
`/private/tmp/inception-runtime-final.zPcE8e/project`。

固定ケースは、標準ライブラリだけで問い合わせCSVを全行検証し、固定時刻で期限超過を
抽出・優先順に表示するCLI。REQUEST・外部受入24検査を実装者とは別に保持する。

## Source検証結果

凍結した実装に対する全体unittestは **570件、264.845秒、全成功**。
コマンドは `PYTHONPATH=agent-workflows/src python3 -B -m unittest discover -s agent-workflows/tests -v`。
ログは `/tmp/inception-runtime-final-regression.log`。
Nixは `nix-instantiate --parse flake.nix` の構文確認のみ成功。

独立reviewで見つかった「B1〜B6省略時の行き止まり」「参照パスの境界逸脱」
「F6成功後の中断」の3件を修正し、別のValidatorが14件の関連テストとsourceで
`required-fixed` と判定した。重複ではなく、3つの異なる原因だった。

歴史的S0/S1/S2受入証拠のbytesは変更せず、現在sourceとの差分は
`agent-workflows/evidence/compatibility/runtime-source-extension.json` で別途固定した。
current bindingの更新記録は `/tmp/refresh-runtime-digest-trace.md`。

## 残る制約

- Kernelの保守的な機密文字列判定は、通常の安全制約文や正規enum
  `access-denied`まで拒否することがある。試験では安全制約の原文を物理参照として
  保持した。判定自体を弱めたり、実データの秘密を通したわけではない。
- helperは外部AIプロセスの強制停止監督ではない。期限は次の操作を拒否し、
  Workerには別途明示予算を渡す。
- F1〜F5途中の中断に対する自動復旧は未対応。証拠と状態を保持してfail-closedで停止する。
- hostのclear操作、全SkillのAI判断品質、実業務での効果、Hの最終人間判定は別検証。
- Nix build/rebuild/switch、migration、activation、live設定変更、Git操作は実施しない。
  sourceが動作してもインストール済み環境への反映済みとは言わない。
