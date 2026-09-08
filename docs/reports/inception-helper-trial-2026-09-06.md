# Inception補助・隔離サンプル再試験（2026-09-06）

## 判定と範囲

新規依頼の保存、1 Skill分の候補handoff、fresh再開の入口は修正・確認した。
モック回答を使った25段階の候補引継ぎと、その物理briefから独立workerが作る
サンプルCLIも確認した。これは、25 SkillのAI判断や既存KernelのB/C/D/E受入を
完走したという結果ではない。通常B7の実承認・Group closure・Run採用への接続は
なお未完であり、業務Workflow全体の本番投入合格とはしない。

## source修正

- `inception_cli.py`: `init/save/resume`。Gitでignoreされたpre-Run領域だけに
  依頼を保存し、handoffは非上書き・single writer。入力と回答/承認receiptの
  digest、前のhandoff、順序、mode、残時間を確認する。
- `recorded`とcompiler/Group受入を分離。通常B7は接続待ちで止まり、rehearsalの
  D12もGroup Eを起動しない。mockは全段階に継承しreal再開で拒否する。
- B compilerのHEAD schemaをKernelに合わせ、revision=0/nullのみを正規genesisと
  する。正のrevision/null、revision=0/非nullを拒否する。
- Grillingは一度に一問を維持。根拠と理由付きでcritical unknown解消済みなら
  `B4.inquiry-resolved`を返し、質問を捏造しない。
- Skillが記載していたB1〜B7 API名に実際のselector呼出しへのaliasを用意した。
- Nix sourceに`agent-workflow-inception` wrapperを追加。live配布はしていない。
- 既存のsource receipt・projectionは既存compilerで再生成し、current digestの
  参照を更新した。過去の受入を新しい業務完走証拠に書き換えていない。

## 試験

| 試験 | 実測 | 結果と限界 |
| --- | --- | --- |
| fresh agentによるentry | 06:48:50〜06:49:29 UTC、39秒 | 新規「未発送注文の期限一覧」依頼をtyped intakeへ保存。次Skillは起動せず停止 |
| 25段階の候補handoff | CLI呼出し計2.514秒 | init + 各save/resumeを別processで実行。固定サンプルの機械的replayでありAI思考時間ではない |
| サンプルCLIのfresh実装 | 06:48:27〜06:52:28 UTC、241秒 | 報告終了記録06:52:58 UTCまで271秒（最終追記は直後）。8分枠内、自己テスト8件成功 |
| 独立受入 | 24件、0.863秒 | 全件合格。実装を見る前に用意したblack-box tests |
| helperのfocused tests | 22件、2.429秒 | 改変、receipt脱落、stale frontier、budget保持、mock越境、保存失敗、B API aliasとgenesis型を含む |
| B compilerのfocused tests | 13件、0.003秒 | genesisとresolved Grillingを含む |
| fresh独立品質review | 06:53:52〜06:55:37 UTC、105秒 | 独自336 checks PASS、確定required 0件。既存の実装者tests・親の受入結果を読まず確認 |

サンプルは`/private/tmp/inception-fixed-trial.Kel9Bj/project`に保持。
初期化06:47:55.145690 UTCから独立品質review終了06:55:37 UTCまで約7分42秒。
固定draftの準備、Workflow本体の修正、source全体回帰の時間はこの値に含まない。
新規entryの別試験は`/private/tmp/inception-entry-forward-WH1b3Bsv`。
元の依頼・drafts・handoffを実装者は変更していない。clearはfresh agentまたは
別processで近似しており、hostのclear操作を自動実行・検証したものではない。

凍結したサンプル実装のSHA-256:

- `sla_report.py`: `4963b953cf3d6d5d90cdbcf79554ac76edb797fb2613051e9483591945f27f6a`
- `README.md`: `1c4f27f95e25e828dc1f37a88dc76b9bd088d66553d5587897037cc3430cba7a`
- `tests/test_sla_report.py`: `24dfe2566fde45517215cc29256d3bcd3ef0088ac515d7d5205c257450649893`

前回のCSV/日時の曖昧な境界を、今回のサンプルでは「未引用フィールド中の引用符を
拒否」「小数秒1〜6桁を受け入れ、7桁以上は拒否」と明記した。実装はPython 3.9.6。
他のPython版や大規模業務で同じ時間・品質になるという一般化はしない。

## 再実行

repository rootで、新しい（まだ存在しない）sample pathを指定する。

```sh
python3 -B agent-workflows/examples/support-report/rehearse.py --project /tmp/new-support-report-case
```

これは依頼とdraftを作るだけで、CLI実装やAIを自動起動しない。別途許可した
sample workerへ`drafts/prepare-worker-briefs.json`を渡し、成果物freeze後に実行する。

```sh
python3 -B agent-workflows/examples/support-report/acceptance.py /tmp/new-support-report-case
PYTHONPATH=agent-workflows/src python3 -B -m unittest discover -s agent-workflows/tests -v
```

## 未実施・残課題

- 実人間の承認から通常Runへ採用するadapter、およびB/C/D closureとGroup Eの
  実業務接続。mock試験でこの欠落を覆い隠さない。
- helperの時間監督は次の候補記録・起動案内を止めるもので、外部AI workerを
  強制終了する監督ではない。sample processにはtimeout、workerには明示予算を使った。
- Nix build/rebuild/switch、migration、activation、live設定変更、stage/commit/pushは未実施。
  `nix-instantiate --parse flake.nix`の構文検証のみ成功した。
- `skill-creator`のquick validatorはPyYAML未導入で起動不能。代わりに既存の
  frontmatter/配布検証、参照先、focused tests、fresh entry試験を使用した。

fresh品質review報告は
`/private/tmp/inception-fixed-trial.Kel9Bj/review/quality-review.md`、
独自probeは同じdirectoryの`quality_probe.py`に保持。
実装者の自己テスト8件、親の受入24件、reviewの336 checksは別々の検証であり、
合算して網羅率を主張しない。

## 最終source回帰

全変更とcurrent digest更新を凍結した後、全体536 testsが195.295秒で成功。
logは`/tmp/inception-regression-release.log`。コマンドは上の全体unittestと同じ。
`git diff --check`、Nix構文検証、source status／source-wide／HTML projectionも成功。

途中の回帰ではcurrent digestの更新漏れと、更新中にimportした旧値による失敗が
あった。これらを成功扱いせず、最後に凍結した同一candidateへ全体回帰を再実行した。
更新履歴は`/tmp/refresh-inception-digest-trace.md`。sourceテスト合格をもって
通常B7や本番Workflowの接続済みとはしない。
