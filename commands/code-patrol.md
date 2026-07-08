# /code-patrol - 自己検証ループ補助

このコマンドは固定の `detect → validate → fix` ループを実行するものではない。
タスクごとに、必要な自己検証ループを設計し、実行を補助する。

## 入力

`$ARGUMENTS` はレビュー対象、追加観点、希望する反復回数、またはユーザー指定のループ方針として扱う。

## 方針

1. まず対象タスクの性質を確認する。
2. 自動テストで判定できる場合は、テスト・ビルド・型チェックを優先する。
3. テストが弱い、または調査が難しい場合は、review quorum を使う。
4. 既知 issue と新規 issue を混同しない。
5. 反復上限、停止条件、エスカレーション条件を明示する。

## ループ設計

実行前に以下を短く提示する。

| 項目 | 内容 |
|---|---|
| Objective | 達成すること |
| SMART goal | 測定可能な完了条件 |
| Scope | 対象と対象外 |
| Loop type | test-fix / review-quorum / hypothesis-experiment 等 |
| Verifier | テスト、ビルド、型チェック、ログ、レビュー |
| Max iterations | 原則 3〜5 |
| Stop condition | 成功、進展なし、上限到達、重大リスク検出 |

## review quorum mode

自動検証が難しい場合は、複数の独立 review pass を使う。
これは「問題が存在しない」証明ではなく、定義済み範囲でのリスク低減策として扱う。

停止条件は次の形にする。

```text
severity >= threshold の NEW / CONFIRMED な unresolved issue が 0
AND N 個の独立 review pass が NEW issue 0 を報告
AND deterministic check が成功、または明示的に waiver されている
```

各 reviewer は異なる観点を持つ。

| 観点 | 例 |
|---|---|
| correctness | 挙動、回帰、境界条件 |
| security | 認可、注入、秘密情報、データ露出 |
| operations | ログ、監視、障害時挙動 |
| testability | テスト不足、再現性、fixture |
| adversarial | 楽観的前提の破壊 |

## Issue ledger

指摘は必ず分類する。

| 状態 | 意味 |
|---|---|
| NEW | 未判断の新規 issue |
| CONFIRMED | 実在し対応が必要 |
| FIXED | 修正済み |
| VERIFIED | 修正後検証済み |
| KNOWN | 既知 issue |
| DUPLICATE | 重複 |
| REJECTED | 誤検知 |
| ACCEPTED_RISK | 既知リスクとして許容 |
| DEFERRED | 後続対応へ延期 |

## 終了報告

最後に以下を報告する。

- 変更内容
- 実行した検証
- issue ledger の最終状態
- review quorum の結果
- 未検証事項
- 残リスク

詳細なループパターンやテンプレートは `self-verification-loop` skill を参照する。
