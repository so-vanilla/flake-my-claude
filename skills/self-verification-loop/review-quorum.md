# Review quorum mode

自動テストだけでは完了判断が難しい場合に使う。これは「問題が存在しない」証明ではなく、定義済み範囲でのリスク低減策である。

## Core rule

```text
unresolved NEW / CONFIRMED issue が 0
AND N 個の独立 review pass が NEW issue 0 を報告
AND deterministic check が成功、または明示的に waiver されている
```

## Review lenses

| lens | 主な観点 |
|---|---|
| correctness | 挙動、回帰、境界条件 |
| security | 認可、注入、秘密情報、データ露出 |
| operations | ログ、監視、障害時挙動 |
| testability | テスト不足、再現性、fixture |
| adversarial | 楽観的前提の破壊 |

## Issue status

| status | 意味 |
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

## Reviewer prompt requirements

- 既知 issue ledger を渡す。
- reviewer ごとに異なる lens を割り当てる。
- ファイルや行番号などの evidence を要求する。
- `NO_NEW_ISSUES` だけの出力は無効にする。
- negative evidence として、何を確認したかを表で返させる。
