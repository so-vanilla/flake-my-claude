---
name: self-verification-loop
description: 非自明な実装タスクで、テスト、レビュー、ログ確認を組み合わせた自己検証ループを設計・実行する。
---

# Self verification loop

この skill は、タスクごとに必要な検証ループを設計するための補助である。

## 決めること

| 項目 | 内容 |
|---|---|
| Objective | 達成すること |
| SMART goal | 測定可能な完了条件 |
| Scope | 対象と対象外 |
| Verifier | test / build / typecheck / log / review |
| Loop shape | fix-test / review-quorum / hypothesis-experiment |
| Max iterations | 原則 3〜5 |
| Stop condition | 成功、進展なし、上限到達、重大リスク検出 |
| Evidence | 最終報告に載せる証拠 |

## ルール

- 明確な検証コマンドがある場合は、コマンド結果を優先する。
- 同じ失敗を2回繰り返したら、原因仮説を更新する。
- 進展がないまま上限に達したら停止し、未解決事項を報告する。
- 完了時は、成功証拠、未検証事項、残リスクを表で報告する。

## Supporting files

- ループ型の選定には [loop-patterns.md](loop-patterns.md) を読む。
- SMART goal が曖昧な場合は [smart-goal-template.md](smart-goal-template.md) を読む。
- 独立レビュー pass を使う場合は [review-quorum.md](review-quorum.md) を読む。
- 最終報告の形式には [verification-report-template.md](verification-report-template.md) を使う。
