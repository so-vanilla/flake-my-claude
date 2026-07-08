# Loop patterns

| パターン | 用途 |
|---|---|
| fix → test → analyze → fix | テスト失敗の修正 |
| detect → validate → fix → verify | レビュー指摘の確認と修正 |
| hypothesis → experiment → update | 原因不明のバグ調査 |
| plan → implement → diff review → test | 複数ファイル実装 |
| migrate slice → verify slice → expand | 大規模移行 |
| log reduce → classify → reproduce → fix | 大量ログ調査 |
| security scan → confirm → patch → regression test | セキュリティ修正 |

## 選び方

- 自動検証が強い場合は、test-fix 系を使う。
- 自動検証が弱い場合は、review-quorum 系を使う。
- 原因不明の場合は、hypothesis-experiment 系を使う。
- 大規模移行では、小さな slice ごとに検証する。
