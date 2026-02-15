# /perm-review - パーミッション権限レビュー

PermissionRequestフックで蓄積されたログを分析し、allow/denyルールの提案と `flake-my-claude/settings.json` への適用を行う。

## 引数

$ARGUMENTS: サブコマンド（省略時はフルレビュー）

- （省略）: フルレビュー（統計 → 提案 → 適用）
- `status`: 統計表示のみ（読み取り専用）
- `cleanup`: アーカイブファイルの削除

## 処理フロー

### 1. リポジトリパス解決

- `ghq list -p so-vanilla/flake-my-claude` でflake-my-claudeリポジトリのローカルパスを取得
- 見つからない場合はエラーメッセージを出して終了

### 2. ログ読み込み

- `~/.claude/permission-logs/requests.jsonl` を読み込む
- ファイルが存在しない、または空の場合:
  - 「パーミッションログがありません。セッションで承認操作を行うとログが蓄積されます」と案内して終了

### 3. 統計表示

以下の統計情報を表示:

```
## パーミッションリクエスト統計

| ツール | リクエスト数 | 例 |
|---|---|---|
| Bash | 42 | npm test, git add ... |
| Edit | 15 | src/index.ts, ... |
| Write | 8 | ... |

合計: 65件（セッション数: 5）
期間: 2025-01-01 〜 2025-01-15
```

- ツール別リクエスト数を降順で表示
- 各ツールの代表的なコマンド/パスを最大3つ表示
- `$ARGUMENTS` が `status` の場合はここで終了

### 4. 既存ルール照合

- ステップ1で取得したflake-my-claudeリポジトリの `settings.json` を読み込む
- 現在のallow/denyルールを解析
- ログ内のリクエストのうち、既存ルールでカバー済みのものを特定・除外
- カバー率を表示:「既存ルールで 30/65件（46%）カバー済み」

### 5. allow提案生成

未カバーのリクエストを分析し、以下の3段階で提案:

#### 高確信度（自動推奨）

以下の条件を**すべて**満たすもの:
- 読み取り系のBashコマンド（副作用なし）
- 3回以上出現

例:
- `Bash(npm test)` — テスト実行（読み取り系）
- `Bash(cargo build *)` — ビルド（ローカル副作用のみ）

#### ワイルドカード候補

同一プレフィックスのコマンドが3回以上:
- `npm run dev`, `npm run build`, `npm run test` → `Bash(npm run *)`
- `cargo test --lib`, `cargo test --doc` → `Bash(cargo test *)`

#### 低確信度（要個別判断）

- 出現1-2回のコマンド
- 副作用の可能性があるコマンド

#### 書き込み系ツールの扱い

**原則: /tmp以外への書き込みを伴うツール（Edit, Write, 書き込み系Bash）はallow推奨しない。**

これらがログに含まれる場合:
- ログには記録・統計には含める
- 提案時には「書き込み系のため手動承認を維持推奨」と明示する
- ユーザーが明示的に求めた場合のみallow提案に含める

### 6. deny提案生成

allowを広げる場合の安全弁として、対応するdenyルールを提案:

#### 危険パターンテーブル

| allowパターン | 推奨deny |
|---|---|
| `Bash(npm *)` | `Bash(npm publish *)`, `Bash(npm install -g *)` |
| `Bash(cargo *)` | `Bash(cargo publish *)`, `Bash(cargo install *)` |
| `Bash(git *)` | `Bash(git push --force *)`, `Bash(git reset --hard *)` |
| `Bash(pip *)` | `Bash(pip install *)` |

- 既存のdenyルールでカバー済みのものは除外
- Write/Editの広いallowが求められた場合、機密パス（`~/.ssh/**`, `~/.config/**`等）のdenyを必ず併記

### 7. ユーザー確認

提案を一覧表示し、各提案についてユーザーに確認:

```
## allow提案

1. [高確信度] Bash(npm test) — 12回出現
   → 適用 / スキップ / 修正

2. [ワイルドカード] Bash(npm run *) — npm run dev(5), npm run build(3), npm run test(4)
   → 適用 / スキップ / 修正

3. [書き込み系] Edit(src/**) — 15回出現
   ⚠️ 書き込み系のため手動承認を維持推奨
   → スキップ（推奨） / 適用 / 修正

## deny提案（安全弁）

4. Bash(npm publish *) — npm run * のallow に対する安全弁
   → 適用 / スキップ
```

AskUserQuestionツールを使い、適用するルールをユーザーに選択させる。提案数が多い場合はカテゴリごとにまとめて確認する。

### 8. settings.json適用

- ステップ1で取得したflake-my-claudeリポジトリの `settings.json` に対してEditツールで変更を適用
- allowルールは既存のallowリストに追加（適切な位置にグループ化）
- denyルールは既存のdenyリストに追加
- 重複チェック: 既に存在するルールは追加しない

### 9. ログアーカイブ

- 処理済みの `requests.jsonl` を `~/.claude/permission-logs/archive/requests-{YYYYMMDD_HHMMSS}.jsonl` にリネーム移動
- Bashツールで `mv` を実行

### 10. 次のアクション案内

以下を提案:

```
## 次のアクション

1. `setup.sh` を実行して設定を反映（macOS環境向け）
2. `git diff settings.json` で変更内容を確認
3. `/commit` で変更をコミット
```

## サブコマンド: cleanup

`$ARGUMENTS` が `cleanup` の場合:

1. `~/.claude/permission-logs/archive/` 内のファイル一覧を表示
2. 全削除するかユーザーに確認
3. 確認後、アーカイブファイルを削除
