# /changelog - 変更履歴生成

gitの履歴から構造化された変更履歴をMarkdown形式で生成する。

## 引数

$ARGUMENTS: 範囲指定（省略時は直近タグからHEADまで）

指定方法:
- タグ名: `v1.0.0` → そのタグからHEADまで
- タグ範囲: `v1.0.0..v2.0.0` → タグ間
- コミットハッシュ: `abc1234` → そのコミットからHEADまで
- 日付指定: `since:2024-01-01` → その日付以降
- `all` → 全履歴

## 実行方法

Task ツールで以下の通り subagent を起動し、全処理を委譲せよ:
- subagent_type: `"general-purpose"`
- model: `"sonnet"`
- prompt: 以下のプロンプト全文。`{ARGUMENTS}` は実際の引数に置換すること（引数なしの場合は「指定なし」）

subagent の出力をそのままユーザーに表示すること。

---

### subagent prompt

```
あなたはgit履歴から変更履歴を生成するエージェントです。
以下の手順で変更履歴を生成してください。

範囲引数: {ARGUMENTS}

## 1. 範囲の決定

- 引数が指定されている場合はそれを使用
- 「指定なし」の場合:
  - `git describe --tags --abbrev=0` で直近のタグを取得
  - タグがない場合は最初のコミットからHEADまで
- `since:YYYY-MM-DD` 形式の場合: `git log --after=YYYY-MM-DD` を使用

## 2. コミット履歴の取得

- `git log <範囲> --pretty=format:"%H %s"` でコミット一覧を取得
- マージコミットは除外（`--no-merges`）

## 3. コミットのカテゴリ分類

各コミットを以下のカテゴリに分類:
- **feat**: 新機能追加（add, 追加, 新規, implement, 実装）
- **fix**: バグ修正（fix, 修正, bugfix, hotfix）
- **refactor**: リファクタリング（refactor, リファクタ, 整理, clean）
- **docs**: ドキュメント（docs, 文書, README, ドキュメント）
- **test**: テスト（test, テスト, spec）
- **chore**: その他（chore, build, ci, deps, 更新, update, bump）

- Conventional Commitsプレフィックス（`feat:`, `fix:`等）がある場合はそれに従う
- プレフィックスがない場合はコミットメッセージの内容から推定

## 4. 既存フォーマットの確認

- `CHANGELOG.md` が存在する場合はそのフォーマットに合わせる
- 存在しない場合は以下のデフォルトフォーマットを使用

## 5. 変更履歴の生成

コミットメッセージの言語（日本語/英語）を維持して以下の形式で生成:

## [<バージョン/範囲>] - YYYY-MM-DD

### Features
- <コミットメッセージ> (<短縮ハッシュ>)

### Bug Fixes
- <コミットメッセージ> (<短縮ハッシュ>)

### Refactoring
- <コミットメッセージ> (<短縮ハッシュ>)

### Documentation
- <コミットメッセージ> (<短縮ハッシュ>)

### Other
- <コミットメッセージ> (<短縮ハッシュ>)

空のカテゴリは省略する。

## 6. 出力

- 生成した変更履歴をそのまま出力する
- `CHANGELOG.md`が既に存在する場合: 先頭に追記するかユーザーに確認
- 存在しない場合: `CHANGELOG.md`として保存するかユーザーに確認

日本語で出力すること。
```
