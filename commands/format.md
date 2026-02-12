# /format - コードフォーマット

プロジェクトに設定されたフォーマッタでコードを整形する。

## 引数

$ARGUMENTS: 対象ファイル/ディレクトリ（省略時はプロジェクト全体）

## 処理フロー

以下のステップを順に実行すること。

### 1. フォーマッタの検出

`devenv.nix` の内容を読み取り、設定されているフォーマッタを特定する:

| 言語/ファイル | フォーマッタ | コマンド |
|---|---|---|
| Rust | rustfmt | `cargo fmt` |
| Node/TypeScript | prettier | `npx prettier --write` |
| Python | ruff | `ruff format` |
| Go | gofumpt | `gofumpt -w` |
| Clojure | cljfmt | `cljfmt fix` |
| Nix | nixfmt | `nixfmt` |
| Haskell | ormolu | `ormolu --mode inplace` |

- `devenv.nix`が存在しない場合は、プロジェクトの設定ファイル（`.prettierrc`, `rustfmt.toml`等）からフォーマッタを推定
- フォーマッタが特定できない場合はユーザーに確認し、devenv.nixへの設定追加を提案

### 2. フォーマット対象の決定

- $ARGUMENTSが指定されている場合はその対象のみ
- 省略時はプロジェクト全体を対象とする
- `.gitignore`で除外されているファイルはスキップ

### 3. フォーマット前の状態記録

- `git diff --stat` で現在の未ステージの変更を記録（フォーマットによる変更と区別するため）

### 4. フォーマット実行

- devenv shell経由で実行を優先: `devenv shell -- <フォーマットコマンド>`
- devenvが存在しない場合は直接実行

### 5. 結果の報告

- `git diff --stat` でフォーマットによる変更を確認
- フォーマット前後のdiffを表示（変更があった場合）
- フォーマットされたファイル数を報告
- 変更がなかった場合は「フォーマット済みです」と報告
