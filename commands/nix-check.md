# /nix-check - Nix/devenv環境健全性チェック

現在のプロジェクトのNix/devenv環境を読み取り専用で検証し、問題点と修正提案を報告する。

**重要: このコマンドは読み取り専用。ファイルの作成・変更・削除は一切行わない。**

## 処理フロー

以下のステップを順に実行し、結果をまとめて報告する。

### 1. ツールバージョン確認

以下のコマンドでバージョンを確認:
- `nix --version`
- `devenv version`
- `direnv --version`

いずれかが見つからない場合は警告する。

### 2. プロジェクトタイプ判定

- `git remote -v` でリモートURLを取得
- URLに `so-vanilla` を含む → **personal**
- それ以外 → **work**
- gitリポジトリ外 → **unknown**（警告付きで続行）

### 3. devenv関連ファイルの存在・内容確認

以下のファイルの存在を確認:
- `devenv.nix` - 存在する場合は内容を読んで言語設定を確認
- `devenv.yaml` - 存在する場合は内容を確認
- `devenv.lock` - 存在確認のみ
- `.devenv.flake.nix` - 存在確認のみ
- `.envrc` - 存在する場合は`use devenv`が含まれるか確認

### 4. プロジェクトタイプに応じたgit管理状態の検証

#### personalの場合
- devenvファイル（devenv.nix, devenv.yaml, devenv.lock, .devenv.flake.nix, .envrc）が`git ls-files`でtracked状態か確認
- `.git/info/exclude` に否定パターン（`!.envrc`, `!devenv.nix`等）が含まれるか確認
- `.gitignore` にdevenvファイルの除外パターンが**含まれていないこと**を確認

#### workの場合
- devenvファイルが`git ls-files`でuntracked状態か確認（trackedされていたら警告）
- `.gitignore` がdevenvファイルで汚染されていないことを確認（`devenv init`が追加した行が残っていないか）
- `.git/info/exclude` または グローバルgitignore でdevenvファイルが無視されているか確認

### 5. flake.nixの検証（personalのみ）

personalプロジェクトの場合のみ:
- `flake.nix` が存在するか確認
- flake-utilsを使用しているか確認（inputsに`flake-utils`があるか）
- devShellが定義されていないことを確認（devShellはdevenvで管理するため不要）

### 6. LSP/linter/formatterの利用可能性チェック

devenv.nixの内容から言語を判定し、対応するツールがpackagesまたはlanguages設定に含まれているか確認:

| 言語 | LSP | Linter | Formatter |
|------|-----|--------|-----------|
| Rust | rust-analyzer | clippy | rustfmt |
| Node/TS | typescript-language-server | eslint | prettier |
| Python | basedpyright | ruff | ruff |
| Go | gopls | golangci-lint | gofumpt |
| Clojure | clojure-lsp | clj-kondo | cljfmt |
| Nix | nixd | - | nixfmt |
| Haskell | haskell-language-server | hlint | ormolu |

### 7. 報告

以下の形式で結果を報告する:

```
## 環境情報
- nix: <version>
- devenv: <version>
- direnv: <version>
- プロジェクトタイプ: personal / work

## ファイル状態
- devenv.nix: ✓存在 / ✗不在
- devenv.yaml: ✓存在 / ✗不在
- .envrc: ✓存在 (use devenv含む) / ✗不在
- flake.nix: ✓存在 / ✗不在 (personalのみ)

## Git管理状態
<プロジェクトタイプに応じた検証結果>

## ツール状態
- LSP: <利用可能なLSP>
- Linter: <利用可能なlinter>
- Formatter: <利用可能なformatter>

## 問題点
<検出された問題のリスト。問題がなければ「問題は検出されませんでした」>

## 修正提案
<各問題に対する具体的な修正手順。問題がなければ省略>
```
