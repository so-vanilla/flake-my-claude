# /init-personal - so-vanilla開発環境初期化

so-vanilla配下のリポジトリ向けにdevenv + flake.nixベースの開発環境を初期化する。

## 引数

$ARGUMENTS: 言語/フレームワーク指定(省略可。省略時は自動検出)

## 処理フロー

以下のステップを順に実行すること。

### 1. gitリポジトリ確認 + so-vanilla判定

- `git rev-parse --is-inside-work-tree` でgitリポジトリ内か確認。リポジトリ外なら中断
- `git remote -v` でリモートURLを取得し、`so-vanilla` を含むか確認
- so-vanillaでない場合: **警告**を表示し「`/init-work` を使うべきでは？」と案内。ユーザーに続行するか確認

### 2. 言語/フレームワーク自動検出

$ARGUMENTSが指定されていればそちらを優先する。未指定の場合、以下のファイルから自動検出:

| ファイル | 言語 |
|---|---|
| Cargo.toml | Rust |
| package.json | Node/TypeScript |
| pyproject.toml / setup.py / requirements.txt | Python |
| go.mod | Go |
| deps.edn / project.clj | Clojure |
| *.el / Cask | Emacs Lisp |
| *.cabal / stack.yaml / cabal.project | Haskell |
| flake.nix (のみ) | Nix |

複数検出された場合や未検出の場合はユーザーに質問する。

### 3. devenv init

- `devenv.nix` が既に存在する場合はスキップ
- 存在しない場合: `devenv init` を実行
- **重要**: `devenv init`が`.gitignore`に行を追加した場合、`git restore .gitignore` で元に戻す（個人環境ではグローバルgitignoreされていないが、so-vanillaリポジトリではdevenvファイルをgit管理するため.gitignoreへの追加は不要）

### 4. .git/info/exclude に否定パターン追加

会社環境(macOS)ではdevenvファイルがグローバルgitignoreされているため、`.git/info/exclude` に以下の否定パターンを追加する。既に存在するパターンは追加しない:

```
!.envrc
!devenv.nix
!devenv.yaml
!devenv.lock
!.devenv.flake.nix
```

### 5. devenv.nix を言語に合わせて構成

検出した言語に応じてdevenv.nixを構成する。LSP、linter、formatterを含める:

| 言語 | LSP | Linter | Formatter |
|------|-----|--------|-----------|
| Rust | rust-analyzer | clippy | rustfmt |
| Node/TS | typescript-language-server | eslint | prettier |
| Python | basedpyright | ruff | ruff |
| Go | gopls | golangci-lint | gofumpt |
| Clojure | clojure-lsp | clj-kondo | cljfmt |
| Nix | nixd | - | nixfmt |
| Haskell | haskell-language-server | hlint | ormolu |
| Emacs Lisp | - | - | - |

devenv.nixではlanguages.*を活用して簡潔に記述する。packagesにはLSP、linter、formatterを追加する。

### 6. flake.nix の作成

- flake.nixが既に存在する場合: devShellが定義されていないことを確認（devShellはdevenvで管理するため不要）。devShellが定義されていたら警告
- 存在しない場合: flake-utilsを使用したflake.nixを作成する。言語に合わせたビルド設定を含める。devShellは定義しない

### 7. .envrc の設定

- `.envrc` が存在しない場合、以下の内容で作成:
  ```
  use devenv
  ```
- `direnv allow` を実行

### 8. 検証

`devenv shell -- echo "OK"` を実行し、環境が正しく構成されたことを確認する。

### 9. 要約出力

以下の情報を報告:
- 検出/設定された言語
- 作成・変更されたファイルのリスト
- 利用可能なツール（LSP、linter、formatter）
- 次のステップ（あれば）
