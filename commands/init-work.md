# /init-work - 会社プロジェクト開発環境初期化

会社プロジェクト（so-vanilla以外）向けにdevenvベースの開発環境を初期化する。devenvファイルはgit管理しない。

## 引数

$ARGUMENTS: 言語/フレームワーク指定(省略可。省略時は自動検出)

## 処理フロー

以下のステップを順に実行すること。

### 1. gitリポジトリ確認 + so-vanilla判定

- `git rev-parse --is-inside-work-tree` でgitリポジトリ内か確認。リポジトリ外なら中断
- `git remote -v` でリモートURLを取得し、`so-vanilla` を含むか確認
- so-vanillaの場合: 「`/init-personal` を使ってください」と案内して**中断**する

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

### 3. devenv init + .gitignore revert

- `devenv.nix` が既に存在する場合はスキップ
- 存在しない場合: `devenv init` を実行
- **重要**: `devenv init`が`.gitignore`に行を追加した場合、**即座に** `git restore .gitignore` で元に戻す

### 4. devenv.nix を言語に合わせて構成

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

### 5. flake.nixは作成しない

会社プロジェクトではflake.nixを使用しない。既存のflake.nixがあっても変更しない。

### 6. .envrc の設定

- `.envrc` が存在しない場合、以下の内容で作成:
  ```
  use devenv
  ```
- `direnv allow` を実行

### 7. devenvファイルのgit除外

`git status --porcelain` でdevenv関連ファイル（`.envrc`, `devenv.nix`, `devenv.yaml`, `devenv.lock`, `.devenv.flake.nix`）がuntrackedとして表示される場合、`.git/info/exclude` に以下の無視パターンを追加する（個人Linux環境ではグローバルgitignoreされていないため必要。会社macOS環境では既にグローバルgitignoreされているため実質的に無害）:

```
.envrc
devenv.nix
devenv.yaml
devenv.lock
.devenv.flake.nix
.devenv/
```

既に記載されているパターンは追加しない。

### 8. 検証

`devenv shell -- echo "OK"` を実行し、環境が正しく構成されたことを確認する。

### 9. 要約出力

以下の情報を報告:
- 検出/設定された言語
- 作成・変更されたファイルのリスト
- 利用可能なツール（LSP、linter、formatter）
- devenvファイルがgit管理外であること
- 次のステップ（あれば）
