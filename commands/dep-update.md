# /dep-update - 依存関係更新

プロジェクトの依存関係を更新する。Nix flakeおよび言語別パッケージマネージャの依存を対象とする。

## 処理フロー

以下のステップを順に実行すること。

### 1. プロジェクトタイプ判定

- `git remote -v` でリモートURLを取得
- URLに `so-vanilla` を含む → **personal**
- それ以外 → **work**

### 2. 依存管理ファイルの検出

以下のファイルの存在を確認し、更新対象を一覧化:

| ファイル | パッケージマネージャ | 更新コマンド |
|---|---|---|
| flake.lock | nix flake | `nix flake update` |
| devenv.lock | devenv | `devenv update` |
| Cargo.lock | cargo | `cargo update` |
| package-lock.json | npm | `npm update` |
| yarn.lock | yarn | `yarn upgrade` |
| pnpm-lock.yaml | pnpm | `pnpm update` |
| go.sum | go | `go get -u ./...` |
| pyproject.toml (uv) | uv | `uv lock --upgrade` |
| pyproject.toml (poetry) | poetry | `poetry update` |
| requirements.txt | pip | ユーザーに確認 |

- 検出されたファイルと更新コマンドをユーザーに表示し、どれを更新するか確認

### 3. 更新前の状態記録

- 各lockファイルの現在の状態を記録（差分表示用）

### 4. 依存の更新

- personalプロジェクトで`flake.lock`が存在する場合: `nix flake update` を実行
- 言語別パッケージマネージャの更新コマンドを実行
- devenv shell経由での実行を優先: `devenv shell -- <更新コマンド>`
- devenvが存在しない場合は直接実行

### 5. 更新結果の確認

- 各lockファイルの差分を `git diff` で表示
- 更新されたパッケージの一覧を報告

### 6. 動作確認の提案

- ビルドの実行を提案（`cargo build`, `npm run build`等）
- テストの実行を提案（`/test` コマンドの使用を案内）
- `nix flake check` の実行を提案（flake.nixが存在する場合）

### 7. 要約出力

以下の情報を報告:
- 更新されたパッケージマネージャ
- 主要な更新内容
- 推奨される次のアクション（ビルド確認、テスト実行）
