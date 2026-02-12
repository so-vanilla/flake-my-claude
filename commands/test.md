# /test - テスト実行

テストフレームワークを自動検出し、テストを実行する。失敗時は分析と修正提案を提示する。

## 引数

$ARGUMENTS: テスト対象（ファイル/ディレクトリ/テスト名。省略時は全テスト実行）

## 処理フロー

以下のステップを順に実行すること。

### 1. テストフレームワークの検出

以下のファイル/設定からテストフレームワークを検出する:

| 検出ファイル | フレームワーク | テストコマンド |
|---|---|---|
| Cargo.toml | cargo test | `cargo test` |
| package.json (scripts.test) | npm/yarn/pnpm | `npm test` / `yarn test` / `pnpm test` |
| package.json (vitest) | vitest | `npx vitest` |
| package.json (jest) | jest | `npx jest` |
| pyproject.toml / pytest.ini / conftest.py | pytest | `pytest` |
| go.mod | go test | `go test ./...` |
| deps.edn (kaocha) | kaocha | `clojure -M:test` |
| project.clj | lein test | `lein test` |
| flake.nix (checks) | nix flake check | `nix flake check` |
| Makefile (test target) | make | `make test` |

- 複数検出された場合はユーザーに選択を求める
- 検出できない場合はユーザーにテストコマンドを質問する

### 2. パッケージマネージャの検出（Node.jsの場合）

Node.jsプロジェクトの場合、ロックファイルからパッケージマネージャを検出:
- `pnpm-lock.yaml` → pnpm
- `yarn.lock` → yarn
- `package-lock.json` → npm

### 3. テストコマンドの構築

- $ARGUMENTSが指定されている場合はテスト対象を絞り込む:
  - ファイルパス指定: そのファイルのテストのみ実行
  - テスト名指定: フレームワークのフィルタ機能を使用（例: `cargo test <名前>`, `pytest -k <名前>`）
  - ディレクトリ指定: そのディレクトリ配下のテスト実行
- devenv shell経由での実行を優先: `devenv shell -- <テストコマンド>`
- devenvが存在しない場合は直接実行

### 4. テスト実行

構築したコマンドを実行する。

### 5. 結果の報告

#### テスト成功の場合
- テスト数、成功数、スキップ数を報告
- カバレッジオプションがある場合は案内:
  - cargo: `cargo tarpaulin`
  - pytest: `pytest --cov`
  - jest/vitest: `--coverage`
  - go: `-cover`

#### テスト失敗の場合
- 失敗したテストの一覧
- 各失敗の原因分析
- 修正提案の提示
- 関連するソースコードの該当箇所を特定
