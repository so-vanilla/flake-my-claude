# Rule

## General
- こちらからはプロンプトを英語で与えることがあるが、特に明記がなければ回答は日本語で行うこと

## Favorite tools
- nix(flake.nix)
- devenv
- emacs
- nix-community/comma(`,`コマンド): nixpkgsのパッケージを一時的に実行できる。利用時は必ずユーザーに確認を取ること

## Workflow
### Environment settings
- グローバルへの言語ランタイム、LSP、フォーマッタなどのインストールは避け、devenvで管理する
- flake.nixの扱いについて
  - so-vanilla配下のリポジトリ(github.com/so-vanilla/*)では、flake.nixを活用しビルド等を定義する。ただし、devshellはdevenvで定義するためflake.nix側では不要
  - それ以外のリポジトリではflake.nixを使用しないこと
  - flake.nixのinputにはflake-utilsを利用すること
- devenvの扱いについて
  - devenvの生成物はグローバルのgitignoreで無視している
  - so-vanilla配下のリポジトリでは.git/info/excludeを利用して上記の無視設定を打ち消し、devenv関連ファイルをgit管理すること
  - それ以外のリポジトリではdevenv関連ファイルをgit管理しない。devenv initが.gitignoreに行を追加した場合はgit restoreなどで元に戻すこと
  - 個人環境(Linux)ではdevenvファイルはグローバルgitignoreされていない。会社環境(macOS)のみグローバルgitignoreされる
  - so-vanilla配下での.git/info/excludeの否定パターンは、会社環境向け

### Worktree
- ghq並列配置方式: リポジトリと同階層に `{repo名}_{ブランチ名(スラッシュはハイフンに変換)}` で配置
  - 例: `flake-my-emacs_feature-eat-org-input`
- プロジェクトタイプ判定: リモートURLに `so-vanilla` を含むか否か

## Coding Style
- 関数型言語・式指向の要素を取り入れる。ただし、その言語の慣習(例: PythonならPEP)が優先される
  - 例: 以下の2つのうち、後者が言語仕様でサポートされており簡潔であれば採用する

  悪い例(文指向):
  ```
  let foo
  if condition {
    foo = a
  } else {
    foo = b
  }
  ```

  良い例(式指向):
  ```
  let foo = if condition {
    a
  } else {
    b
  }
  ```

## Custom Commands

### コマンド一覧

| コマンド | 説明 | 用途 |
|---|---|---|
| `/init-personal` | so-vanilla開発環境初期化 | so-vanilla配下の新規リポジトリのdevenv + flake.nixセットアップ |
| `/init-work` | 会社プロジェクト環境初期化 | 会社リポジトリのdevenvセットアップ |
| `/worktree` | Worktree作成 | ghq並列配置方式でworktreeを作成しdevenv環境を構築 |
| `/nix-check` | Nix/devenv環境チェック | 環境の健全性を読み取り専用で検証 |
| `/commit` | スマートコミット | 変更を分析しリポジトリのスタイルに合わせたコミットメッセージを生成 |
| `/pr` | Pull Request作成 | 全コミットを分析し構造化されたPRを作成 |
| `/test` | テスト実行 | フレームワークを自動検出しdevenv経由でテスト実行 |
| `/format` | コードフォーマット | devenvに設定されたフォーマッタでコード整形 |
| `/explore` | コードベース探索 | プロジェクト構造やコンポーネントの調査・解説（読み取り専用） |
| `/cleanup` | リポジトリ整理 | staleなworktree・マージ済みブランチの検出と整理 |
| `/dep-update` | 依存関係更新 | flake.lockや言語別lockファイルの更新 |
| `/security-review` | セキュリティ監査 | コードのセキュリティ問題を検出・報告（読み取り専用） |
| `/changelog` | 変更履歴生成 | git履歴からカテゴリ分類された変更履歴を生成 |

### ワークフロー連携パターン

superpowersスキルと組み合わせた典型的なワークフロー:

- **機能開発**: `/worktree` → 実装 → `/format` → `/test` → superpowers:verification-before-completion → `/commit` → `/pr` → superpowers:finishing-a-development-branch
- **バグ修正**: superpowers:systematic-debugging → 修正 → `/test` → `/commit`
- **新機能設計**: superpowers:brainstorming → superpowers:writing-plans → 実装 → `/format` → `/test` → `/commit`
- **リリース準備**: `/dep-update` → `/test` → `/security-review` → `/changelog` → `/commit` → `/pr`
- **プロジェクト理解**: `/explore` → `/nix-check` → superpowers:writing-plans
