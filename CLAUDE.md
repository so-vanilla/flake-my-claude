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
