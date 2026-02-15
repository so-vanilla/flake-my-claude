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

## Emacs操作 (emacsclient -e + eat)
- eatターミナルへのキー送信には `eat-term-send-string` を使う
  - Enterキー(送信)は `\r`(CR)を使うこと。`\n`(LF)はマルチライン入力での改行扱いになる
  - 例: `(eat-term-send-string eat-terminal "echo hello\r")`
- eatバッファの `kill-buffer` 前に、プロセスの終了確認プロンプトを無効化すること
  - `(set-process-query-on-exit-flag (get-buffer-process buf) nil)` を先に実行する
  - これを怠るとミニバッファの y/n プロンプトでEmacsサーバーがブロックされ、以降の `emacsclient -e` が全て応答不能になる
  - 一度ブロックされると `emacsclient -e` からは介入不可能。ユーザーがEmacs側で `C-g` するしかない

## マルチエージェントオーケストレーション (Eat Terminal)

### 概要
emacsclient + eatターミナルを使い、複数のClaudeセッションを協調動作させる。
claude-code-ideのメインセッション（主/オーケストレータ）が、eat内のClaudeセッション（従/ワーカー）を管理する。

### ヘルパースクリプト
| スクリプト | 用途 |
|-----------|------|
| `~/.claude/team/init-team.sh <team-id> <phase> [dir]` | 2x2ウィンドウ分割 + 4ワーカー一括生成 |
| `~/.claude/team/spawn-worker.sh <team-id> <worker-num> <role> [dir]` | 個別ワーカー生成 |
| `~/.claude/team/send-message.sh <worker-num> <message> [--force]` | メッセージ送信 |
| `~/.claude/team/wait-workers.sh <team-id> <count> [timeout]` | 完了待機（inotifywait/ポーリングフォールバック） |
| `~/.claude/team/read-buffer.sh <worker-num> [chars]` | バッファ読み取り |
| `~/.claude/team/setup-env.sh <working-dir> [timeout]` | 環境構築（claude -p 非対話実行） |
| `~/.claude/team/cleanup-team.sh <team-id> <count>` | チーム解散 |

### ワークフロー
1. **Phase 1 (plan)**: `init-team.sh <id> plan` → 多視点×思考スタイルの4ワーカーがタスクを分析→プラン生成
2. **Phase 1.5 (env)**: `setup-env.sh <dir>` → claude -p で開発環境を非対話構築（devenv等）
3. **Phase 2 (impl)**: `init-team.sh <id> impl` → タスクフェーズ型の4ワーカーがプランを実装

### ウィンドウレイアウト
中央ウィンドウを2x2分割し、各象限にワーカーを配置:
- 左上: Worker 1 / 右上: Worker 2 / 左下: Worker 3 / 右下: Worker 4

### 通信プロトコル
- **主→従**: `send-message.sh` でプロンプトを直接送信（権限プロンプト検出で安全に中止）
- **従→主**: `/tmp/claude-team/{team-id}/worker-{n}/result.md` に結果書き出し + `done` ファイル作成
- **従→従**: 共有ディレクトリ `/tmp/claude-team/{team-id}/` のファイル経由、オーケストレータが中継
- **待機**: `wait-workers.sh` でイベント駆動待機（inotifywait優先、フォールバックで5秒ポーリング）

### ファイル競合の防止
CCAと同じアプローチ: タスク分解時にファイル所有権を各ワーカーに割り当て、重複を避ける。

### 注意事項
- Phase 2ワーカーは `--dangerously-skip-permissions` で起動する（Phase 1は権限スキップなし）
- cleanup-team.sh は必ず最後に実行すること（Emacsブロック防止のため安全にバッファを削除する）
- send-message.shは権限プロンプト検出時に送信を中止する（--forceで上書き可能）
- perspective.el使用時: init-team/spawn-worker/cleanupはclaude-codeバッファのperspectiveに自動切り替え→操作後に復帰
- 主セッションはdevenv有効化前から起動しているためPATHが古い。テスト・ビルド等は従セッションに委任すること
- ワーカーセッションは `CLAUDE_TEAM_WORKER=1` 環境変数付きで起動される。hookスクリプト（session-status.sh, log-permission-request.sh）はこの変数を検出して早期終了し、主セッションのステータスファイル上書きやログ混入を防ぐ

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
| `/perm-review` | パーミッション権限レビュー | PermissionRequestログを分析しallow/denyルールを提案・適用 |
| `/team` | マルチエージェントチーム | Eat Terminal経由で4ワーカーを生成し、2フェーズ制で協調タスク実行 |

### ワークフロー連携パターン

superpowersスキルと組み合わせた典型的なワークフロー:

- **機能開発**: `/worktree` → 実装 → `/format` → `/test` → superpowers:verification-before-completion → `/commit` → `/pr` → superpowers:finishing-a-development-branch
- **バグ修正**: superpowers:systematic-debugging → 修正 → `/test` → `/commit`
- **新機能設計**: superpowers:brainstorming → superpowers:writing-plans → 実装 → `/format` → `/test` → `/commit`
- **リリース準備**: `/dep-update` → `/test` → `/security-review` → `/changelog` → `/commit` → `/pr`
- **プロジェクト理解**: `/explore` → `/nix-check` → superpowers:writing-plans
- **パーミッション管理**: セッション運用 → `/perm-review status` で統計確認 → `/perm-review` でルール適用 → `/commit`
- **マルチエージェント開発**: `/team <タスク>` → Phase 1(多視点プラン) → Phase 2(並列実装) → 統合 → `/commit`
