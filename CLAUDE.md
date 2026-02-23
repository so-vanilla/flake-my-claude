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

## 安全なファイル操作
- `rm` を使う際は必ず対象を明示的に指定すること。`rm -rf` でディレクトリを丸ごと削除する前に、中身を確認(`ls` または `find`)してから実行する
- ワイルドカード(`*`)を含む `rm` は特に注意。展開結果を事前に確認する（`echo rm target/*` や `ls target/` で確認してから実行）
- 可能な場合は `git clean` や `git checkout` など復元可能な手段を優先する
- 重要なファイルの削除前にはユーザーに確認を取ること

## Emacs操作 (emacsclient -e + eat)
- eatターミナルへのキー送信には `eat-term-send-string` を使う
  - Enterキー(送信)は `\r`(CR)を使うこと。`\n`(LF)はマルチライン入力での改行扱いになる
  - 例: `(eat-term-send-string eat-terminal "echo hello\r")`
- eatバッファの `kill-buffer` 前に、プロセスの終了確認プロンプトを無効化すること
  - `(set-process-query-on-exit-flag (get-buffer-process buf) nil)` を先に実行する
  - これを怠るとミニバッファの y/n プロンプトでEmacsサーバーがブロックされ、以降の `emacsclient -e` が全て応答不能になる
  - 一度ブロックされると `emacsclient -e` からは介入不可能。ユーザーがEmacs側で `C-g` するしかない

## Agent Teams

### 基本
- Agent Teams（in-process）を使い、親エージェントがワーカーを生成する
- ワーカーは親のパーミッション設定を継承（`--dangerously-skip-permissions`）
- ワーカーはセッション開始時にのみ生成可能（途中追加不可）
- `/team <planファイルパス>` でプランに基づくタスク実行を開始

### ツールの使い分け
- **Agent Teams**: 並列化可能な独立タスク（モジュール実装、調査、レビュー等）
- **Ralph Loop** (`/ralph-loop`): 機械的な反復作業（テスト通過までの修正ループ、リファクタリング等）。必ず `--max-iterations` を設定する
- **単一セッション/Subagent**: 順序依存が強い作業、同一ファイルへの集中編集

### ファイル所有権
- 各ワーカーに担当ファイルを明示的に割り当てる
- 同じファイルを複数ワーカーに割り当てない（最も多い失敗原因）
- 共有ファイルが避けられない場合はタスク依存関係で順序制御する

### 注意事項
- ワーカーは会話履歴を継承しない — スポーンプロンプトにコンテキストを詳述する
- 推奨ワーカー数: 2-3人（最大5人）。1ワーカーあたり5-6タスクが目安
- Agent Teams使用時は `/resume`, `/rewind` が機能しない
- Delegate modeは既知のバグがあるため使用しない

## ワークフローガイドライン

- タスクを受けたら、まずプランを立ててユーザーに提示する
- ユーザーの承認後に実装に移る
- `--dangerously-skip-permissions` はツール権限プロンプトをバイパスするが、CLAUDE.mdの指示は引き続き尊重される

### ワークフロー完了後のフィードバック
- ワークフロー（`/team`、Ralph Loop、複数ステップのカスタムコマンド連携等）が完了したら、成果物の報告に加えてワークフロー自体の振り返りを行う
- フィードバック観点:
  - ワーカー間の成果物の不整合（API契約の不一致、命名規則のばらつき等）
  - タスク分割・依存関係の妥当性（ボトルネックや無駄な待ち合わせがなかったか）
  - ファイル所有権の競合や想定外の共有編集の有無
  - プランの粒度（粗すぎ/細かすぎ）に関する所感
- 問題が見つかった場合は具体的な改善提案を添える（次回のプラン構成、ワーカー数の調整等）
- 問題がなかった場合でも「特に問題なし」と簡潔に報告する

## safe-rm（ファイル削除）
- `rm` はdenyされているため、ファイル削除には `~/.claude/bin/safe-rm` を使うこと
- ゴミ箱: `~/.local/share/claude-trash/`
- サブコマンド:
  - `safe-rm delete [-r] <path>...` — ファイル/ディレクトリをゴミ箱に移動（ディレクトリは `-r` 必須）
  - `safe-rm list` — ゴミ箱の内容を一覧表示
  - `safe-rm restore <id>` — IDで指定したアイテムを元の場所に復元（8文字以上の前方一致）
  - `safe-rm empty [--force]` — ゴミ箱を完全削除

## Custom Commands

### コマンド一覧

| コマンド | 説明 | 用途 |
|---|---|---|
| `/init-personal` | so-vanilla開発環境初期化 | so-vanilla配下の新規リポジトリのdevenv + flake.nixセットアップ |
| `/init-work` | 会社プロジェクト環境初期化 | 会社リポジトリのdevenvセットアップ |
| `/worktree` | Worktree作成 | ghq並列配置方式でworktreeを作成しdevenv環境を構築 |
| `/nix-check` | Nix/devenv環境チェック | 環境の健全性を読み取り専用で検証 |
| `/edit-claude` | Claude設定編集 | flake-my-claudeリポジトリで設定・コマンド・スクリプトを編集 |
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
| `/team` | Agent Teamsオーケストレータ | プランファイルに基づきAgent Teamsで並列タスク実行 |
| `/org-agenda` | 今日のアジェンダ表示 | schedule.org + todo.org を読み込み今日の予定・タスクを合成提示（読み取り専用） |
| `/org-bump` | デイリータスクDEADLINE修正 | `++1d` タスクの過去日DEADLINEを今日/明日に修正 |
| `/org-prune` | 古い完了タスク削除 | 1ヶ月以上前のDONE非リピートタスクを一括削除 |
| `/org-todo` | タスク追加 | 会話の文脈からタスク候補を抽出しtodo.orgに追記 |
| `/show-idle-agenda` | Emacs org-agenda 表示 | ウィンドウレイアウトを分析しワークスペースにagendaを表示 |

### ワークフロー連携パターン

superpowersスキルと組み合わせた典型的なワークフロー:

- **機能開発**: `/worktree` → 実装 → `/format` → `/test` → superpowers:verification-before-completion → `/commit` → `/pr` → superpowers:finishing-a-development-branch
- **バグ修正**: superpowers:systematic-debugging → 修正 → `/test` → `/commit`
- **新機能設計**: superpowers:brainstorming → superpowers:writing-plans → 実装 → `/format` → `/test` → `/commit`
- **リリース準備**: `/dep-update` → `/test` → `/security-review` → `/changelog` → `/commit` → `/pr`
- **プロジェクト理解**: `/explore` → `/nix-check` → superpowers:writing-plans
- **パーミッション管理**: セッション運用 → `/perm-review status` で統計確認 → `/perm-review` でルール適用 → `/commit`
- **マルチエージェント開発**: `/team <planファイルパス>` → Agent Teamsで並列実装 → `/commit`
- **org日次運用**: `/org-agenda` → `/org-bump` → 作業 → `/org-todo` → `/org-prune`（月次）
