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

## マルチエージェントオーケストレーション (Eat Terminal)

### 概要
emacsclient + eatターミナルを使い、複数のClaudeセッションを協調動作させる。
claude-code-ideのメインセッション（主/オーケストレータ）が、eat内のClaudeセッション（従/ワーカー）を管理する。

### ヘルパースクリプト・ガイド
| ファイル | 用途 |
|-----------|------|
| `~/.claude/team/init-team.sh <team-id> <phase> [count] [dir] [master-team-id]` | ワーカー生成（1/2/4エージェント対応） |
| `~/.claude/team/spawn-worker.sh <team-id> <worker-num> <role> [dir]` | 個別ワーカー生成 |
| `~/.claude/team/send-message.sh <worker-num> <message> [--force]` | メッセージ送信 |
| `~/.claude/team/wait-workers.sh <team-id> <count> [timeout]` | 完了待機（inotifywait/ポーリングフォールバック） |
| `~/.claude/team/read-buffer.sh <worker-num> [chars]` | バッファ読み取り |
| `~/.claude/team/setup-env.sh <setup\|check\|git-init> <dir>` | 環境構築・gitリポジトリ初期化 |
| `~/.claude/team/team-msg.sh <team-id> <from> <to\|broadcast> <msg>` | ワーカー間メッセージ送信 |
| `~/.claude/team/cleanup-team.sh <team-id> <count> [--keep-worktrees] [--force]` | チーム解散 |
| `~/.claude/team/window-guide.md` | ウィンドウ管理アドバイザリーガイド（Readツールで参照） |

### ワークフロー（4Phase制）
`/team` がオーケストレータとして各Phaseを Skill ツール経由で順次呼び出す:

1. **Phase 1 (要件)**: `/team-plan` → 多視点要件分析（3ラウンド相互参照）→ requirements-plan.md 生成
2. **Phase 2 (設計)**: `/team-design` → 多視点設計分析（3ラウンド相互参照）→ design-plan.md 生成
3. **Phase 3 (環境)**: `/team-env` → git初期化 + devenv構築 + ワーカー検証（既存環境ならスキップ）
4. **Phase 4 (実装)**: `/team-impl` → タスクフェーズ型ワーカーがプランを実装 + 検証

### ロール適合判定
`/team` のStep 2でタスクとデフォルトロールを照合し、マッチしない場合はカスタムロールを提案する。

**Phase 1（要件）デフォルトロール**:
- 4: End User Advocate / Business/Domain Expert / Technical Feasibility / Quality Gatekeeper
- 2: Product(User+Business) / Technical(Engineering+Quality)
- 1: Requirements Analyst

**Phase 2（設計）デフォルトロール**:
- 4: Architect / Interface Designer / Implementation Planner / Integration Engineer
- 2: Architecture(Architect+Interface) / Implementation(Planner+Integration)
- 1: Full-Stack Designer

**Phase 4（実装）デフォルトロール**:
- 4: Researcher / Implementer / Tester / Reviewer
- 2: Builder(Researcher+Implementer) / Verifier(Tester+Reviewer)
- 1: Full-Stack

### エージェント数
タスク複雑度に応じて1, 2, 4から選択。オーケストレータがユーザーに推奨値を提示して確認を得る。

### ウィンドウレイアウト
- **4エージェント**: 2x2グリッド（左上/右上/左下/右下）
- **2エージェント**: 左右分割
- **1エージェント**: ワークスペースウィンドウに直接表示（分割なし）
- ウィンドウ管理の詳細は `~/.claude/team/window-guide.md` を参照

### 3ラウンド制（2+エージェント時、Phase 1/2共通）
1. **Round 1**: 独立分析 → result.md書き出し
2. **Round 2**: 他者のresult.md読み + team-msg.shで質問 → 改訂版result.md
3. **Round 3**: メッセージログ踏まえ妥協・統合版を作成
※ 1エージェント時はRound 1のみ

### 通信プロトコル
- **主→従**: `send-message.sh` でプロンプトを直接送信（権限プロンプト検出で安全に中止）
- **従→主**: `/tmp/claude-team/{team-id}/worker-{n}/result.md` に結果書き出し + `done` ファイル作成
- **従→従**: `team-msg.sh` でユニキャスト（特定ワーカー宛）またはブロードキャスト（全ワーカー宛）。メッセージは相手のeat端末に直接注入される。ログは `/tmp/claude-team/{team-id}/messages/log.txt` に記録。制約: 各宛先5件/ラウンド（返答はカウント外）、クリティカル決定共有可、反論は1往復まで
- **待機**: `wait-workers.sh` でイベント駆動待機（inotifywait優先、フォールバックで5秒ポーリング）

### ファイル競合の防止
タスク分解時にファイル所有権を各ワーカーに割り当て、重複を避ける。

### カスタムロール
- `/tmp/claude-team/$TEAM_ID/custom-roles.txt` に1行1ロールで記述するとデフォルトロールを上書き可能
- 行数は `$WORKER_COUNT` と一致させること（不一致時はデフォルトにフォールバック）

### インターフェース仕様共有（Phase 2）
- `/tmp/claude-team/$TEAM_ID/interface-spec.md` にオーケストレータが仕様を記述
- ワーカーは作業開始前にこのファイルを参照し、定義された命名・シグネチャに従う
- 独自の命名を導入せず、不明点はteam-msg.shで確認

### done検知のdebounce
- send-message.shは送信先ワーカーのdoneファイルを自動削除（再作業のため）
- wait-workers.shは全員done検知後に3秒debounce待機し、doneが消えていないか再確認

### 注意事項
- Phase 4ワーカーは `--dangerously-skip-permissions` で起動する（Phase 1/2は権限スキップなし）
- cleanup-team.sh は必ず最後に実行すること（Emacsブロック防止のため安全にバッファを削除する）
- send-message.shは権限プロンプト検出時に送信を中止する（--forceで上書き可能）
- perspective.el使用時: init-team/spawn-worker/cleanupはclaude-codeバッファのperspectiveに自動切り替え→操作後に復帰
- 主セッションはdevenv有効化前から起動しているためPATHが古い。テスト・ビルド等は従セッションに委任すること。`devenv shell -- <cmd>` で軽微な検証は主セッションからも実行可能
- ワーカーセッションは `CLAUDE_TEAM_WORKER=1` 環境変数付きで起動される。hookスクリプト（session-status.sh, log-permission-request.sh）はこの変数を検出して早期終了し、主セッションのステータスファイル上書きやログ混入を防ぐ
- team-msg.shで送信したメッセージは相手のClaude Code TUIに新しいユーザー入力として表示される
- ワーカー間メッセージに改行は含めないこと（send-message.shの制約）
- MASTER_TEAM_IDは全Phase共通、各Phaseは個別のTEAM_IDを使用（衝突防止）

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
| `/team` | マルチエージェントオーケストレータ | 4Phase制（要件→設計→環境→実装）で協調タスク実行。各Phaseを遅延ロード |
| `/team-plan` | 要件プランニング | 多視点3ラウンド制で要件を分析し requirements-plan.md を生成 |
| `/team-design` | 設計プランニング | 要件プランに基づき多視点3ラウンド制で設計し design-plan.md を生成 |
| `/team-env` | 環境構築 | devenv初期化 + ワーカーによる動作検証 |
| `/team-impl` | 実装 | 設計プランに基づきワーカーが並列実装 + cherry-pick統合 + 検証 |
| `/org-agenda` | 今日のアジェンダ表示 | schedule.org + todo.org を読み込み今日の予定・タスクを合成提示（読み取り専用） |
| `/org-bump` | デイリータスクDEADLINE修正 | `++1d` タスクの過去日DEADLINEを今日/明日に修正 |
| `/org-prune` | 古い完了タスク削除 | 1ヶ月以上前のDONE非リピートタスクを一括削除 |
| `/org-todo` | タスク追加 | 会話の文脈からタスク候補を抽出しtodo.orgに追記 |

### ワークフロー連携パターン

superpowersスキルと組み合わせた典型的なワークフロー:

- **機能開発**: `/worktree` → 実装 → `/format` → `/test` → superpowers:verification-before-completion → `/commit` → `/pr` → superpowers:finishing-a-development-branch
- **バグ修正**: superpowers:systematic-debugging → 修正 → `/test` → `/commit`
- **新機能設計**: superpowers:brainstorming → superpowers:writing-plans → 実装 → `/format` → `/test` → `/commit`
- **リリース準備**: `/dep-update` → `/test` → `/security-review` → `/changelog` → `/commit` → `/pr`
- **プロジェクト理解**: `/explore` → `/nix-check` → superpowers:writing-plans
- **パーミッション管理**: セッション運用 → `/perm-review status` で統計確認 → `/perm-review` でルール適用 → `/commit`
- **マルチエージェント開発**: `/team <タスク>` → Phase 1(要件プラン) → Phase 2(設計プラン) → Phase 3(環境構築) → Phase 4(並列実装+検証) → `/commit`
- **org日次運用**: `/org-agenda` → `/org-bump` → 作業 → `/org-todo` → `/org-prune`（月次）
