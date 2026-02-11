# /worktree - Worktree作成

ghq並列配置方式でgit worktreeを作成し、devenv環境をセットアップする。

## 引数

$ARGUMENTS: ブランチ名（省略時はユーザーに質問する）

## 処理フロー

以下のステップを順に実行すること。

### 1. ブランチ名の取得

- $ARGUMENTSが指定されていればそれをブランチ名とする
- 未指定の場合はユーザーにブランチ名を質問する

### 2. プロジェクトタイプ判定

- `git remote -v` でリモートURLを取得
- URLに `so-vanilla` を含む → **personal**
- それ以外 → **work**

### 3. Worktreeパスの決定

- リポジトリルートを `git rev-parse --show-toplevel` で取得
- リポジトリのディレクトリ名を取得
- ブランチ名のスラッシュ(`/`)をハイフン(`-`)に変換
- Worktreeパス: `{リポジトリルートの親ディレクトリ}/{リポジトリ名}_{変換後ブランチ名}`
- 例: `/home/somura/repos/github.com/so-vanilla/flake-my-emacs` でブランチ `feature/eat-org-input` の場合 → `/home/somura/repos/github.com/so-vanilla/flake-my-emacs_feature-eat-org-input`

### 4. git worktree add

- ブランチが既に存在するか `git branch --list <ブランチ名>` と `git branch -r --list origin/<ブランチ名>` で確認
- 既存ブランチの場合: `git worktree add <パス> <ブランチ名>`
- 新規ブランチの場合: `git worktree add -b <ブランチ名> <パス>`

### 5. ignoredファイルのスマートコピー

ソースworktree（現在のリポジトリルート）から新しいworktreeに以下をコピーする:

**コピーするもの**（存在する場合のみ）:
- `.env*`（.envrcを除く。.envrcはdevenv環境セットアップで処理する）
- `.dir-locals.el`
- `.tool-versions`
- プロジェクト固有の`CLAUDE.md`（存在する場合）

**コピーしないもの**:
- `.devenv/`
- `node_modules/`
- `target/`
- `__pycache__/`
- `.venv/`
- その他のビルド成果物

### 6. devenv環境セットアップ

プロジェクトタイプに応じて処理が異なる:

#### personalの場合
- devenv.nix, devenv.yaml, flake.nix はgit管理済みなのでworktreeに自動的に含まれる
- `.envrc` もgit管理済みなら自動的に含まれる
- `direnv allow` を実行

#### workの場合
- `devenv init` を実行 → `.gitignore`の変更を即座に`git restore .gitignore`でrevert
- ソースworktreeから以下をコピー:
  - `devenv.nix`
  - `devenv.yaml`（存在する場合）
  - `devenv.lock`（存在する場合）
  - `.devenv.flake.nix`（存在する場合）
  - `.envrc`
- `direnv allow` を実行
- `.git/info/exclude`への無視パターン追加は不要（worktreeはメインリポジトリの.git/info/excludeを共有するため）

### 7. 検証 + 報告

- `devenv shell -- echo "OK"` を新しいworktreeで実行し、環境を検証
- 以下を報告:
  - 作成されたworktreeのパス
  - ブランチ名
  - プロジェクトタイプ（personal / work）
  - コピーされたファイル
  - devenv環境の状態
