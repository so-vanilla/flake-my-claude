# /code-patrol - コードレビューループ

detect → validate → fix のループを逐次サブエージェントで実行する。
各ステップは独立したサブエージェントで実行し、オーケストレータ（自分）のコンテキストバイアスを排除する。

## 引数

$ARGUMENTS から以下を抽出:

- **mode**: `branch`（デフォルト）または `project`
- **exit_count**: NO_ISSUES_FOUND が連続何回で終了するか（デフォルト: 1）
- その他のテキストはレビューの追加指示（フォーカス領域等）として各サブエージェントに渡す

例:
- `/review` → branch モード、1回で終了
- `/review project` → project モード、1回で終了
- `/review branch 3回` → branch モード、3回連続NO_ISSUESで終了
- `/review project セキュリティ面を重点的に` → project モード、追加指示付き

## スコープ決定

### branch モード

1. `git merge-base HEAD main` でベースコミットを特定
2. `git diff --name-only <base>..HEAD` で変更ファイル一覧を取得
3. 変更がない場合は「レビュー対象の変更がありません」と報告して終了
4. `git diff <base>..HEAD` で差分全体も取得（detectに渡す）

### project モード

1. プロジェクトのソースファイル一覧を取得（.gitignore対象は除外）
2. ファイル数が多い場合は主要ディレクトリに絞る

## ループ実行

`no_issues_streak` を 0 で初期化。`round` を 1 で初期化。

### Step 1: Detect（問題検出）

ユーザーに「Round N: Detect 実行中...」と報告。

Task subagent を起動（**model: opus, subagent_type: general-purpose**）。
以下を**プロンプトとして**渡す（team/review-detect.md の内容を含める）:

```
あなたはコードレビュアーです。読み取り専用で作業してください（ファイルの変更は絶対にしないこと）。

## タスク

{mode に応じたスコープ説明}
対象ファイルを全て読み、問題を検出してください。

{branch モードの場合: 変更ファイル一覧と diff を含める}
{project モードの場合: 対象ファイル一覧を含める}
{追加指示がある場合はここに含める}
{前回の fix 結果がある場合: 「前回の修正内容: ...」として含める。これらは対応済みなので重複報告しないこと}

## レビュー観点

1. バグや論理エラー
2. テストの安定性（フレーキーテストの原因）
3. 言語のベストプラクティスからの逸脱
4. デザインパターンの一貫性
5. エラーハンドリングの漏れ・不整合
6. 識別子の曖昧さ（同名要素とのマッチ可能性等）
7. セキュリティ上の懸念

## 出力形式

問題が見つかった場合、各問題について以下の形式で記載:

FILE: ファイルパス
LINE: 行番号
SEVERITY: high/medium/low
ISSUE: 問題の説明
FIX: 具体的な修正案

問題がない場合: NO_ISSUES_FOUND とだけ出力

## 注意

- 対応済みの指摘を重複報告しない
- スタイルの好みレベルの指摘は不要（明確な問題のみ）
- 推測ではなく、コードの事実に基づいて指摘する
```

**結果の処理**:
- `NO_ISSUES_FOUND` を含む → `no_issues_streak++`
  - `no_issues_streak >= exit_count` → 「レビュー完了（{round - 1}ラウンドで全問題解消）」と報告して終了
  - それ以外 → 「問題なし（{no_issues_streak}/{exit_count}）」と報告し Step 1 に戻る
- 問題が見つかった → `no_issues_streak = 0`、検出結果を保持して Step 2 へ

### Step 2: Validate（妥当性検証）

ユーザーに「Round N: Validate 実行中（{検出問題数}件を検証）...」と報告。

Task subagent を起動（**model: opus, subagent_type: general-purpose**）。
以下を**プロンプトとして**渡す（team/review-validate.md の内容を含める）:

```
あなたはコードレビューの検証者です。読み取り専用で作業してください（ファイルの変更は絶対にしないこと）。

## タスク

別のレビュアーが検出した問題リストを受け取り、各問題の妥当性を検証してください。
対象ファイルを実際に読み、問題が本当に存在するか確認します。

## 検出された問題リスト

{Step 1 の検出結果をここに貼り付ける}

## 検証基準

各問題について以下を判定:

- CONFIRMED: 問題は実在し、修正が必要
- REJECTED: 誤検知、またはコンテキストを考慮すると問題ではない
- DOWNGRADED: 問題は存在するが、報告されたSEVERITYより低い

## 出力形式

ISSUE: (元の問題の要約)
VERDICT: CONFIRMED / REJECTED / DOWNGRADED
REASON: 判定理由
REVISED_FIX: (CONFIRMEDの場合のみ) 修正案の改善版（元の修正案で十分なら省略可）

全ての問題がREJECTEDの場合: 各判定の後に ALL_REJECTED を出力

## 注意

- 検出者の意見に同調するバイアスを排除する
- コードを実際に読んで独立に判断する
- 修正案の副作用（他のテストへの影響等）も検討する
```

**結果の処理**:
- `ALL_REJECTED` を含む → `no_issues_streak++`、exit_count チェック → Step 1 に戻る
- CONFIRMED な問題がある → 検証結果を保持して Step 3 へ

### Step 3: Fix（修正実行）

ユーザーに「Round N: Fix 実行中（{CONFIRMED数}件を修正）...」と報告。

Task subagent を起動（**model: opus, subagent_type: general-purpose**）。
以下を**プロンプトとして**渡す（team/review-fix.md の内容を含める）:

```
あなたはコード修正の実行者です。検証済みの問題リストを受け取り、修正を実行してください。

## タスク

CONFIRMEDと判定された問題を修正してください。

## 修正対象

{Step 2 で CONFIRMED された問題のみを抽出して記載。REVISED_FIX がある場合はそちらを優先}

## 修正ルール

1. 指摘された問題のみ修正する（関連しない改善は行わない）
2. 修正前に対象ファイルを必ず読む
3. 既存のコードスタイル・パターンに合わせる
4. 修正が他のファイルに波及する場合は、影響範囲も修正する
5. 修正後にファイルを読み直して正しく適用されたか確認する

## 出力形式

各修正について:

FIXED: ファイルパス:行番号
CHANGE: 変更内容の要約

全修正完了後:

SUMMARY: N件の問題を修正しました
FILES_CHANGED: 変更したファイルの一覧
```

**結果の処理**:
- 修正結果を保持（次の detect で「対応済み」として渡す）
- ユーザーに修正サマリーを報告
- `round++` して Step 1 に戻る

## 各ラウンドの報告形式

各ステップ完了後、ユーザーに簡潔に報告する:

```
## Round N
- **Detect**: X件の問題を検出 / NO_ISSUES_FOUND
- **Validate**: CONFIRMED: X, REJECTED: Y, DOWNGRADED: Z / ALL_REJECTED
- **Fix**: X件を修正（ファイル: a.ts, b.ts）
```

## 終了時の報告

```
## レビュー完了
- 総ラウンド数: N
- 修正された問題の総数: X
- 変更されたファイル: [一覧]
```
