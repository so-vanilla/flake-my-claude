Claude Code の設定・コマンド・スクリプトを編集する。

作業ディレクトリ: ~/repos/github.com/so-vanilla/flake-my-claude
（~/.claude はシンボリックリンク先。編集は必ず上記リポジトリで行うこと）

$ARGUMENTS に編集指示がある場合はそれに従う。
ない場合はユーザーに何を編集したいか確認する。

### 手順

1. サンドボックス確認:
   - カレントディレクトリが ~/repos/github.com/so-vanilla/flake-my-claude **以外**の場合、sandboxが有効だとflake-my-claude配下のファイルを編集できない
   - その場合はユーザーに「sandboxを無効にしてから再度 `/edit-claude` を実行してください（`/config` または設定ファイルで `sandbox.enabled: false` に変更）」と案内して中断する
   - カレントディレクトリがflake-my-claudeの場合はそのまま続行

2. 作業リポジトリの確認:
   - ~/repos/github.com/so-vanilla/flake-my-claude が存在することを確認
   - 存在しない場合はユーザーに報告して中断

3. 編集対象の特定:
   - $ARGUMENTS の内容から編集対象ファイルを特定する
   - 主なファイル構成:
     - CLAUDE.md: プロジェクトルール・コマンド一覧
     - commands/*.md: カスタムコマンド定義
     - team/*.sh: マルチエージェントスクリプト
     - hooks/*.sh: フックスクリプト
     - settings.json: Claude Code設定
     - .claude/settings.json: パーミッションルール

4. 編集の実行:
   - 対象ファイルを ~/repos/github.com/so-vanilla/flake-my-claude 配下で編集する
   - ~/.claude/ 配下のファイルを直接編集しないこと
