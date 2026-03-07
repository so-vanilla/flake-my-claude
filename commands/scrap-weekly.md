# /scrap-weekly - ウィークリーニューススクラップ

過去7日分のdailyスクラップから重要記事をピックアップし、不足分を補完してweeklyインデックスを作成する。

## 引数

なし

## 実行方法

Task ツールで以下の通り subagent を起動し、全処理を委譲せよ:
- subagent_type: `"general-purpose"`
- model: `"sonnet"`
- prompt: 以下のプロンプト全文。`{TODAY}` は実際の今日の日付（YYYY-MM-DD）、`{WEEK_START}` は7日前の日付（YYYY-MM-DD）、`{WEEK_NUMBER}` は今週のISO週番号（WNN形式、例: W10）に置換すること

subagent の出力をそのままユーザーに表示すること。

---

### subagent prompt

```
あなたは週間ニュースダイジェストを作成するエージェントです。
今日の日付は {TODAY}、対象期間は {WEEK_START} から {TODAY} まで（ISO週番号: {WEEK_NUMBER}）です。

以下の手順を順に実行してください。

## 1. 既存dailyインデックスの読み込み

Glob ツールで `~/org/org-roam/*daily_scrap*.org` を検索し、対象期間内のインデックスファイルを特定する。

各インデックスファイルを Read ツールで読み込み、記事リンク一覧を取得する。

dailyインデックスが見つからない日がある場合は、その日付を記録しておく（ステップ3で補完する）。

## 2. 重要記事のピックアップ

dailyインデックスに含まれる記事から、以下の基準で重要記事を選定する:
- 複数日にわたって関連する話題（継続的なニュース）
- 影響範囲の大きいもの（業界全体に関わるもの、重要な技術リリース等）
- ユーザーの関心分野（AI, Nix, Emacs, Linux CLI）に直接関係するもの

選定した記事のIDとタイトルを記録する。

## 3. 不足分の補完

以下の場合、WebSearchで追加の記事を取得する:
- dailyを実行しなかった日がある場合、その日の重要ニュースを検索
- 対象期間の主要なニュースで、dailyで拾えていないものがある場合

追加検索のクエリ例:
- "今週の技術ニュース まとめ {TODAY}"
- "AI ニュース 今週 {WEEK_START}"
- "NixOS Nix 最新"

追加記事がある場合は、daily同様に個別のorg-roamノードを作成する:
- タグは `scrap-weekly` を使用（`scrap-daily` ではない）
- ファイル形式はdailyと同じ（概要は文ごとに改行を入れて読みやすくする）

### 追加記事のノード作成

タイムスタンプは `date +%Y%m%d%H%M%S` で生成、UUIDは `uuidgen` で生成する。

```org
:PROPERTIES:
:ID:       <UUID>
:END:
#+title: <記事タイトル>
#+filetags: :scrap-weekly:<カテゴリタグ>:

* 概要
<記事の要約>

* リンク
<記事URL>
```

## 4. weeklyインデックスの作成

全ての選定記事（daily既存 + weekly追加）をまとめたインデックスを作成する:
- パス: `~/org/org-roam/<TIMESTAMP>-weekly_scrap_{TODAY の年}_{WEEK_NUMBER}.org`
  - 例: `20260307120000-weekly_scrap_2026_W10.org`

```org
:PROPERTIES:
:ID:       <UUID>
:END:
#+title: Weekly Scrap {TODAY の年} {WEEK_NUMBER}
#+filetags: :scrap-weekly:

* 今週のハイライト
<今週最も重要だったニュース2-3件の簡潔なサマリー>

* 記事一覧
** IT・AI
- [[id:<UUID>][記事タイトル]] :<カテゴリ>:
...

** Nix・Linux
- [[id:<UUID>][記事タイトル]] :<カテゴリ>:
...

** 政治・経済
- [[id:<UUID>][記事タイトル]] :<カテゴリ>:
...
```

記事はカテゴリグループごとにまとめる。既存daily記事はそのIDへのリンク、新規weekly記事は新しいIDへのリンクとする。

## 5. org-roam DB同期

以下のコマンドでorg-roamのDBを同期する:
```
emacsclient -e '(org-roam-db-sync)'
```

## 6. 報告

以下の情報をユーザーに報告する:
- weeklyインデックスファイルのパス
- 対象期間とdailyインデックスの有無（日別）
- カテゴリ別の記事数サマリー
- 今週のハイライト（トップ3のニュース概要）
- 追加で取得した記事数

日本語で出力すること。
```
