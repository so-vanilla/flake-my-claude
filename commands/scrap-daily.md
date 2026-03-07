# /scrap-daily - デイリーニューススクラップ

前日のニュースや技術ブログを収集し、org-roamノードとして蓄積する。

## 引数

なし

## 実行方法

Task ツールで以下の通り subagent を起動し、全処理を委譲せよ:
- subagent_type: `"general-purpose"`
- model: `"sonnet"`
- prompt: 以下のプロンプト全文。`{TODAY}` は実際の今日の日付（YYYY-MM-DD）、`{YESTERDAY}` は前日の日付（YYYY-MM-DD）に置換すること

subagent の出力をそのままユーザーに表示すること。

---

### subagent prompt

```
あなたはニュースや技術ブログを収集し、org-roamノードとして保存するエージェントです。
今日の日付は {TODAY}、前日は {YESTERDAY} です。

以下の手順を順に実行してください。

## 1. WebSearchでニュース検索

以下の検索クエリを並列で実行する:
- "技術トレンド 最新ニュース {YESTERDAY}"
- "AI 人工知能 最新ニュース {YESTERDAY}"
- "NixOS Nix アップデート"
- "Linux CLI ツール 新作 話題"
- "Emacs 最新 {YESTERDAY}"
- "Claude Code AI開発ツール"
- "政治 主要ニュース {YESTERDAY}"
- "経済 マーケット ニュース {YESTERDAY}"

各クエリの結果から有用な記事を選定する。

## 2. 記事選定・カテゴリ分類

検索結果から以下の基準で記事を選定する:
- 重複排除（同じニュースの別サイト記事は1つだけ残す）
- 1日あたり10-15記事を目安
- 各記事にカテゴリタグを割り当てる:
  - it-news: 一般的なIT・テクノロジーニュース
  - ai: AI・機械学習関連
  - nix: Nix, NixOS関連
  - linux-tools: Linuxツール・CLIツール関連
  - emacs: Emacs関連
  - other-it: 上記に当てはまらないIT話題
  - politics: 政治関連
  - economics: 経済・マーケット関連

## 3. WebFetchで記事詳細を取得

選定した各記事のURLに対してWebFetchを実行し、内容の要約を取得する。
- 概要は記事の内容が十分つかめる程度（3-5文程度）にする。文ごとに改行を入れて読みやすくする
- WebFetchが失敗した場合は検索結果の情報で補完する

## 4. org-roamノードの作成

### 4.1 タイムスタンプとUUIDの準備

まず `date +%Y%m%d%H%M%S` でベースのタイムスタンプを取得する。
各ファイルごとにタイムスタンプの秒部分をインクリメントして一意にする。

UUIDは `uuidgen` コマンドで生成する。記事数+1個（インデックス用1つ + 各記事用）を一括で生成:
```
for i in $(seq 1 <N>); do uuidgen; done
```

### 4.2 記事ファイルの作成

各記事について、以下の形式でファイルを作成する:
- パス: `~/org/org-roam/<TIMESTAMP>-scrap_<slug>.org`
- slugは記事タイトルから生成（英数字・ハイフンのみ、小文字、30文字以内）

```org
:PROPERTIES:
:ID:       <UUID>
:END:
#+title: <記事タイトル>
#+filetags: :scrap-daily:<カテゴリタグ>:

* 概要
<記事の要約>

* リンク
<記事URL>
```

### 4.3 インデックスファイルの作成

全記事のリンクをまとめたインデックスファイルを作成する:
- パス: `~/org/org-roam/<TIMESTAMP>-daily_scrap_{TODAY のアンダースコア区切り}.org`
  - 例: `20260307120000-daily_scrap_2026_03_07.org`

```org
:PROPERTIES:
:ID:       <UUID>
:END:
#+title: Daily Scrap {TODAY}
#+filetags: :scrap-daily:

* 記事一覧
- [[id:<記事1のUUID>][記事タイトル1]] :<カテゴリ>:
- [[id:<記事2のUUID>][記事タイトル2]] :<カテゴリ>:
...
```

## 5. org-roam DB同期

以下のコマンドでorg-roamのDBを同期する:
```
emacsclient -e '(org-roam-db-sync)'
```

## 6. 報告

以下の情報をユーザーに報告する:
- インデックスファイルのパス
- カテゴリ別の記事数サマリー
- 各記事のタイトルとカテゴリの一覧

日本語で出力すること。
```
