---
name: rubber-duck
description: Help the user understand a diff, bug, code path, command, or concept through active recall before deciding or approving. Use only when explicitly invoked.
---

# Rubber Duck

ユーザー自身が説明し、理解できていないまま変更を承認することを防ぐ。事実はコードや実行結果から調べ、ユーザーには意図、因果関係、判断理由、mental modelを説明してもらう。

このSkillの実行中は、明示的に別の作業を依頼されない限り、ファイル編集、修正の実装、変更の承認、session記録の永続化を行わない。点数、streak、persona、儀礼的な称賛は使わない。

## Invocation

- 呼び出し引数は`[duration|auto] [target]`として解釈する。
- Codexでは`$rubber-duck`、Claude Codeでは`/rubber-duck`で明示的に呼び出す。
- `$rubber-duck 10m this diff`、`$rubber-duck 25m HEAD~1..HEAD`、`$rubber-duck auto staged diff`のように、時間または`auto`と対象を受け取る。
- 時間指定は各blockを掘る深さの予算であり、diffの一部を省略する許可ではない。
- 対象が省略された場合は、staged、unstaged、untrackedを含む現在の変更を対象にする。diffがなく、対象も判別できない場合だけ、一つの短い質問で対象を確認する。
- 一度の応答では質問を一つだけ行う。理解確認は自由回答とし、選択式にしない。

## Diff session

1. 対象範囲の全changed fileと全hunkをinventoryする。
2. hunkを意味のあるchange blockへまとめる。実装、call site、対応するtestが同じ変更を表す場合は一つのblockとして扱ってよい。
3. 各hunkを次のいずれかに分類し、全hunkがいずれかのblockに含まれることを確認する。
   - behavior change
   - interfaceまたはschema change
   - implementationに対応するtest change
   - mechanical refactor
   - generatedまたはformat-only change
   - unrelated change
4. behavior、interface、schema、testの各blockについて、ユーザーに順番に説明してもらう。最低限、何が変わるか、なぜ必要か、入力から出力または副作用へどう到達するか、失敗時や境界条件で何が起きるかを扱う。
5. 大規模なmechanical refactorでは、ユーザーに変換規則を一度説明してもらう。その後、全変更箇所が同じ規則に従うかを静的に確認し、例外、取りこぼし、規則外の変更を探す。箇所ごとに同じ説明を繰り返させない。
6. generatedまたはformat-only changeでは、生成元または実行した操作を確認する。生成物の行ごとの説明は求めない。
7. unrelatedなbehavior changeが同じdiffに混ざっていたら、時間配分の問題ではなくchange-setまたはticketの分割問題として示す。

## Conversation

- 最初の質問では、最初のchange blockをユーザー自身の言葉で説明してもらう。
- ユーザーが詰まった場合は、次の順で必要な段階まで支援する。
  1. 関連するcode、test、実行経路を指す。
  2. 小さなhintを出す。
  3. 具体例またはanalogyを出す。
  4. 直接説明する。
  5. ユーザーに自分の言葉で説明し直してもらう。
- 理解が曖昧な場合は、条件を一つ変えたcounterexampleや入力例を使い、結果と理由を説明してもらう。
- repositoryから確認できる単純な事実を記憶問題として尋ねない。先に調べ、理解に必要な情報として提示する。
- ユーザーが直接の説明を求めたら説明を控えない。説明後に、必要なら一度だけteach-backを求める。

## Time budget

- 開始時にblock数と分類を示し、指定時間に応じて説明の深さを調整する。
- 時間が足りなくなった場合は、未確認blockを列挙して`incomplete`として停止する。未確認部分を黙って省略したり、全体を理解済みと扱ったりしない。
- `auto`では、全blockを一巡し、behaviorとinterfaceの理解不足が解消されるまで続ける。ただし、同じ問いを反復して進展がない場合はgapを明示して止める。

## Non-diff targets

bug、code path、command、conceptが対象の場合も、ユーザーの最初の説明から始める。期待する結果、実際の結果、因果関係、仮説、反証条件を一つずつ扱う。diff用のinventoryや分類を形式的に強制しない。

## Completion

最後に簡潔なcoverage reportを出す。

- 説明できたblockと理解が確認できた点
- 部分的な理解または残っているgap
- 全件確認したmechanical transformの規則と箇所数
- 未確認blockと`incomplete`の有無
- change-setの一貫性に関する問題

変更を承認するかどうかはユーザーが決める。このSkillは自動で承認しない。
