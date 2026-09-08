# 問い合わせの期限超過レポート（隔離サンプル）

毎朝の問い合わせCSVから対応優先順位を決めるPython CLIを作る。
Python標準ライブラリのみ。ネットワーク、外部サービス、Git commit、設定変更は禁止。

呼出し: `python3 -m sla_report tickets.csv --as-of 2026-09-06T09:00:00+09:00 --format json`
formatはjson（既定）かmarkdown。as-ofは必須で、実行時刻に依存しない。
CSV列はid,priority,status,due_at,summary。priorityはcritical/high/normal/low、
statusはopen/in_progress/closed。timezone付きISO 8601、UTC offsetとZを扱う。
このサンプルの受入精度は秒および小数秒1〜6桁。範囲外は明示的に拒否する。

open/in_progressかつdue_atがas-ofより前の行だけ抽出。同時刻は含めない。
priority順、due_atの実時刻順、idの文字列順に並べる。
JSONはcountとticketsを持ち、ticketsは入力の5列を保持する。
Markdownは表と件数。summaryの縦棒・改行、日本語を正しく表示する。

列不足・空/重複id・未知のpriority/status・不正/naive日時・不正CSV・
存在しないファイルはexit 2、stderrに原因を表示。CSV値なら行と項目も表示。
closed行も検証し、異常時の部分stdoutは禁止。正常時exit 0。
CSVはカンマ区切り、引用符は二重引用符。引用符を含むフィールドは全体を
引用し内部の引用符を二重化する。未引用フィールドの引用符は不正として拒否する。

READMEの使用例、対応日時精度、再実行可能なテストを用意する。
Grillingの回答・目的承認はこの隔離試験に限りモック。実ユーザーの承認ではない。
