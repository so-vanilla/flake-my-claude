#!/usr/bin/env bash
# Usage: spawn-worker.sh <team-id> <worker-num> <role> [working-dir] [--skip-permissions|--no-skip-permissions]
set -euo pipefail

TEAM_ID="$1"
WORKER_NUM="$2"
ROLE="$3"
WORK_DIR="${4:-$(pwd)}"
SKIP_PERMS="${5:---no-skip-permissions}"
BUFFER_NAME="*eat-claude-worker-${WORKER_NUM}*"
RESULT_DIR="/tmp/claude-team/${TEAM_ID}/worker-${WORKER_NUM}"

mkdir -p "$RESULT_DIR"

# team-info.txtからチーム構成を読み取り
TEAM_INFO_FILE="/tmp/claude-team/${TEAM_ID}/team-info.txt"
TEAM_ROSTER=""
WORKER_COUNT=1
if [[ -f "$TEAM_INFO_FILE" ]]; then
  WORKER_COUNT=$(grep '^WORKER_COUNT=' "$TEAM_INFO_FILE" | cut -d= -f2)
  for i in $(seq 1 "${WORKER_COUNT:-1}"); do
    PEER_ROLE=$(grep "^WORKER_${i}_ROLE=" "$TEAM_INFO_FILE" | cut -d= -f2-)
    if [[ "$i" -eq "$WORKER_NUM" ]]; then
      TEAM_ROSTER="${TEAM_ROSTER}  - Worker ${i}: ${PEER_ROLE} ← あなた
"
    else
      TEAM_ROSTER="${TEAM_ROSTER}  - Worker ${i}: ${PEER_ROLE}
"
    fi
  done
fi

# メッセージ宛先一覧を生成
MSG_TARGETS=""
if [[ "${WORKER_COUNT:-1}" -gt 1 ]]; then
  for i in $(seq 1 "${WORKER_COUNT:-1}"); do
    [[ "$i" -eq "$WORKER_NUM" ]] && continue
    MSG_TARGETS="${MSG_TARGETS}  - Worker ${i} 宛: ~/.claude/team/team-msg.sh ${TEAM_ID} ${WORKER_NUM} ${i} \"メッセージ\"
"
  done
  MSG_TARGETS="${MSG_TARGETS}  - 全員宛: ~/.claude/team/team-msg.sh ${TEAM_ID} ${WORKER_NUM} broadcast \"メッセージ\"
"
fi

# フェーズの判定（team-info.txtから）
CURRENT_PHASE=""
if [[ -f "$TEAM_INFO_FILE" ]]; then
  CURRENT_PHASE=$(grep '^PHASE=' "$TEAM_INFO_FILE" | cut -d= -f2)
fi

# impl phase向けの追加指示を構築
IMPL_INSTRUCTIONS=""
if [[ "$CURRENT_PHASE" == "impl" ]]; then
  # worktreeブランチ指示
  IMPL_INSTRUCTIONS="
ブランチ管理:
- あなたは専用のworktreeブランチで作業しています。このブランチから離れないでください
- mainブランチにcheckoutすることは絶対にしないでください
- git checkout main, git switch main は禁止です
- コミットは現在のブランチに対して行ってください
"
  # interface-spec.md参照指示（ファイルはオーケストレータが後から作成する場合がある）
  INTERFACE_SPEC="/tmp/claude-team/${TEAM_ID}/interface-spec.md"
  IMPL_INSTRUCTIONS="${IMPL_INSTRUCTIONS}
インターフェース仕様:
- ${INTERFACE_SPEC} に共有インターフェース仕様が定義されている場合があります
- 作業開始前にこのファイルの存在を確認し、存在する場合は必ず参照してください
- 関数シグネチャ、データ構造、命名規約は仕様に厳密に従ってください
- 仕様に定義されている名前と異なる独自の命名を導入しないでください
- 不明点がある場合はteam-msg.shで他ワーカーに確認してください
"
fi

# ワーカー用システムプロンプトをファイルに書き出し
cat > "${RESULT_DIR}/system-prompt.txt" << PROMPT_EOF
あなたはチーム(ID: ${TEAM_ID})のワーカー${WORKER_NUM}（従）です。
役割: ${ROLE}

チーム構成:
${TEAM_ROSTER}
重要なルール:
- 作業結果は ${RESULT_DIR}/result.md に書き出すこと（Bashのechoやcatで書く）
- result.mdは200行以内に収めること。詳細は result-detail.md 等に分割してよい
- 全作業完了後、最後に必ず ${RESULT_DIR}/done ファイルを作成すること（touch コマンドで作成。内容は空でよい）
- 他ワーカーの成果物は /tmp/claude-team/${TEAM_ID}/worker-*/result.md で参照できる
- あなたはオーケストレータ（主）の指示に従って作業する従セッションである
- タスクは1回で完結させること。他ワーカーへの追加要求は行わない
- 作業が完了したらresult.mdに書き出してdoneファイルを作成し、それ以上の作業はしない
${IMPL_INSTRUCTIONS}
ワーカー間メッセージング:
他のワーカーにメッセージを送信できます。メッセージは相手のClaude Code TUIに新しいユーザー入力として表示されます。
${MSG_TARGETS:+
メッセージ送信先:
${MSG_TARGETS}}
- ログ参照: cat /tmp/claude-team/${TEAM_ID}/messages/log.txt
- 制約: メッセージに改行を含めないこと。送信後は0.5秒程度の間隔を空けること

メッセージング制約:
- ラウンドあたり各宛先に最大5メッセージまで（自分から送信したもののみカウント。相手からの返答はカウントしない）
- メッセージの用途: 決定の理由や重要度を問う質問 + 全体にクリティカルな決定事項の共有
- ただし共有はresult.mdの該当箇所を強調する形でもよい（メッセージ送信は必須ではない）
- 質問を送ったら返信を待たずに自分の作業を進めること
- 延々と議論を続けることは禁止。反論は1往復まで

出力トークン制限対策:
- 大きな出力は分割して書き出すこと
- result.mdへの書き込みは1回あたり200行以下を目安にする
- 200行を超える場合は複数回に分けてEditツールで追記する
- 特に長い分析結果は result.md（要約・結論）と result-detail.md（詳細）に分割する
PROMPT_EOF

PROMPT_FILE="${RESULT_DIR}/system-prompt.txt"

# 安全規則を追記
cat >> "$PROMPT_FILE" << 'SAFETY_EOF'

安全規則（権限スキップ時の必須遵守事項）:
- rm/削除コマンドは対象を明示的に指定すること。`rm -rf` の広範囲適用は禁止
- git push、git push --force は禁止
- /tmp/claude-team/ 以外のシステムファイル（/etc, ~/.config 等）の変更禁止
- 指示の範囲外の「改善」「リファクタリング」「最適化」は行わないこと
- 不明点はresult.mdに書き出して判断をオーケストレータに委ねること
SAFETY_EOF

# 出力トークン上限の情報を追記
if [[ -n "${CLAUDE_CODE_MAX_OUTPUT_TOKENS:-}" ]]; then
  echo "出力トークン上限: ${CLAUDE_CODE_MAX_OUTPUT_TOKENS}。これを超えないよう分割出力すること。" >> "$PROMPT_FILE"
fi

# claude起動引数の構築
if [[ "$SKIP_PERMS" == "--skip-permissions" ]]; then
  CLAUDE_LIST_ARGS="\"--dangerously-skip-permissions\" \"--append-system-prompt\" (with-temp-buffer (insert-file-contents \"${PROMPT_FILE}\") (buffer-string))"
else
  CLAUDE_LIST_ARGS="\"--append-system-prompt\" (with-temp-buffer (insert-file-contents \"${PROMPT_FILE}\") (buffer-string))"
fi

# eatバッファ作成 + Claude起動（perspective.el対応、nilガード付き）
emacsclient -e "(let* ((use-persp (featurep 'perspective))
         (orig-persp (and use-persp (persp-current-name)))
         (_persp-switched
           (when (and use-persp orig-persp)
             (seq-find
               (lambda (name)
                 (when (and name (stringp name))
                   (persp-switch name t)
                   (seq-some
                     (lambda (b)
                       (string-match-p \"claude-code\" (buffer-name b)))
                     (persp-current-buffers))))
               (seq-remove #'null (persp-names))))))
  (require 'eat)
  (let* ((buf (get-buffer-create \"${BUFFER_NAME}\"))
         (default-directory \"${WORK_DIR}/\")
         (process-environment (cons \"CLAUDE_TEAM_WORKER=1\" process-environment)))
    (with-current-buffer buf
      (unless (eq major-mode 'eat-mode) (eat-mode))
      (eat-exec buf \"claude-worker-${WORKER_NUM}\"
                \"claude\" nil
                (list ${CLAUDE_LIST_ARGS})))
    ;; perspective.elにバッファを追加
    (when use-persp (persp-add-buffer buf))
    ;; 元のperspectiveに復帰
    (when (and use-persp orig-persp
               (not (string= orig-persp (persp-current-name))))
      (persp-switch orig-persp t))
    \"${BUFFER_NAME}\"))"

# --dangerously-skip-permissions時の確認プロンプトを自動応答
if [[ "$SKIP_PERMS" == "--skip-permissions" ]]; then
  for _i in $(seq 1 30); do
    TAIL=$(emacsclient -e "(with-current-buffer \"${BUFFER_NAME}\"
      (buffer-substring-no-properties
        (max (point-min) (- (point-max) 500)) (point-max)))" 2>/dev/null || echo "")
    if echo "$TAIL" | grep -q "Yes, I accept"; then
      break
    fi
    sleep 0.5
  done
  # 下矢印で"Yes, I accept"を選択し、遅延後にEnterで確定
  emacsclient -e "(let ((buf (get-buffer \"${BUFFER_NAME}\")))
    (with-current-buffer buf
      (eat-term-send-string eat-terminal \"\e[B\"))
    (run-at-time 0.2 nil
      (lambda ()
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (eat-term-send-string eat-terminal \"\r\"))))))"
fi

echo "Worker ${WORKER_NUM} spawned in ${BUFFER_NAME} (dir: ${WORK_DIR})"
