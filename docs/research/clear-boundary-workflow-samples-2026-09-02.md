# Clear境界に関するWorkflow実例調査

調査日: 2026-09-02（Asia/Tokyo）

## 結論

主要なWorkflowは、各step後に同じ方式で会話をclearしているわけではない。共通するのは、会話より長寿命なspec、plan、task、story、ledger、state、checkpointを残し、次のchat、session、agentがそれを読み直せるようにすることである。

このため本設計では、Workflow / Group / Skillの三層を維持しつつ、実行時だけContext Epochを記録する。Group終了は既定のclear境界とし、Groupが大きい場合は同じGroup内でも安全な成果物境界でEpochを閉じる。

## 実例

| 実例 | 明示的なclear | 実質的なcontext境界 | 再開の正本 | 本設計への示唆 |
| --- | --- | --- | --- | --- |
| OpenAI Responses API | chatのclearではなくserver-sideまたはstandalone compaction | `compact_threshold`到達、またはapplicationがcompactを呼ぶ時 | APIが返すcanonical next context。ただしopaque | compactionを監査・進捗の正本にせず、人間可読Checkpointも別に残す。公式例の200Kは有用な初期値だが普遍的推奨値とは書かれていない |
| Superpowers | controller sessionをtaskごとにclearしない | taskごとにfresh implementer subagent、spec review、quality review。compaction後はledgerから復旧 | plan、task brief、`.superpowers/sdd/.../progress.md`、Git history、review package | root contextを調整に限定し、workerへtask単位のfresh contextを与える。clearよりledgerとartifact handoffを重視 |
| AWS AI-DLC | stageごとのclearは必須ではない。新sessionからresume可能 | stage、gate、checkpoint、intent単位 | `aidlc-state.md`、recovery、audit、phase artifacts、runtime graph | semantic stageとsessionを分離し、state revisionとresume validationを決定的にする |
| GitHub Spec Kit | commandごとのclear要件はない | Spec → Plan → Tasks → Implement、review gate、独立sub-feature | `spec.md`、`plan.md`、`tasks.md`、constitution等 | phase artifactを次phaseの入力にする。大きすぎるfeatureは独立specへ分ける |
| OpenSpec | 明示clearなし | proposal → specs → design → tasks → apply → archive | change directory、delta specs、tasks、main specs | artifactは往復可能で、境界はgateより情報の閉じ方。Groupを過度に不可逆にしない |
| BMAD | full methodではworkflowやstoryごとにfresh chatを明示 | PRD、architecture、epic/story、dev、review | planning artifacts、story file、`sprint-status.yaml`、project context | 本設計のGroup clearに最も近い。密な一workflowを終えて文書で次chatへ渡す |
| Anthropic 2025 long-running harness | session間でfresh context | initializer sessionとincremental coding sessions | progress file、Git history、tests、working tree | compactionだけでは足りず、fresh readerが理解できるprogress artifactが必要 |
| Anthropic 2026 harness | 強いmodelでは明示resetを外しautomatic compactionで連続実行した例 | planner、generator、evaluatorとstructured artifact handoff | plan、generated app、evaluation artifacts | resetの有効性はmodelとtaskに依存する。固定教義にせずpilotで調整する |
| BMAD/Spec Kit等のlarge feature分割 | 大きいcycleを複数の独立cycleへ分ける | story/sub-feature | storyまたはsub-feature固有のspec/plan/tasks | token上限だけでなく、独立して受入可能な成果物を境界にする |
| 個人dotfilesの代表パターン | Workflow clearは扱わない | session、project-local instruction、on-demand Skill | Git管理された短いpolicy、project docs、tool-owned runtime state | always-onを小さくする助けにはなるが、長期RunのCheckpointは別途必要 |

## 一次資料

- OpenAI Compaction: https://developers.openai.com/api/docs/guides/compaction
- OpenAI GPT-5.6 model guidance: https://developers.openai.com/api/docs/guides/latest-model
- Superpowers subagent-driven development: https://github.com/obra/superpowers/blob/main/skills/subagent-driven-development/SKILL.md
- Superpowers writing plans: https://github.com/obra/superpowers/blob/main/skills/writing-plans/SKILL.md
- AWS AI-DLC CLI and resume: https://github.com/awslabs/aidlc-workflows/blob/main/docs/guide/12-cli-commands.md
- GitHub Spec Kit: https://github.github.com/spec-kit/
- Spec Kit workflows: https://github.github.com/spec-kit/reference/workflows.html
- Spec Kit spec of specs: https://github.github.com/spec-kit/concepts/spec-of-specs.html
- OpenSpec overview: https://github.com/Fission-AI/OpenSpec/blob/main/docs/overview.md
- BMAD workflow map: https://github.com/bmad-code-org/BMAD-METHOD/blob/main/docs/reference/workflow-map.md
- BMAD getting started: https://github.com/bmad-code-org/BMAD-METHOD/blob/main/docs/tutorials/getting-started.md
- Anthropic 2025 long-running harness: https://www.anthropic.com/engineering/effective-harnesses-for-long-running-agents
- Anthropic 2026 harness design: https://www.anthropic.com/engineering/harness-design-long-running-apps
- 12-Factor Agents pause/resume: https://github.com/humanlayer/12-factor-agents/blob/main/content/factor-06-launch-pause-resume.md

## 採用する設計上の解釈

1. Groupは意味・成果物・承認の境界であり、Context Epochは実行時のcontext容器である。EpochをLevel 4にはしない。
2. Group終了時はCheckpointを作ってclearする。同じGroupが大きい場合はGroupを完了扱いにせず、中間Checkpointから新Epochで続ける。
3. 目安はtarget 200K、通常上限300K、絶対上限500Kとし、次の高出力作業を始める前に予測して切る。
4. compaction、model memory、自然言語summaryは正本にしない。version、revision、digest、authority、未検証事項、次の一手を物理stateへ保存する。
5. Sub Agentはtaskごとに独立Epochを持ち、rootへ全文を戻さずreportとartifact referenceを返す。
6. 実装前に同一task分布でresume成功、制約想起、手戻り、token、latencyを測り、200K/300K/500Kを調整する。
