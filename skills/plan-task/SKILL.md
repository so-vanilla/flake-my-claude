---
name: plan-task
description: 中〜大規模タスクの実行前に、/clear 後も再開可能な plan file と SMART goal を作成・更新する。
model: opus
---

# Plan task

この skill は plan mode の代替ではなく、永続化された plan file を中心に作業を進めるための補助である。

## 使う場面

- 複数ファイル変更。
- 設計判断がある。
- 破壊的操作や不可逆操作がある。
- Sub Agent や Agent Teams を使う。
- 自己検証ループを伴う。
- `/clear` 後の再開性が必要。

## 実行ルール

- 実装前に SMART goal を提案する。
- goal は検証可能な受け入れ条件を含める。
- plan file だけで背景、進捗、残タスク、検証状態が復元できるようにする。
- 方針が変わったら decision log を更新する。
- 実装や検証のたびに Progress と Evidence を更新する。

## Plan file location

- Repository root を特定する。
- plan file は `<repository-root>/claude-plans/current.md` に作成・更新する。
- `docs/` には作成しない。

## Supporting files

- plan file の雛形は [plan-file-template.md](plan-file-template.md) を使う。
