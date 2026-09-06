# Task package

## Identity

- Task ID: `<task-id>`
- Candidate digest: `sha256:<candidate-digest>`
- Loop level: `<artifact|section|workflow>`
- Assigned worker and output path: `<worker>` / `<path>`

## Contract

- Write scope: `<closed path list>`
- Acceptance checks: `<named checks>`
- Non-goals: `<explicit exclusions>`
- Stop conditions and finite budget: `<conditions>`

## Receipt

Record the immutable package digest, changed paths, terminal status, and the
worker report path. Do not grant Finding closure, HEAD, Git, or external-action
authority from this package.
