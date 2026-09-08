# Archive or continue candidate

Status: candidate — performs no archive, cleanup, or next-Run creation

- Human run-outcome ref: `<path, version, digest>`
- Mode: `<archive | continue>`
- Retention entries: `<artifact ref, retain flag, rationale>`
- Important decisions: `<candidate ref, promoted flag, promotion ref>`
- Rollback artifacts: `<physical refs>`
- Next-Run handoff: `<physical ref when continuing>`

## Cleanup candidates

Each entry records `path`, `reason`, `requires_human_approval: true`, and
`executed: false`. Stop when an important decision is unpromoted or retention
or rollback evidence is missing.
