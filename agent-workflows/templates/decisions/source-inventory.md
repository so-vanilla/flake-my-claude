# Decision source inventory candidate

Status: candidate

- Run: `<run-id>`
- Period: `<period>`
- Groups searched: `<group ids>`
- Searched scopes: `<events, checkpoints, reports, artifacts, ADRs>`
- Unreadable scopes: `<explicit list, even when empty>`
- Excluded scopes: `<explicit list, even when empty>`
- Coverage: `<complete | partial>`

## Physical decision sources

Record only `path`, `version`, and `sha256` digest. Do not copy source text into
this inventory and do not classify a missing or unreadable source as absent.
Use `partial` whenever an unreadable scope remains; preserve that scope in the
candidate and stop classification until a later complete inventory supersedes it.
