# Human run-outcome event

Status: human decision required

- Objective-audit ref: `<path, version, digest>`
- Outcome: `<achieved | partially-achieved | not-achieved | superseded | abandoned>`
- Reason: `<reason>`
- Remaining tasks: `<explicit list, even when empty>`
- Human actor: `<stable human id>`
- Authority digest: `<sha256>`
- Expected HEAD: `<revision and transaction digest>`

`achieved` is invalid when objective-state is not `achieved` or any material
unverified item remains. This event records a human outcome decision; the
compiler validates it but never grants or applies it.
