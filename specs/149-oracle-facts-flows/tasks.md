# Tasks — #149 oracle flows on the facts API

## Slice S1 — migrate src/Oracle writes to *FromFacts
- [x] T149-S1 Repoint legacy mpfs write ops in `src/Oracle/*` to their `*FromFacts` variants (record fields from #147); flows still validate; `./gate.sh` green.
  - Done: `src/Oracle/Config/Cli.hs` now uses `mpfsRequestInsertFromFacts` /
    `mpfsRequestUpdateFromFacts` (pure swaps; identical arg shape and flow).
  - Out of scope: `src/Oracle/Token/Cli.hs` update-token stays on legacy
    `mpfsUpdateToken`. `mpfsUpdateTokenFromFacts` drops the `[RequestRefId]` subset
    arg and (offchain e483c50) merges ALL pending requests while skipping the
    owner/processability checks — a fail-open change vs the validated `wanted`
    subset. Migrating it needs a subset-capable facts path + validation rework,
    split to **#166**.
