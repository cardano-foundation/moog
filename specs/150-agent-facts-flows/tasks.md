# Tasks — #150 agent flows on the facts API

## Slice S1 — migrate src/User/Agent writes to *FromFacts
- [ ] T150-S1 Repoint legacy mpfs write ops in `src/User/Agent/*` to their `*FromFacts` variants (record fields from #147); flows still validate; `./gate.sh` green.
