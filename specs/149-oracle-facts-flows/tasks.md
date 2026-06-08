# Tasks — #149 oracle flows on the facts API

## Slice S1 — migrate src/Oracle writes to *FromFacts
- [ ] T149-S1 Repoint legacy mpfs write ops in `src/Oracle/*` to their `*FromFacts` variants (record fields from #147); flows still validate; `./gate.sh` green.
