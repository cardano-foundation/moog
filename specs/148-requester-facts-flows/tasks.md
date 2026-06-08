# Tasks — #148 requester flows on the facts API

## Slice S1 — migrate src/User/Requester writes to *FromFacts
- [x] T148-S1 Repoint legacy mpfs write ops in `src/User/Requester/*` to their `*FromFacts` variants (record fields from #147); flows still validate; `./gate.sh` green.
