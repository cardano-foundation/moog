# Tasks — #147 facts-only foundation

## Slice S1 — pin cardano-mpfs-offchain to latest main
- [X] T147-S1 Bump `cabal.project` offchain tag to `c9eb9ea` + nix32 `--sha256:`; add new `cage-tx` subdir if present; `./gate.sh` green.

## Slice S2 — token reads on /tokens/* — SPLIT OUT to #155 (blocked on offchain #305 + #243); not part of this PR

## Slice S3 — client-side cage builders from facts + record wiring
- [X] T147-S3 RED unit tests facts→unsigned-tx for request insert/delete/update, update, retract; GREEN implement builders + expose on `MPFS m` record (legacy retained); `./gate.sh` green.
