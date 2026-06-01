# Tasks — #147 facts-only foundation

## Slice S1 — pin cardano-mpfs-offchain to latest main
- [ ] T147-S1 Bump `cabal.project` offchain tag to `c9eb9ea` + nix32 `--sha256:`; add new `cage-tx` subdir if present; `./gate.sh` green.

## Slice S2 — token reads on /tokens/*
- [ ] T147-S2 RED unit test for `/tokens/:id` (`TokenResponse`) + `/tokens/:id/facts/:key` decode; GREEN switch `getToken`/`getTokenFacts` to the new endpoints; `./gate.sh` green.

## Slice S3 — client-side cage builders from facts + record wiring
- [ ] T147-S3 RED unit tests facts→unsigned-tx for request insert/delete/update, update, retract; GREEN implement builders + expose on `MPFS m` record (legacy retained); `./gate.sh` green.
