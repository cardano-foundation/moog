# Plan — #147 facts-only foundation

## Tech stack

Haskell + Servant client (`MPFS.API`), haskell.nix, `cardano-mpfs-offchain`
(`cardano-mpfs-api`, `cardano-mpfs-client`, new `cage-tx`) pinned via
`cabal.project` source-repository-package. Unit tests under `test/`.
Gate: `nix build --quiet --no-link .#moog-agent .#unit-tests`.

## Modules in scope

- `cabal.project` — offchain pin.
- `src/MPFS/API.hs` — facts endpoints + record.
- `src/MPFS/*.hs` — client-side cage builders (new `Request.hs`/`Update.hs`/
  `Retract.hs` mirroring `Boot.hs`/`End.hs`).
- `src/Core/Types/MPFS.hs` — only if client wiring needs it.
- `test/MPFS/*` — unit tests.

## Slices (one bisect-safe commit each)

### Slice S1 — pin cardano-mpfs-offchain to latest main
Bump `cabal.project` tag → `c9eb9ea9acc6b928ec30d4fd6d88180811876147`,
recompute `--sha256:` (nix32, via `nix flake prefetch` +
`nix hash convert --to nix32`), add the new `cage-tx` subdir if the tip
carves it. Proof: `./gate.sh` green. No unit test (dependency pin —
RED-skip; proof is the build).

### Slice S2 — token reads on /tokens/*
RED: unit test decoding a `/tokens/:id` `TokenResponse` into moog's token
view, and per-key `/tokens/:id/facts/:key`. GREEN: switch `getToken`/
`getTokenFacts` clients to the new endpoints; adapt decode.

### Slice S3 — client-side cage builders from facts + record wiring
RED: unit tests asserting facts → unsigned tx for request insert/delete/
update, update, retract. GREEN: implement the builders (mirroring
boot/end), expose them on the `MPFS m` record. Legacy ops remain.

## Order rationale

S1 first — the new facts types must exist to build against. S2 before S3 —
builders and validation read token state. S3 settles the record the flow
children consume.
