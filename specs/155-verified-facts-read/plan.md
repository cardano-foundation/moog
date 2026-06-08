# Plan — #155 verified facts read

## Tech stack
Haskell + Servant client (`MPFS.API`), `cardano-mpfs-client` (`tokenFacts`),
`cardano-mpfs-verify` (`Verify.Read.verifyTokenFacts`, `verifiedTokenFacts`),
`cardano-mpfs-api` (`FactsResponse`, `FactEntry`). Gate: build + `nix run .#unit-tests`.

## Modules in scope
- `src/MPFS/API.hs` — `getTokenFacts` client + record wiring.
- `src/MPFS/Read.hs` (new, optional) — facts fetch+verify+map, mirroring
  `MPFS.Boot`/`MPFS.Request` (getStatus → TrustedRoot → tokenFacts → verifyTokenFacts
  → map [FactEntry] to moog facts).
- `src/Core/Types/Fact.hs` — only if the map needs a helper (prefer not).
- `test/MPFS/*` — unit test.

## Slice (one bisect-safe commit)

### S1 — verified facts read
RED: unit test that a `FactsResponse` fixture verifies + maps to the facts shape
`parseFacts` consumes, and that a verification failure returns no facts / errors
(fails closed). GREEN: implement `getTokenFacts` over the bulk endpoint with
`verifyTokenFacts`, map `[FactEntry]` to moog's facts JSValue, wire into the
`MPFS m` record. Keep legacy `getToken` and other ops intact.

## Notes
- TrustedRoot: same source as the write builders (server `/status` root for now).
- `getToken`/requests reads are NOT in scope (#159, blocked on offchain #310).
