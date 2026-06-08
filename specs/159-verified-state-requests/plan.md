# Plan — #159 verified state + requests read

## Modules
- `cabal.project` — offchain pin → 1b91a1f (done in scaffold).
- `src/MPFS/Read.hs` (extend; #155 created it) — `getToken` fetch+verify+assemble.
- `src/MPFS/API.hs` — record wiring for `mpfsGetToken`.
- `src/Oracle/Types.hs` — only if the Token mapping needs a constructor helper.
- `test/MPFS/*` — unit tests.

Verifiers (offchain `Cardano.MPFS.Client.Verify.Read`): `verifyTokenState`,
`verifiedTokenState`, `verifyTokenRequests`, `verifiedTokenRequests`. Mirror how
#155 used `verifyTokenFacts` in `MPFS/Read.hs`, and how boot/write builders source
the TrustedRoot (+ Blueprint via `loadCageConfig`).

## Slice (one bisect-safe commit)

### S1 — verified getToken (state + requests)
RED: unit test that a `TokenResponse` + `RequestsResponse` pair verifies and
assembles into moog's `Token { tokenRefId, tokenState, tokenRequests }`, and that
a tampered state / incomplete request-set fails closed. GREEN: implement `getToken`
over `/tokens/:id` (verifyTokenState) + `/tokens/:id/requests` (verifyTokenRequests),
map to moog's `Token`, wire `mpfsGetToken`. `mpfsGetTokenRequests` (derived from
getToken in Effects) then works. Keep facts read + write ops intact.

## Notes
- Map `WitnessedTokenState` → moog `TokenState` (root, owner); token output ref → `tokenRefId`.
- Map `[WitnessedRequest]` (`wrRequest :: RequestJSON`) → moog `[RequestZoo]`.
- If a real design choice arises (e.g. RequestJSON→RequestZoo mapping, or the
  Blueprint/prefix input for verifyTokenRequests), Q-file.
