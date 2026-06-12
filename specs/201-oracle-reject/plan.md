# Plan — #201 oracle reject

## Tech Stack

Haskell + Servant client (`MPFS.API`), `cardano-mpfs-offchain` reject facts and
cage builders, OptEnvConf CLI parsers, Hspec unit/integration tests, Nix dev
shell gate. The branch targets `moog-v2`.

## Modules In Scope

- `src/MPFS/Reject.hs` — new reject helper mirroring `MPFS.Update`.
- `src/MPFS/API.hs` — `/facts/reject` endpoint, MPFS record field, client wiring.
- `src/Oracle/Token/{Cli,Options}.hs` — `RejectToken` command and parser.
- `moog.cabal` — expose new module and any new spec module.
- `test/MPFS/WriteFactsSpec.hs`, `test/MockMPFS.hs` — request-body and record tests.
- `test/Oracle/Token/CliSpec.hs` — token reject command behavior tests.
- `test-integration/MPFS/APISpec.hs` or a focused integration spec — live MPFS
  reject proof if the existing harness can support the timing wait without
  secrets.

## Slice S1 — MPFS Reject Foundation

RED: unit tests for `rejectTokenFactsRequest` and `mpfsRejectTokenFromFacts` on
the MPFS record. GREEN: add `MPFS.Reject`, wire `RejectFactsEndpoint` in
`MPFS.API`, expose the new record field, and update `moog.cabal`.

The helper should mirror `MPFS.Update`: build the request body from token id,
address, and request refs; read `currentUtxoRoot`; `POST /facts/reject`; verify
with `verifyRejectFacts`; call `rejectCageTxWithEval`; return `WithUnsignedTx`.

## Slice S2 — Oracle Token Reject CLI

RED: unit tests for `tokenCmdCore (RejectToken ...)` and `oracle token reject`
parsing. GREEN: add `RejectToken`, reuse update-style token parsing,
oracle-owner check, pending-ref lookup, and non-empty-subset validation, then
submit through `mpfsRejectTokenFromFacts`.

Local token decoding does not currently retain request `submittedAt`, request
fee, or config process/retract times. The CLI must therefore require explicit
request refs and fail closed through `/facts/reject` plus `verifyRejectFacts`
for Phase-3 eligibility, rather than trying to infer rejectability from partial
local data.

## Slice S3 — Reject Boundary Proof

RED: add the narrowest integration or smoke test the existing MPFS harness can
run: boot a token with short process/retract windows, create a pending request,
wait until it is rejectable, call the reject command or MPFS reject operation,
and assert the token root is unchanged while the request disappears. Also prove
a processable request subset is refused if the harness can make that stable.

GREEN: implement only the test-support code needed for this proof. If the live
boundary requires preprod credentials or an operator wallet not available in CI,
write an explicit smoke recipe and capture local evidence in `WIP.md` instead
of weakening the automated gate.

## Gate

Per-slice focused tests:

- S1: `nix develop --accept-flake-config -c cabal test unit-tests --test-show-details=direct --test-option=--match --test-option="facts-based MPFS write requests"`
- S2: `nix develop --accept-flake-config -c cabal test unit-tests --test-show-details=direct --test-option=--match --test-option="Oracle.Token.Cli"`
- S3: focused integration command selected by the slice based on the harness.

Final gate: `./gate.sh`.

## Order Rationale

S1 creates the MPFS operation the CLI consumes. S2 adds the user-facing command
with local validation and submission. S3 exercises the live MPFS boundary after
the command path exists.

