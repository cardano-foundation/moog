# #178 — Validate the oracle service against the new facts-only MPFS

## P1 user story
As an operator, the oracle observes incoming requests, validates them by type, and applies the
valid ones via `update-token` against the new facts-only MPFS — proven end-to-end by the
integration suite (which now self-hosts the offchain `mpfs-devnet-server`, #177).

## Context (from scoping)
The oracle is **already on the v2 facts API**: `tokenCmdCore`'s `UpdateToken` calls
`mpfsUpdateTokenFromFacts` (`src/Oracle/Token/Cli.hs:179`); config writes use
`mpfsRequestInsertFromFacts`/`mpfsRequestUpdateFromFacts` (`src/Oracle/Config/Cli.hs`);
`validateRequest` dispatches 12 typed request kinds (`src/Oracle/Validate/Request.hs:42`). So
this ticket is primarily **validation coverage**, not a rewrite — plus restoring the request-tx
specs #177 dropped. Fix any breakage the new coverage surfaces.

Gaps to close:
- `test-integration/MPFS/APISpec.hs` has a `-- TODO(#178)` where three request-tx specs
  (`requestInsert`/`requestDelete`/`requestUpdate`) were dropped; they must be **restored +
  migrated** to `requestInsertFromFacts`/`requestDeleteFromFacts`/`requestUpdateFromFacts`
  (`src/MPFS/Request.hs:128-207`).
- No integration test drives a full oracle **request → validate → apply** cycle against a real
  (self-hosted) MPFS.

## Acceptance criteria
- [ ] The three dropped request-tx specs are restored in `test-integration/MPFS/APISpec.hs`,
      migrated to the `*FromFacts` v2 API, and pass against the self-hosted devnet.
- [ ] An integration spec drives a full oracle cycle against the devnet MPFS: boot a token →
      a requester submits a request via the facts API → the oracle validates it
      (`validateRequest`) → applies it via `UpdateToken`/`mpfsUpdateTokenFromFacts` → the token
      state reflects the applied change. Passes.
- [ ] `nix run .#integration-tests` green on the self-hosted fleet (orchestrator-verified on the PR).
- [ ] Any oracle code path found broken against the new MPFS is fixed (none expected — verify).

## Non-goals
- Agent reconcile/report (#179). E2E harness (#186). The #153 cutover.

## Parent
#181. Depends on #177 (the self-hosting harness — merged). Parallel with #179 (disjoint:
`src/Oracle/**` + the oracle integration specs vs `src/User/Agent/**`). Only shared touch is the
one-line spec-list addition in `test-integration/Main.hs`.
