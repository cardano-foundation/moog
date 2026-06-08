# Spec — #159 verified token state + pending-requests read

Parent epic #146. Base `moog-v2`. PR target `moog-v2`. Follows #155.

## P1 user story

As moog, I read a token's state via `GET /tokens/:id` and its pending requests via
`GET /tokens/:id/requests`, verify both against the trusted root
(`verifyTokenState`, `verifyTokenRequests` — request-set completeness), and
assemble them into moog's `Token` before any oracle validation.

## Context

`mpfsGetToken` is consumed entirely for `tokenRequests` (Effects/Cli/Oracle).
The new API splits state (`/tokens/:id`) from requests (`/tokens/:id/requests`).
The read verifiers now exist on the pinned offchain (`1b91a1f`):
`Cardano.MPFS.Client.Verify.Read.{verifyTokenState, verifyTokenRequests}`.
The verified facts read (#155) already landed.

## Functional requirements

- FR1 — offchain pinned to `1b91a1f` (done in scaffold) so `verifyTokenRequests`
  is available.
- FR2 — `mpfsGetToken` fetches `GET /tokens/:id` (`TokenResponse`) and verifies
  with `verifyTokenState (TrustedRoot …)`; maps the verified `WitnessedTokenState`
  to moog's `TokenState` (root + owner) and the token's output ref to `tokenRefId`.
- FR3 — pending requests fetched from `GET /tokens/:id/requests`
  (`RequestsResponse`) and verified with `verifyTokenRequests` (request-set
  completeness); the verified `[WitnessedRequest]` mapped to moog's `[RequestZoo]`.
- FR4 — the two are assembled into moog's `Token { tokenRefId, tokenState,
  tokenRequests }` so all `mpfsGetToken` / `mpfsGetTokenRequests` consumers keep
  working unchanged.
- FR5 — verification failure fails closed.
- Trusted root sourced as in the write builders / #155 (server `/status` root).
  `verifyTokenRequests` may also need the `Blueprint` (request-address prefix) —
  obtain it as boot/write builders do (`loadCageConfig`).

## Success criteria

- Unit tests: a `TokenResponse` + `RequestsResponse` pair verifies and assembles
  into a moog `Token`; tampered/incomplete fails closed.
- `./gate.sh` green (build + `nix run .#unit-tests`).
- Legacy write ops + facts read untouched; no consumer signature changes.

## Non-goals

- Removing legacy read routes (#151); independent trusted-root sourcing.
