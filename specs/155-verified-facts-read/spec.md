# Spec — #155 verified token facts read

Parent epic #146. Branch base `moog-v2`. PR target `moog-v2`.

## P1 user story

As moog, I read a token's full fact set from `GET /tokens/:id/facts` and verify
it with `verifyTokenFacts` (MPF root reconstruction == independently-trusted
root) before building my domain view.

## Context

Reads half of the cutover, part 1. The offchain bulk endpoint
(`GET /tokens/:id/facts` → `FactsResponse`) and the read verifier
(`Cardano.MPFS.Client.Verify.Read.verifyTokenFacts`) are pinned on `moog-v2`.
This migrates `mpfsGetTokenFacts` only. `getToken`/pending-requests reads are
split to #159 (blocked on offchain #310 `verifyTokenRequests`).

## Functional requirements

- FR1 — `getTokenFacts` calls the bulk endpoint (via `cardano-mpfs-client`
  `tokenFacts`) returning `FactsResponse`.
- FR2 — the response is verified with `verifyTokenFacts (TrustedRoot …)`; the
  trusted root is obtained the same way the write builders obtain it
  (`/status` `currentUtxoRoot` for now — independent-root hardening is a
  separate, tracked concern).
- FR3 — the verified `[FactEntry]` is mapped to the JSValue/facts shape moog's
  `parseFacts` (`Core.Types.Fact`) consumes, so all 6 `mpfsGetTokenFacts` call
  sites keep working unchanged.
- FR4 — verification failure fails closed (no facts returned / explicit error).

## Success criteria

- Unit test: a `FactsResponse` fixture verifies and maps to moog facts; a
  tampered/!complete fixture fails closed.
- `./gate.sh` green (build + `nix run .#unit-tests`).
- Legacy `getToken`/requests untouched (that's #159); no other behavior change.

## Non-goals

- `getToken`/pending-requests read (#159), legacy route deletion (#151),
  independent trusted-root sourcing.
