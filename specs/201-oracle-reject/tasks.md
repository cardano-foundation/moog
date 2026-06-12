# Tasks — #201 oracle reject

## Slice S1 — MPFS reject foundation
- [X] T201-S1 RED unit tests for `rejectTokenFactsRequest` and MPFS record wiring.
- [X] T201-S1 GREEN `MPFS.Reject`, `/facts/reject` endpoint, `mpfsRejectTokenFromFacts`, and cabal module exposure.
- [X] T201-S1 Proof: focused MPFS write-facts unit tests and `./gate.sh` pass.

## Slice S2 — oracle token reject CLI
- [X] T201-S2 RED unit tests for `RejectToken` validation/submission and parser shape.
- [X] T201-S2 GREEN `oracle token reject` command requiring explicit `-o <request-ref>` refs and routing through `mpfsRejectTokenFromFacts`.
- [X] T201-S2 Proof: focused Oracle.Token.Cli unit tests and `./gate.sh` pass.

## Slice S3 — reject boundary proof
- [X] T201-S3 RED integration/smoke proof for expired-request reject and fail-closed processable subset.
- [X] T201-S3 GREEN harness support or documented operator smoke recipe for any non-CI live-boundary portion.
- [X] T201-S3 Proof: focused integration/smoke command and `./gate.sh` pass.
