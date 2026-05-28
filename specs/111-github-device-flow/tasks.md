# Tasks — moog#111

## Slice 0 — Orchestration bootstrap

- [X] T111-S0.1 Add `gate.sh` with the ticket gate commands.
- [X] T111-S0.2 Add `specs/111-github-device-flow/spec.md`.
- [X] T111-S0.3 Add `specs/111-github-device-flow/plan.md`.
- [X] T111-S0.4 Add `specs/111-github-device-flow/tasks.md`.
- [X] T111-S0.5 Commit with subject `chore(#111): bootstrap device-flow ticket gate`.
- [X] T111-S0.6 Open a draft PR linked to #111.

## Slice 1 — Public API + mock-server unit coverage

- [X] T111-S1.1 Add `Lib.GitHub.Auth.DeviceFlow` to the library exposed modules and add a testable internal module as needed.
- [X] T111-S1.2 Add the cabal dependencies needed for JSON HTTP client behavior and embedded WAI mock-server tests.
- [X] T111-S1.3 RED: add `test/Lib/GitHub/Auth/DeviceFlowSpec.hs` covering `authorization_pending -> success`, `slow_down -> success`, `expired_token`, `access_denied`, malformed JSON, callback fields, and parameter-sourced `client_id`.
- [X] T111-S1.4 GREEN: implement the device-code request, callback invocation, polling loop, interval slow-down behavior, terminal errors, success token parsing, and network/decode error mapping.
- [X] T111-S1.5 Run `./gate.sh` green.
- [X] T111-S1.6 Commit with subject `feat(auth): add GitHub OAuth device flow client` and trailer `Tasks: T111-S1`.

## Slice 2 — Live-boundary smoke harness

- [X] T111-S2.1 Add the cabal executable stanza for `moog-github-device-flow-smoke`.
- [X] T111-S2.2 RED: add a focused test proving smoke output redacts the token to first 4 characters plus total length and never includes the full token.
- [X] T111-S2.3 GREEN: implement `app/moog-github-device-flow-smoke.hs` using the exported `runDeviceFlow` API and a required `--client-id` argument.
- [X] T111-S2.4 Run `./gate.sh` green.
- [X] T111-S2.5 Commit with subject `feat(auth): add GitHub device-flow live smoke` and trailer `Tasks: T111-S2`.

## Slice 3 — Real OAuth App smoke proof

- [ ] T111-S3.1 Obtain the #110 OAuth App client id from the parent epic via Q/A file if it is not already available.
- [ ] T111-S3.2 Run `cabal run moog-github-device-flow-smoke -- --client-id <id>` against github.com.
- [ ] T111-S3.3 Record the redacted transcript artifact in `WIP.md` and the PR body; verify it contains only token first 4 characters and length.
- [ ] T111-S3.4 Run `./gate.sh` green after recording PR metadata.
- [ ] T111-S3.5 Commit any final PR metadata updates with subject `docs(#111): record device-flow live smoke proof`.

## Finalization (orchestrator)

- [ ] T111-F1 Audit the PR body against delivered behavior and test evidence.
- [ ] T111-F2 Run finalization audit for this branch and `specs/111-github-device-flow/tasks.md`.
- [ ] T111-F3 Drop `gate.sh` in `chore(#111): drop gate.sh (ready for review)`.
- [ ] T111-F4 Mark the draft PR ready for review.
- [ ] T111-F5 Wait for review and merge. Do not self-merge.

## Post-merge cleanup (orchestrator)

- [ ] T111-C1 After merge, remove `/code/moog-issue-111`.
- [ ] T111-C2 Delete local and remote branch `feat/111-deviceflow`.
- [ ] T111-C3 Run `git worktree prune`.
