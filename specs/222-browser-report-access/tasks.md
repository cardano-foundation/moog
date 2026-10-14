# Tasks

- [ ] Establish baseline and ignored runnable branch gate; record exact source and receipts.
- [ ] Implement browser configuration, bounded sessions and single-use state with executable failure controls.
- [ ] Implement GitHub web sign-in, callback, fresh membership authorization and safe report redirect through production HTTP routes; verify callback binding/replay, allowed/denied/removed memberships, expiry, sign-out, upstream/destination failures and CLI separation.
- [ ] Update Nix packaging/Compose configuration, API and operator documentation, rollback and verification commands.
- [ ] Run branch gate, record exact commits and verification limits, deliver reviewable local commits and PR-ready handoff. Production OAuth configuration/account verification stays pending until genuine evidence exists.

Task review: one vertical source/test slice per behavior; no separated failing-test commit in accepted history; no external agent audit claimed. Parent-authored mandate, child implementation only. Mark completion with commit/receipt references; no hand-typed GREEN conclusions.
