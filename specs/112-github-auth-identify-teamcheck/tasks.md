# Tasks - moog#112

## Slice 0 - Orchestration bootstrap

- [X] T112-S0.1 Confirm `/code/moog-issue-112` exists on `feat/112-identify-teamcheck` based on `origin/main`.
- [X] T112-S0.2 Add `specs/112-github-auth-identify-teamcheck/spec.md`.
- [X] T112-S0.3 Add `specs/112-github-auth-identify-teamcheck/plan.md`.
- [X] T112-S0.4 Add `specs/112-github-auth-identify-teamcheck/tasks.md`.
- [X] T112-S0.5 Commit with subject `docs(#112): add auth identify teamcheck plan`.
- [X] T112-S0.6 Open a draft PR linked to #112.

## Slice 1 - Team membership API and fixture matrix

- [X] T112-S1.1 Add `Lib.GitHub.Auth.TeamCheck` to the library exposed modules and add a testable internal module as needed.
- [X] T112-S1.2 Add any cabal module declarations needed for the new source and test modules.
- [X] T112-S1.3 RED: add `test/Lib/GitHub/Auth/TeamCheckSpec.hs` covering active, pending, not member, token invalid, SSO required, and other error fixture responses.
- [X] T112-S1.4 GREEN: implement `Org`, `TeamSlug`, `MembershipResult`, `checkTeamMembership`, token auth, status mapping, and SSO URL extraction.
- [X] T112-S1.5 Run the focused TeamCheck unit tests and `./gate.sh` green.
- [X] T112-S1.6 Commit with subject `feat(auth): add GitHub team membership check` and trailer `Tasks: T112-S1`.
- [X] T112-S1.7 Announce `NOTE RELEASE: membership-result-type-pinned at <commit-or-PR-url>` in `/tmp/epic-109/moog-112/STATUS.md`.

## Slice 2 - GitHub identity API

- [ ] T112-S2.1 Add `Lib.GitHub.Auth.Identify` to the library exposed modules and add a testable internal module as needed.
- [ ] T112-S2.2 Add any cabal module declarations needed for the new source and test modules.
- [ ] T112-S2.3 RED: add `test/Lib/GitHub/Auth/IdentifySpec.hs` covering a successful fixture `/user` response and a non-success response.
- [ ] T112-S2.4 GREEN: implement `GitHubError` and `whoami :: OAuthToken -> IO (Either GitHubError GH.Login)`.
- [ ] T112-S2.5 Run the focused Identify unit tests and `./gate.sh` green.
- [ ] T112-S2.6 Commit with subject `feat(auth): add GitHub whoami lookup` and trailer `Tasks: T112-S2`.

## Slice 3 - Live-boundary auth smoke

- [ ] T112-S3.1 Add the cabal executable stanza for `moog-github-auth-smoke`.
- [ ] T112-S3.2 RED: add focused tests proving smoke output redacts token material and reports expected-login/membership outcomes.
- [ ] T112-S3.3 GREEN: implement `app/Moog/GitHub/AuthSmoke.hs` and `app/moog-github-auth-smoke.hs` using the exported `whoami` and `checkTeamMembership` APIs.
- [ ] T112-S3.4 Run the focused smoke tests and `./gate.sh` green.
- [ ] T112-S3.5 Commit with subject `feat(auth): add GitHub auth live smoke` and trailer `Tasks: T112-S3`.
- [ ] T112-S3.6 Raise a parent Q-file asking the operator to run the live smoke with a real token for `pragma/antithesis`.

## Finalization (orchestrator)

- [ ] T112-F1 Audit the PR body against delivered behavior and test evidence.
- [ ] T112-F2 Run final `./gate.sh` at HEAD.
- [ ] T112-F3 Confirm all implementation tasks in this file are checked.
- [ ] T112-F4 Mark the draft PR ready for review after required live-smoke evidence is recorded or explicitly parked by the parent.
- [ ] T112-F5 Wait for review and merge. Do not self-merge.

## Post-merge cleanup (orchestrator)

- [ ] T112-C1 After merge, remove `/code/moog-issue-112`.
- [ ] T112-C2 Delete local and remote branch `feat/112-identify-teamcheck`.
- [ ] T112-C3 Run `git worktree prune`.
