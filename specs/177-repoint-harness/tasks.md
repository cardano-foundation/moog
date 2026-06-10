# Tasks — #177 (revised)

## Slice A — extract canary devnet lifecycle into the library
- [X] T177-SA move lifecycle helpers from app/moog-mpfs-v2-canary.hs to a library module (src/MPFS/Devnet.hs)
- [X] T177-SA expose the module from moog.cabal library; canary exe imports it (no behavior change)
- [X] T177-SA proof: `nix build .#moog-mpfs-v2-canary` builds green

## Slice B — integration harness boots devnet-server with funded genesis
- [X] T177-SB harness bracket: create/load wallet(s), patch genesis to fund, launch mpfs-devnet-server, wait ready
- [X] T177-SB run existing integration specs against MOOG_MPFS_HOST=the devnet-server
- [X] T177-SB also: compile-fix APISpec to v2 (drop request-tx specs → #178); docker-compose.yml/local-preprod.sh removal deferred to slice D (e2e still uses them)
- [X] T177-SB proof: locally boots a funded token against new MPFS; suite connects

## Slice C — integration-tests CI: offchain checkout build, self-hosted, moog-v2
- [X] T177-SC integration-tests.yaml mirrors the canary Prepare step (offchain checkout + devnet-server build + env capture)
- [X] T177-SC `[self-hosted, moog]`; run on moog-v2 (drop the PR-skip guard)
- [X] T177-SC drop docker-compose up/down + yaci topup steps (harness self-funds + self-boots)
- [X] T177-SC proof: actionlint clean; orchestrator watches the integration-tests check go green on PR #182

## Slice D — e2e harness + CI (mirror B+C for e2e), remove docker-compose.yml
- [ ] T177-SD e2e harness (test-E2E/) self-hosts the offchain devnet-server with funded genesis
- [ ] T177-SD E2E workflow: offchain checkout + devnet-server build + self-hosted + moog-v2
- [ ] T177-SD remove test-E2E/fixtures/docker-compose.yml; update local-preprod.sh
- [ ] T177-SD proof: e2e scenario boots a funded token against the new MPFS
