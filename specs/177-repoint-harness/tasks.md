# Tasks — #177 (revised)

## Slice A — extract canary devnet lifecycle into the library
- [ ] T177-SA move lifecycle helpers from app/moog-mpfs-v2-canary.hs to a library module (src/MPFS/Devnet.hs)
- [ ] T177-SA expose the module from moog.cabal library; canary exe imports it (no behavior change)
- [ ] T177-SA proof: `nix build .#moog-mpfs-v2-canary` builds green

## Slice B — integration harness boots devnet-server with funded genesis
- [ ] T177-SB harness bracket: create/load wallet(s), patch genesis to fund, launch mpfs-devnet-server, wait ready
- [ ] T177-SB run existing integration specs against MOOG_MPFS_HOST=the devnet-server
- [ ] T177-SB remove docker-compose mpfs/yaci services + yaci topup; update local-preprod.sh
- [ ] T177-SB proof: locally boots a funded token against new MPFS; suite connects

## Slice C — CI: offchain checkout build, self-hosted, moog-v2 trigger
- [ ] T177-SC integration-tests.yaml + e2e mirror the canary Prepare step (offchain checkout + devnet-server build + env capture)
- [ ] T177-SC `[self-hosted, moog]`; trigger/run on moog-v2 (drop the PR-skip guard)
- [ ] T177-SC drop docker-compose up/down + topup steps
- [ ] T177-SC proof: green run on moog-v2 reaching booted funded token
