# Tasks — #186 (e2e harness → new MPFS, hermetic)

## Slice A — e2e harness self-hosts the offchain devnet-server with funded cfhal genesis
- [ ] T186-SA wrap test-E2E/Main.hs hspec in a devnet bracket (reuse src/MPFS/Devnet.hs): load the e2e wallet(s) incl. the cfhal requester wallet, patch genesis to fund them, launch mpfs-devnet-server, setEnv MOOG_MPFS_HOST, wait ready, run e2eSpec
- [ ] T186-SA the shell scenarios (validateATestRun.sh etc.) run the real moog CLI against the devnet MPFS (requester + oracle flows; no agent/Antithesis)
- [ ] T186-SA proof: locally a scenario boots a funded token + completes the requester/oracle flow against the new MPFS

## Slice B — E2E CI hermetic on moog-v2 + remove docker-compose
- [x] T186-SB E2E-test.yaml mirrors integration-tests.yaml: offchain checkout at the cabal.project-pinned rev (NOT main), devnet-server build, [self-hosted, moog], run on moog-v2; keep the cfhal MOOG_REQUESTER_WALLET secret setup; drop docker-compose/yaci/topup
- [x] T186-SB remove test-E2E/fixtures/docker-compose.yml; update local-preprod.sh
- [ ] T186-SB proof: green E2E run on moog-v2 (self-hosted) against the pinned 0.2.0 server (orchestrator pushes + watches PR #197)
