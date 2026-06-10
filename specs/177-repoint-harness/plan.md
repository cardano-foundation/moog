# Plan — #177 (revised after reading the canary + harness)

## Key facts (established)
- The #152 canary builds `mpfs-devnet-server` from the **offchain repo's own flake**
  (`.github/workflows/mpfs-v2-canary.yaml`: separate `cardano-mpfs-offchain` checkout,
  `nix build .#mpfs-devnet-server`, blueprint + genesis captured from the **offchain**
  devshell). moog's own flake does **not** expose the devnet-server; `cabal.project` pins
  offchain only for the libraries (api/cage-tx/client/verify).
- The canary **exe** (`app/moog-mpfs-v2-canary.hs`) owns the full lifecycle in Haskell:
  `canaryWallet` → `fundedGenesis`/`patchInitialFunds` (patch `shelley-genesis.json
  initialFunds` to pre-fund the wallet) → `withDevnetServer` (launch the server). These
  helpers are **inline in app/**, so they are not importable by the test harness.
- The integration harness is a thin client: `test-integration/MPFS/APISpec.hs` reads
  `MOOG_MPFS_HOST` (`getEnv`) and runs servant `ClientM` against it. It does **not** own a
  server — today docker-compose (`yaci-cli` + `mpfs:v1.1.0`) provides the host, and the CI
  funds via yaci topup.

## Strategy
Mirror the canary, not docker-compose. Extract the canary's devnet lifecycle into the
library, have the integration harness boot the offchain `mpfs-devnet-server` with
genesis-funded test wallet(s), and wire CI to build the server from the offchain checkout
(like the canary). Drop docker-compose + yaci.

## Slices (bisect-safe; one commit each)

### Slice A — extract the canary devnet lifecycle into the library
Move the reusable lifecycle helpers (`canaryWallet`/wallet funding, `fundedGenesis`,
`patchInitialFunds`, `findDonor`, `withDevnetServer`, env plumbing for
`MPFS_BLUEPRINT`/`E2E_GENESIS_DIR`/`MPFS_DEVNET_SERVER`/`MPFS_DEVNET_PATH`) from
`app/moog-mpfs-v2-canary.hs` into a library module (e.g. `src/MPFS/Devnet.hs`), exposed
from `moog.cabal`'s library. Canary exe imports them — **no behavior change**.
Proof: `nix build .#moog-mpfs-v2-canary` builds; helpers exported.

### Slice B — integration harness boots the devnet-server with funded genesis
Add a harness entrypoint (a `withDevnetMPFS`-style bracket in the integration test setup,
or a thin launcher the `integration-tests` suite uses) that: creates/loads the test
wallet(s), patches genesis to fund them (Slice-A helpers), launches `mpfs-devnet-server`
on the integration port, waits on `/tokens .indexerStatus.ready`, then runs the existing
specs against `MOOG_MPFS_HOST`. Remove the docker-compose `mpfs`/`yaci` services and the
yaci-topup path. Update `local-preprod.sh` accordingly.
Proof: locally, `MPFS_DEVNET_SERVER=… MPFS_BLUEPRINT=… E2E_GENESIS_DIR=…` (built from the
offchain flake) → the harness boots a funded token against the new MPFS and the suite
connects (downstream oracle/agent flow failures are #178/#179, acceptable here).

### Slice C — CI: build server from offchain checkout, self-hosted, moog-v2 trigger
Rewrite `integration-tests.yaml` (+ the e2e workflow) to mirror the canary's Prepare step
(checkout offchain, `nix build .#mpfs-devnet-server`, capture blueprint/genesis/PATH via
the profile-rooted non-login-shell devshell), run on `[self-hosted, moog]`, and actually
run on `moog-v2` (replace the `if: github.event_name != 'pull_request'` skip with a
push/dispatch trigger on moog-v2). Drop the docker-compose up/down + yaci topup steps.
Proof: a green run on moog-v2 reaching the booted, funded token.

## Risk / Q-file triggers
- If the integration suite needs distinct funded roles (requester/oracle/agent), patch all
  their addresses into `initialFunds` (CI currently uses one `wallet.json` for all).
- If extracting the canary helpers forces a behavior change to the canary, Q-file before
  changing canary semantics — the canary must stay green (it is the migration's proof).
