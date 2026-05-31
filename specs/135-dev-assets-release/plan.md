# Implementation Plan: Dev-Assets Release Standard

## Technical Shape

Moog will adopt the same release architecture as
`lambdasistemi/cardano-tx-tools`: a single executable spec list in `flake.nix`
drives Linux artifact packages, Darwin/Homebrew packages, and workflow matrix
entries for `moog`, `moog-oracle`, and `moog-agent`.

The Linux packages will call `inputs.dev-assets.lib.mkLinuxArtifacts` with:

- `glibcPackage = project.packages.<exe>`
- `muslPackage = project.musl64.moog.components.exes.<exe>` on `x86_64-linux`
- `muslPackage = project.aarch64-musl.moog.components.exes.<exe>` on
  `aarch64-linux`
- `bundlers = inputs.bundlers`

The Darwin packages will call `inputs.dev-assets.lib.mkDarwinHomebrewBundle`
per executable. Parent answers selected `lambdasistemi/tap`, independent Linux
and Darwin release workflows, clean replacement of the old combined tarballs,
and stop-and-Q-file handling for any dev-assets API gap.

## Slice Breakdown

### Slice 1: Planning and Decision Gate

Create `spec.md`, `plan.md`, `tasks.md`, and the four parent decision files.
This slice is non-behavioral and establishes the implementation contract.

### Slice 2: Flake Release Packages

Add `dev-assets` and `bundlers` inputs, replace bespoke Linux/Darwin artifact
wiring with shared dev-assets lib calls, expose release/dev packages and the
Linux smoke app, then update `flake.lock`.

### Slice 3: Release Workflows and CI Gate

Add `release.yml` and `darwin-release.yml`, preserve the docker release path,
bump existing `paolino/dev-assets/*` action pins, and add the aarch64 cached-GHC
dry-run gate to CI.

### Slice 4: Tarball Compatibility and PR Polish

Run final local gates, push the branch, and open the draft PR with the breaking
release-asset-layout migration called out prominently.

## Validation

- `nix flake check --no-build` for evaluation.
- `nix build .#moog-linux-release-artifacts -L` on x86_64 where practical.
- `nix run .#linux-artifact-smoke -- --artifacts-dir result --artifact-version <version> --executable-name moog --usage-grep Usage:`
  or the equivalent workflow action smoke.
- `nix build --dry-run .#packages.aarch64-linux.moog` on the aarch64 runner in
  CI to reject uncached GHC source builds.
- GitHub Actions release workflow checks on the draft PR.

## Risks

- The current branch does not contain a `release-tag.yaml`; only docker-image
  and tarball workflows are present. The release-asset integration answer must
  confirm whether adding independent release workflows is sufficient.
- Removing old combined tarball outputs is an intentional breaking release asset
  layout change; the PR body must call out the downstream migration.
- Darwin bundle generation can only be fully proven on macOS CI.
- A dev-assets lib gap must be escalated through `Q-004`; this ticket must not
  patch the shared repo directly.
