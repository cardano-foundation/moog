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
per executable. Homebrew tap, tarball disposition, and any release-asset
integration details are parent-arbitrated through `Q-001` through `Q-004`.

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

Apply the parent-approved old-tarball disposition, update any workflow names or
README badges affected by the release workflow replacement, run final local
gates, push the branch, and open the draft PR.

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
- Removing old `linux64.tarball`, `linux-aarch64.tarball`, and
  `darwin64.tarball` outputs may break external consumers unless the parent
  approves removal or aliases.
- Darwin bundle generation can only be fully proven on macOS CI.
- A dev-assets lib gap must be escalated through `Q-004`; this ticket must not
  patch the shared repo directly.
