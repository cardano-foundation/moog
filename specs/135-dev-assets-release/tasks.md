# Tasks: Dev-Assets Release Standard

## Slice 1 — Planning and Decision Gate

- [X] T135-S1 Write the feature specification, implementation plan, and slice task list.
- [X] T135-S1 Write Q-001 through Q-004 for parent-arbitrated release decisions.
- [X] T135-S1 Commit the planning artifacts with `Tasks: T135-S1`.

## Slice 2 — Flake Release Packages

- [X] T135-S2 Add pinned `dev-assets` and `bundlers` inputs to `flake.nix`.
- [X] T135-S2 Replace bespoke Linux/Darwin artifact imports with dev-assets lib outputs for `moog`, `moog-oracle`, and `moog-agent`.
- [X] T135-S2 Expose Linux release/dev packages and the Linux artifact smoke app.
- [X] T135-S2 Expose Darwin release/dev Homebrew packages.
- [X] T135-S2 Update `flake.lock` and prove flake evaluation.
- [X] T135-S2 Commit the flake package slice with `Tasks: T135-S2`.

## Slice 3 — Release Workflows and CI Gate

- [X] T135-S3 Add Linux release workflow covering x86_64 and aarch64 for the three release executables.
- [X] T135-S3 Add Darwin/Homebrew workflow for the three release executables.
- [X] T135-S3 Bump existing `paolino/dev-assets/*` action pins to the approved revision.
- [X] T135-S3 Add the aarch64 cached-GHC dry-run gate to CI.
- [X] T135-S3 Retire `tarballs.yaml`, redirect `CI/release.sh` and `justfile`, and update downstream CI docs.
- [X] T135-S3 Prove workflow YAML and workflow expressions locally where possible.
- [X] T135-S3 Commit the workflow slice with `Tasks: T135-S3`.

## Slice 4 — Tarball Disposition, Verification, and PR

- [X] T135-S4 Run final local gates and record any CI-only coverage.
- [X] T135-S4 Push the branch and open a draft PR with release/tarball disposition documented.
- [X] T135-S4 Commit any final compatibility or documentation edits with `Tasks: T135-S4`.
