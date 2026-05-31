# Feature Specification: Dev-Assets Release Standard

## Primary User Story

As a moog maintainer, I need the release pipeline to publish the shared
dev-assets artifact set for `moog`, `moog-oracle`, and `moog-agent` so Linux
and macOS users can install the release executables through the same channels
used by the Cardano tooling fleet.

## User Stories

1. As a Linux user, I can download AppImage, DEB, RPM, and static musl tarball
   artifacts for each release executable on x86_64 and aarch64.
2. As a macOS user, I can install the release executables through Homebrew from
   the approved tap.
3. As a maintainer, I can run pull-request release checks that build and smoke
   the artifact matrix without publishing a real release.
4. As a release operator, pushing a `v*` tag publishes release assets without
   disturbing the existing docker image release flow.

## Functional Requirements

- FR-001: `flake.nix` MUST add pinned `dev-assets` and `bundlers` inputs.
- FR-002: Linux release packages MUST be exposed as
  `<exe>-linux-release-artifacts` and `<exe>-linux-dev-release-artifacts` for
  `moog`, `moog-oracle`, and `moog-agent`.
- FR-003: Linux artifacts MUST be built through
  `dev-assets.lib.mkLinuxArtifacts` with glibc and musl packages wired per
  executable.
- FR-004: The existing static musl cross outputs MUST be reused rather than
  replaced by a new cross project unless the dev-assets API cannot express the
  needed shape.
- FR-005: A Linux artifact smoke app MUST be exposed through
  `dev-assets.lib.mkLinuxArtifactSmoke`.
- FR-006: Darwin release packages MUST be exposed through
  `dev-assets.lib.mkDarwinHomebrewBundle` for the three release executables.
- FR-007: The release workflows MUST build the full Linux matrix on x86_64 and
  aarch64 and a Darwin/Homebrew matrix on macOS.
- FR-008: Existing docker release behavior MUST remain intact.
- FR-009: Existing workflow pins to `paolino/dev-assets/*` MUST be updated to
  the parent-approved pinned revision.
- FR-010: CI MUST include a dry-run aarch64 GHC cache gate before expensive arm
  builds.
- FR-011: Old bespoke tarball outputs MUST have an explicit disposition in the
  PR body and implementation, based on parent decision.

## Non-Goals

- No moog runtime, oracle, agent, or Antithesis behavior changes.
- No release tag creation.
- No merge or ready-for-review transition without parent sign-off.
- No changes inside `paolino/dev-assets`; any required shared-library change is
  parent-arbitrated.

## Success Criteria

- Pull-request workflows evaluate/build the new release artifact packages.
- Local x86_64 checks build and smoke at least one Linux release artifact set.
- CI proves aarch64 evaluation and the release matrices on their native
  runners.
- A draft PR is open against `cardano-foundation/moog` with old tarball
  disposition documented.
