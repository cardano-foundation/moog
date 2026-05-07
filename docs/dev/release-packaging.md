# Release Packaging

Moog currently has two packaging paths.

The existing release path builds platform tarballs from the flake outputs and
uploads them to a GitHub release:

```bash
nix build .#linux64.tarball
nix build .#linux-aarch64.tarball
nix build .#darwin64.tarball
```

Those outputs are still the release contract used by `CI/release.sh` and the
`release-linux` / `release-macos` just recipes.

The macOS Homebrew spike adds two Darwin-only flake outputs:

```bash
nix build .#darwin-release-artifacts
nix build .#darwin-dev-homebrew-artifacts
```

Both are built with `paolino/dev-assets` through
`lib.mkDarwinHomebrewBundle`. The output directory contains:

- a relocatable `aarch64-darwin` tarball with the release executables under
  `bin/`: `moog`, `moog-oracle`, and `moog-agent`
- a generated Homebrew formula
- `SHA256SUMS`
- `release-metadata.json`

`darwin-release-artifacts` produces `moog.rb` and uses the Cabal package
version as the Homebrew version. `darwin-dev-homebrew-artifacts` produces
`moog-dev.rb`, embeds the source revision in the artifact and formula version,
and points the formula at the moving `dev-homebrew` release tag.

The release executable set is defined once in `nix/moog-project.nix` as
`releaseExecutables`. Linux tarballs, macOS tarballs, and Homebrew artifacts
all consume that set so a new first-party executable is not exposed on only one
platform by accident.

## Pull Request Verification

The existing `Build tarballs` workflow runs on pull requests. It keeps the
legacy Linux and macOS tarball jobs and adds a PR-only
`macos-homebrew-artifacts` job.

That job invokes the shared action:

```yaml
uses: paolino/dev-assets/darwin-homebrew-release@spike/darwin-homebrew-action
```

In pull requests the action is intentionally local-only:

- it builds `.#darwin-dev-homebrew-artifacts`
- it extracts the tarball and checks the listed commands exist
- it runs Moog's tarball smoke commands
- it copies the generated formula into a local tap checkout
- it rewrites the formula URL to `file://<built-tarball>`
- it runs `brew install`, `brew test`, and Moog's brew smoke commands
- it uploads the generated tarball and formula as workflow artifacts
- it does not create GitHub releases or push tap commits

Moog does not currently have a visible `cardano-foundation/homebrew-tap`
repository. During the spike the PR job uses `cardano-foundation/moog` as a
temporary local tap source only. This is enough to prove the generated formula
and Homebrew install path before merge. Before enabling publication, replace the
workflow inputs with the real tap name and tap repository.

## Release Impact

Merging the spike as-is does not change the current release command:

```bash
just release-macos
```

That command still builds `.#darwin64.tarball` and uploads the legacy tarball
asset. The new Homebrew outputs are additive.

To turn the spike into the release path:

1. Merge or pin the shared `paolino/dev-assets` action and Nix helper.
2. Create or identify the Cardano Foundation Homebrew tap repository.
3. Change `tap-name` and `tap-repository` in `tarballs.yaml` to that tap.
4. Replace the spike action ref with `@main` or a pinned commit SHA.
5. Add a controlled dispatch or tag publishing path with a tap write token.
