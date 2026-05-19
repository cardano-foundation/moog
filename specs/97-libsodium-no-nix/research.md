# Research — Repair libsodium fork install in no-Nix build

- **Issue:** [#97](https://github.com/cardano-foundation/moog/issues/97)
- **Spec:** [`spec.md`](spec.md)
- **Phase:** Research

## Failure under investigation

```
Downloading config.guess and config.sub...
Done.
configure: error: cannot run /bin/bash ./build-aux/config.sub
```

From [run 26026528215](https://github.com/cardano-foundation/moog/actions/runs/26026528215/job/76501180598).
Timing in the failed run: `Downloading config.guess and config.sub...`
prints at `10:04:26`, `Done.` at `10:07:27` — a ~3-minute stretch with
no further progress.

## Root cause

`intersectmbo/libsodium`'s `autogen.sh` (at the failing revision
`dbb48cce5429cb6585c9034f002568964f1ce567`) unconditionally tries to
*overwrite* the in-tree `build-aux/config.{guess,sub}` by downloading
fresh copies from `git.savannah.gnu.org`'s gitweb CGI:

```sh
[ -z "$DO_NOT_UPDATE_CONFIG_SCRIPTS" ] &&
  command -v curl >/dev/null 2>&1 && {
  echo "Downloading config.guess and config.sub..."

  curl -sL -o config.guess \
    'https://git.savannah.gnu.org/gitweb/?p=config.git;a=blob_plain;f=config.guess;hb=HEAD' &&
    mv -f config.guess build-aux/config.guess

  curl -sL -o config.sub \
    'https://git.savannah.gnu.org/gitweb/?p=config.git;a=blob_plain;f=config.sub;hb=HEAD' &&
    mv -f config.sub build-aux/config.sub

  echo "Done."
}
```

Two things make this brittle:

1. `git.savannah.gnu.org/gitweb` is a CGI endpoint with no SLA. When
   it is slow or returning 5xx, `curl` returns control quickly but
   the script never observes the failure.
2. `curl -sL` runs **without `--fail`**, so HTTP errors are silently
   written to the output file. An HTML error page lands at
   `build-aux/config.sub`, and `configure` then fails the moment it
   tries to execute it: `cannot run /bin/bash ./build-aux/config.sub`.

The 3-minute "Downloading…" stretch in the failed run is consistent
with savannah's gitweb timing out or returning a slow error page.

## Mitigation already built into `autogen.sh`

The very first line of the download block is a documented escape
hatch:

```sh
[ -z "$DO_NOT_UPDATE_CONFIG_SCRIPTS" ] && ...
```

Setting `DO_NOT_UPDATE_CONFIG_SCRIPTS` to any non-empty value short-
circuits the curl block entirely. `configure` then runs against the
in-tree `build-aux/config.{guess,sub}` that the fork already commits:

```
build-aux/config.guess  49938
build-aux/config.sub    35819
depcomp                 23568
install-sh              15358
ltmain.sh              333035
missing                  6878
test-driver              4879
```

(Listing of `build-aux/` at the failing revision; sizes are bytes.)

These are the canonical autoconf helper scripts and are correct for
`ubuntu-latest` x86_64. They are only stale in the sense that they
do not know about architectures introduced after the fork pin; for
the supported runner, they are fine.

## Options considered

### Option A — Set `DO_NOT_UPDATE_CONFIG_SCRIPTS=1` in the workflow step *(recommended)*

Add `DO_NOT_UPDATE_CONFIG_SCRIPTS=1` to the `Install libsodium fork`
step (either as `env:` on the step or inline before `./autogen.sh`).
The committed `build-aux/config.{guess,sub}` are used; the curl
block is skipped.

- **Pros:** one-line workflow change; uses an escape hatch *documented
  in the fork's own `autogen.sh`*; eliminates the dependency on
  `git.savannah.gnu.org` for this step; deterministic; cache key
  semantics unchanged.
- **Cons:** if `intersectmbo/libsodium` ever bumps to a runner arch
  not covered by the committed `config.sub`, we would get a clear
  "unsupported arch" error and would need to bump the iohk-nix sodium
  pin (which is normal practice) or override `config.sub` once. No
  current evidence of this for `ubuntu-latest`.
- **Effect on FR3:** fork revision pin via iohk-nix is unchanged.
- **Effect on FR2:** removes the cache-miss dependency on a flaky
  third-party endpoint. The `~/.local` cache remains a speed
  optimization only.

### Option B — Pin to a release tarball of the fork

Download a release tarball that ships pre-generated `configure` and
skip `./autogen.sh` entirely.

- **Rejected:** `intersectmbo/libsodium` has **no GitHub releases and
  no tags** (only branches like `master`, `stable`,
  `draft-irtf-cfrg-vrf-03`). There is no tarball pinned to the
  iohk-nix-resolved revision; building one ourselves would mean
  forking the fork.

### Option C — Patch the curl call with `--fail` + retries

Wrap `curl` to fail loudly on HTTP errors and retry with backoff,
falling back to the in-tree files when savannah is down.

- **Rejected:** moves the failure mode from "silent corrupt file"
  to "slow noisy retries"; still depends on a flaky upstream;
  duplicates what Option A already gives us for free; requires
  patching upstream `autogen.sh` (out of scope) or wrapping the
  invocation with shell, increasing the surface we maintain.

### Option D — Manually overwrite `build-aux/config.{guess,sub}` with the apt-provided ones

After `git checkout`, copy `/usr/share/automake-*/config.{guess,sub}`
over the in-tree files before running `./autogen.sh`.

- **Rejected:** the autogen.sh download block runs *after*
  `autoreconf -ivf`, so any pre-copy we do is immediately overwritten
  unless we *also* set `DO_NOT_UPDATE_CONFIG_SCRIPTS`. At that point
  Option A is strictly simpler — the in-tree files are already
  Cardano-blessed via the fork pin.

### Option E — Patch the fork upstream

Open a PR on `intersectmbo/libsodium` to use `curl --fail` or
remove the download block.

- **Rejected for this ticket:** out of scope per the issue's
  non-goals ("Do not migrate MOOG away from the Cardano libsodium
  fork"). Would also unblock far more than just MOOG; better as a
  separate follow-up. The supported escape hatch (Option A) is
  available *today* without coordination.

## Recommendation

**Adopt Option A.** Surgical, one-line workflow change; uses the
fork's own documented escape hatch; eliminates the savannah CGI
dependency from MOOG's CI without changing the fork pin, the cache
contract, or any Nix path.

The implementation slice is small enough to live in one
bisect-safe commit:

- Set `DO_NOT_UPDATE_CONFIG_SCRIPTS: 1` on the `Install libsodium
  fork` step (or `export` it inline before `./autogen.sh`).
- Inline an explanatory comment so the next person to touch this
  step knows *why* the env var is set.

Live proof for FR1/FR3/FR5 is a `Build moog without nix`
`workflow_dispatch` run on this branch with the `~/.local` cache
invalidated, attached to the PR before marking ready.

## Cross-cutting observations (out of scope, noted only)

- `cardano-foundation/developer-portal`'s `installing-cardano-node.md`
  uses the same unguarded `./autogen.sh && ./configure` pattern and
  is vulnerable to the identical failure. A follow-up PR on the
  developer-portal repo could apply the same env var. Not in scope
  here; flagged so the find does not get lost.
- The fork's `secp256k1` and `blst` steps in MOOG's workflow do not
  download `config.{guess,sub}` from savannah, so they are not
  affected. The plan does not touch them (per spec exclusions).

## Sources

- `intersectmbo/libsodium` at
  `dbb48cce5429cb6585c9034f002568964f1ce567`:
  - [`autogen.sh`](https://github.com/intersectmbo/libsodium/blob/dbb48cce5429cb6585c9034f002568964f1ce567/autogen.sh)
  - [`build-aux/`](https://github.com/intersectmbo/libsodium/tree/dbb48cce5429cb6585c9034f002568964f1ce567/build-aux)
- MOOG failing run:
  <https://github.com/cardano-foundation/moog/actions/runs/26026528215/job/76501180598>
- `intersectmbo/libsodium` releases / tags: none.
- `cardano-foundation/developer-portal` install doc: same
  unguarded pattern in `installing-cardano-node.md`.
