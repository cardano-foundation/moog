# Contributing to the Moog CLI

Instructions to obtain a copy of the repository are in the top-level
[CONTRIBUTING.md][../CONTRIBUTING.md]

## Building the Moog CLI from source

There are two ways to build from source:

1. The bare metal build, after installing GHC and some libraries
2. Using Nix

### Bare metal build

If you have built the cardano-node from source, you have everything you need to
build Moog.

If not, you must first follow [these instructions][prereq] up until they say to
download the source code for cardano-node (which you don't need to do).

Then, go from the top-level directory of your cloned Moog and enter:

```
cd cli
cabal update
cabal build
cabal run moog -- --help
```

If this prints the moog help instructions to your terminal, you have successfully
built the CLI.

Additionally, it is recommended to run the unit tests:

```
just unit
```

### Nix build

Alternatively, if you don't want or cannot install required libraries, you can
use Nix.

If you have not already, install [Nix][Nix] itself.

We recommend configuring Nix so that you are a "trusted user", which allows Nix
to download and use cached binaries. Without this, expect hours of compilation
time.

To build the local source using Nix:

```
nix develop
cabal build
```

If you are a Nix user and want to run the CLI directly from the official
repository, without cloning the repository:

```bash
nix shell github:cardano-foundation/moog?dir=cli#moog
```


<!-- MARKDOWN LINKS & IMAGES -->

[Nix]: https://nixos.org/download.html
[prereq]: https://developers.cardano.org/docs/operate-a-stake-pool/node-operations/installing-cardano-node/#building-via-cabal
