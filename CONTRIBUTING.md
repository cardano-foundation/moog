
# Contributing to Moog

Thank you for considering to contribute to Moog and help the Cardano community
improve the security of the network!

We especially welcome help improving the documentation and packaging.

The preferred way of contributing is through our [GitHub repository][Repo], by
opening issues or submitting pull requests.

Other ways to get in touch are provided on the [Moog website][Moog].

We also maintain a repository where we develop containers as test assets to run
a cardano-network in Antithesis. [cardano-node-antithesis][cardano-node-antithesis]

# Contributing to the Moog CLI

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

[Moog]: https://cardano-foundation.github.io/moog
[Repo]: https://github.com/cardano-foundation/moog
[Nix]: https://nixos.org/download.html
[prereq]: https://developers.cardano.org/docs/operate-a-stake-pool/node-operations/installing-cardano-node/#building-via-cabal
[cardano-node-antithesis]: https://github.com/cardano-foundation/cardano-node-antithesis
