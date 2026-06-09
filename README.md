![](/assets/tartarus.jpeg)

<!-- Badges -->
[![Unit Tests](https://github.com/cardano-foundation/moog/actions/workflows/unit-tests.yaml/badge.svg)](https://github.com/cardano-foundation/moog/actions/workflows/unit-tests.yaml)
[![Integration Tests](https://github.com/cardano-foundation/moog/actions/workflows/integration-tests.yaml/badge.svg)](https://github.com/cardano-foundation/moog/actions/workflows/integration-tests.yaml)
[![E2E Tests](https://github.com/cardano-foundation/moog/actions/workflows/E2E-test.yaml/badge.svg)](https://github.com/cardano-foundation/moog/actions/workflows/E2E-test.yaml)
[![Publish Docs](https://github.com/cardano-foundation/moog/actions/workflows/publish-site.yaml/badge.svg)](https://github.com/cardano-foundation/moog/actions/workflows/publish-site.yaml)
[![Linux Release](https://github.com/cardano-foundation/moog/actions/workflows/release.yml/badge.svg)](https://github.com/cardano-foundation/moog/actions/workflows/release.yml)
[![Darwin Release](https://github.com/cardano-foundation/moog/actions/workflows/darwin-release.yml/badge.svg)](https://github.com/cardano-foundation/moog/actions/workflows/darwin-release.yml)
[![Build docker images](https://github.com/cardano-foundation/moog/actions/workflows/docker-images.yaml/badge.svg)](https://github.com/cardano-foundation/moog/actions/workflows/docker-images.yaml)
# Moog

> **⚠️ Project state — MPFS v2 migration in progress.**
> moog is migrating from the legacy MPFS HTTP API to the new facts-only MPFS
> service ([cardano-mpfs-offchain](https://github.com/lambdasistemi/cardano-mpfs-offchain)),
> where the server returns indexed *facts* and the client builds, signs, and
> submits transactions. The current release (**v0.5.1.4**) runs against the
> **legacy** MPFS server in production; the oracle `token boot` / `token end`
> commands were removed because they already targeted the new API, which
> production does not yet serve. The full client-side facts cutover is happening
> on a long-lived **`moog-v2`** branch and will land as a single coordinated
> release. **`main` is frozen during the migration** — see
> [#144](https://github.com/cardano-foundation/moog/issues/144).

Moog is for Cardano network components testing with Antithesis.

## Overview

The [Cardano blockchain's][Cardano] core node software implements complex
algorithms and protocols who run in a networked, concurrent context. Many
nodes, some of which could be adversarial, collaborate to achieve the overall
system's behaviour of being a decentralized, immutable ledger. Such a system is
subject to many unpredictable factors such as communication delays, network
partitions, nodes appearing or disappearing.

[Antithesis][Antithesis] is a testing tool which is capable of generating
random events (such as communication delays, network partitions, nodes
appearing or disappearing) in a simulated environment, such that if any
specific random sequence of combination of events leads to a software error, it
can be reproduced.

Moog aims to facilitate the use of Antithesis for testing various
components of the Cardano ecosystem, while tracking these test efforts on the
blockchain.

## Pre-requisites

Moog is intended for Cardano developers and builders. Familiarity with the
Cardano network, its tools and ecosystem is required.

Moog runs on MacOS and Linux.

Before using Moog there are several preliminary steps, including creating
a wallet and registering yourself as a user. These are described in the
user manual.

## Getting started

Instructions for end-users are available on the [Moog website][Moog].

Instructions for contributors and developers are in
[Contributing](./CONTRIBUTING.md)

## Usage

Moog is used via a command-line-interface (CLI). If you have followed the
"Getting started" instructions you can check you have a working system by
invoking:

```
moog --help
```

This should print out Moog's inline help.

Further instructions for setting up and using Moog are in the user manuals
available on the [Moog webiste][Moog].

## See Also

- Other projects by [HAL][HAL]
- Other projects by the [Cardano Foundation][CF]
- About [Cardano][Cardano]

<!-- MARKDOWN LINKS & IMAGES -->

[Moog]: https://cardano-foundation.github.io/moog
[Antithesis]: https://antithesis.com
[HAL]: https://github.com/cardano-foundation/hal
[CF]: https://github.com/cardano-foundation
[Cardano]: https://cardano.org/
