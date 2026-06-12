!!! warning
    Moog is still at an early stage and experimental. Contributions and ideas are [most welcome](https://github.com/cardano-foundation/moog/).

!!! info "MPFS v2 migration in progress"
    moog is migrating from the legacy MPFS HTTP API to the new facts-only MPFS
    service ([cardano-mpfs-offchain](https://github.com/lambdasistemi/cardano-mpfs-offchain)),
    where the server returns indexed *facts* and the client builds, signs, and
    submits transactions. Current releases (**v0.5.1.5** at the time of
    writing) run against the **legacy** MPFS server in production; the oracle
    `token boot` / `token end` commands were removed in v0.5.1.3 because they
    already targeted the new API, which production does not yet serve
    ([#144](https://github.com/cardano-foundation/moog/issues/144)). The full
    client-side facts cutover is developed on the long-lived
    [`moog-v2`](https://github.com/cardano-foundation/moog/tree/moog-v2)
    branch.

# Moog

**Testing Cardano on Cardano with Antithesis**

Moog is a portal to the [Antithesis](https://antithesis.com/) service. It makes
it possible for an organisation that has an Antithesis licence to provide to
other users the opportunity to submit their own Antithesis tests, which will
then be run by the organisation that has access to Antithesis. The details of
this transaction is then stored on the Cardano blockchain.

## Getting Started

We call ***users*** of Moog the people who wish to submit requests for an
Antithesis test to be run. They should read the [User Manual](user/intro.md).

We call ***operators*** of Moog the people who are providing a portal to their
Antithesis instance. This involves setting up some infrastructure, running
automated services, as well as having human agents performing occasional tasks.
This is all described in the [Operations Manual](ops/intro.md).

## About

In particular, the [Cardano Foundation](https://cardanofoundation.org/) is
currently operating the prototype implementation of a Moog portal to allow
developers within the Cardano ecosystem to use run Antithesis tests, in an
effort to contribute to improving the quality of software within the Cardano
ecosystem.
