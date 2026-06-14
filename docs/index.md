!!! warning
    Moog is still at an early stage and experimental. Contributions and ideas are [most welcome](https://github.com/cardano-foundation/moog/).

!!! info "MPFS v2 migration: client shipped (v2.0.0), infrastructure still 0.5.1.x"
    moog has migrated its client off the legacy MPFS HTTP API to the new
    facts-only MPFS service
    ([cardano-mpfs-offchain](https://github.com/lambdasistemi/cardano-mpfs-offchain)),
    where the server returns indexed *facts* and the client builds, signs, and
    submits transactions. This cutover shipped as **v2.0.0** — a clean major
    break from the 0.5.x line — off the long-lived **`moog-v2`** branch.

    **The deployed infrastructure is still at 0.5.1.x.** The production MPFS
    server and the running oracle/agent services have **not** yet been upgraded
    to the facts-only API, so the **v2.0.0** client targets a backend production
    does not yet serve. Keep the **0.5.1.x** releases for anything running
    against the live deployment until the infrastructure is upgraded.
    **`main` is frozen during the migration.**

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
