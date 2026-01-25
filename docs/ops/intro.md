# Introduction

This operations manual explains how to create and operate a Moog portal. 

Operating Moog requires:

- having an [Antithesis](https://antithesis.com/) account in which to execute
  test runs
- running several services
  * an oracle process, which represents the specific portal, and knows how to query
    GitHub in order to verify claims made by users
  * an agent process, which forward test-runs to Antithesis platform and report results back
  * an MPFS service which stores the details about the transactions between
    users and the portal on the [Cardano](https://cardano.org/) blockchain
    ([MPFS](https://cardano-foundation.github.io/mpfs) is the Merkle Patricia
    Forestry Service, an HTTP service which manages, through smart contracts, a
    data structure called a Merkle Patricia Forestry on the blockchain).
- interventions by a human agent, for example to whitelist the GitHub
  repository of a new user.

The details of how to do all this are described in the rest of the Operations
Manual.

Moog infrastructure is currently operated by [the Cardano
Foundation](https://cardanofoundation.org/) while the project is still in its
experimental phase. Those instructions are however useful for anyone interested
in operating Moog.
