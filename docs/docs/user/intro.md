# Introduction

Users of Moog submit requests for an Antithesis test involving Cardano nodes to
be run.

Moog involves several components:

- a command-line-interface (CLI) tool with which users make requests;
- a service (hosted by [Cardano Foundation](https://cardanofoundation.org/))
  called the Oracle which knows how to query GitHub and has access to the
  Cardano Foundation's Antithesis account;
- a service (hosted by Cardano Foundation) called MPFS which stores the facts
  produced by and for Moog on the Cardano blockchain;
- the [Cardano blockchain](https://cardano.org/);
- [Antithesis](https://antithesis.com/).

Most users will only [install](installation.md) and [configure](configuration.md) the
CLI tool, then [use](usage.md) it to submit test run requests.

