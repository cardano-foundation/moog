# Introduction

Moog provides a command-line-interface (CLI) tool which is used to interact
with the Moog system.

This user manual explains how to:

- [Install](installation.md) the CLI tool.
- [Configure](configuration.md) your CLI tool, in particular so that it
  interacts with your chosen Moog portal.
- [Use](usage.md) Moog to submit Antithesis test run requests.

!!! info "The MPFS service is untrusted"
    Moog verifies every response from the MPFS service client-side against a
    trusted, on-chain-anchored Merkle root, so you do not have to trust the
    service to be honest about the system state. See
    [Verification](configuration.md#verification-you-do-not-have-to-trust-the-mpfs-service)
    in Configuration.
