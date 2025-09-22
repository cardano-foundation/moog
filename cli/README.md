# Antithesis CLI

## Context

This is a command line interface (CLI) to track accesses to expensive resources. In particular we are focusing on the Cardano Foundation controlled Antithesis instance. The only command relative to the specific instance  is the `anti agent run-test` command, which is used to run tests on the Antithesis platform. The rest of the commands are independent of the resource. This CLI uses MPFS as a backend meaning all data is stored on the Cardano blockchain. Currently MPFS is only supporting preprod.
The CLI should be used to book tests on the Antithesis platform. Currently it supports only projects on GitHub.

## Prerequisites

Building the CLI requires access to specific libraries from the cardano stack. Using [Nix](https://nixos.org/download.html) is the easiest way to build the code, as it will bring in all the dependencies needed to build the CLI.

Running the CLI requires an MPFS backend. You can either run your own MPFS service or use a public one. A public one is hosted at `https://mpfs.plutimus.com`.

The CLI can run on Linux and MacOS.

## Installation via tarballs

You can download the latest tarball for your platform from the [releases page](https://github.com/cardano-foundation/antithesis/releases).

## Installation via Nix

### Nix cache

Be careful to be a trusted nix user so the caches indicated in the flake will kick in. Without them expect hours of compilation time.

### Install as nix shell

To get the last version of the code, you can use the following command:

```bash
nix shell github:cardano-foundation/antithesis?dir=cli#anti
```

### Building the tarballs

On linux, you can build a nix derivation with

```bash
nix build github:cardano-foundation/antithesis?dir=cli#linux64.tarball
```

On macOS, you can build a nix derivation with

```bash
nix build .#macos64.tarball
```

## Running the CLI

### Improving CLI

You can enable bash completion for the `anti` command by adding the following line to your `.bashrc` or `.bash_profile` file:
```bash
source <(anti --bash-completion-script "$(which anti)")
```

You can have a pretty output (not valid JSON,  but easier to read) by passing `--pretty` switch or setting the `ANTI_PRETTY` environment variable to any value:
```bash
export ANTI_PRETTY=1
```

For scripting purposes you can disable the pretty effect of the env-var by passing `--no-pretty` switch.

### Environment variables and preliminaries

#### MPFS host
If you do not want to host your own MPFS service, you can use a public one at `https://mpfs.plutimus.com`.

In any case set the `ANTI_MPFS_HOST` environment variable to point to the MPFS service you want to use.

```bash
export ANTI_MPFS_HOST=https://mpfs.plutimus.com
```

#### Your wallet

Currently the anti CLI works only by reading a wallet file containing a mnemonic phrase.

The anti command will read the wallet file from the `ANTI_WALLET_FILE` environment variable.

```bash
export ANTI_WALLET_FILE=wallet.json
```

Optionally you can provide a passphrase to encrypt the mnemonic phrase in the wallet file:

> Setting a passphrase is highly recommended to protect your wallet

A less secure way to provide the passphrase is to set the `ANTI_WALLET_PASSPHRASE` environment variable:

```bash
read -s -p "Enter your wallet passphrase: " ANTI_WALLET_PASSPHRASE
export ANTI_WALLET_PASSPHRASE
```
You can create a wallet file with the `anti wallet create` command:

```bash
anti wallet create
```

A more secure way is to let the CLI prompt you for the passphrase when needed.

```bash
anti wallet create --ask-wallet-passphrase
```

If you set the `ANTI_INTERACTIVE_SECRETS` environment variable to any value, the CLI will prompt you for the passphrase every time it needs it.

```bash
export ANTI_INTERACTIVE_SECRETS=1
```

You can review this wallet info anytime with

```bash
anti wallet info
```

You can encrypt the wallet's secret (if previously you chose to store it in unencrypted way, ie., you used `anti wallet create`) using

``` bash
anti wallet encrypt path/to/encrypted/secret/file --ask-wallet-passphrase
```

Also, you can decrypt previously encrypted wallet's secret (if previously you chose to store it in encrypted way, ie., you used `anti wallet create --ask-wallet-passphrase`) using

``` bash
anti wallet decrypt path/to/decrypted/secret/file
```

For the both cases `ANTI_WALLET_FILE` is set as before.

>  Store a copy of your encrypted/plaintext wallet file in a password manager. Think twice before storing a plaintext wallet file. Store your passphrase in a password manager too. Currently we do not support hardware wallets like Ledger or Trezor.

> Fund your wallet with some tAda tokens on preprod, for example using the [Cardano Testnet Faucet](https://docs.cardano.org/cardano-testnets/tools/faucet/).


#### Antithesis token

This is the unique token that identifies the Antithesis access interface. You need to refer to it setting the `ANTI_TOKEN_ID` environment variable.

```bash
export ANTI_TOKEN_ID=21c523c3b4565f1fc1ad7e54e82ca976f60997d8e7e9946826813fabf341069b
```

#### Set the timeout for the `anti` command

When submitting txs to the chain, it's quite convenient to wait for the transaction to be included in the chain, so that you can immediately use the result of the transaction.

To do that, you can set the `ANTI_WAIT` environment variable to the number of seconds you want to wait for the transaction to be included in the chain.

```bash
export ANTI_WAIT=120
```

#### Configuring access to GitHub

The tool will query the GitHub platform on your behalf in order to obtain (public) information about repositories (where Antithesis test runs are stored) and users (requesting a test run).

In order to make this possible you must provide a GitHub Personal Access Token (PAT) for read-only access to public data. See the GitHub platform's documentation for how to create one.

Provide your GitHub PAT to the tool by setting an environment variable:

```bash
export ANTI_GITHUB_PAT="github_pat_31A...<snipped>...xL"
```

### Querying the token state

You can query the state of the Antithesis token with the following command:

```bash
anti token
```

This will show
- the token owner a.k.a. the oracle identity
- the pending change requests
- the root of the mpf tree
- the current UTxO of the state

### Querying facts of the Antithesis token

You can always query the Antithesis token and its facts

```bash
anti facts
```

Will query all facts

But you can also query specific facts, for example:

```bash
anti facts users
```
will report the GitHub registered users.

Or

```bash
anti facts test-runs
```
will report all the test runs.


```bash
anti facts test-runs pending
```
will report the pending test runs.

You can also query facts for a specific test-run id:

```bash
anti facts test-runs -i id1 -i id2 ..
```

This is useful if you stored the test-run id when you created the test-run.
Test-run ids are facts ids so you can also look them up via `anti facts`

Finally

```bash
anti facts --help
```
will show you all the available facts you can query.



## Design

[Interface Design document](docs/antithesis-interface.md)
[Code Design](docs/code-design.md)

## Manuals

Depending on your role you can access the different manuals.


- [Requester manual](docs/requester-role.md). A test-run requester is the regular user of the system. They can register users and roles, and request test-runs.
- [Agent manual](docs/antithesis-agent-role.md). This is a special role that holds the key to the Antithesis platform. It is responsible  for
  whitelisting repository and managing test-runs, from pending state to running and completed.
- [Oracle manual](docs/oracle-role.md). The oracle is the owner of the Antithesis token. It is almost mechanically responsible for merging change requests from the agent and the requester.
- [Secrets management manual](docs/secrets-management.md). This manual describes how to manage secrets in a secure way.
Finally, the [Real-world scenario manual][realWorld] provides a realistic overall use case involving all three roles.

[realWorld]: docs/real-world.md
