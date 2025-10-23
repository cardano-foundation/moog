# Moog

## Context

This is a command line interface (CLI) to track accesses to expensive resources. In particular we are focusing on the Cardano Foundation controlled Antithesis instance. The only command relative to the specific instance  is the `moog agent run-test` command, which is used to run tests on the Antithesis platform. The rest of the commands are independent of the resource. This CLI uses MPFS as a backend meaning all data is stored on the Cardano blockchain. Currently MPFS is only supporting preprod.
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
nix shell github:cardano-foundation/antithesis?dir=cli#moog
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

You can enable bash completion for the `moog` command by adding the following line to your `.bashrc` or `.bash_profile` file:
```bash
source <(moog --bash-completion-script "$(which moog)")
```

You can have a pretty output (not valid JSON,  but easier to read) by passing `--pretty` switch or setting the `MOOG_PRETTY` environment variable to any value:
```bash
export MOOG_PRETTY=1
```

For scripting purposes you can disable the pretty effect of the env-var by passing `--no-pretty` switch.

### Environment variables and preliminaries

#### MPFS host
If you do not want to host your own MPFS service, you can use a public one at `https://mpfs.plutimus.com`.

In any case set the `MOOG_MPFS_HOST` environment variable to point to the MPFS service you want to use.

```bash
export MOOG_MPFS_HOST=https://mpfs.plutimus.com
```

#### Your wallet

Currently the moog CLI works only by reading a wallet file containing a mnemonic phrase.

The moog command will read the wallet file from the `MOOG_WALLET_FILE` environment variable.

```bash
export MOOG_WALLET_FILE=wallet.json
```

Optionally you can provide a passphrase to encrypt the mnemonic phrase in the wallet file:

> Setting a passphrase is highly recommended to protect your wallet

A less secure way to provide the passphrase is to set the `MOOG_WALLET_PASSPHRASE` environment variable:

```bash
read -s -p "Enter your wallet passphrase: " MOOG_WALLET_PASSPHRASE
export MOOG_WALLET_PASSPHRASE
```
You can create a wallet file with the `moog wallet create` command:

```bash
moog wallet create
```

A more secure way is to let the CLI prompt you for the passphrase when needed.

```bash
moog wallet create --ask-wallet-passphrase
```

If you set the `MOOG_INTERACTIVE_SECRETS` environment variable to any value, the CLI will prompt you for the passphrase every time it needs it.

```bash
export MOOG_INTERACTIVE_SECRETS=1
```

You can review this wallet info anytime with

```bash
moog wallet info
```

You can encrypt the wallet's secret (if previously you chose to store it in unencrypted way, ie., you used `moog wallet create`) using

``` bash
moog wallet encrypt path/to/encrypted/secret/file --ask-wallet-passphrase
```

Also, you can decrypt previously encrypted wallet's secret (if previously you chose to store it in encrypted way, ie., you used `moog wallet create --ask-wallet-passphrase`) using

``` bash
moog wallet decrypt path/to/decrypted/secret/file
```

For the both cases `MOOG_WALLET_FILE` is set as before.

>  Store a copy of your encrypted/plaintext wallet file in a password manager. Think twice before storing a plaintext wallet file. Store your passphrase in a password manager too. Currently we do not support hardware wallets like Ledger or Trezor.

> Fund your wallet with some tAda tokens on preprod, for example using the [Cardano Testnet Faucet](https://docs.cardano.org/cardano-testnets/tools/faucet/).


#### Antithesis token

This is the unique token that identifies the Antithesis access interface. You need to refer to it setting the `MOOG_TOKEN_ID` environment variable.

```bash
export MOOG_TOKEN_ID=21c523c3b4565f1fc1ad7e54e82ca976f60997d8e7e9946826813fabf341069b
```

#### Set the timeout for the `moog` command

When submitting txs to the chain, it's quite convenient to wait for the transaction to be included in the chain, so that you can immediately use the result of the transaction.

To do that, you can set the `MOOG_WAIT` environment variable to the number of seconds you want to wait for the transaction to be included in the chain.

```bash
export MOOG_WAIT=120
```

#### Configuring access to GitHub

The tool will query the GitHub platform on your behalf in order to obtain (public) information about repositories (where Antithesis test runs are stored) and users (requesting a test run).

In order to make this possible you must provide a GitHub Personal Access Token (PAT) for read-only access to public data. See the GitHub platform's documentation for how to create one.

Provide your GitHub PAT to the tool by setting an environment variable:

```bash
export MOOG_GITHUB_PAT="github_pat_31A...<snipped>...xL"
```

### Querying the token state

You can query the state of the Antithesis token with the following command:

```bash
moog token
```

This will show
- the token owner a.k.a. the oracle identity
- the pending change requests
- the root of the mpf tree
- the current UTxO of the state

### Querying facts of the Antithesis token

You can always query the Antithesis token and its facts

```bash
moog facts
```

Will query all facts

But you can also query specific facts, for example:

```bash
moog facts users
```
will report the GitHub registered users.

Or

```bash
moog facts test-runs
```
will report all the test runs.


```bash
moog facts test-runs pending
```
will report the pending test runs.

You can also query facts for a specific test-run id:

```bash
moog facts test-runs -i id1 -i id2 ..
```

This is useful if you stored the test-run id when you created the test-run.
Test-run ids are facts ids so you can also look them up via `moog facts`

Finally

```bash
moog facts --help
```
will show you all the available facts you can query.
