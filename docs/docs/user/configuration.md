# Configuration

## Prerequisites

You must have previously [installed](installation.md) Moog's CLI.

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

