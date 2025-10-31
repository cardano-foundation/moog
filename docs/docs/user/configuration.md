import AsciinemaEmbed from '@site/src/components/AsciinemaEmbed';

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

Moog is a [DApp](https://en.wikipedia.org/wiki/Decentralized_application) that
allows one to request Antithesis tests execution by posting transactions to
Cardano blockchain (currently on preprod network). As such it requires a
"wallet" to pay for fees and sign transactions.

Currently the Moog CLI works only by reading a wallet file containing a mnemonic phrase.

The `moog` command will read the wallet file from the `MOOG_WALLET_FILE` environment variable.

<AsciinemaEmbed
  src="/moog/video/wallet-create.cast"
  options={{ autoplay: false, theme: 'asciinema', speed: 1.0 }}
/>

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

:::warning
Store a copy of your encrypted/plaintext wallet file in a password manager.
Think twice before storing a plaintext wallet file. Store your passphrase in a
password manager too. Currently we do not support hardware wallets like Ledger
or Trezor.
:::

Fund your wallet with some tAda tokens on preprod, for example using the
[Cardano Testnet
Faucet](https://docs.cardano.org/cardano-testnets/tools/faucet/).


#### Antithesis token

This is the unique token that identifies the (Cardano Foundation) Antithesis access interface. You need to refer to it setting the `MOOG_TOKEN_ID` environment variable.

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

The tool will query the GitHub platform on your behalf in order to obtain (public) information about users, repositories and test assets as stored into repositories.

In order to make this possible you must provide a GitHub Personal Access Token (PAT) for read-only access to public data. See the GitHub platform's documentation for how to create one.

Provide your GitHub PAT to the tool by setting an environment variable:

```bash
export MOOG_GITHUB_PAT="github_pat_31A...<snipped>...xL"
```

### Querying the token state

You can now query the state of the (Cardano Foundation) Antithesis token with the following command:

```bash
moog token
```

This will show
- the token owner a.k.a. the oracle identity
- the pending change requests
- the root of the mpf tree
- the current UTxO of the state

### Querying facts of the Antithesis token

<AsciinemaEmbed
  src="/moog/video/facts-querying.cast"
  options={{ autoplay: false, theme: 'asciinema', speed: 1.0 }}
/>


You can always query the Antithesis token and all its facts

```bash
moog facts
```

But you can also query specific facts, for example to report the GitHub registered users:

```bash
moog facts users
```

Or to query all test-run states:

```bash
moog facts test-runs
```

To report the pending test-runs:

```bash
moog facts test-runs pending
```

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

All facts come with their last-modification slot, that you can use to sort them. (Only externally via jq, currently)

```bash
moog facts test-runs | jq '[.[] | select(.key.repository.repo == "moog")] | sort_by(.slot) | last' | jq
{
  "id": "1879281b7787d6e8cfc35e322510afd3438d53720e7e5731839deae2279e2f39",
  "key": {
    "commitId": "c5ce800a2a008237df21b4927d33f8b3d953f20b",
    "directory": "compose/testnets/cardano_node_master",
    "platform": "github",
    "repository": {
      "organization": "cardano-foundation",
      "repo": "moog"
    },
    "requester": "cfhal",
    "try": 1,
    "type": "test-run"
  },
  "slot": 106050979,
  "value": {
    "duration": 1,
    "from": {
      "from": {
        "duration": 1,
        "phase": "pending",
        "signature": "68d2457749aaeafea128f2fbdb05575ee1bb2961a2b4b42dc31631bcbdb29200a073a623ceedeb5ef770e1f79a6c92c5d0c6ccb023f89e60bbae4f794dafb20f"
      },
      "phase": "accepted"
    },
    "outcome": "success",
    "phase": "finished",
    "url": "Jqt4G6cEKgOH3n0nkNcN84jb/EU26UD1qMZRZpqB1uL7Fiaqab4X8EHnj1lEajxMOxMDd5CJejGBoBag09svp9zADaqYyXgwRSJFxjLYyZDbQVZL1QhUU6jRyAT79iFXzfUkq2mdvWX4ygsRZKklzZ0OD5qqF0SqTEjO8XT3l16NFlp5sqZJbTYzaWr2zxrpkcq3TJxMmGWrnrmgFFIvxw3G8ljZy3aNBEEt0AwUmVVf/PfNN+pp36paZtD2xOMoCBiDUeaTFeJt0r5laxsgVBzPJSWyj84yTiFZg7E+0cIxgRdx7A9PRrgFOwjXb+QQKPIOihCVauZga2fOLdS2mc6E9qGiApjm4q8Nv0Ii7VNiw3NYbINpW5VJ3XhE6gYWgV/sQWzPb3/kwofVgcCMAA7I9ADVA2EqFDRX6q5vqR99VZiLZsoMsyDIgoG2Rjm0/OwTbMsaNg5A9s73kJyRsPQG+/wK5slvSS1S2BfJ+e2NVzHJtHcyqMW/SgIZ7tSttFjNiyJRJKAVeRdhxVjP7yn5kJ5iHz2pGiSJYSlowRgsQ6XIVph0ce5E8GBEUB2IG2nQPo1FvRYMmSrx/k6aWIeS"
  }
}
```
