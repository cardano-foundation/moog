# Antithesis CLI

## Building the code

### Nix

To build the code, you can use [Nix](https://nixos.org/download.html) installed.

```bash
nix shell .#anti
```

## Targeting the MPFS service

If you do not want to host your own MPFS service, you can use a public one at `https://mpfs.plutimus.com`.

In any case set the `ANTI_MPFS_HOST` environment variable to point to the MPFS service you want to use.

```bash
export ANTI_MPFS_HOST=https://mpfs.plutimus.com
```

## Providing your wallet to the `anti` command

ATM the anti CLI works only by reading a wallet file containing a mnemonic phrase.

The anti command will read the wallet file from the `ANTI_WALLET_FILE` environment variable.

```bash
export ANTI_WALLET_FILE=tmp/my-wallet.json
```

You can create a wallet file with the `anti wallet create` command:

```bash
anti wallet create
```

It will fail to re-create the file if it already exists. You can review this wallet info anytime with

```bash
anti wallet info
```

Get the wallet address with:

```bash
anti wallet info | jq -r .result.address
```

Remember the owner of the wallet with

```bash
export OWNER=$(anti wallet info | jq -r .result.owner)
```

Fund your wallet with some tAda tokens on preprod, for example using the [Cardano Testnet Faucet](https://docs.cardano.org/cardano-testnets/tools/faucet/).

## Set the timeout for the `anti` command

When submitting txs to the chain, it's quite convenient to wait for the transaction to be included in the chain, so that you can immediately use the result of the transaction.

To do that, you can set the `ANTI_WAIT` environment variable to the number of seconds you want to wait for the transaction to be included in the chain.

## Requester role

This is the role of the user that wants to run tests using the Antithesis platform.

### Antithesis token

This is the unique token that identifies the Antithesis access interface. You need to refer to it setting the `ANTI_TOKEN_ID` environment variable.

```bash
export ANTI_TOKEN_ID=865ebcf5e1d6bafcc121030a6e167474a426271d965b78e36d90485adf540575
```

Before you can request a test run, you need to register yourself as github user with an ed25519 public key.

### Registering a user

To register yourself as a user, you can use the `anti requester register-user` command.

```bash
anti requester register-user --platform github --username alice --pubkeyhash AAAAC3NzaC1lZDI1NTE5AAAAIO773JHqlyLm5XzOjSe+Q5yFJyLFuMLL6+n63t4t7HR8
```

As with all other requests, once submitted regularly you have to wait for the oracle to merge your request into the Antithesis token.
You can use the `anti oracle token get` command to inspect your pending requests in the Antithesis token.

```bash
anti oracle token get | jq '.result.requests' | jq ".[] | select(.owner == \"${OWNER}\")"
```

Until your requests is there, you cannot proceed with the next steps.

As with all requests to an mpfs you can retract your request using the `anti retract` command, anytime before the oracle merges it into the Antithesis token.

Get the `outputRefId` of your request from pending requests command output and use it to retract your request

```bash
anti retract -o 9ec36572e01bca9f7d32d791a5a6c529ef91c10d536f662735af26311b2c8766-0
```

### Unregistering a user

To unregister yourself as a user, you can use the `anti requester unregister-user` command.

```bash
anti requester unregister-user --platform github --username alice --pubkeyhash AAAAC3NzaC1lZDI1NTE5AAAAIO773JHqlyLm5XzOjSe+Q5yFJyLFuMLL6+n63t4t7HR8
```

## Oracle role

This is the role of the user that wants to run an oracle service for the Antithesis platform. There will be only one token and so there will be only one oracle service running at a time, but we document it here for completeness.

### Creating the anti token

To create the Antithesis token, you can use the `anti oracle token create` command.

```bash
anti oracle token create
```

It will create the Antithesis token. This token is a unique identifier for the Antithesis platform and will be used by all users to interact with the platform. You have to ditribute it so that users can set the `ANTI_TOKEN_ID` environment variable to point to it.

To test the role change the ANTI_TOKEN_ID environment variable to the new create token id

You can review the token info anytime with

```bash
anti oracle token get
```

### Updating the anti token

Updating the token with new requests is done with the `anti oracle token update` command. As with retract you have to provide the `outputRefId` of the request you want to update. Multiple requests can be updated at once, so you can provide multiple `-o` options.

```bash
anti oracle token update -o b6fc7cca5bcae74e6a5983f7922d0e0985285f1f19e62ccc9cb9fd4d3766a81b-0
```

### Deleting the anti token

To delete the Antithesis token, you can use the `anti oracle token delete` command.

```bash
anti oracle token delete
```

## Antithesis Agent Role

TBD
