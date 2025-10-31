import AsciinemaEmbed from '@site/src/components/AsciinemaEmbed';

# Usage

This describes Moog's usage for a user who wants to run tests using the Antithesis platform.

Before you can request a test run, you need to register yourself as GitHub user with an SSH ed25519 public key.

## Registrations

A couple of requests have to be made once before the regular test-run requests can succeed. These requests are used to register the user and the project on the Antithesis platform.

Currently we only support GitHub as a platform, but we plan to support other platforms in the future.

Registration and unregistrations can be requested by anyone. The oracle role will simply validate the facts against the GitHub platform and update the Antithesis token accordingly. You cannot register a user or project that is already registered.

Be careful that there is no imperativity here, so i.e. you cannot unregister a user public key if it's present in GitHub.

### Registering a user public key

To register yourself as a user, you can use the `moog requester register-user` command.

It's required that you publish an ed25519 public key in your GitHub account.

<AsciinemaEmbed
  src="/moog/video/register-user.cast"
  options={{ autoplay: false, theme: 'asciinema', speed: 1.0 }}
/>

First collect your wallet `vkey`
```bash
export MYPK=$(moog wallet info --no-pretty | jq -r '.publicKey')
```

Now make sure you already have a Github repository for your user under [your-profile-repo](https://github.com/your-username/your-username) and add an `moog-cli.vkey` file in the root of the repository containing your wallet public key (the `vkey` you just collected above).

```bash
export GITHUB_USERNAME=your-github-username
```

```bash
set -e
git clone git@github.com:${GITHUB_USERNAME}/${GITHUB_USERNAME}.git
cd ${GITHUB_USERNAME}
echo $MYPK > moog-cli.vkey
git add moog-cli.vkey
git commit -m "Add Antithesis wallet vkey"
git push origin main
cd ..
rm -rf ${GITHUB_USERNAME}
```

Then register your user with the `moog requester register-user` command.
```bash
moog requester register-user --platform github --username ${GITHUB_USERNAME} --vkey $MYPK
```

As with all other requests, once submitted you have to wait for the oracle to merge your request into the Antithesis token.

You can use the `moog token` command to inspect your pending requests in the Antithesis token.

You can use the `moog facts` command to query the Antithesis token and see if your user is part of the facts.

```bash
moog token --no-pretty | jq '.requests'
```

As long as your request is listed by `moog token`, you cannot proceed with the next steps.

As with all requests to an MPFS you can retract your request using the `moog retract` command, anytime before the oracle merges it into the Antithesis token.

Get the `outputRefId` of your request from pending requests command output and use it to retract your request

```bash
moog retract -o 9ec36572e01bca9f7d32d791a5a6c529ef91c10d536f662735af26311b2c8766-0
```

Currently the oracle is not able to justify a request rejection. But moog cli will apply the oracle validation before submitting it, so rejections will be caught before submitting the request.

### Unregistering a user public key

To unregister a user, you can use the `moog requester unregister-user` command.

<AsciinemaEmbed
  src="/moog/video/unregister-user.cast"
  options={{ autoplay: false, theme: 'asciinema', speed: 1.0 }}
/>

```bash
moog requester unregister-user --platform github --username ${GITHUB_USERNAME} --vkey $MYPK
```

> Unregistering will not work if the user is correcly registered. To unregister you need to falsify the registration, i.e. remove the file `moog-cli.vkey` from your GitHub repository or change its content to a different key.

## Unregistering the legacy SSH public key.

In the past we supported SSH public keys for user registration. This is now deprecated and will be removed in the future. If you have an SSH public key registered, you can unregister it and register a new one with the wallet vkey. To do that, you have to unregister the SSH key first, then register the wallet vkey.

> Because SSH support is deprecated, unregistering the SSH key does not require the key to be absent from GitHub, so you can unregister it even if it's still present in your GitHub account. This is to ease migration to vkey and can lead to surprises as anyone can unregister your user if you used SSH key registration.

### Registering a role

This is necessary to register a user as a GitHub repository antithesis test requester.

<AsciinemaEmbed
  src="/moog/video/register-role.cast"
  options={{ autoplay: false, theme: 'asciinema', speed: 1.0 }}
/>

Before you do this make sure your repository CODEOWNERS file contains a line like this:

```
antithesis: @your-github-username
```

CODEOWNERS file can be in the root of the repository, or in the `.github` or `docs` directories.

You can have as many users as you want but registering them as test-run requesters has to be done one by one.

To register a role, you can use the `moog requester register-role` command.

```bash
moog requester register-role --platform github --username alice --repository yourorg/yourrepo
```

> Registering a role is not enough to gain rights to request test-runs. Your repository have to be white-listed by the agent. This requires you to get in contact with the agent and ask them to white-list your repository.

### Unregistering a role

```bash
moog requester unregister-role --platform github --username alice --repository yourorg/yourrepo
```

## Test-runs


### Setup

Once you are registered as a user and a role, you can request test-runs.

<AsciinemaEmbed
  src="/moog/video/test-quick-prep.cast"
  options={{ autoplay: false, theme: 'asciinema', speed: 1.0 }}
/>

Before doing that make sure you have a commit in your repository containing a directory with "valid" test assets inside.

You can obtain a set of standard test assets by running the following command:

If you are in your repository directory, you can run:

```bash
moog requester generate-assets -D ./path/to/your/test/directory
```

Once you modified them you can try to run them locally  with the `moog requester test-run` command.

```bash
moog agent test-run -D ./path/to/your/test/directory (TBD)
```

Then commit and push the changes to your repository so you will have a commit hash to use when requesting the test-run.

In general your test assets directory should contain at least a `docker-compose.yaml` file with all its images published on a public registry (like DockerHub or GHCR).
Any other asset in the test assets directory can be made available to any of your containers via a bind mount from the current working directory on the host.

i.e. if you have an asset file `myconfig.json` in the test directory, it will be available to a container if you add a volume mount like this to your `docker-compose.yaml` file:

```yaml
    volumes:
      - ./myconfig.json:/etc/myapp/config.json:ro
```

### Requesting a test-run

To request a test-run, you can use the `moog requester create-test` command.

<AsciinemaEmbed
  src="/moog/video/submit-test.cast"
  options={{ autoplay: false, theme: 'asciinema', speed: 1.0 }}
/>


```bash
moog requester create-test --platform github --username alice --repository yourorg/yourrepo --directory ./path/to/your/test/directory --commit your_commit_hash --try 1 --duration 2
```

> This command  will spit out the test-run-id (just a hash of the key) that you can use later to query the status of your test-run state.

You can request multiple test-runs for the same commit but you have to specify a different `--try` number for each request.

You can use the `--no-faults` option to disable the fault injector for this test-run.

### Checking the test-run status

You can check the status of your test-run requests with the `moog facts test-runs` command.

```bash
moog facts test-runs -i <your_test_run_id>
```

You can find all running test-runs for a user with

```bash
moog facts test-runs running --whose alice
```

The URL of the results is going to be encrypted so that only the requester will be able to decrypt it.

Decryption will happen depending on the `MOOG_WALLET_FILE` environment variable to be pointing to the requester wallet.

Old SSH registered requester will have to set up the same SSH env vars used to create the test-run.

> Careful that SSH setup will take over vkey (wallet) setup if both are setup. Remove SSH env vars once you migrate to vkey or the URL decryption will be tried via SSH.
