# Requester role

This is the role of the user who wants to run tests using the Antithesis platform.

Before you can request a test run, you need to register yourself as GitHub user with an SSH ed25519 public key.

## Registrations

A couple of requests have to be made once before the regular test-run requests can succeed. These requests are used to register the user and the project on the Antithesis platform.

Currently we only support GitHub as a platform, but we plan to support other platforms in the future.

Registration and unregistrations can be requested by anyone. The oracle role will simply validate the facts against the GitHub platform and update the Antithesis token accordingly. You cannot register a user or project that is already registered.

Be careful that there is no imperativity here, so i.e. you cannot unregister a user public key if it's present in GitHub.

### Registering a user public key

To register yourself as a user, you can use the `anti requester register-user` command.

You must provide (using the incorrectly-named `pubkeyhash` flag) the actual (base64-encoded) ed15519 public key (without the ssh-ed25519 prefix and without the optional key-id). So, for example, if your GitHub username is `alice` and you have configured this public key in your GitHub account:

```bash
$ cat ~/.ssh/id_ed25519.pub
ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO773JHqlyLm5XzOjSe+Q5yFJyLFuMLL6+n63t4t7HR8 myname@myprovider.com
```

then you should copy/paste the second field into the following command:

```bash
anti requester register-user --platform github --username alice --pubkeyhash AAAAC3NzaC1lZDI1NTE5AAAAIO773JHqlyLm5XzOjSe+Q5yFJyLFuMLL6+n63t4t7HR8
```

As with all other requests, once submitted you have to wait for the oracle to merge your request into the Antithesis token.

You can use the `anti token` command to inspect your pending requests in the Antithesis token.

You can use the `anti facts` command to query the Antithesis token and see if your user is part of the facts.

```bash
anti token --no-pretty | jq '.requests'
```

As long as your request is listed by `anti token`, you cannot proceed with the next steps.

As with all requests to an MPFS you can retract your request using the `anti retract` command, anytime before the oracle merges it into the Antithesis token.

Get the `outputRefId` of your request from pending requests command output and use it to retract your request

```bash
anti retract -o 9ec36572e01bca9f7d32d791a5a6c529ef91c10d536f662735af26311b2c8766-0
```
Currently the oracle is not able to justify a request rejection. But anti cli will apply the oracle validation before submitting it, so rejections will be caught before submitting the request.

### Unregistering a user public key

To unregister a user, you can use the `anti requester unregister-user` command.

```bash
anti requester unregister-user --platform github --username alice --pubkeyhash AAAAC3NzaC1lZDI1NTE5AAAAIO773JHqlyLm5XzOjSe+Q5yFJyLFuMLL6+n63t4t7HR8
```

### Registering a role

This is necessary to register a user as a GitHub repository antithesis test requester.

Before you do this make sure your repository CODEOWNERS file contains a line like this:

```
antithesis: @your-github-username
```

You can have as many users as you want but registering them as test-run requesters has to be done one by one.

To register a role, you can use the `anti requester register-role` command.

```bash
anti requester register-role --platform github --username alice --repository yourorg/yourrepo
```

> Registering a role is not enough to gain rights to request test-runs. Your repository have to be white-listed by the agent. This requires you to get in contact with the agent and ask them to white-list your repository.

### Unregistering a role

```bash
anti requester unregister-role --platform github --username alice --repository yourorg/yourrepo
```

## Test-runs


### Setup

Once you are registered as a user and a role, you can request test-runs.

Before doing that make sure you have a commit in your repository containing a directory with "valid" test assets inside.

You can obtain a set of standard test assets by running the following command:

If you are in your repository directory, you can run:

```bash
anti requester generate-assets -D ./path/to/your/test/directory
```

Once you modified them you can try to run them locally  with the `anti requester test-run` command.

```bash
anti agent test-run -D ./path/to/your/test/directory (TBD)
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

#### SSH key setup
Before proceding be careful to set the necessary signing assets in your environment variables.

- `anti` will use the SSH private key to sign the request
- The private key has to be an ed25519 key.
- The public key corresponding to the private key has to be registered in your github account [see above](#registering-a-user-public-key).

Multiple keys file are supported, in this case you have to specify which key to use with the `ANTI_SSH_KEY_SELECTOR` environment variable or the `--ssh-key-selector` option.
In case you don't know the selector, you can inspect the keys in your file with

```bash
anti ssh-selectors --ssh-file PATH_TO_YOUR_SSH_FILE --ask-ssh-passphrase
```

If multiple keys are present in the file and you don't specify a selector, the first key in the file will be used.

To link to your private key file, set the `ANTI_SSH_FILE` environment variable to point to it.


As with the wallet passphrase you can set the password in the environment variable (not recommended)

```bash
read -s -p "Enter password to decrypt the SSH private key: " ANTI_SSH_PASSWORD
export ANTI_SSH_PASSWORD
```

Or better paste it from a password manager each time you need it using the `--ask-ssh-password` option

Or set the `ANTI_INTERACTIVE_SECRETS` environment variable to any value to imply the `--ask-ssh-password` option

```bash
export ANTI_INTERACTIVE_SECRETS=1
```

> The file at ANTI_SSH_FILE path has to be the encrypted ssh private key matching the user registration [see above](#registering-a-user-public-key).

#### Requesting the test-run

To request a test-run, you can use the `anti requester create-test` command.

```bash
anti requester create-test --platform github --username alice --repository yourorg/yourrepo --directory ./path/to/your/test/directory --commit your_commit_hash --try 1 --duration 2
```
> This command  will spit out the test-run-id (just a hash of the key) that you can use later to query the status of your test-run state.

You can request multiple test-runs for the same commit but you have to specify a different `--try` number for each request.

#### Checking the test-run status

You can check the status of your test-run requests with the `anti facts test-runs` command.

```bash
anti facts test-runs -i <your_test_run_id>
```

You can find all running test-runs for a user with

```bash
anti facts test-runs running --whose alice
```

Because the result URL is encrypted, you need to provide the requester SSH for decryption (see above). This work for the done selector and the full test-runs list.

```bash
anti facts test-runs done --ssh-file PATH_TO_YOUR_SSH_FILE --ask-ssh-passphrase
```

```bash
anti facts test-runs --ssh-file PATH_TO_YOUR_SSH_FILE --ask-ssh-passphrase
```
