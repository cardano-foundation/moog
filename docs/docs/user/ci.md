# Continuous Integration

_Moog_ can be easily integrated in a _Continuous Integration_ process to trigger Antithesis tests run on demand. Here is some example configuring [GitHub actions](https://github.com/features/actions) but this should be straightforward to adapt to any other kind of CI engine.

These tests are triggered every 6 hours

```
name: "Antithesis Tests"
on:
  schedule:
    # run every 6 hours
    - cron:  '5 1,7,13,19 * * *'
```

Linux/x86 is currently the best supported platform by Moog.

```
jobs:
  run-antithesis:
    runs-on: ubuntu-latest
```

We need to define a number of environment variables that will be used by Moog _requester_ to request tests run. The detailed meaning of these variables is explained in the [usage](./usage) section of the documentation:

```
    env:
      MOOG_WALLET_FILE: ${{ vars.MOOG_WALLET_FILE }}
      MOOG_MPFS_HOST: ${{ vars.MOOG_MPFS_HOST }}
      MOOG_TOKEN_ID: ${{ vars.MOOG_TOKEN_ID }}
      MOOG_PLATFORM: github
      MOOG_REQUESTER: ${{ vars.MOOG_REQUESTER }}
      MOOG_TEST_DIRECTORY: ${{ vars.MOOG_TEST_DIRECTORY }}
```

* `MOOG_WALLET_FILE` is the file that will store the mnemonics of the wallet used to pay transaction fees
* `MOOG_MPFS_HOST` is the URL of the [_Merkle-Patricia Forestry Service_](https://cardano-foundation.github.io/mpfs/) Moog shall use to retrieve information about the system's state
* `MOOG_TOKEN_ID` is the identifier of the particular "database" Moog will be using (a 32-bytes hash denoting a Cardano native asset name)
* `MOOG_PLATFORM` is hardcoded to `github` as this is currently the only supported source forge
* `MOOG_REQUESTER` is the username under which the tests will be run. Note this name must both exist in the _platform_ and be [registered](./usage) with Moog
* `MOOG_TEST_DIRECTORY` points at a directory in the source code which contains a valid Antithesis `docker-compose.yaml` configuration file for tests submission

We also need to set some _secrets_ that will be used by Moog to authenticate and authorize the requests:

```
      MOOG_GITHUB_PAT: ${{ secrets.MOOG_GITHUB_PAT }}
      MOOG_REQUESTER_WALLET: ${{ secrets.MOOG_REQUESTER_WALLET }}
```

* `MOOG_REQUESTER_WALLET` is the base64-encoded JSON content of the wallet file created by `moog wallet create`, typically the _mnemonics_ (see [Configuration](./configuration))
* `MOOG_GITHUB_PAT` is obviously specific to `github` platform and is a _Personal Authentication Token_ used to query Github API

We set a timeout of 10 hours for this job as we'll be waiting for test execution result, adjust accordingly

```
    timeout-minutes: 600
```

These are the permissions we need for _this job_ and which will be set for the particular token attached to it

```
    # required permissions to be able to push to registry
    permissions:
      packages: write
      contents: read
      attestations: write
      id-token: write
```

Now come the sequence of actions needed to run the tests. We first do some setup for docker, and checkout the code

```
    steps:
    - name: 🚧 Set up Docker Buildx
      uses: docker/setup-buildx-action@v3

    - name: 📥 Checkout repository
      uses: actions/checkout@v4
      with:
        ref: ${{ github.event.inputs.ref_name || '' }}

    - name: 🔑 Login Docker Registry
      uses: docker/login-action@v3
      with:
          registry: ${{ env.REGISTRY }}
          username: ${{ github.actor }}
          password: ${{ secrets.GITHUB_TOKEN }}
```

We then identify the latest available release of Moog,

```
    - id: moog
      name: Get latest moog release tag
      uses: pozetroninc/github-action-get-latest-release@master
      with:
        repository: cardano-foundation/moog
```

retrieve it from GitHub packages,

```
    - name: Download moog from latest release
      uses: robinraju/release-downloader@v1.10
      with:
        repository: cardano-foundation/moog
        tag: ${{ steps.moog.outputs.release }}
        fileName: "moog-*-linux64.tar.gz"
        out-file-path: "."
        extract: true
```

and ultimately install `moog` executable to be available for later steps.

```
    - name: Install moog
      run: |
        echo "Release: ${{ steps.moog.outputs.release }}"
        sudo install -m 0755 moog /usr/local/bin/moog
        moog --version
```

The wallet's secret (mnemonics) are extracted and temporarily stored in the local filesystem for use by Moog.

```
    - name: Configure moog wallet
      run: printf '%s' "$MOOG_REQUESTER_WALLET" | base64 --decode > $MOOG_WALLET_FILE
```

Finally, we are able to submit our test request:

```
    - name: Submit test
      id: request
      run: |
        set -euo pipefail
```

This part of the script computes the trial number for this test run. Test runs are identified by the Git commit SHA plus a trial number in order to avoid duplicate requests. We use the `moog facts test-runs` command parameterise by the user id to retrieve relevant data.

```
        TRY=$(moog facts test-runs --whose $MOOG_REQUESTER | jq 'map(select(.key.commitId == "${{ github.sha }}")) | length')
        TRY=$(($TRY + 1)) # start with try=1
        echo "TRY=$TRY"
```

At last, we request test execution. `$DURATION` should be set to the requested duration (in hours) of the test run. If all goes well transaction and test run identifiers should be printed.

```
        RESULT=$(moog requester create-test -d "$MOOG_TEST_DIRECTORY" \
          -c ${{ github.sha }} \
          -r "$GITHUB_REPOSITORY" \
          --try $TRY \
          -t $DURATION)
        echo $RESULT

        # Passing -e and -r in separate jq calls ensures we both unwrap
        # the string quotes and fail if the json isn't as expected.
        ID=$(echo $RESULT | jq -e .value.testRunId | jq -r .)
        TXID=$(echo $RESULT | jq -e .txHash | jq -r .)
        echo "id=$ID" >> "$GITHUB_OUTPUT"
        echo "txHash=$TXID" >> "$GITHUB_OUTPUT"
```

The last part of the job simply waits for the result to be published by the Antithesis agent, repeatedly running `moog facts test-runs --test-run-id "$ID"` command and checking its output to tell whether or not the test is finished.

```
    - name: Wait for results
      run: timeout $((($DURATION + 1) * 3600)) ./scripts/wait-for-test.sh "${{ steps.request.outputs.id }}"
```

when the test completes, final result is printed in the logs along with the URL where the detailed Antithesis report can be found. The job succeeds if and only if the test `outcome` is a success.

```
final result: [{"id":"722bb7a35a6b9318536b6750751e5f13ed6e1dcc83fd113d98cf4e4b02ea1e14","key":{"commitId":"0ce357a0faabf970eb38d7191bf5f8bb8a36684b","directory":"docker/testnet","platform":"github","repository":{"organization":"pragma-org","repo":"amaru"},"requester":"abailly","try":2,"type":"test-run"},"slot":107737174,"value":{"duration":3,"from":{"from":{"duration":3,"faults_enabled":true,"phase":"pending","signature":"d1a70d0f60d358330f89d58c50f9971f16c54223d31932b3335561cd4efaecf4f9145c0f48f7098a66597062d7b558fe7785276cf67676bad0c787306b54be00"},"phase":"accepted"},"outcome":"failure","phase":"finished","url":"https://cardano.antithesis.com/report/F7dbAImlN-ZGCzC2_rkNKayN/Jw7X8mEfgH6iFqDy1W9jEDeSJ9rQWwAGN_y_eZ3oHwk.html?auth=v2.public.eyJuYmYiOiIyMDI1LTExLTE3VDIxOjU2OjEwLjI0NTMxNzAxNVoiLCJzY29wZSI6eyJSZXBvcnRTY29wZVYxIjp7ImFzc2V0IjoiSnc3WDhtRWZnSDZpRnFEeTFXOWpFRGVTSjlyUVd3QUdOX3lfZVozb0h3ay5odG1sIiwicmVwb3J0X2lkIjoiRjdkYkFJbWxOLVpHQ3pDMl9ya05LYXlOIn19ffVFhhos4dhGHfry7p0K54c_cOHbkxnJW3Il926JxHeMIh3rRZgjuBbMUlyfVCl7sEG1Sc6nNRHr36gTOfM0xAQ"}}]
failed
Error: Process completed with exit code 1.
```
