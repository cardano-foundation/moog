# Real-World Scenario

This manual describes a real-world scenario, involving an interaction between the three roles involved in using the system (requester, agent and oracle). It is based on an end-to-end test bash script.

## Pre-requisites

The following scenario assumes that each role has fulfilled its pre-requisites, e.g.:

* ```moog``` command is available
* wallet created and funded
* environment variables have been set

For details of how to achieve this, see [configuration](user/configuration.md).

## The Scenario

### Setting up an interface

To bootstrap the system, the oracle must create an moog token:

```
moog oracle token boot

```

This token is unique for a system. It enables all the actors to collaborate. It needs to be shared by some means with users. For example, the one that has been created (currently on preprod) for this project (an interface the the moogthesis instance managed by Cardano Foundation) is provided in the [configuration](user/configuration.md). In order to "connect" to this interface all roles need to set the MOOG_TOKEN_ID environment variable to the value that was returned by the above command.

Then, the agent must configure this interface on-chain, in particular test durations and the public key hash of the agent:

```
agent=$(moog wallet info | jq -r '.owner')
export MOOG_AGENT_PUBLIC_KEY_HASH=$agent
moog oracle config set \
    --min-test-duration 1 \
    --max-test-duration 4 \
    --agent-pkh "$MOOG_AGENT_PUBLIC_KEY_HASH"
```

In other words, the identity of the agent for this interface is defined by providing the public key hash from their wallet.

Now, the interface to the Antithesis instance is ready to be used by requesters.

### Setting up a test repository

In order to be able to run tests, requesters must first register themselves, using their GitHub username (e.g. "myghname") and public key hash:

```
moog requester register-user \
    --platform github \
    --username myghname \
    --pubkeyhash  ABAAC3NeaC3CCAILjrzNvy85HbzXV2lsW2lkjsq3Nrj84kjp3puarZX
```

If the username and pubkeyhash are valid, this request will be added to a queue, which the oracle can then include by updating the token. When this has been done the user should appear when requesting facts:

```
moog facts users
```

This process (where requests must get validated by the oracle before they become facts) applies to all requests, and for the sake of conciseness we will omit to make this explicit in what follows.

Although the user is now registered, it is not yet allowed to request test runs. For this it must be given a role, i.e. associated with a specific repository (containing test specifications and identifying this user in the CODEOWNERS file). To achieve this the requester now adds a request:

```
moog requester register-role \
    --platform github \
    --username myghname \
    --repository myghname/myghrepo
```

When the oracle includes this new request on the token, the last step is for the requester to ask the agent to whitelist this repository. This would be the result of an informal discussion over on our [Discord][Discord] channel, or by sending an email to [hal\@cardanofoundation.org][]. When the agent has decided to whitelist the repository, they invoke:

```
moog agent white-list \
    --platform github \
    --repository myghname/myghrepo
```

The result of the above set up can be verified with:

```
moog facts users
moog facts roles
moog facts white-list
```

The system is now ready to accept test run requests. The above only needs to be set up once (for a given requester and a given test repository). Then, the following commands (to run tests) can be performed repeatedly.

### Running tests on Antithesis

The requester can now request to run an antithesis test.

```
moog requester create-test \
    --platform github \
    --username myghname \
    --repository myghname/myghrepo \
    --directory antithesis-test \
    --commit a7741a44dfddfe05822e1a49862ceea43ecd657d \
    --try 1 \
    --duration 1
```

This request must be accepted or rejected by the agent. The agent can inspect the repository and the specified directory in order to determine whether they contain valid specifications for an antithesis test.

For example, to reject the test run with no reason, the agent would invoke:

```
validation=$(moog agent query)
references=$(echo "$validation" | jq -r '.pending | .[] | .id')
moog agent reject-test -i "$references"
```

If this happens, the requester can try to improve the test code and try a second request:

```
moog requester create-test \
    --platform github \
    --username myghname \
    --repository myghname/myghrepo \
    --directory antithesis-test \
    --commit a7741a44dfddfe05822e1a49862ceea43ecd657d \
    --try 2 \
    --duration 1
```

Supposing the agent now sees that the repository contains a better test scenario, the agent could accept the test run by invoking:

```
validation=$(moog agent query)
references=$(echo "$validation" | jq -r '.pending | .[] | .id')
moog agent accept-test -i "$references"
```

When Antithesis has finished the test run, the agent should publish the results:

```
validation=$(moog agent query)
references=$(echo "$validation" | jq -r '.running | .[] | .id')
moog agent report-test -i "$references" \
    --duration 1 \
    --url "https://example.com/report"
```


[Discord]: https://discord.gg/sVUnen7t
