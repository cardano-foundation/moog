# Antithesis Interface

## Context
  * `alice` is a user who wants to run tests in Antithesis for her cardano-node
  * `antithesis` is an agent that controls and monitors test-run's on the antithesis platform
  * `token` is a stateful unique token locked on-chain tracking the interactions between `alice` and `antithesis`. As a side effect it tracks also alice github credentials and alice github roles. It's a fact container, with a smart contract controlling the update process of the facts.
  * `oracle` is an agent in charge of the `token` updates. It does everything in its power to prevent inconsistent data to enter/exit the `token`.

## Sequence Diagram

```mermaid
sequenceDiagram
    participant GitHub as Github Platform
    actor Alice
    actor Oracle
    participant Token as Cardano Token
    actor Antithesis
    participant AntithesisPlatform as Antithesis Platform


    %% Step 1: Alice registers as an identified user
    Alice->>+GitHub: Add an Ed25519 SSH key to github profile
    Alice->>+Token: anti requester register-user
    Oracle->>+GitHub: Validate alice credentials
    Oracle->>+Token: anti oracle update-token

    %% Step 2: Alice registers role as an antithesis test runner
    Alice->>+GitHub: Add herself under Antithesis key in CODEOWNERS file in her repository
    Alice->>+Token: anti requester register-role
    Oracle->>+GitHub: Validate alice role in CODEOWNERS
    Oracle->>+Token: anti oracle update-token

    %% Step 3: Agent white-list alice repository
    Antithesis->>+Token: anti agent white-list
    Oracle->>+Token: Validate antithesis identity

    %% Step 4: Alice requests test runs
    loop alice test runs request
    Alice->>+Token: anti requester create-test
    Oracle->>+Token: Check alice is maintainer
    Oracle->>+Token: Check platform/repo/commit prefix and run-index
    Oracle->>+Token: Validate alice SSH signature of the request
    Oracle->>+Token: anti oracle update-token

    %% Step 5: Antithesis processes test run request
    alt Antithesis rejects request
        Antithesis->>+Token: anti agent reject-test
        Oracle->>+Token: Validate antithesis identity and test state
        Oracle->>+Token: anti oracle update-token
    else Antithesis accepts request
        Antithesis->>+AntithesisPlatform: Start test run
        Antithesis->>+Oracle: anti agent accept-test
        Oracle->>+Token: Validate antithesis identity and test state
        Oracle->>+Token: anti oracle update-token
    end

    %% Step 6: Antithesis updates test run results
    Antithesis->>+AntithesisPlatform: Poll platform for test results
    Antithesis->>+Token: anti agent finish-test
    Oracle->>+Token: Validate antithesis identity and test state
    Oracle->>+Token: anti oracle update-token
    end
```
