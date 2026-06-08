# Tasks — #166 oracle update-token via facts (subset + validation)

## Slice S1 — thread request subset + rewire oracle
- [X] T166-S1 Make updateTokenFromFacts/updateTokenFactsRequest carry [RequestRefId] (-> urRequests, stop hardcoding []); widen mpfsUpdateTokenFromFacts record op; rewire Oracle/Token/Cli.hs UpdateToken to mpfsUpdateTokenFromFacts (subset), keeping oracle-side validation; `./gate.sh` green.
