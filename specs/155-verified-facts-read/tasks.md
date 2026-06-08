# Tasks — #155 verified facts read

## Slice S1 — verified facts read
- [ ] T155-S1 RED unit test (FactsResponse → moog facts; fails closed on bad verify); GREEN `getTokenFacts` via bulk `/tokens/:id/facts` + `verifyTokenFacts`, map `[FactEntry]` to moog facts, wire record; legacy reads intact; `./gate.sh` green.
