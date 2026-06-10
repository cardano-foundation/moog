# Tasks — #178 (oracle validation)

## Slice A — restore + migrate the dropped request-tx integration specs
- [ ] T178-SA restore requestInsert/requestDelete/requestUpdate specs in test-integration/MPFS/APISpec.hs (from git history of the TODO(#178) drop)
- [ ] T178-SA migrate them to requestInsertFromFacts/requestDeleteFromFacts/requestUpdateFromFacts (src/MPFS/Request.hs)
- [ ] T178-SA proof: integration suite passes against the self-hosted devnet (locally), new specs green

## Slice B — end-to-end oracle request→validate→apply cycle spec
- [ ] T178-SB integration spec: boot token → requester submits request (facts) → oracle validateRequest → UpdateToken/mpfsUpdateTokenFromFacts → assert token state reflects the change
- [ ] T178-SB fix any oracle breakage surfaced against the new MPFS
- [ ] T178-SB proof: spec passes against the self-hosted devnet (locally); orchestrator confirms green on the PR CI
