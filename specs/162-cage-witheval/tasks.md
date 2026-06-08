# Tasks — #162 adapt cage builders to offchain *WithEval (eval-context)

## Slice S1 — *WithEval migration
- [X] T162-S1 Add eval-context fetch+decode (mirror offchain Workflow.resolveEvalContext, in ClientM: GET /eval-context -> decodeEvalContext -> DecodedEvalContext); migrate src/MPFS/{Boot,End,Retract,Update}.hs to *WithEval (thread DecodedEvalContext as first arg); imports updated; `./gate.sh` green.
