# Tasks — #159 verified state + requests read

## Slice S1 — verified getToken (state + requests)
- [ ] T159-S1 RED unit test (TokenResponse+RequestsResponse → moog Token; tampered/incomplete fails closed); GREEN getToken via /tokens/:id (verifyTokenState) + /tokens/:id/requests (verifyTokenRequests), assemble moog Token, wire mpfsGetToken; facts/write ops intact; `./gate.sh` green.
