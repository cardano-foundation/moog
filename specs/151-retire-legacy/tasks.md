# Tasks — #151 retire ALL legacy MPFS surface

## Slice S1 — remove all legacy ops/routes (all now 0-consumer)
- [ ] T151-S1 Remove ALL legacy MPFS record ops + Servant routes + client fns: requestInsert/Delete/Update, retractChange, updateToken, submitTransaction(legacy), waitNBlocks, getTransaction(legacy), and legacy token/:id + token/:id/facts read routes. KEEP *FromFacts, v2 submit/await, /tokens/* read clients (MPFS.Read), boot/end, and RequestInsert/Delete/UpdateBody (used by *FromFacts). `./gate.sh` green.
