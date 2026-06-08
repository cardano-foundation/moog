# Tasks — #151 retire dead legacy MPFS surface

## Slice S1 — remove 0-consumer legacy ops/routes
- [ ] T151-S1 Remove legacy MPFS ops with no live consumers (requestInsert/Delete/Update, retractChange, submitTransaction(legacy), waitNBlocks, getTransaction(legacy)) + their Servant routes + client fns + record fields, and the legacy token/:id + token/:id/facts route defs. KEEP mpfsUpdateToken (+ update-token route, pending #166), the *FromFacts ops, RequestInsert/Delete/UpdateBody (used by *FromFacts), v2 submit/await, the /tokens/* read clients, boot/end. `./gate.sh` green.
