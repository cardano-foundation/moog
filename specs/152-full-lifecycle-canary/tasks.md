# Tasks — #152 full-lifecycle live-boundary canary

## Slice S1 — extend canary beyond boot/end to the write/submit cycle
- [X] T152-S1 Extend app/moog-mpfs-v2-canary.hs (bootThenEndCanary) to exercise the facts write+submit path against the paired devnet: boot -> a requester request insert (+submit via /tx/submit) -> oracle update-token (merge the request) -> end; assert each tx is observed. Emit structured success JSON. Update .github/workflows/mpfs-v2-canary.yaml to run the full-cycle. `./gate.sh` green (builds canary exe). Live run happens in CI against the paired offchain devnet.
