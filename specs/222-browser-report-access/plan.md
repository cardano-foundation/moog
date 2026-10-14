# Plan

Extend current Haskell/WAI Antithesis proxy with isolated browser routes and an in-memory bounded session/state store. Reuse GitHub identity/team and Antithesis client boundaries; browser fresh membership must bypass existing CLI membership caching. Keep OAuth secrets server-side. Standard top-level navigation avoids third-party cookie and CORS fetch dependencies.

One approved implementation seat: Codex gpt-6.1-sol/high in the current window; parent owns mandate, planning artifacts and acceptance. No extra reviewer, agents or draft tools. Child owns implementation, tests, wiring and relevant docs/deployment examples. Independent agent audit omitted from this roster; disclose it.

Slices: browser config/store and OAuth boundaries; routed sign-in/callback/report/sign-out with local HTTP integration; Nix/Compose/API documentation and operator verification/rollback. Each behavior slice contains meaningful executable RED/GREEN evidence and remains bisect safe. Keep existing CLI tests passing. Follow repository constitution, Haskell/Nix conventions, default gate and strongest feasible HTTP integration.

Before live auth deployment: parent verifies configuration/app callback/secret availability and concrete allowed/denied account evidence. Missing credentials block only real OAuth installation/acceptance; implement and verify the configurable service first. Do not alter OAuth applications, credentials, production deployment or org permissions in this worker run.

Plan review: requirement/ownership consistency checked against issue222 and constitution. Security boundaries and outstanding production OAuth configuration recorded; implementation can proceed without inventing credentials or declaring live acceptance.
