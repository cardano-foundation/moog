# Browser access through the Antithesis proxy

As an active member of pragma-org/antithesis-access, I open a report from the dashboard, sign in with GitHub at the Antithesis proxy, and reach the signed Antithesis report URL. Other users receive an explanatory denial. The report URL is a capability: subsequent sharing remains possible; our service controls disclosure only.

Contract: dashboard navigation to `/browser/report/<run-id>` starts sign-in when needed; callback `/browser/callback`; proxy-origin account/sign-out pages and server sessions. Only a validated run ID crosses from the public dashboard. Preserve existing bearer-authenticated CLI API behavior. GitHub user/team authorization is independent of requester, Cardano and Moog CLI.

Acceptance is issue222: single-use random browser-bound OAuth state, S256 PKCE, server-side token exchange, fresh active-team check before every disclosure, opaque rotated sessions, Secure/HttpOnly/host-only cookies, protected same-origin sign-out POST, expiry/replay denial, destination HTTPS tenant/report-path allowlist, no-store/no-referrer private responses, safe distinguishable pending/missing/denied/upstream states, no private values in logs or public assets. Real local HTTP integration must exercise redirects, callback and mocks through production routing; production OAuth acceptance remains separately evidenced.

Use configurable proposed defaults of one-hour sessions and five-minute single-use login state. Required browser client ID, client-secret file and callback are explicit deployment settings; never invent a credential. Preserve the existing CLI path if browser configuration is absent, with browser access safely unavailable.

Non-goals: Cardano transactions, wallet access, requester-only permissions, changes to agent/oracle, public report URL publication, report URL revocation, collector requester-coverage fix, rehosting or replacing the proxy service. Use product name Antithesis proxy.
