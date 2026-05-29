# Feature Specification: Antithesis Runs CLI

## Primary User Story

As a Moog operator, I can run `moog antithesis runs` to authenticate with
GitHub through device flow, call the Moog Antithesis proxy, and receive the
proxied Antithesis runs JSON on stdout.

## Functional Requirements

- Add a top-level `antithesis` command group to the `moog` binary.
- Support the v1 subcommand `moog antithesis runs` with no flags.
- On first use, read no cached token, start GitHub device-flow auth with the
  public Moog OAuth client id and `read:org` scope, print the verification URL
  and user code to stderr, cache the OAuth token, and continue.
- On later uses, read `~/.config/moog/github-oauth.json` and reuse the cached
  token.
- Create the token cache file with mode `0600`.
- Call `GET $MOOG_ANTITHESIS_PROXY_URL/api/v1/runs`, defaulting the proxy URL
  to `https://antithesis-proxy.plutimus.com`.
- Print the JSON response to stdout and exit 0 on success.
- On proxy 401, delete the cached token, perform device-flow auth once more,
  and retry once.
- On 403, print the proxy body, include the SSO URL when present, and exit 2.
- On proxy 5xx or network failure, print a clear stderr message and exit 3.
- On HTTP 200 with non-JSON body, print a clear stderr message and exit 4.
- Document the exit codes in `moog antithesis runs --help`.

## Out Of Scope

- `moog antithesis run <id>` and run-create commands.
- Paging and filtering flags.
- A standalone `moog auth login` or logout command.

## Acceptance Criteria

- Unit tests cover token cache write, cache read, file mode `0600`, and
  cache eviction plus one retry after a 401 response.
- The parser recognizes `moog antithesis runs`.
- The runtime command maps proxy outcomes to the requested exit codes.
- Manual smoke is documented for an operator-run call against the deployed or
  local staging proxy.
