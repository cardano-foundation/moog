# Secrets management tips

## Secrets in docker containers

When passing secrets to a docker container in a compose you can use the `ANTI_SECRETS_FILE` environment variable to point to a file containing the secrets in yaml format. So instead of setting say `ANTI_SSH_PASSWORD` and `ANTI_GITHUB_PAT` you can create a file `secrets.yaml` with the following content:

```yaml
sshPassword: your_ssh_password
githubPAT: your_github_pat
```

and then pass it to the container with someething like:

```yaml
services:
  anti:
    .....
    environment:
      - ANTI_SECRETS_FILE=/run/secrets/anti_secrets
    secrets:
      - anti_secrets

secrets:
  anti_secrets:
    file: ./secrets.yaml
```

These are the supported secrets for different commands:
```yaml
sshPassword: requester_ssh_password
githubPAT: requester_or_oracle_github_pat
walletPassphrase: anyone_wallet_passphrase_if_any
antithesisPassword: agent_antithesis_platform_password
slackWebhook: agent_slack_webhook_url
```