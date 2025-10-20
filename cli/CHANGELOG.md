# Changelog for anti-cli

### v0.3.2.0 - 2025-10-20
#### Bug Fixes
- Fixed oracle process image to contain the docker command
- Removed json nulls as value in commands outputs

---

### v0.3.1.0 - 2025-10-02
#### Added
- Validation of Docker Compose configuration after checking for the file's existence.

#### Bug Fixes
- `anti --version` now correctly shows `0.3.1.0`.

---

### v0.3.0.0 - 2025-09-30
#### Changes
- **SSH authentication is now deprecated:**
    - Users can still use existing SSH-based registrations to create test-runs (up to but not including v0.5.0.0).
    - SSH-based new registrations are no longer supported.
    - SSH-based unregistrations are not validated (anyone can unregister any SSH-registered user).

#### Added
- Support for registrations (or re-registration of old users) via `vkey` publishing.

---

### v0.2.2.0 - 2025-09-25
#### Added
- Support for CODEOWNERS file in `.github` and `docs` directories.

---

### v0.2.1.0 - 2025-09-20
#### Changes
- GitHub usernames and repository names are now case-insensitive, aligning with GitHub's naming conventions.
- Duplicate registrations differing only by letter case are no longer allowed.

---

### v0.2.0.3 - 2025-09-15
#### Changes
- Removed support for multiple SSH keys for the same GitHub user.
- Registrations with multiple SSH keys are no longer allowed.

---

### v0.2.0.1 - 2025-09-10
#### Bug Fixes
- Fixed a bug that allowed unregistering valid users and roles.

---

### v0.2.0.0 - 2025-09-05
#### Added
- `anti agent` executable to automate the agent role.
- Wallet encryption/decryption for all roles, available at `anti wallet` command.
- `anti agent push-test` command for the agent to start a test.
- `anti agent report-results` now encrypts the URL with results using the requester's SSH public key.
- `anti facts test-runs done` now decrypts the result URL when the requester SSH key is accessible.
- `anti agent collect-email-results` command that scans the agent email via IMAP to collect results.
- More filters in `anti facts test-runs` command: `--whose` and `--test-run-id`.
- Requesters can now provide any test assets, as long as they include a working `docker-compose.yml` file.

#### Bugs
- Interactive secret input switch is still available but may not appear in some help messages or may appear twice (e.g., `--ask-ssh-password`).

---

### v0.1.1.0 - 2025-08-25
#### Added
- `anti` executable.