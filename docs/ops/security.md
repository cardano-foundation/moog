# Security Considerations

## Remote MPFS Service

The MPFS service is not able to sign transactions for the user. Nevertheless the current implementation doesn't perform any intent-check on the unsigned transactions it receives. Therefore, when using a remote MPFS service, users should ensure that they trust the operator of the service, as a malicious operator could potentially alter the unsigned transactions before sending them back to the user.

To improve the situation [an issue is open in GitHub](https://github.com/cardano-foundation/moog/issues).
