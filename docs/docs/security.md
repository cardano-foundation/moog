# Security Considerations

## Remote MPFS Service

The MPFS service is not able to sign transactions for the user. Nevertheless the current implementation doesn't perform any intent-check on the unsigned transactions it receives. Therefore, when using a remote MPFS service, users should ensure that they trust the operator of the service, as a malicious operator could potentially alter the unsigned transactions before sending them back to the user.

To improve the situation an issue is open here: [MOOG-4e81d1a](https://app.radicle.xyz/nodes/ash.radicle.garden/rad:z2a7Te5b28CX5YyPQ7ihrdG2EEUsC/issues/4e81d1ab975d99c5c2a6ea53d3d49915475540fa)