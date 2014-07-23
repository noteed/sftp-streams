# sftp-streams

Attempt to implement the SSH File Transfer Protocol.

## Status

At this point, there is a simple executable `sftp-mitm` that can sit between
`sshd` and the SFTP SSH subsystem (e.g. on Ubuntu this is
`/usr/lib/openssh/sftp-server`).

The program parses the packets exchanged by a regular client and the real SFTP
program on the server, and display them.

## Docker image

Paths are hard-coded in the executable. There is a Dockerfile to create an
image that matches the expectations. Before building the image, you need to
provide a public SSH key `insecure_id_rsa.pub` and the `sftp-mitm` binary (put
them in the `images/sftp-mitm` directory).
