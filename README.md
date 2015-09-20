# sftp-streams

Attempt to implement the SSH File Transfer Protocol.

## Status

At this point, there is a simple executable `sftp-mitm` that can sit between
`sshd` and the SFTP SSH subsystem (e.g. on Ubuntu this is
`/usr/lib/openssh/sftp-server`).

The program parses the packets exchanged by a regular client and the real SFTP
program on the server, and display them.

There is also a simple executable `sftp-mem` that implements a few commands.
Those commands are exercised in `test-sftp-sshfs.sh`: it uses SSHFS to talk to
the executable which exposes a directory held in memory.

## Docker image

Paths are hard-coded in the executable. There is a Dockerfile to create an
image that matches the expectations. Before building the image, you need to
provide a public SSH key `insecure_id_rsa.pub` and the `sftp-mitm` binary (put
them in the `images/sftp-mitm` directory).

## Notes

I have started to implement this package by looking at
http://tools.ietf.org/html/draft-ietf-secsh-filexfer-13 (which describes
protocol version 6) then I have switched to
http://tools.ietf.org/html/draft-ietf-secsh-filexfer-02 when I saw that my
client was using protocol version 3.

The version of `sftp` (i.e. the client) I used defaults to protocol version 3.
I have not tried protocol versions 1 and 2 (accessible with the `-1` and `-2`
flags). 

The client can re-use packet IDs very quickly (e.g. after 3 or 4 requests).

`SSH_FXP_READ` packets request 32768 bytes at a time. I think I can use the
`-B` flag to influence that size.

If the server response includes less than the requested size, the client will
issue a second request to try to get the "missing" bytes, but not again 32768
bytes. It does so even if the preceding `SSH_FXP_STAT` request lets it know the
size of the file and thus that the file was complete.

It is necessary to create a directory that you plan to transfer with `put -r`:

    > mkdir some-directory
    > put -r some-directory

otherwise the server fails. I think the reason is that the client first issues
`SSH_FXP_REALPATH` to get the correct path of the directory before transfering
it. That requests fail because the path doesn't exist yet.

Actually, by preceding the `put -r` by `mkdir`, everything (from the user point
of view) works. In reality, the `put -r` also tries to do a `SSH_FXP_MKDIR`
which fails because the directory already exists.

For debugging, I could have used the `-D` flag instead of passing through sshd.

Instead of configuring sshd's subsystem to use the `sftp-mitm` program, the
`-s` option could have been used.
