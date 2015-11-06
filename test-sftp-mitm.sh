#! /bin/bash

echo Running sftp-mitm in a Docker container...
SFTPD_ID=$(docker run -d sftp-mitm /usr/sbin/sshd -D)
SFTPD_IP=$(docker inspect $SFTPD_ID | grep IPAddress | awk '{ print $2 }' | tr -d ',"')

# Increase chances sshd is ready.
# TODO Maybe poll until the banner is accessible ?
sleep 1

# Avoid downloading files in the current directory.
STARTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
TEMPDIR=$(mktemp -d)
cd $TEMPDIR

echo Running sftp...
sftp sftp@$SFTPD_IP < $STARTDIR/test-batch.txt
ssh sftp@$SFTPD_IP cat debug.txt

echo Killing container...
docker kill $SFTPD_ID
