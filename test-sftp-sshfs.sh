#! /bin/bash

echo Running sftp-mitm in a Docker container...
SFTPD_ID=$(docker run -d sftp-mitm /usr/sbin/sshd -D)
SFTPD_IP=$(docker inspect $SFTPD_ID | grep IPAddress | awk '{ print $2 }' | tr -d ',"')

# Increase chances sshd is ready.
# TODO Maybe poll until the banner is accessible ?
sleep 1

mkdir -p sshfs
sshfs sftp@$SFTPD_IP: sshfs

echo "Run"
echo "  fusermount -u sshfs"
echo "  docker exec $SFTPD_ID cat /home/sftp/debug.txt"
echo "  docker kill $SFTPD_ID"
echo "when done."
