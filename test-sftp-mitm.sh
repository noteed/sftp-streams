#! /bin/bash

echo Running sftp-mitm in Docker container...
SFTPD_ID=$(docker run -d sftp-mitm /usr/sbin/sshd -D)
SFTPD_IP=$(docker inspect $SFTPD_ID | grep IPAddress | awk '{ print $2 }' | tr -d ',"')

# Increase chances sshd is ready.
# TODO Maybe poll until the banner is accessible ?
sleep 1

echo Running sftp...
sftp sftp@$SFTPD_IP <<EOF
ls
mkdir /home/sftp/somedir
cd  /home/sftp/somedir
mkdir bin
put -r bin
cd bin
get sftp-mitm.hs delete-me.test-file
EOF
ssh sftp@$SFTPD_IP ls -la somedir/bin
ssh sftp@$SFTPD_IP cat debug.txt
rm delete-me.test-file

echo Killing container...
docker kill $SFTPD_ID
