#! /bin/bash

echo Running sftp-mitm in Docker container...
SFTPD_ID=$(docker run -d sftp-mitm /usr/sbin/sshd -D)

echo Running sftp...
sftp sftp@172.16.44.2 <<EOF
mkdir /home/sftp/somedir
cd  /home/sftp/somedir
mkdir bin
put -r bin
EOF
ssh sftp@172.16.44.2 ls -la somedir/bin
ssh sftp@172.16.44.2 cat debug.txt

echo Killing container...
docker kill $SFTPD_ID
