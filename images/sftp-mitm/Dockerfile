FROM ubuntu:12.04
MAINTAINER Vo Minh Thu <thu@hypered.io>

RUN apt-get update
RUN apt-get install -q -y language-pack-en
RUN update-locale LANG=en_US.UTF-8

RUN apt-get install -q -y vim

# Install an SSH server.
RUN apt-get -q -y install openssh-server
# No idea why this directory is not created, but sshd needs it.
RUN mkdir /var/run/sshd

# Backend
# -------

# Install sftp-mitm dependency
RUN apt-get install -q -y libgmp10

RUN useradd -m -s /bin/bash sftp
RUN mkdir -p /home/sftp/.ssh
ADD insecure_id_rsa.pub /home/sftp/.ssh/authorized_keys
RUN chown -R sftp:sftp /home/sftp/.ssh

RUN mv /usr/lib/openssh/sftp-server /usr/lib/openssh/sftp-server-original
ADD sftp-mitm /usr/lib/openssh/sftp-server

# Create known directory structure
RUN mkdir /home/sftp/project
RUN echo '# Some project' > /home/sftp/project/README.md
RUN echo 'Hello world.' > /home/sftp/project/hello.txt
RUN mkdir /home/sftp/project/empty
RUN mkdir /home/sftp/project/tests
RUN echo 'Not a test.' > /home/sftp/project/tests/test-00.txt
RUN chown -R sftp:sftp /home/sftp/project
