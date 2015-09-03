#! /usr/bin/env bash

docker run \
  -v `pwd`:/home/gusdev/sftp-streams \
  images.reesd.com/reesd/stack \
  cabal install sftp-streams/sftp-streams.cabal
