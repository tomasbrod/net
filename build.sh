#!/bin/bash
set -e
#if [ ! -d bin ]; then mkdir bin; fi
mkdir -p bin
make -C ed25519
gitver=$(git describe --always --dirty --abbrev=16 --tags || true)
echo Git version: $gitver
echo "const GIT_VERSION='$gitver';" >gitver.inc
fpc @fpopt.cfg brodnetd.pas
mv bin/brodnetd ./
