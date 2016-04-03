#!/bin/bash
set -e
mkdir -p bin
make -C ed25519
gitver=$(git describe --always --dirty --abbrev=16 --tags || true)
echo Git version: $gitver
echo "const GIT_VERSION='$gitver';" >gitver.inc
fpc @fpopt.cfg bnprof.pas
fpc @fpopt.cfg bnmut.pas
fpc @fpopt.cfg bnc.pas
fpc @fpopt.cfg brodnetd.pas
ln -f -t ./ bin/brodnetd bin/bnprof bin/bnc bin/bnmut
