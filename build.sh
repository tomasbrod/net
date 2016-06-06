#!/bin/bash
set -e
mkdir -p bin
gitver=$(git describe --always --long --dirty --abbrev=16 --tags || true)
const(){ eval "$1"; }
BUILD_VERSION=0
source gitver.inc||: 2>/dev/null
(( buildver= BUILD_VERSION + 1 ))
echo Git version: $gitver Build: $buildver
echo "const GIT_VERSION='$gitver';
const BUILD_VERSION=$buildver;
const BUILD_HOST='$HOSTNAME';" >gitver.inc
make -C ed25519
fpc @fpopt.cfg brodnetd.pas
fpc @fpopt.cfg bnedit.pas
#fpc @fpopt.cfg bnmut.pas
#fpc @fpopt.cfg bnc.pas
ln -f -t ./ bin/brodnetd bin/bnedit bin/bnc
