#!/bin/bash
set -e
mkdir -p bin

#get information from git
gitver=$(git describe --always --long --dirty --abbrev=16 --tags || true)
echo Git version: $gitver

#create gitver.inc
echo "const GIT_VERSION='$gitver';" >gitver.inc

make -C alg
fpc @fpopt.cfg brodnetd.pas
fpc @fpopt.cfg bnc.pas
fpc @fpopt.cfg bnedit.pas

echo Built version: $gitver
