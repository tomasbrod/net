#!/bin/bash
set -e
mkdir -p bin

#get information from git
gitver=$(git describe --always --long --dirty --abbrev=16 --tags || true)
echo Git version: $gitver

#create gitver.pas
echo "unit gitver; interface
var GIT_VERSION:string='$gitver';
implementation end." >gitver.pas

make -C ed25519
fpc @fpopt.cfg brodnetd.pas
fpc @fpopt.cfg bnedit.pas
#fpc @fpopt.cfg bnmut.pas
fpc @fpopt.cfg bnc.pas
ln -f -t ./ bin/brodnetd bin/bnedit bin/bnc
