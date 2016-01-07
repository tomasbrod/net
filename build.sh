#!/bin/bash
set -e
#if [ ! -d bin ]; then mkdir bin; fi
mkdir -p bin
make -C ed25519
fpc @fpopt.cfg brodnetd.pas
mv bin/brodnetd ./
