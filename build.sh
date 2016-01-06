#!/bin/bash
if [ ! -d bin ]; then mkdir bin; fi
make -C ed25519
fpc @fpopt.cfg brodnetd.pas
mv bin/brodnetd ./
