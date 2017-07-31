#!/bin/bash
set -e
mkdir -p bin

if [ -e gitver.inc ]; then
  function const () { eval $@; }
  source gitver.inc
  let ++VER_BUILD
else
  VER_BUILD=0
fi

#get information from git
githash=$(git rev-parse HEAD)
gittag=$(git describe --always --abbrev=0 --match 'v[0-9]*')
gitcnt=$(git rev-list --count $gittag..HEAD)
gitbranch=$(git symbolic-ref --quiet --short HEAD)
if git diff-index --quiet HEAD --
  then gitclean=true; else gitclean=false; fi
ver1=${gittag#v}; ver1=${ver1%%-*};

echo Git version: $ver1 $gitbranch $gitcnt $USER $VER_BUILD

#create gitver.inc
echo "const VER_CLEAN=$gitclean;
const VER_HASH='$githash';
const VER_BRANCH='$gitbranch';
const VER_USER='$USER@$HOSTNAME';
const VER_MINOR=$ver1;
const VER_PATCH=$gitcnt;
const VER_BUILD=$VER_BUILD;" >gitver.inc

make -C alg
fpc @fpopt.cfg brodnetd.pas
fpc @fpopt.cfg bnc.pas
fpc @fpopt.cfg bnedit.pas

echo Built version: $gitver
