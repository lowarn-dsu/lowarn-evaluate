#!/bin/bash

set -e

OLDVERSION=$1
NEWVERSION=$2
CONFIGFILE=$(lowarn-cli locate)
PROJECTDIR=$(dirname "$CONFIGFILE")

DIRNAME="$OLDVERSION-$NEWVERSION"

cd "$PROJECTDIR/updates"

cp -r ../update-template/ "./$DIRNAME"
sed -i -e "s/OLDLOWARNVERSION/$OLDVERSION/g;s/NEWLOWARNVERSION/$NEWVERSION/g" "./$DIRNAME/package.yaml"
hpack "./$DIRNAME/package.yaml"
