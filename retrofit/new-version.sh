#!/bin/bash

set -e

OLDVERSION=$1
CONFIGFILE=$(lowarn-cli locate)
PROJECTDIR=$(dirname "$CONFIGFILE")

COMMITID=$(sed "$((OLDVERSION + 1))q;d" "$PROJECTDIR"/.lowarn-retrofit/*/commit-map)

cd "$PROJECTDIR/versions"

NEWVERSION=$(find . -maxdepth 1 -mindepth 1 -type d -printf "%f\n" | sort | head -n 1)

lowarn-cli retrofit version --version "$OLDVERSION" apply source

CABALVERSION=$(awk '/^version:\s+(.+)/{ print $2 }' ./"$OLDVERSION"/source/*.cabal)

cp "./$NEWVERSION/simplify.patch" "./$OLDVERSION/simplify.patch"
sed -i -e "s/$NEWVERSION/$OLDVERSION/g;" "./$OLDVERSION/simplify.patch"
cp "./$NEWVERSION/retrofit.patch" "./$OLDVERSION/retrofit.patch"
sed -i -e "s/$NEWVERSION/$OLDVERSION/g;" "./$OLDVERSION/retrofit.patch"

cat >"./$OLDVERSION/version-info.yaml" <<EOL
commit: $COMMITID
type:
version: $CABALVERSION
EOL

lowarn-cli retrofit version --version "$OLDVERSION" apply simplify

new-update.sh "$OLDVERSION" "$NEWVERSION"
