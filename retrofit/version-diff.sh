#!/bin/bash

set -e

NUM=$1
CONFIGFILE=$(lowarn-cli locate)
PROJECTDIR=$(dirname "$CONFIGFILE")

cd "$PROJECTDIR"/.lowarn-retrofit/*/repo

while true
do
  COMMIT1=$(sed "${NUM}q;d" ../commit-map)
  COMMIT2=$(sed "$((NUM + 1))q;d" ../commit-map)

  echo "$NUM"
  echo "---"
  git diff --name-only "$COMMIT1..$COMMIT2"
  echo "---"
  read -r -p "Find out more (Y)? " yn
  case $yn in
    [Yy]* ) git show "$COMMIT2" --name-only; read -r; git diff "$COMMIT1..$COMMIT2" ;;
    * ) ;;
  esac
  NUM=$((NUM - 1))
done
