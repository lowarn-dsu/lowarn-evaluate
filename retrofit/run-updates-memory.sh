#!/bin/bash

set -e

CONFIGFILE=$(lowarn-cli locate)
PROJECTDIR=$(dirname "$CONFIGFILE")
cd "$PROJECTDIR"

NUM=${1:-1}

while true
do
  echo $NUM
  VERSION=$(find versions -maxdepth 1 -mindepth 1 -type d -printf "%f\n" | sort | sed -n "${NUM}p")
  echo "$VERSION"

  for _ in {1..10}; do
    sleep 0.5
    smem -c "uss pss rss" -P ^lowarn-cli | tail -n 1 | sed -E "s/^/$NUM,$VERSION/;s/\\s+$//;s/\\s+/,/g" >> memory-over-time.csv
  done

  pgrep lowarn-cli | xargs kill -s SIGUSR2
  NUM=$((NUM + 1))
done
