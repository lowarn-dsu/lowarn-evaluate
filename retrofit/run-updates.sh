#!/bin/bash

set -e

NUM=0

while true
do
  echo $NUM
  read -r
  pgrep lowarn-cli | xargs kill -s SIGUSR2
  NUM=$((NUM + 1))
done
