#!/bin/bash

# simplecli-test generates the output of the test cases for simplecli

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

ARGS="${DIR}/args.in"
CLI="${DIR}/../simplecli"

while read args; do
  echo "------ ./simplecli $args ------"
  $CLI $args
done < $ARGS
