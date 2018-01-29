#!/bin/bash

# simplecli-test generates the output of the test cases for simplecli

ARGS="args.in"
CLI="$(pwd)/../simplecli"

while read args; do
  echo "------ ./simplecli $args ------"
  $CLI $args
done < $ARGS
