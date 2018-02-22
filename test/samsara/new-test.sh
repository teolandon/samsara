#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

NUMS=$(seq -s, -w 0 100)

IFS=', ' read -r -a array <<< "$NUMS"

NUM=$(ls -1 "$DIR/arith" | wc -l)

NAME=$1
TEXT=$2
RESULT=$3

if [[ "$NAME" != *.arith ]]; then
  NAME="$NAME.arith"
fi

echo "$TEXT" > "$DIR/arith/${array[NUM]}-${NAME}"

printf '%s:\n%s\n' "${array[NUM]}-$NAME" "$RESULT" >> "$DIR/arith-correct.out"
