#!/bin/bash

RED='\033[0;31m'
GREEN='\033[0;32m'
CLEAR='\033[0m'

function pass() {
  echo -e "${GREEN}Passed${CLEAR}"
}

function fail() {
  echo -e "${RED}Failed${CLEAR}"
  FAILED=1
}

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

SAMSARA="${DIR}/../../samsara"

cd "${DIR}/arith"
FILES=$(ls)

$SAMSARA $FILES > "${DIR}/arith-generated.out" 2>&1

cd "${DIR}/lex"
FILES=$(ls)

$SAMSARA -lex $FILES > "${DIR}/lex-generated.out" 2>&1

cd "${DIR}/parse"
FILES=$(ls)

$SAMSARA -parse $FILES > "${DIR}/parse-generated.out" 2>&1

echo -n "Evaluation tests: "
diff "${DIR}/arith-correct.out" "${DIR}/arith-generated.out"

if [[ $? == 0 ]]; then
  pass
else
  fail
fi

echo -n "Lexing tests: "
diff "${DIR}/lex-correct.out" "${DIR}/lex-generated.out"

if [[ $? == 0 ]]; then
  pass
else
  fail
fi

echo -n "Parsing tests: "
diff "${DIR}/parse-correct.out" "${DIR}/parse-generated.out"

if [[ $? == 0 ]]; then
  pass
else
  fail
fi

if [[ -z $FAILED ]]; then
  exit 0;
else
  exit 1;
fi
