#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

SAMSARA="${DIR}/../../samsara"
FILES=$(ls $DIR/arith/)

cd "${DIR}/arith"
$SAMSARA $FILES > "${DIR}/generated.out" 2>&1
diff "${DIR}/correct.out" "${DIR}/generated.out"
