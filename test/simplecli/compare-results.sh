#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

SCRIPT="${DIR}/generate-results.sh"
CORRECT="${DIR}/correct.out"
GENERATED="${DIR}/generated.out"

. $SCRIPT > $GENERATED 2>&1
diff $CORRECT $GENERATED
