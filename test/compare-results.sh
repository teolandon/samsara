#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

SCRIPT="${DIR}/generate-results.sh"
CORRECT="${DIR}/correct-results.out"
GENERATED="${DIR}/generated-results.out"

. $SCRIPT > $GENERATED
diff $CORRECT $GENERATED
