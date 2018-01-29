#!/bin/bash

SCRIPT="generate-results.sh"
CORRECT="correct-results.out"
GENERATED="generated-results.out"

. $SCRIPT > $GENERATED
diff $CORRECT $GENERATED
