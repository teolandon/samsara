# -----------------------------
# File:           Makefile
# Author:         Theo Kalfas
# Created:        2018-01-28
# -----------------------------

.PHONY: test

# Compiler:
CC = ocamlopt

# Test script
CLI_TEST = test/simplecli/compare-results.sh
SAMSARA_TEST = test/samsara/test.sh

# Compilation Flags:
FLAGS =

all: samsara

samsara: samsara.ml
	$(CC) -o samsara samsara.ml

simplecli: simplecli.ml
	$(CC) -o simplecli simplecli.ml

clean:
	rm -f simplecli samsara *.cmi *.cmx *.o test/simplecli/generated-results.out

test: simplecli samsara
	$(CLI_TEST)
	$(SAMSARA_TEST)
