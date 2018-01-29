# -----------------------------
# File:           Makefile
# Author:         Theo Kalfas
# Created:        2018-01-28
# -----------------------------

.PHONY = test

# Compiler:
CC = ocamlopt

# Test script
TEST = test/compare-results.sh

# Compilation Flags:
FLAGS =

all: simplecli

simplecli: simplecli.ml
	$(CC) -o simplecli simplecli.ml

clean:
	rm -f simplecli *.cmi *.cmx *.o test/generated-results.out

test: simplecli
	$(TEST)
