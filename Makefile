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

samsara: parser.cmx lexer.cmx samsara.cmx
	$(CC) -o samsara parser.cmx lexer.cmx samsara.cmx

parser.cmx: parser.ml parser.mli
	$(CC)    parser.mli
	$(CC) -c parser.ml

lexer.cmx: lexer.ml lexer.mli
	$(CC)    lexer.mli
	$(CC) -c lexer.ml

samsara.cmx: samsara.ml
	$(CC) -c samsara.ml

simplecli: simplecli.ml
	$(CC) -o simplecli simplecli.ml

clean:
	rm -f simplecli samsara *.cmo *.cmi *.cmx *.o test/simplecli/generated-results.out

test: simplecli samsara
	$(CLI_TEST)
	$(SAMSARA_TEST)
