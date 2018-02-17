# -----------------------------
# File:           Makefile
# Author:         Theo Kalfas
# Created:        2018-01-28
# -----------------------------

.PHONY: test

# Compiler:
CC = ocamlbuild

# Test script
CLI_TEST = test/simplecli/compare-results.sh
SAMSARA_TEST = test/samsara/test.sh

# Compilation Flags:
FLAGS = -use-menhir -use-ocamlfind

all: samsara

samsara: parser.mly lexer.mll samsara.ml
	$(CC) $(FLAGS) samsara.native

simplecli: simplecli.ml
	ocamlopt -o simplecli simplecli.ml
	rm -f simplecli.o simplecli.cmx simplecli.cmi

clean:
	rm -f simplecli samsara.native *.cmo *.cmi *.cmx *.o test/samsara/generated.out test/simplecli/generated.out
	rm -r _build

test: simplecli samsara
	$(CLI_TEST)
	$(SAMSARA_TEST)
