# File:           Makefile
# Author:         Theo Kalfas
# Created:        2018-01-28
# -----------------------------------------------------------------------------

# Compiler:
CC = ocamlopt

# Compilation Flags:
FLAGS =

all: simplecli

simplecli: simplecli.ml
	$(CC) -o simplecli simplecli.ml

clean:
	rm -f simplecli *.cmi *.cmx *.o
