# File:           Makefile
# Author:         Theo Kalfas
# Created:        2018-01-28
# -----------------------------------------------------------------------------

# Compiler:
CC = ocamlopt

# Compilation Flags:
FLAGS =

all: simple-cli

simple-cli: simple-cli.ml
	$(CC) -o simple-cli simple-cli.ml

clean:
	rm -f simple-cli *.cmi *.cmx *.o
