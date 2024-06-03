.PHONY: install test

all:
	dune build

install:
	dune install ocalisp

test:
	dune exec ocalisp -- -test rosetta-lisp/test
