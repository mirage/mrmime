.PHONY: build clean test doc

build:
	dune build

doc:
	dune build @doc

test:
	dune runtest

clean:
	dune clean
