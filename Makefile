.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop lib

play:
	OCAMLRUNPARAM=b dune exec bin/play.exe

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

zip:
	rm -f crossword.zip
	zip -r crossword.zip . -x@exclude.lst

clean:
	dune clean
	rm -f crossword.zip

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh
