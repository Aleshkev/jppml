.PHONY: interpreter
.DEFAULT_GOAL := interpreter

interpreter:
	cabal update
	cabal build
	cp "$$(cabal list-bin exe:jppml | tail -n 1)" ./interpreter
