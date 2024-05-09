.PHONY: interpreter
.DEFAULT_GOAL := interpreter

interpreter:
	cabal build
	cp "$$(cabal list-bin jppml | tail -n 1)" ./interpreter
