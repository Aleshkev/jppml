.PHONY: interpreter, pack
.DEFAULT_GOAL := interpreter

interpreter:
	cabal update
	cabal build
	cp "$$(cabal list-bin exe:jppml | tail -n 1)" ./interpreter

pack:
	rm -f jonasz_aleszkiewicz.zip
	rm -rf jonasz_aleszkiewicz/

	git archive HEAD -o _flat.zip
	unzip _flat.zip -d jonasz_aleszkiewicz/
	zip jonasz_aleszkiewicz.zip -r jonasz_aleszkiewicz/
	
	rm -rf jonasz_aleszkiewicz/
	rm -f _flat.zip
