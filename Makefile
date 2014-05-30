.PHONY: samples

all: samples

run:
	@echo
	@echo
	@echo
	@echo
	@echo
	cabal run

test:
	runhaskell -Wall test.hs

compile:
	@echo
	@echo
	@echo
	@echo
	@echo
	ghc -Wall Main.hs

samples:
	(cd samples; make)

clean:
	rm -f *.hi *.o *~
