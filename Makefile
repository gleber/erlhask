.PHONY: samples

all: samples cabal.sandbox.config compile

cabal.sandbox.config:
	cabal sandbox init
	cabal update
	cabal install cabal-install
	cabal install --only-dep --enable-tests
	cabal configure --enable-tests

run:
	@echo
	@echo
	@echo
	@echo
	@echo
	cabal run

test:
	cabal test

compile:
	@echo
	@echo
	@echo
	@echo
	@echo
	cabal build

samples:
	(cd samples; make)

clean:
	rm -f *.hi *.o *~

clean-dist:
	rm -f dist .cabal-sandbox
