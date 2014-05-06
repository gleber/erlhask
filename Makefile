.PHONY: samples

all: samples

run:
	@echo
	@echo
	@echo
	@echo
	@echo
	runhaskell -Wall Main.hs

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
