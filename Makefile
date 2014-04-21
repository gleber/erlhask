.PHONY: samples

all: samples

run:
	@echo
	@echo
	@echo
	@echo
	@echo
	runhaskell -XStandaloneDeriving ErlHask.hs

samples:
	(cd samples; make)

clean:
	rm -f *.hi *.o *~
