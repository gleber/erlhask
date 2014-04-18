.PHONY: samples

all: samples

run:
	runhaskell -XStandaloneDeriving ErlHask.hs

samples:
	(cd samples; make)

clean:
	rm -f *.hi *.o *~
