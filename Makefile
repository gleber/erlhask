all: samples

run:
	runhaskell -XStandaloneDeriving ErlHask.hs

samples:
	(cd samples; make)
