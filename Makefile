GHCFLAGS=-Wall -XMultiParamTypeClasses
PRODUCT=microscheme

$(PRODUCT): *.hs
	ghc $(GHCFLAGS) -o $(PRODUCT) *.hs

clean:
	rm -f $(PRODUCT) *.o *.hi
