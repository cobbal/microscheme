GHCFLAGS=-Wall -XMultiParamTypeClasses

Main: *.hs
	ghc $(GHCFLAGS) *.hs

clean:
	rm -f Main *.o *.hi
