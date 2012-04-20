Main: *.hs
	ghc -Wall *.hs

clean:
	rm -f Main *.o *.hi
