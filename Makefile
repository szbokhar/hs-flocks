all:
	ghc --make -Wall Main.hs
	./Main

clean:
	rm *.hi *.o Main
