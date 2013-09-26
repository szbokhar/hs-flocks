all: build
	./Main

build: Main.hs Vector.hs Bird.hs
	ghc --make -Wall Main.hs

clean:
	rm *.hi *.o Main
