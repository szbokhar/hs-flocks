all: build
	./Main

build: Main.hs Vector.hs Bird.hs
	ghc --make -O3 -Wall Main.hs

clean:
	rm *.hi *.o Main
	rm -rf doc

doc:
	haddock Main.hs -h -o doc -B /opt/local/share/doc/ghc
