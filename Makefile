all: build
	./FlockSimulation

build:
	ghc --make -O3 -Wall Main.hs -o FlockSimulation

clean:
	rm -rf *.hi *.o FlockSimulation doc

doc:
	haddock Main.hs -h -o doc

tag:
	echo ":ctags" | ghci Main.hs > /dev/null
