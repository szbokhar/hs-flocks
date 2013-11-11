all: build
	./FlockSimulation

build:
	ghc --make -threaded -Odph -rtsopts -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000 -O3 -Wall Main.hs -o FlockSimulation

build-llvm:
	ghc --make -threaded -Odph -rtsopts -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000 -fllvm -optlo-O3 -Wall Main.hs -o FlockSimulation

clean:
	rm -rf *.hi *.o FlockSimulation doc

doc:
	haddock Main.hs -h -o doc

tag:
	echo ":ctags" | ghci Main.hs > /dev/null
