all: build
	./FlockSimulation

build:
	ghc --make -O3 -W Main.hs -o FlockSimulation

profile:
	ghc -prof -auto-all -O3 Main.hs -o FlockSimulation

clean:
	rm -rf *.hi *.o FlockSimulation doc

doc:
	haddock Main.hs -h -o doc

tag:
	echo ":ctags" | ghci Main.hs > /dev/null
