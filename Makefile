all: build
	./FlockSimulation

build:
	ghc --make -O3 -W Main.hs -o FlockSimulation

clean:
	rm *.hi *.o FlockSilumation
	rm -rf doc

doc:
	haddock Main.hs -h -o doc

tag:
	echo ":ctags" | ghci Main.hs > /dev/null
