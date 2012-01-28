PRJ := hs-bonez

.PHONY: clean build build-dev deploy run

clean:
	cabal clean

build: clean
	cabal configure
	cabal build

build-dev: clean
	cabal configure -fdevelopment
	cabal build

deploy: build
	cabal install

run: build-dev
	cabal install -fdevelopment
	$(HOME)/.cabal/bin/hs-bonez
