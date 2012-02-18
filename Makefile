.PHONY: clean install install-dev

clean:
	cabal clean

install-dev: clean
	cabal-dev install --force-reinstalls -fdevelopment

install: clean
	cabal install
