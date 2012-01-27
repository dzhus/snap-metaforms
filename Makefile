PRJ := hs-bonez

deploy-prod:
	cabal clean
	cabal install

deploy-dev:
	cabal clean
	cabal install -fdevelopment
	$(HOME)/.cabal/bin/hs-bonez
