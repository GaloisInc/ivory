
default:
	cabal build

clean-sandbox:
	-rm -rf .cabal-sandbox
	-rm cabal.sandbox.config
	-rm -rf dist

create-sandbox:
	cabal sandbox init
	cabal install --dependencies-only

