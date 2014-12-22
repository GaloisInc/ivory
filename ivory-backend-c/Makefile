
default:
	cabal build

create-sandbox:
	cabal sandbox init
	cabal sandbox add-source ../ivory
	cabal sandbox add-source ../ivory-opts
	cabal sandbox add-source ../ivory-artifact
	cabal install --dependencies-only

clean:
	-rm -rf dist
	-rm -rf cabal.sandbox.config
	-rm -rf .cabal-sandbox
