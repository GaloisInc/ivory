
default:
	cabal build

create-sandbox:
	cabal sandbox init
	cabal sandbox add-source ../ivory
	cabal sandbox add-source ../ivory-artifact
	cabal install --dependencies-only

