
default:
	cabal build

create-sandbox:
	cabal sandbox init
	cabal sandbox add-source ../ivory
	cabal sandbox add-source ../ivory-opts
	cabal sandbox add-source ../ivory-backend-c
	cabal sandbox add-source ../ivory-artifact
	cabal install --dependencies-only
