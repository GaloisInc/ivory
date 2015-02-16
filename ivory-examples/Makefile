
create-sandbox:
	cabal sandbox init
	cabal sandbox add-source ../ivory
	cabal sandbox add-source ../ivory-hw
	cabal sandbox add-source ../ivory-opts
	cabal sandbox add-source ../ivory-stdlib
	cabal sandbox add-source ../ivory-backend-c
	cabal sandbox add-source ../ivory-artifact
	cabal sandbox add-source ../ivory-eval
	cabal sandbox add-source ../ivory-quickcheck
	cabal install --dependencies-only

# -------------------

.PHONY : clean
clean :
	rm -rf dist
