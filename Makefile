all: build

BIN=.cabal-sandbox/bin

PACKAGE= \
  ivory \
  ivory-artifact \
  ivory-backend-aadl \
  ivory-backend-acl2 \
  ivory-backend-c \
  ivory-examples \
  ivory-hw \
  ivory-model-check \
  ivory-opts \
  ivory-quickcheck \
  ivory-serialize \
  ivory-stdlib

PACKAGEDIR=$(foreach p, $(PACKAGE), $(p)/)

cabal.sandbox.config:
	cabal sandbox init
	echo "tests: True" >> cabal.sandbox.config

.PHONY: build
build: cabal.sandbox.config
	cabal sandbox add-source $(PACKAGEDIR)
	cabal install $(PACKAGEDIR)

.PHONY: test
test: build
	./$(BIN)/ivory-c-clang-test clang-test-dir
	./$(BIN)/ivory-fibtutorial
	./$(BIN)/ivory-concrete
	# Created from ivory-model-check
	./$(find . -name "test" | grep "\.\/dist/.*/test/test")

.PHONY: veryclean
veryclean:
	-rm -rf cabal.sandbox.config
	-rm -rf .cabal-sandbox
	-rm -rf dist
