all: test

BIN=.cabal-sandbox/bin

PACKAGE= \
  ivory \
  ivory-backend-aadl \
  ivory-backend-acl2 \
  ivory-backend-c \
  ivory-examples \
  ivory-hw \
  ivory-opts \
  ivory-quickcheck \
  ivory-serialize \
  ivory-stdlib
#   ivory-model-check \

.cabal-sandbox:
	cabal sandbox init

.PHONY: add-srcs
add-srcs: .cabal-sandbox
	cabal sandbox add-source $(PACKAGE)

.PHONY: build
build: add-srcs
	cabal install $(PACKAGE)

.PHONY: test
test: build
	./$(BIN)/ivory-c-clang-test clang-test-dir
	./$(BIN)/ivory-fibtutorial
	./$(BIN)/ivory-concrete

.PHONY: veryclean
veryclean:
	-rm -rf cabal.sandbox.confg
	-rm -rf .cabal-sandbox

