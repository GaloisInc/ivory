all: test

IVORY_EX_TEST_DIR=test-dir-c-files
BIN=.cabal-sandbox/bin

PACKAGE= \
  ivory \
  ivory-artifact \
  ivory-backend-acl2 \
  ivory-backend-c \
  ivory-eval \
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

# Can't do `cabal run` since there's no cabal file at the top level. Also,
# binaries are built in the top-level .cabal file, but not tests.
.PHONY: test
test: build
	./$(BIN)/ivory-c-clang-test $(IVORY_EX_TEST_DIR)
	cd $(IVORY_EX_TEST_DIR) && gcc -Wall -Wextra -I. -std=c99 -c *.c *.h -Wno-missing-field-initializers -Wno-unused-parameter -Wno-unused-variable -DIVORY_DEPLOY

	# The following are cabal "test" targets
	./$(shell find ivory-model-check/dist/ -path "*/test/test")
	./$(shell find ivory-eval/dist/ -path "*/test/test")
	./$(shell find ivory-quickcheck/dist/ -path "*/test/test")

.PHONY: veryclean
veryclean:
	-rm -rf cabal.sandbox.config
	-rm -rf .cabal-sandbox
	-rm -rf dist
	-rm -rf $(IVORY_EX_TEST_DIR)
