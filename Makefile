include stack.mk

all: test

IVORY_EX_TEST_DIR=test-dir-c-files

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

TEST_TARGETS=ivory-model-check ivory-eval ivory-quickcheck

.PHONY: test
test: default
	stack exec -- ivory-c-clang-test $(IVORY_EX_TEST_DIR)
	cd $(IVORY_EX_TEST_DIR) && gcc -Wall -Wextra -I. -std=c99 -c *.c *.h -Wno-missing-field-initializers -Wno-unused-parameter -Wno-unused-variable -DIVORY_DEPLOY

	stack test $(TEST_TARGETS)

.PHONY: veryclean
veryclean:
	stack clean
	-rm -rf $(IVORY_EX_TEST_DIR)

# Travis-ci specfic ############################################################

TRAVIS_STACK ?= stack --no-terminal --system-ghc --skip-ghc-check

travis-test:
	$(TRAVIS_STACK) build --test --no-run-tests --haddock --no-haddock-deps --pedantic
	$(TRAVIS_STACK) exec -- ivory-c-clang-test $(IVORY_EX_TEST_DIR)
	cd $(IVORY_EX_TEST_DIR) && gcc -Wall -Wextra -I. -std=c99 -c *.c *.h -Wno-missing-field-initializers -Wno-unused-parameter -Wno-unused-variable -DIVORY_DEPLOY
	$(TRAVIS_STACK) test $(TEST_TARGETS)
