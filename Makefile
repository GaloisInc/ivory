include stack.mk

all: test

STACK_CMD ?= stack

IVORY_EX_TEST_DIR=test-dir-c-files

PACKAGE= \
  ivory \
  ivory-examples \
  ivory-opts \
  ivory-hw \
  ivory-quickcheck \
  ivory-stdlib \
  ivory-serialize \
  ivory-artifact \
  ivory-backend-c \
  ivory-eval

TEST_TARGETS=ivory-eval ivory-quickcheck

.PHONY: generic-test
generic-test: default
	$(STACK_CMD) build --test --no-run-tests --haddock --no-haddock-deps --pedantic
	$(STACK_CMD) test $(TEST_TARGETS)
	$(STACK_CMD) exec -- ivory-c-clang-test $(IVORY_EX_TEST_DIR)
	cp ivory-examples/data/*.h $(IVORY_EX_TEST_DIR)/
	cd $(IVORY_EX_TEST_DIR) &&			\
		gcc	-Wall -Wextra -Werror		\
			-Wno-missing-field-initializers	\
			-Wno-unused-parameter		\
			-Wno-unused-variable		\
			-DIVORY_DEPLOY -I. -std=c99 -c	\
			*.c *.h

.PHONY: test
test: generic-test

.PHONY: sdist
sdist:
	$(foreach p, $(PACKAGE), stack sdist $(p)/;)

.PHONY: veryclean
veryclean:
	stack clean
	-rm -rf $(IVORY_EX_TEST_DIR)

travis-test: generic-test
travis-test: STACK_CMD = stack --no-terminal
