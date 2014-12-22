# for testing purposes
MAIN="int main(void) { return 0; }"

# Where to put all our generated files
TESTDIR=test_dir
IDIR=runtime
CFLAGS=-I$(IDIR) -O2

EXEC=cabal run ivory-c-clang-test

# Our tests
TESTS=PID.out FibLoop.out FunPtr.out String.out Factorial.out \
	  Float.out Alloc.out Area.out Cond.out Forever.out PublicPrivate.out \
	  Overflow.out AddrOfRegression.out

all : clean $(TESTS)

%.out : $(TESTDIR)/%.c
	echo $(MAIN) >> $<
	clang -Wall -Wno-missing-braces $(CFLAGS) -lm -DIVORY_TEST -o $(TESTDIR)/$@ $<

.PRECIOUS : $(TESTDIR)/%.c
$(TESTDIR)/%.c : examples/%.hs | $(TESTDIR)
	# Will generate all the files on the first run
	$(EXEC) $(TESTDIR)

$(TESTDIR):
	mkdir -p $@

# -- CBMC ------------

.PHONY : cbmc1 cbmc2
cbmc1 :
	cd $(TESTDIR); cbmc -I ../$(IDIR) -DIVORY_CBMC --function ovf1 Overflow.c
cbmc2 :
	cd $(TESTDIR); cbmc -I ../$(IDIR) -DIVORY_CBMC --function ovf2 Overflow.c

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
	rm -rf $(TESTDIR)
	rm -rf dist
