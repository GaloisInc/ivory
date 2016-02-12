default:
	stack build .
test:
	stack build . --test

clean:

distclean: clean
	stack clean

.PHONY: default test clean distclean
