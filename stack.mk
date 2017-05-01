default:
	stack build . --install-ghc

clean:

distclean: clean
	stack clean

.PHONY: default clean distclean
