default:
	stack build .

clean:

distclean: clean
	stack clean

.PHONY: default clean distclean
