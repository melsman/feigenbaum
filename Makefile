PROGNAME=feigenbaum
include lib/github.com/diku-dk/lys/common.mk

.PHONY: doclean
doclean:
	$(MAKE) clean
	rm -rf *~ interp.c interp.h
