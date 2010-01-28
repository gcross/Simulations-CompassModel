#@+leo-ver=4-thin
#@+node:gcross.20091211100630.1234:@thin Makefile
#@@language Makefile
#@@tabwidth 4

all: \
	programs/simulate-compass \
	programs/simulate-compass-pbc \

LIBS = 

HFLAGS = -O2 -fvia-C -optc=-O3 -isources

GHCMAKE = ghc --make ${HFLAGS} ${LIBS}

programs/%: sources/%.hs Makefile
	${GHCMAKE} $< -o $@

clean:
	rm -f programs/* sources/*.o sources/*.hi
#@-node:gcross.20091211100630.1234:@thin Makefile
#@-leo
