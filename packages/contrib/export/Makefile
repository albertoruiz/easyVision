GHCV=ghc-7.4.0.20111219
# ghc --version

GHC=/home/brutus/apps/ghc74020111219/lib/ghc-7.4.0.20111219
# ghc --print-libdir

CABAL=/home/brutus/.cabal/lib

all:
	ghc -Wall --make -O2 test.hs -o testh -v
	gcc -Wall -O3 -o testc test.c HTools.o \
        -I$(GHC)/include \
        -L$(CABAL)/hmatrix-0.13.1.0/$(GHCV) -lHShmatrix-0.13.1.0 -lgsl -llapack -lblas \
        -L$(CABAL)/vector-0.9/$(GHCV) -lHSvector-0.9 \
        -L$(CABAL)/primitive-0.4.0.1/$(GHCV) -lHSprimitive-0.4.0.1 \
	    -L$(CABAL)/storable-complex-0.2.1/$(GHCV) -lHSstorable-complex-0.2.1 \
        -L$(GHC)/process-1.1.0.1 -lHSprocess-1.1.0.1 \
        -L$(GHC)/directory-1.1.0.2 -lHSdirectory-1.1.0.2 \
        -L$(GHC)/unix-2.5.1.0 -lHSunix-2.5.1.0 \
        -L$(GHC)/old-time-1.1.0.0 -lHSold-time-1.1.0.0 \
        -L$(GHC)/old-locale-1.0.0.4 -lHSold-locale-1.0.0.4 \
        -L$(GHC)/filepath-1.3.0.0 -lHSfilepath-1.3.0.0 \
        -L$(GHC)/binary-0.5.1.0 -lHSbinary-0.5.1.0 \
        -L$(GHC)/containers-0.4.2.1 -lHScontainers-0.4.2.1 \
        -L$(GHC)/deepseq-1.3.0.0 -lHSdeepseq-1.3.0.0 \
        -L$(GHC)/bytestring-0.9.2.1 -lHSbytestring-0.9.2.1 \
        -L$(GHC)/array-0.4.0.0 -lHSarray-0.4.0.0 \
        -L$(GHC) -lHSrts \
        -L$(GHC)/base-4.5.0.0 -lHSbase-4.5.0.0 \
        -L$(GHC)/integer-gmp-0.4.0.0 -lHSinteger-gmp-0.4.0.0 \
        -lgmp \
        -L$(GHC)/ghc-prim-0.2.0.0 -lHSghc-prim-0.2.0.0 \
        -lm -lrt -ldl \


clean:
	rm -f testc testh *.hi *.o *_stub.h

