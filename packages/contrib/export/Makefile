all:
	ghc --make -O2 Main.hs -v
	gcc -O3 pru.c Module.o\
        -I/home/brutus/apps/ghc74020111219/lib/ghc-7.4.0.20111219/include \
        -L/home/brutus/apps/ghc74020111219/lib/ghc-7.4.0.20111219 -lHSrts \
        -L/home/brutus/apps/ghc74020111219/lib/ghc-7.4.0.20111219/base-4.5.0.0 -lHSbase-4.5.0.0 \
        -L/home/brutus/apps/ghc74020111219/lib/ghc-7.4.0.20111219/integer-gmp-0.4.0.0 -lHSinteger-gmp-0.4.0.0 \
        -lgmp \
        -L/home/brutus/apps/ghc74020111219/lib/ghc-7.4.0.20111219/ghc-prim-0.2.0.0 -lHSghc-prim-0.2.0.0 \
        -lm -lrt -ldl

clean:
	rm -f a.out Main *.hi *.o *_stub.h

