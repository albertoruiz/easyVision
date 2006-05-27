ipp=/opt/intel/ipp/5.0/ia32/
		
all:
		ghc -O --make Main.hs -L$(ipp)/sharedlib -lguide -lippcore -lippi -lipps
		
clean:
		rm -f a.out *.o *.hi
