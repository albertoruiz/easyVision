ipp=/opt/intel/ipp/5.0/ia32/
		
all:
		gcc -O -c -I$(ipp)/include mycvCamera.c
		ghc -O --make Main.hs mycvCamera.o -L$(ipp)/sharedlib -lguide -lippcore -lippi -lipps -lippcc -lippvc -lraw1394   -ldc1394_control 
		
clean:
		rm -f a.out *.o *.hi
