ipp=/opt/intel/ipp/5.0/ia32/
		
all: auxc
		ghc -O --make Main.hs mycvCamera.o auxIpp.o -L$(ipp)/sharedlib -lguide -lippcore -lippi -lipps -lippcc -lippvc -lraw1394 -ldc1394_control 
		
auxc: mycvCamera.o auxIpp.o
		
mycvCamera.o: mycvCamera.c mycvCamera.h
		gcc -O -c -I$(ipp)/include mycvCamera.c
		
auxIpp.o: auxIpp.c auxIpp.h
		gcc -O -c -I$(ipp)/include auxIpp.c		
		
clean:
		rm -f a.out *.o *.hi
