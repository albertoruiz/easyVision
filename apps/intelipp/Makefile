IPP=/opt/intel/ipp/5.0/ia32/

LIBS=mycvCamera.o auxIpp.o -L$(IPP)/sharedlib -lguide -lippcore -lippi -lipps -lippcc -lippvc -lraw1394 -ldc1394_control
		
all: hessian warp		
		
hessian: auxc
		ghc -O --make hessian.hs -o hessian $(LIBS)
		
warp: auxc
		ghc -O --make warp.hs -o warp -i.. $(LIBS)
		
auxc: mycvCamera.o auxIpp.o
		
mycvCamera.o: mycvCamera.c mycvCamera.h
		gcc -O -c -I$(IPP)/include mycvCamera.c
		
auxIpp.o: auxIpp.c auxIpp.h
		gcc -O -c -I$(IPP)/include auxIpp.c		
		
clean:
		rm -f a.out *.o *.hi
