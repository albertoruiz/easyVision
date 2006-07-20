IPP=/opt/intel/ipp/5.0/ia32/

LIBS=mycvCamera.o auxIpp.o -L$(IPP)/sharedlib -lguide -lippcore -lippi -lipps -lippcc -lippvc -lraw1394 -ldc1394_control
		
all: hessian warp player frontal

frontal: auxc
		ghc -O --make frontal.hs -o frontal -i.. $(LIBS)

hessian: auxc
		ghc -O --make hessian.hs -o hessian $(LIBS)
		
player: auxc
		ghc -O --make player.hs -o player $(LIBS)		
		
warp: auxc
		ghc -O --make warp.hs -o warp -i.. $(LIBS)
		
auxc: mycvCamera.o auxIpp.o
		
mycvCamera.o: mycvCamera.c mycvCamera.h
		gcc -O -c -I$(IPP)/include mycvCamera.c
		
auxIpp.o: auxIpp.c auxIpp.h
		gcc -O -c -I$(IPP)/include auxIpp.c		
		
clean:
		rm -f a.out *.o *.hi
