IPP=/opt/intel/ipp/5.0/ia32/

LIBS= -i.. ../Ipp/mycvCamera.o ../Ipp/auxIpp.o -L$(IPP)/sharedlib -lguide -lippcore -lippi -lipps -lippcc -lippvc -lraw1394 -ldc1394_control
		
all: hessian warp player frontal

frontal: frontal.hs
		ghc -O --make frontal.hs -o frontal $(LIBS)

pose: pose.hs
		ghc -O --make pose.hs -o pose $(LIBS)

hessian: hessian.hs
		ghc -O --make hessian.hs -o hessian $(LIBS)
		
player: player.hs
		ghc -O --make player.hs -o player $(LIBS)		
		
warp: warp.hs
		ghc -O --make warp.hs -o warp  $(LIBS)
		
auxc: mycvCamera.o auxIpp.o
		
mycvCamera.o: mycvCamera.c mycvCamera.h
		gcc -O -c -I$(IPP)/include mycvCamera.c
		
auxIpp.o: auxIpp.c auxIpp.h
		gcc -O -c -I$(IPP)/include auxIpp.c
		
clean:
		rm -f a.out *.o *.hi
