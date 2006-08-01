IPP=/opt/intel/ipp/5.0/ia32/

LIBS= -i.. ../Ipp/mycvCamera.o ../Ipp/auxIpp.o -L$(IPP)/sharedlib -lguide -lippcore -lippi -lipps -lippcc -lippvc -lraw1394 -ldc1394_control
		
all: hessian warp player frontal

frontal: auxc frontal.hs
		ghc -O --make frontal.hs -o frontal $(LIBS)

pose: pose.hs
		ghc -O --make pose.hs -o pose $(LIBS)

hessian: hessian.hs
		ghc -O --make hessian.hs -o hessian $(LIBS)
		
player: player.hs
		ghc -O --make player.hs -o player $(LIBS)		
		
warp: warp.hs
		ghc -O --make warp.hs -o warp  $(LIBS)
		
auxc: ../Ipp/mycvCamera.o ../Ipp/auxIpp.o
		
../Ipp/mycvCamera.o: ../Ipp/mycvCamera.c ../Ipp/mycvCamera.h
		gcc -O -c -I$(IPP)/include ../Ipp/mycvCamera.c -o ../Ipp/mycvCamera.o
		
../Ipp/auxIpp.o: ../Ipp/auxIpp.c ../Ipp/auxIpp.h
		gcc -O -c -I$(IPP)/include ../Ipp/auxIpp.c -o ../Ipp/auxIpp.o
		
clean:
		rm -f a.out *.o *.hi ../Ipp/*.o ../Ipp/*.hi
