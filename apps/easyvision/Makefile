IPP=/opt/intel/ipp/5.0/ia32/

LIBS= -i.. ../Ipp/mycvCamera.o ../Ipp/auxIpp.o -L$(IPP)/sharedlib -lguide -lippcore -lippi -lipps -lippcc -lippvc -lraw1394 -ldc1394_control

all: hessian warp player frontal pose automatch

automatch: auxc automatch.hs
		ghc -O --make automatch.hs -o automatch $(LIBS)

frontal: auxc frontal.hs
		ghc -O --make frontal.hs -o frontal $(LIBS)

pose: auxc pose.hs
		ghc -O --make pose.hs -o pose $(LIBS)

hessian: auxc hessian.hs
		ghc -O --make hessian.hs -o hessian $(LIBS)

player: auxc player.hs
		ghc -O --make player.hs -o player $(LIBS)

warp: auxc warp.hs
		ghc -O --make warp.hs -o warp  $(LIBS)

auxc: ../Ipp/mycvCamera.o ../Ipp/auxIpp.o

../Ipp/mycvCamera.o: ../Ipp/mycvCamera.c ../Ipp/mycvCamera.h
		gcc -O -c -I$(IPP)/include ../Ipp/mycvCamera.c -o ../Ipp/mycvCamera.o

../Ipp/auxIpp.o: ../Ipp/auxIpp.c ../Ipp/auxIpp.h
		gcc -O -c -I$(IPP)/include ../Ipp/auxIpp.c -o ../Ipp/auxIpp.o

clean:
		rm -f a.out *.o *.hi ../Ipp/*.o ../Ipp/*.hi ../Vision/*.o ../Vision/*.hi
