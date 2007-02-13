IPP=/opt/intel/ipp/5.0/ia32/

LIBS= -i.. ../Ipp/mycvCamera.o ../Ipp/auxIpp.o -L$(IPP)/sharedlib -lguide -lippcore -lippi -lipps -lippcc -lippvc -lippcv -lraw1394 -ldc1394_control

all: hessian warp frontal pose automatch hmplayer ransac stereo detect

%: %.hs auxc
	ghc -O --make $< -o $@ $(LIBS)

auxc: ../Ipp/mycvCamera.o ../Ipp/auxIpp.o

../Ipp/mycvCamera.o: ../Ipp/mycvCamera.c ../Ipp/mycvCamera.h
	gcc -O -c -I$(IPP)/include ../Ipp/mycvCamera.c -o ../Ipp/mycvCamera.o

../Ipp/auxIpp.o: ../Ipp/auxIpp.c ../Ipp/auxIpp.h
	gcc -O -c -I$(IPP)/include ../Ipp/auxIpp.c -o ../Ipp/auxIpp.o

clean:
	rm -f a.out *.o *.hi ../Ipp/*.o ../Ipp/*.hi ../Vision/*.o ../Vision/*.hi
