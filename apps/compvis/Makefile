IPP=/opt/intel/ipp/5.0/ia32/

LIBS= -i.. ../ImagProc/Ipp/auxIpp.o -L$(IPP)/sharedlib -lguide -lippcore -lippi -lipps -lippcc -lippvc -lippcv

all: hessian warp frontal pose automatch hmplayer ransac stereo detect

%: %.hs auxc
	ghc -O --make $< -o $@ $(LIBS)

auxc: ../ImagProc/Ipp/auxIpp.o

../ImagProc/Ipp/auxIpp.o: ../ImagProc/Ipp/auxIpp.c ../ImagProc/Ipp/auxIpp.h
	gcc -O -c -I$(IPP)/include ../ImagProc/Ipp/auxIpp.c -o ../ImagProc/Ipp/auxIpp.o

clean:
	rm -f a.out *.o *.hi ../EasyVision/*.o ../EasyVision/*.hi ../ImagProc/*.o ../ImagProc/*.hi ../ImagProc/Ipp/*.o ../ImagProc/Ipp/*.hi ../Vision/*.o ../Vision/*.hi
