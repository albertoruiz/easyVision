IPP =/opt/intel/ipp/5.0/ia32

LIBS = -i../lib ../lib/ImagProc/Ipp/auxIpp.o -L../lib/ImagProc/Segments -lsegments -L$(IPP)/sharedlib -lguide -lippcore -lippi -lipps -lippcc -lippvc -lippcv

ALL = demo hessian warp frontal pose automatch brush stereo detect interpolate rectangles autostereo augmented

all: $(ALL) 

%: %.hs auxc
	ghc -O --make $< -o $@ $(LIBS)

auxc: ../lib/ImagProc/Ipp/auxIpp.o ../lib/ImagProc/Segments/libsegments.a

../lib/ImagProc/Ipp/auxIpp.o: ../lib/ImagProc/Ipp/auxIpp.c ../lib/ImagProc/Ipp/auxIpp.h
	gcc -O -c -I$(IPP)/include ../lib/ImagProc/Ipp/auxIpp.c -o ../lib/ImagProc/Ipp/auxIpp.o

../lib/ImagProc/Segments/libsegments.a:
	cd ../lib/ImagProc/Segments; make

clean:
	rm -f $(ALL) *.o *.hi ../lib/*.o ../lib/*.hi ../lib/EasyVision/*.o ../lib/EasyVision/*.hi ../lib/ImagProc/*.o ../lib/ImagProc/*.hi ../lib/ImagProc/Ipp/*.o ../lib/ImagProc/Ipp/*.hi ../lib/Vision/*.o ../lib/Vision/*.hi ../lib/Classifier/*.o ../lib/Classifier/*.hi
