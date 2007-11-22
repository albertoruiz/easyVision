IPP=/opt/intel/ipp/5.2_beta/ia32

LIBS = -i../lib ../lib/ImagProc/Ipp/auxIpp.o -L../lib/ImagProc/Segments -lsegments \
       -L$(IPP)/sharedlib -lguide -lippcore -lippi -lipps -lippcc -lippvc -lippcv

ALL = demo hessian warp frontal pose automatch brush \
      stereo detect interpolate rectangles hotpoints \
      autostereo augmented contours dynamic justsave \
      whiteboard autopano virpano simpleclassifier classifier

CDIR = ../lib/ImagProc/Ipp

all: $(ALL) 

%: %.hs auxc
	ghc -O -fvia-C -threaded --make $< -o $@ $(LIBS)

auxc: $(CDIR)/auxIpp.o ../lib/ImagProc/Segments/libsegments.a

$(CDIR)/auxIpp.o: $(CDIR)/auxIpp.c $(CDIR)/auxIpp.h
	gcc -O -c -I$(IPP)/include $(CDIR)/auxIpp.c -o $(CDIR)/auxIpp.o

../lib/ImagProc/Segments/libsegments.a:
	cd ../lib/ImagProc/Segments; make

clean:
	rm -f $(ALL) *.o *.hi ../lib/*.o ../lib/*.hi \
        ../lib/EasyVision/*.o ../lib/EasyVision/*.hi \
        ../lib/ImagProc/*.o ../lib/ImagProc/*.hi \
        ../lib/ImagProc/Ipp/*.o ../lib/ImagProc/Ipp/*.hi \
        ../lib/Vision/*.o ../lib/Vision/*.hi \
        ../lib/Classifier/*.o ../lib/Classifier/*.hi
