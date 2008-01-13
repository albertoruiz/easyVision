IPP=/opt/intel/ipp/5.3/ia32

ALL = demo hessian warp frontal pose automatch brush \
      stereo detect interpolate rectangles hotpoints \
      autostereo augmented contours dynamic justsave \
      ocr whiteboard autopano virpano simpleclassifier \
      classifier cards onlyRects catalog pseudocolor \
      segments demostereo

CDIR = ../lib/ImagProc/Ipp

CONTRIBPATH = ../lib/ImagProc/C
CONTRIB = $(CONTRIBPATH)/Segments/libsegments.a \
          $(CONTRIBPATH)/burns/burns.o

LIBS = -i../lib ../lib/ImagProc/Ipp/auxIpp.o \
       -L$(CONTRIBPATH)/Segments -lsegments \
       -i$(CONTRIBPATH)/burns $(CONTRIBPATH)/burns/burns.o $(CONTRIBPATH)/burns/bitmap.o \
       -L$(IPP)/sharedlib -lguide -lippcore -lippi -lipps -lippcc -lippvc -lippcv

all: $(ALL) 

%: %.hs auxc
	ghc -O -fvia-C -threaded --make $< -o $@ $(LIBS)

auxc: $(CDIR)/auxIpp.o $(CONTRIB)

$(CDIR)/auxIpp.o: $(CDIR)/auxIpp.c $(CDIR)/auxIpp.h
	gcc -O4 -c -I$(IPP)/include $(CDIR)/auxIpp.c -o $(CDIR)/auxIpp.o

$(CONTRIBPATH)/Segments/libsegments.a:
	cd $(CONTRIBPATH)/Segments/; make

$(CONTRIBPATH)/burns/burns.o:
	cd $(CONTRIBPATH)/burns/; make Burns.o

clean:
	rm -f $(ALL) *.o *.hi ../lib/*.o ../lib/*.hi \
        ../lib/EasyVision/*.o ../lib/EasyVision/*.hi \
        ../lib/ImagProc/*.o ../lib/ImagProc/*.hi \
        ../lib/ImagProc/Ipp/*.o ../lib/ImagProc/Ipp/*.hi \
        ../lib/Vision/*.o ../lib/Vision/*.hi \
        ../lib/Classifier/*.o ../lib/Classifier/*.hi
