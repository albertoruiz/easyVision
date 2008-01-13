EASYVISION=../../../
IPP=/opt/intel/ipp/5.3/ia32
INCLUDE = -I$(IPP)/include
LIBDIR = -L/usr/lib -L$(IPP)/sharedlib
LIBS = -lm -lippi -lippcc -lippcv -lGL -lglut
CC = gcc
CFLAGS = -g

OBJECTS=bitmap.o burns.o main.o

.SUFFIXES: .o .cpp .cxx

.c.o: 
	$(CC) $(CFLAGS) $(INCLUDE) -c -o $*.o $<

main: $(OBJECTS)
	$(CC) $(CFLAGS) -o $@ $(OBJECTS) $(INCLUDE) $(LIBDIR) $(LIBS)

Burns.hs: burns.h Burns.hsc
	hsc2hs $(INCLUDE) Burns.hsc

Burns.o: Burns.hs burns.o bitmap.o
	ghc --make Burns.hs burns.o bitmap.o ../$(EASYVISION)/lib/ImagProc/Ipp/auxIpp.o -i../$(EASYVISION)/lib $(LIBDIR) $(LIBS)

clean:
	rm -rf $(OBJECTS) main Burns Burns.hs Burns.o Burns.hi
