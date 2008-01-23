OBJ = mycvRunnings.o  mycvTablesEdgels.o  mycvUtilsSegs.o mycvSegments.o  mycvUtils.o

libsegments.a: $(OBJ)
	ar -cru libsegments.a $(OBJ)

%.o: %.c
	gcc -I$(IPP)/include -O4 -c $<

clean:
	rm -rf libsegments.a $(OBJ)