OBJ = mycvRunnings.o  mycvTablesEdgels.o  mycvUtilsSegs.o mycvSegments.o  mycvUtils.o

IPP =/opt/intel/ipp/5.3/ia32/include

libsegments.a: $(OBJ)
	ar -cru libsegments.a $(OBJ)

%.o: %.c
	gcc -I$(IPP) -O4 -c $<

clean:
	rm -rf libsegments.a $(OBJ)