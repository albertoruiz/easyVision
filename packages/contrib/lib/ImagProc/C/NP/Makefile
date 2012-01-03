lib:
	gcc -O3 -c npcp1.c -fPIC
	gcc -O3 -c npcp2.c -fPIC
	gcc -O3 -c npcp3.c -fPIC -msse2 -I.
	gcc npcp1.o npcp2.o npcp3.o -shared -o ../../../../../../lib/libnpcp.so

