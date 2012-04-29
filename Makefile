projects = tour demos vision/geom vision/multiview patrec gpu

all:
	cd packages; make
	cd packages; make optional
	cd projects; make

