all:
	cd packages; make
	cd packages; make optional
	cd projects; make

