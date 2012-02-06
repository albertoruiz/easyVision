stage3   = contours gui apps contrib
stage2   = imagproc camera $(stage3)
packages = util patrec vision $(stage2)
optional = gpu opencv models3ds
all      = $(packages) $(optional)
old      = features old-gui ev 

fastinstall = cabal install --disable-library-profiling --disable-documentation
fullinstall = cabal install --enable-library-profiling  --enable-documentation

all:
	for p in $(packages); do cd $$p; $(fullinstall) ; cd ..; done

optional:
	for p in $(optional); do cd $$p; $(fastinstall) ; cd ..; done

old:
	for p in $(old); do cd $$p; $(fastinstall) ; cd ..; done

fast3:
	for p in $(stage3); do cd $$p; $(fastinstall) ; cd ..; done

fast2:
	for p in $(stage2); do cd $$p; $(fastinstall) ; cd ..; done

fast:
	for p in $(packages); do cd $$p; $(fastinstall) ; cd ..; done

clean:
	for p in $(packages); do cd $$p; cabal clean; cd ..; done

unregister:
	ghc-pkg unregister prcv-util --force

fromgui:
	for p in gui ev contrib; do cd $$p; cabal install; cd ..; done

