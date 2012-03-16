stage3   = contours gui apps
stage2   = imagproc camera $(stage3)
packages = util patrec vision $(stage2)
optional = gpu opencv models3ds examples zbar clipping tesseract
all      = $(packages) $(optional)
old      = features old-gui ev 

fastinstall = cabal install --disable-library-profiling --disable-documentation
normalinstall = cabal install --enable-documentation
fullinstall = cabal install --enable-library-profiling --enable-documentation

all:
	for p in $(packages); do cd $$p; $(normalinstall) ; cd ..; done

optional:
	cd contrib; for p in $(optional); do cd $$p; $(normalinstall) ; cd ..; done; cd ..

prof:
	for p in $(packages); do cd $$p; $(fullinstall) ; cd ..; done

oldp:
	cd old; for p in $(old); do cd $$p; $(fastinstall) ; cd ..; done; cd ..

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

