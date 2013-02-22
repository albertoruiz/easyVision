stage3   = contours gui apps contrib/clipping contrib/hcontours
stage2   = imagproc camera $(stage3)
packages = util patrec vision $(stage2)
optional = gpu opencv models3ds examples zbar tesseract
all      = $(packages) $(optional)

forcefast = cabal install --disable-library-profiling --disable-documentation --force-reinstalls
fastinstall = cabal install --disable-library-profiling --disable-documentation
normalinstall = cabal install --enable-documentation
fullinstall = cabal install --enable-library-profiling --enable-documentation
profinstall = cabal install --enable-library-profiling --enable-executable-profiling --disable-documentation

basic:
	for p in $(packages); do cd $$p; $(normalinstall) ; cd -; done

optional:
	cd contrib; for p in $(optional); do cd $$p; $(normalinstall) ; cd ..; done; cd ..

optionalfast:
	cd contrib; for p in $(optional); do cd $$p; $(fastinstall) ; cd ..; done; cd ..

optionalprof:
	cd contrib; for p in $(optional); do cd $$p; $(profinstall) ; cd ..; done; cd ..

prof:
	for p in $(packages); do cd $$p; $(profinstall) ; cd -; done;	cd contrib; for p in $(optional); do cd $$p; $(profinstall) ; cd ..; done; cd ..


fast3:
	for p in $(stage3); do cd $$p; $(fastinstall) ; cd -; done

fast2:
	for p in $(stage2); do cd $$p; $(fastinstall) ; cd -; done

fast:
	for p in $(packages); do cd $$p; $(fastinstall) ; cd -; done

clean:
	for p in $(packages); do cd $$p; cabal clean; cd -; done

unregister:
	ghc-pkg unregister prcv-util --force

reinstall:
	for p in $(packages); do cd $$p; cabal clean; $(forcefast); cd -; done

