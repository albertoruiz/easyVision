packages = util patrec vision imagproc      \
           camera contours gui features ev  \
           contrib apps gpu opencv models3ds

fastinstall = cabal install --disable-library-profiling --disable-documentation

all:
	for p in $(packages); do cd $$p; cabal install; cd ..; done

fast:
	for p in $(packages); do cd $$p; $(fastinstall) ; cd ..; done

clean:
	for p in $(packages); do cd $$p; cabal clean; cd ..; done

unregister:
	ghc-pkg unregister prcv-util --force

fromgui:
	for p in gui ev contrib; do cd $$p; cabal install; cd ..; done

