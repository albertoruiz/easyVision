packages = util patrec vision imagproc      \
           camera gui contours features ev  \
           contrib gpu opencv models3ds

all:
	for p in $(packages); do cd $$p; cabal install; cd ..; done

clean:
	for p in $(packages); do cd $$p; cabal clean; cd ..; done

unregister:
	ghc-pkg unregister prcv-util --force

fromgui:
	for p in gui ev contrib; do cd $$p; cabal install; cd ..; done

