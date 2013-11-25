all:
	for p in base gui geometry patrec contours contrib/examples ipp; \
	    do cd $$p; cabal install --force-reinstall --enable-documentation ; cd -; done

