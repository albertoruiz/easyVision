all:
	for p in base gui geometry classifier contours contrib/examples ipp hvision; \
	    do cd $$p; cabal install --force-reinstall --enable-documentation ; cd -; done

