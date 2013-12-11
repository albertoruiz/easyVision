pkgs=base gui geometry classifier contours custom contrib/examples contrib/opencv contrib/clipping contrib/hcontours ipp hvision

all:
	for p in $(pkgs); \
	    do cd $$p; cabal install --force-reinstall --enable-documentation ; cd -; done

local:
	for p in $(pkgs); \
	    do cd $$p; cabal sdist && uploadlocal ; cd -; done

