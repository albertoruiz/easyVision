pkgs=tools base gui geometry classifier contours custom contrib/examples contrib/opencv contrib/clipping contrib/hcontours contrib/CLK ipp hvision devel

all:
	for p in $(pkgs); do \
	  if [ -e $$p ]; then \
	    cd $$p; cabal install --force-reinstall --enable-documentation ; cd -; \
	  fi; \
	done

fast:
	for p in $(pkgs); do \
	  if [ -e $$p ]; then \
	    cd $$p; cabal install --force-reinstall ; cd -; \
	  fi; \
	done

local:
	for p in $(pkgs); do \
	  if [ -e $$p ]; then \
	    cd $$p; cabal sdist && uploadlocal ; cd -; \
	  fi; \
	done

