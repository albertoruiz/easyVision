IPPICV='http://sourceforge.net/projects/opencvlibrary/files/3rdparty/ippicv/ippicv_linux_20141027.tgz'

pkgs=tools base gui geometry classifier contours custom contrib/examples ip/opencv contrib/clipping contrib/hcontours contrib/CLK ippicv ipp hvision devel

getippicv:
	if [ ! -e ../lib/ippicv_lnx ]; then   \
	  cd ../lib;                          \
	  wget $(IPPICV);                     \
	  tar xzvf ippicv_linux_20141027.tgz; \
	  rm ippicv_linux_20141027.tgz;       \
	fi


selpkgs=contours contrib/clipping contrib/hcontours contrib/CLK

all:
	for p in $(pkgs); do \
	  if [ -e $$p ]; then \
	    cd $$p; cabal install --force-reinstall --enable-documentation ; cd -; \
	  fi; \
	done

selected:
	for p in $(selpkgs); do \
	  if [ -e $$p ]; then \
	    cd $$p; cabal install --force-reinstall ; cd -; \
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

