IPPICV='http://sourceforge.net/projects/opencvlibrary/files/3rdparty/ippicv/ippicv_linux_20141027.tgz'

pkgs=tools base gui geometry classifier contours/contours ip/custom contrib/examples ip/opencv contours/clipping contours/hcontours contours/CLK ip/ippicv ip/ipp hvision devel

getippicv:
	if [ ! -e ../lib/ippicv_lnx ]; then   \
	  cd ../lib;                          \
	  wget $(IPPICV);                     \
	  tar xzvf ippicv_linux_20141027.tgz; \
	  rm ippicv_linux_20141027.tgz;       \
	fi


selpkgs=contours/contours contours/clipping contours/hcontours contours/CLK

cabalcmd =                         \
      for p in $(1); do            \
	  if [ -e $$p ]; then          \
	    cd $$p; cabal $(2) ; cd -; \
	  fi;                          \
	done

all:
	$(call cabalcmd, $(pkgs), install --force-reinstall --enable-documentation)

selected:
	$(call cabalcmd, $(selpkgs), install --force-reinstall)

fast:
	$(call cabalcmd, $(pkgs), install --force-reinstall)

clean:
	$(call cabalcmd, $(pkgs), clean)

