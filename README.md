Haskell packages for Computer Vision
====================================

This is an experimental [Haskell][haskell] framework for fast prototyping of computer vision and image processing applications. We take advantage of the expressive power of functional programming to write elegant programs supported by standard low level specialized libraries.

[help](http://dis.um.es/~alberto/ev/help.html)

----

Part of this project depended on a noncomercial version of IPP which is no longer
available from Intel. We are in the process of replacing this dependency by custom
functions, opencv, and [IPPICV](http://code.opencv.org/projects/opencv/wiki/Opencv3), an IPP subset freely available for opencv users.

installation instructions
-------------------------

(Tested on Ubuntu 12.04 32bit)

This branch works better with ghc >= 7.8.3

1. Get the source code:

        $ git clone git://github.com/albertoruiz/easyVision.git

2. Add the following environment variables to `~/.bashrc`:

        export EASYVISION=/your/path/to/easyVision/
        export LD_LIBRARY_PATH=$EASYVISION/lib/lib32

2. (IPP 7.1) Add the following environment variables to `~/.bashrc`:

        export IPP_INC=/opt/intel/composerxe/ipp/include
        export IPP_SHARED="/opt/intel/ipp/lib/ia32/:/opt/intel/lib/ia32/"
        export IPP_LIBS="ippcore ippi ipps ippcc ippvc ippcv iomp5"
        export LD_LIBRARY_PATH=$IPP_SHARED:$EASYVISION/lib/lib32
        export IPP_LINK=-pthread

    Modify as required to make sure that the IPP .h headers are in IPP_INC and 
    the corresponding .so libs are in IPP_SHARED.

    Continue installation in a new terminal.


5. Install the required libraries:

        $ sudo apt-get install libgsl0-dev libatlas-base-dev libglpk-dev
        $ sudo apt-get install mplayer mencoder imagemagick

    It is also recommended that you install opencv including the nonfree modules.

6. Optional: cuda, tesseract, zbar, etc.


7. Install the Haskell packages:

        $ cd easyVision/packages
        $ make getippicv
        $ make

    The basic system will be correctly installed if several hVision-* packages are shown by

        $ ghc-pkg list

8. Run the demos:

        $ cd ../projects/tour
        $ make play
        $ ./play

Sorry, at the moment most example programs depend on IPP 7.1.

