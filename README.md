Haskell packages for Computer Vision
====================================

This is an experimental [Haskell][haskell] framework for fast prototyping of computer vision and image processing applications. We take advantage of the expressive power of functional programming to write elegant programs supported by standard low level specialized libraries.

help
----

- [Introductory slides][slides]

- [Tutorial][tutorial] (in construction)

- [Blog][blog]

related work
------------

- There is an [early approach][yale] by the Yale Haskell Group.

- Computer Vision packages by [Noam Lewis][lewis] and [Ville Tirronen][tirronen] in Hackage.

- [Repa][repa]

- [JuicyPixels][JuicyPixels]

[haskell]: http://www.haskell.org
[slides]: http://dis.um.es/~alberto/material/ev1.pdf
[tutorial]: http://dis.um.es/~alberto/material/ev2.pdf
[blog]: http://covector.blogspot.com/
[yale]: http://haskell.cs.yale.edu/?post_type=publication&p=196
[lewis]: http://hackage.haskell.org/package/HOpenCV
[tirronen]: http://hackage.haskell.org/package/CV
[repa]: http://hackage.haskell.org/package/repa-examples
[JuicyPixels]: http://hackage.haskell.org/package/JuicyPixels

installation instructions
-------------------------

(Tested on Ubuntu 12.04)

1. Get the source code:

        $ git clone git://github.com/albertoruiz/easyVision.git

    Checkout the reorg branch:

        $ git checkout reorg

2. Install IPP. You can download the noncomercial version 7.1 for Linux
   (go to the link "Intel Integrated Performance Primitives (Intel IPP) 7.1 for Linux")

        http://software.intel.com/en-us/articles/non-commercial-software-download/

    Install using sudo in the default location.

3. Add the following environment variables to `~/.bashrc`:

        export IPP_INC=/opt/intel/composerxe/ipp/include
        export IPP_SHARED="/opt/intel/ipp/lib/ia32/:/opt/intel/lib/ia32/"
        export IPP_LIBS="ippcore ippi ipps ippcc ippvc ippcv iomp5"
        export IPP_LINK=-pthread
        export EASYVISION=/your/path/to/easyVision/
        export LD_LIBRARY_PATH=$IPP_SHARED:$EASYVISION/lib/lib32

    Modify as required to make sure that the IPP .h headers are in IPP_INC and 
    the corresponding .so libs are in IPP_SHARED.

    Continue installation in a new terminal.

4.  Install the Haskell Platform:

        $ sudo apt-get install haskell-platform
        $ cabal update

    Note that this branch works better with GHC 7.6.3. The official binary package is available from:

        https://www.haskell.org/ghc/download_ghc_7_6_3#x86linux
    
        $ ./configure --prefix=/path/to/desired/location/of/ghc7.6.3
        $ ./make install

    Change the path in .bashrc so this ghc is used instead of the ghc supplied by the Haskell Platform and
    continue the installation in a new terminal.

5. Install the required libraries:

        $ sudo apt-get install libgsl0-dev libatlas-base-dev libglpk-dev
        $ sudo apt-get install mplayer mencoder imagemagick

6. Install optional libraries:

    OPENCV:

        $ sudo apt-get install libcv-dev libcvaux-dev libhighgui-dev

    SIFTGPU:

        $ sudo apt-get install nvidia-current nvidia-cg-toolkit libdevil-dev g++

    tesseract:

        $ sudo apt-get install tesseract-ocr-dev

    zbar:
        
        $ sudo apt-get install libzbar-dev


7. Install the Haskell packages:

        $ cd easyVision/packages
        $ make

    The basic system will be correctly installed if "hVision-0.3.0" is shown by 

        $ ghc-pkg list

8. Run the demos:

        $ cd ../projects/demos
        $ make
        $ make demo

9. Read the tutorial.

10. Enjoy!

