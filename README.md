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

(Tested on Ubuntu)

1. Get the source code:

        $ git clone git://github.com/albertoruiz/easyVision.git

2. Install IPP. You can download a noncomercial version for Linux:

        http://software.intel.com/en-us/articles/non-commercial-software-download/

    Install using sudo in the default location.

3. Add the following environment variables to `~/.bashrc`:

        export IPP_INC=/opt/intel/composerxe/ipp/include
        export IPP_SHARED="/opt/intel/ipp/lib/ia32/:/opt/intel/lib/ia32/"
        export IPP_LIBS="ippcore ippi ipps ippcc ippvc ippcv iomp5"
        export IPP_LINK=-pthread
        export EASYVISION=/your/path/to/easyVision/
        export LD_LIBRARY_PATH=$IPP_SHARED:$EASYVISION/lib/lib32

    Make sure that the IPP .h headers are in IPP_INC and the corresponding .so
    libs are in IPP_SHARED (modify as required in 64bit machines). Directory
    structure and required auxiliary libs frequently change in different IPP versions.

    Continue installation in a new terminal.

4. Install the Haskell Platform:

        $ sudo apt-get install haskell-platform
        $ cabal update

    You will probably get the message that a new version of cabal-install is available.
    Don't worry about that now.

5. Install the required libraries:

        $ sudo apt-get install libgsl0-dev liblapack-dev libglpk-dev
        $ sudo apt-get install libghc6-glut-dev mplayer mencoder imagemagick

6. Install optional libraries:

    ATLAS (optimized LAPACK):

        $ sudo apt-get install libatlas-base-dev

    OPENCV:

        $ sudo apt-get install libcv-dev libcvaux-dev libhighgui-dev

    SIFTGPU:

        $ sudo apt-get install nvidia-current nvidia-cg-toolkit libdevil-dev g++

    tesseract:

        $ sudo apt-get install tesseract-ocr-dev

    zbar:
        
        $ sudo apt-get install libzbar-dev

    3ds:

        $ sudo apt-get install libglew1.5-dev lib3ds-dev


7. Install the Haskell packages:

        $ cd easyVision/packages
        $ make

    The basic system will be correctly installed if "ev-apps-0.1.0" is shown by 

        $ ghc-pkg list

    You can also install the optional packages:

        $ make optional

    (Dont' worry if you get any error here)

8. Run the demos:

        $ cd ../projects/demos
        $ make
        $ make demo

9. Read the tutorial.

10. Enjoy!

