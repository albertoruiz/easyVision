installation instructions
-------------------------

(Tested on Ubuntu x86)

1. Install the Haskell Platform (we need ghc and cabal-install) and run:

        $ cabal update

2. Install IPP. You can download a noncomercial version for Linux:

    http://software.intel.com/en-us/articles/non-commercial-software-download/

    Add to .bahsrc:

    For IPP 7.0, 32 bit:

        export IPP_INC=/opt/intel/composerxe/ipp/include
        export IPP_SHARED="/opt/intel/composerxe/ipp/lib/ia32:/opt/intel/compilerpro-12.0.2.137/compiler/lib/ia32/"
        export IPP_LIBS="ippcore ippi ipps ippcc ippvc ippcv iomp5"
        export IPP_LINK=-pthread 

    For IPP 5.3, 32 bit:

        export IPP_INC=/path/to/your/ipp/path/ia32/include
        export IPP_SHARED=/path/to/your/ipp/path/ia32/sharedlib
        export IPP_LIBS=guide ippcore ippi ipps ippcc ippvc ippcv
        export IPP_LINK=

    For IPP 5.3, 64 bit:

        export IPP_INC=/path/to/your/ipp/path/ia32/include
        export IPP_SHARED=/path/to/your/ipp/path/ia32/sharedlib
        export IPP_LIBS=guide ippcoreem64t ippiem64t ippsem64t ippccem64t ippvcem64t ippcvem64t
        export IPP_LINK=-pthread

3. Get the source code:

        $ git clone git://github.com/AlbertoRuiz/easyVision.git

4. Add to .bahsrc:

        export EASYVISION=/your/path/to/easyVision/
        export LD_LIBRARY_PATH=$IPP_SHARED:$EASYVISION/lib

5. Install the foreign libraries. In a new terminal:

        $ sudo apt-get install libgsl0-dev liblapack-dev libglpk-dev
        $ sudo apt-get install libghc6-glut-dev
        $ sudo apt-get install mplayer imagemagick libglew1.5-dev lib3ds-dev

    OPENCV (Optional):

        $ sudo apt-get install libcv-dev libcvaux-dev libhighgui-dev

    CUDA (Optional, currently only for i386):

        $ sudo apt-get install nvidia-current nvidia-cg-toolkit libdevil-dev g++


7. Install the packages:

        $ cd easyVision/packages
        $ make

    The basic system will be correctly installed if "ev-apps-0.1.0" is shown by 

        $ ghc-pkg list

8. Run the demos:

        $ cd ../projects/demos
        $ make
        $ ./demo.sh
        $ cd ../tour
        $ make
        $ ./demo.sh

9. Enjoy!

