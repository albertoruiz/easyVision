installation instructions
-------------------------

(Tested on Ubuntu)

1. Get the source code:

        $ git clone git://github.com/AlbertoRuiz/easyVision.git

2. Install IPP. You can download a noncomercial version for Linux:

        http://software.intel.com/en-us/articles/non-commercial-software-download/

    Install using sudo in the default location.

3. Add the following environment variables to .bahsrc (modify as required in 64bit machines):

        export IPP_INC=/opt/intel/composerxe/ipp/include
        export IPP_SHARED="/opt/intel/ipp/lib/ia32/:/opt/intel/lib/ia32/"
        export IPP_LIBS="ippcore ippi ipps ippcc ippvc ippcv iomp5"
        export IPP_LINK=-pthread
        export EASYVISION=/your/path/to/easyVision/
        export LD_LIBRARY_PATH=$IPP_SHARED:$EASYVISION/lib

    (continue installation in a new terminal)

4. Install the Haskell Platform:

        $ sudo apt-get install haskell-platform
        $ cabal update

5. Install the required libraries:

        $ sudo apt-get install libgsl0-dev liblapack-dev libglpk-dev
        $ sudo apt-get install libghc6-glut-dev mplayer mencoder imagemagick

6. Install optional libraries:

    OPENCV:

        $ sudo apt-get install libcv-dev libcvaux-dev libhighgui-dev

    CUDA (currently only for i386):

        $ sudo apt-get install nvidia-current nvidia-cg-toolkit libdevil-dev g++

    zbar:
        
        $ sudo apt-get install libzbar-dev

    3ds:

        $ sudo apt-get install libglew1.5-dev lib3ds-dev


7. Install the Haskell packages:

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

