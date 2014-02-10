hVision Help

# [hVision][project] tutorial

_hVision is a suite of Haskell packages for computer vision, image processing,
pattern recognition and related applications_



## installation

[installation instructions](git)


## working environment

We need just an editor and a command line terminal, but
I recommend that you configure your editor to compile and run programs directly using shortcut keys. I use gedit with "snippets" and "external tools" configured to do many useful tasks.

It is also convenient to open a navigation tab with the local Haddock documentation page
created by "cabal install" with the option "--enable-documentation".

## hello world!

Show a single image:

CODEFILE(../examples/hello.hs)

**play**, **nplay**: show a live video sequence captured from the default input source (webcam):

CODEFILE(../examples/play.hs)

CODEFILE(../tour/nplay.hs)

**chanShow**:

CODEFILE(../tour/chanShow.hs)

**matrix**: use matrix functions on images

CODEFILE(../tour/matrix.hs)

**playgray**:

CODEFILE(../tour/playgray.hs)

**play1**: show different stages of the processing pipeline

CODEFILE(../tour/play1.hs)

**arrIO**: Apply an IO operation to each element in the input pipeline:

CODEFILE(../tour/arrIO.hs)

**arrows**: Arrow notation (include diagram):

**loop**: circuit loop in arrow notation:

CODEFILE(../tour/loop.hs)

**choice**: circuit with choice.

CODEFILE(../tour/choice.hs)

**circuit**, **nocircuit**: arrow notation and recursive do.

**fast-slow**: pipeline at two frame rates. Check that there is no leaks.

**play0**: create a pipeline step explicitly using transUI

**play3**: insert frames in the video sequence.

CODEFILE(../tour/play3.hs)

**play4**: add frame rate measurement window:

CODEFILE(../tour/play4.hs)

**testwebcam**: explicitly open the webcam image source.

CODEFILE(../tour/testwebcam.hs)


**play5**: the pipeline can be used with sequences of any kind of information. This is a clock:

CODEFILE(../tour/play5.hs)

**play6**: In this example we generate random numbers at about 1000Hz, and take the average of each group of 100 elements, producing a final 10Hz output.

CODEFILE(../tour/play6.hs)


**runmode0**: work without GUI

CODEFILE(../tour/runmode0.hs)

**runmode1**: normal threaded mode

CODEFILE(../tour/runmode1.hs)

**runmode00**: explicitly open the input source and extract elements:

CODEFILE(../tour/runmode00.hs)


**runmode01**: run a pipeline wihout a GUI:

CODEFILE(../tour/runmode01.hs)


**nogui**: run a pipeline without GUI:

CODEFILE(../tour/nogui.hs)




**runmode2**: threaded GUI, returning result:

CODEFILE(../tour/runmode01.hs)

**runmode02**: nonthreaded GUI, returning result:

CODEFILE(../tour/runmode02.hs)

**runmode3**: nonthreaded GUI, returning result:

CODEFILE(../tour/runmode3.hs)

**runmode03**: threaded GUI, returning result:

CODEFILE(../tour/runmode03.hs)

**runmode4**: nonthreaded GUI, returning result:

CODEFILE(../tour/runmode4.hs)

**runmode04**: load single image from the command line:

CODEFILE(../tour/runmode04.hs)

**runmode05**: load lazily all images in a directory:

CODEFILE(../tour/runmode05.hs)

**runmode06**: process pipeline and the work with it:

CODEFILE(../tour/runmode05.hs)


**runS**: same thing.

CODEFILE(../tour/runS.hs)

**scanl1**: scanl to perform a recursive computation. The video is shown inside the video. Run as

    ./scanl1 ../../data/videos/rcube.avi

CODEFILE(../tour/scanl1.hs)


**single**: read a list of image files and lazily work with them. Run as

    (fixme)

CODEFILE(../tour/single.hs)


**skip**: drop the first n frames of the input sequence

    (fixme)

CODEFILE(../tour/smon.hs)

**smon**: show different things in a window, chosen with the mouse wheel.


CODEFILE(../tour/smon.hs)


**passROI**: example of an interactive window to capture the region of interest.

CODEFILE(../tour/passROI.hs)

**interface**: example of an intercative window which captures clicked points:

CODEFILE(../tour/interface.hs)


**stand1**: "standalone" interactive graphic window. Click to change state:

CODEFILE(../tour/stand1.hs)

**stand2**: "browser" window. Use the wheel to see the elements of a list of things:

CODEFILE(../tour/stand2.hs)

**stand3**: "editor" window. change the elements of a list of things:

CODEFILE(../tour/stand3.hs)

**draw**, **drawParam**, **drawParam3D**: example of drawing functions.


**interactive3D**: add elements to a 3D graphic from ghci.

**param**, **param2**, **param3**: interactive parameter window:

CODEFILE(../tour/param.hs)

CODEFILE(../tour/param2.hs)

CODEFILE(../tour/param3.hs)

**connect**: connect windows

CODEFILE(../tour/connect.hs)

**clickPoints**: another example of the "connect" function, using the general purpuse "clickPoints" window

CODEFILE(../tour/clickPoints.hs)

**noleak**, **noleak2**: check that there are no space leaks caused by observed results no longer used.

**noguiraw**: tests of low level lazyIO functions.

**batch**, **batch2** (run modes?)

**cinematic**: very interesting.

- - -

WIP

**lm**

**grid**, **grid2**: fast slow?

**focus**

**concat**, **concat2**

**contrib**

- - -

[back to help][help]

INCLUDE(LINKS)

INCLUDE(HIGHLIGHT)

