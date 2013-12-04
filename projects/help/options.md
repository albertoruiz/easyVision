hVision Help

# hVision command line options

- - -

## video source selection

**video files**

The programs using the "run" function take the input source from the command line. Video files can be given directly:

    ./program ../../data/videos/rot4.avi

We can add "suboptions":

    ./program ../../data/videos/rot4.avi:size=320x240

**webcam**

The input "uvc" refers to webcams connected to the computer:

    ./program uvc:dev=1:size=640x480:fps=30


By default we use the webcam most recently connected, with standard parameters,
so in live video applications we usually don't need any command line argument:

    ./program

It is recommended that your editor / IDE is configured to be able to compile and run (or run in interpreted mode) the current program without the need of moving to a terminal.

**directories of still images**

We can work with all images in a directory, which are supplied to the application in
a transparent way.

    ./program --photos=/path/to/images/
    
    ./program --sphotos=/path/to/images/



- - -

[general GUI help][help]

INCLUDE(LINKS)

