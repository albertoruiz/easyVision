hVision Help

# hVision command line options

- - -

## video source selection

**Video files**

The programs using the "run" function take the input source from the command line. Video files can be given directly:

<pre><samp>
    ./program ../../data/videos/rot4.avi
</samp></pre>

We can add "suboptions":

<pre><samp>
    ./program ../../data/videos/rot4.avi:size=320x240
</samp></pre>

**Webcam**

The input "uvc" refers to webcams connected to the computer:

<pre><samp>
    ./program uvc:dev=1:size=640x480:fps=30
</samp></pre>

By default we use the webcam most recently connected, with standard parameters,
so in live video applications we usually don't need any command line argument:

    ./program

It is recommended that your editor / IDE is configured to be able to compile and run (or run in interpreted mode) the current program without the need of moving to a terminal.

The suboption "drop" in the source url is used to return the most recent captured frame.

The suboption "keep" in the source url is used to create a queue with all captures frames.
This is used to guarantee that no frame is lost in the process, but it may produce a memory leak.

**Directories of still images**

We can work with all images in a directory, which are supplied to the application in
a transparent way.

<pre><samp>
    ./program --photos=/path/to/images/
</samp></pre>

The user must explicitly move forward or backward pressing keys in the window.
Alternatively, the images can be automatically loaded without user participation:
    
<pre><samp>
    ./program --sphotos=/path/to/images/
</samp></pre>

- - -

[general help][help]

INCLUDE(LINKS)
    
INCLUDE(HIGHLIGHT)

