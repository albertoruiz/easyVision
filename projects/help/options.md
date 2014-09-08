!TITLE(hVision Help - command line options)

# hVision command line options

- - -

## input source selection

The programs using the "run" function take the input source from the command line.

HSFILE(../tour/play.hs)




**Video files**

Video files can be given directly:

<pre id="samp">
    ./play ../../data/videos/rot4.avi
</pre>

We can add "suboptions":

<pre id="samp">
    ./play ../../data/videos/rot4.avi:size=320x240
</pre>

**Webcam**

The input "uvc" refers to webcams connected to the computer:

<pre id="samp">
    ./play uvc:dev=1:size=640x480:fps=30
</pre>

By default we use the webcam most recently connected, with reasonable default parameters,
so in live video applications we usually don't need any command line argument:

<pre id="samp">
    ./play
</pre>

It is recommended that your editor / IDE is configured to be able to compile and run (or run in interpreted mode) the current program without the need of moving to a terminal.

**"drop" and "keep"**

The suboption "drop" in the source url is used to return the most recent captured frame. This is useful to discard older frames that for some reason have not been processed and are no longer required.

The suboption "keep" in the source url creates a fifo queue with all captured frames.
This is used to guarantee that no frame is lost in the pipeline, but it may produce a memory leak.

**Directories of still images**

We can work with all images in a directory, which are supplied to the application in
a transparent way.

<pre id="samp">
    ./play --photos=../../data/images/pano
</pre>

The user must explicitly move forward or backward pressing keys in the window.
Alternatively, the images can be automatically loaded without user participation:
    
<pre id="samp">
    ./play --sphotos=../../data/images/pano
</pre>



- - -

[back to help][help]

!INCLUDE(LINKS)


