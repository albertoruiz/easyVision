#include "v4l2uvc.h"
#include <stdio.h>
#include <stdlib.h>
#include <ippcc.h>


const char *videodevice = NULL;
const char *mode = NULL;
int format = V4L2_PIX_FMT_YUYV;
int i;
int grabmethod = 1;
//int width = 640;
//int height = 480;
//float fps = 30.0;     // Requested frame rate
//float frmrate = 0.0;  // Measured frame rate
char *avifilename = NULL;

struct vdIn *videoIn;


void * openUVC(char * videodevice, int width, int height, int fps){

//    videodevice = "/dev/video0";

    videoIn = (struct vdIn *) calloc(1, sizeof(struct vdIn));

    if (init_videoIn (videoIn, (char *) videodevice,
                      width, height, fps, format,
                      grabmethod, avifilename) < 0) exit(1);

    return (void *) videoIn;
}

void grabUVC(void * v, Ipp8u * p, int width, int height) {

    if (uvcGrab(videoIn) < 0) { printf("Error grabbing\n"); exit(1); }

    IppiSize roiSize = {width,height};
    int steps[3] = {width,width/2,width/2};

    Ipp8u * dest [3] = {p, p + (width*height), p + (width*height+width*height/4)};

    ippiYCbCr422ToYCbCr420_8u_C2P3R(videoIn->framebuffer, width*2, dest, steps, roiSize);


//    memcpy(p, videoIn->framebuffer, videoIn->width * (videoIn->height) * 2);

//    close_v4l2(videoIn);
//	free(videoIn);

}



