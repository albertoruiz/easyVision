#include "v4l2uvc.h"
#include <stdio.h>
#include <stdlib.h>
#include <ippcc.h>


// videodevice = "/dev/video0";
// width = 640;
// height = 480;
// fps = 30.0;

struct vdIn * openUVC(char * videodevice, int width, int height, int fps){

    int format = V4L2_PIX_FMT_YUYV;
    int grabmethod = 1;
    char *avifilename = NULL;

    struct vdIn *videoIn = (struct vdIn *) calloc(1, sizeof(struct vdIn));

    if (init_videoIn (videoIn, videodevice,
                      width, height, fps, format,
                      grabmethod, avifilename) < 0) exit(1);

    return videoIn;
}

////////////////////////////////////////////////////////////////////////////////////

void grabUVC(struct vdIn * v, Ipp8u * p) {

    if (uvcGrab(v) < 0) { printf("Error grabbing\n"); exit(1); }

    IppiSize roiSize = {v->width,v->height};
    int steps[3] = {v->width,v->width/2,v->width/2};

    Ipp8u * dest [3] = {p, p + (v->width*v->height), p + (v->width*v->height+v->width*v->height/4)};

    ippiYCbCr422ToYCbCr420_8u_C2P3R(v->framebuffer, v->width*2, dest, steps, roiSize);

//    captura sin transformación de formato
//    memcpy(p, videoIn->framebuffer, videoIn->width * (videoIn->height) * 2);

}

////////////////////////////////////////////////////////////////////////////////////

//    close_v4l2(videoIn);
//	  free(videoIn);


