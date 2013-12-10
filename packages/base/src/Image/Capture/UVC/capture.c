#include "v4l2uvc.h"
#include <stdio.h>
#include <stdlib.h>

struct vdIn * openUVC(char * videodevice, int* width, int* height, int* fps){

    int format = V4L2_PIX_FMT_YUYV;
    int grabmethod = 1;
    char *avifilename = NULL;

    struct vdIn *videoIn = (struct vdIn *) calloc(1, sizeof(struct vdIn));

    if (init_videoIn (videoIn, videodevice,
                      *width, *height, *fps, format,
                      grabmethod, avifilename) < 0) exit(1);

    *width  = videoIn->width;
    *height = videoIn->height;
    *fps    = videoIn->fps;

    return videoIn;
}

////////////////////////////////////////////////////////////////////////////////////

int grabUVC(int mode, struct vdIn * v, unsigned char * p) {
    int ok = uvcGrab(v);
    if (ok<0) return ok;
    
    switch (mode) {

      case 0 : { // YCbCr 422
        memcpy(p, v->framebuffer, v->width * (v->height) * 2);
        return ok;
      }

      default : return 1;
    }

}

////////////////////////////////////////////////////////////////////////////////////

//    close_v4l2(videoIn);
//	  free(videoIn);

////////////////////////////////////////////////////////////////////////////////////

