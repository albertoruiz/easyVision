#include "v4l2uvc.h"
#include <stdio.h>
#include <stdlib.h>
// #include <ippcc.h>


// videodevice = "/dev/video0";
// width = 640;
// height = 480;
// fps = 30.0;

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

#define clip(a) ((a)<0?0:(a)>255?255:(a))

#define R(C,D,E) clip(( 298 * (C)             + 409 * (E) + 128) >> 8)
#define G(C,D,E) clip(( 298 * (C) - 100 * (D) - 208 * (E) + 128) >> 8)
#define B(C,D,E) clip(( 298 * (C) + 516 * (D)             + 128) >> 8)

int grabUVC(int mode, struct vdIn * v, unsigned char * p) {
    int ok = uvcGrab(v);
    if (ok<0) return ok;
    
    switch (mode) {
/*
      case 2 : { // YUV 420
        IppiSize roiSize = {v->width,v->height};
        int steps[3] = {v->width,v->width/2,v->width/2};
        Ipp8u * dest [3] = {p, p + (v->width*v->height), p + (v->width*v->height+v->width*v->height/4)};
        ippiYCbCr422ToYCbCr420_8u_C2P3R(v->framebuffer, v->width*2, dest, steps, roiSize);
        return ok;
      }
      case 4: { // RGB with IPP
        IppiSize roiSize = {v->width,v->height};
        Ipp8u * dest = p;
        ippiYCbCr422ToRGB_8u_C2C3R(v->framebuffer, v->width*2, dest, v->width*3, roiSize);
        return ok;
      }
*/

      case 3: { // grayscale
        // copy the gray plane
        int k, d = 0;
        unsigned char * s = v->framebuffer;
        for(k=0; k<(v->width*v->height); k++) {
          p[k] = s[d]; d+=2;
        }
        return ok;
      }

      case 0 : { // YCbCr 422
        memcpy(p, v->framebuffer, v->width * (v->height) * 2);
        return ok;
      }

      case 1: { // RGB
        int k, ks=0, kd=0;
        unsigned char * s = v->framebuffer;
        for(k=0; k<(v->width*v->height/2); k++) {
          int y1 = s[ks++]-16;
          int u  = s[ks++]-128;
          int y2 = s[ks++]-16;
          int v  = s[ks++]-128;
          p[kd++] = R(y1,u,v);
          p[kd++] = G(y1,u,v);
          p[kd++] = B(y1,u,v);
          p[kd++] = R(y2,u,v);
          p[kd++] = G(y2,u,v);
          p[kd++] = B(y2,u,v);
        }
        return ok;
      }

      default : return 1;
    }

}

#undef clip
#undef R
#undef G
#undef B

////////////////////////////////////////////////////////////////////////////////////

//    close_v4l2(videoIn);
//	  free(videoIn);

////////////////////////////////////////////////////////////////////////////////////

