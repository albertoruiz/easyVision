#include <stdio.h>
#include <stdlib.h>
#include "wrappers.h"

#define R(C,D,E) clip(( 298 * (C)             + 409 * (E) + 128) >> 8)
#define G(C,D,E) clip(( 298 * (C) - 100 * (D) - 208 * (E) + 128) >> 8)
#define B(C,D,E) clip(( 298 * (C) + 516 * (D)             + 128) >> 8)


int exampleInvert(IM1(src), IM1(dst)) {
    int r,c;
    TRAV(dst,20,r,c) {
        P(dst,r,c) = 255 - P(src,r,c);
    }
    return 0;
}

int yuv2yuyv(IM1(x),IM2(y)) {
    int r, c;
    unsigned char * p = xp, * q = yp;
    TRAV(x,0,r,c) {
        *(q++) = *(p++);
        *(q++) = 0;
    }
    return 0;
}


int yuyv2rgb(IM2(x),IM3(y)) {
    int r, c;
    unsigned char * p = xp, * q = yp;
    TRAV(x,0,r,c) {
        *(q++) = *p;
        *(q++) = *p;
        *(q++) = *(p++);
        p++;
    }
    return 0;
}


/*

      case 2 : { // YUV 420
        IppiSize roiSize = {v->width,v->height};
        int steps[3] = {v->width,v->width/2,v->width/2};
        Ipp8u * dest [3] = {p, p + (v->width*v->height), p + (v->width*v->height+v->width*v->height/4)};
        ippiYCbCr422ToYCbCr420_8u_C2P3R(v->framebuffer, v->width*2, dest, steps, roiSize);
        return ok;
      }

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

*/

#undef R
#undef G
#undef B


