#include <stdio.h>
#include <stdlib.h>
#include "wrappers.h"

#define R(C,D,E) CLIP(( 298 * (C)             + 409 * (E) + 128) >> 8)
#define G(C,D,E) CLIP(( 298 * (C) - 100 * (D) - 208 * (E) + 128) >> 8)
#define B(C,D,E) CLIP(( 298 * (C) + 516 * (D)             + 128) >> 8)


int exampleInvert(IM1(src), IM1(dst)) {
    int r,c;
    TRAV(dst,20,r,c) {
        P(dst,r,c) = 255 - P(src,r,c);
    }
    return 0;
}

int gray2rgb(IM1(src), IM1(dst)) {
    int r,c;
    TRAV(src,0,r,c) {
        int v = P(src,r,c);
        PM(dst,r,c,0)=v;
        PM(dst,r,c,1)=v;
        PM(dst,r,c,2)=v;
    }
    return 0;
}


int rgb2gray(IM1(src), IM1(dst)) {
    int r,c;
    TRAV(src,0,r,c) {
        P(dst,r,c) = 0.2125*PM(src,r,c,0) + 0.7154*PM(src,r,c,1) + 0.0721*PM(src,r,c,2);
    }
    return 0;
}



int yuv2yuyv(IM1(s),IM2(d)) {
    int w = sc2-sc1+1, h = sr2-sr1+1, w2 = w/2;
    unsigned char *py = sp, *pu = py + w*h, *pv = pu + w*h/4;
    unsigned char *d = dp;
    int i,j;
    for (i=0; i<h; i++) {
        for (j=0; j<w2; j++) {
            *(d++) = py[i*w+2*j];
            *(d++) = pu[(i/2)*w2+j];
            *(d++) = py[i*w+2*j+1];
            *(d++) = pv[(i/2)*w2+j];
        }
    }
    return 0;
}


int yuyv2gray(IM2(src),IM1(dst)) {
    int r,c;
    TRAV(dst,0,r,c) {
        P(dst,r,c) = P2(src,r,c,0);
    }
    return 0;
}


int yuyv2rgb(IM2(x),IM3(y)) {
    int k, ks=0, kd=0;
    unsigned char * s = xp, * p = yp;
    int width = xc2-xc1+1, height = xr2-xr1+1;
    for(k=0; k<(width*height/2); k++) {
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
    return 0;
}

#undef R
#undef G
#undef B

