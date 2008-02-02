#include<stdlib.h>
#include<stdio.h>

int getPoints32f(float * pSrc, int sstep, int sr1, int sr2, int sc1, int sc2,
                 int max, int* tot, int* hp) {
    int r,c;
    int pos = 0;
    int maxpos = 2*max-3;
    for (r=sr1; r<=sr2; r++) {
        for(c=sc1; c<=sc2; c++) {
            if(pos>=maxpos) {
                *tot = max;
                return 1;
            }
            if(*(pSrc+r*sstep/4+c) > 0.) {
                hp[pos] =  r;
                hp[pos+1] = c;
                pos+=2;
            }
        }
    }
    *tot = pos;
    return 0;
}

//---------------------------- Local binary patterns ---------------------

// fills a buffer int[255]

int lbp8u(int delta, unsigned char * pSrc, int sstep, int sr1, int sr2, int sc1, int sc2, int* histogram) {
    #define X(r,c) (*(pSrc+(r)*sstep+(c)))
    int r,c;
    int k;
    for(k=0; k<256; k++) {
        histogram[k]=0;
    }
    for (r=sr1+1; r<sr2; r++) {
        for(c=sc1+1; c<sc2; c++) {
            int x = X(r,c)+delta;
            int x0 = X(r-1,c-1) > x ? 1 : 0;
            int x1 = X(r-1,c)   > x ? 2 : 0;
            int x2 = X(r-1,c+1) > x ? 4 : 0;
            int x3 = X(r,c-1)   > x ? 8 : 0;
            int x4 = X(r,c+1)   > x ? 16: 0;
            int x5 = X(r+1,c-1) > x ? 32: 0;
            int x6 = X(r+1,c)   > x ? 64: 0;
            int x7 = X(r+1,c+1) > x ? 128:0;
            int val = x0+x1+x2+x3+x4+x5+x6+x7;
            histogram[val]++;
        }
    }
    return 0;
    #undef X
}

//---------------------------- simple color code ---------------------

// this test version overwrites the hsv with prototype color

#define H(V,D) else if (*h<=V) { *h=(D); *v=255; *s=255; }

int hsvcodeTest(int kb, int kg, int kw,
                unsigned char * pSrc, int sstep, int sr1, int sr2, int sc1, int sc2) {
    int r,c;
    for (r=sr1; r<=sr2; r++) {
        for(c=sc1; c<=sc2; c++) {
            unsigned char* h = pSrc + r*sstep + c*3;
            unsigned char* s = h+1;
            unsigned char* v = s+1;
            if(*v<kb) {                        // black
                *v = 0;
            } else if ((*s<kg) && (*v<kw)) {   // gray
                *v = 128;
                *s = 0;
            } else if ((*s<kg)) {              // white
                *v = 255;
                *s = 0;
            }
             H(12,0)     // red
             H(26,18)    // orange
             H(46,36)    // yellow
             H(116,80)   // green
             H(135,125)  // cyan ??
             H(200,168)  // blue
             H(230,215)  // magenta ??
             else { *h=0; *v=255; *s=255; }    // red
        }
    }
    return 0;
}

#undef H

// this version creates color codes

#define H(V,D) else if (*h<=V) { *d=(D); }

int hsvcode(int kb, int kg, int kw,
                unsigned char * pSrc, int sstep,
                unsigned char * pDst, int dstep,
                int sr1, int sr2, int sc1, int sc2) {
    int r,c;
    for (r=sr1; r<=sr2; r++) {
        for(c=sc1; c<=sc2; c++) {
            unsigned char* h = pSrc + r*sstep + c*3;
            unsigned char* s = h+1;
            unsigned char* v = s+1;
            unsigned char* d = pDst + r*dstep + c;
            if(*v<kb) {                        // black
                *d = 1;
            } else if ((*s<kg) && (*v<kw)) {   // gray
                *d = 0;
            } else if ((*s<kg)) {              // white
                *d = 2;
            }
             H(12,3)    // red
             H(26,4)    // orange
             H(46,5)    // yellow
             H(116,6)   // green
             H(135,7)   // cyan ??
             H(200,8)   // blue
             H(230,9)   // magenta ??
             else { *d=3; }    // red
        }
    }
    return 0;
}

#undef H
