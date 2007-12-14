#include <ipp.h>
#include<stdlib.h>
#include<stdio.h>

#define hauxWarpPerspective(M) \
int auxWarpPerspective##M(void * pSrc, int sstep, int sh, int sw,       \
                               int sr1, int sr2, int sc1, int sc2,      \
                               void * pDst, int dstep,                  \
                               int dr1, int dr2, int dc1, int dc2,      \
                               const double *h, int interp)

#define auxWarpPerspective(M) \
hauxWarpPerspective(M)                                                  \
{                                                                       \
    IppiSize srcSize = {sw,sh};                                         \
    IppiRect srcRoi = {sc1,sr1,sc2-sc1+1,sr2-sr1+1};                    \
    IppiRect dstRoi = {dc1,dr1,dc2-dc1+1,dr2-dr1+1};                    \
    const double coeff [3][3] = {{h[0],h[1],h[2]},                      \
                                 {h[3],h[4],h[5]},                      \
                                 {h[6],h[7],h[8]}};                     \
    return ippiWarpPerspective##M(pSrc,srcSize,sstep,srcRoi,pDst,dstep,dstRoi,coeff,interp); \
}

// TO DO: add roi offset in pSrc & pDst
auxWarpPerspective(_32f_C1R)
auxWarpPerspective(_8u_C1R)
auxWarpPerspective(_8u_C3R)

int auxResize_32f_C1R(void * pSrc, int sstep, int sh, int sw,
                      int sr1, int sr2, int sc1, int sc2,
                      void * pDst, int dstep,
                      int dr1, int dr2, int dc1, int dc2,
                      int interp)
{
    IppiSize srcSize = {sw,sh};
    IppiRect srcRoi = {sc1,sr1,sc2-sc1+1,sr2-sr1+1};
    IppiSize dstRoi = {dc2-dc1+1,dr2-dr1+1};
    double xf = (double)(dc2-dc1+1)/(sc2-sc1+1);
    double yf = (double)(dr2-dr1+1)/(sr2-sr1+1);
    return ippiResize_32f_C1R(pSrc,srcSize,sstep,srcRoi,
                              pDst,dstep,dstRoi,
                              xf,yf,interp);
}

int auxResize_8u_C1R(void * pSrc, int sstep, int sh, int sw,
                      int sr1, int sr2, int sc1, int sc2,
                      void * pDst, int dstep,
                      int dr1, int dr2, int dc1, int dc2,
                      int interp)
{
    IppiSize srcSize = {sw,sh};
    IppiRect srcRoi = {sc1,sr1,sc2-sc1+1,sr2-sr1+1};
    IppiSize dstRoi = {dc2-dc1+1,dr2-dr1+1};
    double xf = (double)(dc2-dc1+1)/(sc2-sc1+1);
    double yf = (double)(dr2-dr1+1)/(sr2-sr1+1);
    return ippiResize_8u_C1R(pSrc,srcSize,sstep,srcRoi,
                              pDst,dstep,dstRoi,
                              xf,yf,interp);
}

int auxResize_8u_C3R(void * pSrc, int sstep, int sh, int sw,
                      int sr1, int sr2, int sc1, int sc2,
                      void * pDst, int dstep,
                      int dr1, int dr2, int dc1, int dc2,
                      int interp)
{
    IppiSize srcSize = {sw,sh};
    IppiRect srcRoi = {sc1,sr1,sc2-sc1+1,sr2-sr1+1};
    IppiSize dstRoi = {dc2-dc1+1,dr2-dr1+1};
    double xf = (double)(dc2-dc1+1)/(sc2-sc1+1);
    double yf = (double)(dr2-dr1+1)/(sr2-sr1+1);
    return ippiResize_8u_C3R(pSrc,srcSize,sstep,srcRoi,
                              pDst,dstep,dstRoi,
                              xf,yf,interp);
}


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

//---------------- Discrete Cosine Transform ------------------

int auxDCTFwd_32f_C1R(float * pSrc, int sstep,
                      int sr1, int sr2, int sc1, int sc2,
                      float * pDst, int dstep,
                      int dr1, int dr2, int dc1, int dc2)
{
    int res;
    IppiSize roiSize = {dc2-dc1+1,dr2-dr1+1};
    IppiDCTFwdSpec_32f* context;
    res = ippiDCTFwdInitAlloc_32f (&context, roiSize, ippAlgHintFast);
    res = ippiDCTFwd_32f_C1R(pSrc+sr1*sstep/4+sc1,sstep,pDst+dr1*dstep/4+dc1,dstep,context,NULL);
    ippiDCTFwdFree_32f(context);
    return res;
}

int auxDCTInv_32f_C1R(float * pSrc, int sstep,
                      int sr1, int sr2, int sc1, int sc2,
                      float * pDst, int dstep,
                      int dr1, int dr2, int dc1, int dc2)
{
    int res;
    IppiSize roiSize = {dc2-dc1+1,dr2-dr1+1};
    IppiDCTInvSpec_32f* context;
    res = ippiDCTInvInitAlloc_32f (&context, roiSize, ippAlgHintFast);
    res = ippiDCTInv_32f_C1R(pSrc+sr1*sstep/4+sc1,sstep,pDst+dr1*dstep/4+dc1,dstep,context,NULL);
    ippiDCTInvFree_32f(context);
    return res;
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
