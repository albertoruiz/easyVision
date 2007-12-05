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
