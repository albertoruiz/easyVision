#include <ipp.h>
#include <stdlib.h>
#include <stdio.h>
#include "wrappers.h"

//---------------- RESIZE ------------------------------------------------------

// TO DO: reuse buffers
#define RESIZE(M,F,H,C,I,J) \
int auxResize##M(I(s),I(d))                                        \
{                                                                              \
    IppiSize dstRoi = {dc2-dc1+1,dr2-dr1+1};                                   \
    int specSize, initSize, bufSize, nChannel=C;                               \
    IppiSize srcSizeR = {sc2-sc1+1,sr2-sr1+1};                                 \
    IppiPoint dstOffset = {0,0};                                               \
                                                                               \
    F(srcSizeR,dstRoi,ippLinear, 0, &specSize, &initSize);                     \
                                                                               \
    IppiResizeSpec_32f* pSpec=(IppiResizeSpec_32f*)ippsMalloc_8u(specSize);    \
                                                                               \
    H(srcSizeR, dstRoi, pSpec);                                                \
                                                                               \
    ippiResizeGetBufferSize_8u(pSpec,dstRoi,nChannel,&bufSize);                \
    Ipp8u* pBuffer=ippsMalloc_8u(bufSize);                                     \
    int r = J(ROI(s,C),sstep, ROI(d,C), dstep, dstOffset, dstRoi,              \
              ippBorderRepl, 0, pSpec, pBuffer);                               \
                                                                               \
    ippsFree(pSpec);                                                           \
    ippsFree(pBuffer);                                                         \
                                                                               \
    return r;                                                                  \
                                                                               \
}

RESIZE(_32f_C1R,ippiResizeGetSize_32f,ippiResizeLinearInit_32f,1,IMF,ippiResizeLinear_32f_C1R)
RESIZE(_8u_C1R,ippiResizeGetSize_8u,ippiResizeLinearInit_8u,1,IM1,ippiResizeLinear_8u_C1R)
RESIZE(_8u_C3R,ippiResizeGetSize_8u,ippiResizeLinearInit_8u,3,IM1,ippiResizeLinear_8u_C3R)


