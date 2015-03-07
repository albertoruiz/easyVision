/* generated automatically by adapter.hs */

#include <ipp.h>

int ippiCopy_8u_C1Rx(Ipp8u* pSrc, int srcStep, Ipp8u* pDst, int dstStep, IppiSize* roiSize) {
    return ippiCopy_8u_C1R(pSrc, srcStep, pDst, dstStep, *roiSize);
}

int ippiRGBToGray_8u_C3C1Rx(Ipp8u* pSrc, int srcStep, Ipp8u* pDst, int dstStep, IppiSize* roiSize) {
    return ippiRGBToGray_8u_C3C1R(pSrc, srcStep, pDst, dstStep, *roiSize);
}

