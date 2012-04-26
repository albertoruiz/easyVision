/* generated automatically by adapter.hs */

#include <ipp.h>

int ippiAdd_8u_C1RSfsx(Ipp8u* pSrc1, int src1Step, Ipp8u* pSrc2, int src2Step, Ipp8u* pDst, int dstStep, IppiSize* roiSize, int scaleFactor) {
    return ippiAdd_8u_C1RSfs(pSrc1, src1Step, pSrc2, src2Step, pDst, dstStep, *roiSize, scaleFactor);
}

int ippiSub_8u_C1RSfsx(Ipp8u* pSrc1, int src1Step, Ipp8u* pSrc2, int src2Step, Ipp8u* pDst, int dstStep, IppiSize* roiSize, int scaleFactor) {
    return ippiSub_8u_C1RSfs(pSrc1, src1Step, pSrc2, src2Step, pDst, dstStep, *roiSize, scaleFactor);
}

int ippiMulC_32f_C1Rx(Ipp32f* pSrc, int srcStep, Ipp32f value, Ipp32f* pDst, int dstStep, IppiSize* roiSize) {
    return ippiMulC_32f_C1R(pSrc, srcStep, value, pDst, dstStep, *roiSize);
}

int ippiAdd_32f_C1Rx(Ipp32f* pSrc1, int src1Step, Ipp32f* pSrc2, int src2Step, Ipp32f* pDst, int dstStep, IppiSize* roiSize) {
    return ippiAdd_32f_C1R(pSrc1, src1Step, pSrc2, src2Step, pDst, dstStep, *roiSize);
}

int ippiSub_32f_C1Rx(Ipp32f* pSrc1, int src1Step, Ipp32f* pSrc2, int src2Step, Ipp32f* pDst, int dstStep, IppiSize* roiSize) {
    return ippiSub_32f_C1R(pSrc1, src1Step, pSrc2, src2Step, pDst, dstStep, *roiSize);
}

int ippiMul_32f_C1Rx(Ipp32f* pSrc1, int src1Step, Ipp32f* pSrc2, int src2Step, Ipp32f* pDst, int dstStep, IppiSize* roiSize) {
    return ippiMul_32f_C1R(pSrc1, src1Step, pSrc2, src2Step, pDst, dstStep, *roiSize);
}

int ippiDiv_32f_C1Rx(Ipp32f* pSrc1, int src1Step, Ipp32f* pSrc2, int src2Step, Ipp32f* pDst, int dstStep, IppiSize* roiSize) {
    return ippiDiv_32f_C1R(pSrc1, src1Step, pSrc2, src2Step, pDst, dstStep, *roiSize);
}

int ippiAbs_32f_C1Rx(Ipp32f* pSrc, int srcStep, Ipp32f* pDst, int dstStep, IppiSize* roiSize) {
    return ippiAbs_32f_C1R(pSrc, srcStep, pDst, dstStep, *roiSize);
}

int ippiSqrt_32f_C1Rx(Ipp32f* pSrc, int srcStep, Ipp32f* pDst, int dstStep, IppiSize* roiSize) {
    return ippiSqrt_32f_C1R(pSrc, srcStep, pDst, dstStep, *roiSize);
}

int ippiMagnitudePack_32f_C1Rx(Ipp32f* pSrc, int srcStep, Ipp32f* pDst, int dstStep, IppiSize* dstRoiSize) {
    return ippiMagnitudePack_32f_C1R(pSrc, srcStep, pDst, dstStep, *dstRoiSize);
}

int ippiFFTInitAlloc_R_32fx(IppiFFTSpec_R_32f** pFFTSpec, int orderX, int orderY, int flag, IppHintAlgorithm hint) {
    return ippiFFTInitAlloc_R_32f(pFFTSpec, orderX, orderY, flag, hint);
}

int ippiFFTFree_R_32fx(IppiFFTSpec_R_32f* pFFTSpec) {
    return ippiFFTFree_R_32f(pFFTSpec);
}

int ippiFFTGetBufSize_R_32fx(IppiFFTSpec_R_32f* pFFTSpec, int* pSize) {
    return ippiFFTGetBufSize_R_32f(pFFTSpec, pSize);
}

int ippiFFTFwd_RToPack_32f_C1Rx(Ipp32f* pSrc, int srcStep, Ipp32f* pDst, int dstStep, IppiFFTSpec_R_32f* pFFTSpec, Ipp8u* pBuffer) {
    return ippiFFTFwd_RToPack_32f_C1R(pSrc, srcStep, pDst, dstStep, pFFTSpec, pBuffer);
}

int ippiMirror_8u_C1Rx(Ipp8u* pSrc, int srcStep, Ipp8u* pDst, int dstStep, IppiSize* roiSize, IppiAxis flip) {
    return ippiMirror_8u_C1R(pSrc, srcStep, pDst, dstStep, *roiSize, flip);
}

int ippiRemap_8u_C1Rx(Ipp8u* pSrc, IppiSize* srcSize, int srcStep, IppiRect* srcROI, Ipp32f* pxMap, int xMapStep, Ipp32f* pyMap, int yMapStep, Ipp8u* pDst, int dstStep, IppiSize* dstRoiSize, int interpolation) {
    return ippiRemap_8u_C1R(pSrc, *srcSize, srcStep, *srcROI, pxMap, xMapStep, pyMap, yMapStep, pDst, dstStep, *dstRoiSize, interpolation);
}

int ippiRemap_8u_C3Rx(Ipp8u* pSrc, IppiSize* srcSize, int srcStep, IppiRect* srcROI, Ipp32f* pxMap, int xMapStep, Ipp32f* pyMap, int yMapStep, Ipp8u* pDst, int dstStep, IppiSize* dstRoiSize, int interpolation) {
    return ippiRemap_8u_C3R(pSrc, *srcSize, srcStep, *srcROI, pxMap, xMapStep, pyMap, yMapStep, pDst, dstStep, *dstRoiSize, interpolation);
}

int ippiRemap_32f_C1Rx(Ipp32f* pSrc, IppiSize* srcSize, int srcStep, IppiRect* srcROI, Ipp32f* pxMap, int xMapStep, Ipp32f* pyMap, int yMapStep, Ipp32f* pDst, int dstStep, IppiSize* dstRoiSize, int interpolation) {
    return ippiRemap_32f_C1R(pSrc, *srcSize, srcStep, *srcROI, pxMap, xMapStep, pyMap, yMapStep, pDst, dstStep, *dstRoiSize, interpolation);
}

int ippiSum_8u_C1Rx(Ipp8u* pSrc, int srcStep, IppiSize* roiSize, Ipp64f* pSum) {
    return ippiSum_8u_C1R(pSrc, srcStep, *roiSize, pSum);
}

int ippiSum_32f_C1Rx(Ipp32f* pSrc, int srcStep, IppiSize* roiSize, Ipp64f* pSum, IppHintAlgorithm hint) {
    return ippiSum_32f_C1R(pSrc, srcStep, *roiSize, pSum, hint);
}

int ippiHistogramRange_8u_C1Rx(Ipp8u* pSrc, int srcStep, IppiSize* roiSize, Ipp32s* pHist, Ipp32s* pLevels, int nLevels) {
    return ippiHistogramRange_8u_C1R(pSrc, srcStep, *roiSize, pHist, pLevels, nLevels);
}

int ippiFilterMedian_8u_C1Rx(Ipp8u* pSrc, int srcStep, Ipp8u* pDst, int dstStep, IppiSize* dstRoiSize, IppiSize* maskSize, IppiPoint* anchor) {
    return ippiFilterMedian_8u_C1R(pSrc, srcStep, pDst, dstStep, *dstRoiSize, *maskSize, *anchor);
}

int ippiFilterMax_8u_C1Rx(Ipp8u* pSrc, int srcStep, Ipp8u* pDst, int dstStep, IppiSize* dstRoiSize, IppiSize* maskSize, IppiPoint* anchor) {
    return ippiFilterMax_8u_C1R(pSrc, srcStep, pDst, dstStep, *dstRoiSize, *maskSize, *anchor);
}

int ippiFilterMax_32f_C1Rx(Ipp32f* pSrc, int srcStep, Ipp32f* pDst, int dstStep, IppiSize* dstRoiSize, IppiSize* maskSize, IppiPoint* anchor) {
    return ippiFilterMax_32f_C1R(pSrc, srcStep, pDst, dstStep, *dstRoiSize, *maskSize, *anchor);
}

int ippiFilterMin_8u_C1Rx(Ipp8u* pSrc, int srcStep, Ipp8u* pDst, int dstStep, IppiSize* dstRoiSize, IppiSize* maskSize, IppiPoint* anchor) {
    return ippiFilterMin_8u_C1R(pSrc, srcStep, pDst, dstStep, *dstRoiSize, *maskSize, *anchor);
}

int ippiFilterMin_32f_C1Rx(Ipp32f* pSrc, int srcStep, Ipp32f* pDst, int dstStep, IppiSize* dstRoiSize, IppiSize* maskSize, IppiPoint* anchor) {
    return ippiFilterMin_32f_C1R(pSrc, srcStep, pDst, dstStep, *dstRoiSize, *maskSize, *anchor);
}

int ippiFilterBox_8u_C1Rx(Ipp8u* pSrc, int srcStep, Ipp8u* pDst, int dstStep, IppiSize* dstRoiSize, IppiSize* maskSize, IppiPoint* anchor) {
    return ippiFilterBox_8u_C1R(pSrc, srcStep, pDst, dstStep, *dstRoiSize, *maskSize, *anchor);
}

int ippiFilterBox_32f_C1Rx(Ipp32f* pSrc, int srcStep, Ipp32f* pDst, int dstStep, IppiSize* dstRoiSize, IppiSize* maskSize, IppiPoint* anchor) {
    return ippiFilterBox_32f_C1R(pSrc, srcStep, pDst, dstStep, *dstRoiSize, *maskSize, *anchor);
}

int ippiFilterSobelVert_32f_C1Rx(Ipp32f* pSrc, int srcStep, Ipp32f* pDst, int dstStep, IppiSize* roiSize) {
    return ippiFilterSobelVert_32f_C1R(pSrc, srcStep, pDst, dstStep, *roiSize);
}

int ippiFilterSobelHoriz_32f_C1Rx(Ipp32f* pSrc, int srcStep, Ipp32f* pDst, int dstStep, IppiSize* roiSize) {
    return ippiFilterSobelHoriz_32f_C1R(pSrc, srcStep, pDst, dstStep, *roiSize);
}

int ippiFilterLaplace_32f_C1Rx(Ipp32f* pSrc, int srcStep, Ipp32f* pDst, int dstStep, IppiSize* roiSize, IppiMaskSize mask) {
    return ippiFilterLaplace_32f_C1R(pSrc, srcStep, pDst, dstStep, *roiSize, mask);
}

int ippiFilterGauss_8u_C1Rx(Ipp8u* pSrc, int srcStep, Ipp8u* pDst, int dstStep, IppiSize* roiSize, IppiMaskSize mask) {
    return ippiFilterGauss_8u_C1R(pSrc, srcStep, pDst, dstStep, *roiSize, mask);
}

int ippiFilterGauss_32f_C1Rx(Ipp32f* pSrc, int srcStep, Ipp32f* pDst, int dstStep, IppiSize* roiSize, IppiMaskSize mask) {
    return ippiFilterGauss_32f_C1R(pSrc, srcStep, pDst, dstStep, *roiSize, mask);
}

int ippiFilterHipass_8u_C1Rx(Ipp8u* pSrc, int srcStep, Ipp8u* pDst, int dstStep, IppiSize* roiSize, IppiMaskSize mask) {
    return ippiFilterHipass_8u_C1R(pSrc, srcStep, pDst, dstStep, *roiSize, mask);
}

int ippiFilter_8u_C1Rx(Ipp8u* pSrc, int srcStep, Ipp8u* pDst, int dstStep, IppiSize* dstRoiSize, Ipp32s* pKernel, IppiSize* kernelSize, IppiPoint* anchor, int divisor) {
    return ippiFilter_8u_C1R(pSrc, srcStep, pDst, dstStep, *dstRoiSize, pKernel, *kernelSize, *anchor, divisor);
}

int ippiFilter_32f_C1Rx(Ipp32f* pSrc, int srcStep, Ipp32f* pDst, int dstStep, IppiSize* dstRoiSize, Ipp32f* pKernel, IppiSize* kernelSize, IppiPoint* anchor) {
    return ippiFilter_32f_C1R(pSrc, srcStep, pDst, dstStep, *dstRoiSize, pKernel, *kernelSize, *anchor);
}

int ippiFilterColumn_8u_C1Rx(Ipp8u* pSrc, int srcStep, Ipp8u* pDst, int dstStep, IppiSize* dstRoiSize, Ipp32s* pKernel, int kernelSize, int yAnchor, int divisor) {
    return ippiFilterColumn_8u_C1R(pSrc, srcStep, pDst, dstStep, *dstRoiSize, pKernel, kernelSize, yAnchor, divisor);
}

int ippiFilterColumn_32f_C1Rx(Ipp32f* pSrc, int srcStep, Ipp32f* pDst, int dstStep, IppiSize* dstRoiSize, Ipp32f* pKernel, int kernelSize, int yAnchor) {
    return ippiFilterColumn_32f_C1R(pSrc, srcStep, pDst, dstStep, *dstRoiSize, pKernel, kernelSize, yAnchor);
}

int ippiFilterRow_8u_C1Rx(Ipp8u* pSrc, int srcStep, Ipp8u* pDst, int dstStep, IppiSize* dstRoiSize, Ipp32s* pKernel, int kernelSize, int xAnchor, int divisor) {
    return ippiFilterRow_8u_C1R(pSrc, srcStep, pDst, dstStep, *dstRoiSize, pKernel, kernelSize, xAnchor, divisor);
}

int ippiFilterRow_32f_C1Rx(Ipp32f* pSrc, int srcStep, Ipp32f* pDst, int dstStep, IppiSize* dstRoiSize, Ipp32f* pKernel, int kernelSize, int xAnchor) {
    return ippiFilterRow_32f_C1R(pSrc, srcStep, pDst, dstStep, *dstRoiSize, pKernel, kernelSize, xAnchor);
}

int ippiThreshold_Val_8u_C1Rx(Ipp8u* pSrc, int srcStep, Ipp8u* pDst, int dstStep, IppiSize* roiSize, Ipp8u threshold, Ipp8u value, IppCmpOp ippCmpOp) {
    return ippiThreshold_Val_8u_C1R(pSrc, srcStep, pDst, dstStep, *roiSize, threshold, value, ippCmpOp);
}

int ippiThreshold_Val_32f_C1Rx(Ipp32f* pSrc, int srcStep, Ipp32f* pDst, int dstStep, IppiSize* roiSize, Ipp32f threshold, Ipp32f value, IppCmpOp ippCmpOp) {
    return ippiThreshold_Val_32f_C1R(pSrc, srcStep, pDst, dstStep, *roiSize, threshold, value, ippCmpOp);
}

int ippiComputeThreshold_Otsu_8u_C1Rx(Ipp8u* pSrc, int srcStep, IppiSize* roiSize, Ipp8u* pThreshold) {
    return ippiComputeThreshold_Otsu_8u_C1R(pSrc, srcStep, *roiSize, pThreshold);
}

int ippiCopy_8u_C3C1Rx(Ipp8u* pSrc, int srcStep, Ipp8u* pDst, int dstStep, IppiSize* roiSize) {
    return ippiCopy_8u_C3C1R(pSrc, srcStep, pDst, dstStep, *roiSize);
}

int ippiCopy_8u_C1C3Rx(Ipp8u* pSrc, int srcStep, Ipp8u* pDst, int dstStep, IppiSize* roiSize) {
    return ippiCopy_8u_C1C3R(pSrc, srcStep, pDst, dstStep, *roiSize);
}

int ippiCopy_8u_C1Rx(Ipp8u* pSrc, int srcStep, Ipp8u* pDst, int dstStep, IppiSize* roiSize) {
    return ippiCopy_8u_C1R(pSrc, srcStep, pDst, dstStep, *roiSize);
}

int ippiCopy_8u_C3Rx(Ipp8u* pSrc, int srcStep, Ipp8u* pDst, int dstStep, IppiSize* roiSize) {
    return ippiCopy_8u_C3R(pSrc, srcStep, pDst, dstStep, *roiSize);
}

int ippiCopy_8u_C1MRx(Ipp8u* pSrc, int srcStep, Ipp8u* pDst, int dstStep, IppiSize* roiSize, Ipp8u* pMask, int maskStep) {
    return ippiCopy_8u_C1MR(pSrc, srcStep, pDst, dstStep, *roiSize, pMask, maskStep);
}

int ippiCopy_32f_C1Rx(Ipp32f* pSrc, int srcStep, Ipp32f* pDst, int dstStep, IppiSize* roiSize) {
    return ippiCopy_32f_C1R(pSrc, srcStep, pDst, dstStep, *roiSize);
}

int ippiCopy_32f_C1MRx(Ipp32f* pSrc, int srcStep, Ipp32f* pDst, int dstStep, IppiSize* roiSize, Ipp8u* pMask, int maskStep) {
    return ippiCopy_32f_C1MR(pSrc, srcStep, pDst, dstStep, *roiSize, pMask, maskStep);
}

int ippiSet_8u_C1Rx(Ipp8u value, Ipp8u* pDst, int dstStep, IppiSize* roiSize) {
    return ippiSet_8u_C1R(value, pDst, dstStep, *roiSize);
}

int ippiSet_8u_C3Rx(Ipp8u value[3], Ipp8u* pDst, int dstStep, IppiSize* roiSize) {
    return ippiSet_8u_C3R(value, pDst, dstStep, *roiSize);
}

int ippiSet_32f_C1Rx(Ipp32f value, Ipp32f* pDst, int dstStep, IppiSize* roiSize) {
    return ippiSet_32f_C1R(value, pDst, dstStep, *roiSize);
}

int ippiImageJaehne_32f_C1Rx(Ipp32f* pDst, int DstStep, IppiSize* roiSize) {
    return ippiImageJaehne_32f_C1R(pDst, DstStep, *roiSize);
}

int ippiScale_8u32f_C1Rx(Ipp8u* pSrc, int srcStep, Ipp32f* pDst, int dstStep, IppiSize* roiSize, Ipp32f vMin, Ipp32f vMax) {
    return ippiScale_8u32f_C1R(pSrc, srcStep, pDst, dstStep, *roiSize, vMin, vMax);
}

int ippiScale_32f8u_C1Rx(Ipp32f* pSrc, int srcStep, Ipp8u* pDst, int dstStep, IppiSize* roiSize, Ipp32f vMin, Ipp32f vMax) {
    return ippiScale_32f8u_C1R(pSrc, srcStep, pDst, dstStep, *roiSize, vMin, vMax);
}

int ippiMaxIndx_8u_C1Rx(Ipp8u* pSrc, int srcStep, IppiSize* roiSize, Ipp8u* pMax, int* pIndexX, int* pIndexY) {
    return ippiMaxIndx_8u_C1R(pSrc, srcStep, *roiSize, pMax, pIndexX, pIndexY);
}

int ippiMaxIndx_32f_C1Rx(Ipp32f* pSrc, int srcStep, IppiSize* roiSize, Ipp32f* pMax, int* pIndexX, int* pIndexY) {
    return ippiMaxIndx_32f_C1R(pSrc, srcStep, *roiSize, pMax, pIndexX, pIndexY);
}

int ippiMinMax_32f_C1Rx(Ipp32f* pSrc, int srcStep, IppiSize* roiSize, Ipp32f* pMin, Ipp32f* pMax) {
    return ippiMinMax_32f_C1R(pSrc, srcStep, *roiSize, pMin, pMax);
}

int ippiMaxEvery_8u_C1IRx(Ipp8u* pSrc, int srcStep, Ipp8u* pSrcDst, int srcDstStep, IppiSize* roiSize) {
    return ippiMaxEvery_8u_C1IR(pSrc, srcStep, pSrcDst, srcDstStep, *roiSize);
}

int ippiMinEvery_8u_C1IRx(Ipp8u* pSrc, int srcStep, Ipp8u* pSrcDst, int srcDstStep, IppiSize* roiSize) {
    return ippiMinEvery_8u_C1IR(pSrc, srcStep, pSrcDst, srcDstStep, *roiSize);
}

int ippiMaxEvery_32f_C1IRx(Ipp32f* pSrc, int srcStep, Ipp32f* pSrcDst, int srcDstStep, IppiSize* roiSize) {
    return ippiMaxEvery_32f_C1IR(pSrc, srcStep, pSrcDst, srcDstStep, *roiSize);
}

int ippiMinEvery_32f_C1IRx(Ipp32f* pSrc, int srcStep, Ipp32f* pSrcDst, int srcDstStep, IppiSize* roiSize) {
    return ippiMinEvery_32f_C1IR(pSrc, srcStep, pSrcDst, srcDstStep, *roiSize);
}

int ippiAnd_8u_C1Rx(Ipp8u* pSrc1, int src1Step, Ipp8u* pSrc2, int src2Step, Ipp8u* pDst, int dstStep, IppiSize* roiSize) {
    return ippiAnd_8u_C1R(pSrc1, src1Step, pSrc2, src2Step, pDst, dstStep, *roiSize);
}

int ippiOr_8u_C1Rx(Ipp8u* pSrc1, int src1Step, Ipp8u* pSrc2, int src2Step, Ipp8u* pDst, int dstStep, IppiSize* roiSize) {
    return ippiOr_8u_C1R(pSrc1, src1Step, pSrc2, src2Step, pDst, dstStep, *roiSize);
}

int ippiXor_8u_C1Rx(Ipp8u* pSrc1, int src1Step, Ipp8u* pSrc2, int src2Step, Ipp8u* pDst, int dstStep, IppiSize* roiSize) {
    return ippiXor_8u_C1R(pSrc1, src1Step, pSrc2, src2Step, pDst, dstStep, *roiSize);
}

int ippiNot_8u_C1Rx(Ipp8u* pSrc, int srcStep, Ipp8u* pDst, int dstStep, IppiSize* roiSize) {
    return ippiNot_8u_C1R(pSrc, srcStep, pDst, dstStep, *roiSize);
}

int ippiCompare_8u_C1Rx(Ipp8u* pSrc1, int src1Step, Ipp8u* pSrc2, int src2Step, Ipp8u* pDst, int dstStep, IppiSize* roiSize, IppCmpOp ippCmpOp) {
    return ippiCompare_8u_C1R(pSrc1, src1Step, pSrc2, src2Step, pDst, dstStep, *roiSize, ippCmpOp);
}

int ippiCompareC_8u_C1Rx(Ipp8u* pSrc, int srcStep, Ipp8u value, Ipp8u* pDst, int dstStep, IppiSize* roiSize, IppCmpOp ippCmpOp) {
    return ippiCompareC_8u_C1R(pSrc, srcStep, value, pDst, dstStep, *roiSize, ippCmpOp);
}

int ippiCompare_32f_C1Rx(Ipp32f* pSrc1, int src1Step, Ipp32f* pSrc2, int src2Step, Ipp8u* pDst, int dstStep, IppiSize* roiSize, IppCmpOp ippCmpOp) {
    return ippiCompare_32f_C1R(pSrc1, src1Step, pSrc2, src2Step, pDst, dstStep, *roiSize, ippCmpOp);
}

int ippiErode3x3_8u_C1Rx(Ipp8u* pSrc, int srcStep, Ipp8u* pDst, int dstStep, IppiSize* roiSize) {
    return ippiErode3x3_8u_C1R(pSrc, srcStep, pDst, dstStep, *roiSize);
}

int ippiDilate3x3_8u_C1Rx(Ipp8u* pSrc, int srcStep, Ipp8u* pDst, int dstStep, IppiSize* roiSize) {
    return ippiDilate3x3_8u_C1R(pSrc, srcStep, pDst, dstStep, *roiSize);
}

int ippiRGBToYUV420_8u_C3P3Rx(Ipp8u* pSrc, int srcStep, Ipp8u* pDst[3], int dstStep[3], IppiSize* roiSize) {
    return ippiRGBToYUV420_8u_C3P3R(pSrc, srcStep, pDst, dstStep, *roiSize);
}

int ippiYUV420ToRGB_8u_P3C3Rx(const Ipp8u* const pSrc[3], int srcStep[3], Ipp8u* pDst, int dstStep, IppiSize* roiSize) {
    return ippiYUV420ToRGB_8u_P3C3R(pSrc, srcStep, pDst, dstStep, *roiSize);
}

int ippiYUV420ToRGB_8u_P3Rx(const Ipp8u* const pSrc[3], int srcStep[3], Ipp8u* pDst[3], int dstStep, IppiSize* roiSize) {
    return ippiYUV420ToRGB_8u_P3R(pSrc, srcStep, pDst, dstStep, *roiSize);
}

int ippiRGBToGray_8u_C3C1Rx(Ipp8u* pSrc, int srcStep, Ipp8u* pDst, int dstStep, IppiSize* roiSize) {
    return ippiRGBToGray_8u_C3C1R(pSrc, srcStep, pDst, dstStep, *roiSize);
}

int ippiRGBToHSV_8u_C3Rx(Ipp8u* pSrc, int srcStep, Ipp8u* pDst, int dstStep, IppiSize* roiSize) {
    return ippiRGBToHSV_8u_C3R(pSrc, srcStep, pDst, dstStep, *roiSize);
}

int ippiHSVToRGB_8u_C3Rx(Ipp8u* pSrc, int srcStep, Ipp8u* pDst, int dstStep, IppiSize* roiSize) {
    return ippiHSVToRGB_8u_C3R(pSrc, srcStep, pDst, dstStep, *roiSize);
}

int ippiSampleLine_8u_C1Rx(Ipp8u* pSrc, int srcStep, IppiSize* roiSize, Ipp8u* pDst, IppiPoint* pt1, IppiPoint* pt2) {
    return ippiSampleLine_8u_C1R(pSrc, srcStep, *roiSize, pDst, *pt1, *pt2);
}

int ippiSampleLine_32f_C1Rx(Ipp32f* pSrc, int srcStep, IppiSize* roiSize, Ipp32f* pDst, IppiPoint* pt1, IppiPoint* pt2) {
    return ippiSampleLine_32f_C1R(pSrc, srcStep, *roiSize, pDst, *pt1, *pt2);
}

int ippiAbsDiff_8u_C1Rx(Ipp8u* pSrc1, int src1Step, Ipp8u* pSrc2, int src2Step, Ipp8u* pDst, int dstStep, IppiSize* roiSize) {
    return ippiAbsDiff_8u_C1R(pSrc1, src1Step, pSrc2, src2Step, pDst, dstStep, *roiSize);
}

int ippiAbsDiff_32f_C1Rx(Ipp32f* pSrc1, int src1Step, Ipp32f* pSrc2, int src2Step, Ipp32f* pDst, int dstStep, IppiSize* roiSize) {
    return ippiAbsDiff_32f_C1R(pSrc1, src1Step, pSrc2, src2Step, pDst, dstStep, *roiSize);
}

int ippiIntegral_8u32f_C1Rx(Ipp8u* pSrc, int srcStep, Ipp32f* pDst, int dstStep, IppiSize* roiSize, Ipp32f val) {
    return ippiIntegral_8u32f_C1R(pSrc, srcStep, pDst, dstStep, *roiSize, val);
}

int ippiSqrIntegral_8u32f64f_C1Rx(Ipp8u* pSrc, int srcStep, Ipp32f* pDst, int dstStep, Ipp64f* pSqr, int sqrStep, IppiSize* roiSize, Ipp32f val, Ipp64f valSqr) {
    return ippiSqrIntegral_8u32f64f_C1R(pSrc, srcStep, pDst, dstStep, pSqr, sqrStep, *roiSize, val, valSqr);
}

int ippiRectStdDev_32f_C1Rx(Ipp32f* pSrc, int srcStep, Ipp64f* pSqr, int sqrStep, Ipp32f* pDst, int dstStep, IppiSize* roiSize, IppiRect* rect) {
    return ippiRectStdDev_32f_C1R(pSrc, srcStep, pSqr, sqrStep, pDst, dstStep, *roiSize, *rect);
}

int ippiCannyGetSizex(IppiSize* roiSize, int* bufferSize) {
    return ippiCannyGetSize(*roiSize, bufferSize);
}

int ippiCanny_32f8u_C1Rx(Ipp32f* pSrcDx, int srcDxStep, Ipp32f* pSrcDy, int srcDyStep, Ipp8u* pDstEdges, int dstEdgeStep, IppiSize* roiSize, Ipp32f lowThresh, Ipp32f highThresh, Ipp8u* pBuffer) {
    return ippiCanny_32f8u_C1R(pSrcDx, srcDxStep, pSrcDy, srcDyStep, pDstEdges, dstEdgeStep, *roiSize, lowThresh, highThresh, pBuffer);
}

int ippiDistanceTransform_3x3_8u32f_C1Rx(Ipp8u* pSrc, int srcStep, Ipp32f* pDst, int dstStep, IppiSize* roiSize, Ipp32f* pMetrics) {
    return ippiDistanceTransform_3x3_8u32f_C1R(pSrc, srcStep, pDst, dstStep, *roiSize, pMetrics);
}

int ippiDistanceTransform_5x5_8u32f_C1Rx(Ipp8u* pSrc, int srcStep, Ipp32f* pDst, int dstStep, IppiSize* roiSize, Ipp32f* pMetrics) {
    return ippiDistanceTransform_5x5_8u32f_C1R(pSrc, srcStep, pDst, dstStep, *roiSize, pMetrics);
}

int ippiFloodFillGetSizex(IppiSize* roiSize, int* pBufSize) {
    return ippiFloodFillGetSize(*roiSize, pBufSize);
}

int ippiFloodFillGetSize_Gradx(IppiSize* roiSize, int* pBufSize) {
    return ippiFloodFillGetSize_Grad(*roiSize, pBufSize);
}

int ippiFloodFill_8Con_8u_C1IRx(Ipp8u* pImage, int imageStep, IppiSize* roiSize, IppiPoint* seed, Ipp8u newVal, IppiConnectedComp* pRegion, Ipp8u* pBuffer) {
    return ippiFloodFill_8Con_8u_C1IR(pImage, imageStep, *roiSize, *seed, newVal, pRegion, pBuffer);
}

int ippiFloodFill_Grad8Con_8u_C1IRx(Ipp8u* pImage, int imageStep, IppiSize* roiSize, IppiPoint* seed, Ipp8u newVal, Ipp8u minDelta, Ipp8u maxDelta, IppiConnectedComp* pRegion, Ipp8u* pBuffer) {
    return ippiFloodFill_Grad8Con_8u_C1IR(pImage, imageStep, *roiSize, *seed, newVal, minDelta, maxDelta, pRegion, pBuffer);
}

int ippiUndistortGetSizex(IppiSize* roiSize, int pBufsize[]) {
    return ippiUndistortGetSize(*roiSize, pBufsize);
}

int ippiCreateMapCameraUndistort_32f_C1Rx(Ipp32f pxMap[], int xStep, Ipp32f pyMap[], int yStep, IppiSize* roiSize, Ipp32f fx, Ipp32f fy, Ipp32f cx, Ipp32f cy, Ipp32f k1, Ipp32f k2, Ipp32f p1, Ipp32f p2, Ipp8u pBuffer[]) {
    return ippiCreateMapCameraUndistort_32f_C1R(pxMap, xStep, pyMap, yStep, *roiSize, fx, fy, cx, cy, k1, k2, p1, p2, pBuffer);
}

int ippiUndistortRadial_8u_C1Rx(Ipp8u* pSrc, int srcStep, Ipp8u* pDst, int dstStep, IppiSize* roiSize, Ipp32f fx, Ipp32f fy, Ipp32f cx, Ipp32f cy, Ipp32f k1, Ipp32f k2, Ipp8u pBuffer[]) {
    return ippiUndistortRadial_8u_C1R(pSrc, srcStep, pDst, dstStep, *roiSize, fx, fy, cx, cy, k1, k2, pBuffer);
}

int ippiUndistortRadial_32f_C1Rx(Ipp32f* pSrc, int srcStep, Ipp32f* pDst, int dstStep, IppiSize* roiSize, Ipp32f fx, Ipp32f fy, Ipp32f cx, Ipp32f cy, Ipp32f k1, Ipp32f k2, Ipp8u pBuffer[]) {
    return ippiUndistortRadial_32f_C1R(pSrc, srcStep, pDst, dstStep, *roiSize, fx, fy, cx, cy, k1, k2, pBuffer);
}

int ippiUndistortRadial_8u_C3Rx(Ipp8u* pSrc, int srcStep, Ipp8u* pDst, int dstStep, IppiSize* roiSize, Ipp32f fx, Ipp32f fy, Ipp32f cx, Ipp32f cy, Ipp32f k1, Ipp32f k2, Ipp8u pBuffer[]) {
    return ippiUndistortRadial_8u_C3R(pSrc, srcStep, pDst, dstStep, *roiSize, fx, fy, cx, cy, k1, k2, pBuffer);
}

int ippiSegmentWatershedGetBufferSize_8u_C1Rx(IppiSize* roiSize, int pBufSize[]) {
    return ippiSegmentWatershedGetBufferSize_8u_C1R(*roiSize, pBufSize);
}

int ippiSegmentWatershed_8u_C1IRx(Ipp8u* pSrc, int srcStep, Ipp8u* pMarker, int markerStep, IppiSize* roiSize, IppiNorm norm, int flag, Ipp8u* pBuffer) {
    return ippiSegmentWatershed_8u_C1IR(pSrc, srcStep, pMarker, markerStep, *roiSize, norm, flag, pBuffer);
}

