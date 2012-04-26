-- generated automatically by adapter.hs

{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS #-}

module ImagProc.Ipp.Adapt where

import Foreign
import Foreign.C.Types
import ImagProc.Ipp.Structs

foreign import ccall "adapt.h ippiAdd_8u_C1RSfsx"
    ippiAdd_8u_C1RSfsx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> Int -> IO Int
ippiAdd_8u_C1RSfs pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize scaleFactor = do
    proiSize <- new roiSize
    r <- ippiAdd_8u_C1RSfsx pSrc1 src1Step pSrc2 src2Step pDst dstStep proiSize scaleFactor
    free proiSize
    return r

foreign import ccall "adapt.h ippiSub_8u_C1RSfsx"
    ippiSub_8u_C1RSfsx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> Int -> IO Int
ippiSub_8u_C1RSfs pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize scaleFactor = do
    proiSize <- new roiSize
    r <- ippiSub_8u_C1RSfsx pSrc1 src1Step pSrc2 src2Step pDst dstStep proiSize scaleFactor
    free proiSize
    return r

foreign import ccall "adapt.h ippiMulC_32f_C1Rx"
    ippiMulC_32f_C1Rx :: Ptr Float -> Int -> Float -> Ptr Float -> Int -> Ptr IppiSize -> IO Int
ippiMulC_32f_C1R pSrc srcStep value pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiMulC_32f_C1Rx pSrc srcStep value pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiAdd_32f_C1Rx"
    ippiAdd_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> IO Int
ippiAdd_32f_C1R pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiAdd_32f_C1Rx pSrc1 src1Step pSrc2 src2Step pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiSub_32f_C1Rx"
    ippiSub_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> IO Int
ippiSub_32f_C1R pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiSub_32f_C1Rx pSrc1 src1Step pSrc2 src2Step pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiMul_32f_C1Rx"
    ippiMul_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> IO Int
ippiMul_32f_C1R pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiMul_32f_C1Rx pSrc1 src1Step pSrc2 src2Step pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiDiv_32f_C1Rx"
    ippiDiv_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> IO Int
ippiDiv_32f_C1R pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiDiv_32f_C1Rx pSrc1 src1Step pSrc2 src2Step pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiAbs_32f_C1Rx"
    ippiAbs_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> IO Int
ippiAbs_32f_C1R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiAbs_32f_C1Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiSqrt_32f_C1Rx"
    ippiSqrt_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> IO Int
ippiSqrt_32f_C1R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiSqrt_32f_C1Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiMagnitudePack_32f_C1Rx"
    ippiMagnitudePack_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> IO Int
ippiMagnitudePack_32f_C1R pSrc srcStep pDst dstStep dstRoiSize = do
    pdstRoiSize <- new dstRoiSize
    r <- ippiMagnitudePack_32f_C1Rx pSrc srcStep pDst dstStep pdstRoiSize
    free pdstRoiSize
    return r

foreign import ccall "adapt.h ippiFFTInitAlloc_R_32fx"
    ippiFFTInitAlloc_R_32fx :: Ptr () -> Int -> Int -> Int -> CInt -> IO Int
ippiFFTInitAlloc_R_32f pFFTSpec orderX orderY flag hint = do
    r <- ippiFFTInitAlloc_R_32fx pFFTSpec orderX orderY flag hint
    return r

foreign import ccall "adapt.h ippiFFTFree_R_32fx"
    ippiFFTFree_R_32fx :: Ptr () -> IO Int
ippiFFTFree_R_32f pFFTSpec = do
    r <- ippiFFTFree_R_32fx pFFTSpec
    return r

foreign import ccall "adapt.h ippiFFTGetBufSize_R_32fx"
    ippiFFTGetBufSize_R_32fx :: Ptr () -> Ptr CInt -> IO Int
ippiFFTGetBufSize_R_32f pFFTSpec pSize = do
    r <- ippiFFTGetBufSize_R_32fx pFFTSpec pSize
    return r

foreign import ccall "adapt.h ippiFFTFwd_RToPack_32f_C1Rx"
    ippiFFTFwd_RToPack_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr () -> Ptr CUChar -> IO Int
ippiFFTFwd_RToPack_32f_C1R pSrc srcStep pDst dstStep pFFTSpec pBuffer = do
    r <- ippiFFTFwd_RToPack_32f_C1Rx pSrc srcStep pDst dstStep pFFTSpec pBuffer
    return r

foreign import ccall "adapt.h ippiMirror_8u_C1Rx"
    ippiMirror_8u_C1Rx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> CInt -> IO Int
ippiMirror_8u_C1R pSrc srcStep pDst dstStep roiSize flip = do
    proiSize <- new roiSize
    r <- ippiMirror_8u_C1Rx pSrc srcStep pDst dstStep proiSize flip
    free proiSize
    return r

foreign import ccall "adapt.h ippiRemap_8u_C1Rx"
    ippiRemap_8u_C1Rx :: Ptr CUChar -> Ptr IppiSize -> Int -> Ptr IppiRect -> Ptr Float -> Int -> Ptr Float -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> Int -> IO Int
ippiRemap_8u_C1R pSrc srcSize srcStep srcROI pxMap xMapStep pyMap yMapStep pDst dstStep dstRoiSize interpolation = do
    psrcSize <- new srcSize
    psrcROI <- new srcROI
    pdstRoiSize <- new dstRoiSize
    r <- ippiRemap_8u_C1Rx pSrc psrcSize srcStep srcROI pxMap xMapStep pyMap yMapStep pDst dstStep pdstRoiSize interpolation
    free psrcSize
    free psrcROI
    free pdstRoiSize
    return r

foreign import ccall "adapt.h ippiRemap_8u_C3Rx"
    ippiRemap_8u_C3Rx :: Ptr CUChar -> Ptr IppiSize -> Int -> Ptr IppiRect -> Ptr Float -> Int -> Ptr Float -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> Int -> IO Int
ippiRemap_8u_C3R pSrc srcSize srcStep srcROI pxMap xMapStep pyMap yMapStep pDst dstStep dstRoiSize interpolation = do
    psrcSize <- new srcSize
    psrcROI <- new srcROI
    pdstRoiSize <- new dstRoiSize
    r <- ippiRemap_8u_C3Rx pSrc psrcSize srcStep srcROI pxMap xMapStep pyMap yMapStep pDst dstStep pdstRoiSize interpolation
    free psrcSize
    free psrcROI
    free pdstRoiSize
    return r

foreign import ccall "adapt.h ippiRemap_32f_C1Rx"
    ippiRemap_32f_C1Rx :: Ptr Float -> Ptr IppiSize -> Int -> Ptr IppiRect -> Ptr Float -> Int -> Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> Int -> IO Int
ippiRemap_32f_C1R pSrc srcSize srcStep srcROI pxMap xMapStep pyMap yMapStep pDst dstStep dstRoiSize interpolation = do
    psrcSize <- new srcSize
    psrcROI <- new srcROI
    pdstRoiSize <- new dstRoiSize
    r <- ippiRemap_32f_C1Rx pSrc psrcSize srcStep srcROI pxMap xMapStep pyMap yMapStep pDst dstStep pdstRoiSize interpolation
    free psrcSize
    free psrcROI
    free pdstRoiSize
    return r

foreign import ccall "adapt.h ippiSum_8u_C1Rx"
    ippiSum_8u_C1Rx :: Ptr CUChar -> Int -> Ptr IppiSize -> Ptr Double -> IO Int
ippiSum_8u_C1R pSrc srcStep roiSize pSum = do
    proiSize <- new roiSize
    r <- ippiSum_8u_C1Rx pSrc srcStep proiSize pSum
    free proiSize
    return r

foreign import ccall "adapt.h ippiSum_32f_C1Rx"
    ippiSum_32f_C1Rx :: Ptr Float -> Int -> Ptr IppiSize -> Ptr Double -> CInt -> IO Int
ippiSum_32f_C1R pSrc srcStep roiSize pSum hint = do
    proiSize <- new roiSize
    r <- ippiSum_32f_C1Rx pSrc srcStep proiSize pSum hint
    free proiSize
    return r

foreign import ccall "adapt.h ippiHistogramRange_8u_C1Rx"
    ippiHistogramRange_8u_C1Rx :: Ptr CUChar -> Int -> Ptr IppiSize -> Ptr Int32 -> Ptr Int32 -> Int -> IO Int
ippiHistogramRange_8u_C1R pSrc srcStep roiSize pHist pLevels nLevels = do
    proiSize <- new roiSize
    r <- ippiHistogramRange_8u_C1Rx pSrc srcStep proiSize pHist pLevels nLevels
    free proiSize
    return r

foreign import ccall "adapt.h ippiFilterMedian_8u_C1Rx"
    ippiFilterMedian_8u_C1Rx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> Ptr IppiSize -> Ptr IppiPoint -> IO Int
ippiFilterMedian_8u_C1R pSrc srcStep pDst dstStep dstRoiSize maskSize anchor = do
    pdstRoiSize <- new dstRoiSize
    pmaskSize <- new maskSize
    panchor <- new anchor
    r <- ippiFilterMedian_8u_C1Rx pSrc srcStep pDst dstStep pdstRoiSize pmaskSize panchor
    free pdstRoiSize
    free pmaskSize
    free panchor
    return r

foreign import ccall "adapt.h ippiFilterMax_8u_C1Rx"
    ippiFilterMax_8u_C1Rx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> Ptr IppiSize -> Ptr IppiPoint -> IO Int
ippiFilterMax_8u_C1R pSrc srcStep pDst dstStep dstRoiSize maskSize anchor = do
    pdstRoiSize <- new dstRoiSize
    pmaskSize <- new maskSize
    panchor <- new anchor
    r <- ippiFilterMax_8u_C1Rx pSrc srcStep pDst dstStep pdstRoiSize pmaskSize panchor
    free pdstRoiSize
    free pmaskSize
    free panchor
    return r

foreign import ccall "adapt.h ippiFilterMax_32f_C1Rx"
    ippiFilterMax_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> Ptr IppiSize -> Ptr IppiPoint -> IO Int
ippiFilterMax_32f_C1R pSrc srcStep pDst dstStep dstRoiSize maskSize anchor = do
    pdstRoiSize <- new dstRoiSize
    pmaskSize <- new maskSize
    panchor <- new anchor
    r <- ippiFilterMax_32f_C1Rx pSrc srcStep pDst dstStep pdstRoiSize pmaskSize panchor
    free pdstRoiSize
    free pmaskSize
    free panchor
    return r

foreign import ccall "adapt.h ippiFilterMin_8u_C1Rx"
    ippiFilterMin_8u_C1Rx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> Ptr IppiSize -> Ptr IppiPoint -> IO Int
ippiFilterMin_8u_C1R pSrc srcStep pDst dstStep dstRoiSize maskSize anchor = do
    pdstRoiSize <- new dstRoiSize
    pmaskSize <- new maskSize
    panchor <- new anchor
    r <- ippiFilterMin_8u_C1Rx pSrc srcStep pDst dstStep pdstRoiSize pmaskSize panchor
    free pdstRoiSize
    free pmaskSize
    free panchor
    return r

foreign import ccall "adapt.h ippiFilterMin_32f_C1Rx"
    ippiFilterMin_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> Ptr IppiSize -> Ptr IppiPoint -> IO Int
ippiFilterMin_32f_C1R pSrc srcStep pDst dstStep dstRoiSize maskSize anchor = do
    pdstRoiSize <- new dstRoiSize
    pmaskSize <- new maskSize
    panchor <- new anchor
    r <- ippiFilterMin_32f_C1Rx pSrc srcStep pDst dstStep pdstRoiSize pmaskSize panchor
    free pdstRoiSize
    free pmaskSize
    free panchor
    return r

foreign import ccall "adapt.h ippiFilterBox_8u_C1Rx"
    ippiFilterBox_8u_C1Rx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> Ptr IppiSize -> Ptr IppiPoint -> IO Int
ippiFilterBox_8u_C1R pSrc srcStep pDst dstStep dstRoiSize maskSize anchor = do
    pdstRoiSize <- new dstRoiSize
    pmaskSize <- new maskSize
    panchor <- new anchor
    r <- ippiFilterBox_8u_C1Rx pSrc srcStep pDst dstStep pdstRoiSize pmaskSize panchor
    free pdstRoiSize
    free pmaskSize
    free panchor
    return r

foreign import ccall "adapt.h ippiFilterBox_32f_C1Rx"
    ippiFilterBox_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> Ptr IppiSize -> Ptr IppiPoint -> IO Int
ippiFilterBox_32f_C1R pSrc srcStep pDst dstStep dstRoiSize maskSize anchor = do
    pdstRoiSize <- new dstRoiSize
    pmaskSize <- new maskSize
    panchor <- new anchor
    r <- ippiFilterBox_32f_C1Rx pSrc srcStep pDst dstStep pdstRoiSize pmaskSize panchor
    free pdstRoiSize
    free pmaskSize
    free panchor
    return r

foreign import ccall "adapt.h ippiFilterSobelVert_32f_C1Rx"
    ippiFilterSobelVert_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> IO Int
ippiFilterSobelVert_32f_C1R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiFilterSobelVert_32f_C1Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiFilterSobelHoriz_32f_C1Rx"
    ippiFilterSobelHoriz_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> IO Int
ippiFilterSobelHoriz_32f_C1R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiFilterSobelHoriz_32f_C1Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiFilterLaplace_32f_C1Rx"
    ippiFilterLaplace_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> CInt -> IO Int
ippiFilterLaplace_32f_C1R pSrc srcStep pDst dstStep roiSize mask = do
    proiSize <- new roiSize
    r <- ippiFilterLaplace_32f_C1Rx pSrc srcStep pDst dstStep proiSize mask
    free proiSize
    return r

foreign import ccall "adapt.h ippiFilterGauss_8u_C1Rx"
    ippiFilterGauss_8u_C1Rx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> CInt -> IO Int
ippiFilterGauss_8u_C1R pSrc srcStep pDst dstStep roiSize mask = do
    proiSize <- new roiSize
    r <- ippiFilterGauss_8u_C1Rx pSrc srcStep pDst dstStep proiSize mask
    free proiSize
    return r

foreign import ccall "adapt.h ippiFilterGauss_32f_C1Rx"
    ippiFilterGauss_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> CInt -> IO Int
ippiFilterGauss_32f_C1R pSrc srcStep pDst dstStep roiSize mask = do
    proiSize <- new roiSize
    r <- ippiFilterGauss_32f_C1Rx pSrc srcStep pDst dstStep proiSize mask
    free proiSize
    return r

foreign import ccall "adapt.h ippiFilterHipass_8u_C1Rx"
    ippiFilterHipass_8u_C1Rx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> CInt -> IO Int
ippiFilterHipass_8u_C1R pSrc srcStep pDst dstStep roiSize mask = do
    proiSize <- new roiSize
    r <- ippiFilterHipass_8u_C1Rx pSrc srcStep pDst dstStep proiSize mask
    free proiSize
    return r

foreign import ccall "adapt.h ippiFilter_8u_C1Rx"
    ippiFilter_8u_C1Rx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> Ptr Int32 -> Ptr IppiSize -> Ptr IppiPoint -> Int -> IO Int
ippiFilter_8u_C1R pSrc srcStep pDst dstStep dstRoiSize pKernel kernelSize anchor divisor = do
    pdstRoiSize <- new dstRoiSize
    pkernelSize <- new kernelSize
    panchor <- new anchor
    r <- ippiFilter_8u_C1Rx pSrc srcStep pDst dstStep pdstRoiSize pKernel pkernelSize panchor divisor
    free pdstRoiSize
    free pkernelSize
    free panchor
    return r

foreign import ccall "adapt.h ippiFilter_32f_C1Rx"
    ippiFilter_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> Ptr Float -> Ptr IppiSize -> Ptr IppiPoint -> IO Int
ippiFilter_32f_C1R pSrc srcStep pDst dstStep dstRoiSize pKernel kernelSize anchor = do
    pdstRoiSize <- new dstRoiSize
    pkernelSize <- new kernelSize
    panchor <- new anchor
    r <- ippiFilter_32f_C1Rx pSrc srcStep pDst dstStep pdstRoiSize pKernel pkernelSize panchor
    free pdstRoiSize
    free pkernelSize
    free panchor
    return r

foreign import ccall "adapt.h ippiFilterColumn_8u_C1Rx"
    ippiFilterColumn_8u_C1Rx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> Ptr Int32 -> Int -> Int -> Int -> IO Int
ippiFilterColumn_8u_C1R pSrc srcStep pDst dstStep dstRoiSize pKernel kernelSize yAnchor divisor = do
    pdstRoiSize <- new dstRoiSize
    r <- ippiFilterColumn_8u_C1Rx pSrc srcStep pDst dstStep pdstRoiSize pKernel kernelSize yAnchor divisor
    free pdstRoiSize
    return r

foreign import ccall "adapt.h ippiFilterColumn_32f_C1Rx"
    ippiFilterColumn_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> Ptr Float -> Int -> Int -> IO Int
ippiFilterColumn_32f_C1R pSrc srcStep pDst dstStep dstRoiSize pKernel kernelSize yAnchor = do
    pdstRoiSize <- new dstRoiSize
    r <- ippiFilterColumn_32f_C1Rx pSrc srcStep pDst dstStep pdstRoiSize pKernel kernelSize yAnchor
    free pdstRoiSize
    return r

foreign import ccall "adapt.h ippiFilterRow_8u_C1Rx"
    ippiFilterRow_8u_C1Rx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> Ptr Int32 -> Int -> Int -> Int -> IO Int
ippiFilterRow_8u_C1R pSrc srcStep pDst dstStep dstRoiSize pKernel kernelSize xAnchor divisor = do
    pdstRoiSize <- new dstRoiSize
    r <- ippiFilterRow_8u_C1Rx pSrc srcStep pDst dstStep pdstRoiSize pKernel kernelSize xAnchor divisor
    free pdstRoiSize
    return r

foreign import ccall "adapt.h ippiFilterRow_32f_C1Rx"
    ippiFilterRow_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> Ptr Float -> Int -> Int -> IO Int
ippiFilterRow_32f_C1R pSrc srcStep pDst dstStep dstRoiSize pKernel kernelSize xAnchor = do
    pdstRoiSize <- new dstRoiSize
    r <- ippiFilterRow_32f_C1Rx pSrc srcStep pDst dstStep pdstRoiSize pKernel kernelSize xAnchor
    free pdstRoiSize
    return r

foreign import ccall "adapt.h ippiThreshold_Val_8u_C1Rx"
    ippiThreshold_Val_8u_C1Rx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> CUChar -> CUChar -> CInt -> IO Int
ippiThreshold_Val_8u_C1R pSrc srcStep pDst dstStep roiSize threshold value ippCmpOp = do
    proiSize <- new roiSize
    r <- ippiThreshold_Val_8u_C1Rx pSrc srcStep pDst dstStep proiSize threshold value ippCmpOp
    free proiSize
    return r

foreign import ccall "adapt.h ippiThreshold_Val_32f_C1Rx"
    ippiThreshold_Val_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> Float -> Float -> CInt -> IO Int
ippiThreshold_Val_32f_C1R pSrc srcStep pDst dstStep roiSize threshold value ippCmpOp = do
    proiSize <- new roiSize
    r <- ippiThreshold_Val_32f_C1Rx pSrc srcStep pDst dstStep proiSize threshold value ippCmpOp
    free proiSize
    return r

foreign import ccall "adapt.h ippiComputeThreshold_Otsu_8u_C1Rx"
    ippiComputeThreshold_Otsu_8u_C1Rx :: Ptr CUChar -> Int -> Ptr IppiSize -> Ptr CUChar -> IO Int
ippiComputeThreshold_Otsu_8u_C1R pSrc srcStep roiSize pThreshold = do
    proiSize <- new roiSize
    r <- ippiComputeThreshold_Otsu_8u_C1Rx pSrc srcStep proiSize pThreshold
    free proiSize
    return r

foreign import ccall "adapt.h ippiCopy_8u_C3C1Rx"
    ippiCopy_8u_C3C1Rx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> IO Int
ippiCopy_8u_C3C1R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiCopy_8u_C3C1Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiCopy_8u_C1C3Rx"
    ippiCopy_8u_C1C3Rx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> IO Int
ippiCopy_8u_C1C3R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiCopy_8u_C1C3Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiCopy_8u_C1Rx"
    ippiCopy_8u_C1Rx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> IO Int
ippiCopy_8u_C1R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiCopy_8u_C1Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiCopy_8u_C3Rx"
    ippiCopy_8u_C3Rx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> IO Int
ippiCopy_8u_C3R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiCopy_8u_C3Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiCopy_8u_C1MRx"
    ippiCopy_8u_C1MRx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> Ptr CUChar -> Int -> IO Int
ippiCopy_8u_C1MR pSrc srcStep pDst dstStep roiSize pMask maskStep = do
    proiSize <- new roiSize
    r <- ippiCopy_8u_C1MRx pSrc srcStep pDst dstStep proiSize pMask maskStep
    free proiSize
    return r

foreign import ccall "adapt.h ippiCopy_32f_C1Rx"
    ippiCopy_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> IO Int
ippiCopy_32f_C1R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiCopy_32f_C1Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiCopy_32f_C1MRx"
    ippiCopy_32f_C1MRx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> Ptr CUChar -> Int -> IO Int
ippiCopy_32f_C1MR pSrc srcStep pDst dstStep roiSize pMask maskStep = do
    proiSize <- new roiSize
    r <- ippiCopy_32f_C1MRx pSrc srcStep pDst dstStep proiSize pMask maskStep
    free proiSize
    return r

foreign import ccall "adapt.h ippiSet_8u_C1Rx"
    ippiSet_8u_C1Rx :: CUChar -> Ptr CUChar -> Int -> Ptr IppiSize -> IO Int
ippiSet_8u_C1R value pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiSet_8u_C1Rx value pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiSet_8u_C3Rx"
    ippiSet_8u_C3Rx :: Ptr CUChar -> Ptr CUChar -> Int -> Ptr IppiSize -> IO Int
ippiSet_8u_C3R value pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiSet_8u_C3Rx value pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiSet_32f_C1Rx"
    ippiSet_32f_C1Rx :: Float -> Ptr Float -> Int -> Ptr IppiSize -> IO Int
ippiSet_32f_C1R value pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiSet_32f_C1Rx value pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiImageJaehne_32f_C1Rx"
    ippiImageJaehne_32f_C1Rx :: Ptr Float -> Int -> Ptr IppiSize -> IO Int
ippiImageJaehne_32f_C1R pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiImageJaehne_32f_C1Rx pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiScale_8u32f_C1Rx"
    ippiScale_8u32f_C1Rx :: Ptr CUChar -> Int -> Ptr Float -> Int -> Ptr IppiSize -> Float -> Float -> IO Int
ippiScale_8u32f_C1R pSrc srcStep pDst dstStep roiSize vMin vMax = do
    proiSize <- new roiSize
    r <- ippiScale_8u32f_C1Rx pSrc srcStep pDst dstStep proiSize vMin vMax
    free proiSize
    return r

foreign import ccall "adapt.h ippiScale_32f8u_C1Rx"
    ippiScale_32f8u_C1Rx :: Ptr Float -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> Float -> Float -> IO Int
ippiScale_32f8u_C1R pSrc srcStep pDst dstStep roiSize vMin vMax = do
    proiSize <- new roiSize
    r <- ippiScale_32f8u_C1Rx pSrc srcStep pDst dstStep proiSize vMin vMax
    free proiSize
    return r

foreign import ccall "adapt.h ippiMaxIndx_8u_C1Rx"
    ippiMaxIndx_8u_C1Rx :: Ptr CUChar -> Int -> Ptr IppiSize -> Ptr CUChar -> Ptr CInt -> Ptr CInt -> IO Int
ippiMaxIndx_8u_C1R pSrc srcStep roiSize pMax pIndexX pIndexY = do
    proiSize <- new roiSize
    r <- ippiMaxIndx_8u_C1Rx pSrc srcStep proiSize pMax pIndexX pIndexY
    free proiSize
    return r

foreign import ccall "adapt.h ippiMaxIndx_32f_C1Rx"
    ippiMaxIndx_32f_C1Rx :: Ptr Float -> Int -> Ptr IppiSize -> Ptr Float -> Ptr CInt -> Ptr CInt -> IO Int
ippiMaxIndx_32f_C1R pSrc srcStep roiSize pMax pIndexX pIndexY = do
    proiSize <- new roiSize
    r <- ippiMaxIndx_32f_C1Rx pSrc srcStep proiSize pMax pIndexX pIndexY
    free proiSize
    return r

foreign import ccall "adapt.h ippiMinMax_32f_C1Rx"
    ippiMinMax_32f_C1Rx :: Ptr Float -> Int -> Ptr IppiSize -> Ptr Float -> Ptr Float -> IO Int
ippiMinMax_32f_C1R pSrc srcStep roiSize pMin pMax = do
    proiSize <- new roiSize
    r <- ippiMinMax_32f_C1Rx pSrc srcStep proiSize pMin pMax
    free proiSize
    return r

foreign import ccall "adapt.h ippiMaxEvery_8u_C1IRx"
    ippiMaxEvery_8u_C1IRx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> IO Int
ippiMaxEvery_8u_C1IR pSrc srcStep pSrcDst srcdstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiMaxEvery_8u_C1IRx pSrc srcStep pSrcDst srcdstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiMinEvery_8u_C1IRx"
    ippiMinEvery_8u_C1IRx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> IO Int
ippiMinEvery_8u_C1IR pSrc srcStep pSrcDst srcdstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiMinEvery_8u_C1IRx pSrc srcStep pSrcDst srcdstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiMaxEvery_32f_C1IRx"
    ippiMaxEvery_32f_C1IRx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> IO Int
ippiMaxEvery_32f_C1IR pSrc srcStep pSrcDst srcdstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiMaxEvery_32f_C1IRx pSrc srcStep pSrcDst srcdstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiMinEvery_32f_C1IRx"
    ippiMinEvery_32f_C1IRx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> IO Int
ippiMinEvery_32f_C1IR pSrc srcStep pSrcDst srcdstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiMinEvery_32f_C1IRx pSrc srcStep pSrcDst srcdstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiAnd_8u_C1Rx"
    ippiAnd_8u_C1Rx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> IO Int
ippiAnd_8u_C1R pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiAnd_8u_C1Rx pSrc1 src1Step pSrc2 src2Step pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiOr_8u_C1Rx"
    ippiOr_8u_C1Rx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> IO Int
ippiOr_8u_C1R pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiOr_8u_C1Rx pSrc1 src1Step pSrc2 src2Step pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiXor_8u_C1Rx"
    ippiXor_8u_C1Rx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> IO Int
ippiXor_8u_C1R pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiXor_8u_C1Rx pSrc1 src1Step pSrc2 src2Step pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiNot_8u_C1Rx"
    ippiNot_8u_C1Rx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> IO Int
ippiNot_8u_C1R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiNot_8u_C1Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiCompare_8u_C1Rx"
    ippiCompare_8u_C1Rx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> CInt -> IO Int
ippiCompare_8u_C1R pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize ippCmpOp = do
    proiSize <- new roiSize
    r <- ippiCompare_8u_C1Rx pSrc1 src1Step pSrc2 src2Step pDst dstStep proiSize ippCmpOp
    free proiSize
    return r

foreign import ccall "adapt.h ippiCompareC_8u_C1Rx"
    ippiCompareC_8u_C1Rx :: Ptr CUChar -> Int -> CUChar -> Ptr CUChar -> Int -> Ptr IppiSize -> CInt -> IO Int
ippiCompareC_8u_C1R pSrc srcStep value pDst dstStep roiSize ippCmpOp = do
    proiSize <- new roiSize
    r <- ippiCompareC_8u_C1Rx pSrc srcStep value pDst dstStep proiSize ippCmpOp
    free proiSize
    return r

foreign import ccall "adapt.h ippiCompare_32f_C1Rx"
    ippiCompare_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> CInt -> IO Int
ippiCompare_32f_C1R pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize ippCmpOp = do
    proiSize <- new roiSize
    r <- ippiCompare_32f_C1Rx pSrc1 src1Step pSrc2 src2Step pDst dstStep proiSize ippCmpOp
    free proiSize
    return r

foreign import ccall "adapt.h ippiErode3x3_8u_C1Rx"
    ippiErode3x3_8u_C1Rx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> IO Int
ippiErode3x3_8u_C1R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiErode3x3_8u_C1Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiDilate3x3_8u_C1Rx"
    ippiDilate3x3_8u_C1Rx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> IO Int
ippiDilate3x3_8u_C1R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiDilate3x3_8u_C1Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiRGBToYUV420_8u_C3P3Rx"
    ippiRGBToYUV420_8u_C3P3Rx :: Ptr CUChar -> Int -> Ptr () -> Ptr CInt -> Ptr IppiSize -> IO Int
ippiRGBToYUV420_8u_C3P3R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiRGBToYUV420_8u_C3P3Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiYUV420ToRGB_8u_P3C3Rx"
    ippiYUV420ToRGB_8u_P3C3Rx :: Ptr () -> Ptr CInt -> Ptr CUChar -> Int -> Ptr IppiSize -> IO Int
ippiYUV420ToRGB_8u_P3C3R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiYUV420ToRGB_8u_P3C3Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiYUV420ToRGB_8u_P3Rx"
    ippiYUV420ToRGB_8u_P3Rx :: Ptr () -> Ptr CInt -> Ptr () -> Int -> Ptr IppiSize -> IO Int
ippiYUV420ToRGB_8u_P3R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiYUV420ToRGB_8u_P3Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiRGBToGray_8u_C3C1Rx"
    ippiRGBToGray_8u_C3C1Rx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> IO Int
ippiRGBToGray_8u_C3C1R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiRGBToGray_8u_C3C1Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiRGBToHSV_8u_C3Rx"
    ippiRGBToHSV_8u_C3Rx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> IO Int
ippiRGBToHSV_8u_C3R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiRGBToHSV_8u_C3Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiHSVToRGB_8u_C3Rx"
    ippiHSVToRGB_8u_C3Rx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> IO Int
ippiHSVToRGB_8u_C3R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiHSVToRGB_8u_C3Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiSampleLine_8u_C1Rx"
    ippiSampleLine_8u_C1Rx :: Ptr CUChar -> Int -> Ptr IppiSize -> Ptr CUChar -> Ptr IppiPoint -> Ptr IppiPoint -> IO Int
ippiSampleLine_8u_C1R pSrc srcStep roiSize pDst pt1 pt2 = do
    proiSize <- new roiSize
    ppt1 <- new pt1
    ppt2 <- new pt2
    r <- ippiSampleLine_8u_C1Rx pSrc srcStep proiSize pDst ppt1 ppt2
    free proiSize
    free ppt1
    free ppt2
    return r

foreign import ccall "adapt.h ippiSampleLine_32f_C1Rx"
    ippiSampleLine_32f_C1Rx :: Ptr Float -> Int -> Ptr IppiSize -> Ptr Float -> Ptr IppiPoint -> Ptr IppiPoint -> IO Int
ippiSampleLine_32f_C1R pSrc srcStep roiSize pDst pt1 pt2 = do
    proiSize <- new roiSize
    ppt1 <- new pt1
    ppt2 <- new pt2
    r <- ippiSampleLine_32f_C1Rx pSrc srcStep proiSize pDst ppt1 ppt2
    free proiSize
    free ppt1
    free ppt2
    return r

foreign import ccall "adapt.h ippiAbsDiff_8u_C1Rx"
    ippiAbsDiff_8u_C1Rx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> IO Int
ippiAbsDiff_8u_C1R pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiAbsDiff_8u_C1Rx pSrc1 src1Step pSrc2 src2Step pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiAbsDiff_32f_C1Rx"
    ippiAbsDiff_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> IO Int
ippiAbsDiff_32f_C1R pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiAbsDiff_32f_C1Rx pSrc1 src1Step pSrc2 src2Step pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiIntegral_8u32f_C1Rx"
    ippiIntegral_8u32f_C1Rx :: Ptr CUChar -> Int -> Ptr Float -> Int -> Ptr IppiSize -> Float -> IO Int
ippiIntegral_8u32f_C1R pSrc srcStep pDst dstStep roiSize val = do
    proiSize <- new roiSize
    r <- ippiIntegral_8u32f_C1Rx pSrc srcStep pDst dstStep proiSize val
    free proiSize
    return r

foreign import ccall "adapt.h ippiSqrIntegral_8u32f64f_C1Rx"
    ippiSqrIntegral_8u32f64f_C1Rx :: Ptr CUChar -> Int -> Ptr Float -> Int -> Ptr Double -> Int -> Ptr IppiSize -> Float -> Double -> IO Int
ippiSqrIntegral_8u32f64f_C1R pSrc srcStep pDst dstStep pSqr sqrStep roiSize val valSqr = do
    proiSize <- new roiSize
    r <- ippiSqrIntegral_8u32f64f_C1Rx pSrc srcStep pDst dstStep pSqr sqrStep proiSize val valSqr
    free proiSize
    return r

foreign import ccall "adapt.h ippiRectStdDev_32f_C1Rx"
    ippiRectStdDev_32f_C1Rx :: Ptr Float -> Int -> Ptr Double -> Int -> Ptr Float -> Int -> Ptr IppiSize -> Ptr IppiRect -> IO Int
ippiRectStdDev_32f_C1R pSrc srcStep pSqr sqrStep pDst dstStep roiSize rect = do
    proiSize <- new roiSize
    prect <- new rect
    r <- ippiRectStdDev_32f_C1Rx pSrc srcStep pSqr sqrStep pDst dstStep proiSize rect
    free proiSize
    free prect
    return r

foreign import ccall "adapt.h ippiCannyGetSizex"
    ippiCannyGetSizex :: Ptr IppiSize -> Ptr CInt -> IO Int
ippiCannyGetSize roiSize bufferSize = do
    proiSize <- new roiSize
    r <- ippiCannyGetSizex proiSize bufferSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiCanny_32f8u_C1Rx"
    ippiCanny_32f8u_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> Float -> Float -> Ptr CUChar -> IO Int
ippiCanny_32f8u_C1R pSrcDx srcDxStep pSrcDy srcDyStep pDstEdges dstEdgeStep roiSize lowThresh highThresh pBuffer = do
    proiSize <- new roiSize
    r <- ippiCanny_32f8u_C1Rx pSrcDx srcDxStep pSrcDy srcDyStep pDstEdges dstEdgeStep proiSize lowThresh highThresh pBuffer
    free proiSize
    return r

foreign import ccall "adapt.h ippiDistanceTransform_3x3_8u32f_C1Rx"
    ippiDistanceTransform_3x3_8u32f_C1Rx :: Ptr CUChar -> Int -> Ptr Float -> Int -> Ptr IppiSize -> Ptr Float -> IO Int
ippiDistanceTransform_3x3_8u32f_C1R pSrc srcStep pDst dstStep roiSize pMetrics = do
    proiSize <- new roiSize
    r <- ippiDistanceTransform_3x3_8u32f_C1Rx pSrc srcStep pDst dstStep proiSize pMetrics
    free proiSize
    return r

foreign import ccall "adapt.h ippiDistanceTransform_5x5_8u32f_C1Rx"
    ippiDistanceTransform_5x5_8u32f_C1Rx :: Ptr CUChar -> Int -> Ptr Float -> Int -> Ptr IppiSize -> Ptr Float -> IO Int
ippiDistanceTransform_5x5_8u32f_C1R pSrc srcStep pDst dstStep roiSize pMetrics = do
    proiSize <- new roiSize
    r <- ippiDistanceTransform_5x5_8u32f_C1Rx pSrc srcStep pDst dstStep proiSize pMetrics
    free proiSize
    return r

foreign import ccall "adapt.h ippiFloodFillGetSizex"
    ippiFloodFillGetSizex :: Ptr IppiSize -> Ptr CInt -> IO Int
ippiFloodFillGetSize roiSize pBufSize = do
    proiSize <- new roiSize
    r <- ippiFloodFillGetSizex proiSize pBufSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiFloodFillGetSize_Gradx"
    ippiFloodFillGetSize_Gradx :: Ptr IppiSize -> Ptr CInt -> IO Int
ippiFloodFillGetSize_Grad roiSize pBufSize = do
    proiSize <- new roiSize
    r <- ippiFloodFillGetSize_Gradx proiSize pBufSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiFloodFill_8Con_8u_C1IRx"
    ippiFloodFill_8Con_8u_C1IRx :: Ptr CUChar -> Int -> Ptr IppiSize -> Ptr IppiPoint -> CUChar -> Ptr IppiConnectedComp -> Ptr CUChar -> IO Int
ippiFloodFill_8Con_8u_C1IR pImage imageStep roiSize seed newVal pRegion pBuffer = do
    proiSize <- new roiSize
    pseed <- new seed
    r <- ippiFloodFill_8Con_8u_C1IRx pImage imageStep proiSize pseed newVal pRegion pBuffer
    free proiSize
    free pseed
    return r

foreign import ccall "adapt.h ippiFloodFill_Grad8Con_8u_C1IRx"
    ippiFloodFill_Grad8Con_8u_C1IRx :: Ptr CUChar -> Int -> Ptr IppiSize -> Ptr IppiPoint -> CUChar -> CUChar -> CUChar -> Ptr IppiConnectedComp -> Ptr CUChar -> IO Int
ippiFloodFill_Grad8Con_8u_C1IR pImage imageStep roiSize seed newVal minDelta maxDelta pRegion pBuffer = do
    proiSize <- new roiSize
    pseed <- new seed
    r <- ippiFloodFill_Grad8Con_8u_C1IRx pImage imageStep proiSize pseed newVal minDelta maxDelta pRegion pBuffer
    free proiSize
    free pseed
    return r

foreign import ccall "adapt.h ippiUndistortGetSizex"
    ippiUndistortGetSizex :: Ptr IppiSize -> Ptr CInt -> IO Int
ippiUndistortGetSize roiSize pBufsize = do
    proiSize <- new roiSize
    r <- ippiUndistortGetSizex proiSize pBufsize
    free proiSize
    return r

foreign import ccall "adapt.h ippiCreateMapCameraUndistort_32f_C1Rx"
    ippiCreateMapCameraUndistort_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Ptr CUChar -> IO Int
ippiCreateMapCameraUndistort_32f_C1R pxMap xStep pyMap yStep roiSize fx fy cx cy k1 k2 p1 p2 pBuffer = do
    proiSize <- new roiSize
    r <- ippiCreateMapCameraUndistort_32f_C1Rx pxMap xStep pyMap yStep proiSize fx fy cx cy k1 k2 p1 p2 pBuffer
    free proiSize
    return r

foreign import ccall "adapt.h ippiUndistortRadial_8u_C1Rx"
    ippiUndistortRadial_8u_C1Rx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> Float -> Float -> Float -> Float -> Float -> Float -> Ptr CUChar -> IO Int
ippiUndistortRadial_8u_C1R pSrc srcStep pDst dstStep roiSize fx fy cx cy k1 k2 pBuffer = do
    proiSize <- new roiSize
    r <- ippiUndistortRadial_8u_C1Rx pSrc srcStep pDst dstStep proiSize fx fy cx cy k1 k2 pBuffer
    free proiSize
    return r

foreign import ccall "adapt.h ippiUndistortRadial_32f_C1Rx"
    ippiUndistortRadial_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> Float -> Float -> Float -> Float -> Float -> Float -> Ptr CUChar -> IO Int
ippiUndistortRadial_32f_C1R pSrc srcStep pDst dstStep roiSize fx fy cx cy k1 k2 pBuffer = do
    proiSize <- new roiSize
    r <- ippiUndistortRadial_32f_C1Rx pSrc srcStep pDst dstStep proiSize fx fy cx cy k1 k2 pBuffer
    free proiSize
    return r

foreign import ccall "adapt.h ippiUndistortRadial_8u_C3Rx"
    ippiUndistortRadial_8u_C3Rx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> Float -> Float -> Float -> Float -> Float -> Float -> Ptr CUChar -> IO Int
ippiUndistortRadial_8u_C3R pSrc srcStep pDst dstStep roiSize fx fy cx cy k1 k2 pBuffer = do
    proiSize <- new roiSize
    r <- ippiUndistortRadial_8u_C3Rx pSrc srcStep pDst dstStep proiSize fx fy cx cy k1 k2 pBuffer
    free proiSize
    return r

foreign import ccall "adapt.h ippiSegmentWatershedGetBufferSize_8u_C1Rx"
    ippiSegmentWatershedGetBufferSize_8u_C1Rx :: Ptr IppiSize -> Ptr CInt -> IO Int
ippiSegmentWatershedGetBufferSize_8u_C1R roiSize pBufSize = do
    proiSize <- new roiSize
    r <- ippiSegmentWatershedGetBufferSize_8u_C1Rx proiSize pBufSize
    free proiSize
    return r

foreign import ccall "adapt.h ippiSegmentWatershed_8u_C1IRx"
    ippiSegmentWatershed_8u_C1IRx :: Ptr CUChar -> Int -> Ptr CUChar -> Int -> Ptr IppiSize -> CInt -> Int -> Ptr CUChar -> IO Int
ippiSegmentWatershed_8u_C1IR pSrc srcStep pMarker markerStep roiSize norm flag pBuffer = do
    proiSize <- new roiSize
    r <- ippiSegmentWatershed_8u_C1IRx pSrc srcStep pMarker markerStep proiSize norm flag pBuffer
    free proiSize
    return r

