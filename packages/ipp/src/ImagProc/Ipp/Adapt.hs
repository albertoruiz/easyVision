-- generated automatically by adapter.hs

{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS #-}

module ImagProc.Ipp.Adapt where

import Foreign
import Foreign.C.Types
import ImagProc.Ipp.Structs

foreign import ccall "ippiAdd_8u_C1RSfsx"
    ippiAdd_8u_C1RSfsx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> Int -> IO CInt
ippiAdd_8u_C1RSfs pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize scaleFactor = do
    proiSize <- new roiSize
    r <- ippiAdd_8u_C1RSfsx pSrc1 src1Step pSrc2 src2Step pDst dstStep proiSize scaleFactor
    free proiSize
    return r

foreign import ccall "ippiSub_8u_C1RSfsx"
    ippiSub_8u_C1RSfsx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> Int -> IO CInt
ippiSub_8u_C1RSfs pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize scaleFactor = do
    proiSize <- new roiSize
    r <- ippiSub_8u_C1RSfsx pSrc1 src1Step pSrc2 src2Step pDst dstStep proiSize scaleFactor
    free proiSize
    return r

foreign import ccall "ippiAddC_8u_C1RSfsx"
    ippiAddC_8u_C1RSfsx :: Ptr Word8 -> Int -> Word8 -> Ptr Word8 -> Int -> Ptr IppiSize -> Int -> IO CInt
ippiAddC_8u_C1RSfs pSrc srcStep value pDst dstStep roiSize scaleFactor = do
    proiSize <- new roiSize
    r <- ippiAddC_8u_C1RSfsx pSrc srcStep value pDst dstStep proiSize scaleFactor
    free proiSize
    return r

foreign import ccall "ippiSubC_8u_C1RSfsx"
    ippiSubC_8u_C1RSfsx :: Ptr Word8 -> Int -> Word8 -> Ptr Word8 -> Int -> Ptr IppiSize -> Int -> IO CInt
ippiSubC_8u_C1RSfs pSrc srcStep value pDst dstStep roiSize scaleFactor = do
    proiSize <- new roiSize
    r <- ippiSubC_8u_C1RSfsx pSrc srcStep value pDst dstStep proiSize scaleFactor
    free proiSize
    return r

foreign import ccall "ippiAddC_32f_C1Rx"
    ippiAddC_32f_C1Rx :: Ptr Float -> Int -> Float -> Ptr Float -> Int -> Ptr IppiSize -> IO CInt
ippiAddC_32f_C1R pSrc srcStep value pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiAddC_32f_C1Rx pSrc srcStep value pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiMulC_32f_C1Rx"
    ippiMulC_32f_C1Rx :: Ptr Float -> Int -> Float -> Ptr Float -> Int -> Ptr IppiSize -> IO CInt
ippiMulC_32f_C1R pSrc srcStep value pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiMulC_32f_C1Rx pSrc srcStep value pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiAdd_32f_C1Rx"
    ippiAdd_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> IO CInt
ippiAdd_32f_C1R pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiAdd_32f_C1Rx pSrc1 src1Step pSrc2 src2Step pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiSub_32f_C1Rx"
    ippiSub_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> IO CInt
ippiSub_32f_C1R pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiSub_32f_C1Rx pSrc1 src1Step pSrc2 src2Step pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiMul_32f_C1Rx"
    ippiMul_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> IO CInt
ippiMul_32f_C1R pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiMul_32f_C1Rx pSrc1 src1Step pSrc2 src2Step pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiDiv_32f_C1Rx"
    ippiDiv_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> IO CInt
ippiDiv_32f_C1R pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiDiv_32f_C1Rx pSrc1 src1Step pSrc2 src2Step pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiAbs_32f_C1Rx"
    ippiAbs_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> IO CInt
ippiAbs_32f_C1R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiAbs_32f_C1Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiSqrt_32f_C1Rx"
    ippiSqrt_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> IO CInt
ippiSqrt_32f_C1R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiSqrt_32f_C1Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiMagnitudePack_32f_C1Rx"
    ippiMagnitudePack_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> IO CInt
ippiMagnitudePack_32f_C1R pSrc srcStep pDst dstStep dstRoiSize = do
    pdstRoiSize <- new dstRoiSize
    r <- ippiMagnitudePack_32f_C1Rx pSrc srcStep pDst dstStep pdstRoiSize
    free pdstRoiSize
    return r

foreign import ccall "ippiFFTInitAlloc_R_32fx"
    ippiFFTInitAlloc_R_32fx :: Ptr () -> Int -> Int -> Int -> CInt -> IO CInt
ippiFFTInitAlloc_R_32f pFFTSpec orderX orderY flag hint = do
    r <- ippiFFTInitAlloc_R_32fx pFFTSpec orderX orderY flag hint
    return r

foreign import ccall "ippiFFTFree_R_32fx"
    ippiFFTFree_R_32fx :: Ptr () -> IO CInt
ippiFFTFree_R_32f pFFTSpec = do
    r <- ippiFFTFree_R_32fx pFFTSpec
    return r

foreign import ccall "ippiFFTGetBufSize_R_32fx"
    ippiFFTGetBufSize_R_32fx :: Ptr () -> Ptr CInt -> IO CInt
ippiFFTGetBufSize_R_32f pFFTSpec pSize = do
    r <- ippiFFTGetBufSize_R_32fx pFFTSpec pSize
    return r

foreign import ccall "ippiFFTFwd_RToPack_32f_C1Rx"
    ippiFFTFwd_RToPack_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr () -> Ptr Word8 -> IO CInt
ippiFFTFwd_RToPack_32f_C1R pSrc srcStep pDst dstStep pFFTSpec pBuffer = do
    r <- ippiFFTFwd_RToPack_32f_C1Rx pSrc srcStep pDst dstStep pFFTSpec pBuffer
    return r

foreign import ccall "ippiMirror_8u_C1Rx"
    ippiMirror_8u_C1Rx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> CInt -> IO CInt
ippiMirror_8u_C1R pSrc srcStep pDst dstStep roiSize flip = do
    proiSize <- new roiSize
    r <- ippiMirror_8u_C1Rx pSrc srcStep pDst dstStep proiSize flip
    free proiSize
    return r

foreign import ccall "ippiRemap_8u_C1Rx"
    ippiRemap_8u_C1Rx :: Ptr Word8 -> Ptr IppiSize -> Int -> Ptr IppiRect -> Ptr Float -> Int -> Ptr Float -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> Int -> IO CInt
ippiRemap_8u_C1R pSrc srcSize srcStep srcROI pxMap xMapStep pyMap yMapStep pDst dstStep dstRoiSize interpolation = do
    psrcSize <- new srcSize
    psrcROI <- new srcROI
    pdstRoiSize <- new dstRoiSize
    r <- ippiRemap_8u_C1Rx pSrc psrcSize srcStep srcROI pxMap xMapStep pyMap yMapStep pDst dstStep pdstRoiSize interpolation
    free psrcSize
    free psrcROI
    free pdstRoiSize
    return r

foreign import ccall "ippiRemap_8u_C3Rx"
    ippiRemap_8u_C3Rx :: Ptr Word8 -> Ptr IppiSize -> Int -> Ptr IppiRect -> Ptr Float -> Int -> Ptr Float -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> Int -> IO CInt
ippiRemap_8u_C3R pSrc srcSize srcStep srcROI pxMap xMapStep pyMap yMapStep pDst dstStep dstRoiSize interpolation = do
    psrcSize <- new srcSize
    psrcROI <- new srcROI
    pdstRoiSize <- new dstRoiSize
    r <- ippiRemap_8u_C3Rx pSrc psrcSize srcStep srcROI pxMap xMapStep pyMap yMapStep pDst dstStep pdstRoiSize interpolation
    free psrcSize
    free psrcROI
    free pdstRoiSize
    return r

foreign import ccall "ippiRemap_32f_C1Rx"
    ippiRemap_32f_C1Rx :: Ptr Float -> Ptr IppiSize -> Int -> Ptr IppiRect -> Ptr Float -> Int -> Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> Int -> IO CInt
ippiRemap_32f_C1R pSrc srcSize srcStep srcROI pxMap xMapStep pyMap yMapStep pDst dstStep dstRoiSize interpolation = do
    psrcSize <- new srcSize
    psrcROI <- new srcROI
    pdstRoiSize <- new dstRoiSize
    r <- ippiRemap_32f_C1Rx pSrc psrcSize srcStep srcROI pxMap xMapStep pyMap yMapStep pDst dstStep pdstRoiSize interpolation
    free psrcSize
    free psrcROI
    free pdstRoiSize
    return r

foreign import ccall "ippiSum_8u_C1Rx"
    ippiSum_8u_C1Rx :: Ptr Word8 -> Int -> Ptr IppiSize -> Ptr Double -> IO CInt
ippiSum_8u_C1R pSrc srcStep roiSize pSum = do
    proiSize <- new roiSize
    r <- ippiSum_8u_C1Rx pSrc srcStep proiSize pSum
    free proiSize
    return r

foreign import ccall "ippiSum_32f_C1Rx"
    ippiSum_32f_C1Rx :: Ptr Float -> Int -> Ptr IppiSize -> Ptr Double -> CInt -> IO CInt
ippiSum_32f_C1R pSrc srcStep roiSize pSum hint = do
    proiSize <- new roiSize
    r <- ippiSum_32f_C1Rx pSrc srcStep proiSize pSum hint
    free proiSize
    return r

foreign import ccall "ippiHistogramRange_8u_C1Rx"
    ippiHistogramRange_8u_C1Rx :: Ptr Word8 -> Int -> Ptr IppiSize -> Ptr Int32 -> Ptr Int32 -> Int -> IO CInt
ippiHistogramRange_8u_C1R pSrc srcStep roiSize pHist pLevels nLevels = do
    proiSize <- new roiSize
    r <- ippiHistogramRange_8u_C1Rx pSrc srcStep proiSize pHist pLevels nLevels
    free proiSize
    return r

foreign import ccall "ippiFilterMedian_8u_C1Rx"
    ippiFilterMedian_8u_C1Rx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> Ptr IppiSize -> Ptr IppiPoint -> IO CInt
ippiFilterMedian_8u_C1R pSrc srcStep pDst dstStep dstRoiSize maskSize anchor = do
    pdstRoiSize <- new dstRoiSize
    pmaskSize <- new maskSize
    panchor <- new anchor
    r <- ippiFilterMedian_8u_C1Rx pSrc srcStep pDst dstStep pdstRoiSize pmaskSize panchor
    free pdstRoiSize
    free pmaskSize
    free panchor
    return r

foreign import ccall "ippiFilterMax_8u_C1Rx"
    ippiFilterMax_8u_C1Rx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> Ptr IppiSize -> Ptr IppiPoint -> IO CInt
ippiFilterMax_8u_C1R pSrc srcStep pDst dstStep dstRoiSize maskSize anchor = do
    pdstRoiSize <- new dstRoiSize
    pmaskSize <- new maskSize
    panchor <- new anchor
    r <- ippiFilterMax_8u_C1Rx pSrc srcStep pDst dstStep pdstRoiSize pmaskSize panchor
    free pdstRoiSize
    free pmaskSize
    free panchor
    return r

foreign import ccall "ippiFilterMax_32f_C1Rx"
    ippiFilterMax_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> Ptr IppiSize -> Ptr IppiPoint -> IO CInt
ippiFilterMax_32f_C1R pSrc srcStep pDst dstStep dstRoiSize maskSize anchor = do
    pdstRoiSize <- new dstRoiSize
    pmaskSize <- new maskSize
    panchor <- new anchor
    r <- ippiFilterMax_32f_C1Rx pSrc srcStep pDst dstStep pdstRoiSize pmaskSize panchor
    free pdstRoiSize
    free pmaskSize
    free panchor
    return r

foreign import ccall "ippiFilterMin_8u_C1Rx"
    ippiFilterMin_8u_C1Rx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> Ptr IppiSize -> Ptr IppiPoint -> IO CInt
ippiFilterMin_8u_C1R pSrc srcStep pDst dstStep dstRoiSize maskSize anchor = do
    pdstRoiSize <- new dstRoiSize
    pmaskSize <- new maskSize
    panchor <- new anchor
    r <- ippiFilterMin_8u_C1Rx pSrc srcStep pDst dstStep pdstRoiSize pmaskSize panchor
    free pdstRoiSize
    free pmaskSize
    free panchor
    return r

foreign import ccall "ippiFilterMin_32f_C1Rx"
    ippiFilterMin_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> Ptr IppiSize -> Ptr IppiPoint -> IO CInt
ippiFilterMin_32f_C1R pSrc srcStep pDst dstStep dstRoiSize maskSize anchor = do
    pdstRoiSize <- new dstRoiSize
    pmaskSize <- new maskSize
    panchor <- new anchor
    r <- ippiFilterMin_32f_C1Rx pSrc srcStep pDst dstStep pdstRoiSize pmaskSize panchor
    free pdstRoiSize
    free pmaskSize
    free panchor
    return r

foreign import ccall "ippiFilterBox_8u_C1Rx"
    ippiFilterBox_8u_C1Rx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> Ptr IppiSize -> Ptr IppiPoint -> IO CInt
ippiFilterBox_8u_C1R pSrc srcStep pDst dstStep dstRoiSize maskSize anchor = do
    pdstRoiSize <- new dstRoiSize
    pmaskSize <- new maskSize
    panchor <- new anchor
    r <- ippiFilterBox_8u_C1Rx pSrc srcStep pDst dstStep pdstRoiSize pmaskSize panchor
    free pdstRoiSize
    free pmaskSize
    free panchor
    return r

foreign import ccall "ippiFilterBox_32f_C1Rx"
    ippiFilterBox_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> Ptr IppiSize -> Ptr IppiPoint -> IO CInt
ippiFilterBox_32f_C1R pSrc srcStep pDst dstStep dstRoiSize maskSize anchor = do
    pdstRoiSize <- new dstRoiSize
    pmaskSize <- new maskSize
    panchor <- new anchor
    r <- ippiFilterBox_32f_C1Rx pSrc srcStep pDst dstStep pdstRoiSize pmaskSize panchor
    free pdstRoiSize
    free pmaskSize
    free panchor
    return r

foreign import ccall "ippiFilterSobelVert_32f_C1Rx"
    ippiFilterSobelVert_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> IO CInt
ippiFilterSobelVert_32f_C1R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiFilterSobelVert_32f_C1Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiFilterSobelHoriz_32f_C1Rx"
    ippiFilterSobelHoriz_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> IO CInt
ippiFilterSobelHoriz_32f_C1R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiFilterSobelHoriz_32f_C1Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiFilterLaplace_32f_C1Rx"
    ippiFilterLaplace_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> CInt -> IO CInt
ippiFilterLaplace_32f_C1R pSrc srcStep pDst dstStep roiSize mask = do
    proiSize <- new roiSize
    r <- ippiFilterLaplace_32f_C1Rx pSrc srcStep pDst dstStep proiSize mask
    free proiSize
    return r

foreign import ccall "ippiFilterGauss_8u_C1Rx"
    ippiFilterGauss_8u_C1Rx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> CInt -> IO CInt
ippiFilterGauss_8u_C1R pSrc srcStep pDst dstStep roiSize mask = do
    proiSize <- new roiSize
    r <- ippiFilterGauss_8u_C1Rx pSrc srcStep pDst dstStep proiSize mask
    free proiSize
    return r

foreign import ccall "ippiFilterGauss_32f_C1Rx"
    ippiFilterGauss_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> CInt -> IO CInt
ippiFilterGauss_32f_C1R pSrc srcStep pDst dstStep roiSize mask = do
    proiSize <- new roiSize
    r <- ippiFilterGauss_32f_C1Rx pSrc srcStep pDst dstStep proiSize mask
    free proiSize
    return r

foreign import ccall "ippiFilterHipass_8u_C1Rx"
    ippiFilterHipass_8u_C1Rx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> CInt -> IO CInt
ippiFilterHipass_8u_C1R pSrc srcStep pDst dstStep roiSize mask = do
    proiSize <- new roiSize
    r <- ippiFilterHipass_8u_C1Rx pSrc srcStep pDst dstStep proiSize mask
    free proiSize
    return r

foreign import ccall "ippiFilter_8u_C1Rx"
    ippiFilter_8u_C1Rx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> Ptr Int32 -> Ptr IppiSize -> Ptr IppiPoint -> Int -> IO CInt
ippiFilter_8u_C1R pSrc srcStep pDst dstStep dstRoiSize pKernel kernelSize anchor divisor = do
    pdstRoiSize <- new dstRoiSize
    pkernelSize <- new kernelSize
    panchor <- new anchor
    r <- ippiFilter_8u_C1Rx pSrc srcStep pDst dstStep pdstRoiSize pKernel pkernelSize panchor divisor
    free pdstRoiSize
    free pkernelSize
    free panchor
    return r

foreign import ccall "ippiFilter_32f_C1Rx"
    ippiFilter_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> Ptr Float -> Ptr IppiSize -> Ptr IppiPoint -> IO CInt
ippiFilter_32f_C1R pSrc srcStep pDst dstStep dstRoiSize pKernel kernelSize anchor = do
    pdstRoiSize <- new dstRoiSize
    pkernelSize <- new kernelSize
    panchor <- new anchor
    r <- ippiFilter_32f_C1Rx pSrc srcStep pDst dstStep pdstRoiSize pKernel pkernelSize panchor
    free pdstRoiSize
    free pkernelSize
    free panchor
    return r

foreign import ccall "ippiFilterColumn_8u_C1Rx"
    ippiFilterColumn_8u_C1Rx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> Ptr Int32 -> Int -> Int -> Int -> IO CInt
ippiFilterColumn_8u_C1R pSrc srcStep pDst dstStep dstRoiSize pKernel kernelSize yAnchor divisor = do
    pdstRoiSize <- new dstRoiSize
    r <- ippiFilterColumn_8u_C1Rx pSrc srcStep pDst dstStep pdstRoiSize pKernel kernelSize yAnchor divisor
    free pdstRoiSize
    return r

foreign import ccall "ippiFilterColumn_32f_C1Rx"
    ippiFilterColumn_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> Ptr Float -> Int -> Int -> IO CInt
ippiFilterColumn_32f_C1R pSrc srcStep pDst dstStep dstRoiSize pKernel kernelSize yAnchor = do
    pdstRoiSize <- new dstRoiSize
    r <- ippiFilterColumn_32f_C1Rx pSrc srcStep pDst dstStep pdstRoiSize pKernel kernelSize yAnchor
    free pdstRoiSize
    return r

foreign import ccall "ippiFilterRow_8u_C1Rx"
    ippiFilterRow_8u_C1Rx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> Ptr Int32 -> Int -> Int -> Int -> IO CInt
ippiFilterRow_8u_C1R pSrc srcStep pDst dstStep dstRoiSize pKernel kernelSize xAnchor divisor = do
    pdstRoiSize <- new dstRoiSize
    r <- ippiFilterRow_8u_C1Rx pSrc srcStep pDst dstStep pdstRoiSize pKernel kernelSize xAnchor divisor
    free pdstRoiSize
    return r

foreign import ccall "ippiFilterRow_32f_C1Rx"
    ippiFilterRow_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> Ptr Float -> Int -> Int -> IO CInt
ippiFilterRow_32f_C1R pSrc srcStep pDst dstStep dstRoiSize pKernel kernelSize xAnchor = do
    pdstRoiSize <- new dstRoiSize
    r <- ippiFilterRow_32f_C1Rx pSrc srcStep pDst dstStep pdstRoiSize pKernel kernelSize xAnchor
    free pdstRoiSize
    return r

foreign import ccall "ippiCrossCorrValid_NormLevel_32f_C1Rx"
    ippiCrossCorrValid_NormLevel_32f_C1Rx :: Ptr Float -> Int -> Ptr IppiSize -> Ptr Float -> Int -> Ptr IppiSize -> Ptr Float -> Int -> IO CInt
ippiCrossCorrValid_NormLevel_32f_C1R pSrc srcStep srcRoiSize pTpl tplStep tplRoiSize pDst dstStep = do
    psrcRoiSize <- new srcRoiSize
    ptplRoiSize <- new tplRoiSize
    r <- ippiCrossCorrValid_NormLevel_32f_C1Rx pSrc srcStep psrcRoiSize pTpl tplStep ptplRoiSize pDst dstStep
    free psrcRoiSize
    free ptplRoiSize
    return r

foreign import ccall "ippiCrossCorrValid_NormLevel_8u32f_C1Rx"
    ippiCrossCorrValid_NormLevel_8u32f_C1Rx :: Ptr Word8 -> Int -> Ptr IppiSize -> Ptr Word8 -> Int -> Ptr IppiSize -> Ptr Float -> Int -> IO CInt
ippiCrossCorrValid_NormLevel_8u32f_C1R pSrc srcStep srcRoiSize pTpl tplStep tplRoiSize pDst dstStep = do
    psrcRoiSize <- new srcRoiSize
    ptplRoiSize <- new tplRoiSize
    r <- ippiCrossCorrValid_NormLevel_8u32f_C1Rx pSrc srcStep psrcRoiSize pTpl tplStep ptplRoiSize pDst dstStep
    free psrcRoiSize
    free ptplRoiSize
    return r

foreign import ccall "ippiCrossCorrValid_NormLevel_8u32f_C3Rx"
    ippiCrossCorrValid_NormLevel_8u32f_C3Rx :: Ptr Word8 -> Int -> Ptr IppiSize -> Ptr Word8 -> Int -> Ptr IppiSize -> Ptr Float -> Int -> IO CInt
ippiCrossCorrValid_NormLevel_8u32f_C3R pSrc srcStep srcRoiSize pTpl tplStep tplRoiSize pDst dstStep = do
    psrcRoiSize <- new srcRoiSize
    ptplRoiSize <- new tplRoiSize
    r <- ippiCrossCorrValid_NormLevel_8u32f_C3Rx pSrc srcStep psrcRoiSize pTpl tplStep ptplRoiSize pDst dstStep
    free psrcRoiSize
    free ptplRoiSize
    return r

foreign import ccall "ippiSqrDistanceValid_Norm_32f_C1Rx"
    ippiSqrDistanceValid_Norm_32f_C1Rx :: Ptr Float -> Int -> Ptr IppiSize -> Ptr Float -> Int -> Ptr IppiSize -> Ptr Float -> Int -> IO CInt
ippiSqrDistanceValid_Norm_32f_C1R pSrc srcStep srcRoiSize pTpl tplStep tplRoiSize pDst dstStep = do
    psrcRoiSize <- new srcRoiSize
    ptplRoiSize <- new tplRoiSize
    r <- ippiSqrDistanceValid_Norm_32f_C1Rx pSrc srcStep psrcRoiSize pTpl tplStep ptplRoiSize pDst dstStep
    free psrcRoiSize
    free ptplRoiSize
    return r

foreign import ccall "ippiSqrDistanceValid_Norm_8u32f_C1Rx"
    ippiSqrDistanceValid_Norm_8u32f_C1Rx :: Ptr Word8 -> Int -> Ptr IppiSize -> Ptr Word8 -> Int -> Ptr IppiSize -> Ptr Float -> Int -> IO CInt
ippiSqrDistanceValid_Norm_8u32f_C1R pSrc srcStep srcRoiSize pTpl tplStep tplRoiSize pDst dstStep = do
    psrcRoiSize <- new srcRoiSize
    ptplRoiSize <- new tplRoiSize
    r <- ippiSqrDistanceValid_Norm_8u32f_C1Rx pSrc srcStep psrcRoiSize pTpl tplStep ptplRoiSize pDst dstStep
    free psrcRoiSize
    free ptplRoiSize
    return r

foreign import ccall "ippiSqrDistanceValid_Norm_8u32f_C3Rx"
    ippiSqrDistanceValid_Norm_8u32f_C3Rx :: Ptr Word8 -> Int -> Ptr IppiSize -> Ptr Word8 -> Int -> Ptr IppiSize -> Ptr Float -> Int -> IO CInt
ippiSqrDistanceValid_Norm_8u32f_C3R pSrc srcStep srcRoiSize pTpl tplStep tplRoiSize pDst dstStep = do
    psrcRoiSize <- new srcRoiSize
    ptplRoiSize <- new tplRoiSize
    r <- ippiSqrDistanceValid_Norm_8u32f_C3Rx pSrc srcStep psrcRoiSize pTpl tplStep ptplRoiSize pDst dstStep
    free psrcRoiSize
    free ptplRoiSize
    return r

foreign import ccall "ippiThreshold_Val_8u_C1Rx"
    ippiThreshold_Val_8u_C1Rx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> Word8 -> Word8 -> CInt -> IO CInt
ippiThreshold_Val_8u_C1R pSrc srcStep pDst dstStep roiSize threshold value ippCmpOp = do
    proiSize <- new roiSize
    r <- ippiThreshold_Val_8u_C1Rx pSrc srcStep pDst dstStep proiSize threshold value ippCmpOp
    free proiSize
    return r

foreign import ccall "ippiThreshold_Val_32f_C1Rx"
    ippiThreshold_Val_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> Float -> Float -> CInt -> IO CInt
ippiThreshold_Val_32f_C1R pSrc srcStep pDst dstStep roiSize threshold value ippCmpOp = do
    proiSize <- new roiSize
    r <- ippiThreshold_Val_32f_C1Rx pSrc srcStep pDst dstStep proiSize threshold value ippCmpOp
    free proiSize
    return r

foreign import ccall "ippiComputeThreshold_Otsu_8u_C1Rx"
    ippiComputeThreshold_Otsu_8u_C1Rx :: Ptr Word8 -> Int -> Ptr IppiSize -> Ptr Word8 -> IO CInt
ippiComputeThreshold_Otsu_8u_C1R pSrc srcStep roiSize pThreshold = do
    proiSize <- new roiSize
    r <- ippiComputeThreshold_Otsu_8u_C1Rx pSrc srcStep proiSize pThreshold
    free proiSize
    return r

foreign import ccall "ippiCopy_8u_C3C1Rx"
    ippiCopy_8u_C3C1Rx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> IO CInt
ippiCopy_8u_C3C1R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiCopy_8u_C3C1Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiCopy_8u_C1C3Rx"
    ippiCopy_8u_C1C3Rx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> IO CInt
ippiCopy_8u_C1C3R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiCopy_8u_C1C3Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiCopy_8u_C1Rx"
    ippiCopy_8u_C1Rx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> IO CInt
ippiCopy_8u_C1R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiCopy_8u_C1Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiCopy_8u_C3Rx"
    ippiCopy_8u_C3Rx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> IO CInt
ippiCopy_8u_C3R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiCopy_8u_C3Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiCopy_8u_C1MRx"
    ippiCopy_8u_C1MRx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> Ptr Word8 -> Int -> IO CInt
ippiCopy_8u_C1MR pSrc srcStep pDst dstStep roiSize pMask maskStep = do
    proiSize <- new roiSize
    r <- ippiCopy_8u_C1MRx pSrc srcStep pDst dstStep proiSize pMask maskStep
    free proiSize
    return r

foreign import ccall "ippiCopy_32f_C1Rx"
    ippiCopy_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> IO CInt
ippiCopy_32f_C1R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiCopy_32f_C1Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiCopy_32f_C1MRx"
    ippiCopy_32f_C1MRx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> Ptr Word8 -> Int -> IO CInt
ippiCopy_32f_C1MR pSrc srcStep pDst dstStep roiSize pMask maskStep = do
    proiSize <- new roiSize
    r <- ippiCopy_32f_C1MRx pSrc srcStep pDst dstStep proiSize pMask maskStep
    free proiSize
    return r

foreign import ccall "ippiSet_8u_C1Rx"
    ippiSet_8u_C1Rx :: Word8 -> Ptr Word8 -> Int -> Ptr IppiSize -> IO CInt
ippiSet_8u_C1R value pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiSet_8u_C1Rx value pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiSet_8u_C3Rx"
    ippiSet_8u_C3Rx :: Ptr Word8 -> Ptr Word8 -> Int -> Ptr IppiSize -> IO CInt
ippiSet_8u_C3R value pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiSet_8u_C3Rx value pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiSet_32f_C1Rx"
    ippiSet_32f_C1Rx :: Float -> Ptr Float -> Int -> Ptr IppiSize -> IO CInt
ippiSet_32f_C1R value pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiSet_32f_C1Rx value pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiImageJaehne_32f_C1Rx"
    ippiImageJaehne_32f_C1Rx :: Ptr Float -> Int -> Ptr IppiSize -> IO CInt
ippiImageJaehne_32f_C1R pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiImageJaehne_32f_C1Rx pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiScale_8u32f_C1Rx"
    ippiScale_8u32f_C1Rx :: Ptr Word8 -> Int -> Ptr Float -> Int -> Ptr IppiSize -> Float -> Float -> IO CInt
ippiScale_8u32f_C1R pSrc srcStep pDst dstStep roiSize vMin vMax = do
    proiSize <- new roiSize
    r <- ippiScale_8u32f_C1Rx pSrc srcStep pDst dstStep proiSize vMin vMax
    free proiSize
    return r

foreign import ccall "ippiScale_32f8u_C1Rx"
    ippiScale_32f8u_C1Rx :: Ptr Float -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> Float -> Float -> IO CInt
ippiScale_32f8u_C1R pSrc srcStep pDst dstStep roiSize vMin vMax = do
    proiSize <- new roiSize
    r <- ippiScale_32f8u_C1Rx pSrc srcStep pDst dstStep proiSize vMin vMax
    free proiSize
    return r

foreign import ccall "ippiMaxIndx_8u_C1Rx"
    ippiMaxIndx_8u_C1Rx :: Ptr Word8 -> Int -> Ptr IppiSize -> Ptr Word8 -> Ptr CInt -> Ptr CInt -> IO CInt
ippiMaxIndx_8u_C1R pSrc srcStep roiSize pMax pIndexX pIndexY = do
    proiSize <- new roiSize
    r <- ippiMaxIndx_8u_C1Rx pSrc srcStep proiSize pMax pIndexX pIndexY
    free proiSize
    return r

foreign import ccall "ippiMaxIndx_32f_C1Rx"
    ippiMaxIndx_32f_C1Rx :: Ptr Float -> Int -> Ptr IppiSize -> Ptr Float -> Ptr CInt -> Ptr CInt -> IO CInt
ippiMaxIndx_32f_C1R pSrc srcStep roiSize pMax pIndexX pIndexY = do
    proiSize <- new roiSize
    r <- ippiMaxIndx_32f_C1Rx pSrc srcStep proiSize pMax pIndexX pIndexY
    free proiSize
    return r

foreign import ccall "ippiMinMax_32f_C1Rx"
    ippiMinMax_32f_C1Rx :: Ptr Float -> Int -> Ptr IppiSize -> Ptr Float -> Ptr Float -> IO CInt
ippiMinMax_32f_C1R pSrc srcStep roiSize pMin pMax = do
    proiSize <- new roiSize
    r <- ippiMinMax_32f_C1Rx pSrc srcStep proiSize pMin pMax
    free proiSize
    return r

foreign import ccall "ippiMaxEvery_8u_C1IRx"
    ippiMaxEvery_8u_C1IRx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> IO CInt
ippiMaxEvery_8u_C1IR pSrc srcStep pSrcDst srcdstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiMaxEvery_8u_C1IRx pSrc srcStep pSrcDst srcdstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiMinEvery_8u_C1IRx"
    ippiMinEvery_8u_C1IRx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> IO CInt
ippiMinEvery_8u_C1IR pSrc srcStep pSrcDst srcdstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiMinEvery_8u_C1IRx pSrc srcStep pSrcDst srcdstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiMaxEvery_32f_C1IRx"
    ippiMaxEvery_32f_C1IRx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> IO CInt
ippiMaxEvery_32f_C1IR pSrc srcStep pSrcDst srcdstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiMaxEvery_32f_C1IRx pSrc srcStep pSrcDst srcdstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiMinEvery_32f_C1IRx"
    ippiMinEvery_32f_C1IRx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> IO CInt
ippiMinEvery_32f_C1IR pSrc srcStep pSrcDst srcdstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiMinEvery_32f_C1IRx pSrc srcStep pSrcDst srcdstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiAnd_8u_C1Rx"
    ippiAnd_8u_C1Rx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> IO CInt
ippiAnd_8u_C1R pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiAnd_8u_C1Rx pSrc1 src1Step pSrc2 src2Step pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiOr_8u_C1Rx"
    ippiOr_8u_C1Rx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> IO CInt
ippiOr_8u_C1R pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiOr_8u_C1Rx pSrc1 src1Step pSrc2 src2Step pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiXor_8u_C1Rx"
    ippiXor_8u_C1Rx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> IO CInt
ippiXor_8u_C1R pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiXor_8u_C1Rx pSrc1 src1Step pSrc2 src2Step pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiNot_8u_C1Rx"
    ippiNot_8u_C1Rx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> IO CInt
ippiNot_8u_C1R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiNot_8u_C1Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiCompare_8u_C1Rx"
    ippiCompare_8u_C1Rx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> CInt -> IO CInt
ippiCompare_8u_C1R pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize ippCmpOp = do
    proiSize <- new roiSize
    r <- ippiCompare_8u_C1Rx pSrc1 src1Step pSrc2 src2Step pDst dstStep proiSize ippCmpOp
    free proiSize
    return r

foreign import ccall "ippiCompareC_8u_C1Rx"
    ippiCompareC_8u_C1Rx :: Ptr Word8 -> Int -> Word8 -> Ptr Word8 -> Int -> Ptr IppiSize -> CInt -> IO CInt
ippiCompareC_8u_C1R pSrc srcStep value pDst dstStep roiSize ippCmpOp = do
    proiSize <- new roiSize
    r <- ippiCompareC_8u_C1Rx pSrc srcStep value pDst dstStep proiSize ippCmpOp
    free proiSize
    return r

foreign import ccall "ippiErode3x3_8u_C1Rx"
    ippiErode3x3_8u_C1Rx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> IO CInt
ippiErode3x3_8u_C1R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiErode3x3_8u_C1Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiDilate3x3_8u_C1Rx"
    ippiDilate3x3_8u_C1Rx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> IO CInt
ippiDilate3x3_8u_C1R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiDilate3x3_8u_C1Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiRGBToYCbCr422_8u_C3C2Rx"
    ippiRGBToYCbCr422_8u_C3C2Rx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> IO CInt
ippiRGBToYCbCr422_8u_C3C2R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiRGBToYCbCr422_8u_C3C2Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiYCbCr422ToRGB_8u_C2C3Rx"
    ippiYCbCr422ToRGB_8u_C2C3Rx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> IO CInt
ippiYCbCr422ToRGB_8u_C2C3R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiYCbCr422ToRGB_8u_C2C3Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiRGBToYUV420_8u_C3P3Rx"
    ippiRGBToYUV420_8u_C3P3Rx :: Ptr Word8 -> Int -> Ptr () -> Ptr CInt -> Ptr IppiSize -> IO CInt
ippiRGBToYUV420_8u_C3P3R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiRGBToYUV420_8u_C3P3Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiYUV420ToRGB_8u_P3C3Rx"
    ippiYUV420ToRGB_8u_P3C3Rx :: Ptr () -> Ptr CInt -> Ptr Word8 -> Int -> Ptr IppiSize -> IO CInt
ippiYUV420ToRGB_8u_P3C3R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiYUV420ToRGB_8u_P3C3Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiYUV420ToRGB_8u_P3Rx"
    ippiYUV420ToRGB_8u_P3Rx :: Ptr () -> Ptr CInt -> Ptr () -> Int -> Ptr IppiSize -> IO CInt
ippiYUV420ToRGB_8u_P3R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiYUV420ToRGB_8u_P3Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiRGBToGray_8u_C3C1Rx"
    ippiRGBToGray_8u_C3C1Rx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> IO CInt
ippiRGBToGray_8u_C3C1R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiRGBToGray_8u_C3C1Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiRGBToHSV_8u_C3Rx"
    ippiRGBToHSV_8u_C3Rx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> IO CInt
ippiRGBToHSV_8u_C3R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiRGBToHSV_8u_C3Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiHSVToRGB_8u_C3Rx"
    ippiHSVToRGB_8u_C3Rx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> IO CInt
ippiHSVToRGB_8u_C3R pSrc srcStep pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiHSVToRGB_8u_C3Rx pSrc srcStep pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiColorTwist32f_8u_C3Rx"
    ippiColorTwist32f_8u_C3Rx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> Ptr Float -> IO CInt
ippiColorTwist32f_8u_C3R pSrc srcStep pDst dstStep roiSize twist = do
    proiSize <- new roiSize
    r <- ippiColorTwist32f_8u_C3Rx pSrc srcStep pDst dstStep proiSize twist
    free proiSize
    return r

foreign import ccall "ippiSampleLine_8u_C1Rx"
    ippiSampleLine_8u_C1Rx :: Ptr Word8 -> Int -> Ptr IppiSize -> Ptr Word8 -> Ptr IppiPoint -> Ptr IppiPoint -> IO CInt
ippiSampleLine_8u_C1R pSrc srcStep roiSize pDst pt1 pt2 = do
    proiSize <- new roiSize
    ppt1 <- new pt1
    ppt2 <- new pt2
    r <- ippiSampleLine_8u_C1Rx pSrc srcStep proiSize pDst ppt1 ppt2
    free proiSize
    free ppt1
    free ppt2
    return r

foreign import ccall "ippiSampleLine_32f_C1Rx"
    ippiSampleLine_32f_C1Rx :: Ptr Float -> Int -> Ptr IppiSize -> Ptr Float -> Ptr IppiPoint -> Ptr IppiPoint -> IO CInt
ippiSampleLine_32f_C1R pSrc srcStep roiSize pDst pt1 pt2 = do
    proiSize <- new roiSize
    ppt1 <- new pt1
    ppt2 <- new pt2
    r <- ippiSampleLine_32f_C1Rx pSrc srcStep proiSize pDst ppt1 ppt2
    free proiSize
    free ppt1
    free ppt2
    return r

foreign import ccall "ippiAbsDiff_8u_C1Rx"
    ippiAbsDiff_8u_C1Rx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> IO CInt
ippiAbsDiff_8u_C1R pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiAbsDiff_8u_C1Rx pSrc1 src1Step pSrc2 src2Step pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiAbsDiff_32f_C1Rx"
    ippiAbsDiff_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> IO CInt
ippiAbsDiff_32f_C1R pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize = do
    proiSize <- new roiSize
    r <- ippiAbsDiff_32f_C1Rx pSrc1 src1Step pSrc2 src2Step pDst dstStep proiSize
    free proiSize
    return r

foreign import ccall "ippiIntegral_8u32f_C1Rx"
    ippiIntegral_8u32f_C1Rx :: Ptr Word8 -> Int -> Ptr Float -> Int -> Ptr IppiSize -> Float -> IO CInt
ippiIntegral_8u32f_C1R pSrc srcStep pDst dstStep roiSize val = do
    proiSize <- new roiSize
    r <- ippiIntegral_8u32f_C1Rx pSrc srcStep pDst dstStep proiSize val
    free proiSize
    return r

foreign import ccall "ippiSqrIntegral_8u32f64f_C1Rx"
    ippiSqrIntegral_8u32f64f_C1Rx :: Ptr Word8 -> Int -> Ptr Float -> Int -> Ptr Double -> Int -> Ptr IppiSize -> Float -> Double -> IO CInt
ippiSqrIntegral_8u32f64f_C1R pSrc srcStep pDst dstStep pSqr sqrStep roiSize val valSqr = do
    proiSize <- new roiSize
    r <- ippiSqrIntegral_8u32f64f_C1Rx pSrc srcStep pDst dstStep pSqr sqrStep proiSize val valSqr
    free proiSize
    return r

foreign import ccall "ippiCannyGetSizex"
    ippiCannyGetSizex :: Ptr IppiSize -> Ptr CInt -> IO CInt
ippiCannyGetSize roiSize bufferSize = do
    proiSize <- new roiSize
    r <- ippiCannyGetSizex proiSize bufferSize
    free proiSize
    return r

foreign import ccall "ippiCanny_32f8u_C1Rx"
    ippiCanny_32f8u_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> Float -> Float -> Ptr Word8 -> IO CInt
ippiCanny_32f8u_C1R pSrcDx srcDxStep pSrcDy srcDyStep pDstEdges dstEdgeStep roiSize lowThresh highThresh pBuffer = do
    proiSize <- new roiSize
    r <- ippiCanny_32f8u_C1Rx pSrcDx srcDxStep pSrcDy srcDyStep pDstEdges dstEdgeStep proiSize lowThresh highThresh pBuffer
    free proiSize
    return r

foreign import ccall "ippiDistanceTransform_3x3_8u32f_C1Rx"
    ippiDistanceTransform_3x3_8u32f_C1Rx :: Ptr Word8 -> Int -> Ptr Float -> Int -> Ptr IppiSize -> Ptr Float -> IO CInt
ippiDistanceTransform_3x3_8u32f_C1R pSrc srcStep pDst dstStep roiSize pMetrics = do
    proiSize <- new roiSize
    r <- ippiDistanceTransform_3x3_8u32f_C1Rx pSrc srcStep pDst dstStep proiSize pMetrics
    free proiSize
    return r

foreign import ccall "ippiDistanceTransform_5x5_8u32f_C1Rx"
    ippiDistanceTransform_5x5_8u32f_C1Rx :: Ptr Word8 -> Int -> Ptr Float -> Int -> Ptr IppiSize -> Ptr Float -> IO CInt
ippiDistanceTransform_5x5_8u32f_C1R pSrc srcStep pDst dstStep roiSize pMetrics = do
    proiSize <- new roiSize
    r <- ippiDistanceTransform_5x5_8u32f_C1Rx pSrc srcStep pDst dstStep proiSize pMetrics
    free proiSize
    return r

foreign import ccall "ippiFastMarchingGetBufferSize_8u32f_C1Rx"
    ippiFastMarchingGetBufferSize_8u32f_C1Rx :: Ptr IppiSize -> Ptr CInt -> IO CInt
ippiFastMarchingGetBufferSize_8u32f_C1R roiSize pBufferSize = do
    proiSize <- new roiSize
    r <- ippiFastMarchingGetBufferSize_8u32f_C1Rx proiSize pBufferSize
    free proiSize
    return r

foreign import ccall "ippiFastMarching_8u32f_C1Rx"
    ippiFastMarching_8u32f_C1Rx :: Ptr Word8 -> Int -> Ptr Float -> Int -> Ptr IppiSize -> Float -> Ptr Word8 -> IO CInt
ippiFastMarching_8u32f_C1R pSrc srcStep pDst dstStep roiSize radius pBuffer = do
    proiSize <- new roiSize
    r <- ippiFastMarching_8u32f_C1Rx pSrc srcStep pDst dstStep proiSize radius pBuffer
    free proiSize
    return r

foreign import ccall "ippiFloodFillGetSizex"
    ippiFloodFillGetSizex :: Ptr IppiSize -> Ptr CInt -> IO CInt
ippiFloodFillGetSize roiSize pBufSize = do
    proiSize <- new roiSize
    r <- ippiFloodFillGetSizex proiSize pBufSize
    free proiSize
    return r

foreign import ccall "ippiFloodFillGetSize_Gradx"
    ippiFloodFillGetSize_Gradx :: Ptr IppiSize -> Ptr CInt -> IO CInt
ippiFloodFillGetSize_Grad roiSize pBufSize = do
    proiSize <- new roiSize
    r <- ippiFloodFillGetSize_Gradx proiSize pBufSize
    free proiSize
    return r

foreign import ccall "ippiFloodFill_8Con_8u_C1IRx"
    ippiFloodFill_8Con_8u_C1IRx :: Ptr Word8 -> Int -> Ptr IppiSize -> Ptr IppiPoint -> Word8 -> Ptr IppiConnectedComp -> Ptr Word8 -> IO CInt
ippiFloodFill_8Con_8u_C1IR pImage imageStep roiSize seed newVal pRegion pBuffer = do
    proiSize <- new roiSize
    pseed <- new seed
    r <- ippiFloodFill_8Con_8u_C1IRx pImage imageStep proiSize pseed newVal pRegion pBuffer
    free proiSize
    free pseed
    return r

foreign import ccall "ippiFloodFill_Grad8Con_8u_C1IRx"
    ippiFloodFill_Grad8Con_8u_C1IRx :: Ptr Word8 -> Int -> Ptr IppiSize -> Ptr IppiPoint -> Word8 -> Word8 -> Word8 -> Ptr IppiConnectedComp -> Ptr Word8 -> IO CInt
ippiFloodFill_Grad8Con_8u_C1IR pImage imageStep roiSize seed newVal minDelta maxDelta pRegion pBuffer = do
    proiSize <- new roiSize
    pseed <- new seed
    r <- ippiFloodFill_Grad8Con_8u_C1IRx pImage imageStep proiSize pseed newVal minDelta maxDelta pRegion pBuffer
    free proiSize
    free pseed
    return r

foreign import ccall "ippiUndistortGetSizex"
    ippiUndistortGetSizex :: Ptr IppiSize -> Ptr CInt -> IO CInt
ippiUndistortGetSize roiSize pBufsize = do
    proiSize <- new roiSize
    r <- ippiUndistortGetSizex proiSize pBufsize
    free proiSize
    return r

foreign import ccall "ippiCreateMapCameraUndistort_32f_C1Rx"
    ippiCreateMapCameraUndistort_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Ptr Word8 -> IO CInt
ippiCreateMapCameraUndistort_32f_C1R pxMap xStep pyMap yStep roiSize fx fy cx cy k1 k2 p1 p2 pBuffer = do
    proiSize <- new roiSize
    r <- ippiCreateMapCameraUndistort_32f_C1Rx pxMap xStep pyMap yStep proiSize fx fy cx cy k1 k2 p1 p2 pBuffer
    free proiSize
    return r

foreign import ccall "ippiUndistortRadial_8u_C1Rx"
    ippiUndistortRadial_8u_C1Rx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> Float -> Float -> Float -> Float -> Float -> Float -> Ptr Word8 -> IO CInt
ippiUndistortRadial_8u_C1R pSrc srcStep pDst dstStep roiSize fx fy cx cy k1 k2 pBuffer = do
    proiSize <- new roiSize
    r <- ippiUndistortRadial_8u_C1Rx pSrc srcStep pDst dstStep proiSize fx fy cx cy k1 k2 pBuffer
    free proiSize
    return r

foreign import ccall "ippiUndistortRadial_32f_C1Rx"
    ippiUndistortRadial_32f_C1Rx :: Ptr Float -> Int -> Ptr Float -> Int -> Ptr IppiSize -> Float -> Float -> Float -> Float -> Float -> Float -> Ptr Word8 -> IO CInt
ippiUndistortRadial_32f_C1R pSrc srcStep pDst dstStep roiSize fx fy cx cy k1 k2 pBuffer = do
    proiSize <- new roiSize
    r <- ippiUndistortRadial_32f_C1Rx pSrc srcStep pDst dstStep proiSize fx fy cx cy k1 k2 pBuffer
    free proiSize
    return r

foreign import ccall "ippiUndistortRadial_8u_C3Rx"
    ippiUndistortRadial_8u_C3Rx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> Float -> Float -> Float -> Float -> Float -> Float -> Ptr Word8 -> IO CInt
ippiUndistortRadial_8u_C3R pSrc srcStep pDst dstStep roiSize fx fy cx cy k1 k2 pBuffer = do
    proiSize <- new roiSize
    r <- ippiUndistortRadial_8u_C3Rx pSrc srcStep pDst dstStep proiSize fx fy cx cy k1 k2 pBuffer
    free proiSize
    return r

foreign import ccall "ippiSegmentWatershedGetBufferSize_8u_C1Rx"
    ippiSegmentWatershedGetBufferSize_8u_C1Rx :: Ptr IppiSize -> Ptr CInt -> IO CInt
ippiSegmentWatershedGetBufferSize_8u_C1R roiSize pBufSize = do
    proiSize <- new roiSize
    r <- ippiSegmentWatershedGetBufferSize_8u_C1Rx proiSize pBufSize
    free proiSize
    return r

foreign import ccall "ippiSegmentWatershed_8u_C1IRx"
    ippiSegmentWatershed_8u_C1IRx :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> Ptr IppiSize -> CInt -> Int -> Ptr Word8 -> IO CInt
ippiSegmentWatershed_8u_C1IR pSrc srcStep pMarker markerStep roiSize norm flag pBuffer = do
    proiSize <- new roiSize
    r <- ippiSegmentWatershed_8u_C1IRx pSrc srcStep pMarker markerStep proiSize norm flag pBuffer
    free proiSize
    return r

