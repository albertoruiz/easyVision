{-# OPTIONS -fffi #-}

-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Ipp.Wrappers
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Raw access to some IPP functions. You can use them directly, with the help of "ImagProc.Ipp.Core", but it is probably easier to use the higher level interface supplied by "ImagProc.ImageProcessing".

-}
-----------------------------------------------------------------------------


module ImagProc.Ipp.Wrappers where

import Foreign
import Foreign.C.Types

type ROISize = Double
type IPPPoint = Double

foreign import ccall "auxIpp.h ippiImageJaehne_32f_C1R"
  ippiImageJaehne_32f_C1R :: Ptr() -> Int -> ROISize -> IO Int

foreign import ccall "auxIpp.h ippiSet_32f_C1R"
  ippiSet_32f_C1R :: Float -> Ptr() -> Int -> ROISize -> IO Int

foreign import ccall "auxIpp.h ippiSet_8u_C1R"
  ippiSet_8u_C1R :: CUChar -> Ptr() -> Int -> ROISize -> IO Int

foreign import ccall "auxIpp.h ippiSet_8u_C3R"
  ippiSet_8u_C3R :: Ptr(CUChar) -> Ptr() -> Int -> ROISize -> IO Int

foreign import ccall "auxIpp.h ippiFilterGauss_32f_C1R"
     ippiFilterGauss_32f_C1R :: Ptr() -> Int -> Ptr() -> Int -> ROISize -> Int -> IO Int

foreign import ccall "auxIpp.h ippiFilterLaplace_32f_C1R"
     ippiFilterLaplace_32f_C1R :: Ptr() -> Int -> Ptr() -> Int -> ROISize -> Int -> IO Int

foreign import ccall "auxIpp.h ippiFilterHipass_8u_C1R"
     ippiFilterHipass_8u_C1R :: Ptr() -> Int -> Ptr() -> Int -> ROISize -> Int -> IO Int

foreign import ccall "auxIpp.h ippiCopy_32f_C1R"
     ippiCopy_32f_C1R :: Ptr() -> Int -> Ptr() -> Int -> ROISize -> IO Int

foreign import ccall "auxIpp.h ippiCopy_32f_C1MR"
     ippiCopy_32f_C1MR :: Ptr() -> Int -> Ptr() -> Int -> ROISize -> Ptr() -> Int -> IO Int

foreign import ccall "auxIpp.h ippiCopy_8u_C1R"
     ippiCopy_8u_C1R :: Ptr() -> Int -> Ptr() -> Int -> ROISize -> IO Int

foreign import ccall "auxIpp.h ippiCopy_8u_C3R"
     ippiCopy_8u_C3R :: Ptr() -> Int -> Ptr() -> Int -> ROISize -> IO Int

foreign import ccall "auxIpp.h ippiScale_32f8u_C1R"
     ippiScale_32f8u_C1R :: Ptr() -> Int -> Ptr() -> Int -> ROISize -> Float -> Float -> IO Int

foreign import ccall "auxIpp.h ippiScale_8u32f_C1R"
     ippiScale_8u32f_C1R :: Ptr() -> Int -> Ptr() -> Int -> ROISize -> Float -> Float -> IO Int

foreign import ccall "auxIpp.h ippiFilterSobelVert_32f_C1R"
     ippiFilterSobelVert_32f_C1R :: Ptr() -> Int -> Ptr() -> Int -> ROISize -> IO Int

foreign import ccall "auxIpp.h ippiFilterSobelHoriz_32f_C1R"
     ippiFilterSobelHoriz_32f_C1R :: Ptr() -> Int -> Ptr() -> Int -> ROISize -> IO Int

foreign import ccall "auxIpp.h ippiAbs_32f_C1R"
     ippiAbs_32f_C1R :: Ptr() -> Int -> Ptr() -> Int -> ROISize -> IO Int

foreign import ccall "auxIpp.h ippiAdd_32f_C1R"
     ippiAdd_32f_C1R :: Ptr() -> Int -> Ptr() -> Int -> Ptr() -> Int -> ROISize -> IO Int

foreign import ccall "auxIpp.h ippiSub_32f_C1R"
     ippiSub_32f_C1R :: Ptr() -> Int -> Ptr() -> Int -> Ptr() -> Int -> ROISize -> IO Int

foreign import ccall "auxIpp.h ippiSub_8u_C1RSfs"
     ippiSub_8u_C1RSfs :: Ptr() -> Int -> Ptr() -> Int -> Ptr() -> Int -> ROISize -> Int -> IO Int

foreign import ccall "auxIpp.h ippiMul_32f_C1R"
     ippiMul_32f_C1R :: Ptr() -> Int -> Ptr() -> Int -> Ptr() -> Int -> ROISize -> IO Int

foreign import ccall "auxIpp.h ippiAbsDiff_8u_C1R"
     ippiAbsDiff_8u_C1R :: Ptr() -> Int -> Ptr() -> Int -> Ptr() -> Int -> ROISize -> IO Int

foreign import ccall "auxIpp.h ippiAbsDiff_32f_C1R"
     ippiAbsDiff_32f_C1R :: Ptr() -> Int -> Ptr() -> Int -> Ptr() -> Int -> ROISize -> IO Int

foreign import ccall "auxIpp.h ippiSum_8u_C1R"
     ippiSum_8u_C1R :: Ptr() -> Int -> ROISize -> Ptr Double -> IO Int

foreign import ccall "auxIpp.h ippiSum_32f_C1R"
     ippiSum_32f_C1R :: Ptr() -> Int -> ROISize -> Ptr Double -> IO Int

foreign import ccall "auxIpp.h ippiFilterMax_32f_C1R"
     ippiFilterMax_32f_C1R :: Ptr() -> Int -> Ptr() -> Int -> ROISize -> Double -> Double -> IO Int

foreign import ccall "auxIpp.h ippiCompare_32f_C1R"
     ippiCompare_32f_C1R :: Ptr() -> Int -> Ptr() -> Int -> Ptr() -> Int -> ROISize -> Int -> IO Int

foreign import ccall "auxIpp.h ippiThreshold_Val_32f_C1R"
     ippiThreshold_Val_32f_C1R ::  Ptr() -> Int -> Ptr() -> Int -> ROISize -> Float -> Float -> Int -> IO Int

foreign import ccall "auxIpp.h ippiThreshold_Val_8u_C1R"
     ippiThreshold_Val_8u_C1R ::  Ptr() -> Int -> Ptr() -> Int -> ROISize -> CUChar -> CUChar -> Int -> IO Int

foreign import ccall "auxIpp.h ippiSqrt_32f_C1R"
     ippiSqrt_32f_C1R :: Ptr() -> Int -> Ptr() -> Int -> ROISize -> IO Int

foreign import ccall "auxIpp.h ippiMinMax_32f_C1R"
     ippiMinMax_32f_C1R :: Ptr() -> Int -> ROISize -> Ptr Float -> Ptr Float -> IO Int

foreign import ccall "auxIpp.h ippiMaxIndx_32f_C1R"
     ippiMaxIndx_32f_C1R :: Ptr() -> Int -> ROISize -> Ptr Float -> Ptr Int -> Ptr Int -> IO Int

foreign import ccall "auxIpp.h ippiMaxIndx_8u_C1R"
     ippiMaxIndx_8u_C1R :: Ptr() -> Int -> ROISize -> Ptr CUChar -> Ptr Int -> Ptr Int -> IO Int


foreign import ccall "auxIpp.h ippiMulC_32f_C1R"
     ippiMulC_32f_C1R :: Ptr() -> Int -> Float -> Ptr() -> Int -> ROISize -> IO Int

foreign import ccall "auxIpp.h ippiRGBToGray_8u_C3C1R"
    ippiRGBToGray_8u_C3C1R :: Ptr() -> Int -> Ptr() -> Int -> ROISize -> IO Int

foreign import ccall "auxIpp.h ippiYUV420ToRGB_8u_P3C3R"
     ippiYUV420ToRGB_8u_P3C3R :: Ptr () -> Ptr Int -> Ptr() -> Int -> ROISize -> IO Int

foreign import ccall "auxIpp.h ippiYUV420ToRGB_8u_P3R"
     ippiYUV420ToRGB_8u_P3R :: Ptr () -> Ptr Int -> Ptr() -> Int -> ROISize -> IO Int

foreign import ccall "auxIpp.h ippiRGBToYUV420_8u_C3P3R"
     ippiRGBToYUV420_8u_C3P3R :: Ptr() -> Int -> Ptr () -> Ptr Int -> ROISize -> IO Int

foreign import ccall "auxIpp.h ippiIntegral_8u32f_C1R"
     ippiIntegral_8u32f_C1R :: Ptr() -> Int -> Ptr() -> Int -> ROISize -> Float -> IO Int

foreign import ccall "auxIpp.h ippiCannyGetSize"
     ippiCannyGetSize :: ROISize -> Ptr Int -> IO Int

foreign import ccall "auxIpp.h ippiCanny_32f8u_C1R"
     ippiCanny_32f8u_C1R :: Ptr() -> Int -> Ptr() -> Int
                         -> Ptr() -> Int -> ROISize -> Float -> Float -> Ptr() -> IO Int

foreign import ccall "auxIpp.h ippiFilterMedian_8u_C1R"
     ippiFilterMedian_8u_C1R :: Ptr() -> Int -> Ptr() -> Int -> ROISize -> ROISize -> IPPPoint -> IO Int

foreign import ccall "auxIpp.h ippiHistogramRange_8u_C1R"
     ippiHistogramRange_8u_C1R :: Ptr () -> Int -> ROISize -> Ptr Int -> Ptr Int -> Int -> IO Int

foreign import ccall "auxIpp.h ippiDilate3x3_8u_C1R"
     ippiDilate3x3_8u_C1R :: Ptr() -> Int -> Ptr() -> Int -> ROISize -> IO Int

foreign import ccall "auxIpp.h ippiErode3x3_8u_C1R"
     ippiErode3x3_8u_C1R :: Ptr() -> Int -> Ptr() -> Int -> ROISize -> IO Int

foreign import ccall "auxIpp.h ippiDilate_8u_C1R"
     ippiDilate_8u_C1R :: Ptr() -> Int -> Ptr() -> Int -> ROISize -> Ptr CChar -> ROISize -> IPPPoint -> IO Int

foreign import ccall "auxIpp.h ippiErode_8u_C1R"
     ippiErode_8u_C1R :: Ptr() -> Int -> Ptr() -> Int -> ROISize -> Ptr CChar -> ROISize -> IPPPoint -> IO Int

foreign import ccall "auxIpp.h ippiNot_8u_C1R"
     ippiNot_8u_C1R :: Ptr() -> Int -> Ptr() -> Int -> ROISize -> IO Int



foreign import ccall "auxIpp.h auxWarpPerspective_32f_C1R"
     warpPerspective32f :: Ptr() -> Int -> Int -> Int ->
                           Int -> Int -> Int -> Int ->
                           Ptr() -> Int ->
                           Int -> Int -> Int -> Int ->
                           Ptr Double -> Int ->
                           IO Int

foreign import ccall "auxIpp.h auxWarpPerspective_8u_C1R"
     warpPerspectiveGray :: Ptr() -> Int -> Int -> Int ->
                           Int -> Int -> Int -> Int ->
                           Ptr() -> Int ->
                           Int -> Int -> Int -> Int ->
                           Ptr Double -> Int ->
                           IO Int

foreign import ccall "auxIpp.h auxWarpPerspective_8u_C3R"
     warpPerspectiveRGB :: Ptr() -> Int -> Int -> Int ->
                           Int -> Int -> Int -> Int ->
                           Ptr() -> Int ->
                           Int -> Int -> Int -> Int ->
                           Ptr Double -> Int ->
                           IO Int


foreign import ccall "auxIpp.h ippGetStatusString" ippGetStatusString :: Int -> IO (Ptr CChar)

foreign import ccall "auxIpp.h getPoints32f"
    c_getPoints32f :: Ptr Float -> Int -> Int -> Int -> Int -> Int ->
                      Int -> Ptr Int -> Ptr Int -> IO Int

foreign import ccall "auxIpp.h auxResize_32f_C1R"
     c_resize32f :: Ptr() -> Int -> Int -> Int ->
                           Int -> Int -> Int -> Int ->
                           Ptr() -> Int ->
                           Int -> Int -> Int -> Int ->
                           Int ->
                           IO Int

foreign import ccall "auxIpp.h auxResize_8u_C1R"
     c_resize8u :: Ptr() -> Int -> Int -> Int ->
                           Int -> Int -> Int -> Int ->
                           Ptr() -> Int ->
                           Int -> Int -> Int -> Int ->
                           Int ->
                           IO Int

foreign import ccall "auxIpp.h auxDCTFwd_32f_C1R"
     auxDCTFwd_32f_C1R :: Ptr Float -> Int ->
                          Int -> Int -> Int -> Int ->
                          Ptr Float -> Int ->
                          Int -> Int -> Int -> Int ->
                          IO Int

foreign import ccall "auxIpp.h auxDCTInv_32f_C1R"
     auxDCTInv_32f_C1R :: Ptr Float -> Int ->
                          Int -> Int -> Int -> Int ->
                          Ptr Float -> Int ->
                          Int -> Int -> Int -> Int ->
                          IO Int

foreign import ccall "auxIpp.h ippiFFTInitAlloc_R_32f"
    ippiFFTInitAlloc_R_32f :: Ptr (Ptr()) -> Int -> Int -> Int -> Int -> IO Int

foreign import ccall "auxIpp.h ippiFFTFree_R_32f"
    ippiFFTFree_R_32f :: Ptr () -> IO Int

foreign import ccall "auxIpp.h ippiFFTGetBufSize_R_32f"
    ippiFFTGetBufSize_R_32f :: Ptr () -> Ptr Int -> IO Int

foreign import ccall "auxIpp.h ippiFFTFwd_RToPack_32f_C1R"
    ippiFFTFwd_RToPack_32f_C1R :: Ptr () -> Int -> Ptr() -> Int -> Ptr() -> Ptr() -> IO Int

foreign import ccall "auxIpp.h ippiMagnitudePack_32f_C1R"
    ippiMagnitudePack_32f_C1R :: Ptr () -> Int -> Ptr() -> Int -> ROISize -> IO Int

foreign import ccall "auxIpp.h ippiDistanceTransform_3x3_8u32f_C1R" 
    ippiDistanceTransform_3x3_8u32f_C1R :: Ptr () -> Int -> Ptr() -> Int -> ROISize -> Ptr Float -> IO Int

foreign import ccall "auxIpp.h ippiDistanceTransform_5x5_8u32f_C1R" 
    ippiDistanceTransform_5x5_8u32f_C1R :: Ptr () -> Int -> Ptr() -> Int -> ROISize -> Ptr Float -> IO Int


foreign import ccall "auxIpp.h ippiFloodFillGetSize"
    ippiFloodFillGetSize :: ROISize -> Ptr Int -> IO Int

foreign import ccall "auxIpp.h ippiFloodFillGetSize_Grad"
    ippiFloodFillGetSize_Grad :: ROISize -> Ptr Int -> IO Int

foreign import ccall "auxIpp.h ippiFloodFill_8Con_8u_C1IR"
    ippiFloodFill_8Con_8u_C1IR :: Ptr() -> Int -> ROISize -> IPPPoint -> CUChar -> Ptr () -> Ptr () -> IO Int

foreign import ccall "auxIpp.h ippiFloodFill_Grad8Con_8u_C1IR"
    ippiFloodFill_Grad8Con_8u_C1IR :: Ptr() -> Int -> ROISize -> IPPPoint -> CUChar -> CUChar -> CUChar -> Ptr () -> Ptr () -> IO Int

foreign import ccall "auxIpp.h lbp8u"
     lbp8u :: Int -> Ptr () -> Int -> Int -> Int -> Int -> Int -> Ptr Int -> IO Int
