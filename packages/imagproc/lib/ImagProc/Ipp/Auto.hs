-- generated automatically by adapter.hs

{-# OPTIONS #-}

module ImagProc.Ipp.Auto where

import ImagProc.Ipp.AutoGen
import ImagProc.Ipp.Adapt

-------- arity 0 -------------

{- |     Sets pixels in the image buffer to a constant value
 -}
ioSet_8u_C1R value = {-# SCC "ippiSet_8u_C1R" #-} auto_0_8u_C1R (f value) "ippiSet_8u_C1R"
    where f value pDst dstStep roiSize = ippiSet_8u_C1R value pDst dstStep roiSize

{- |     Sets pixels in the image buffer to a constant value
 -}
ioSet_8u_C3R value = {-# SCC "ippiSet_8u_C3R" #-} auto_0_8u_C3R (f value) "ippiSet_8u_C3R"
    where f value pDst dstStep roiSize = ippiSet_8u_C3R value pDst dstStep roiSize

{- |     Sets pixels in the image buffer to a constant value
 -}
ioSet_32f_C1R value = {-# SCC "ippiSet_32f_C1R" #-} auto_0_32f_C1R (f value) "ippiSet_32f_C1R"
    where f value pDst dstStep roiSize = ippiSet_32f_C1R value pDst dstStep roiSize

-------- arity 1 -------------

{- |     Adds, subtracts, or multiplies pixel values of an image and a constant
              and places the scaled results in the same image.
 -}
ioMulC_32f_C1R value = {-# SCC "ippiMulC_32f_C1R" #-} auto_1_32f_C1R (f value) "ippiMulC_32f_C1R"
    where f value pSrc srcStep pDst dstStep roiSize = ippiMulC_32f_C1R pSrc srcStep value pDst dstStep roiSize

{- |     computes absolute value of each pixel of a source image and
              places results in the destination image;
              for in-place flavors - in the same source image -}
ioAbs_32f_C1R  = {-# SCC "ippiAbs_32f_C1R" #-} auto_1_32f_C1R f "ippiAbs_32f_C1R"
    where f pSrc srcStep pDst dstStep roiSize = ippiAbs_32f_C1R pSrc srcStep pDst dstStep roiSize

{- |     computes square roots of pixel values of a source image and
              places results in the destination image;
              for in-place flavors - in the same image -}
ioSqrt_32f_C1R  = {-# SCC "ippiSqrt_32f_C1R" #-} auto_1_32f_C1R f "ippiSqrt_32f_C1R"
    where f pSrc srcStep pDst dstStep roiSize = ippiSqrt_32f_C1R pSrc srcStep pDst dstStep roiSize

{- | 
    Computes magnitude of elements of an image in RCPack2D packed format. -}
ioMagnitudePack_32f_C1R  = {-# SCC "ippiMagnitudePack_32f_C1R" #-} auto_1_32f_C1R f "ippiMagnitudePack_32f_C1R"
    where f pSrc srcStep pDst dstStep dstRoiSize = ippiMagnitudePack_32f_C1R pSrc srcStep pDst dstStep dstRoiSize

{- |      Mirrors an image about a horizontal
               or vertical axis, or both
 -}
ioMirror_8u_C1R flip = {-# SCC "ippiMirror_8u_C1R" #-} auto_1_8u_C1R (f flip) "ippiMirror_8u_C1R"
    where f flip pSrc srcStep pDst dstStep roiSize = ippiMirror_8u_C1R pSrc srcStep pDst dstStep roiSize flip

{- |             Transforms the source image by remapping its pixels
                          dst[i,j] = src[xMap[i,j], yMap[i,j]] -}
ioRemap_8u_C1R srcSize srcROI pxMap xMapStep pyMap yMapStep interpolation = {-# SCC "ippiRemap_8u_C1R" #-} auto_1_8u_C1R (f srcSize srcROI pxMap xMapStep pyMap yMapStep interpolation) "ippiRemap_8u_C1R"
    where f srcSize srcROI pxMap xMapStep pyMap yMapStep interpolation pSrc srcStep pDst dstStep dstRoiSize = ippiRemap_8u_C1R pSrc srcSize srcStep srcROI pxMap xMapStep pyMap yMapStep pDst dstStep dstRoiSize interpolation

{- |             Transforms the source image by remapping its pixels
                          dst[i,j] = src[xMap[i,j], yMap[i,j]] -}
ioRemap_8u_C3R srcSize srcROI pxMap xMapStep pyMap yMapStep interpolation = {-# SCC "ippiRemap_8u_C3R" #-} auto_1_8u_C3R (f srcSize srcROI pxMap xMapStep pyMap yMapStep interpolation) "ippiRemap_8u_C3R"
    where f srcSize srcROI pxMap xMapStep pyMap yMapStep interpolation pSrc srcStep pDst dstStep dstRoiSize = ippiRemap_8u_C3R pSrc srcSize srcStep srcROI pxMap xMapStep pyMap yMapStep pDst dstStep dstRoiSize interpolation

{- |             Transforms the source image by remapping its pixels
                          dst[i,j] = src[xMap[i,j], yMap[i,j]] -}
ioRemap_32f_C1R srcSize srcROI pxMap xMapStep pyMap yMapStep interpolation = {-# SCC "ippiRemap_32f_C1R" #-} auto_1_32f_C1R (f srcSize srcROI pxMap xMapStep pyMap yMapStep interpolation) "ippiRemap_32f_C1R"
    where f srcSize srcROI pxMap xMapStep pyMap yMapStep interpolation pSrc srcStep pDst dstStep dstRoiSize = ippiRemap_32f_C1R pSrc srcSize srcStep srcROI pxMap xMapStep pyMap yMapStep pDst dstStep dstRoiSize interpolation

{- |   Performs horizontal median filtering -}
ioFilterMedian_8u_C1R maskSize anchor = {-# SCC "ippiFilterMedian_8u_C1R" #-} auto_1_8u_C1R (f maskSize anchor) "ippiFilterMedian_8u_C1R"
    where f maskSize anchor pSrc srcStep pDst dstStep dstRoiSize = ippiFilterMedian_8u_C1R pSrc srcStep pDst dstStep dstRoiSize maskSize anchor

{- |   Applies the "max" filter to an image -}
ioFilterMax_8u_C1R maskSize anchor = {-# SCC "ippiFilterMax_8u_C1R" #-} auto_1_8u_C1R (f maskSize anchor) "ippiFilterMax_8u_C1R"
    where f maskSize anchor pSrc srcStep pDst dstStep dstRoiSize = ippiFilterMax_8u_C1R pSrc srcStep pDst dstStep dstRoiSize maskSize anchor

{- |   Applies the "max" filter to an image -}
ioFilterMax_32f_C1R maskSize anchor = {-# SCC "ippiFilterMax_32f_C1R" #-} auto_1_32f_C1R (f maskSize anchor) "ippiFilterMax_32f_C1R"
    where f maskSize anchor pSrc srcStep pDst dstStep dstRoiSize = ippiFilterMax_32f_C1R pSrc srcStep pDst dstStep dstRoiSize maskSize anchor

{- |   Applies the "min" filter to an image -}
ioFilterMin_8u_C1R maskSize anchor = {-# SCC "ippiFilterMin_8u_C1R" #-} auto_1_8u_C1R (f maskSize anchor) "ippiFilterMin_8u_C1R"
    where f maskSize anchor pSrc srcStep pDst dstStep dstRoiSize = ippiFilterMin_8u_C1R pSrc srcStep pDst dstStep dstRoiSize maskSize anchor

{- |   Applies the "min" filter to an image -}
ioFilterMin_32f_C1R maskSize anchor = {-# SCC "ippiFilterMin_32f_C1R" #-} auto_1_32f_C1R (f maskSize anchor) "ippiFilterMin_32f_C1R"
    where f maskSize anchor pSrc srcStep pDst dstStep dstRoiSize = ippiFilterMin_32f_C1R pSrc srcStep pDst dstStep dstRoiSize maskSize anchor

{- |              Blurs an image using a simple box filter -}
ioFilterBox_8u_C1R maskSize anchor = {-# SCC "ippiFilterBox_8u_C1R" #-} auto_1_8u_C1R (f maskSize anchor) "ippiFilterBox_8u_C1R"
    where f maskSize anchor pSrc srcStep pDst dstStep dstRoiSize = ippiFilterBox_8u_C1R pSrc srcStep pDst dstStep dstRoiSize maskSize anchor

{- |              Blurs an image using a simple box filter -}
ioFilterBox_32f_C1R maskSize anchor = {-# SCC "ippiFilterBox_32f_C1R" #-} auto_1_32f_C1R (f maskSize anchor) "ippiFilterBox_32f_C1R"
    where f maskSize anchor pSrc srcStep pDst dstStep dstRoiSize = ippiFilterBox_32f_C1R pSrc srcStep pDst dstStep dstRoiSize maskSize anchor

{- |   Perform linear filtering of an image using one of -}
ioFilterSobelVert_32f_C1R  = {-# SCC "ippiFilterSobelVert_32f_C1R" #-} auto_1_32f_C1R f "ippiFilterSobelVert_32f_C1R"
    where f pSrc srcStep pDst dstStep roiSize = ippiFilterSobelVert_32f_C1R pSrc srcStep pDst dstStep roiSize

{- |   Perform linear filtering of an image using one of -}
ioFilterSobelHoriz_32f_C1R  = {-# SCC "ippiFilterSobelHoriz_32f_C1R" #-} auto_1_32f_C1R f "ippiFilterSobelHoriz_32f_C1R"
    where f pSrc srcStep pDst dstStep roiSize = ippiFilterSobelHoriz_32f_C1R pSrc srcStep pDst dstStep roiSize

{- |    Perform linear filtering of an image using one of -}
ioFilterLaplace_32f_C1R mask = {-# SCC "ippiFilterLaplace_32f_C1R" #-} auto_1_32f_C1R (f mask) "ippiFilterLaplace_32f_C1R"
    where f mask pSrc srcStep pDst dstStep roiSize = ippiFilterLaplace_32f_C1R pSrc srcStep pDst dstStep roiSize mask

{- |    Perform linear filtering of an image using one of -}
ioFilterGauss_8u_C1R mask = {-# SCC "ippiFilterGauss_8u_C1R" #-} auto_1_8u_C1R (f mask) "ippiFilterGauss_8u_C1R"
    where f mask pSrc srcStep pDst dstStep roiSize = ippiFilterGauss_8u_C1R pSrc srcStep pDst dstStep roiSize mask

{- |    Perform linear filtering of an image using one of -}
ioFilterGauss_32f_C1R mask = {-# SCC "ippiFilterGauss_32f_C1R" #-} auto_1_32f_C1R (f mask) "ippiFilterGauss_32f_C1R"
    where f mask pSrc srcStep pDst dstStep roiSize = ippiFilterGauss_32f_C1R pSrc srcStep pDst dstStep roiSize mask

{- |    Perform linear filtering of an image using one of -}
ioFilterHipass_8u_C1R mask = {-# SCC "ippiFilterHipass_8u_C1R" #-} auto_1_8u_C1R (f mask) "ippiFilterHipass_8u_C1R"
    where f mask pSrc srcStep pDst dstStep roiSize = ippiFilterHipass_8u_C1R pSrc srcStep pDst dstStep roiSize mask

{- |   Performs horizontal median filtering -}
ioFilter_8u_C1R pKernel kernelSize anchor divisor = {-# SCC "ippiFilter_8u_C1R" #-} auto_1_8u_C1R (f pKernel kernelSize anchor divisor) "ippiFilter_8u_C1R"
    where f pKernel kernelSize anchor divisor pSrc srcStep pDst dstStep dstRoiSize = ippiFilter_8u_C1R pSrc srcStep pDst dstStep dstRoiSize pKernel kernelSize anchor divisor

{- |   Performs horizontal median filtering -}
ioFilter_32f_C1R pKernel kernelSize anchor = {-# SCC "ippiFilter_32f_C1R" #-} auto_1_32f_C1R (f pKernel kernelSize anchor) "ippiFilter_32f_C1R"
    where f pKernel kernelSize anchor pSrc srcStep pDst dstStep dstRoiSize = ippiFilter_32f_C1R pSrc srcStep pDst dstStep dstRoiSize pKernel kernelSize anchor

{- |     Filters an image using a spatial 32s kernel consisting of a
              single column -}
ioFilterColumn_8u_C1R pKernel kernelSize yAnchor divisor = {-# SCC "ippiFilterColumn_8u_C1R" #-} auto_1_8u_C1R (f pKernel kernelSize yAnchor divisor) "ippiFilterColumn_8u_C1R"
    where f pKernel kernelSize yAnchor divisor pSrc srcStep pDst dstStep dstRoiSize = ippiFilterColumn_8u_C1R pSrc srcStep pDst dstStep dstRoiSize pKernel kernelSize yAnchor divisor

{- |     Filters an image using a spatial 32s kernel consisting of a
              single column -}
ioFilterColumn_32f_C1R pKernel kernelSize yAnchor = {-# SCC "ippiFilterColumn_32f_C1R" #-} auto_1_32f_C1R (f pKernel kernelSize yAnchor) "ippiFilterColumn_32f_C1R"
    where f pKernel kernelSize yAnchor pSrc srcStep pDst dstStep dstRoiSize = ippiFilterColumn_32f_C1R pSrc srcStep pDst dstStep dstRoiSize pKernel kernelSize yAnchor

{- |    Filters an image using a spatial 32s kernel consisting of a
             single row -}
ioFilterRow_8u_C1R pKernel kernelSize xAnchor divisor = {-# SCC "ippiFilterRow_8u_C1R" #-} auto_1_8u_C1R (f pKernel kernelSize xAnchor divisor) "ippiFilterRow_8u_C1R"
    where f pKernel kernelSize xAnchor divisor pSrc srcStep pDst dstStep dstRoiSize = ippiFilterRow_8u_C1R pSrc srcStep pDst dstStep dstRoiSize pKernel kernelSize xAnchor divisor

{- |    Filters an image using a spatial 32s kernel consisting of a
             single row -}
ioFilterRow_32f_C1R pKernel kernelSize xAnchor = {-# SCC "ippiFilterRow_32f_C1R" #-} auto_1_32f_C1R (f pKernel kernelSize xAnchor) "ippiFilterRow_32f_C1R"
    where f pKernel kernelSize xAnchor pSrc srcStep pDst dstStep dstRoiSize = ippiFilterRow_32f_C1R pSrc srcStep pDst dstStep dstRoiSize pKernel kernelSize xAnchor

{- |     Performs thresholding of an image using the specified level
 -}
ioThreshold_Val_8u_C1R threshold value ippCmpOp = {-# SCC "ippiThreshold_Val_8u_C1R" #-} auto_1_8u_C1R (f threshold value ippCmpOp) "ippiThreshold_Val_8u_C1R"
    where f threshold value ippCmpOp pSrc srcStep pDst dstStep roiSize = ippiThreshold_Val_8u_C1R pSrc srcStep pDst dstStep roiSize threshold value ippCmpOp

{- |     Performs thresholding of an image using the specified level
 -}
ioThreshold_Val_32f_C1R threshold value ippCmpOp = {-# SCC "ippiThreshold_Val_32f_C1R" #-} auto_1_32f_C1R (f threshold value ippCmpOp) "ippiThreshold_Val_32f_C1R"
    where f threshold value ippCmpOp pSrc srcStep pDst dstStep roiSize = ippiThreshold_Val_32f_C1R pSrc srcStep pDst dstStep roiSize threshold value ippCmpOp

{- |   copy pixel values from the source image to the destination  image

 -}
ioCopy_8u_C3C1R  = {-# SCC "ippiCopy_8u_C3C1R" #-} auto_1_8u_C3C1R f "ippiCopy_8u_C3C1R"
    where f pSrc srcStep pDst dstStep roiSize = ippiCopy_8u_C3C1R pSrc srcStep pDst dstStep roiSize

{- |   copy pixel values from the source image to the destination  image

 -}
ioCopy_8u_C1C3R  = {-# SCC "ippiCopy_8u_C1C3R" #-} auto_1_8u_C1C3R f "ippiCopy_8u_C1C3R"
    where f pSrc srcStep pDst dstStep roiSize = ippiCopy_8u_C1C3R pSrc srcStep pDst dstStep roiSize

{- |   copy pixel values from the source image to the destination  image

 -}
ioCopy_8u_C1R  = {-# SCC "ippiCopy_8u_C1R" #-} auto_1_8u_C1R f "ippiCopy_8u_C1R"
    where f pSrc srcStep pDst dstStep roiSize = ippiCopy_8u_C1R pSrc srcStep pDst dstStep roiSize

{- |   copy pixel values from the source image to the destination  image

 -}
ioCopy_8u_C3R  = {-# SCC "ippiCopy_8u_C3R" #-} auto_1_8u_C3R f "ippiCopy_8u_C3R"
    where f pSrc srcStep pDst dstStep roiSize = ippiCopy_8u_C3R pSrc srcStep pDst dstStep roiSize

{- |   copy pixel values from the source image to the destination  image

 -}
ioCopy_8u_C1MR pMask maskStep = {-# SCC "ippiCopy_8u_C1MR" #-} auto_1_8u_C1MR (f pMask maskStep) "ippiCopy_8u_C1MR"
    where f pMask maskStep pSrc srcStep pDst dstStep roiSize = ippiCopy_8u_C1MR pSrc srcStep pDst dstStep roiSize pMask maskStep

{- |   copy pixel values from the source image to the destination  image

 -}
ioCopy_32f_C1R  = {-# SCC "ippiCopy_32f_C1R" #-} auto_1_32f_C1R f "ippiCopy_32f_C1R"
    where f pSrc srcStep pDst dstStep roiSize = ippiCopy_32f_C1R pSrc srcStep pDst dstStep roiSize

{- |   copy pixel values from the source image to the destination  image

 -}
ioCopy_32f_C1MR pMask maskStep = {-# SCC "ippiCopy_32f_C1MR" #-} auto_1_32f_C1MR (f pMask maskStep) "ippiCopy_32f_C1MR"
    where f pMask maskStep pSrc srcStep pDst dstStep roiSize = ippiCopy_32f_C1MR pSrc srcStep pDst dstStep roiSize pMask maskStep

{- |    Scales pixel values of an image and converts them to another bit depth
              dst = a + b * src;
              a = type_min_dst - b * type_min_src;
              b = (type_max_dst - type_min_dst) / (type_max_src - type_min_src).
 -}
ioScale_8u32f_C1R vMin vMax = {-# SCC "ippiScale_8u32f_C1R" #-} auto_1_8u32f_C1R (f vMin vMax) "ippiScale_8u32f_C1R"
    where f vMin vMax pSrc srcStep pDst dstStep roiSize = ippiScale_8u32f_C1R pSrc srcStep pDst dstStep roiSize vMin vMax

{- |    Scales pixel values of an image and converts them to another bit depth
              dst = a + b * src;
              a = type_min_dst - b * type_min_src;
              b = (type_max_dst - type_min_dst) / (type_max_src - type_min_src).
 -}
ioScale_32f8u_C1R vMin vMax = {-# SCC "ippiScale_32f8u_C1R" #-} auto_1_32f8u_C1R (f vMin vMax) "ippiScale_32f8u_C1R"
    where f vMin vMax pSrc srcStep pDst dstStep roiSize = ippiScale_32f8u_C1R pSrc srcStep pDst dstStep roiSize vMin vMax

{- |         Performs corresponding bitwise logical operation between pixels of two image
                  (AndC/OrC/XorC  - between pixel of the source image and a constant)
 -}
ioNot_8u_C1R  = {-# SCC "ippiNot_8u_C1R" #-} auto_1_8u_C1R f "ippiNot_8u_C1R"
    where f pSrc srcStep pDst dstStep roiSize = ippiNot_8u_C1R pSrc srcStep pDst dstStep roiSize

{- |   Compares pixel values of two images, or pixel values of an image to a constant -}
ioCompareC_8u_C1R value ippCmpOp = {-# SCC "ippiCompareC_8u_C1R" #-} auto_1_8u_C1R (f value ippCmpOp) "ippiCompareC_8u_C1R"
    where f value ippCmpOp pSrc srcStep pDst dstStep roiSize = ippiCompareC_8u_C1R pSrc srcStep value pDst dstStep roiSize ippCmpOp

{- |    Performs not in-place erosion/dilation using a 3x3 mask
 -}
ioErode3x3_8u_C1R  = {-# SCC "ippiErode3x3_8u_C1R" #-} auto_1_8u_C1R f "ippiErode3x3_8u_C1R"
    where f pSrc srcStep pDst dstStep roiSize = ippiErode3x3_8u_C1R pSrc srcStep pDst dstStep roiSize

{- |    Performs not in-place erosion/dilation using a 3x3 mask
 -}
ioDilate3x3_8u_C1R  = {-# SCC "ippiDilate3x3_8u_C1R" #-} auto_1_8u_C1R f "ippiDilate3x3_8u_C1R"
    where f pSrc srcStep pDst dstStep roiSize = ippiDilate3x3_8u_C1R pSrc srcStep pDst dstStep roiSize

{- |  -}
ioYUV420ToRGB_8u_P3C3R  = {-# SCC "ippiYUV420ToRGB_8u_P3C3R" #-} auto_1_8u_P3C3R f "ippiYUV420ToRGB_8u_P3C3R"
    where f pSrc srcStep pDst dstStep roiSize = ippiYUV420ToRGB_8u_P3C3R pSrc srcStep pDst dstStep roiSize

{- |  -}
ioYUV420ToRGB_8u_P3R  = {-# SCC "ippiYUV420ToRGB_8u_P3R" #-} auto_1_8u_P3R f "ippiYUV420ToRGB_8u_P3R"
    where f pSrc srcStep pDst dstStep roiSize = ippiYUV420ToRGB_8u_P3R pSrc srcStep pDst dstStep roiSize

{- |     Converts an RGB image to gray scale (fixed coefficients) -}
ioRGBToGray_8u_C3C1R  = {-# SCC "ippiRGBToGray_8u_C3C1R" #-} auto_1_8u_C3C1R f "ippiRGBToGray_8u_C3C1R"
    where f pSrc srcStep pDst dstStep roiSize = ippiRGBToGray_8u_C3C1R pSrc srcStep pDst dstStep roiSize

{- |     Converts an RGB image to the HSV color model and vice versa -}
ioRGBToHSV_8u_C3R  = {-# SCC "ippiRGBToHSV_8u_C3R" #-} auto_1_8u_C3R f "ippiRGBToHSV_8u_C3R"
    where f pSrc srcStep pDst dstStep roiSize = ippiRGBToHSV_8u_C3R pSrc srcStep pDst dstStep roiSize

{- |     Converts an RGB image to the HSV color model and vice versa -}
ioHSVToRGB_8u_C3R  = {-# SCC "ippiHSVToRGB_8u_C3R" #-} auto_1_8u_C3R f "ippiHSVToRGB_8u_C3R"
    where f pSrc srcStep pDst dstStep roiSize = ippiHSVToRGB_8u_C3R pSrc srcStep pDst dstStep roiSize

{- |    calculates pixel sum on subimage
 -}
ioIntegral_8u32f_C1R val = {-# SCC "ippiIntegral_8u32f_C1R" #-} auto_1_8u32f_C1R (f val) "ippiIntegral_8u32f_C1R"
    where f val pSrc srcStep pDst dstStep roiSize = ippiIntegral_8u32f_C1R pSrc srcStep pDst dstStep roiSize val

{- |    calculates pixel sum on subimage
 -}
ioSqrIntegral_8u32f64f_C1R val valSqr = {-# SCC "ippiSqrIntegral_8u32f64f_C1R" #-} auto_1_8u32f64f_C1R (f val valSqr) "ippiSqrIntegral_8u32f64f_C1R"
    where f val valSqr pSrc srcStep pDst dstStep pSqr sqrStep roiSize = ippiSqrIntegral_8u32f64f_C1R pSrc srcStep pDst dstStep pSqr sqrStep roiSize val valSqr

{- |  For every non-zero pixel in the source image, the functions calculate
           distance between that pixel and nearest zero pixel.
 -}
ioDistanceTransform_3x3_8u32f_C1R pMetrics = {-# SCC "ippiDistanceTransform_3x3_8u32f_C1R" #-} auto_1_8u32f_C1R (f pMetrics) "ippiDistanceTransform_3x3_8u32f_C1R"
    where f pMetrics pSrc srcStep pDst dstStep roiSize = ippiDistanceTransform_3x3_8u32f_C1R pSrc srcStep pDst dstStep roiSize pMetrics

{- |  For every non-zero pixel in the source image, the functions calculate
           distance between that pixel and nearest zero pixel.
 -}
ioDistanceTransform_5x5_8u32f_C1R pMetrics = {-# SCC "ippiDistanceTransform_5x5_8u32f_C1R" #-} auto_1_8u32f_C1R (f pMetrics) "ippiDistanceTransform_5x5_8u32f_C1R"
    where f pMetrics pSrc srcStep pDst dstStep roiSize = ippiDistanceTransform_5x5_8u32f_C1R pSrc srcStep pDst dstStep roiSize pMetrics

{- |  correct camera distortion
 -}
ioUndistortRadial_8u_C1R fx fy cx cy k1 k2 pBuffer = {-# SCC "ippiUndistortRadial_8u_C1R" #-} auto_1_8u_C1R (f fx fy cx cy k1 k2 pBuffer) "ippiUndistortRadial_8u_C1R"
    where f fx fy cx cy k1 k2 pBuffer pSrc srcStep pDst dstStep roiSize = ippiUndistortRadial_8u_C1R pSrc srcStep pDst dstStep roiSize fx fy cx cy k1 k2 pBuffer

{- |  correct camera distortion
 -}
ioUndistortRadial_32f_C1R fx fy cx cy k1 k2 pBuffer = {-# SCC "ippiUndistortRadial_32f_C1R" #-} auto_1_32f_C1R (f fx fy cx cy k1 k2 pBuffer) "ippiUndistortRadial_32f_C1R"
    where f fx fy cx cy k1 k2 pBuffer pSrc srcStep pDst dstStep roiSize = ippiUndistortRadial_32f_C1R pSrc srcStep pDst dstStep roiSize fx fy cx cy k1 k2 pBuffer

{- |  correct camera distortion
 -}
ioUndistortRadial_8u_C3R fx fy cx cy k1 k2 pBuffer = {-# SCC "ippiUndistortRadial_8u_C3R" #-} auto_1_8u_C3R (f fx fy cx cy k1 k2 pBuffer) "ippiUndistortRadial_8u_C3R"
    where f fx fy cx cy k1 k2 pBuffer pSrc srcStep pDst dstStep roiSize = ippiUndistortRadial_8u_C3R pSrc srcStep pDst dstStep roiSize fx fy cx cy k1 k2 pBuffer


------ arity 2 -------------

{- |     Adds, subtracts, or multiplies pixel values of two
              source images and places the scaled result in the destination image.
 -}
ioAdd_8u_C1RSfs scaleFactor = {-# SCC "ippiAdd_8u_C1RSfs" #-} auto_2_8u_C1RSfs (f scaleFactor) "ippiAdd_8u_C1RSfs"
    where f scaleFactor pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize = ippiAdd_8u_C1RSfs pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize scaleFactor

{- |     Adds, subtracts, or multiplies pixel values of two
              source images and places the scaled result in the destination image.
 -}
ioSub_8u_C1RSfs scaleFactor = {-# SCC "ippiSub_8u_C1RSfs" #-} auto_2_8u_C1RSfs (f scaleFactor) "ippiSub_8u_C1RSfs"
    where f scaleFactor pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize = ippiSub_8u_C1RSfs pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize scaleFactor

{- |     Adds, subtracts, or multiplies pixel values of two
              source images and places the scaled result in the destination image.
 -}
ioAdd_32f_C1R  = {-# SCC "ippiAdd_32f_C1R" #-} auto_2_32f_C1R f "ippiAdd_32f_C1R"
    where f pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize = ippiAdd_32f_C1R pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize

{- |     Adds, subtracts, or multiplies pixel values of two
              source images and places the scaled result in the destination image.
 -}
ioSub_32f_C1R  = {-# SCC "ippiSub_32f_C1R" #-} auto_2_32f_C1R f "ippiSub_32f_C1R"
    where f pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize = ippiSub_32f_C1R pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize

{- |     Adds, subtracts, or multiplies pixel values of two
              source images and places the scaled result in the destination image.
 -}
ioMul_32f_C1R  = {-# SCC "ippiMul_32f_C1R" #-} auto_2_32f_C1R f "ippiMul_32f_C1R"
    where f pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize = ippiMul_32f_C1R pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize

{- |         Performs corresponding bitwise logical operation between pixels of two image
                  (AndC/OrC/XorC  - between pixel of the source image and a constant)
 -}
ioAnd_8u_C1R  = {-# SCC "ippiAnd_8u_C1R" #-} auto_2_8u_C1R f "ippiAnd_8u_C1R"
    where f pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize = ippiAnd_8u_C1R pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize

{- |         Performs corresponding bitwise logical operation between pixels of two image
                  (AndC/OrC/XorC  - between pixel of the source image and a constant)
 -}
ioOr_8u_C1R  = {-# SCC "ippiOr_8u_C1R" #-} auto_2_8u_C1R f "ippiOr_8u_C1R"
    where f pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize = ippiOr_8u_C1R pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize

{- |         Performs corresponding bitwise logical operation between pixels of two image
                  (AndC/OrC/XorC  - between pixel of the source image and a constant)
 -}
ioXor_8u_C1R  = {-# SCC "ippiXor_8u_C1R" #-} auto_2_8u_C1R f "ippiXor_8u_C1R"
    where f pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize = ippiXor_8u_C1R pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize

{- |   Compares pixel values of two images, or pixel values of an image to a constant -}
ioCompare_8u_C1R ippCmpOp = {-# SCC "ippiCompare_8u_C1R" #-} auto_2_8u_C1R (f ippCmpOp) "ippiCompare_8u_C1R"
    where f ippCmpOp pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize = ippiCompare_8u_C1R pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize ippCmpOp

{- |   Compares pixel values of two images, or pixel values of an image to a constant -}
ioCompare_32f_C1R ippCmpOp = {-# SCC "ippiCompare_32f_C1R" #-} auto_2_32f_C1R (f ippCmpOp) "ippiCompare_32f_C1R"
    where f ippCmpOp pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize = ippiCompare_32f_C1R pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize ippCmpOp

{- |  Calculate absolute difference between corresponding pixels of the two images
           or between image pixels and scalar.
 -}
ioAbsDiff_8u_C1R  = {-# SCC "ippiAbsDiff_8u_C1R" #-} auto_2_8u_C1R f "ippiAbsDiff_8u_C1R"
    where f pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize = ippiAbsDiff_8u_C1R pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize

{- |  Calculate absolute difference between corresponding pixels of the two images
           or between image pixels and scalar.
 -}
ioAbsDiff_32f_C1R  = {-# SCC "ippiAbsDiff_32f_C1R" #-} auto_2_32f_C1R f "ippiAbsDiff_32f_C1R"
    where f pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize = ippiAbsDiff_32f_C1R pSrc1 src1Step pSrc2 src2Step pDst dstStep roiSize

{- |    Calculates standard deviation on rectangular window
 -}
ioRectStdDev_32f_C1R rect = {-# SCC "ippiRectStdDev_32f_C1R" #-} auto_2_32f_C1R (f rect) "ippiRectStdDev_32f_C1R"
    where f rect pSrc srcStep pSqr sqrStep pDst dstStep roiSize = ippiRectStdDev_32f_C1R pSrc srcStep pSqr sqrStep pDst dstStep roiSize rect


------ inplace arity 2 ------

{- |             calculation min/max value for every element of two images -}
ioMaxEvery_8u_C1IR  = {-# SCC "ippiMaxEvery_8u_C1IR" #-} auto_11_8u_C1IR f "ippiMaxEvery_8u_C1IR"
    where f pSrc srcStep pSrcDst srcDstStep roiSize = ippiMaxEvery_8u_C1IR pSrc srcStep pSrcDst srcDstStep roiSize

{- |             calculation min/max value for every element of two images -}
ioMinEvery_8u_C1IR  = {-# SCC "ippiMinEvery_8u_C1IR" #-} auto_11_8u_C1IR f "ippiMinEvery_8u_C1IR"
    where f pSrc srcStep pSrcDst srcDstStep roiSize = ippiMinEvery_8u_C1IR pSrc srcStep pSrcDst srcDstStep roiSize

{- |             calculation min/max value for every element of two images -}
ioMaxEvery_32f_C1IR  = {-# SCC "ippiMaxEvery_32f_C1IR" #-} auto_11_32f_C1IR f "ippiMaxEvery_32f_C1IR"
    where f pSrc srcStep pSrcDst srcDstStep roiSize = ippiMaxEvery_32f_C1IR pSrc srcStep pSrcDst srcDstStep roiSize

{- |             calculation min/max value for every element of two images -}
ioMinEvery_32f_C1IR  = {-# SCC "ippiMinEvery_32f_C1IR" #-} auto_11_32f_C1IR f "ippiMinEvery_32f_C1IR"
    where f pSrc srcStep pSrcDst srcDstStep roiSize = ippiMinEvery_32f_C1IR pSrc srcStep pSrcDst srcDstStep roiSize


----------------------------
