-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Ipp.Pure
Copyright   :  (c) Alberto Ruiz 2007
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Purely functional image processing.

-}
-----------------------------------------------------------------------------

module ImagProc.Ipp.Pure (
    (.*),
    (|+|),(|-|),absDiff,(|*|),
    andI,orI,notI,
    absDiff8u, sub8u,
    float, toGray, scale32f8u, scale8u32f,
    rgbToHSV, hsvToRGB,
    thresholdVal32f, thresholdVal8u,
    filterMax32f, filterBox,
    maxEvery, minEvery,
    sobelVert, sobelHoriz,
    gauss, laplace, median, highPass8u,
    magnitudePack,
    abs32f, sqrt32f, mirror8u,
    dilate3x3, erode3x3
)
where

import ImagProc.Ipp.Core
import ImagProc.Ipp.Auto
import Foreign

infixl 7  |*|, .*
infixl 6  |+|, |-|


mkId f = unsafePerformIO . f id
mkInt f a b = unsafePerformIO (f intersection a b)
mkShrink s f = unsafePerformIO . f (shrink s)

-- should be generic using clone (must break cycle of import)
mkIdIPInt32f f a b = unsafePerformIO $ do
    let roi = intersection (theROI a) (theROI b)
    r <- ioCopy_32f_C1R (const roi) b
    f undefined (modifyROI (const roi) a) r
    return r


-- | image scaling
(.*) :: Float -> ImageFloat -> ImageFloat
v .* im = unsafePerformIO $ ioMulC_32f_C1R v id im

-- | image arithmetic, pixel by pixel
(|+|), (|-|), (|*|), absDiff :: ImageFloat -> ImageFloat -> ImageFloat
(|+|) = mkInt ioAdd_32f_C1R
(|-|) = flip (mkInt ioSub_32f_C1R)
(|*|) = mkInt ioMul_32f_C1R
absDiff = mkInt ioAbsDiff_32f_C1R

-- | image logic, pixel by pixel
andI, orI :: ImageGray -> ImageGray -> ImageGray
andI = mkInt ioAnd_8u_C1R
orI  = mkInt ioOr_8u_C1R

notI :: ImageGray -> ImageGray
notI = mkId ioNot_8u_C1R

absDiff8u:: ImageGray -> ImageGray -> ImageGray
absDiff8u = mkInt ioAbsDiff_8u_C1R

sub8u :: Int -> ImageGray -> ImageGray -> ImageGray
sub8u k = flip (mkInt (ioSub_8u_C1RSfs k))

float :: ImageGray -> ImageFloat
float = mkId (ioScale_8u32f_C1R 0 1)

toGray :: ImageFloat -> ImageGray
toGray = scale32f8u 0 1

scale32f8u :: Float -> Float -> ImageFloat -> ImageGray
scale32f8u mn mx = mkId (ioScale_32f8u_C1R mn mx)

scale8u32f :: Float -> Float -> ImageGray -> ImageFloat
scale8u32f mn mx = mkId (ioScale_8u32f_C1R mn mx)

rgbToHSV :: ImageRGB -> ImageRGB
rgbToHSV = mkId ioRGBToHSV_8u_C3R

hsvToRGB :: ImageRGB -> ImageRGB
hsvToRGB = mkId ioHSVToRGB_8u_C3R

-- | The result is the source image in which the pixels verifing the comparation with a threshold are set to a desired value.
thresholdVal32f :: Float          -- ^ threshold
                -> Float          -- ^ value
                -> IppCmp         -- ^ comparison function
                -> ImageFloat     -- ^ source image
                -> ImageFloat  -- ^ result
thresholdVal32f t v cmp = mkId (ioThreshold_Val_32f_C1R t v (codeCmp cmp))

-- | The result is the source image in which the pixels verifing the comparation with a threshold are set to a desired value.
thresholdVal8u :: CUChar          -- ^ threshold
                -> CUChar          -- ^ value
                -> IppCmp         -- ^ comparison function
                -> ImageGray     -- ^ source image
                -> ImageGray  -- ^ result
thresholdVal8u t v cmp = mkId (ioThreshold_Val_8u_C1R t v (codeCmp cmp))

------------------------------

-- | Changes each pixel by the maximum value in its neighbourhood of given diameter.
filterMax32f :: Int -> ImageFloat -> ImageFloat
filterMax32f r = mkShrink (d,d) (ioFilterMax_32f_C1R sz pt) where
    d = (r-1) `quot` 2
    sz = IppiSize (fi r) (fi r)
    pt = IppiPoint (fi d) (fi d)

-------------------------------

-- | image average in rectangles of given semiheight and semiwidth
filterBox :: Int -> Int -> ImageFloat -> ImageFloat
filterBox h w = mkShrink (h,w) (ioFilterBox_32f_C1R sz pt) where
    sz = IppiSize (fi (2*w+1)) (fi (2*h+1))
    pt = IppiPoint (fi w) (fi h)

-----------------------------------

-- | Applies a vertical Sobel filter (typically used for computing gradient images).
sobelVert :: ImageFloat -> ImageFloat
sobelVert = mkShrink (1,1) ioFilterSobelVert_32f_C1R

-- | Applies a horizontal Sobel filter (typically used for computing gradient images).
sobelHoriz ::ImageFloat -> ImageFloat
sobelHoriz = mkShrink (1,1) ioFilterSobelHoriz_32f_C1R

-- | Convolution with a gaussian mask of the desired size.
gauss :: Mask -> ImageFloat -> ImageFloat
gauss Mask5x5 = mkShrink (2,2) (ioFilterGauss_32f_C1R (codeMask Mask5x5))
gauss Mask3x3 = mkShrink (1,1) (ioFilterGauss_32f_C1R (codeMask Mask3x3))

-- | Convolution with a laplacian mask of the desired size.
laplace :: Mask -> ImageFloat -> ImageFloat
laplace Mask5x5 = mkShrink (2,2) (ioFilterLaplace_32f_C1R (codeMask Mask5x5))
laplace Mask3x3 = mkShrink (1,1) (ioFilterLaplace_32f_C1R (codeMask Mask3x3))


-- | Median Filter
median :: Mask -> ImageGray -> ImageGray
median mask = mkShrink (s,s) (ioFilterMedian_8u_C1R m p) where
    s = case mask of
                Mask3x3 -> 1
                Mask5x5 -> 2
    m = case mask of
                Mask3x3 -> IppiSize 3 3
                Mask5x5 -> IppiSize 5 5
    p = IppiPoint (fi s) (fi s)

-- | High pass filter
highPass8u :: Mask -> ImageGray -> ImageGray
highPass8u Mask5x5 = mkShrink (2,2) (ioFilterHipass_8u_C1R (codeMask Mask5x5))
highPass8u Mask3x3 = mkShrink (1,1) (ioFilterHipass_8u_C1R (codeMask Mask3x3))

-- | Computes the magnitude of a complex packed 32f image (typically produced by the FFT computed by the result of 'genFFT')
magnitudePack :: ImageFloat -> ImageFloat
magnitudePack = mkId ioMagnitudePack_32f_C1R

---------------------------------------------------

-- | The result contains the absolute values of the pixels in the input image.
abs32f :: ImageFloat -> ImageFloat
abs32f  = mkId ioAbs_32f_C1R

-- | The result contains the square roots of the pixels in the input image.
sqrt32f :: ImageFloat -> ImageFloat
sqrt32f = mkId ioSqrt_32f_C1R

----------------------------------------------------

mirror8u :: Int -> ImageGray -> ImageGray
mirror8u axis = mkId (ioMirror_8u_C1R (fi axis))

-----------------------------------------------------

-- | dilatation 3x3
dilate3x3 :: ImageGray -> ImageGray
dilate3x3 = mkShrink (1,1) ioDilate3x3_8u_C1R

-- | erosion 3x3
erode3x3 :: ImageGray -> ImageGray
erode3x3 = mkShrink (1,1) ioErode3x3_8u_C1R

------------------------------------------------------

-- | pixelwise maximum of two images
maxEvery :: ImageFloat -> ImageFloat -> ImageFloat
maxEvery = mkIdIPInt32f ioMaxEvery_32f_C1IR

-- | pixelwise minimum of two images
minEvery :: ImageFloat -> ImageFloat -> ImageFloat
minEvery = mkIdIPInt32f ioMinEvery_32f_C1IR

------------------------------------------------------
