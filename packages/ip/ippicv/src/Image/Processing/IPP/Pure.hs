-----------------------------------------------------------------------------
{- |
Module      :  Image.Processing.IPP.Pure
Copyright   :  (c) Alberto Ruiz 2006-15
License     :  GPL

Maintainer  :  Alberto Ruiz <aruiz@um.es>
Stability   :  provisional

-}
-----------------------------------------------------------------------------

module Image.Processing.IPP.Pure (
    rgbToGray
)
where

import Image.Processing.IPP.Core
import Image.Processing.IPP.Auto
--import Foreign.Ptr


mkId f = unsafePerformIO . f id

{-
mkInt f a b = unsafePerformIO (f intersection intersection intersection a b)

mkShrink s f = unsafePerformIO . f (shrink s)

mkRel f x y = unsafePerformIO (f g (flip g) g x y) where
    g a b = intersection a b' where
        d = getShift b a
        b' = shift d b
-}

{-
-- should be generic using clone (must break cycle of import)
mkIdIPInt32f f a b = unsafePerformIO $ do
    let r = intersection (roi a) (roi b)
    x <- ioCopy_32f_C1R (const r) b
    _ <- f undefined (setROI r a) x
    return x
-}

{-
mkIdIPInt8u f a b = unsafePerformIO $ do
    let r = intersection (roi a) (roi b)
    x <- ioCopy_8u_C1R (const r) b
    _ <- f undefined (setROI r a) x
    return x
-}

{-

infixl 7  |*|, .*
infixl 6  |+|, |-|


-- | image scaling
(.*) :: Float -> Image Float -> Image Float
v .* im = unsafePerformIO $ ioMulC_32f_C1R v id im

-- | add constant
(.+) :: Float -> Image Float -> Image Float
v .+ im = unsafePerformIO $ ioAddC_32f_C1R v id im

-- | image sum, pixel by pixel
(|+|) :: Image Float -> Image Float -> Image Float
(|+|) = mkInt ioAdd_32f_C1R

-- | image difference, pixel by pixel
(|-|) :: Image Float -> Image Float -> Image Float
(|-|) = flip (mkInt ioSub_32f_C1R)

-- | image product, pixel by pixel
(|*|) :: Image Float -> Image Float -> Image Float
(|*|) = mkInt ioMul_32f_C1R

-- | image division, pixel by pixel
(|/|) :: Image Float -> Image Float -> Image Float
(|/|) = flip (mkInt ioDiv_32f_C1R)

-- | absolute difference of images, pixel by pixel
absDiff32f :: Image Float -> Image Float -> Image Float
absDiff32f = mkInt ioAbsDiff_32f_C1R

andI :: Image Word8 -> Image Word8 -> Image Word8
andI = mkInt ioAnd_8u_C1R

orI :: Image Word8 -> Image Word8 -> Image Word8
orI  = mkInt ioOr_8u_C1R

notI :: Image Word8 -> Image Word8
notI = mkId ioNot_8u_C1R

xorI :: Image Word8 -> Image Word8 -> Image Word8
xorI  = mkInt ioXor_8u_C1R


-- | image sum, pixel by pixel
add8u :: Int -> Image Word8 -> Image Word8 -> Image Word8
add8u k = flip (mkInt (ioAdd_8u_C1RSfs k))

-- | absolute difference of images, pixel by pixel
absDiff8u:: Image Word8 -> Image Word8 -> Image Word8
absDiff8u = mkInt ioAbsDiff_8u_C1R

-- | absolute difference of images, pixel by pixel
absDiff8u3:: Image Word24 -> Image Word24 -> Image Word24
absDiff8u3 = mkInt ioAbsDiff_8u_C3R


-- | image difference
sub8u :: Int -> Image Word8 -> Image Word8 -> Image Word8
sub8u k = flip (mkInt (ioSub_8u_C1RSfs k))

-- | image difference of ROIS
sub8uRel :: Int -> Image Word8 -> Image Word8 -> Image Word8
sub8uRel k = flip (mkRel (ioSub_8u_C1RSfs k))

-- | compare with a constant
compareC8u :: Word8 -> IppCmp -> Image Word8 -> Image Word8
compareC8u v cmp = mkId (ioCompareC_8u_C1R v (codeCmp cmp))

-- | add constant
addC8u' :: Int -> Word8 -> Image Word8 -> Image Word8
addC8u' k v = mkId (ioAddC_8u_C1RSfs v k)

-- | sub constant
subC8u' :: Int -> Word8 -> Image Word8 -> Image Word8
subC8u' k v = mkId (ioSubC_8u_C1RSfs v k)

-- | add or sub constant
addC8u :: Int -> Int -> Image Word8 -> Image Word8
addC8u k v | v > 0     = addC8u' k (fromIntegral v)
           | otherwise = subC8u' k (fromIntegral (-v))

-- | compare 8u images
compare8u :: IppCmp -> Image Word8 -> Image Word8 -> Image Word8
compare8u cmp = mkInt (ioCompare_8u_C1R (codeCmp cmp))


-- | conversion from discrete gray level images (0-255) to floating point (0->0, 255->1)
toFloat :: Image Word8 -> Image Float
toFloat = mkId (ioScale_8u32f_C1R 0 1)

-- | the inverse of 'float'
toGray :: Image Float -> Image Word8
toGray = scale32f8u 0 1

-- | similar to 'toGray' with explicit conversion range
scale32f8u :: Float -> Float -> Image Float -> Image Word8
scale32f8u mn mx = mkId (ioScale_32f8u_C1R mn mx)

-- | similar to 'float' with explicit conversion range
scale8u32f :: Float -> Float -> Image Word8 -> Image Float
scale8u32f mn mx = mkId (ioScale_8u32f_C1R mn mx)

rgbToHSV :: Image Word24 -> Image Word24
rgbToHSV = mkId ioRGBToHSV_8u_C3R

hsvToRGB :: Image Word24 -> Image Word24
hsvToRGB = mkId ioHSVToRGB_8u_C3R

-}

rgbToGray :: Image Word24 -> Image Word8
rgbToGray = mkId ioRGBToGray_8u_C3C1R

{-

yCbCrToRGB :: Image Word16 -> Image Word24
yCbCrToRGB = mkId ioYCbCr422ToRGB_8u_C2C3R

rgbToYCbCr :: Image Word24 -> Image Word16
rgbToYCbCr = mkId ioRGBToYCbCr422_8u_C3C2R


-- | The result is the source image in which the pixels verifing the comparation with a threshold are set to a desired value.
thresholdVal32f :: Float          -- ^ threshold
                -> Float          -- ^ value
                -> IppCmp         -- ^ comparison function
                -> Image Float     -- ^ source image
                -> Image Float  -- ^ result
thresholdVal32f t v cmp = mkId (ioThreshold_Val_32f_C1R t v (codeCmp cmp))

-- | The result is the source image in which the pixels verifing the comparation with a threshold are set to a desired value.
thresholdVal8u  :: Word8          -- ^ threshold
                -> Word8          -- ^ value
                -> IppCmp         -- ^ comparison function
                -> Image Word8     -- ^ source image
                -> Image Word8  -- ^ result
thresholdVal8u t v cmp = mkId (ioThreshold_Val_8u_C1R t v (codeCmp cmp))

------------------------------

-- | Changes each pixel by the maximum value in its neighbourhood of given radius.
filterMax32f :: Int -> Image Float -> Image Float
filterMax32f 0 = id
filterMax32f r = mkShrink (r,r) (ioFilterMax_32f_C1R sz pt) where
    d = fi (2*r+1)
    sz = IppiSize d d
    pt = IppiPoint (fi r) (fi r)

-- | Changes each pixel by the minimum value in its neighbourhood of given radius.
filterMin32f :: Int -> Image Float -> Image Float
filterMin32f 0 = id
filterMin32f r = mkShrink (r,r) (ioFilterMin_32f_C1R sz pt) where
    d = fi (2*r+1)
    sz = IppiSize d d
    pt = IppiPoint (fi r) (fi r)

-- | Changes each pixel by the maximum value in its neighbourhood of given radius.
filterMax8u :: Int -> Image Word8 -> Image Word8
filterMax8u 0 = id
filterMax8u r = mkShrink (r,r) (ioFilterMax_8u_C1R sz pt) where
    d = fi (2*r+1)
    sz = IppiSize d d
    pt = IppiPoint (fi r) (fi r)

-- | Changes each pixel by the minimum value in its neighbourhood of given radius.
filterMin8u :: Int -> Image Word8 -> Image Word8
filterMin8u 0 = id
filterMin8u r = mkShrink (r,r) (ioFilterMin_8u_C1R sz pt) where
    d = fi (2*r+1)
    sz = IppiSize d d
    pt = IppiPoint (fi r) (fi r)

-------------------------------

-- | image average in rectangles of given semiheight and semiwidth
filterBox32f :: Int -> Int -> Image Float -> Image Float
filterBox32f 0 0 = id
filterBox32f h w = mkShrink (h,w) (ioFilterBox_32f_C1R sz pt) where
    sz = IppiSize (fi (2*w+1)) (fi (2*h+1))
    pt = IppiPoint (fi w) (fi h)

-- | image average in rectangles of given semiheight and semiwidth
filterBox8u :: Int -> Int -> Image Word8 -> Image Word8
filterBox8u 0 0 = id
filterBox8u h w = mkShrink (h,w) (ioFilterBox_8u_C1R sz pt) where
    sz = IppiSize (fi (2*w+1)) (fi (2*h+1))
    pt = IppiPoint (fi w) (fi h)

-----------------------------------

-- | Applies a vertical Sobel filter (typically used for computing gradient images).
sobelVert :: Image Float -> Image Float
sobelVert = mkShrink (1,1) ioFilterSobelVert_32f_C1R

-- | Applies a horizontal Sobel filter (typically used for computing gradient images).
sobelHoriz ::Image Float -> Image Float
sobelHoriz = mkShrink (1,1) ioFilterSobelHoriz_32f_C1R

-- | Convolution with a gaussian mask of the desired size.
gauss32f :: Mask -> Image Float -> Image Float
gauss32f Mask5x5 = mkShrink (2,2) (ioFilterGauss_32f_C1R (codeMask Mask5x5))
gauss32f Mask3x3 = mkShrink (1,1) (ioFilterGauss_32f_C1R (codeMask Mask3x3))

-- | Convolution with a gaussian mask of the desired size.
gauss8u :: Mask -> Image Word8 -> Image Word8
gauss8u Mask5x5 = mkShrink (2,2) (ioFilterGauss_8u_C1R (codeMask Mask5x5))
gauss8u Mask3x3 = mkShrink (1,1) (ioFilterGauss_8u_C1R (codeMask Mask3x3))


-- | Convolution with a laplacian mask of the desired size.
laplace :: Mask -> Image Float -> Image Float
laplace Mask5x5 = mkShrink (2,2) (ioFilterLaplace_32f_C1R (codeMask Mask5x5))
laplace Mask3x3 = mkShrink (1,1) (ioFilterLaplace_32f_C1R (codeMask Mask3x3))


-- | Median Filter
median :: Mask -> Image Word8 -> Image Word8
median mask = mkShrink (s,s) (ioFilterMedian_8u_C1R m p) where
    s = case mask of
                Mask3x3 -> 1
                Mask5x5 -> 2
    m = case mask of
                Mask3x3 -> IppiSize 3 3
                Mask5x5 -> IppiSize 5 5
    p = IppiPoint (fi s) (fi s)

-- | Median Filter of given window radius
filterMedian :: Int -> Image Word8 -> Image Word8
filterMedian r im | r <= 0    = im
                  | otherwise = mkShrink (r,r) (ioFilterMedian_8u_C1R s p) im
  where
    m = 2*r+1
    s = IppiSize (fi m) (fi m)
    p = IppiPoint (fi r) (fi r)
    

-- | High pass filter
highPass8u :: Mask -> Image Word8 -> Image Word8
highPass8u Mask5x5 = mkShrink (2,2) (ioFilterHipass_8u_C1R (codeMask Mask5x5))
highPass8u Mask3x3 = mkShrink (1,1) (ioFilterHipass_8u_C1R (codeMask Mask3x3))

-- | Computes the magnitude of a complex packed 32f image (typically produced by the FFT computed by the result of 'genFFT')
magnitudePack :: Image Float -> Image Float
magnitudePack = mkId ioMagnitudePack_32f_C1R

---------------------------------------------------

-- | The result contains the absolute values of the pixels in the input image.
abs32f :: Image Float -> Image Float
abs32f  = mkId ioAbs_32f_C1R

-- | The result contains the square roots of the pixels in the input image.
sqrt32f :: Image Float -> Image Float
sqrt32f = mkId ioSqrt_32f_C1R

----------------------------------------------------

mirror8u :: Int -> Image Word8 -> Image Word8
mirror8u axis = mkId (ioMirror_8u_C1R (fi axis))

-----------------------------------------------------

dilate3x3 :: Image Word8 -> Image Word8
dilate3x3 = mkShrink (1,1) ioDilate3x3_8u_C1R

erode3x3 :: Image Word8 -> Image Word8
erode3x3 = mkShrink (1,1) ioErode3x3_8u_C1R

------------------------------------------------------

-- | pixelwise maximum of two images
maxEvery32f :: Image Float -> Image Float -> Image Float
maxEvery32f = mkIdIPInt32f ioMaxEvery_32f_C1IR

-- | pixelwise minimum of two images
minEvery32f :: Image Float -> Image Float -> Image Float
minEvery32f = mkIdIPInt32f ioMinEvery_32f_C1IR


-- | pixelwise maximum of two images
maxEvery8u :: Image Word8 -> Image Word8 -> Image Word8
maxEvery8u = mkIdIPInt8u ioMaxEvery_8u_C1IR

-- | pixelwise minimum of two images
minEvery8u :: Image Word8 -> Image Word8 -> Image Word8
minEvery8u = mkIdIPInt8u ioMinEvery_8u_C1IR

------------------------------------------------------

undistortRadial8u fx fy cx cy k1 k2 = mkId (ioUndistortRadial_8u_C1R fx fy cx cy k1 k2 nullPtr)
undistortRadial8u3 fx fy cx cy k1 k2 = mkId (ioUndistortRadial_8u_C3R fx fy cx cy k1 k2 nullPtr)
undistortRadial32f fx fy cx cy k1 k2 = mkId (ioUndistortRadial_32f_C1R fx fy cx cy k1 k2 nullPtr)

-}

