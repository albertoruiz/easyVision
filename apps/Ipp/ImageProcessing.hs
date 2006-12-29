-----------------------------------------------------------------------------
{- |
Module      :  Ipp.ImageProcessing
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

A collection of frequently used image processing functions.

-}
-----------------------------------------------------------------------------

module Ipp.ImageProcessing (
-- * IPP auxiliary structures
  Mask(..)
, IppCmp(..)
-- * Utilities
, jaehne32f
, set32f
, copyROI32f
, times
, partit
-- * Image manipulation
, rgbToGray
, scale8u32f
, scale32f8u
, copy32f
, copy8u
, copy8uC3
, copyMask32f
, resize32f
, warp, warpOn
-- * Image arithmetic
, scale32f
, (|*|), (|+|), (|-|)
, abs32f, sqrt32f
, compare32f
, thresholdVal32f
, minmax
, integral
-- * Basic image processing
, gauss
, sobelVert, sobelHoriz
, secondOrder
, hessian
, filterMax32f
, localMax
, getPoints32f
-- * Computation of interest points
, getCorners
)
where

import Ipp.Core
import Ipp.Wrappers
import Foreign
import Vision --hiding ((|-|),(|+|))
import GSL hiding (size)

---------------------------------------

imgAsR1 roifun im = do 
    r <- imgAs im
    return r {vroi = roifun (vroi im)}

cr1 f im r = f // src im (vroi r) // dst r (vroi r)

imgAsR2 roifun im1 im2 = do 
    r <- imgAs im1
    return r {vroi = roifun (vroi im1) (vroi im2)}

cr2 f im1 im2 r = f // src im1 (vroi r) // src im2 (vroi r)// dst r (vroi r)

----------------------------------------

-- | Writes into a existing image a desired value in a specified roi.
set32f :: Float      -- ^ desired value
       -> ImageFloat -- ^ input image
       -> ROI        -- ^ roi
       -> IO ()
set32f v (F im) roi = ippiSet_32f_C1R v // dst im roi // checkIPP "set32f" [im]

-- | Creates a 8u Gray image from a 8uC3 RGB image
rgbToGray :: ImageRGB      -- ^ input image
          -> IO ImageGray  -- ^ result
rgbToGray (C im) = do
    r' <- img Gray (isize im)
    let r = r' {vroi = vroi im}
    cr1 ippiRGBToGray_8u_C3C1R im r // checkIPP "RGBToGray" [im]
    return (G r)


-- | Creates a 32f image from an 8u image.
scale8u32f :: Float             -- ^ desired value corresponding to 0
           -> Float             -- ^ desired value corresponding to 255
           -> ImageGray         -- ^ input image
           -> IO ImageFloat     -- ^ result
scale8u32f vmin vmax (G im) = do
    r' <- img I32f (isize im)
    let r = r' {vroi = vroi im}
    (cr1 ippiScale_8u32f_C1R im r) vmin vmax // checkIPP "scale8u32f" [im]
    return (F r)

-- | Creates a 8u image from an 32f image.
scale32f8u :: Float             -- ^ desired value corresponding to 0
           -> Float             -- ^ desired value corresponding to 255
           -> ImageFloat        -- ^ input image
           -> IO ImageGray      -- ^ result
scale32f8u vmin vmax (F im) = do
    r' <- img Gray (isize im)
    let r = r' {vroi = vroi im}
    (cr1 ippiScale_32f8u_C1R im r) vmin vmax // checkIPP "scale8u32f" [im]
    return (G r)

-- | Creates an integral (cumulative sum) 32f image from an 8u image. Obtains a roi of the same size, but each pixel has the sum of the pixels strictly less than its position, so the first row and column contains zeroes and the last ones are not taken into account (sorry for the sentence).
integral :: ImageGray -> IO ImageFloat
integral (G im) = do
    r' <- img I32f (isize im)
    -- strange roi ...
    let ROI r1 r2 c1 c2 = vroi im
    let roi = ROI r1 (r2-1) c1 (c2-1)-- `intersection` vroi r'
    let r = r' {vroi = roi}
    (cr1 ippiIntegral_8u32f_C1R im r) 0 // checkIPP "integral" [im]
    return (F r {vroi = vroi im})

-- | Copies the roi of the input image into the destination image.
copyROI32f :: ImageFloat -> ImageFloat -> IO ()
copyROI32f (F r) (F im) = ippiCopy_32f_C1R // src im (vroi im) // dst r (vroi im) // checkIPP "copyROI32f" [im]

simplefun1F ippfun roifun msg = g where
    g (F im) = do
        r <- imgAsR1 roifun im
        cr1 ippfun im r // checkIPP msg [im]
        return (F r) 

simplefun1G ippfun roifun msg = g where
    g (G im) = do
        r <- imgAsR1 roifun im
        cr1 ippfun im r // checkIPP msg [im]
        return (G r)

simplefun1C ippfun roifun msg = g where
    g (C im) = do
        r <- imgAsR1 roifun im
        cr1 ippfun im r // checkIPP msg [im]
        return (C r)


-- | Creates a image of the same size as the source and copies its roi.
copy32f :: ImageFloat -> IO ImageFloat
copy32f = simplefun1F ippiCopy_32f_C1R id "copy32f"

-- | Creates a image of the same size as the source and copies its roi.
copy8u :: ImageGray -> IO ImageGray
copy8u = simplefun1G ippiCopy_8u_C1R id "copy8u"

-- | Creates a image of the same size as the source and copies its roi.
copy8uC3 :: ImageRGB -> IO ImageRGB
copy8uC3 = simplefun1C ippiCopy_8u_C3R id "copy8uC3"


-- | The result contains the absolute values of the pixels in the input image.
abs32f :: ImageFloat -> IO ImageFloat
abs32f  = simplefun1F ippiAbs_32f_C1R  id "abs32f"

-- | The result contains the square roots of the pixels in the input image.
sqrt32f :: ImageFloat -> IO ImageFloat
sqrt32f = simplefun1F ippiSqrt_32f_C1R id "sqrt32f"

-- | Applies a vertical Sobel filter (typically used for computing gradient images).
sobelVert :: ImageFloat -> IO ImageFloat
sobelVert = simplefun1F ippiFilterSobelVert_32f_C1R (shrink (1,1)) "sobelVert"

-- | Applies a horizontal Sobel filter (typically used for computing gradient images).
sobelHoriz ::ImageFloat -> IO ImageFloat
sobelHoriz = simplefun1F ippiFilterSobelHoriz_32f_C1R (shrink (1,1)) "sobelHoriz"

data Mask = Mask3x3 | Mask5x5
code Mask3x3 = 33
code Mask5x5 = 55

-- | Convolution with a gaussian mask of the desired size.
gauss :: Mask -> ImageFloat -> IO ImageFloat
gauss mask = simplefun1F f (shrink (s,s)) "gauss" where
    s = case mask of
                Mask3x3 -> 1
                Mask5x5 -> 2
    f ps ss pd sd r = ippiFilterGauss_32f_C1R ps ss pd sd r (code mask)

-- | The result is the source image in which the pixels verifing the comparation with a threshold are set to a desired value.
thresholdVal32f :: Float          -- ^ threshold
                -> Float          -- ^ value
                -> IppCmp         -- ^ comparison function
                -> ImageFloat     -- ^ source image
                -> IO ImageFloat  -- ^ result
thresholdVal32f t v cmp = simplefun1F f id "thresholdVal32f" where
    f ps ss pd sd r = ippiThreshold_Val_32f_C1R ps ss pd sd r t v (codeCmp cmp)

-- | Changes each pixel by the maximum value in its neighbourhood of given radius.
filterMax32f :: Int -> ImageFloat -> IO ImageFloat
filterMax32f sz = simplefun1F f (shrink (d,d)) "filterMax32f" where
    d = (sz-1) `quot` 2
    f ps ss pd sd r = ippiFilterMax_32f_C1R ps ss pd sd r (ippRect sz sz) (ippRect d d)

---------------------------------------------

simplefun2 ippfun roifun msg = g where
    g (F im1) (F im2) = do
        r <- imgAsR2 roifun im1 im2
        cr2 ippfun im1 im2 r // checkIPP msg [im1,im2]
        return (F r)

infixl 7  |*|
infixl 6  |+|, |-|
-- | Pixel by pixel multiplication.
(|*|) :: ImageFloat -> ImageFloat -> IO ImageFloat
(|*|) = simplefun2 ippiMul_32f_C1R intersection "mul32f"

-- | Pixel by pixel addition.
(|+|) :: ImageFloat -> ImageFloat -> IO ImageFloat
(|+|) = simplefun2 ippiAdd_32f_C1R intersection "add32f"

-- | Pixel by pixel substraction.
(|-|) :: ImageFloat -> ImageFloat -> IO ImageFloat
(|-|) = flip $ simplefun2 ippiSub_32f_C1R intersection "sub32f" -- more natural argument order

-- | Multiplies the pixel values of an image by a given value.
scale32f :: Float -> ImageFloat -> IO ImageFloat
scale32f v = simplefun1F f id "mulC32f" where
    f ps ss pd sd r = ippiMulC_32f_C1R ps ss v pd sd r

codeCmp IppCmpLess      = 0
codeCmp IppCmpLessEq    = 1
codeCmp IppCmpEq        = 2
codeCmp IppCmpGreaterEq = 3
codeCmp IppCmpGreater   = 4

data IppCmp = IppCmpLess | IppCmpLessEq | IppCmpEq | IppCmpGreaterEq | IppCmpGreater

-- | The result is the pixelswise comparation of the two source images.
compare32f :: IppCmp -> ImageFloat -> ImageFloat -> IO ImageGray
compare32f cmp (F im1) (F im2) = do
    r <- img Gray (isize im1)
    let roi = intersection (vroi im1) (vroi im2)
    (ippiCompare_32f_C1R // src im1 roi // src im2 roi // dst r roi) (codeCmp cmp) // checkIPP "compare32f" [im1,im2]
    return (G r {vroi = roi})

-- | Creates a copy of the source image only on corresponding pixels in which mask=255
copyMask32f :: ImageFloat    -- ^ source image
            -> ImageGray     -- ^ mask image
            -> IO ImageFloat -- ^ result
copyMask32f (F im) (G mask) = do
    r <- imgAs im
    let roi = intersection (vroi im) (vroi mask)
    set32f 0.0 (F r) (fullroi r)
    ippiCopy_32f_C1MR // src im roi // dst r roi // src mask roi // checkIPP "copyMask32f" [im,mask]
    return $ F r {vroi = roi}

-- | Nonmaximum supression. Given an I32f image returns a copy of the input image with all the pixels which are not local maxima set to 0.0.
localMax :: Int         -- ^ radius of the filterMax32f
         -> ImageFloat  -- ^ input image
         -> IO ImageFloat   -- ^ result
localMax r g = do
    mg   <- filterMax32f r g
    mask <- compare32f IppCmpEq mg g
    r    <- copyMask32f g mask
    return r

-- | Given a desired size (height, width) it produces a test image using @ippiImageJaehne_32f_C1R@.
jaehne32f :: Size -> IO ImageFloat
jaehne32f s = do
    w <- img I32f s
    ippiImageJaehne_32f_C1R // dst w (fullroi w) // checkIPP "ippiImageJaehne_32f_C1R" [w]
    return (F w)

-- | Given an image I32f, computes the first and second order derivatives (gx,gy,gxx,gyy,gxy).
secondOrder :: ImageFloat -> IO (ImageFloat,ImageFloat,ImageFloat,ImageFloat,ImageFloat)
secondOrder image = do
    gx  <- sobelVert image
    gy  <- sobelHoriz image
    gxx <- sobelVert gx
    gyy <- sobelHoriz gy
    gxy <- sobelHoriz gx
    return (gx,gy,gxx,gyy,gxy)    

-- | Obtains the determinant of the hessian operator from the 'secondOrder' derivatives.
hessian :: (ImageFloat,ImageFloat,ImageFloat,ImageFloat,ImageFloat) -> IO ImageFloat
hessian (gx,gy,gxx,gyy,gxy) = do
    ab <- gxx |*| gyy
    cc <- gxy |*| gxy
    h  <- ab  |-| cc
    return h

-- | Repeats an action a given number of times. For example, @(3 `times` fun) x = fun x >>= fun >>= fun@
times :: (Monad m, Num a1) => a1 -> (a -> m a) -> a -> m a
times 0 f = return
times n f = g where
    g x = do
        v <- f x >>= times (n-1) f
        return v

-- | Returns the minimum and maximum value in an image32f
minmax :: ImageFloat -> IO (Float,Float)
minmax (F im) = do
    mn <- malloc 
    mx <- malloc
    (ippiMinMax_32f_C1R // dst im (vroi im)) mn mx // checkIPP "minmax" [im]
    a <- peek mn
    b <- peek mx
    free mn
    free mx
    return (a,b)

warpOn' h (F r) (F im) = do
    coefs <- newArray (concat h)
    let Size h w = isize im
    warpPerspective32f (ptr im) (step im) h w
                           (r1 (vroi im)) (r2 (vroi im)) (c1 (vroi im)) (c2 (vroi im))
                           (ptr r) (step r)
                           (r1 (vroi r)) (r2 (vroi r)) (c1 (vroi r)) (c2 (vroi r))
                           coefs inter_LINEAR //warningIPP "warpOn" [im]
    free coefs

warp' s h im = do
    r' <- img I32f s
    let r = F r'
    set32f 0.0 r (fullroi r')
    warpOn' h r im
    return r


adapt dst h src = toLists $ inv (pixelToPointTrans (size dst)) <> h <> pixelToPointTrans (size src)

-- | Apply a homography (defined on normalized points, see 'pixelsToPoints') to an image.
warp :: Size              -- ^ desired size of the result
     -> Matrix Double     -- ^ homography
     -> ImageFloat        -- ^ source image
     -> IO ImageFloat     -- ^ result
warp s h im = do
    r' <- img I32f s
    let r = F r'
    set32f 0.0 r (fullroi r')
    warpOn h r im
    return r

-- | The same as 'warp', but the result is written over a preexisting image.
warpOn :: Matrix Double   -- ^ homography
       -> ImageFloat      -- ^ destination image
       -> ImageFloat      -- ^ source image
       -> IO ()
warpOn h r im = warpOn' (adapt r h im) r im


inter_NN         =  1 :: Int  
inter_LINEAR     =  2 :: Int  
inter_CUBIC      =  4 :: Int
inter_SUPER      =  8 :: Int
inter_LANCZOS    = 16 :: Int
--inter_SMOOTH_EDGE = (1 << 31) :: Int

-- | Explores an image and returns a list of pixels (as [row,column]) where the image is greater than 0.0.
getPoints32f :: Int -> ImageFloat -> IO [Pixel]
getPoints32f mx (F im) = do
    r <- mallocArray (2*mx)
    ptot <- malloc
    ok <- c_getPoints32f (castPtr (ptr im)) (step im) 
                   (r1 (vroi im)) (r2 (vroi im)) (c1 (vroi im)) (c2 (vroi im))
                   mx ptot r
    tot <- peek ptot
    hp <- peekArray tot r
    free ptot
    free r
    return (partitPixel hp)

-- | Partitions a list into a list of lists of a given length.
partit :: Int -> [a] -> [[a]]
partit _ [] = []
partit n l  = take n l : partit n (drop n l)

partitPixel :: [Int] -> [Pixel]
partitPixel [] = []
partitPixel [a] = error "partitPixel on a list with odd number of entries"
partitPixel (r:c:l) = Pixel r c : partitPixel l

----------------------------------------------------------
-- TO DO: parameters with a record

-- | Returns a list of interest points in the image (as unnormalized [x,y]). They are the local minimum of the determinant of the hessian (saddlepoints).
getCorners :: Int       -- ^ degree of smoothing (e.g. 1)
           -> Int       -- ^ radius of the localmin filter (e.g. 7)
           -> Float     -- ^ fraction of the maximum response allowed (e.g. 0.1)
           -> Int       -- ^ maximum number of interest points
           -> ImageFloat  -- ^ source image
           -> IO [Pixel]  -- ^ result
getCorners smooth rad prop maxn im = do
    let suaviza = smooth `times` gauss Mask5x5
    h <- suaviza im >>= secondOrder >>= hessian >>= scale32f (-1.0)
    (mn,mx) <- minmax h
    hotPoints <- localMax rad h
              >>= thresholdVal32f (mx*prop) 0.0 IppCmpLess
              >>= getPoints32f maxn
    return hotPoints

--------------------------------------------------------------------

genResize32f dst droi im sroi interp = do
    c_resize32f (ptr im) (step im) (height $ isize im) (width $ isize im)
                 (r1 sroi) (r2 sroi) (c1 sroi) (c2 sroi)
                 (ptr dst) (step dst)
                 (r1 droi) (r2 droi) (c1 droi) (c2 droi)
                 interp // checkIPP "genResize32f" [im]

-- | Resizes the roi of a given image.
resize32f :: Size -> ImageFloat -> IO ImageFloat
resize32f s (F im) = do
    r <- img I32f s
    genResize32f r (fullroi r) im (vroi im) inter_LINEAR
    return (F r)
