-----------------------------------------------------------------------------
{- |
Module      :  Vision.Ipp.Typical
Copyright   :  (c) Alberto Ruiz 2006
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

High level access to some IPP functions.

-}
-----------------------------------------------------------------------------

module Ipp.Typical (
-- * IPP auxiliary structures
  Mask(..)
, IppCmp(..)
-- * Utilities
, jaehne32f
, set32f
, copyROI32f
, times
, partit
, pixelToPoint, pixelToPointTrans
-- * Image manipulation
, scale8u32f
, copy32f
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
import Vision hiding ((|-|),(|+|))
import GSL

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
set32f :: Float    -- ^ desired value
       -> Img      -- ^ input image
       -> ROI      -- ^ roi
       -> IO ()
set32f v im roi = ippiSet_32f_C1R v // dst im roi // checkIPP "set32f" [im]

-- | Creates a 32f image from an 8u image.
scale8u32f :: Float       -- ^ desired value corresponding to 0
           -> Float       -- ^ desired value corresponding to 255
           -> Img         -- ^ input image
           -> IO Img      -- ^ result
scale8u32f vmin vmax im = do
    r' <- img I32f (height im) (width im)
    let r = r' {vroi = vroi im}
    (cr1 ippiScale_8u32f_C1R im r) vmin vmax // checkIPP "scale8u32f" [im]
    return r

-- | Copies the roi of the input image into the destination image.
copyROI32f :: Img -> Img -> IO ()
copyROI32f r im = ippiCopy_32f_C1R // src im (vroi im) // dst r (vroi im) // checkIPP "copyROI32f" [im]

simplefun1 ippfun roifun msg = g where
    g im = do
        r <- imgAsR1 roifun im
        cr1 ippfun im r // checkIPP msg [im]
        return r 

-- | Creates a image of the same size as the source and copies its roi.
copy32f :: Img -> IO Img
copy32f = simplefun1 ippiCopy_32f_C1R id "copy32f"

-- | The result contains the absolute values of the pixels in the input image.
abs32f :: Img -> IO Img
abs32f  = simplefun1 ippiAbs_32f_C1R  id "abs32f"    

-- | The result contains the square roots of the pixels in the input image.
sqrt32f :: Img -> IO Img
sqrt32f = simplefun1 ippiSqrt_32f_C1R id "sqrt32f" 

-- | Applies a vertical Sobel filter (typically used for computing gradient images).
sobelVert :: Img -> IO Img
sobelVert = simplefun1 ippiFilterSobelVert_32f_C1R (shrink (1,1)) "sobelVert"

-- | Applies a horizontal Sobel filter (typically used for computing gradient images).
sobelHoriz :: Img -> IO Img
sobelHoriz = simplefun1 ippiFilterSobelHoriz_32f_C1R (shrink (1,1)) "sobelHoriz"

data Mask = Mask3x3 | Mask5x5
code Mask3x3 = 33
code Mask5x5 = 55

-- | Convolution with a gaussian mask of the desired size.
gauss :: Mask -> Img -> IO Img
gauss mask = simplefun1 f (shrink (s,s)) "gauss" where
    s = case mask of
                Mask3x3 -> 1
                Mask5x5 -> 2
    f ps ss pd sd r = ippiFilterGauss_32f_C1R ps ss pd sd r (code mask)

-- | The result is the source image in which the pixels verifing the comparation with a threshold are set to a desired value.
thresholdVal32f :: Float     -- ^ threshold
                -> Float     -- ^ value
                -> IppCmp    -- ^ comparison function
                -> Img       -- ^ source image
                -> IO Img    -- ^ result
thresholdVal32f t v cmp = simplefun1 f id "thresholdVal32f" where
    f ps ss pd sd r = ippiThreshold_Val_32f_C1R ps ss pd sd r t v (codeCmp cmp)

-- | Changes each pixel by the maximum value in its neighbourhood of given radius.
filterMax32f :: Int -> Img -> IO Img
filterMax32f sz = simplefun1 f (shrink (d,d)) "filterMax32f" where
    d = (sz-1) `quot` 2
    f ps ss pd sd r = ippiFilterMax_32f_C1R ps ss pd sd r (ippRect sz sz) (ippRect d d)

---------------------------------------------

simplefun2 ippfun roifun msg = g where
    g im1 im2 = do
        r <- imgAsR2 roifun im1 im2
        cr2 ippfun im1 im2 r // checkIPP msg [im1,im2]
        return r

infixl 7  |*|
infixl 6  |+|, |-|
-- | Pixel by pixel multiplication.
(|*|) :: Img -> Img -> IO Img
(|*|) = simplefun2 ippiMul_32f_C1R intersection "mul32f"

-- | Pixel by pixel addition.
(|+|) :: Img -> Img -> IO Img
(|+|) = simplefun2 ippiAdd_32f_C1R intersection "add32f"

-- | Pixel by pixel substraction.
(|-|) :: Img -> Img -> IO Img
(|-|) = flip $ simplefun2 ippiSub_32f_C1R intersection "sub32f" -- more natural argument order

-- | Multiplies the pixel values of an image by a given value.
scale32f :: Float -> Img -> IO Img
scale32f v = simplefun1 f id "mulC32f" where
    f ps ss pd sd r = ippiMulC_32f_C1R ps ss v pd sd r

codeCmp IppCmpLess      = 0
codeCmp IppCmpLessEq    = 1
codeCmp IppCmpEq        = 2
codeCmp IppCmpGreaterEq = 3
codeCmp IppCmpGreater   = 4

data IppCmp = IppCmpLess | IppCmpLessEq | IppCmpEq | IppCmpGreaterEq | IppCmpGreater

-- | The result is the pixelswise comparation of the two source images.
compare32f :: IppCmp -> Img -> Img -> IO Img
compare32f cmp im1 im2 = do
    r <- img Gray (height im1) (width im1)
    let roi = intersection (vroi im1) (vroi im2)
    (ippiCompare_32f_C1R // src im1 roi // src im2 roi // dst r roi) (codeCmp cmp) // checkIPP "compare32f" [im1,im2]
    return r {vroi = roi}

-- | Creates a copy of the source image only on corresponding pixels in which mask=255
copyMask32f :: Img    -- ^ source image
            -> Img    -- ^ mask image
            -> IO Img    -- ^ result
copyMask32f im mask = do
    r <- imgAs im
    let roi = intersection (vroi im) (vroi mask)
    set32f 0.0 r (fullroi r)
    ippiCopy_32f_C1MR // src im roi // dst r roi // src mask roi // checkIPP "copyMask32f" [im,mask]
    return $ r {vroi = roi}

-- | Nonmaximum supression. Given an I32f image returns a copy of the input image with all the pixels which are not local maxima set to 0.0.
localMax :: Int        -- ^ radius of the filterMax32f
         -> Img        -- ^ input image
         -> IO Img        -- ^ result
localMax r g = do
    mg   <- filterMax32f r g
    mask <- compare32f IppCmpEq mg g
    r    <- copyMask32f g mask
    return r

-- | Given a desired size (height, width) it produces a test image using @ippiImageJaehne_32f_C1R@.
jaehne32f :: (Int, Int) -> IO Img
jaehne32f (r,c) = do
    w <- img I32f r c
    ippiImageJaehne_32f_C1R // dst w (fullroi w) // checkIPP "ippiImageJaehne_32f_C1R" [w]
    return w

-- | Given an image I32f, computes the first and second order derivatives (gx,gy,gxx,gyy,gxy).
secondOrder :: Img -> IO (Img,Img,Img,Img,Img)
secondOrder image = do
    gx  <- sobelVert image
    gy  <- sobelHoriz image
    gxx <- sobelVert gx
    gyy <- sobelHoriz gy
    gxy <- sobelHoriz gx
    return (gx,gy,gxx,gyy,gxy)    

-- | Obtains the determinant of the hessian operator in an image32f
hessian :: Img -> IO Img
hessian image = do
    (gx,gy,gxx,gyy,gxy) <- secondOrder image
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
minmax :: Img -> IO (Float,Float)
minmax im = do
    mn <- malloc 
    mx <- malloc
    (ippiMinMax_32f_C1R // dst im (vroi im)) mn mx // checkIPP "minmax" [im]
    a <- peek mn
    b <- peek mx
    free mn
    free mx
    return (a,b)

warpOn' h r im = do
    coefs <- newArray (concat h)
    warpPerspective32f (ptr im) (step im) (height im) (width im)
                           (r1 (vroi im)) (r2 (vroi im)) (c1 (vroi im)) (c2 (vroi im))
                           (ptr r) (step r)
                           (r1 (vroi r)) (r2 (vroi r)) (c1 (vroi r)) (c2 (vroi r))
                           coefs inter_LINEAR //warningIPP "warpOn" [im]
    free coefs

warp' (height, width) h im = do
    r <- img I32f height width
    set32f 0.0 r (fullroi r)
    warpOn' h r im
    return r


adapt dst h src = toList $ inv (pixelToPointTrans dst) <> h <> pixelToPointTrans src

-- | Apply a homography (defined on normalized points, see 'pixelToPoint') to an image.
warp :: (Int,Int)         -- ^ desired size of the result (height,width)
     -> Matrix            -- ^ homography (GSL Matrix) 
     -> Img               -- ^ source image
     -> IO Img            -- ^ result
warp (height, width) h im = do
    r <- img I32f height width
    set32f 0.0 r (fullroi r)
    warpOn h r im
    return r

-- | The same as 'warp', but the result is written over a preexisting image.
warpOn :: Matrix          -- ^ homography (GSL Matrix)
       -> Img             -- ^ destination image 
       -> Img             -- ^ source image
       -> IO ()
warpOn h r im = warpOn' (adapt r h im) r im


inter_NN         =  1 :: Int  
inter_LINEAR     =  2 :: Int  
inter_CUBIC      =  4 :: Int
inter_SUPER      =  8 :: Int
inter_LANCZOS    = 16 :: Int
--inter_SMOOTH_EDGE = (1 << 31) :: Int

-- | Explores an image and returns a list of pixels (as [row,column]) where the image is greater than 0.0.
getPoints32f :: Int -> Img -> IO [[Int]]
getPoints32f mx im = do
    r <- mallocArray (2*mx)
    ptot <- malloc
    ok <- c_getPoints32f (castPtr (ptr im)) (step im) 
                   (r1 (vroi im)) (r2 (vroi im)) (c1 (vroi im)) (c2 (vroi im))
                   mx ptot r
    tot <- peek ptot
    hp <- peekArray tot r
    free ptot
    free r
    return (partit 2 hp)

-- | Partitions a list into a list of lists of a given length.
partit :: Int -> [a] -> [[a]]
partit _ [] = []
partit n l  = take n l : partit n (drop n l)

----------------------------------------------------------
-- TO DO: parameters with a record

-- | Returns a list of interest points in the image (as unnormalized [x,y]). They are the local minimum of the determinant of the hessian (saddlepoints).
getCorners :: Int       -- ^ degree of smoothing (e.g. 1)
           -> Int       -- ^ radius of the localmin filter (e.g. 7)
           -> Float     -- ^ fraction of the maximum response allowed (e.g. 0.1)
           -> Int       -- ^ maximum number of interest points
           -> Img       -- ^ source image (I32f or Gray)
           -> IO [[Double]]  -- ^ result
getCorners smooth rad prop maxn im@Img{itype = I32f} = do
    let suaviza = smooth `times` gauss Mask5x5
    h <- suaviza im >>= hessian >>= scale32f (-1.0)
    (mn,mx) <- minmax h
    hotPoints <- localMax rad h
              >>= thresholdVal32f (mx*prop) 0.0 IppCmpLess
              >>= getPoints32f maxn
    --return $ pixel2point im $ map (reverse . map fromIntegral) hotPoints
    return $ map (reverse . map fromIntegral) hotPoints

getCorners s r p mx im@Img{itype = Gray} =
    scale8u32f 0 1 im >>= getCorners s r p mx

------------------------------------------------------------------

-- | Auxiliary homogeneous transformation from pixels to points
pixelToPointTrans :: Img -> Matrix
pixelToPointTrans im = nor where
    w = fromIntegral (width im) -1
    h = fromIntegral (height im) -1
    r = (h+1)/(w+1)
    nor = realMatrix
        [[-2/w,      0, 1]
        ,[   0, -2*r/h, r]
        ,[   0,      0, 1]]

-- | Trasformation from pixels to normalized points.
pixelToPoint :: Img -> [[Double]]->[[Double]]
pixelToPoint im = fix where
    nor = pixelToPointTrans im
    fix = ht nor

--------------------------------------------------------------------

genResize32f dst droi im sroi interp = do
    c_resize32f (ptr im) (step im) (height im) (width im)
                 (r1 sroi) (r2 sroi) (c1 sroi) (c2 sroi)
                 (ptr dst) (step dst)
                 (r1 droi) (r2 droi) (c1 droi) (c2 droi)
                 interp // checkIPP "genResize32f" [im]

-- | Resizes the roi of a given image.
resize32f :: (Int,Int) -> Img -> IO Img
resize32f (h,w) im = do
    r <- img I32f h w
    genResize32f r (fullroi r) im (vroi im) inter_LINEAR
    return r
