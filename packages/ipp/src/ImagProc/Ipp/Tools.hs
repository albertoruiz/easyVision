-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Tools
Copyright   :  (c) Alberto Ruiz 2006-13
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

-}
-----------------------------------------------------------------------------

module ImagProc.Ipp.Tools (
    --binarize8u, binarize32f, autoBinarize,
    localMax,
    Grads(..), gradients,
    --canny,
    hessian,
    --getCorners,
    gaussS, gaussS',
    rotateROI,
    pyramid,
    resizeIfGT
) where

import ImagProc.Ipp.Core
import ImagProc.Ipp.AdHoc
import ImagProc.Ipp.Pure
import ImagProc.Ipp.Generic
import Util.Geometry
import Data.List(transpose)
import Util.Rotation(rot3)
import Util.Homogeneous(desp)
import Numeric.LinearAlgebra hiding (constant)

{-
-- | Binarizes a gray level image.
binarize8u :: CUChar    -- ^ threshold
           -> ImageGray -- ^ image source
           -> ImageGray -- ^ result: higher values -> 255, lower values -> 0
binarize8u th = thresholdVal8u (th-1) 255 IppCmpGreater . thresholdVal8u th 0 IppCmpLess

autoBinarize :: ImageGray -> ImageGray
autoBinarize x = binarize8u (otsuThreshold x) x

-- | Binarizes a float image.
binarize32f :: Float    -- ^ threshold
           -> ImageFloat -- ^ image source
           -> ImageFloat -- ^ result: higher values -> 1, lower values -> 0
binarize32f th = thresholdVal32f th 0 IppCmpLess . thresholdVal32f th 1 IppCmpGreater

-}
                  
-- | Nonmaximum supression. Given an I32f image returns a copy of the input image with all the pixels which are not local maxima set to 0.0.
localMax :: Int           -- ^ radius of the 'filterMax'
         -> Image Float   -- ^ input image
         -> Image Float   -- ^ result
localMax r g = copyMask32f b g mask where
    mg = filterMax r g
    mask = compare32f IppCmpEq mg g
    b = setROI (roi mg) (constant 0 (size g))




data Grads = Grads { gm, gx, gy, gxx, gyy, gxy :: ImageFloat }

-- | convenience function for frequently used image gradients
gradients :: ImageFloat -> Grads
gradients image = Grads {gm = m, gx = x, gy = y, gxx = xx, gyy = yy, gxy = xy }
    where x  = sobelVert image
          y  = sobelHoriz image
          xx = sobelVert x
          yy = sobelHoriz y
          xy = sobelHoriz x
          m = sqrt32f $ x |*| x |+| y |*| y

-- | Obtains the determinant of the hessian operator
hessian :: Grads -> ImageFloat
hessian g = gxx g |*| gyy g |-| gxy g |*| gxy g

-----------------------------------------------------------------

{-
-- TO DO: parameters with a record

-- | Returns a list of interest points in the image (as unnormalized [x,y]). They are the local minimum of the determinant of the hessian (saddlepoints).
getCorners :: Int       -- ^ degree of smoothing (e.g. 1)
           -> Int       -- ^ radius of the localmin filter (e.g. 3)
           -> Float     -- ^ fraction of the maximum response allowed (e.g. 0.1)
           -> Int       -- ^ maximum number of interest points
           -> ImageFloat  -- ^ source image
           -> [Pixel]    -- ^ result
getCorners smooth rad prop maxn im = hotPoints where
    suaviza x = iterate (gauss Mask5x5) x !! smooth
    h = (-1.0) .* hessian (gradients (suaviza im))
    (mn,mx) = minmax h
    hotPoints = getPoints32f maxn
              $ thresholdVal32f (mx*prop) 0.0 IppCmpLess
              $ localMax rad h
-}
--------------------------------------------------------------------

-- | A version of gaussS with controlable precision. 
gaussS' :: Float      -- ^ mask radius (in sigma units)
        -> Float      -- ^ sigma
        -> ImageFloat -- ^ source image
        -> ImageFloat -- ^ result
gaussS' ext s = convolution mask . convolution (transpose mask)
    where mask = nor [map (fg s) [-k .. k]] where k = fromIntegral (ceiling (ext*s))
          fg s x = exp (-0.5* (x/s)^2)
          nor m = map (map (/s)) m where s = sum (concat m)

-- | Gaussian filter of given sigma (implemented as two 1-d filters). Mask radius is 3 sigma (@gaussS = gaussS' 3@).
gaussS :: Float -> ImageFloat -> ImageFloat
gaussS s | s > 0     = gaussS' 3 s
         | otherwise = id

--------------------------------------------------------------------

{-

-- | Canny's edge detector.
canny :: (Float,Float) -- ^ low and high threshold
      -> Grads         -- ^ image gradient
      -> ImageGray     -- ^ resulting image
canny th g = canny32f ((-1) .* gx g, gy g) th

-}

--------------------------------------------------------------------

-- | rotates a roi a given angle. 
--   (A slightly larger roi (3/2) is actually rotated to ensure that the whole final region has valid data,
--   but this cannot be guaranteed if the desired roi is too close to the valid roi of the source.)

rotateROI angle roi im = im' where
    sz = size im
    p = roiCenter $ roi
    rad = roiRadius roi
    [Point x y] = pixelsToPoints sz [p]
    roi2 = roiFromPixel (3*rad`div`2) p
    f2 = modifyROI (const roi2)
    f1 = modifyROI (const roi)
    r  = desp (x,y) <> rot3 (-angle) <> desp (-x,-y)
    im' = f1 $ warp zeroP sz r (f2 im)

----------------------------------------------------------------------

pyramid x = zipWith g (iterate decimate x) [0..]
    where g x k = modifyROI (shrink (k,k)) x

decimate x = resizeFull sz2 . gauss8u Mask3x3 $  x
    where (Size h w) = size x
          sz2 = (Size (h `div` 2) (w `div` 2))

----------------------------------------------------------------------

-- ^ resize image preserving aspect ratio if any dimension is greater than given size
resizeIfGT sz x = if (size x) == sz' then x else resize sz' x
  where
    sz' = limitSize sz (size x)

