{-# OPTIONS  #-}
-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.ImageProcessing
Copyright   :  (c) Alberto Ruiz 2006/8
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  requires IPP

Image processing functions as pure functions

-}
-----------------------------------------------------------------------------

module ImagProc.ImageProcessing (
    module ImagProc.Ipp.Structs,
    module ImagProc.Ipp.AdHoc,
    module ImagProc.Ipp.Pure,
    module ImagProc.Generic,
    binarize8u, localMax,
    secondOrder, hessian,
    getCorners
) where

import ImagProc.Ipp.Core
import ImagProc.Ipp.AdHoc
import ImagProc.Ipp.Pure
import ImagProc.Ipp.Structs
import ImagProc.Generic
import ImagProc.C.Simple


-- | Binarizes an image.
binarize8u :: CUChar -- ^ threshold
           -> Bool   -- ^ True = higher values -> 255, False = higher values -> 0
           -> ImageGray -- ^ image source
           -> ImageGray
binarize8u th True = thresholdVal8u (th-1) 255 IppCmpGreater . thresholdVal8u th 0 IppCmpLess
binarize8u th False = notI . binarize8u th True


-- | Nonmaximum supression. Given an I32f image returns a copy of the input image with all the pixels which are not local maxima set to 0.0.
localMax :: Int         -- ^ diameter of the filterMax32f
         -> ImageFloat  -- ^ input image
         -> ImageFloat   -- ^ result
localMax d g = copyMask32f g mask where
    mg = filterMax32f d g
    mask = compare32f IppCmpEq mg g

-- | Given an image I32f, computes the first and second order derivatives (gx,gy,gxx,gyy,gxy).
secondOrder :: ImageFloat -> (ImageFloat,ImageFloat,ImageFloat,ImageFloat,ImageFloat)
secondOrder image = (gx,gy,gxx,gyy,gxy) where
    gx  = sobelVert image
    gy  = sobelHoriz image
    gxx = sobelVert gx
    gyy = sobelHoriz gy
    gxy = sobelHoriz gx

-- | Obtains the determinant of the hessian operator from the 'secondOrder' derivatives.
hessian :: (ImageFloat,ImageFloat,ImageFloat,ImageFloat,ImageFloat) -> ImageFloat
hessian (_,_,gxx,gyy,gxy) = gxx |*| gyy |-| gxy |*| gxy

-----------------------------------------------------------------
-- TO DO: parameters with a record

-- | Returns a list of interest points in the image (as unnormalized [x,y]). They are the local minimum of the determinant of the hessian (saddlepoints).
getCorners :: Int       -- ^ degree of smoothing (e.g. 1)
           -> Int       -- ^ radius of the localmin filter (e.g. 7)
           -> Float     -- ^ fraction of the maximum response allowed (e.g. 0.1)
           -> Int       -- ^ maximum number of interest points
           -> ImageFloat  -- ^ source image
           -> [Pixel]    -- ^ result
getCorners smooth rad prop maxn im = hotPoints where
    suaviza x = iterate (gauss Mask5x5) x !! smooth
    h = (-1.0) .* hessian (secondOrder (suaviza im))
    (mn,mx) = minmax h
    hotPoints = getPoints32f maxn
              $ thresholdVal32f (mx*prop) 0.0 IppCmpLess
              $ localMax rad h

