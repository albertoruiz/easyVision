{-# OPTIONS -fbang-patterns #-}
-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Descriptors
Copyright   :  (c) Alberto Ruiz 2008
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  ghc

Interest point descriptors.

-}
-----------------------------------------------------------------------------

module ImagProc.Descriptors (
    histodir,
    angles,
    usurfRaw,
    surfRaw,
    normRot
)
where

import ImagProc.Ipp.Core
import ImagProc.ImageFold
import Numeric.LinearAlgebra
import Graphics.UI.GLUT hiding (Size,Point)
import Data.Packed.ST
import ImagProc.ImageProcessing hiding ((.*))
import Vision(unitary)
import Vision(rot3,scaling,desp,unitary)
import GHC.Float(float2Double,double2Float)

captureDirs !p0 !p1 !p2 !k !s =
    if (uval p0 k > 1)
        then atan2 (uval p2 k) (uval p1 k) : s
        else s
{-# INLINE captureDirs #-}

histo n ds = runSTVector $ do
    h <- newVector (0::Double) n -- number of bins
    let inc k = modifyVector h k (+1)
    mapM_ inc ds
    return h

-- computes the bin index (0..n-1) of an angle a
getBin :: Int -> Float -> Int
getBin n a = round ((fromIntegral n)*(pi + a)/2/pi) `rem` n

-------------------------------------------------------------------------

-- | histogram of gradient directions in the roi. Only positions with |g| > 1 are included.
histodir' :: ImageFloat -- ^ |g|
          -> ImageFloat -- ^ gx
          -> ImageFloat -- ^ gy
          -> Vector Double -- ^ normalized histogram
histodir' ga gx gy =  nor $ histo 36 $ map (getBin 36) $ foldImage3 captureDirs [] ga gx gy
    where nor = (k .*)
          k = recip . fromIntegral . validArea $ ga

--------------------------------------------------------------------------

-- | histogram of gradient directions in the roi, weighted by |ga|
histodir :: ImageFloat -- ^ |g|
         -> ImageFloat -- ^ gx
         -> ImageFloat -- ^ gy
         -> Vector Double -- ^ normalized histogram
histodir ga gx gy = (slavmat <>) $ (k .*) $ runSTVector $ do
    h <- newVector (0::Double) 36
    let addDir !p0 !p1 !p2 !k = modifyVector h (getBin 36 $ atan2 (uval p2 k) (uval p1 k)) (+ float2Double (uval p0 k))
        {-# INLINE addDir #-}
    traverseImage3 addDir ga gx gy
    return h
  where k = recip . fromIntegral . validArea $ ga


--     let inc k = modifyVector h k (+1)
--         addDir !p0 !p1 !p2 !k = if uval p0 k > 1
--                                       then inc $ getBin 36 $ atan2 (uval p1 k) (uval p2 k)
--                                       else return ()


rrotate as = last as : init as

l0 = [1,2]++replicate 33 0 ++[1]
slavmat = (fromLists $ take 36 $ iterate rrotate l0)/4

-------------------------------

-- | predominant angles in a histogram obtained by 'histodir'
angles :: Vector Double -> [Double]
angles oris = [10 * fromIntegral (1+vectorMaxIndex oris) * pi / 180]


--------------------------------------------------------------------------

-- | the SURF descriptor without rotation invariance
usurfRaw :: Int -- ^ subdivisions (e.g. 3 or 4)
         -> (ImageFloat,ImageFloat) -- ^ gx,gy
         -> ROI
         -> Vector Double
usurfRaw n (gx,gy) roi = v where
    v = unitary $ fromList $ concat $ vsx++vsy
    rois = roiGrid n n roi
    mapr f rois im = map g rois where g r = f (modifyROI (const r) im)
    vsx = mapr sd rois gx
    vsy = mapr sd rois gy
    sd im = [sum32f im, sum32f (abs32f im)]

-- | the SURF descriptor
surfRaw :: Double -- ^ angle
         -> Int -- ^ subdivisions (e.g. 3 or 4)
         -> ImageFloat -- ^ source
         -> ROI        -- ^ region of interest
         -> Vector Double
surfRaw angle n im roi = usurfRaw n (gx,gy) roi where
    (gx,gy,_,_,_) = secondOrder $ normRot angle roi im


normRot :: Double -> ROI -> ImageFloat -> ImageFloat
normRot angle roi im = im' where
    sz = size im
    p = roiCenter $ roi
    rad = roiRadius roi
    [Point x y] = pixelsToPoints sz [p]
    roi2 = roiFromPixel (3*rad`div`2) p
    f2 = modifyROI (const roi2)
    f1 = modifyROI (const roi)
    r  = desp (x,y) <> rot3 (-angle) <> desp (-x,-y)
    im' = f1 $ warp 0 sz r (f2 im)
