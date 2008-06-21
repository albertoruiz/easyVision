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
    drawHisto,
    surfDesc
)
where

import ImagProc.Ipp.Core
import ImagProc.ImageFold
import Numeric.LinearAlgebra
import Graphics.UI.GLUT
import Data.Packed.ST
import EasyVision.Draw
import ImagProc.ImageProcessing hiding ((.*))
import Vision(unitary)
import Debug.Trace
import GHC.Float(float2Double,double2Float)

debug x = trace (show x) x

captureDirs !p0 !p1 !p2 !k !s =
    if (uval p0 k > 1)
        then atan2 (uval p1 k) (uval p2 k) : s
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
    let inc k = modifyVector h k (+1)
        addDir !p0 !p1 !p2 !k = modifyVector h (getBin 36 $ atan2 (uval p1 k) (uval p2 k)) (+ float2Double (uval p0 k))
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

--------------------------------------------------------------------------

-- | representation of the elements of a vector as vertical bars from a starting position
drawHisto :: Int -- ^ column
          -> Int -- ^ row
          -> Vector Double -- ^ input vector
          -> IO ()
drawHisto x y v = do
    let f k = vertex (Vertex2 x1 y1) >> vertex (Vertex2 x2 y2)
            where x1 = fromIntegral (x+k) :: GLint
                  y1 = fromIntegral y
                  x2 = x1
                  y2 = y1 + round (-v@>k)
    renderPrimitive Lines $ do
        mapM_ f [0..dim v -1]
        vertex (Pixel y x) >> vertex (Pixel y (x+dim v -1))

--------------------------------------------------------------------------

-- | the SURF descriptor
surfDesc :: Int -- ^ subdivisions (e.g. 3 or 4)
         -> (ImageFloat,ImageFloat) -- ^ gx,gy
         -> ROI
         -> Vector Double
surfDesc n (gx,gy) roi = v where
    v = unitary $ fromList $ concat $ vsx++vsy
    rois = roiGrid n n roi
    mapr f rois im = map g rois where g r = f (modifyROI (const r) im)
    vsx = mapr sd rois gx
    vsy = mapr sd rois gy
    sd im = [sum32f im, sum32f (abs32f im)]
