-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.InterestPoints
Copyright   :  (c) Alberto Ruiz 2008
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  ghc

Scale and rotation invariant interest points.

-}
-----------------------------------------------------------------------------

module ImagProc.InterestPoints (
    DetailedInterestPoint(..),
    getSigmas,
    fullHessian,
    surf, usurf,
    -- * devel tools
    ExtPyr(..), mkExtPyr,
    multiscalePoints
)
where

import ImagProc.Images
import ImagProc.Ipp.Pure
import ImagProc.ImageProcessing
import Numeric.LinearAlgebra hiding ((.*))
import ImagProc.ScaleSpace
import ImagProc.Descriptors


data DetailedInterestPoint = DIP {
    ipRawPosition :: Pixel,
    ipRawScale    :: Int,
    ip            :: InterestPoint,
    ipHistoOris   :: Vector Double,
    ipPatch       :: ImageFloat }

type Grads = (ImageFloat,ImageFloat,ImageFloat,ImageFloat,ImageFloat)

data ExtPyr = EP {
    pyrImg :: ImageFloat,
    pyrGrad :: Grads,
    pyrAbsGrad :: ImageFloat,
    pyrResp :: (ImageFloat,Float) }


mkHessP = mkExtPyr k fun where
    k sigma = sigma^2/10
    fun = sqrt32f
        . thresholdVal32f 0 0 IppCmpLess
        . hessian

mkExtPyr k fun img sigma =
    EP { pyrImg =  smoothed,
         pyrGrad = grads,
         pyrAbsGrad = sqrt32f $ gx |*| gx |+| gy |*| gy,
         pyrResp = (resp,sigma) }
    where
        smoothed = k sigma .* gaussS sigma img
        grads@(gx,gy,_,_,_) = secondOrder smoothed
        resp     = fun grads

-- | specific method to obtain invariant descriptors of image regions
type Descriptor = ExtPyr -> (Pixel,Int) -> DetailedInterestPoint

-- | This is a reference implementation of the determinant of Hessian method without any
-- optimization for speed (although it is not too slow...)
fullHessian :: Descriptor
            -> [Float] -- ^ desired scales
            -> Int     -- ^ maximum number of points per level (e.g. 100)
            -> Float   -- ^ response threshold
            -> ImageFloat -- ^ Input image
            -> [DetailedInterestPoint]
fullHessian d s n h = fst . multiscalePoints mkHessP d s n h


multiscalePoints resp descript sigmas nmax h imr = (feats,pyr)
    where responses = map (resp imr) sigmas
          (pts,pyr) = getLocalMaxima nmax h (map pyrResp responses)
          feats = concat $ reverse $ zipWith f (tail responses) pts
             where f a bs = map (descript a) bs


----------------------------------------------------------------------------------

-- | scale-only invariant version of 'surf'.
usurf :: Int -- ^ size of region in scale units (e.g. 2 or 3)
      -> Int -- ^ number of rows and columns in the grid (e.g. 3 or 4)
      -> Descriptor
usurf rad grid EP { pyrImg = im,
                    pyrAbsGrad = ga,
                    pyrGrad = (gx,gy,_,_,_) 
                   }
               x@(p,n) = r where
    r = DIP { ipRawPosition = p,
              ipRawScale    = n,
              ip            = pt,
              ipPatch       = patch,
              ipHistoOris   = oris }
    pt = IP { ipPosition = head $ pixelsToPoints (size ga) [p],
              ipScale = fromIntegral n / w2,
              ipOrientation = angle,
              ipDescriptor = feat }
    roi = roiFromPixel (rad*n) p
    f = modifyROI (const roi)
    oris = histodir (f ga) (f gx) (f gy)
    angle = head $ angles oris
    feat = usurfRaw grid (gx,gy) roi
    Size _ w = size ga
    w2 = fromIntegral w / 2
    patch = f im


-- | Scale and rotation invariant descriptor of Bay, Tuytelaars and Van Gool, ECCV 2006. (draft)
surf :: Int -- ^ size of region in scale units (e.g. 2 or 3)
     -> Int -- ^ number of rows and columns in the grid (e.g. 3 or 4)
     -> Descriptor
surf rad grid EP { pyrImg = im,
                   pyrAbsGrad = ga,
                   pyrGrad = (gx,gy,_,_,_) 
                   }
               x@(p,n) = r where
    r = DIP { ipRawPosition = p,
              ipRawScale    = n,
              ip            = pt,
              ipPatch       = patch,
              ipHistoOris   = oris }
    pt = IP { ipPosition = head $ pixelsToPoints (size ga) [p],
              ipScale = fromIntegral n / w2,
              ipOrientation = angle,
              ipDescriptor = feat }
    roi = roiFromPixel (rad*n) p
    f = modifyROI (const roi)
    oris = histodir (f ga) (f gx) (f gy)
    angle = head $ angles oris
    feat = usurfRaw grid (gx',gy') roi
        where (gx',gy',_,_,_) = secondOrder patch
    Size _ w = size ga
    w2 = fromIntegral w / 2
    patch = rotateROI angle roi im

