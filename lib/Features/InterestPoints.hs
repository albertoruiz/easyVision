-----------------------------------------------------------------------------
{- |
Module      :  Features.InterestPoints
Copyright   :  (c) Alberto Ruiz 2008
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional
Portability :  ghc

Scale and rotation invariant interest points.

-}
-----------------------------------------------------------------------------

module Features.InterestPoints (
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
import Features.ScaleSpace
import Features.Descriptors
import GHC.Float(float2Double)

data DetailedInterestPoint = DIP {
    ipRawPosition :: Pixel,
    ipRawScale    :: Int,
    ip            :: InterestPoint,
    ipHistoOris   :: Vector Double,
    ipPatch       :: ImageFloat }

data ExtPyr = EP {
    pyrImg :: ImageFloat,
    pyrGrad :: Grads,
    pyrResp :: (ImageFloat,Float) }


mkHessP = mkExtPyr k fun where
    k sigma = sigma^2/10
    fun = sqrt32f
        . thresholdVal32f 0 0 IppCmpLess
        . hessian

mkExtPyr k fun img sigma =
    EP { pyrImg =  smoothed,
         pyrGrad = grads,
         pyrResp = (resp,sigma) }
    where
        smoothed = k sigma .* gaussS sigma img
        grads    = gradients smoothed
        resp     = fun grads

-- | specific method to obtain invariant descriptors of image regions
type Descriptor = ExtPyr -> (Pixel,Float) -> DetailedInterestPoint

-- | This is a reference implementation of the determinant of Hessian method without any
-- optimization for speed (although it is not too slow...)
fullHessian :: Descriptor
            -> [Float] -- ^ desired scales
            -> Int     -- ^ maximum number of points per level (e.g. 100)
            -> Float   -- ^ response threshold
            -> ImageFloat -- ^ Input image
            -> [DetailedInterestPoint]
fullHessian d s n h = concat . reverse . fst . multiscalePoints mkHessP d s n h


multiscalePoints resp descript sigmas nmax h imr = (feats,pyr)
    where responses = map (resp imr) sigmas
          (pts,pyr) = getLocalMaxima nmax h (map pyrResp responses)
          feats = zipWith f (tail responses) pts
             where f a bs = map (descript a) bs

----------------------------------------------------------------------------------

-- | scale-only invariant version of 'surf'.
usurf :: Float -- ^ size of region in scale units (e.g. 2 or 3)
      -> Int -- ^ number of rows and columns in the grid (e.g. 3 or 4)
      -> Descriptor
usurf rad grid EP { pyrImg = im,
                    pyrGrad = Grads {gm = dm, gx = dx, gy = dy}
                   }
               x@(p,s) = r where
    r = DIP { ipRawPosition = p,
              ipRawScale    = round s,
              ip            = pt,
              ipPatch       = patch,
              ipHistoOris   = oris }
    pt = IP { ipPosition = head $ pixelsToPoints (size im) [p],
              ipScale = float2Double $ 2 * s / fromIntegral (width (size im)),
              ipOrientation = angle,
              ipDescriptor = feat }
    roi = roiFromPixel (round (rad*s)) p
    f = modifyROI (const roi)
    oris = (slavmat <>) $ fromList $ histoDir (f dm) (f dx) (f dy) (float2Double $ rad*s/2) (Pixel 0 0) 36
           -- histodir (f dm) (f dx) (f dy)
    angle = head $ angles oris
    feat = usurfRaw grid (dx,dy) roi
    patch = f im


-- | Scale and rotation invariant descriptor of Bay, Tuytelaars and Van Gool, ECCV 2006. (draft)
surf :: Float -- ^ size of region in scale units (e.g. 2 or 3)
     -> Int -- ^ number of rows and columns in the grid (e.g. 3 or 4)
     -> Descriptor
surf rad grid EP { pyrImg = im,
                   pyrGrad = Grads {gm = dm, gx = dx, gy = dy}
                   }
               x@(p,s) = r where
    r = DIP { ipRawPosition = p,
              ipRawScale    = round s,
              ip            = pt,
              ipPatch       = patch,
              ipHistoOris   = oris }
    pt = IP { ipPosition = head $ pixelsToPoints (size im) [p],
              ipScale = float2Double $ 2 * s / fromIntegral (width (size im)),
              ipOrientation = angle,
              ipDescriptor = feat }
    roi = roiFromPixel (round (rad*s)) p
    f = modifyROI (const roi)
    oris = (slavmat <>) $ fromList $ histoDir (f dm) (f dx) (f dy) (float2Double $ rad*s/2) p 36
           -- histodir (f dm) (f dx) (f dy)
    angle = head $ angles oris
    feat = usurfRaw grid (dx',dy') roi
        where Grads {gx = dx', gy = dy'} = gradients patch
    patch = rotateROI angle roi im


