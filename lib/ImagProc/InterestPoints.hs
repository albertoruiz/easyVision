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
    DetailedInterestPoint(..), ExtPyr(..),
    getSigmas,
    fullHessian,
    surf, usurf
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


mkHessP img sigma =
    EP { pyrImg =  smoothed,
         pyrGrad = grads,
         pyrAbsGrad = abs32f gx |+| abs32f gy,
         pyrResp = (resp,sigma) }
    where
        smoothed = k .* gaussS sigma img
        k = sigma^2/10
        grads@(gx,gy,_,_,_) = secondOrder smoothed
        resp     = fun grads
        fun = sqrt32f
            . thresholdVal32f 0 0 IppCmpLess
            . hessian

-- | specific method to obtain invariant descriptors of image regions
type Descriptor = ExtPyr -> (Pixel,Int) -> DetailedInterestPoint

-- | This is a reference implementation of the determinant of Hessian method without any
-- optimization for speed (although it is not too slow...)
fullHessian :: [Float] -- ^ desired scales
            -> Int     -- ^ maximum number of points per level (e.g. 100)
            -> Float   -- ^ response threshold
            -> Descriptor
            -> ImageFloat -- ^ Input image
            -> [DetailedInterestPoint]
fullHessian sigmas nmax h descript imr = feats
    where responses = map (mkHessP imr) sigmas
          (pts,pyr) = getLocalMaxima nmax h (map pyrResp responses)
          feats = concat $ reverse $ zipWith f (tail responses) pts
             where f a bs = map (descript a) bs

----------------------------------------------------------------------------------

-- | scale only invariant version of 'surf'.
usurf :: ExtPyr -> (Pixel,Int) -> DetailedInterestPoint
usurf EP { pyrImg = im,
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
    roi = roiFromPixel (2*n) p
    f = modifyROI (const roi)
    oris = histodir (f ga) (f gx) (f gy)
    angle = head $ angles oris
    feat = usurfRaw 3 (gx,gy) roi
    Size _ w = size ga
    w2 = fromIntegral w / 2
    patch = f im


-- | Scale and rotation invariant descriptor of Bay, Tuytelaars and Van Gool, ECCV 2006.
surf :: ExtPyr -> (Pixel,Int) -> DetailedInterestPoint
surf EP { pyrImg = im,
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
    roi = roiFromPixel (2*n) p
    f = modifyROI (const roi)
    oris = histodir (f ga) (f gx) (f gy)
    angle = head $ angles oris
    feat = surfRaw angle 3 im roi
    Size _ w = size ga
    w2 = fromIntegral w / 2
    patch = normRot angle roi im
