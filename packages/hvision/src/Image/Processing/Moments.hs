{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
{- |
Module      :  Image.Processing.Moments
Copyright   :  (c) Alberto Ruiz 2010
License     :  GPL

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

-}
-----------------------------------------------------------------------------

module Image.Processing.Moments (
    ImageBasis(..),
    imageBasis,
    momentsImage
) where

import Image.Processing.IPP
import Image
import Numeric.LinearAlgebra.HMatrix

data ImageBasis = ImageBasis { zeroIb, oneIb, xIb, yIb, x2Ib, y2Ib, xyIb :: Image Float }

imageBasis :: Size -> ImageBasis
imageBasis (Size r c) = ImageBasis {
    zeroIb = mat2img $ konst 0 (r,c),
    oneIb = mat2img $ konst 1 (r,c),
    xIb = xI,
    yIb = yI,
    x2Ib = xI |*| xI,
    y2Ib = yI |*| yI,
    xyIb = xI |*| yI }
  where
    xcoord = linspace c (1,-1::Double)
    ar = fromIntegral r / fromIntegral c
    ycoord = linspace r (ar,-ar::Double)
    xI = mat2img $ single $ fromRows (replicate r xcoord)
    yI = mat2img $ single $ fromColumns (replicate c ycoord)

-- | (mx,my,cxx,cyy,cxy)
momentsImage :: ImageBasis -> Image Float -> (Double,Double,Double,Double,Double)
momentsImage ImageBasis {..} g = (mx,my,cxx,cyy,cxy)
  where
    s = sum32f g
    sx = sum32f (g |*| xIb)
    sy = sum32f (g |*| yIb)
    sx2 = sum32f (g |*| x2Ib)
    sy2 = sum32f (g |*| y2Ib)
    sxy = sum32f (g |*| xyIb)
    mx = sx/s
    my = sy/s
    cxx = sx2/s - mx*mx
    cyy = sy2/s - my*my
    cxy = sxy/s - mx*my


