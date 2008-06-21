-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.ROI
Copyright   :  (c) Alberto Ruiz 2006-8
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  provisional

Manipulation of regions of interest.

-}
-----------------------------------------------------------------------------

module ImagProc.ROI
( roiSize,
  shrink,
  shift,
  intersection,
  union,
  roiArea,
  inROI
) where

import ImagProc.Base

-- | Size of a ROI
roiSize :: ROI -> Size
roiSize ROI { r1=a, r2=b, c1=x, c2=y} = Size { height = b-a+1, width = y-x+1 }

-- | Creates a new roi by reducing in (r,c) units the rows and columns or a given roi. If r or c are negative the roi expands.
shrink :: (Int,Int)  -> ROI -> ROI
shrink (r,c) roi =
    ROI {r1=(r1 roi) +r, 
         r2=(r2 roi) -r,
         c1=(c1 roi) +c,
         c2=(c2 roi) -c}

-- | Creates a new roi by moving (r,c) units the position of a given roi.
shift :: (Int,Int)  -> ROI -> ROI
shift (r,c) roi =
    ROI {r1=(r1 roi) +r, 
         r2=(r2 roi) +r,
         c1=(c1 roi) +c,
         c2=(c2 roi) +c}

-- | Creates a new roi as the intersection of two given roi's.
intersection :: ROI -> ROI -> ROI
intersection a b = ROI { r1 = max (r1 a) (r1 b)
                       , r2 = min (r2 a) (r2 b)
                       , c1 = max (c1 a) (c1 b)
                       , c2 = min (c2 a) (c2 b)
                       }

-- | obtains the minimum ROI which contains the arguments
union :: ROI -> ROI -> ROI
union a b = ROI { r1 = min (r1 a) (r1 b)
                , r2 = max (r2 a) (r2 b)
                , c1 = min (c1 a) (c1 b)
                , c2 = max (c2 a) (c2 b)
                }

-- | 'ROI'\'s area in pixels
roiArea :: ROI -> Int
roiArea (ROI r1 r2 c1 c2) = w*h where
    w = c2-c1+1
    h = r2-r1+1

-- | checks that a pixel is in a ROI
inROI :: ROI -> Pixel -> Bool
inROI (ROI r1 r2 c1 c2) (Pixel r c) = r1 <= r && r <= r2 && c1 <= c && c <= c2
