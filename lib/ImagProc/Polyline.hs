-----------------------------------------------------------------------------
{- |
Module      :  ImagProc.Polyline
Copyright   :  (c) Alberto Ruiz 2007
License     :  GPL-style

Maintainer  :  Alberto Ruiz (aruiz at um dot es)
Stability   :  very provisional
Portability :  hmm...

Some operations with polylines.

-}
-----------------------------------------------------------------------------

module ImagProc.Polyline (
    Polyline(..),
    perimeter,
    orientation,
    contour
)
where

import ImagProc.Images
import ImagProc.Ipp.Core
import Foreign.C.Types(CUChar)
import Debug.Trace

debug x = trace (show x) x

data Polyline = Closed [Point]
              | Open   [Point]

distPoints (Point a b) (Point x y) = (a-x)^2+(b-y)^2

-- | (for an open polyline is the length)
perimeter :: Polyline -> Double
perimeter (Open l) = perimeter' l
perimeter (Closed l) = perimeter' (last l:l)

perimeter' [_] = 0
perimeter' (a:b:rest) = distPoints a b + perimeter' (b:rest)

-- | Oriented area of a closed polyline. The clockwise sense is positive in the x-y world frame (\"floor\",z=0) and negative in the camera frame.
--
-- area = abs.orientation.
orientation :: Polyline -> Double
orientation (Open _) = error "undefined orientation of open polyline"
orientation (Closed l) = -0.5 * orientation' (last l:l)

orientation' [_] = 0
orientation' (Point x1 y1:r@(Point x2 y2:_)) = x1*y2-x2*y1 + orientation' r

--------------------------------------------------------------

data Dir = ToRight | ToLeft | ToDown | ToUp
nextPos :: ImageGray -> CUChar -> (Pixel,Dir) -> (Pixel,Dir)

nextPos im v (Pixel r c, ToRight) = case (a,b) of
    (False,False) -> (Pixel (r+1) c, ToDown)
    (False,True)  -> (Pixel r (c+1), ToRight)
    _             -> (Pixel (r-1) c, ToUp)
  where
    a = val8u im (Pixel (r-1) c) == v
    b = val8u im (Pixel r c) == v

nextPos im v (Pixel r c, ToDown) = case (a,b) of
    (False,False) -> (Pixel r (c-1), ToLeft)
    (False,True)  -> (Pixel (r+1) c, ToDown)
    _             -> (Pixel r (c+1), ToRight)
  where
    a = val8u im (Pixel r c) == v
    b = val8u im (Pixel r (c-1)) == v

nextPos im v (Pixel r c, ToLeft) = case (a,b) of
    (False,False) -> (Pixel (r-1) c, ToUp)
    (False,True)  -> (Pixel r (c-1), ToLeft)
    _             -> (Pixel (r+1) c, ToDown)
  where
    a = val8u im (Pixel r (c-1)) == v
    b = val8u im (Pixel (r-1) (c-1)) == v

nextPos im v (Pixel r c, ToUp) = case (a,b) of
    (False,False) -> (Pixel r (c+1), ToRight)
    (False,True)  -> (Pixel (r-1) c, ToUp)
    _             -> (Pixel r (c-1), ToLeft)
  where
    a = val8u im (Pixel (r-1) (c-1)) == v
    b = val8u im (Pixel (r-1) c) == v


-- | extracts a contour with given value from the given ROI of an image.
contour :: ImageGray -> Pixel -> CUChar -> Polyline
contour im start v = Closed $ pixelsToPoints (size im) $ clean $ iterate (nextPos im v) (start, ToRight)
    where clean ((a,_):rest) = a : clean' a rest
          clean' p ((v,_):rest) | p == v    = []
                                | otherwise = v: clean' p rest
